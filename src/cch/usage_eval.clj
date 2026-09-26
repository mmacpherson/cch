(ns cch.usage-eval
  "Estimands and uncertainty for the usage-forecast backtests.

  A backtest scores forecasts at repeated checkpoints of each window. Two
  choices make a comparison between models well defined:

  * The estimand. Each checkpoint score is a per-row difference between two
    models. Rows are averaged within (window, horizon stratum), strata are
    averaged within a window, and windows are averaged with equal weight.
    Early resets, caps, and different checkpoint counts therefore do not
    shift weight between windows, and the horizon mix is fixed rather than
    implied by the checkpoint grid.
  * Uncertainty. Checkpoints within a window are correlated, and so are
    adjacent windows (habits persist across weeks). Standard errors and
    intervals come from a moving-block bootstrap over calendar weeks: whole
    weeks of windows are resampled in blocks of consecutive weeks.

  Pure functions of scored rows {:win :week :hours ...}.")

(defn stratum
  "Horizon stratum of a checkpoint `hours` into its window: the day of the
  window for the 7d meter (1-7), the hour for the 5h meter (1-5)."
  [window-key hours]
  (if (= window-key :five-hour)
    (long (max 1 (min 5 (Math/ceil hours))))
    (long (max 1 (min 7 (Math/ceil (/ hours 24.0)))))))

(defn- mean [xs] (/ (reduce + 0.0 xs) (count xs)))

(defn window-values
  "Per-window estimand contributions from rows carrying a value under
  `value-fn`: mean within stratum, then mean over strata.
  Returns [{:win :week :value} ...]."
  [rows window-key value-fn]
  (for [[win wrows] (group-by :win rows)
        :let [strata (into {} (for [[st srows] (group-by #(stratum window-key (:hours %)) wrows)]
                                [st (mean (map value-fn srows))]))]]
    {:win win
     :week (:week (first wrows))
     :agent (:agent (first wrows))
     :strata strata
     :value (mean (vals strata))}))

(defn estimate
  "Equal-window-weight mean of window values."
  [wvals]
  (when (seq wvals) (mean (map :value wvals))))

(def agent-weights
  "Fixed agent weights for the pooled 7d endpoint, stating the maintainer's
  priorities (Claude = Codex >> AGY). Percent of quota is not comparable
  across plans and Codex's irregular resets inflate its window count, so
  neither windows nor percent used can stand in for importance."
  {"claude-code" 0.46 "codex" 0.46 "agy" 0.08})

(defn horizon-estimate
  "Equal weight per horizon stratum: each stratum's mean over the windows
  that reached it, then the mean over strata. A window cut short (a
  provider-granted reset after two days) informs only its early strata, so
  late horizons are judged by the windows that got there."
  [wvals]
  (let [by-stratum (group-by key (mapcat :strata wvals))]
    (when (seq by-stratum)
      (mean (for [[_ kvs] by-stratum] (mean (map val kvs)))))))

(defn agent-weighted
  "An estimator over window values carrying :agent: `within` (default
  horizon-estimate) inside each agent, combined with `weights`,
  renormalized over the agents present (a bootstrap resample can miss a
  sparse agent)."
  [weights & {:keys [within] :or {within horizon-estimate}}]
  (fn [wvals]
    (let [by-agent (group-by :agent wvals)
          present (filter by-agent (keys weights))
          total (reduce + (map weights present))]
      (when (pos? total)
        (/ (reduce + (map #(* (weights %) (within (by-agent %))) present)) total)))))

(defn block-bootstrap
  "Moving-block bootstrap over calendar weeks of `estimator` (default the
  window-weighted mean). Returns {:est :se :lo :hi} (95% percentile
  interval); se/lo/hi are NaN below `min-weeks` calendar weeks."
  [wvals & {:keys [block n-boot seed estimator min-weeks]
            :or {block 2 n-boot 2000 seed 7 estimator estimate min-weeks 6}}]
  (let [by-week (group-by :week wvals)
        weeks (vec (sort (keys by-week)))
        nw (count weeks)
        r (java.util.Random. seed)
        est (estimator wvals)]
    ;; with only a handful of calendar weeks the resamples are near-copies
    ;; of the data and a percentile interval is meaningless
    (if (< nw (max 2 min-weeks))
      {:est est :se Double/NaN :lo Double/NaN :hi Double/NaN}
      (let [block (min block nw)
            n-blocks (long (Math/ceil (/ nw (double block))))
            starts (inc (- nw block))
            stats (double-array
                    (for [_ (range n-boot)]
                      (let [picked (mapcat (fn [_] (let [s (.nextInt r starts)]
                                                     (mapcat #(get by-week (weeks %)) (range s (+ s block)))))
                                           (range n-blocks))]
                        (estimator picked))))
            m (mean stats)
            sd (Math/sqrt (/ (reduce + (map #(Math/pow (- % m) 2) stats)) (dec n-boot)))]
        (java.util.Arrays/sort stats)
        {:est est :se sd
         :lo (aget stats (int (* 0.025 n-boot)))
         :hi (aget stats (int (* 0.975 n-boot)))}))))

(defn paired
  "Paired comparison of `rows` against `base` (same checkpoints, same order)
  on key `k` (e.g. :crps): the per-row difference, under the estimand above,
  with block-bootstrap uncertainty. Returns {:delta :se :lo :hi}."
  [rows base window-key k & {:keys [estimator] :or {estimator estimate}}]
  (let [diffs (map (fn [r b] (assoc b :d (- (k r) (k b)))) rows base)
        {:keys [est se lo hi]} (block-bootstrap (window-values diffs window-key :d) :estimator estimator)]
    {:delta est :se se :lo lo :hi hi}))

(defn level
  "Estimand-weighted level of key `k` (e.g. coverage as 0/1) with
  block-bootstrap uncertainty. Returns {:est :se :lo :hi}."
  [rows window-key value-fn & {:keys [estimator] :or {estimator estimate}}]
  (block-bootstrap (window-values rows window-key value-fn) :estimator estimator))
