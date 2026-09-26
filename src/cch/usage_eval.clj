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
  (for [[win wrows] (group-by :win rows)]
    {:win win
     :week (:week (first wrows))
     :value (mean (for [[_ srows] (group-by #(stratum window-key (:hours %)) wrows)]
                    (mean (map value-fn srows))))}))

(defn estimate
  "Equal-window-weight mean of window values."
  [wvals]
  (when (seq wvals) (mean (map :value wvals))))

(defn block-bootstrap
  "Moving-block bootstrap over calendar weeks of the window-weighted mean.
  Returns {:est :se :lo :hi} (95% percentile interval)."
  [wvals & {:keys [block n-boot seed] :or {block 2 n-boot 2000 seed 7}}]
  (let [by-week (group-by :week wvals)
        weeks (vec (sort (keys by-week)))
        nw (count weeks)
        r (java.util.Random. seed)
        est (estimate wvals)]
    (if (< nw 2)
      {:est est :se Double/NaN :lo Double/NaN :hi Double/NaN}
      (let [block (min block nw)
            n-blocks (long (Math/ceil (/ nw (double block))))
            starts (inc (- nw block))
            stats (double-array
                    (for [_ (range n-boot)]
                      (let [picked (mapcat (fn [_] (let [s (.nextInt r starts)]
                                                     (mapcat #(get by-week (weeks %)) (range s (+ s block)))))
                                           (range n-blocks))]
                        (estimate picked))))
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
  [rows base window-key k]
  (let [diffs (map (fn [r b] (assoc b :d (- (k r) (k b)))) rows base)
        {:keys [est se lo hi]} (block-bootstrap (window-values diffs window-key :d))]
    {:delta est :se se :lo lo :hi hi}))

(defn level
  "Estimand-weighted level of key `k` (e.g. coverage as 0/1) with
  block-bootstrap uncertainty. Returns {:est :se :lo :hi}."
  [rows window-key value-fn]
  (block-bootstrap (window-values rows window-key value-fn)))
