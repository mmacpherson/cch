(ns cch.usage-backtest
  "Rolling-origin replay of the usage forecast against local history.

  For each completed window, refit the model on data available before the
  window started, then forecast at fixed checkpoints using only observations
  up to each checkpoint. Forecasts are scored against the window's pct at its
  actual end (early and granted resets included); windows that hit the cap
  are right-censored at 100%. The production rate-Bayes projection is scored
  on the same checkpoints for comparison.

  Reads the local event DB and prints aggregate metrics only. Run with
  `just usage-backtest`."
  (:require [cch.db :as db]
            [cch.forecast :as forecast]
            [cch.numeric :as num]
            [cch.projections :as proj]
            [cch.usage-baselines :as base]
            [cch.usage-ct :as ct]
            [cch.usage-eval :as ev]
            [cch.usage-model :as m]
            [cch.usage-stan :as stan]
            [cch.usage-ticks :as ticks]
            [clojure.java.shell :as shell]
            [clojure.string :as str])
  (:import (java.time ZoneId)))

(def ^:private window-sql {:seven-day "seven_day" :five-hour "five_hour"})

(def ^:private plan
  {:seven-day {:checkpoint-secs (* 12 3600) :refit :per-window :min-train 5}
   :five-hour {:checkpoint-secs 1800 :refit :weekly :min-train 20}})

(defn- raw-observations [agent window-key]
  ;; agent and window come from fixed lists in `run`, never from input.
  (->> (db/query (format (str "SELECT CAST(observed_at/1000 AS INTEGER) AS ts, used_percentage AS pct,"
                              " resets_at FROM usage_observations WHERE agent='%s' AND window_key='%s'"
                              " ORDER BY observed_at")
                         agent (window-sql window-key)))
       (mapv (fn [{:keys [ts pct resets_at]}] [(long ts) (double pct) (long resets_at)]))))

(defn- aggregate [obs]
  (->> obs
       (reduce (fn [acc [ts pct r]]
                 (update acc [r (* 3600 (quot ts 3600))] (fnil max 0.0) pct))
               {})
       (map (fn [[[r h] p]] {:resets-at r :hour h :pct p}))
       (sort-by :hour)
       vec))

(defn- index-of-ts
  "First index in `ts-arr` (sorted) with value >= t."
  ^long [^longs ts-arr t]
  (let [i (java.util.Arrays/binarySearch ts-arr (long t))]
    (if (neg? i)
      (- (inc i))
      (loop [i i] (if (and (pos? i) (= (aget ts-arr (dec i)) t)) (recur (dec i)) i)))))

(defn- history
  "Observation history with O(log n) as-of views: `rows-before` returns the
  hourly aggregate rows the forecaster would have seen at t."
  [obs]
  (let [hourly (aggregate obs)
        hour-arr (long-array (map :hour hourly))
        ts-arr (long-array (map first obs))]
    {:obs obs
     :rows-before
     (fn [t]
       (let [h0 (* 3600 (quot t 3600))
             complete (subvec hourly 0 (index-of-ts hour-arr h0))
             partial (subvec obs (index-of-ts ts-arr h0) (index-of-ts ts-arr t))]
         (into complete (aggregate partial))))}))

(defn- window-obs
  "Observations belonging to window `w` (reset clustered), in time order."
  [obs spec w]
  (filterv (fn [[ts _ r]] (and (>= ts (:start w)) (< ts (:end w))
                               (<= (Math/abs (- r (:end w))) (:cluster-secs spec))))
           obs))

(defn- pct-at [wobs t]
  (reduce (fn [mx [ts pct]] (if (< ts t) (max mx pct) (reduced mx))) 0.0 wobs))

(defn- rate-projection
  "Production rate-Bayes projection at t, for comparison."
  [wobs window-key w prior-finals t]
  (let [bucket (if (= window-key :five-hour) 60 360)
        {:keys [prior-mu prior-sigma]} (forecast/prior-params window-key prior-finals)
        samples (->> wobs
                     (take-while #(< (first %) t))
                     (reduce (fn [{:keys [mx seen out] :as acc} [ts pct]]
                               (cond
                                 (< pct mx) acc
                                 (seen (quot ts bucket)) (assoc acc :mx (max mx pct))
                                 :else {:mx (max mx pct) :seen (conj seen (quot ts bucket))
                                        :out (conj out {:ts ts :pct pct})}))
                             {:mx 0.0 :seen #{} :out []})
                     :out)
        x (or (:pct (peek samples)) 0.0)
        r (proj/rate-bayes-projection samples {:now t :resets-at (:eff-end w) :last-pct x
                                                :prior-mu prior-mu :prior-sigma prior-sigma})]
    {:median (or (:proj r) x) :lo (get-in r [:band :lo] x) :hi (get-in r [:band :hi] x)}))

(defn- score-row [{:keys [median lo hi]} final capped?]
  {:err (if capped? (max 0.0 (- 100.0 median)) (Math/abs (- median final)))
   :cov (if capped? (>= hi 99.0) (<= (- lo 1.0) final (+ hi 1.0)))})

(defn replay
  "Backtest one agent/window. Returns scored checkpoint rows."
  [agent window-key & {:keys [now zone] :or {now (quot (System/currentTimeMillis) 1000)
                                             zone (ZoneId/systemDefault)}}]
  (let [spec (m/specs window-key)
        {:keys [checkpoint-secs refit min-train]} (plan window-key)
        obs (raw-observations agent window-key)
        {:keys [rows-before]} (history obs)
        wins (m/windows (rows-before now) spec)
        done? (fn [w] (or (:cap w) (< (:eff-end w) (- now 3600))))
        fit-at (memoize (fn [t] (m/fit-model (rows-before t) spec zone t)))
        refit-time (fn [w] (case refit
                             :per-window (:start w)
                             :weekly (* 604800 (quot (:start w) 604800))))]
    (vec
      (for [[i w] (map-indexed vector wins)
            :when (and (>= i min-train) (done? w))
            :let [model (fit-at (refit-time w))
                  final (m/cum-at w (inc (:eff-end w)))
                  capped? (boolean (:cap w))
                  prior-finals (->> (subvec wins 0 i) (map :final) reverse (take 12)
                                    (filter #(>= % 10.0)) vec)
                  wobs (window-obs obs spec w)]
            t (range (+ (:start w) checkpoint-secs) (- (m/live-end w) 300) checkpoint-secs)
            :let [x (pct-at wobs t)
                  g (m/forecast model (rows-before t) spec zone t (:eff-end w) x :path? false)
                  b (rate-projection wobs window-key w prior-finals t)]]
        {:hours (/ (- t (:start w)) 3600.0)
         :capped? capped? :p-cap (:p-cap g)
         :model (score-row g final capped?)
         :rate (score-row b final capped?)}))))

(defn- summarize [rows]
  (let [n (count rows)
        mean (fn [f] (/ (reduce + 0.0 (map f rows)) (max 1 n)))
        cap-rate (mean #(if (:capped? %) 1.0 0.0))]
    {:n n
     :rate-mae (mean (comp :err :rate)) :rate-cov (mean #(if (get-in % [:rate :cov]) 1.0 0.0))
     :model-mae (mean (comp :err :model)) :model-cov (mean #(if (get-in % [:model :cov]) 1.0 0.0))
     :brier (mean #(Math/pow (- (:p-cap %) (if (:capped? %) 1.0 0.0)) 2))
     :brier-climatology (* cap-rate (- 1.0 cap-rate))}))

(defn run
  "Print backtest metrics for each agent/window with enough history."
  [& _]
  (doseq [agent ["claude-code" "codex" "agy"]
          window-key [:seven-day :five-hour]
          :let [rows (replay agent window-key)]
          :when (seq rows)
          :let [span (if (= window-key :seven-day) 24.0 1.0)
                unit (if (= window-key :seven-day) "day" "hour")
                buckets (group-by #(min 5 (int (Math/ceil (/ (:hours %) span)))) rows)]]
    (println (format "\n%s %s" agent (name window-key)))
    (println (format "  %-8s %5s   %14s   %14s   %s" unit "n" "rate MAE / cov" "model MAE / cov" ""))
    (doseq [[k rs] (concat (sort-by key buckets) [[:all rows]])
            :let [s (summarize rs)]]
      (println (format "  %-8s %5d   %6.1f / %3.0f%%   %6.1f / %3.0f%%"
                       (case k :all "all" 5 "5+" (str k))
                       (:n s) (:rate-mae s) (* 100 (:rate-cov s)) (:model-mae s) (* 100 (:model-cov s)))))
    (let [s (summarize rows)]
      (println (format "  P(cap) Brier %.4f vs climatology %.4f" (:brier s) (:brier-climatology s))))))

;; --- profile pooling experiment ---
;;
;; Same replay, but the activity profile comes from a pooling variant while
;; theta, on/off behavior, and the filtered state stay per agent/window.
;; Variants are compared on identical checkpoints.

(def ^:private cells
  (for [agent ["claude-code" "codex" "agy"] window-key [:seven-day :five-hour]]
    [agent window-key]))

(def ^:private variants
  "Profile variants: [label target k]. Target :agent pools the agent's 5h
  and 7d cells; :fleet pools every cell; :two-level shrinks the agent pool
  toward the fleet (with k2) before shrinking the cell toward it."
  [["independent" nil 0]
   ["agent-shared" :agent ##Inf]
   ["fleet-shared" :fleet ##Inf]
   ["agent k=4" :agent 4] ["agent k=12" :agent 12] ["agent k=36" :agent 36]
   ["fleet k=4" :fleet 4] ["fleet k=12" :fleet 12] ["fleet k=36" :fleet 36]
   ["two-level 12/12" [:two-level 12] 12] ["two-level 36/12" [:two-level 12] 36]
   ["fleet EB k" :fleet :eb]
   ["fleet harmonic K=4" :harmonic 4]
   ["harmonic K=4, no weekday" :harmonic-flat 4]])

(def ^:private focus-variants
  "The comparison that decides adoption (claude-code-hooks-20r)."
  #{"independent" "fleet EB k" "fleet harmonic K=4" "harmonic K=4, no weekday"})

(defn- variant-profile
  "Smoothed profile for `cell` at time t under `variant`; nil = independent.
  k = :eb estimates the shrinkage strength per cell (eb-rates); target
  :harmonic fits one fleet shape in the weekday + daily-harmonic basis."
  [{:keys [stats-at weekly-at]} cell t [_ target k]]
  (cond
    (= target :harmonic)
    (m/harmonic-profile (keep #(stats-at % t) cells) k)

    (= target :harmonic-flat)
    (m/harmonic-profile (keep #(stats-at % t) cells) k :weekday? false)

    target
    (let [own (stats-at cell t)
          agent-cells (filter #(= (first %) (first cell)) cells)
          pooled (fn [cs] (m/pooled-rates (keep #(stats-at % t) cs)))
          as-stats (fn [cs] {:usage (apply mapv + (map #(:usage (stats-at % t)) cs))
                             :exposure (apply mapv + (map #(:exposure (stats-at % t)) cs))})
          target-rates (cond
                         (= target :agent) (pooled agent-cells)
                         (= target :fleet) (pooled cells)
                         :else (m/shrunk-rates (as-stats agent-cells) (pooled cells) (second target)))]
      (m/smooth-profile
        (cond
          (= k :eb) (:rates (m/eb-rates (weekly-at cell t) target-rates))
          (Double/isInfinite (double k)) target-rates
          :else (m/shrunk-rates own target-rates k))))))

(defn- cell-data [now [agent window-key :as cell]]
  (let [obs (raw-observations agent window-key)
        {:keys [rows-before]} (history obs)
        spec (m/specs window-key)]
    {:cell cell :spec spec :obs obs :rows-before rows-before
     :wins (m/windows (rows-before now) spec)
     :series (memoize (fn [t] (m/hour-series (m/windows (rows-before t) spec) t)))}))

(defn- replay-variant
  [{:keys [cell spec obs rows-before wins]} access variant now zone]
  (let [[_ window-key] cell
        {:keys [checkpoint-secs refit min-train]} (plan window-key)
        done? (fn [w] (or (:cap w) (< (:eff-end w) (- now 3600))))
        refit-time (fn [w] (case refit
                             :per-window (:start w)
                             :weekly (* 604800 (quot (:start w) 604800))))
        fit-at (memoize (fn [t] (m/fit-model (rows-before t) spec zone t
                                             :profile-override (variant-profile access cell t variant))))]
    (vec
      (for [[i w] (map-indexed vector wins)
            :when (and (>= i min-train) (done? w))
            :let [model (fit-at (refit-time w))
                  final (m/cum-at w (inc (:eff-end w)))
                  capped? (boolean (:cap w))
                  wobs (window-obs obs spec w)]
            t (range (+ (:start w) checkpoint-secs) (- (m/live-end w) 300) checkpoint-secs)
            :let [g (m/forecast model (rows-before t) spec zone t (:eff-end w) (pct-at wobs t) :path? false)
                  in? (fn [lo hi] (if capped? (>= hi 99.0) (<= (- lo 1.0) final (+ hi 1.0))))]]
        {:win i :week (quot (:start w) 604800) :hours (/ (- t (:start w)) 3600.0)
         :crps (m/crps (:draws g) final 100.0)
         :err (if capped? (max 0.0 (- 100.0 (:median g))) (Math/abs (- (:median g) final)))
         :cov50 (in? (:q25 g) (:q75 g))
         :cov90 (in? (:lo g) (:hi g))
         :brier (Math/pow (- (:p-cap g) (if capped? 1.0 0.0)) 2)}))))

(defn- mean [xs] (if (seq xs) (/ (reduce + 0.0 xs) (count xs)) Double/NaN))

(defn- paired-delta
  "CRPS difference vs `base` rows (same checkpoints) under cch.usage-eval's
  estimand (equal weight per window, then per horizon stratum), with a
  calendar-week block-bootstrap SE and 95% interval."
  [rows base window-key]
  (ev/paired rows base window-key :crps))

(def ^:private summary-header
  (format "  %-24s %6s  %-24s %6s %6s %-14s %7s" "variant" "CRPS" "dCRPS vs base [95%]" "MAE" "cov50" "cov90 [95%]" "Brier"))

(defn- summary-line
  "One variant under cch.usage-eval's estimand (equal weight per window,
  then per horizon stratum): levels, the paired CRPS difference vs `base`,
  and calendar-week block-bootstrap intervals."
  [label rows base window-key & {:keys [estimator] :or {estimator ev/estimate}}]
  (let [lv (fn [f] (:est (ev/level rows window-key f :estimator estimator)))
        bool (fn [k] #(if (k %) 1.0 0.0))
        cov90 (ev/level rows window-key (bool :cov90) :estimator estimator)
        {:keys [delta lo hi]} (ev/paired rows base window-key :crps :estimator estimator)]
    ;; too few calendar weeks for an interval: say so, rather than print a
    ;; NaN that reads like a numerical failure
    (format "  %-24s %6.2f  %+6.2f %-17s  %6.1f %5.0f%% %3.0f%% %-9s %s"
            label (lv :crps) delta
            (if (Double/isNaN lo) "[n/a: few weeks]" (format "[%+6.2f, %+6.2f]" lo hi))
            (lv :err) (* 100 (lv (bool :cov50))) (* 100 (:est cov90))
            (if (Double/isNaN (:lo cov90)) "[n/a]" (format "[%2.0f-%3.0f]" (* 100 (:lo cov90)) (* 100 (:hi cov90))))
            (if (:brier (first rows)) (format "%7.4f" (lv :brier)) ""))))

(defn- print-pooled-7d
  "The primary endpoint: 7d accuracy pooled across agents. `by-cell` maps
  cell -> {label rows}, rows paired by position with the `base` label's.
  Windows weigh equally within an agent, and agents combine with the fixed
  weights in cch.usage-eval/agent-weights; the calendar-week bootstrap
  resamples weeks jointly across agents, whose weekly habits are shared."
  [by-cell labels base & {:keys [contrasts title] :or {title "primary endpoint"}}]
  (let [cells7 (sort (filter #(= :seven-day (second %)) (keys by-cell)))
        pooled (fn [label] (vec (for [cell cells7
                                      r (get-in by-cell [cell label])]
                                  (assoc r :win [(first cell) (:win r)] :agent (first cell)))))
        base-rows (pooled base)
        estimator (ev/agent-weighted ev/agent-weights)]
    (when (seq base-rows)
      (println (format "\nseven-day pooled across agents  (%d checkpoints; windows %s; weights %s; %s)"
                       (count base-rows)
                       (str/join " " (for [[a rs] (sort (group-by :agent base-rows))] (str a " " (count (distinct (map :win rs))))))
                       (str/join " " (for [[a w] (sort ev/agent-weights)] (str a " " w)))
                       title))
      (println summary-header)
      (doseq [label labels]
        (println (summary-line label (pooled label) base-rows :seven-day :estimator estimator)))
      (doseq [[a b] contrasts
              :let [{:keys [delta lo hi]} (ev/paired (pooled a) (pooled b) :seven-day :crps :estimator estimator)]]
        (println (format "  %s vs %s  %+6.2f %s" a b delta
                         (if (Double/isNaN lo) "[n/a: few weeks]" (format "[%+.2f, %+.2f]" lo hi))))))))

(defn run-pooling
  "Print the profile pooling experiment for every agent/window with history.
  With `:focus true`, only the variants that decide adoption. `:base` names
  the variant deltas are paired against (default \"independent\")."
  [& {:keys [focus base] :or {base "independent"}}]
  (let [now (quot (System/currentTimeMillis) 1000)
        zone (ZoneId/systemDefault)
        data (into {} (map (fn [c] [c (cell-data now c)]) cells))
        series-at (memoize (fn [cell t] ((:series (data cell)) t)))
        access {:stats-at (memoize (fn [cell t] (m/bin-stats (series-at cell t) zone)))
                :weekly-at (memoize (fn [cell t] (m/weekly-bin-rates (series-at cell t) zone)))}
        chosen (if focus (filter #(focus-variants (first %)) variants) variants)]
    (print-pooled-7d
      (into {} (for [cell cells
                     :let [d (data cell)
                           results (into {} (pmap (fn [v] [(first v) (replay-variant d access v now zone)]) chosen))
                           base-rows (results base)]
                     :when (seq base-rows)]
                 (do (println (format "\n%s %s  (%d checkpoints, %d windows)" (first cell) (name (second cell))
                                      (count base-rows) (count (distinct (map :win base-rows)))))
                     (println summary-header)
                     (doseq [[label] chosen]
                       (println (summary-line label (results label) base-rows (second cell))))
                     [cell results])))
      (map first chosen) base)))

;; --- hierarchical Stan comparison ---
;;
;; Weekly refits (the production job's cadence) of three variants, scored on
;; identical checkpoints: the maximum-likelihood fit with an independent
;; profile, the same fit with the fleet-shrunk profile (estimated k), and the
;; hierarchical posterior from Stan (bin/cch-usage-stan-fit), forecast as a
;; mixture over draws.

(defn- stan-fit!
  "Run the Stan job on data before `t`; returns {cell [fits]} or nil."
  [data t dir {:keys [draws warmup samples]}]
  (let [rows-by-cell (into {} (for [[cell d] data] [cell ((:rows-before d) t)]))
        in (str dir "/stan-data-" t ".json")
        out (str dir "/stan-draws-" t ".json")
        cells (stan/write-fit-data in rows-by-cell t (ZoneId/systemDefault))]
    (if (empty? cells)
      (println (format "  stan fit @ %s: skipped (no cell has usage yet)" (java.time.Instant/ofEpochSecond t)))
      ;; A hard timeout, so one stalled chain cannot block the comparison.
      (let [{:keys [exit err]} (shell/sh "timeout" "3600" "bin/cch-usage-stan-fit" in out
                                         "--draws" (str draws) "--warmup" (str warmup)
                                         "--samples" (str samples))]
        (println (format "  stan fit @ %s: exit %d %s" (java.time.Instant/ofEpochSecond t) exit
                         (last (str/split-lines (str err)))))
        (when (zero? exit) (stan/read-draws out cells t))))))

(defn run-stan
  "Compare independent, fleet-EB, and hierarchical Stan fits on weekly refits.
  Stan fits are cached in `dir` by refit time, so reruns reuse them. `only`
  restricts target cells, and `max-refits` keeps the latest N refit weeks
  (or `first-refits` the earliest N), for smoke tests."
  [& {:keys [dir draws warmup samples only max-refits first-refits]
      ;; Outside target/: a build cleans target/ and would delete a running
      ;; comparison's inputs and cached fits.
      :or {dir (str (System/getProperty "user.home") "/.cache/cch/usage-stan-backtest")
           draws 40 warmup 300 samples 200}}]
  (.mkdirs (java.io.File. ^String dir))
  (let [now (quot (System/currentTimeMillis) 1000)
        zone (ZoneId/systemDefault)
        data (into {} (map (fn [c] [c (cell-data now c)]) cells))
        week 604800
        refit-of (fn [w] (* week (quot (:start w) week)))
        targets (for [[cell d] data
                      :let [[_ wk] cell
                            {:keys [checkpoint-secs min-train]} (plan wk)]
                      [i w] (map-indexed vector (:wins d))
                      :when (and (>= i min-train) (or (:cap w) (< (:eff-end w) (- now 3600)))
                                 (or (nil? only) (only cell)))]
                  {:cell cell :d d :w w :i i :checkpoint-secs checkpoint-secs :refit (refit-of w)})
        refits (sort (distinct (map :refit targets)))
        keep (cond max-refits (set (take-last max-refits refits))
                   first-refits (set (take first-refits refits)))
        targets (if keep (filter #(keep (:refit %)) targets) targets)
        stan-at (memoize (fn [t]
                           (let [cached (str dir "/stan-draws-" t ".json")
                                 cells-file (str dir "/stan-cells-" t ".edn")]
                             (if (.exists (java.io.File. cached))
                               (stan/read-draws cached (read-string (slurp cells-file)) t)
                               (let [fits (stan-fit! data t dir {:draws draws :warmup warmup :samples samples})]
                                 (when fits (spit cells-file (pr-str (vec (keys fits)))))
                                 fits)))))
        series-at (memoize (fn [cell t] ((:series (data cell)) t)))
        fit-at (memoize (fn [cell t eb?]
                          (let [{:keys [spec rows-before]} (data cell)
                                prof (when eb?
                                       (:profile (get (m/fleet-profiles
                                                        (into {} (for [c cells :let [s (series-at c t)] :when (seq s)] [c s]))
                                                        zone)
                                                      cell)))]
                            (m/fit-model (rows-before t) spec zone t :profile-override prof))))
        rows (vec
               (for [{:keys [cell d w i checkpoint-secs refit]} (sort-by :refit targets)
                     :let [fits (get (stan-at refit) cell)]
                     :when (seq fits)
                     :let [{:keys [spec obs rows-before]} d
                           final (m/cum-at w (inc (:eff-end w)))
                           capped? (boolean (:cap w))
                           wobs (window-obs obs spec w)]
                     t (range (+ (:start w) checkpoint-secs) (- (m/live-end w) 300) checkpoint-secs)
                     :let [rows (rows-before t)
                           x (pct-at wobs t)
                           score (fn [g]
                                   (let [in? (fn [lo hi] (if capped? (>= hi 99.0) (<= (- lo 1.0) final (+ hi 1.0))))]
                                     {:crps (m/crps (:draws g) final 100.0)
                                      :err (if capped? (max 0.0 (- 100.0 (:median g))) (Math/abs (- (:median g) final)))
                                      :cov50 (in? (:q25 g) (:q75 g)) :cov90 (in? (:lo g) (:hi g))
                                      :brier (Math/pow (- (:p-cap g) (if capped? 1.0 0.0)) 2)}))]]
                 {:cell cell :win i :week (quot (:start w) 604800) :hours (/ (- t (:start w)) 3600.0)
                  "independent" (score (m/forecast (fit-at cell refit false) rows spec zone t (:eff-end w) x :path? false))
                  "fleet EB k" (score (m/forecast (fit-at cell refit true) rows spec zone t (:eff-end w) x :path? false))
                  "stan" (score (stan/mixture-forecast (take draws fits) rows spec zone t (:eff-end w) x))}))]
    (print-pooled-7d
      (into {} (for [[cell cell-rows] (sort-by key (group-by :cell rows))
                     :let [ids (mapv #(select-keys % [:win :week :hours]) cell-rows)
                           by-label (into {} (for [label ["independent" "fleet EB k" "stan"]]
                                               [label (mapv #(merge %2 (get %1 label)) cell-rows ids)]))]]
                 (do (println (format "\n%s %s  (%d checkpoints, %d windows)" (first cell) (name (second cell))
                                      (count cell-rows) (count (distinct (map :win cell-rows)))))
                     (println summary-header)
                     (doseq [label ["independent" "fleet EB k" "stan"]]
                       (println (summary-line label (by-label label) (by-label "independent") (second cell))))
                     [cell by-label])))
      ["independent" "fleet EB k" "stan"] "independent")))

;; --- momentum experiment (claude-code-hooks-w7v) ---
;;
;; Does carrying state from hour to hour help? Arms share the replay, the
;; checkpoints, and the production (fleet-EB) profile; only the block
;; resolution and the discount differ.

(def ^:private hourly-overrides
  "7d at hourly resolution: the 5h window's block settings and start point."
  (-> (m/specs :five-hour)
      (select-keys [:block-hours :anchor-hour :min-live-hours :filter-lookback-secs :x0])
      (assoc :fit-lookback-secs nil)))

(def ^:private no-memory {5 -30.0})

(defn- momentum-arms [window-key]
  (case window-key
    :seven-day [["A daily" {} nil] ["B hourly+mem" hourly-overrides nil] ["C hourly, d=0" hourly-overrides no-memory]]
    :five-hour [["B hourly+mem" {} nil] ["C hourly, d=0" {} no-memory]]))

(defn- replay-arm
  [{:keys [cell obs rows-before wins]} profile-at [_ overrides fixed] now zone]
  (let [[_ window-key] cell
        spec (merge (m/specs window-key) overrides)
        {:keys [checkpoint-secs refit min-train]} (plan window-key)
        done? (fn [w] (or (:cap w) (< (:eff-end w) (- now 3600))))
        refit-time (fn [w] (case refit
                             :per-window (:start w)
                             :weekly (* 604800 (quot (:start w) 604800))))
        fit-at (memoize (fn [t] (m/fit-model (rows-before t) spec zone t
                                             :profile-override (profile-at cell t) :fixed fixed)))]
    (vec
      (for [[i w] (map-indexed vector wins)
            :when (and (>= i min-train) (done? w))
            :let [model (fit-at (refit-time w))
                  final (m/cum-at w (inc (:eff-end w)))
                  capped? (boolean (:cap w))
                  wobs (window-obs obs spec w)]
            t (range (+ (:start w) checkpoint-secs) (- (m/live-end w) 300) checkpoint-secs)
            :let [g (m/forecast model (rows-before t) spec zone t (:eff-end w) (pct-at wobs t) :path? false)
                  in? (fn [lo hi] (if capped? (>= hi 99.0) (<= (- lo 1.0) final (+ hi 1.0))))]]
        {:win i :week (quot (:start w) 604800) :hours (/ (- t (:start w)) 3600.0)
         :crps (m/crps (:draws g) final 100.0)
         :err (if capped? (max 0.0 (- 100.0 (:median g))) (Math/abs (- (:median g) final)))
         :cov50 (in? (:q25 g) (:q75 g))
         :cov90 (in? (:lo g) (:hi g))
         :brier (Math/pow (- (:p-cap g) (if capped? 1.0 0.0)) 2)}))))

(defn run-momentum
  "Print the momentum experiment for every agent/window with history, or
  only the cells in `only` (e.g. #{[\"codex\" :seven-day]})."
  [& {:keys [only]}]
  (let [now (quot (System/currentTimeMillis) 1000)
        zone (ZoneId/systemDefault)
        data (into {} (map (fn [c] [c (cell-data now c)]) cells))
        series-at (memoize (fn [cell t] ((:series (data cell)) t)))
        profile-at (memoize (fn [cell t]
                              (:profile (get (m/fleet-profiles
                                               (into {} (for [c cells :let [s (series-at c t)] :when (seq s)] [c s]))
                                               zone)
                                             cell))))]
    (doseq [cell (if only (filter only cells) cells)
            :let [[_ wk] cell
                  arms (momentum-arms wk)
                  results (into {} (pmap (fn [a] [(first a) (replay-arm (data cell) profile-at a now zone)]) arms))
                  base (results (ffirst arms))]
            :when (seq base)]
      (println (format "\n%s %s  (%d checkpoints, %d windows; deltas vs %s)" (first cell) (name wk)
                       (count base) (count (distinct (map :win base))) (ffirst arms)))
      (println summary-header)
      (doseq [[label] arms]
        (println (summary-line label (results label) base (second cell))))
      (let [span (if (= wk :seven-day) 24.0 1.0)
            bucket #(min 5 (int (Math/ceil (/ (:hours %) span))))]
        (println (format "  CRPS by elapsed %s:" (if (= wk :seven-day) "day" "hour")))
        (doseq [[label] arms
                :let [by (group-by bucket (results label))]]
          (println (format "    %-15s %s" label
                           (str/join "  " (for [k (sort (keys by))]
                                            (format "%s:%.2f" (if (= k 5) "5+" k) (mean (map :crps (by k)))))))))))))

;; --- continuous-time experiment (claude-code-hooks-lbz) ---
;;
;; Arms A (production blocks), B (continuous time, one timescale) and C
;; (continuous time, two log-OU timescales), refit weekly on identical
;; checkpoints with the production harmonic fleet profile.

(defn run-ct
  "Print the continuous-time experiment. `only` restricts cells;
  `max-refits` keeps the latest N refit weeks (smoke tests)."
  [& {:keys [only max-refits]}]
  (let [now (quot (System/currentTimeMillis) 1000)
        zone (ZoneId/systemDefault)
        week 604800
        data (into {} (map (fn [c] [c (cell-data now c)]) cells))
        series-at (memoize (fn [cell t] ((:series (data cell)) t)))
        profile-at (memoize (fn [t] (m/fleet-harmonic-profile
                                      (into {} (for [c cells :let [s (series-at c t)] :when (seq s)] [c s]))
                                      zone)))
        refit-of (fn [w] (* week (quot (:start w) week)))
        targets (for [[cell d] data
                      :when (or (nil? only) (only cell))
                      :let [[_ wk] cell {:keys [checkpoint-secs min-train]} (plan wk)]
                      [i w] (map-indexed vector (:wins d))
                      :when (and (>= i min-train) (or (:cap w) (< (:eff-end w) (- now 3600))))]
                  {:cell cell :w w :i i :checkpoint-secs checkpoint-secs :refit (refit-of w)})
        keep (when max-refits (set (take-last max-refits (sort (distinct (map :refit targets))))))
        targets (if keep (filter #(keep (:refit %)) targets) targets)
        fit-a (memoize (fn [cell t] (let [{:keys [spec rows-before]} (data cell)]
                                      (m/fit-model (rows-before t) spec zone t :profile-override (profile-at t)))))
        fit-ct (memoize (fn [cell t arm] (let [{:keys [spec rows-before]} (data cell)]
                                           (ct/fit-model (rows-before t) spec zone t arm (profile-at t)))))
        ;; fit all (cell, refit) pairs in parallel first
        _ (dorun (pmap (fn [[cell t arm]] (if (= arm :A) (fit-a cell t) (fit-ct cell t arm)))
                       (for [[cell t] (distinct (map (juxt :cell :refit) targets)) arm [:A :B :C]] [cell t arm])))
        rows (vec
               (pmap
                 (fn [{:keys [cell w i checkpoint-secs refit]}]
                   (let [{:keys [spec obs rows-before]} (data cell)
                         final (m/cum-at w (inc (:eff-end w)))
                         capped? (boolean (:cap w))
                         wobs (window-obs obs spec w)]
                     (vec
                       (for [t (range (+ (:start w) checkpoint-secs) (- (m/live-end w) 300) checkpoint-secs)
                             :let [rows (rows-before t) x (pct-at wobs t)
                                   in? (fn [lo hi] (if capped? (>= hi 99.0) (<= (- lo 1.0) final (+ hi 1.0))))
                                   score (fn [g] {:win i :week (quot (:start w) 604800) :hours (/ (- t (:start w)) 3600.0)
                                                  :crps (m/crps (:draws g) final 100.0)
                                                  :err (if capped? (max 0.0 (- 100.0 (:median g))) (Math/abs (- (:median g) final)))
                                                  :cov90 (in? (:lo g) (:hi g)) :cov50 (in? (:q25 g) (:q75 g))})]]
                         {:cell cell :win i
                          :A (score (m/forecast (fit-a cell refit) rows spec zone t (:eff-end w) x :path? false))
                          :B (score (ct/forecast-arm (fit-ct cell refit :B) rows spec zone t (:eff-end w) x))
                          :C (score (ct/forecast-arm (fit-ct cell refit :C) rows spec zone t (:eff-end w) x))}))))
                 targets))
        rows (vec (apply concat rows))]
    (doseq [[cell cr] (sort-by key (group-by :cell rows))]
      (println (format "\n%s %s  (%d checkpoints, %d windows; deltas vs A)" (first cell) (name (second cell))
                       (count cr) (count (distinct (map :win cr)))))
      (println summary-header)
      (doseq [arm [:A :B :C]]
        (println (summary-line (name arm) (mapv arm cr) (mapv :A cr) (second cell))))
      (let [{:keys [delta lo hi]} (paired-delta (mapv :C cr) (mapv :B cr) (second cell))]
        (println (format "  C vs B  %+6.2f [%+.2f, %+.2f]" delta lo hi))))
    (print-pooled-7d (into {} (for [[cell cr] (group-by :cell rows)]
                                [cell (into {} (for [arm [:A :B :C]]
                                                 [(name arm) (mapv #(merge (select-keys (:A %) [:win :week :hours]) (arm %)) cr)]))]))
                     ["A" "B" "C"] "A" :contrasts [["C" "B"]])))

;; --- count-process ladder (ground floor up) ---

(defn- band-forecast
  "Draws from the rate-Bayes projection's normal band (median, 90% band),
  floored at the current reading, so it is scored like the other rungs."
  [{:keys [median lo hi]} x n seed]
  (let [sd (max 1e-9 (/ (- hi lo) (* 2 1.6448536269514722)))
        r (java.util.Random. seed)
        draws (double-array (repeatedly n #(max x (+ median (* sd (.nextGaussian r))))))]
    (java.util.Arrays/sort draws)
    {:median (num/quantile draws 0.5) :lo (num/quantile draws 0.05)
     :q25 (num/quantile draws 0.25) :q75 (num/quantile draws 0.75) :hi (num/quantile draws 0.95)
     :draws draws}))

(def ^:private ladder-horizons
  "Fixed look-aheads (hours) scored besides the reset: together they score
  the path, i.e. the first-passage curve (P(meter >= L by t) = P(X_t >= L))."
  {:seven-day [24 72] :five-hour [1 3]})

(def ^:private ladder-arms
  ["R0 linear" "R1 Poisson" "R2 +profile" "R3 +bursts (NB)" "R4 +recency (NB, 28d)"
   "R5 +weekly pace" "R6 +bursts +weekly pace" "R7 +on/off blocks" "R8 +drifting pace" "A production"])

(defn run-ladder
  "Print the count-process ladder: nested baselines from linear extrapolation
  up to the production model, each adding one ingredient, on the same
  refits, checkpoints, and estimands as the other experiments.
  `max-refits` keeps the latest N refit weeks (smoke tests)."
  [& {:keys [only max-refits]}]
  (let [now (quot (System/currentTimeMillis) 1000)
        zone (ZoneId/systemDefault)
        week 604800
        data (into {} (map (fn [c] [c (cell-data now c)]) cells))
        series-at (memoize (fn [cell t] ((:series (data cell)) t)))
        profile-at (memoize (fn [t] (m/fleet-harmonic-profile
                                      (into {} (for [c cells :let [s (series-at c t)] :when (seq s)] [c s]))
                                      zone)))
        refit-of (fn [w] (* week (quot (:start w) week)))
        targets (for [[cell d] data
                      :when (or (nil? only) (only cell))
                      :let [[_ wk] cell {:keys [min-train]} (plan wk)]
                      [i w] (map-indexed vector (:wins d))
                      :when (and (>= i min-train) (or (:cap w) (< (:eff-end w) (- now 3600))))]
                  {:cell cell :w w :i i :refit (refit-of w)})
        keep (when max-refits (set (take-last max-refits (sort (distinct (map :refit targets))))))
        targets (if keep (filter #(keep (:refit %)) targets) targets)
        fit-a (memoize (fn [cell t] (let [{:keys [spec rows-before]} (data cell)]
                                      (m/fit-model (rows-before t) spec zone t :profile-override (profile-at t)))))
        ;; rung 4 is rung 3 on the last 28 days: the crudest handle on drift
        ;; (and on plan changes, which rescale the meter)
        fit-rung (memoize (fn [cell t rung]
                            (let [{:keys [spec rows-before]} (data cell)
                                  series (m/hour-series (m/windows (rows-before t) spec) t)
                                  lookback (if (= rung 4) (* 28 86400) (:fit-lookback-secs spec))
                                  series (if lookback (into (sorted-map) (filter #(>= (key %) (- t lookback)) series)) series)]
                              (base/fit (min rung 3) (base/training-steps series (profile-at t) zone)))))
        ;; rung 5 trains on finished windows as replicates: each window's
        ;; total over its activity mass (a truncated window counts only the
        ;; hours it covered)
        fit-weekly (memoize (fn [cell t]
                              (let [{:keys [spec rows-before]} (data cell)
                                    wins (m/windows (rows-before t) spec)
                                    series (m/hour-series wins t)
                                    prof (profile-at t)
                                    lookback (:fit-lookback-secs spec)
                                    mass-of (fn [w] (reduce + 0.0 (for [[h [live _]] (subseq series >= (* 3600 (quot (:start w) 3600)) < (:eff-end w))]
                                                                    (* live (nth prof (m/hour-of-week zone h))))))]
                                (base/fit-weekly
                                  (vec (for [w wins
                                             :when (and (<= (:eff-end w) t) (or (nil? lookback) (>= (:start w) (- t lookback))))]
                                         [(mass-of w) (m/cum-at w (inc (:eff-end w)))]))))))
        ;; rung 6 also needs each window's hours, for the burstiness term
        fit-bw (memoize (fn [cell t]
                          (let [{:keys [spec rows-before]} (data cell)]
                            (base/fit-bursts-weekly
                              (base/window-training (m/windows (rows-before t) spec) t (profile-at t) zone
                                                    (:fit-lookback-secs spec))))))
        fit-oo (memoize (fn [cell t]
                          (let [{:keys [spec rows-before]} (data cell)]
                            (base/fit-onoff
                              (base/block-training (m/windows (rows-before t) spec) t (profile-at t) zone spec
                                                   (:fit-lookback-secs spec))))))
        fit-dr (memoize (fn [cell t]
                          (let [{:keys [spec rows-before]} (data cell)]
                            (base/fit-drift
                              (base/training-blocks (m/hour-series (m/windows (rows-before t) spec) t) t (profile-at t) zone spec
                                                    (:fit-lookback-secs spec))))))
        _ (dorun (pmap (fn [[cell t rung]] (case rung :A (fit-a cell t) :W (fit-weekly cell t) :BW (fit-bw cell t)
                                             :OO (fit-oo cell t) :DR (fit-dr cell t) (fit-rung cell t rung)))
                       (for [[cell t] (distinct (map (juxt :cell :refit) targets)) rung [:A :W :BW :OO :DR 1 2 3 4]] [cell t rung])))
        rows (vec
               (apply concat
                      (pmap
                        (fn [{:keys [cell w i refit]}]
                          (let [{:keys [spec obs rows-before wins]} (data cell)
                                [_ wk] cell
                                {:keys [checkpoint-secs]} (plan wk)
                                final (m/cum-at w (inc (:eff-end w)))
                                capped? (boolean (:cap w))
                                wobs (window-obs obs spec w)
                                prior-finals (->> (subvec wins 0 i) (map :final) reverse (take 12)
                                                  (filter #(>= % 10.0)) vec)
                                prof (profile-at refit)]
                            (vec
                              (for [t (range (+ (:start w) checkpoint-secs) (- (m/live-end w) 300) checkpoint-secs)
                                    :let [x (pct-at wobs t)
                                          series (m/hour-series (m/windows (rows-before t) spec) t)
                                          [done cur] (base/blocks-so-far series (:start w) t prof zone spec)
                                          hist (base/training-blocks series t prof zone spec (:fit-lookback-secs spec))
                                          hist-done (if (seq cur) (pop hist) hist)
                                          ;; each model as a forecaster of the meter at `end`
                                          forecasters
                                          {"R0 linear" #(band-forecast (rate-projection wobs wk (assoc w :eff-end %) prior-finals t) x 3000 (hash [cell t %]))
                                           "R1 Poisson" #(base/forecast (fit-rung cell refit 1) prof zone spec t % x)
                                           "R2 +profile" #(base/forecast (fit-rung cell refit 2) prof zone spec t % x)
                                           "R3 +bursts (NB)" #(base/forecast (fit-rung cell refit 3) prof zone spec t % x)
                                           "R4 +recency (NB, 28d)" #(base/forecast (fit-rung cell refit 4) prof zone spec t % x)
                                           "R5 +weekly pace" #(base/forecast-weekly (fit-weekly cell refit) prof zone spec (:start w) t % x)
                                           "R6 +bursts +weekly pace" #(base/forecast-bursts-weekly (fit-bw cell refit) prof zone spec (:start w) t % x)
                                           "R7 +on/off blocks" #(base/forecast-onoff (fit-oo cell refit) prof zone spec done cur t % x)
                                           ;; rung 8 filters all history's blocks (the pace carries
                                           ;; across windows) up to the block in progress
                                           "R8 +drifting pace" #(base/forecast-drift (fit-dr cell refit) prof zone spec hist-done cur t % x)
                                           "A production" #(m/forecast (fit-a cell refit) (rows-before t) spec zone t % x :path? false)}
                                          ;; horizons: fixed look-aheads, and the reset; a look-ahead past a
                                          ;; window cut short by a granted reset has no outcome and is skipped
                                          horizons (for [h (conj (ladder-horizons wk) :reset)
                                                         :let [end (if (= h :reset) (:eff-end w) (+ t (* 3600 h)))]
                                                         :when (<= end (:eff-end w))]
                                                     [h end (if (= h :reset) final (min 100.0 (pct-at wobs end)))])]]
                                (into {:cell cell}
                                      (for [[label f] forecasters]
                                        [label (into {} (for [[h end y] horizons
                                                              :let [g (f end)
                                                                    capped-y? (>= y 100.0)
                                                                    in? (fn [lo hi] (if capped-y? (>= hi 99.0) (<= (- lo 1.0) y (+ hi 1.0))))]]
                                                          [h {:win i :week (quot (:start w) week) :hours (/ (- t (:start w)) 3600.0)
                                                              :crps (m/crps (:draws g) y 100.0)
                                                              :err (if capped-y? (max 0.0 (- 100.0 (:median g))) (Math/abs (- (:median g) y)))
                                                              :cov90 (in? (:lo g) (:hi g)) :cov50 (in? (:q25 g) (:q75 g))}]))]))))))
                        targets)))
        at (fn [h] (into {} (for [[cell cr] (group-by :cell rows)
                                  :let [cr (filter #(get-in % ["A production" h]) cr)]
                                  :when (seq cr)]
                              [cell (into {} (for [a ladder-arms] [a (mapv #(get-in % [a h]) cr)]))])))
        by-cell (into {} (for [[cell cr] (group-by :cell rows)]
                           [cell (into {} (for [a ladder-arms] [a (mapv #(get % a) cr)]))]))]
    (doseq [[cell arms] (sort-by key (at :reset))
            :let [base-rows (arms "A production")]]
      (println (format "\n%s %s  (%d checkpoints, %d windows; deltas vs A production)" (first cell) (name (second cell))
                       (count base-rows) (count (distinct (map :win base-rows)))))
      (println summary-header)
      (doseq [a ladder-arms]
        (println (summary-line a (arms a) base-rows (second cell)))))
    (doseq [h (ladder-horizons :seven-day)]
      (println (format "\n=== secondary: meter at +%dh ===" h))
      (print-pooled-7d (at h) ladder-arms "A production" :title (format "secondary: meter at +%dh" h)
                       :contrasts [["R7 +on/off blocks" "R6 +bursts +weekly pace"] ["R8 +drifting pace" "R7 +on/off blocks"]
                                   ["A production" "R8 +drifting pace"]]))
    (println "\n=== primary: meter at reset ===")
    (print-pooled-7d (at :reset) ladder-arms "A production"
                     :contrasts [["R1 Poisson" "R0 linear"] ["R2 +profile" "R1 Poisson"]
                                 ["R3 +bursts (NB)" "R2 +profile"] ["R4 +recency (NB, 28d)" "R3 +bursts (NB)"]
                                 ["R5 +weekly pace" "R2 +profile"] ["R6 +bursts +weekly pace" "R5 +weekly pace"]
                                 ["R6 +bursts +weekly pace" "R3 +bursts (NB)"] ["R7 +on/off blocks" "R6 +bursts +weekly pace"]
                                 ["R8 +drifting pace" "R7 +on/off blocks"] ["A production" "R8 +drifting pace"]
                                 ["A production" "R7 +on/off blocks"]])))

;; --- resolution check: does finer binning of the raw readings help? ---

(defn- binned
  "Usage per `step`-second bin over [start, end) from a window's raw readings
  (one sweep): [[bin-start count] ...]."
  [wobs start end step]
  (loop [b (* step (quot start step)) obs (seq wobs) prev 0.0 out (transient [])]
    (if (>= b end)
      (persistent! out)
      (let [bend (+ b step)
            [cur obs] (loop [cur prev obs obs]
                        (if (and obs (< (ffirst obs) bend))
                          (recur (max cur (second (first obs))) (next obs))
                          [cur obs]))]
        (recur bend obs cur (conj! out [b (Math/rint (- cur prev))]))))))

(defn run-resolution
  "Fit rungs 6 and 7 from the raw readings binned hourly vs every 15 minutes.
  Under the gamma process the forecast is invariant to bin size given the
  parameters; only the burstiness estimate (from within-bin dispersion)
  changes. Same refits, checkpoints, and estimands as run-ladder."
  [& {:keys [only max-refits]}]
  (let [now (quot (System/currentTimeMillis) 1000)
        zone (ZoneId/systemDefault)
        week 604800
        data (into {} (map (fn [c] [c (cell-data now c)]) cells))
        series-at (memoize (fn [cell t] ((:series (data cell)) t)))
        profile-at (memoize (fn [t] (m/fleet-harmonic-profile
                                      (into {} (for [c cells :let [s (series-at c t)] :when (seq s)] [c s]))
                                      zone)))
        refit-of (fn [w] (* week (quot (:start w) week)))
        targets (for [[cell d] data
                      :when (or (nil? only) (only cell))
                      :let [[_ wk] cell {:keys [min-train]} (plan wk)]
                      [i w] (map-indexed vector (:wins d))
                      :when (and (>= i min-train) (or (:cap w) (< (:eff-end w) (- now 3600))))]
                  {:cell cell :w w :i i :refit (refit-of w)})
        keep (when max-refits (set (take-last max-refits (sort (distinct (map :refit targets))))))
        targets (if keep (filter #(keep (:refit %)) targets) targets)
        ;; training windows as bins: [[mass count] ...] with block membership
        train (memoize (fn [cell t step]
                         (let [{:keys [spec obs wins]} (data cell)
                               prof (profile-at t)
                               lookback (:fit-lookback-secs spec)
                               per-hour (/ 3600.0 step)]
                           (vec (for [w wins
                                      :when (and (<= (:eff-end w) t) (or (nil? lookback) (>= (:start w) (- t lookback))))
                                      :let [bins (binned (window-obs obs spec w) (:start w) (:eff-end w) step)]
                                      :when (seq bins)]
                                  (mapv (fn [[b y]] [b (/ (nth prof (m/hour-of-week zone b)) per-hour) y]) bins))))))
        fit (memoize (fn [cell t step rung]
                       (let [spec (:spec (data cell))
                             wins (train cell t step)]
                         (if (= rung 6)
                           (base/fit-bursts-weekly
                             (mapv (fn [bins] {:mass (reduce + 0.0 (map second bins)) :total (reduce + 0.0 (map #(nth % 2) bins))
                                               :hours (mapv (fn [[_ a y]] [a y]) bins)})
                                   wins))
                           (base/fit-onoff
                             (mapv (fn [bins]
                                     {:blocks (reduce (fn [acc [b a y]]
                                                        (if (or (empty? acc) (and (zero? (mod b 3600)) (m/block-start? zone spec b)))
                                                          (conj acc [[a y]])
                                                          (conj (pop acc) (conj (peek acc) [a y]))))
                                                      [] bins)})
                                   wins))))))
        variants [["R6 hourly" 6 3600] ["R6 15-min" 6 900] ["R7 hourly" 7 3600] ["R7 15-min" 7 900]]
        _ (dorun (pmap (fn [[cell t rung step]] (fit cell t step rung))
                       (for [[cell t] (distinct (map (juxt :cell :refit) targets)) [_ rung step] variants] [cell t rung step])))
        rows (vec
               (apply concat
                      (pmap
                        (fn [{:keys [cell w i refit]}]
                          (let [{:keys [spec obs rows-before]} (data cell)
                                [_ wk] cell
                                {:keys [checkpoint-secs]} (plan wk)
                                final (m/cum-at w (inc (:eff-end w)))
                                capped? (boolean (:cap w))
                                wobs (window-obs obs spec w)
                                prof (profile-at refit)]
                            (vec
                              (for [t (range (+ (:start w) checkpoint-secs) (- (m/live-end w) 300) checkpoint-secs)
                                    :let [x (pct-at wobs t)
                                          in? (fn [lo hi] (if capped? (>= hi 99.0) (<= (- lo 1.0) final (+ hi 1.0))))
                                          score (fn [g] {:win i :week (quot (:start w) week) :hours (/ (- t (:start w)) 3600.0)
                                                         :crps (m/crps (:draws g) final 100.0)
                                                         :err (if capped? (max 0.0 (- 100.0 (:median g))) (Math/abs (- (:median g) final)))
                                                         :cov90 (in? (:lo g) (:hi g)) :cov50 (in? (:q25 g) (:q75 g))})
                                          series (m/hour-series (m/windows (rows-before t) spec) t)
                                          [done cur] (base/blocks-so-far series (:start w) t prof zone spec)]]
                                (into {:cell cell}
                                      (for [[label rung step] variants]
                                        [label (score (if (= rung 6)
                                                        (base/forecast-bursts-weekly (fit cell refit step 6) prof zone spec (:start w) t (:eff-end w) x)
                                                        (base/forecast-onoff (fit cell refit step 7) prof zone spec done cur t (:eff-end w) x)))]))))))
                        targets)))
        labels (mapv first variants)
        by-cell (into {} (for [[cell cr] (group-by :cell rows)]
                           [cell (into {} (for [a labels] [a (mapv #(get % a) cr)]))]))]
    (doseq [[cell arms] (sort-by key by-cell)
            :let [base-rows (arms "R6 hourly")]]
      (println (format "\n%s %s  (%d checkpoints, %d windows; deltas vs R6 hourly)" (first cell) (name (second cell))
                       (count base-rows) (count (distinct (map :win base-rows)))))
      (println summary-header)
      (doseq [a labels]
        (println (summary-line a (arms a) base-rows (second cell)))))
    (print-pooled-7d by-cell labels "R6 hourly"
                     :contrasts [["R6 15-min" "R6 hourly"] ["R7 15-min" "R7 hourly"]])
    (println "\nburstiness kappa (latest refit): hourly vs 15-min")
    (doseq [cell (sort (keys by-cell))
            :let [t (reduce max (map :refit (filter #(= cell (:cell %)) targets)))]]
      (println (format "  %-24s R6 %.3f vs %.3f   R7 %.3f vs %.3f" (str (first cell) " " (name (second cell)))
                       (:kappa (fit cell t 3600 6)) (:kappa (fit cell t 900 6))
                       (:kappa (fit cell t 3600 7)) (:kappa (fit cell t 900 7)))))))

;; --- continuous-time series on the ticks (cch.usage-ticks) ---

(def ^:private ct-series-arms
  "Continuous-time rungs, each beside its hourly counterpart and production."
  ["P0 Poisson process" "R1 Poisson (hourly)" "P1 +profile" "R2 +profile (hourly)"
   "P2 +weekly pace" "R5 +weekly pace (hourly)" "P3 sessions" "R7 +on/off blocks (hourly)"
   "P4 +drifting pace" "R8 +drifting pace (hourly)" "A production"])

(def ^:private ct-series-contrasts
  [["P0 Poisson process" "R1 Poisson (hourly)"] ["P1 +profile" "R2 +profile (hourly)"]
   ["P2 +weekly pace" "R5 +weekly pace (hourly)"]
   ["P1 +profile" "P0 Poisson process"] ["P2 +weekly pace" "P1 +profile"] ["P3 sessions" "P1 +profile"]
   ["P3 sessions" "R7 +on/off blocks (hourly)"] ["P4 +drifting pace" "P3 sessions"]
   ["P4 +drifting pace" "R8 +drifting pace (hourly)"] ["A production" "P4 +drifting pace"]])

(defn run-ct-series
  "Print the continuous-time tick-process series against its hourly
  counterparts and production, on the same refits, checkpoints, horizons,
  and estimands as run-ladder. `only` restricts cells; `max-refits` keeps
  the latest N refit weeks (small-data runs)."
  [& {:keys [only max-refits]}]
  (let [now (quot (System/currentTimeMillis) 1000)
        zone (ZoneId/systemDefault)
        week 604800
        span {:seven-day 604800 :five-hour 18000}
        data (into {} (map (fn [c] [c (cell-data now c)]) (remove #(= "agy" (first %)) cells)))
        series-at (memoize (fn [cell t] ((:series (data cell)) t)))
        profile-at (memoize (fn [t] (m/fleet-harmonic-profile
                                      (into {} (for [c (keys data) :let [s (series-at c t)] :when (seq s)] [c s]))
                                      zone)))
        refit-of (fn [w] (* week (quot (:start w) week)))
        targets (for [[cell d] data
                      :when (or (nil? only) (only cell))
                      :let [[_ wk] cell {:keys [min-train]} (plan wk)]
                      [i w] (map-indexed vector (:wins d))
                      :when (and (>= i min-train) (or (:cap w) (< (:eff-end w) (- now 3600))))]
                  {:cell cell :w w :i i :refit (refit-of w)})
        keep (when max-refits (set (take-last max-refits (sort (distinct (map :refit targets))))))
        targets (if keep (filter #(keep (:refit %)) targets) targets)
        ;; every cell's tick intervals once; what is known before t (within the
        ;; spec's lookback) is a prefix by the time each interval is revealed
        all-ivs (into {} (for [[cell {:keys [spec obs]}] data]
                           [cell (ticks/intervals obs (:cluster-secs spec) (span (second cell)))]))
        ivs-at (fn [cell t]
                 (let [lookback (:fit-lookback-secs (:spec (data cell)))]
                   (ticks/known-before (all-ivs cell) t (when lookback (- t lookback)))))
        fit-p0 (memoize (fn [cell t] (ticks/fit-p0 (ivs-at cell t))))
        fit-p1 (memoize (fn [cell t] (ticks/fit-p1 (ivs-at cell t) (profile-at t) zone)))
        fit-p2 (memoize (fn [cell t] (ticks/fit-p2 (ivs-at cell t) (profile-at t) zone)))
        ;; P3's session units from what is known before t
        units-at (fn [cell t prof] (ticks/mmpp-units (ivs-at cell t) prof zone))
        fit-p3 (memoize (fn [cell t] (ticks/fit-p3 (units-at cell t (profile-at t)))))
        fit-p4 (memoize (fn [cell t] (ticks/fit-p4 (units-at cell t (profile-at t)))))
        fit-r8 (memoize (fn [cell t]
                          (let [{:keys [spec rows-before]} (data cell)]
                            (base/fit-drift
                              (base/training-blocks (m/hour-series (m/windows (rows-before t) spec) t) t (profile-at t) zone spec
                                                    (:fit-lookback-secs spec))))))
        fit-r7 (memoize (fn [cell t]
                          (let [{:keys [spec rows-before]} (data cell)]
                            (base/fit-onoff
                              (base/block-training (m/windows (rows-before t) spec) t (profile-at t) zone spec
                                                   (:fit-lookback-secs spec))))))
        ;; R5's counterpart: finished quota windows as replicates
        fit-r5 (memoize (fn [cell t]
                          (let [{:keys [spec rows-before]} (data cell)
                                wins (m/windows (rows-before t) spec)
                                series (m/hour-series wins t)
                                prof (profile-at t)
                                lookback (:fit-lookback-secs spec)]
                            (base/fit-weekly
                              (vec (for [w wins
                                         :when (and (<= (:eff-end w) t) (or (nil? lookback) (>= (:start w) (- t lookback))))]
                                     [(reduce + 0.0 (for [[h [live _]] (subseq series >= (* 3600 (quot (:start w) 3600)) < (:eff-end w))]
                                                      (* live (nth prof (m/hour-of-week zone h)))))
                                      (m/cum-at w (inc (:eff-end w)))]))))))
        fit-r (memoize (fn [cell t rung]
                         (let [{:keys [spec rows-before]} (data cell)
                               series (m/hour-series (m/windows (rows-before t) spec) t)
                               lookback (:fit-lookback-secs spec)
                               series (if lookback (into (sorted-map) (filter #(>= (key %) (- t lookback)) series)) series)]
                           (base/fit rung (base/training-steps series (profile-at t) zone)))))
        fit-a (memoize (fn [cell t] (let [{:keys [spec rows-before]} (data cell)]
                                      (m/fit-model (rows-before t) spec zone t :profile-override (profile-at t)))))
        _ (dorun (pmap (fn [[cell t k]] (case k :p0 (fit-p0 cell t) :p1 (fit-p1 cell t) :p2 (fit-p2 cell t) :p3 (fit-p3 cell t) :p4 (fit-p4 cell t) :r7 (fit-r7 cell t) :r8 (fit-r8 cell t)
                                           :r1 (fit-r cell t 1) :r2 (fit-r cell t 2) :r5 (fit-r5 cell t) :a (fit-a cell t)))
                       (for [[cell t] (distinct (map (juxt :cell :refit) targets)) k [:p0 :p1 :p2 :p3 :p4 :r1 :r2 :r5 :r7 :r8 :a]] [cell t k])))
        rows (vec
               (apply concat
                      (pmap
                        (fn [{:keys [cell w i refit]}]
                          (let [{:keys [spec obs rows-before]} (data cell)
                                [_ wk] cell
                                {:keys [checkpoint-secs]} (plan wk)
                                final (m/cum-at w (inc (:eff-end w)))
                                wobs (window-obs obs spec w)
                                prof (profile-at refit)]
                            (vec
                              (for [t (range (+ (:start w) checkpoint-secs) (- (m/live-end w) 300) checkpoint-secs)
                                    :let [x (pct-at wobs t)
                                          ;; this calendar week's tick intervals so far (P2's pace update)
                                          ws (ticks/week-start zone t)
                                          week-ivs (filter #(>= (quot (+ (:t0 %) (:t1 %)) 2) ws)
                                                           (ticks/known-before (all-ivs cell) t (- ws 86400)))
                                          forecasters {"P0 Poisson process" #(ticks/forecast-p0 (fit-p0 cell refit) t % x)
                                                       "R1 Poisson (hourly)" #(base/forecast (fit-r cell refit 1) prof zone spec t % x)
                                                       "P1 +profile" #(ticks/forecast-p1 (fit-p1 cell refit) prof zone t % x)
                                                       "R2 +profile (hourly)" #(base/forecast (fit-r cell refit 2) prof zone spec t % x)
                                                       "P2 +weekly pace" #(ticks/forecast-p2 (fit-p2 cell refit) prof zone week-ivs t % x)
                                                       "R5 +weekly pace (hourly)" #(base/forecast-weekly (fit-r5 cell refit) prof zone spec (:start w) t % x)
                                                       "P3 sessions" #(ticks/forecast-p3 (fit-p3 cell refit) prof zone (units-at cell t prof) t % x)
                                                       "R7 +on/off blocks (hourly)" #(let [series (m/hour-series (m/windows (rows-before t) spec) t)
                                                                                           [done cur] (base/blocks-so-far series (:start w) t prof zone spec)]
                                                                                       (base/forecast-onoff (fit-r7 cell refit) prof zone spec done cur t % x))
                                                       "P4 +drifting pace" #(ticks/forecast-p4 (fit-p4 cell refit) prof zone (units-at cell t prof) t % x)
                                                       "R8 +drifting pace (hourly)" #(let [series (m/hour-series (m/windows (rows-before t) spec) t)
                                                                                           [_ cur] (base/blocks-so-far series (:start w) t prof zone spec)
                                                                                           hist (base/training-blocks series t prof zone spec (:fit-lookback-secs spec))
                                                                                           done (if (seq cur) (pop hist) hist)]
                                                                                       (base/forecast-drift (fit-r8 cell refit) prof zone spec done cur t % x))
                                                       "A production" #(m/forecast (fit-a cell refit) (rows-before t) spec zone t % x :path? false)}
                                          horizons (for [h (conj (ladder-horizons wk) :reset)
                                                         :let [end (if (= h :reset) (:eff-end w) (+ t (* 3600 h)))]
                                                         :when (<= end (:eff-end w))]
                                                     [h end (if (= h :reset) final (min 100.0 (pct-at wobs end)))])]]
                                (into {:cell cell}
                                      (for [[label f] forecasters]
                                        [label (into {} (for [[h end y] horizons
                                                              :let [g (f end)
                                                                    capped-y? (>= y 100.0)
                                                                    in? (fn [lo hi] (if capped-y? (>= hi 99.0) (<= (- lo 1.0) y (+ hi 1.0))))]]
                                                          [h {:win i :week (quot (:start w) week) :hours (/ (- t (:start w)) 3600.0)
                                                              :crps (m/crps (:draws g) y 100.0)
                                                              :err (if capped-y? (max 0.0 (- 100.0 (:median g))) (Math/abs (- (:median g) y)))
                                                              :cov90 (in? (:lo g) (:hi g)) :cov50 (in? (:q25 g) (:q75 g))}]))]))))))
                        targets)))
        at (fn [h] (into {} (for [[cell cr] (group-by :cell rows)
                                  :let [cr (filter #(get-in % ["A production" h]) cr)]
                                  :when (seq cr)]
                              [cell (into {} (for [a ct-series-arms] [a (mapv #(get-in % [a h]) cr)]))])))]
    (doseq [[cell arms] (sort-by key (at :reset))
            :let [base-rows (arms "A production")]]
      (println (format "\n%s %s  (%d checkpoints, %d windows; deltas vs A production)" (first cell) (name (second cell))
                       (count base-rows) (count (distinct (map :win base-rows)))))
      (println summary-header)
      (doseq [a ct-series-arms]
        (println (summary-line a (arms a) base-rows (second cell)))))
    (doseq [h (ladder-horizons :seven-day)]
      (print-pooled-7d (at h) ct-series-arms "A production" :title (format "secondary: meter at +%dh" h)
                       :contrasts ct-series-contrasts))
    (print-pooled-7d (at :reset) ct-series-arms "A production" :contrasts ct-series-contrasts)))
