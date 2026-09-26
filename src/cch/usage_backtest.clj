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
            [cch.projections :as proj]
            [cch.usage-model :as m]
            [cch.usage-stan :as stan]
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
   ["fleet harmonic K=4" :harmonic 4]])

(def ^:private focus-variants
  "The comparison that decides adoption (claude-code-hooks-20r)."
  #{"independent" "fleet EB k" "fleet harmonic K=4"})

(defn- variant-profile
  "Smoothed profile for `cell` at time t under `variant`; nil = independent.
  k = :eb estimates the shrinkage strength per cell (eb-rates); target
  :harmonic fits one fleet shape in the weekday + daily-harmonic basis."
  [{:keys [stats-at weekly-at]} cell t [_ target k]]
  (cond
    (= target :harmonic)
    (m/harmonic-profile (keep #(stats-at % t) cells) k)

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
        {:win i
         :crps (m/crps (:draws g) final 100.0)
         :err (if capped? (max 0.0 (- 100.0 (:median g))) (Math/abs (- (:median g) final)))
         :cov50 (in? (:q25 g) (:q75 g))
         :cov90 (in? (:lo g) (:hi g))
         :brier (Math/pow (- (:p-cap g) (if capped? 1.0 0.0)) 2)}))))

(defn- mean [xs] (if (seq xs) (/ (reduce + 0.0 xs) (count xs)) Double/NaN))

(defn- paired-delta
  "Mean CRPS difference vs `base` rows (same checkpoints), with a standard
  error clustered by window (checkpoints within a window are correlated)."
  [rows base]
  (let [by-win (->> (map (fn [r b] [(:win r) (- (:crps r) (:crps b))]) rows base)
                    (group-by first)
                    vals
                    (map #(mean (map second %))))
        n (count by-win)
        m (mean by-win)
        sd (Math/sqrt (/ (reduce + (map #(Math/pow (- % m) 2) by-win)) (max 1 (dec n))))]
    {:delta (/ (reduce + (map #(- (:crps %1) (:crps %2)) rows base)) (max 1 (count rows)))
     :se (/ sd (Math/sqrt (max 1 n)))}))

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
    (doseq [cell cells
            :let [d (data cell)
                  results (into {} (pmap (fn [v] [(first v) (replay-variant d access v now zone)]) chosen))
                  base-rows (results base)]
            :when (seq base-rows)]
      (println (format "\n%s %s  (%d checkpoints, %d windows)" (first cell) (name (second cell))
                       (count base-rows) (count (distinct (map :win base-rows)))))
      (println (format "  %-16s %7s %16s %6s %6s %6s %7s" "variant" "CRPS" "dCRPS (+-se)" "MAE" "cov50" "cov90" "Brier"))
      (doseq [[label] chosen
              :let [rows (results label)
                    {:keys [delta se]} (paired-delta rows base-rows)
                    frac (fn [k] (* 100.0 (mean (map #(if (k %) 1.0 0.0) rows))))]]
        (println (format "  %-16s %7.2f %+8.2f (%.2f) %6.1f %5.0f%% %5.0f%% %7.4f"
                         label (mean (map :crps rows)) delta se (mean (map :err rows))
                         (frac :cov50) (frac :cov90) (mean (map :brier rows))))))))

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
                 {:cell cell :win i
                  "independent" (score (m/forecast (fit-at cell refit false) rows spec zone t (:eff-end w) x :path? false))
                  "fleet EB k" (score (m/forecast (fit-at cell refit true) rows spec zone t (:eff-end w) x :path? false))
                  "stan" (score (stan/mixture-forecast (take draws fits) rows spec zone t (:eff-end w) x))}))]
    (doseq [[cell cell-rows] (sort-by key (group-by :cell rows))
            :let [base (mapv #(get % "independent") cell-rows)
                  wins (mapv :win cell-rows)]]
      (println (format "\n%s %s  (%d checkpoints, %d windows)" (first cell) (name (second cell))
                       (count cell-rows) (count (distinct wins))))
      (println (format "  %-12s %7s %16s %6s %6s %6s %7s" "variant" "CRPS" "dCRPS (+-se)" "MAE" "cov50" "cov90" "Brier"))
      (doseq [label ["independent" "fleet EB k" "stan"]
              :let [scored (mapv #(assoc (get % label) :win (:win %)) cell-rows)
                    {:keys [delta se]} (paired-delta scored (mapv #(assoc %1 :win %2) base wins))
                    frac (fn [k] (* 100.0 (mean (map #(if (k %) 1.0 0.0) scored))))]]
        (println (format "  %-12s %7.2f %+8.2f (%.2f) %6.1f %5.0f%% %5.0f%% %7.4f"
                         label (mean (map :crps scored)) delta se (mean (map :err scored))
                         (frac :cov50) (frac :cov90) (mean (map :brier scored))))))))

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
        {:win i :hours (/ (- t (:start w)) 3600.0)
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
      (println (format "  %-15s %7s %16s %6s %6s %6s %7s" "arm" "CRPS" "dCRPS (+-se)" "MAE" "cov50" "cov90" "Brier"))
      (doseq [[label] arms
              :let [rows (results label)
                    {:keys [delta se]} (paired-delta rows base)
                    frac (fn [k] (* 100.0 (mean (map #(if (k %) 1.0 0.0) rows))))]]
        (println (format "  %-15s %7.2f %+8.2f (%.2f) %6.1f %5.0f%% %5.0f%% %7.4f"
                         label (mean (map :crps rows)) delta se (mean (map :err rows))
                         (frac :cov50) (frac :cov90) (mean (map :brier rows)))))
      (let [span (if (= wk :seven-day) 24.0 1.0)
            bucket #(min 5 (int (Math/ceil (/ (:hours %) span))))]
        (println (format "  CRPS by elapsed %s:" (if (= wk :seven-day) "day" "hour")))
        (doseq [[label] arms
                :let [by (group-by bucket (results label))]]
          (println (format "    %-15s %s" label
                           (str/join "  " (for [k (sort (keys by))]
                                            (format "%s:%.2f" (if (= k 5) "5+" k) (mean (map :crps (by k)))))))))))))
