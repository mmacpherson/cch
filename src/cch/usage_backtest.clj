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
            [cch.usage-model :as m])
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
   ["two-level 12/12" [:two-level 12] 12] ["two-level 36/12" [:two-level 12] 36]])

(defn- variant-profile
  "Smoothed profile for `cell` at time t under `variant`; nil = independent."
  [stats-at cell t [_ target k]]
  (when target
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
        (if (Double/isInfinite (double k)) target-rates (m/shrunk-rates own target-rates k))))))

(defn- cell-data [zone now [agent window-key :as cell]]
  (let [obs (raw-observations agent window-key)
        {:keys [rows-before]} (history obs)
        spec (m/specs window-key)]
    {:cell cell :spec spec :obs obs :rows-before rows-before
     :wins (m/windows (rows-before now) spec)
     :stats (memoize (fn [t] (m/bin-stats (m/hour-series (m/windows (rows-before t) spec) t) zone)))}))

(defn- replay-variant
  [{:keys [cell spec obs rows-before wins]} stats-at variant now zone]
  (let [[_ window-key] cell
        {:keys [checkpoint-secs refit min-train]} (plan window-key)
        done? (fn [w] (or (:cap w) (< (:eff-end w) (- now 3600))))
        refit-time (fn [w] (case refit
                             :per-window (:start w)
                             :weekly (* 604800 (quot (:start w) 604800))))
        fit-at (memoize (fn [t] (m/fit-model (rows-before t) spec zone t
                                             :profile-override (variant-profile stats-at cell t variant))))]
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
  "Print the profile pooling experiment for every agent/window with history."
  [& _]
  (let [now (quot (System/currentTimeMillis) 1000)
        zone (ZoneId/systemDefault)
        data (into {} (map (fn [c] [c (cell-data zone now c)]) cells))
        stats-at (fn [cell t] (when-let [d (data cell)] ((:stats d) t)))]
    (doseq [cell cells
            :let [d (data cell)
                  results (into {} (pmap (fn [v] [(first v) (replay-variant d stats-at v now zone)]) variants))
                  base (results "independent")]
            :when (seq base)]
      (println (format "\n%s %s  (%d checkpoints, %d windows)" (first cell) (name (second cell))
                       (count base) (count (distinct (map :win base)))))
      (println (format "  %-16s %7s %16s %6s %6s %6s %7s" "variant" "CRPS" "dCRPS (+-se)" "MAE" "cov50" "cov90" "Brier"))
      (doseq [[label] variants
              :let [rows (results label)
                    {:keys [delta se]} (paired-delta rows base)
                    frac (fn [k] (* 100.0 (mean (map #(if (k %) 1.0 0.0) rows))))]]
        (println (format "  %-16s %7.2f %+8.2f (%.2f) %6.1f %5.0f%% %5.0f%% %7.4f"
                         label (mean (map :crps rows)) delta se (mean (map :err rows))
                         (frac :cov50) (frac :cov90) (mean (map :brier rows))))))))
