(ns cch.forecast
  "Statusline data layer.

  Pulls 5h and 7d rate-limit observations from context_snapshots and
  hands them to cch.projections for Bayesian forecasts. The /forecast
  endpoint serves what statusline-command.sh consumes:

      5h: 60% · ~67% · 22m
      7d: 36% · ~94% · 4d 8h

  All filtering (drop-stale monotone pass, time-bucket thinning) is
  pushed into SQLite via window functions so only the clean subset
  crosses the process boundary."
  (:require [cch.db :as db]
            [cch.log :as log]
            [cch.projections :as proj])
  (:import (java.time Instant)))

;; --- Query building ---

(def ^:private window-sql-path
  {:seven-day "seven_day"
   :five-hour "five_hour"})

(def ^:private bucket-secs
  {:seven-day 360
   :five-hour 60})

(def ^:private span-secs
  {:seven-day (* 7 86400)
   :five-hour (* 5 3600)})

(defn- latest-resets-at
  "The most recent resets_at for a given window."
  [window-key]
  (let [wpath (window-sql-path window-key)
        sql   (format
                (str "SELECT json_extract(payload, '$.rate_limits.%s.resets_at') AS resets_at "
                     "FROM context_snapshots "
                     "WHERE json_extract(payload, '$.rate_limits.%s.resets_at') IS NOT NULL "
                     "ORDER BY id DESC LIMIT 1")
                wpath wpath)]
    (some-> (db/query sql) first :resets_at long)))

(defn- filtered-samples
  "Fetch rate-limit samples for `window-key` since `since-iso`, with
   drop-stale and time-bucket thinning done in SQL via window functions."
  [since-iso window-key]
  (let [wpath  (window-sql-path window-key)
        bucket (bucket-secs window-key)
        sql    (format
                 (str
                   "WITH samples AS ("
                   "  SELECT"
                   "    CAST(strftime('%%s', timestamp) AS INTEGER) AS ts,"
                   "    json_extract(payload, '$.rate_limits.%s.used_percentage') AS pct,"
                   "    json_extract(payload, '$.rate_limits.%s.resets_at') AS resets_at"
                   "  FROM context_snapshots"
                   "  WHERE timestamp >= '%s'"
                   "    AND json_extract(payload, '$.rate_limits.%s.used_percentage') IS NOT NULL"
                   "    AND session_id NOT LIKE 'test%%'"
                   "  ORDER BY timestamp ASC"
                   "), "
                   "fresh AS ("
                   "  SELECT *,"
                   "    MAX(pct) OVER ("
                   "      PARTITION BY resets_at"
                   "      ORDER BY ts"
                   "      ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING"
                   "    ) AS prev_max"
                   "  FROM samples"
                   "), "
                   "monotone AS ("
                   "  SELECT * FROM fresh"
                   "  WHERE pct >= COALESCE(prev_max, 0)"
                   "), "
                   "bucketed AS ("
                   "  SELECT *,"
                   "    ROW_NUMBER() OVER ("
                   "      PARTITION BY resets_at, ts / %d"
                   "      ORDER BY ts"
                   "    ) AS bucket_rn"
                   "  FROM monotone"
                   ") "
                   "SELECT ts, pct, resets_at FROM bucketed"
                   " WHERE bucket_rn = 1"
                   " ORDER BY ts")
                 wpath wpath since-iso wpath bucket)]
    (some->> (db/query sql)
             (mapv (fn [{:keys [ts pct resets_at]}]
                     {:ts        (long ts)
                      :pct       (double pct)
                      :resets-at (long resets_at)})))))

(defn- raw-sample-count
  "Total row count before filtering — for the /usage page's sample display."
  [since-iso window-key]
  (let [wpath (window-sql-path window-key)
        sql   (format
                (str "SELECT COUNT(*) AS n FROM context_snapshots"
                     " WHERE timestamp >= '%s'"
                     " AND json_extract(payload, '$.rate_limits.%s.used_percentage') IS NOT NULL"
                     " AND session_id NOT LIKE 'test%%'")
                since-iso wpath)]
    (some-> (db/query sql) first :n long)))

(defn- epoch->iso [secs]
  (str (java.time.Instant/ofEpochSecond secs)))

(defn- rate-chart-samples
  "Fine-grained samples for the burn-rate chart: 60-second buckets,
   monotone-filtered, scoped to the exact current resets_at epoch so
   cross-window contamination is impossible."
  [resets-at]
  (let [sql (format
              (str "WITH samples AS ("
                   "  SELECT CAST(strftime('%%s', timestamp) AS INTEGER) AS ts,"
                   "    CAST(json_extract(payload, '$.rate_limits.seven_day.used_percentage') AS REAL) AS pct"
                   "  FROM context_snapshots"
                   "  WHERE json_extract(payload, '$.rate_limits.seven_day.resets_at') = %d"
                   "    AND json_extract(payload, '$.rate_limits.seven_day.used_percentage') IS NOT NULL"
                   "    AND session_id NOT LIKE 'test%%'"
                   "  ORDER BY timestamp ASC"
                   "), fresh AS ("
                   "  SELECT *,"
                   "    MAX(pct) OVER (ORDER BY ts ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING) AS prev_max"
                   "  FROM samples"
                   "), monotone AS ("
                   "  SELECT * FROM fresh WHERE pct >= COALESCE(prev_max, 0)"
                   "), bucketed AS ("
                   "  SELECT *, ROW_NUMBER() OVER (PARTITION BY ts / 60 ORDER BY ts) AS rn"
                   "  FROM monotone"
                   ") SELECT ts, pct FROM bucketed WHERE rn = 1 ORDER BY ts")
              resets-at)]
    (some->> (db/query sql)
             (mapv (fn [{:keys [ts pct]}]
                     {:ts (long ts) :pct (double pct)})))))

(defn- rate-5h-samples
  "60s-bucketed five-hour window samples for the full 7d span.
   Monotone filter is partitioned per resets_at so each 5h window is
   treated independently. Returns :resets-at so the chart can avoid
   spanning a reset boundary when computing a lookback-window rate."
  [since-iso]
  (let [sql (format
              (str "WITH samples AS ("
                   "  SELECT CAST(strftime('%%s', timestamp) AS INTEGER) AS ts,"
                   "    CAST(json_extract(payload, '$.rate_limits.five_hour.used_percentage') AS REAL) AS pct,"
                   "    json_extract(payload, '$.rate_limits.five_hour.resets_at') AS resets_at"
                   "  FROM context_snapshots"
                   "  WHERE timestamp >= '%s'"
                   "    AND json_extract(payload, '$.rate_limits.five_hour.used_percentage') IS NOT NULL"
                   "    AND session_id NOT LIKE 'test%%'"
                   "  ORDER BY timestamp ASC"
                   "), fresh AS ("
                   "  SELECT *,"
                   "    MAX(pct) OVER (PARTITION BY resets_at ORDER BY ts"
                   "      ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING) AS prev_max"
                   "  FROM samples"
                   "), monotone AS ("
                   "  SELECT * FROM fresh WHERE pct >= COALESCE(prev_max, 0)"
                   "), bucketed AS ("
                   "  SELECT *, ROW_NUMBER() OVER (PARTITION BY resets_at, ts / 60 ORDER BY ts) AS rn"
                   "  FROM monotone"
                   ") SELECT ts, pct, resets_at FROM bucketed WHERE rn = 1 ORDER BY ts")
              since-iso)]
    (some->> (db/query sql)
             (mapv (fn [{:keys [ts pct resets_at]}]
                     {:ts (long ts) :pct (double pct) :resets-at (long resets_at)})))))

(defn- build-current-window []
  (when-let [resets-at (latest-resets-at :seven-day)]
    (let [window-start (- resets-at (* 7 86400))
          in-window    (filtered-samples (epoch->iso window-start) :seven-day)
          now          (-> (Instant/now) .getEpochSecond)
          last-pct     (:pct (last in-window))
          window-info  {:now now :resets-at resets-at
                        :window-start window-start :last-pct last-pct}
          obs-pairs    (mapv #(select-keys % [:ts :pct]) in-window)
          projs        (proj/all-projections obs-pairs window-info)]
      (when (and last-pct (seq projs))
        {:observed        obs-pairs
         :rate-5h-samples (rate-5h-samples (epoch->iso window-start))
         :resets-at       resets-at
         :window-start    window-start
         :now             now
         :last-pct        last-pct
         :samples         (or (raw-sample-count (epoch->iso window-start) :seven-day) 0)
         :projections     projs}))))

;; Cache current-window for 30s — data arrives at most once per minute
;; via the statusLine hook, so re-running 4 queries + LOESS + projections
;; on every browser hit is pure waste.
(def ^:private window-cache (atom {:ts 0 :data nil}))

(defn current-window
  "Data bundle for the /usage page. Cached for 30 s."
  []
  (let [{:keys [ts data]} @window-cache
        now (-> (Instant/now) .getEpochSecond)]
    (if (< (- now ts) 30)
      data
      (let [fresh (build-current-window)]
        (reset! window-cache {:ts now :data fresh})
        fresh))))

(def ^:private window-config
  {:seven-day {:prior-mu 0.55 :prior-sigma 0.045}
   :five-hour {:prior-mu 15.0 :prior-sigma 8.0}})

;; pct is integer-quantized (1% resolution). At ~0.6%/hr aggregate, a 10-min
;; window almost never captures a tick. 20 min gives enough span to see
;; movement at normal rates while staying reactive to bursts. Compare newest
;; vs oldest raw sample across all concurrent sessions.
(def ^:private burn-lookback-secs 1200)

;; Empirical Δ7d/Δ5h co-movement ratio (490k sample pairs). The 5h window
;; ticks ~7.5x more often (proportional to its smaller token budget), so we
;; weight it 7.5x more heavily in the fused estimate.
(def ^:private scale-5h->7d 0.133)
(def ^:private weight-5h    7.5)

(defn- recent-burn-rate-phr
  "Observed burn rate in %/hr over the past ~20 min of raw samples.
   Compares newest vs oldest pct within the window across all concurrent
   sessions. Returns 0.0 when idle, nil when fewer than 2 samples."
  [wpath resets-at now]
  (let [cutoff-iso (epoch->iso (- now burn-lookback-secs))
        sql        (format
                     (str "SELECT CAST(strftime('%%s', timestamp) AS INTEGER) AS ts,"
                          "  json_extract(payload, '$.rate_limits.%s.used_percentage') AS pct"
                          " FROM context_snapshots"
                          " WHERE timestamp >= '%s'"
                          "   AND json_extract(payload, '$.rate_limits.%s.resets_at') = %d"
                          "   AND json_extract(payload, '$.rate_limits.%s.used_percentage') IS NOT NULL"
                          "   AND session_id NOT LIKE 'test%%'"
                          " ORDER BY ts ASC")
                     wpath cutoff-iso wpath resets-at wpath)
        rows       (db/query sql)]
    (when (>= (count rows) 2)
      (let [oldest    (first rows)
            newest    (last rows)
            elapsed-s (- (:ts newest) (:ts oldest))]
        (when (>= elapsed-s 60)
          (/ (- (:pct newest) (:pct oldest))
             (/ elapsed-s 3600.0)))))))

;; --- Empirical Bayes: learned prior from completed windows ---

;; Exponential decay applied across completed weeks — most-recent week has
;; weight 1, each older week is multiplied by this factor.
(def ^:private prior-decay-lambda 0.85)

;; Floor on σ so that a handful of nearly-identical weeks don't collapse
;; the prior to a spike and overwhelm the within-week likelihood.
(def ^:private prior-sigma-floor 0.03)

(defn weighted-prior-params
  "Pure fn: given a seq of completed-window rows [{:final_pct N} ...] ordered
   newest-first, returns {:mu :sigma} in %/hr units using exponentially-decayed
   weights (most-recent weight = 1, each older week × prior-decay-lambda).
   Returns nil when fewer than 2 windows are supplied."
  [rows]
  (when (>= (count rows) 2)
    (let [rates (mapv #(/ (double (:final_pct %)) (* 7.0 24.0)) rows)
          ws    (mapv #(Math/pow prior-decay-lambda %) (range (count rates)))
          sw    (reduce + 0.0 ws)
          mu    (/ (reduce + 0.0 (map * ws rates)) sw)
          ;; Bessel-corrected weighted variance
          sw2   (reduce + 0.0 (map #(* % %) ws))
          var   (/ (reduce + 0.0 (map (fn [w r] (* w (Math/pow (- r mu) 2.0))) ws rates))
                   (- sw (/ sw2 sw)))
          sigma (max prior-sigma-floor (Math/sqrt var))]
      {:mu mu :sigma sigma})))

(defn- learned-prior
  "Query completed 7-day windows (newest-first, up to 12) and derive an
   empirical Bayes prior via weighted-prior-params. Returns nil during the
   first week when there is no history."
  []
  (let [sql (str "SELECT MAX(CAST(json_extract(payload,'$.rate_limits.seven_day.used_percentage') AS REAL))"
                 "  AS final_pct"
                 " FROM context_snapshots"
                 " WHERE json_extract(payload,'$.rate_limits.seven_day.resets_at') < strftime('%s','now')"
                 "   AND json_extract(payload,'$.rate_limits.seven_day.used_percentage') IS NOT NULL"
                 "   AND session_id NOT LIKE 'test%'"
                 " GROUP BY json_extract(payload,'$.rate_limits.seven_day.resets_at')"
                 " ORDER BY json_extract(payload,'$.rate_limits.seven_day.resets_at') DESC"
                 " LIMIT 12")]
    (weighted-prior-params (db/query sql))))

(defn- fused-burn-rate-7d
  "Inverse-variance weighted fusion of 7d-direct and 5h-scaled burn rates,
   both in 7d-percent/hr. 5h weight = 7.5 (proportional to its tick frequency)."
  [resets-at-7d now]
  (let [r7d  (some-> (recent-burn-rate-phr "seven_day" resets-at-7d now) (max 0.0))
        r5at (latest-resets-at :five-hour)
        r5h  (when r5at
               (some-> (recent-burn-rate-phr "five_hour" r5at now)
                       (* scale-5h->7d)
                       (max 0.0)))]
    (cond
      (and r7d r5h) (/ (+ r7d (* weight-5h r5h)) (+ 1.0 weight-5h))
      r5h           r5h
      r7d           r7d)))

(defn- compute-bayes-stats
  "Bayesian projection for one window."
  [window-key]
  (when-let [resets-at (latest-resets-at window-key)]
    (let [base-cfg              (window-config window-key)
          learned               (when (= window-key :seven-day) (learned-prior))
          {:keys [prior-mu prior-sigma]} (merge base-cfg learned)
          wpath        (window-sql-path window-key)
          window-start (- resets-at (span-secs window-key))
          in-window    (filtered-samples (epoch->iso window-start) window-key)
          now          (-> (Instant/now) .getEpochSecond)
          last-pct     (:pct (last in-window))
          window-info  {:now now :resets-at resets-at
                        :window-start window-start :last-pct last-pct
                        :prior-mu prior-mu :prior-sigma prior-sigma}
          obs-pairs    (mapv #(select-keys % [:ts :pct]) in-window)
          proj-result  (when last-pct (proj/bayes-projection obs-pairs window-info))
          local-rate   (if (= window-key :seven-day)
                         (fused-burn-rate-7d resets-at now)
                         (recent-burn-rate-phr wpath resets-at now))]
      (when last-pct
        (let [raw-proj (or (:proj proj-result) last-pct)]
          (cond-> {:current_pct   (Math/round last-pct)
                   :projected_pct (Double/parseDouble (format "%.1f" raw-proj))
                   :secs_left     (max 0 (- resets-at now))}
            (some? local-rate) (assoc :local_rate_phr (Double/parseDouble (format "%.1f" local-rate)))))))))

(def ^:private forecast-cache (atom nil))
(def ^:private bg-thread (atom nil))

(defn- latest-snapshot-id []
  (some-> (db/query "SELECT MAX(id) AS id FROM context_snapshots") first :id))

(defn- do-refresh! []
  (let [fresh {:five_hour (compute-bayes-stats :five-hour)
               :seven_day (compute-bayes-stats :seven-day)}]
    (reset! forecast-cache fresh)))

(defn start-bg-refresh!
  "Start a background thread that polls for new context_snapshots every
   `interval-ms` ms. If the DB's max(id) has advanced since the last
   compute, recompute and update the cache atom; otherwise sleep again.
   Computes once immediately on startup to seed the cache."
  [& {:keys [interval-ms] :or {interval-ms 10000}}]
  (let [last-id (atom nil)
        t (Thread.
            (fn []
              (try (do-refresh!) (catch Exception _))
              (loop []
                (when-not (.isInterrupted (Thread/currentThread))
                  (try
                    (let [lid (latest-snapshot-id)]
                      (when (and lid (not= lid @last-id))
                        (reset! last-id lid)
                        (do-refresh!)))
                    (catch Exception _))
                  (try (Thread/sleep (long interval-ms))
                       (catch InterruptedException _))
                  (recur)))))]
    (.setDaemon t true)
    (.start t)
    (reset! bg-thread t)))

(defn stop-bg-refresh! []
  (when-let [t @bg-thread]
    (.interrupt t)
    (reset! bg-thread nil)))

(defn statusline-stats
  "Current forecast bundle for the statusLine. Sub-millisecond read —
   all computation runs in the background thread started by start-bg-refresh!."
  []
  @forecast-cache)
