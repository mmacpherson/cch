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
            [cch.projections :as proj]
            [clojure.core.async :as async])
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

(def ^:private prior-decay-lambda 0.85)

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
          sw2   (reduce + 0.0 (map #(* % %) ws))
          var   (/ (reduce + 0.0 (map (fn [w r] (* w (Math/pow (- r mu) 2.0))) ws rates))
                   (- sw (/ sw2 sw)))
          sigma (max prior-sigma-floor (Math/sqrt var))]
      {:mu mu :sigma sigma})))

(def ^:private historical-finals-sql
  (str "SELECT final_pct FROM ("
       "  SELECT MAX(CAST(json_extract(payload,'$.rate_limits.seven_day.used_percentage') AS REAL))"
       "    AS final_pct"
       "  FROM context_snapshots"
       "  WHERE json_extract(payload,'$.rate_limits.seven_day.resets_at') < strftime('%s','now')"
       "    AND json_extract(payload,'$.rate_limits.seven_day.used_percentage') IS NOT NULL"
       "    AND session_id NOT LIKE 'test%'"
       "  GROUP BY json_extract(payload,'$.rate_limits.seven_day.resets_at')"
       "  ORDER BY json_extract(payload,'$.rate_limits.seven_day.resets_at') DESC"
       "  LIMIT 12"
       ") WHERE final_pct >= 10"))

(defn- historical-final-pcts
  "Final used_percentage for each completed 7-day window, newest-first, up to 12."
  []
  (some->> (db/query historical-finals-sql)
           (mapv #(double (:final_pct %)))))

(defn- learned-prior
  "Derive an empirical Bayes prior (μ/σ in %/hr) from completed windows.
   Returns nil during the first week when there is no history."
  []
  (some->> (db/query historical-finals-sql)
           (weighted-prior-params)))

(defn- build-current-window []
  (when-let [raw-resets-at (latest-resets-at :seven-day)]
    (let [now          (-> (Instant/now) .getEpochSecond)
          ;; After a weekly reset the DB still has the old resets_at until
          ;; Claude Code posts a fresh snapshot. Shift to the new window.
          resets-at    (if (<= raw-resets-at now)
                         (+ raw-resets-at (* 7 86400))
                         raw-resets-at)
          window-start (- resets-at (* 7 86400))
          raw-samples  (filtered-samples (epoch->iso window-start) :seven-day)
          ;; Stale sessions cache old rate-limit state and keep reporting
          ;; the expired resets_at with high pct values for hours after a
          ;; reset. Only keep samples tagged with the current resets_at.
          in-window    (filterv #(= (:resets-at %) resets-at) raw-samples)
          last-pct     (or (:pct (last in-window)) 0.0)
          hist-finals  (historical-final-pcts)
          window-info  {:now now :resets-at resets-at
                        :window-start window-start :last-pct last-pct
                        :historical-finals hist-finals}
          obs-pairs    (mapv #(select-keys % [:ts :pct]) in-window)
          projs        (proj/all-projections obs-pairs window-info)
          rs           (proj/rate-samples obs-pairs)
          recent-rate  (when (>= (count rs) 2)
                         (let [recent (take-last 3 rs)]
                           (/ (reduce + 0.0 (map :rate recent))
                              (count recent))))]
      {:observed        obs-pairs
       :rate-5h-samples (rate-5h-samples (epoch->iso window-start))
       :resets-at       resets-at
       :window-start    window-start
       :now             now
       :last-pct        last-pct
       :samples         (or (raw-sample-count (epoch->iso window-start) :seven-day) 0)
       :projections     (or projs [])
       :rate-phr        recent-rate})))

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

(defn- compute-window-stats
  "Assemble observations for `window-key`, run `proj-fn` to get the
   forward projection, and return the statusLine stats map.
   `proj-fn` must satisfy `[observed window-info] → {:proj ...} | nil`."
  [window-key proj-fn]
  (when-let [raw-resets-at (latest-resets-at window-key)]
    (let [now          (-> (Instant/now) .getEpochSecond)
          span         (span-secs window-key)
          resets-at    (if (<= raw-resets-at now)
                         (+ raw-resets-at span)
                         raw-resets-at)
          base-cfg     (window-config window-key)
          learned      (when (= window-key :seven-day) (learned-prior))
          {:keys [prior-mu prior-sigma]} (merge base-cfg learned)
          wpath        (window-sql-path window-key)
          window-start (- resets-at span)
          raw-samples  (filtered-samples (epoch->iso window-start) window-key)
          in-window    (filterv #(= (:resets-at %) resets-at) raw-samples)
          last-pct     (:pct (last in-window))
          hist-finals  (when (= window-key :seven-day) (historical-final-pcts))
          window-info  {:now now :resets-at resets-at
                        :window-start window-start :last-pct last-pct
                        :prior-mu prior-mu :prior-sigma prior-sigma
                        :historical-finals hist-finals}
          obs-pairs    (mapv #(select-keys % [:ts :pct]) in-window)
          proj-result  (when last-pct (proj-fn obs-pairs window-info))
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
(def ^:private signal-ch-ref (atom nil))

(defn signal-new-data!
  "Notify the bg thread that a new context snapshot has arrived.
   dropping-buffer 1 means concurrent signals coalesce — at most one
   wakeup is queued regardless of how many sessions post at once."
  []
  (when-let [ch @signal-ch-ref]
    (async/put! ch :signal)))

(defn- do-refresh! []
  (reset! forecast-cache
          {:five_hour  (compute-window-stats :five-hour  proj/bayes-projection)
           :seven_day  (compute-window-stats :seven-day  proj/bayes-projection)}))

(defn start-bg-refresh!
  "Start a background thread that blocks on a channel until signaled,
   sleeps `debounce-ms` to absorb concurrent bursts, then computes once.
   Closing the channel (stop-bg-refresh!) unblocks <!! with nil → clean exit.
   Computes immediately on startup to seed the cache."
  [& {:keys [debounce-ms] :or {debounce-ms 3000}}]
  (let [ch (async/chan (async/dropping-buffer 1))
        t  (Thread.
             (fn []
               (try (do-refresh!) (catch Exception _))
               (loop []
                 (when (async/<!! ch)       ; nil = channel closed → exit
                   (try (Thread/sleep (long debounce-ms))
                        (catch InterruptedException _))
                   (try (do-refresh!) (catch Exception _))
                   (recur)))))]
    (reset! signal-ch-ref ch)
    (.setDaemon t true)
    (.start t)
    (reset! bg-thread t)))

(defn stop-bg-refresh! []
  (when-let [ch @signal-ch-ref]
    (async/close! ch)
    (reset! signal-ch-ref nil))
  (reset! bg-thread nil))

(defn statusline-stats
  "Current forecast bundle for the statusLine. Sub-millisecond atom read —
   all computation runs in the background thread."
  []
  @forecast-cache)
