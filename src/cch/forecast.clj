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

(defn current-window
  "Data bundle for the /usage page: observed snapshots in the current
   7d rate-limit window plus the full set of forward projections."
  []
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
        {:observed     obs-pairs
         :resets-at    resets-at
         :window-start window-start
         :now          now
         :last-pct     last-pct
         :samples      (or (raw-sample-count (epoch->iso window-start) :seven-day) 0)
         :projections  projs}))))

(def ^:private window-config
  {:seven-day {:prior-mu 0.55 :prior-sigma 0.045}
   :five-hour {:prior-mu 15.0 :prior-sigma 8.0}})

;; pct is integer-quantized (1% resolution). At ~0.6%/hr aggregate, a 10-min
;; window almost never captures a tick. 20 min gives enough span to see
;; movement at normal rates while staying reactive to bursts. Compare newest
;; vs oldest raw sample across all concurrent sessions.
(def ^:private burn-lookback-secs 1200)

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

(defn- compute-bayes-stats
  "Bayesian projection for one window."
  [window-key]
  (when-let [resets-at (latest-resets-at window-key)]
    (let [{:keys [prior-mu prior-sigma]} (window-config window-key)
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
          local-rate   (recent-burn-rate-phr wpath resets-at now)]
      (when last-pct
        (let [raw-proj (or (:proj proj-result) last-pct)]
          (cond-> {:current_pct   (Math/round last-pct)
                   :projected_pct (Double/parseDouble (format "%.1f" raw-proj))
                   :secs_left     (max 0 (- resets-at now))}
            (some? local-rate) (assoc :local_rate_phr (Double/parseDouble (format "%.1f" local-rate)))))))))

(defn statusline-stats
  "Bundle for the statusLine: current pct, Bayesian projection at reset,
   and time-to-reset for both windows."
  []
  {:five_hour (compute-bayes-stats :five-hour)
   :seven_day (compute-bayes-stats :seven-day)})
