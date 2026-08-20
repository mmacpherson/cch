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

;; Empirical Δ7d/Δ5h co-movement ratio (490k sample pairs). The 5h window
;; ticks ~7.5x more often (proportional to its smaller token budget), so we
;; weight it 7.5x more heavily in the fused estimate.
(def ^:private scale-5h->7d 0.133)
(def ^:private weight-5h    7.5)

(def ^:const default-agent "claude-code")

(defn- escape-sql-literal
  "Single-quote escape for SQL string literals. Defensive — agent strings
   come from HTTP headers and registry constants, never user prompts, but
   we treat them as untrusted regardless."
  [s]
  (clojure.string/replace (str s) "'" "''"))

(defn- agent-clause
  "AND fragment that scopes a query to the given agent."
  [agent]
  (format " AND agent = '%s' " (escape-sql-literal agent)))

(defn- latest-resets-at
  "The most recent resets_at for a given window, scoped to `agent`."
  [agent window-key]
  (let [wpath (window-sql-path window-key)
        sql   (format
                (str "SELECT json_extract(payload, '$.rate_limits.%s.resets_at') AS resets_at "
                     "FROM context_snapshots "
                     "WHERE json_extract(payload, '$.rate_limits.%s.resets_at') IS NOT NULL "
                     "  AND json_extract(payload, '$.rate_limits.%s.used_percentage') > 0 "
                     "%s"
                     "ORDER BY id DESC LIMIT 1")
                wpath wpath wpath (agent-clause agent))]
    (some-> (db/query sql) first :resets_at long)))

(defn- filtered-samples
  "Fetch rate-limit samples for `window-key` since `since-iso`, scoped to
   `agent`, with drop-stale and time-bucket thinning done in SQL via
   window functions."
  [agent since-iso window-key]
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
                   "    %s"
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
                 wpath wpath since-iso wpath (agent-clause agent) bucket)]
    (some->> (db/query sql)
             (mapv (fn [{:keys [ts pct resets_at]}]
                     {:ts        (long ts)
                      :pct       (double pct)
                      :resets-at (long resets_at)})))))

(defn- raw-sample-count
  "Total row count before filtering for `agent` — for the /usage page's
   sample display."
  [agent since-iso window-key]
  (let [wpath (window-sql-path window-key)
        sql   (format
                (str "SELECT COUNT(*) AS n FROM context_snapshots"
                     " WHERE timestamp >= '%s'"
                     " AND json_extract(payload, '$.rate_limits.%s.used_percentage') IS NOT NULL"
                     " AND session_id NOT LIKE 'test%%'"
                     " %s")
                since-iso wpath (agent-clause agent))]
    (some-> (db/query sql) first :n long)))

(defn- epoch->iso [secs]
  (str (java.time.Instant/ofEpochSecond secs)))

(defn- rate-5h-samples
  "60s-bucketed five-hour window samples for the full 7d span, scoped to `agent`.
   Monotone filter is partitioned per resets_at so each 5h window is
   treated independently. Returns :resets-at so the chart can avoid
   spanning a reset boundary when computing a lookback-window rate."
  [agent since-iso]
  (let [sql (format
              (str "WITH samples AS ("
                   "  SELECT CAST(strftime('%%s', timestamp) AS INTEGER) AS ts,"
                   "    CAST(json_extract(payload, '$.rate_limits.five_hour.used_percentage') AS REAL) AS pct,"
                   "    json_extract(payload, '$.rate_limits.five_hour.resets_at') AS resets_at"
                   "  FROM context_snapshots"
                   "  WHERE timestamp >= '%s'"
                   "    AND json_extract(payload, '$.rate_limits.five_hour.used_percentage') IS NOT NULL"
                   "    AND session_id NOT LIKE 'test%%'"
                   "    %s"
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
              since-iso (agent-clause agent))]
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

(defn- historical-finals-sql [agent]
  (format
    (str "SELECT final_pct FROM ("
         "  SELECT MAX(CAST(json_extract(payload,'$.rate_limits.seven_day.used_percentage') AS REAL))"
         "    AS final_pct"
         "  FROM context_snapshots"
         "  WHERE json_extract(payload,'$.rate_limits.seven_day.resets_at') < strftime('%%s','now')"
         "    AND json_extract(payload,'$.rate_limits.seven_day.used_percentage') IS NOT NULL"
         "    AND session_id NOT LIKE 'test%%'"
         "    %s"
         "  GROUP BY json_extract(payload,'$.rate_limits.seven_day.resets_at')"
         "  ORDER BY json_extract(payload,'$.rate_limits.seven_day.resets_at') DESC"
         "  LIMIT 12"
         ") WHERE final_pct >= 10")
    (agent-clause agent)))

(defn- historical-final-pcts
  "Final used_percentage for each completed 7-day window, newest-first, up to 12."
  [agent]
  (some->> (db/query (historical-finals-sql agent))
           (mapv #(double (:final_pct %)))))

(defn- learned-prior
  "Derive an empirical Bayes prior (μ/σ in %/hr) from completed windows
   for `agent`. Returns nil during the first week when there is no history."
  [agent]
  (some->> (db/query (historical-finals-sql agent))
           (weighted-prior-params)))

(def ^:private window-config
  "Per-window Bayesian prior defaults (μ, σ in %/hr), grounded in observed
  completed-window burn rates (2026-08, claude-code, ~14 5h + 17 7d windows):
  the 5h window burns ~3.75 %/hr, the 7d window ~0.42 %/hr — ≈9× apart, since
  a 5h window packs a week's proportional burn into 5 hours. These baselines
  are the live priors the projection anchors on. `window-priors` also merges a
  learned prior, but that overlay is NOT yet wired into the projection (it
  returns :mu/:sigma while consumers read :prior-mu/:prior-sigma — see the
  follow-up issue), so getting these constants right is what matters today.

  The previous 5h μ=15.0/σ=8.0 was a ~4× overestimate that biased every eo5h
  projection toward premature exhaustion."
  {:seven-day {:prior-mu 0.42 :prior-sigma 0.13}
   :five-hour {:prior-mu 3.75 :prior-sigma 1.3}})

(defn- window-priors
  "Best-available prior for [agent window-key]: window-config baseline
  merged with the empirical-Bayes learned prior (7d only — 5h relies on
  the hardcoded baseline until enough completed 5h windows exist to
  learn from). Both /usage and statusline projections MUST go through
  this so they agree."
  [agent window-key]
  (let [base    (window-config window-key)
        learned (when (= window-key :seven-day) (learned-prior agent))]
    (merge base learned)))

(defn- build-current-window
  "Rich data bundle for the /usage page, for either :seven-day or :five-hour,
   scoped to `agent`. Returns nil when no rate-limit data exists for the
   window."
  [agent window-key]
  (when-let [raw-resets-at (latest-resets-at agent window-key)]
    (let [now          (-> (Instant/now) .getEpochSecond)
          span         (span-secs window-key)
          ;; After a reset the DB still has the old resets_at until the CLI
          ;; posts a fresh snapshot. Shift to the new window.
          resets-at    (if (<= raw-resets-at now)
                         (+ raw-resets-at span)
                         raw-resets-at)
          window-start (- resets-at span)
          raw-samples  (filtered-samples agent (epoch->iso window-start) window-key)
          ;; Stale sessions cache old rate-limit state and keep reporting
          ;; the expired resets_at with high pct values for hours after a
          ;; reset. Only keep samples tagged with the current resets_at.
          in-window    (filterv #(= (:resets-at %) resets-at) raw-samples)
          last-pct     (or (:pct (last in-window)) 0.0)
          ;; Historical priors only derived for 7d today — 5h relies on the
          ;; hardcoded window-config prior. Plenty of 5h windows exist to
          ;; learn from later; that's deferred work.
          hist-finals  (when (= window-key :seven-day) (historical-final-pcts agent))
          {:keys [prior-mu prior-sigma]} (window-priors agent window-key)
          window-info  {:now now :resets-at resets-at
                        :window-start window-start :last-pct last-pct
                        :prior-mu prior-mu :prior-sigma prior-sigma
                        :historical-finals hist-finals}
          obs-pairs    (mapv #(select-keys % [:ts :pct]) in-window)
          projection   (proj/rate-bayes-projection obs-pairs window-info)
          rs           (proj/rate-samples obs-pairs)
          recent-rate  (when (>= (count rs) 2)
                         (let [recent (take-last 3 rs)]
                           (/ (reduce + 0.0 (map :rate recent))
                              (count recent))))
          ;; Rate chart data source: for 7d we enrich with the 60s-bucketed
          ;; 5h-window stream (much denser than the 6m-bucketed 7d samples)
          ;; and scale into 7d-%/hr units. For 5h-native we just reuse the
          ;; observed samples (already 60s-bucketed) at unit scale.
          rate-samples (if (= window-key :seven-day)
                         (rate-5h-samples agent (epoch->iso window-start))
                         in-window)
          rate-scale   (if (= window-key :seven-day) scale-5h->7d 1.0)]
      {:agent           agent
       :window-key      window-key
       :span-secs       span
       :observed        obs-pairs
       :rate-samples    rate-samples
       :rate-scale      rate-scale
       :resets-at       resets-at
       :window-start    window-start
       :now             now
       :last-pct        last-pct
       :samples         (or (raw-sample-count agent (epoch->iso window-start) window-key) 0)
       :projection      projection
       :rate-phr        recent-rate})))

;; Cache current-window per [agent window-key] for 30s — data arrives at
;; most once per minute via the statusLine hook (or per turn for codex),
;; so re-running 4 queries + LOESS + projections on every browser hit is
;; pure waste.
(def ^:private window-cache (atom {}))

(defn current-window
  "Data bundle for the /usage page. Cached for 30 s, per [agent window-key].
   Default agent is 'claude-code'; default window-key is :seven-day."
  ([] (current-window default-agent :seven-day))
  ([window-key] (current-window default-agent window-key))
  ([agent window-key]
   (let [k                  [agent window-key]
         {:keys [ts data]}  (get @window-cache k {:ts 0 :data nil})
         now                (-> (Instant/now) .getEpochSecond)]
     (if (< (- now ts) 30)
       data
       (let [fresh (build-current-window agent window-key)]
         (swap! window-cache assoc k {:ts now :data fresh})
         fresh)))))

;; pct is integer-quantized (1% resolution). At ~0.6%/hr aggregate, a 10-min
;; window almost never captures a tick. 20 min gives enough span to see
;; movement at normal rates while staying reactive to bursts. Compare newest
;; vs oldest raw sample across all concurrent sessions.
(def ^:private burn-lookback-secs 1200)

(defn- recent-burn-rate-phr
  "Observed burn rate in %/hr over the past ~20 min of raw samples,
   scoped to `agent`. Compares newest vs oldest pct within the window
   across all concurrent sessions. Returns 0.0 when idle, nil when fewer
   than 2 samples."
  [agent wpath resets-at now]
  (let [cutoff-iso (epoch->iso (- now burn-lookback-secs))
        sql        (format
                     (str "SELECT CAST(strftime('%%s', timestamp) AS INTEGER) AS ts,"
                          "  json_extract(payload, '$.rate_limits.%s.used_percentage') AS pct"
                          " FROM context_snapshots"
                          " WHERE timestamp >= '%s'"
                          "   AND json_extract(payload, '$.rate_limits.%s.resets_at') = %d"
                          "   AND json_extract(payload, '$.rate_limits.%s.used_percentage') IS NOT NULL"
                          "   AND session_id NOT LIKE 'test%%'"
                          "   %s"
                          " ORDER BY ts ASC")
                     wpath cutoff-iso wpath resets-at wpath (agent-clause agent))
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
   both in 7d-percent/hr, scoped to `agent`. 5h weight = 7.5
   (proportional to its tick frequency)."
  [agent resets-at-7d now]
  (let [r7d  (some-> (recent-burn-rate-phr agent "seven_day" resets-at-7d now) (max 0.0))
        r5at (latest-resets-at agent :five-hour)
        r5h  (when r5at
               (some-> (recent-burn-rate-phr agent "five_hour" r5at now)
                       (* scale-5h->7d)
                       (max 0.0)))]
    (cond
      (and r7d r5h) (/ (+ r7d (* weight-5h r5h)) (+ 1.0 weight-5h))
      r5h           r5h
      r7d           r7d)))

(defn- compute-window-stats
  "Assemble observations for `window-key` scoped to `agent`, run `proj-fn`
   to get the forward projection, and return the statusLine stats map.
   `proj-fn` must satisfy `[observed window-info] → {:proj ...} | nil`."
  [agent window-key proj-fn]
  (when-let [raw-resets-at (latest-resets-at agent window-key)]
    (let [now          (-> (Instant/now) .getEpochSecond)
          span         (span-secs window-key)
          resets-at    (if (<= raw-resets-at now)
                         (+ raw-resets-at span)
                         raw-resets-at)
          {:keys [prior-mu prior-sigma]} (window-priors agent window-key)
          wpath        (window-sql-path window-key)
          window-start (- resets-at span)
          raw-samples  (filtered-samples agent (epoch->iso window-start) window-key)
          in-window    (filterv #(= (:resets-at %) resets-at) raw-samples)
          last-pct     (:pct (last in-window))
          hist-finals  (when (= window-key :seven-day) (historical-final-pcts agent))
          window-info  {:now now :resets-at resets-at
                        :window-start window-start :last-pct last-pct
                        :prior-mu prior-mu :prior-sigma prior-sigma
                        :historical-finals hist-finals}
          obs-pairs    (mapv #(select-keys % [:ts :pct]) in-window)
          proj-result  (when last-pct (proj-fn obs-pairs window-info))
          local-rate   (if (= window-key :seven-day)
                         (fused-burn-rate-7d agent resets-at now)
                         (recent-burn-rate-phr agent wpath resets-at now))]
      (when last-pct
        (let [raw-proj (or (:proj proj-result) last-pct)
              band     (:band proj-result)]
          (cond-> {:current_pct   (Math/round last-pct)
                   :projected_pct (Double/parseDouble (format "%.1f" raw-proj))
                   :secs_left     (max 0 (- resets-at now))}
            (some? local-rate) (assoc :local_rate_phr (Double/parseDouble (format "%.1f" local-rate)))
            band (assoc :band {:lo (Math/round (double (:lo band)))
                               :hi (Math/round (double (:hi band)))})))))))

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

(defn- do-refresh!
  "Refresh the statusline forecast cache. Claude-only — the statusLine
  command is Claude Code's; Codex uses its own built-in status_line
  config. The /usage page's Codex tab pulls live data via current-window
  (its own per-[agent window-key] cache), not via this atom."
  []
  (reset! forecast-cache
          {:five_hour    (compute-window-stats default-agent :five-hour  proj/rate-bayes-projection)
           :seven_day    (compute-window-stats default-agent :seven-day  proj/rate-bayes-projection)
           :computed_at  (-> (Instant/now) .getEpochSecond)}))

(defn- safe-refresh! []
  ;; Catch Throwable, not Exception — an Error (OOM, init failure, etc.)
  ;; would otherwise escape and kill the loop, freezing the cache.
  (try (do-refresh!)
       (catch Throwable t
         (binding [*out* *err*]
           (println "cch.forecast: refresh failed:" (.getMessage t))))))

(defn start-bg-refresh!
  "Start a background thread that refreshes the forecast cache.

   Two wakeup paths, so liveness does not depend on signal delivery:
     - signal channel: a /context-snapshot POST signals immediately,
       loop debounces `debounce-ms` to coalesce bursts, then refreshes.
     - timer: every `max-stale-ms` the loop refreshes anyway, so even
       if signals are dropped/lost the cache never goes stale.

   Closing the channel (stop-bg-refresh!) makes the next take return nil → exit."
  [& {:keys [debounce-ms max-stale-ms]
      :or   {debounce-ms 3000 max-stale-ms 60000}}]
  (let [ch (async/chan (async/dropping-buffer 1))
        t  (Thread.
             (fn []
               (safe-refresh!)
               (loop []
                 (let [timeout-ch (async/timeout max-stale-ms)
                       [v port]   (try (async/alts!! [ch timeout-ch])
                                       (catch Throwable t
                                         (binding [*out* *err*]
                                           (println "cch.forecast: alts!! failed:" (.getMessage t)))
                                         [:tick timeout-ch]))
                       signaled?  (identical? port ch)
                       closed?    (and signaled? (nil? v))]
                   (when-not closed?
                     (when signaled?
                       ;; debounce to absorb a burst of concurrent posts
                       (try (Thread/sleep (long debounce-ms))
                            (catch InterruptedException _)))
                     (safe-refresh!)
                     (recur))))))]
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
