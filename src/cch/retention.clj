(ns cch.retention
  "Scheduled retention for the runner-local event database.

  Without it the database only grows: stored provider payloads are most of
  its size, and nothing reads an old one. Three rules, each with its own
  horizon in days:

    event payloads      events.extra is cleared after 30 days. The row stays
                        (time, session, hook, tool, path, decision), which is
                        what statistics and usage research read. Traces and
                        the /events detail pane need payloads only for recent
                        sessions.
    event rows          deleted after 365 days.
    context snapshots   deleted after 91 days. They are the legacy forecast
                        source; 91 days covers the twelve completed seven-day
                        windows its prior learns from.

  usage_observations and the forecast ledger are kept whole: the usage model
  fits on all history and they are small.

  Configure in the global config; a horizon of 0 turns that rule off:

    retention:
      enabled: true
      event-payload-days: 30
      event-days: 365
      context-snapshot-days: 91

  `cch serve` runs a pass shortly after start and then daily. Work is done in
  SQL, in small transactions with a pause between them, so a pass holds no
  rows in memory and never keeps the event writer waiting long.

  Freed pages are reused by later inserts, but the file shrinks only when it
  is rebuilt (VACUUM). The server does that by itself while idle: once no
  event has been logged for `compact-idle-minutes`, it compacts if
  `compact-interval-days` have passed since the last time or free pages make
  up `compact-free-fraction` of the file. The event writer holds its queue
  for those seconds, so nothing is dropped. 0 turns a trigger off:

    retention:
      compact-interval-days: 14
      compact-free-fraction: 0.3
      compact-idle-minutes: 30

  `cch retention --vacuum` compacts on demand."
  (:require [cch.config :as config]
            [cch.db :as db]
            [cch.log :as log]
            [clojure.java.io :as io]
            [next.jdbc :as jdbc])
  (:import [java.time Instant LocalDateTime ZoneOffset]
           [java.time.format DateTimeFormatter]))

(def defaults
  {:enabled true
   :event-payload-days 30
   :event-days 365
   :context-snapshot-days 91
   :compact-interval-days 14
   :compact-free-fraction 0.3
   :compact-idle-minutes 30
   :interval-hours 24
   :start-delay-seconds 300
   :batch-rows 1000
   :pause-ms 25})

(defn settings
  "Effective retention settings from the global config. A missing, negative
  or non-numeric value falls back to its default. Pure given cfg."
  [cfg]
  (let [given (:retention cfg)]
    (reduce-kv (fn [m k default]
                 (let [v (get given k)]
                   (assoc m k (cond
                                (boolean? default) (if (boolean? v) v default)
                                (and (number? v) (<= 0 v)) v
                                :else default))))
               {} defaults)))

(defn load-settings
  "Effective settings from the global config file."
  []
  (settings (try (config/load-yaml (config/global-config-path))
                 (catch Exception _ nil))))

(def ^:private timestamp-format
  ;; The spelling SQLite writes for events.timestamp: UTC, no zone.
  (.withZone (DateTimeFormatter/ofPattern "yyyy-MM-dd'T'HH:mm:ss.SSS") ZoneOffset/UTC))

(defn cutoff
  "The stored-timestamp spelling of `days` before `now-ms`, or nil when the
  rule is off (0 days). Pure."
  [now-ms days]
  (when (pos? days)
    (.format timestamp-format
             (Instant/ofEpochMilli (- now-ms (long (* days 86400000)))))))

(def ^:private state-table
  ;; `cursor` is the last row id a task has processed, so each pass visits
  ;; only rows that aged in since. Created here rather than in schema.sql so
  ;; the command line works against a database no newer server has opened.
  (str "CREATE TABLE IF NOT EXISTS retention_state ("
       " task TEXT PRIMARY KEY,"
       " cursor INTEGER NOT NULL DEFAULT 0 CHECK (cursor >= 0),"
       " updated_at TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%f', 'now')))"))

(defn- open-connection
  "A write connection whose transactions take the write lock when they begin.
  A transaction that reads first and writes later is refused outright, not
  made to wait, if the event writer commits in between."
  []
  (doto (jdbc/get-connection {:dbtype "sqlite" :dbname (db/db-path)
                              :transaction_mode "IMMEDIATE"})
    ;; The journal mode is left as the event writer set it: changing it
    ;; cannot wait for another connection and fails while one is active.
    (jdbc/execute! ["PRAGMA busy_timeout=5000"])
    (jdbc/execute! [state-table])))

(defn- scalar [conn sql-params]
  (some-> (jdbc/execute-one! conn sql-params) vals first))

(defn- last-id-before
  "The newest id in `table` older than the cutoff timestamp, or nil. Ids rise
  with insertion time, so every row up to it is past the horizon."
  [conn table cutoff-ts]
  (when cutoff-ts
    (if-let [first-kept (scalar conn [(str "SELECT id FROM " table " WHERE timestamp >= ?"
                                           " ORDER BY timestamp LIMIT 1")
                                      cutoff-ts])]
      (dec first-kept)
      (scalar conn [(str "SELECT MAX(id) FROM " table)]))))

(defn- update-count [result]
  (or (:next.jdbc/update-count (first result)) 0))

(defn- batches
  "Call `step!` (returns [rows-changed done?]) until done or `continue?`
  turns false, pausing between calls. Returns the rows changed."
  [step! continue? pause-ms]
  (loop [total 0]
    (let [[n done?] (step!)
          total (+ total n)]
      (if (or done? (not (continue?)))
        total
        (do (Thread/sleep (long pause-ms))
            (recur total))))))

(defn- trim-event-payloads!
  "Clear events.extra up to id `through`, a range of ids per transaction.
  The cursor in retention_state records how far earlier passes got, so each
  pass visits only rows that have aged in since."
  [conn through {:keys [batch-rows pause-ms]} continue?]
  (jdbc/execute! conn ["INSERT OR IGNORE INTO retention_state (task, cursor) VALUES ('event-payloads', 0)"])
  (batches
    (fn []
      (jdbc/with-transaction [tx conn]
        (let [cursor (scalar tx ["SELECT cursor FROM retention_state WHERE task='event-payloads'"])
              upto (min through (+ cursor batch-rows))]
          (if (<= upto cursor)
            [0 true]
            (let [n (update-count
                      (jdbc/execute! tx ["UPDATE events SET extra=NULL WHERE id > ? AND id <= ? AND extra IS NOT NULL"
                                         cursor upto]))]
              (jdbc/execute! tx [(str "UPDATE retention_state SET cursor=?,"
                                      " updated_at=strftime('%Y-%m-%dT%H:%M:%f','now')"
                                      " WHERE task='event-payloads'")
                                 upto])
              [n (>= upto through)])))))
    continue? pause-ms))

(defn- delete-rows!
  "Delete rows of `table` up to id `through`, oldest first, a range of ids
  per transaction."
  [conn table through {:keys [batch-rows pause-ms]} continue?]
  (batches
    (fn []
      (jdbc/with-transaction [tx conn]
        (let [oldest (scalar tx [(str "SELECT MIN(id) FROM " table)])]
          (if (or (nil? oldest) (> oldest through))
            [0 true]
            (let [upto (min through (+ oldest (dec batch-rows)))]
              [(update-count (jdbc/execute! tx [(str "DELETE FROM " table " WHERE id <= ?") upto]))
               (>= upto through)])))))
    continue? pause-ms))

(defn- boundaries
  "The last id past each rule's horizon, nil where the rule is off or
  nothing is old enough."
  [conn {:keys [event-payload-days event-days context-snapshot-days]} now-ms]
  {:event-payloads (last-id-before conn "events" (cutoff now-ms event-payload-days))
   :events (last-id-before conn "events" (cutoff now-ms event-days))
   :context-snapshots (last-id-before conn "context_snapshots"
                                      (cutoff now-ms context-snapshot-days))})

(defn prune!
  "One retention pass. Returns the rows changed per rule:
  {:event-payloads n :events n :context-snapshots n}. `continue?` is polled
  between transactions so a shutdown can end a long first pass early; the
  next pass resumes where it stopped."
  ([s] (prune! s (System/currentTimeMillis) (constantly true)))
  ([s now-ms continue?]
   (with-open [conn (open-connection)]
     (let [{:keys [event-payloads events context-snapshots]} (boundaries conn s now-ms)]
       ;; Delete before trimming: no point clearing a payload about to go.
       {:events (if events (delete-rows! conn "events" events s continue?) 0)
        :context-snapshots (if context-snapshots
                             (delete-rows! conn "context_snapshots" context-snapshots s continue?)
                             0)
        :event-payloads (if event-payloads
                          (trim-event-payloads! conn event-payloads s continue?)
                          0)}))))

(defn pending
  "What a pass would change now, as exact row counts per rule. Counting scans
  the old rows, so this is for the command line, not the server."
  ([s] (pending s (System/currentTimeMillis)))
  ([s now-ms]
   (with-open [conn (open-connection)]
     (let [{:keys [event-payloads events context-snapshots]} (boundaries conn s now-ms)
           n (fn [sql & ids] (if (every? some? ids) (or (scalar conn (into [sql] ids)) 0) 0))]
       {:events (n "SELECT COUNT(*) FROM events WHERE id <= ?" events)
        :context-snapshots (n "SELECT COUNT(*) FROM context_snapshots WHERE id <= ?" context-snapshots)
        ;; Rows about to be deleted are not counted as payloads to clear.
        :event-payloads (n (str "SELECT COUNT(*) FROM events WHERE id <= ? AND id > ? AND extra IS NOT NULL"
                                " AND id > (SELECT COALESCE(MAX(cursor), 0) FROM retention_state"
                                " WHERE task='event-payloads')")
                           event-payloads (or events 0))}))))

(defn vacuum!
  "Rebuild the database file, returning freed pages to the filesystem. Holds
  the write lock throughout and needs free disk space about the size of the
  rebuilt file, so run it with `cch serve` stopped.

  The rebuilt content lands in the write-ahead log, and the file shrinks
  when that is checkpointed, which another process with the database open
  can hold up. Returns true once the checkpoint completed, false if it was
  still held up after `attempts` tries; the next complete checkpoint by any
  process then shrinks the file."
  ([] (vacuum! 10))
  ([attempts]
   (with-open [conn (open-connection)]
     (jdbc/execute! conn ["VACUUM"])
     (loop [left attempts]
       (let [busy (scalar conn ["PRAGMA wal_checkpoint(TRUNCATE)"])]
         (cond
           (zero? (or busy 0)) true
           (<= left 1) false
           :else (do (Thread/sleep 1000) (recur (dec left)))))))))

;; --- Compaction while idle ---

(defn compact-due?
  "Whether to compact now. `since-ms` is the time since the last compaction,
  `free-fraction` the share of the file that is free pages, `idle-ms` the
  time since the last logged event. Pure."
  [{:keys [compact-interval-days compact-free-fraction compact-idle-minutes]}
   {:keys [since-ms free-fraction idle-ms]}]
  (and (>= idle-ms (* compact-idle-minutes 60000))
       (or (and (pos? compact-free-fraction) (>= free-fraction compact-free-fraction))
           (and (pos? compact-interval-days) (>= since-ms (* compact-interval-days 86400000))))
       true))

(defn- compaction-state
  "The inputs to compact-due? as the database stands. A database seen for the
  first time starts its interval now."
  [now-ms]
  (with-open [conn (open-connection)]
    (jdbc/execute! conn ["INSERT OR IGNORE INTO retention_state (task, cursor) VALUES ('compaction', ?)"
                         (quot now-ms 1000)])
    (let [last-secs (scalar conn ["SELECT cursor FROM retention_state WHERE task='compaction'"])
          pages (or (scalar conn ["PRAGMA page_count"]) 0)
          free (or (scalar conn ["PRAGMA freelist_count"]) 0)
          newest (scalar conn ["SELECT MAX(timestamp) FROM events"])]
      {:since-ms (- now-ms (* 1000 last-secs))
       :free-fraction (if (pos? pages) (/ (double free) pages) 0.0)
       :idle-ms (if newest
                  (- now-ms (.toEpochMilli (.toInstant (LocalDateTime/parse newest) ZoneOffset/UTC)))
                  Long/MAX_VALUE)})))

(defn- room-to-rebuild?
  "The rebuild writes a second copy before the file shrinks."
  []
  (let [file (io/file (db/db-path))]
    (> (.getUsableSpace (.getAbsoluteFile file)) (.length file))))

(defn compact-if-due!
  "Compact when compact-due? says so and the disk has room. Returns
  :compacted, :held-up (rebuilt, but another process delayed the shrink),
  :no-room, or nil when not due. The interval restarts in every non-nil
  case, so a failed attempt is not retried hourly."
  ([s] (compact-if-due! s (System/currentTimeMillis)))
  ([s now-ms]
   (when (compact-due? s (compaction-state now-ms))
     (let [outcome (cond
                     (not (room-to-rebuild?)) :no-room
                     (log/with-writes-held vacuum!) :compacted
                     :else :held-up)]
       (with-open [conn (open-connection)]
         (jdbc/execute! conn [(str "UPDATE retention_state SET cursor=?,"
                                   " updated_at=strftime('%Y-%m-%dT%H:%M:%f','now')"
                                   " WHERE task='compaction'")
                              (quot now-ms 1000)]))
       outcome))))

;; --- Scheduler ---

(defonce ^:private worker (atom nil))

(defn- report [result]
  (when (some pos? (vals result))
    (println (format "cch retention: cleared %d event payloads, deleted %d events and %d context snapshots"
                     (:event-payloads result) (:events result) (:context-snapshots result)))))

(defn- report-compaction [outcome]
  (case outcome
    :compacted (println "cch retention: compacted the event database")
    :held-up (println "cch retention: rebuilt the event database; it shrinks at the next complete checkpoint")
    :no-room (println "cch retention: compaction skipped, not enough free disk space")
    nil))

(defn start!
  "Start the retention worker. It wakes hourly, runs a retention pass when
  `interval-hours` have passed since the last, and compacts when due. It
  reads the config every time, so a changed setting or `enabled: false`
  takes effect without a restart."
  []
  (when-not @worker
    (let [running (atom true)
          sleep! (fn [ms] (try (Thread/sleep (long ms))
                               (catch InterruptedException _ (reset! running false))))
          guarded (fn [label f]
                    (try (f)
                         (catch InterruptedException _ (reset! running false))
                         (catch Exception e
                           (println (str "cch retention: " label " failed:")
                                    (.getSimpleName (class e))))))
          t (Thread.
              (fn []
                (sleep! (* 1000 (:start-delay-seconds (load-settings))))
                (loop [last-pass nil]
                  (when @running
                    (let [s (load-settings)
                          now (System/currentTimeMillis)
                          pass? (and (:enabled s)
                                     (or (nil? last-pass)
                                         (>= (- now last-pass) (* 3600000 (max 1 (:interval-hours s))))))]
                      (when pass?
                        (guarded "pass" #(report (prune! s now (fn [] @running)))))
                      (when (and (:enabled s) @running)
                        (guarded "compaction" #(report-compaction (compact-if-due! s))))
                      (when @running (sleep! 3600000))
                      (recur (if pass? now last-pass))))))
              "cch-retention")]
      (.setDaemon t true)
      (.start t)
      (reset! worker {:running running :thread t}))))

(defn stop! []
  (when-let [{:keys [running ^Thread thread]} @worker]
    (reset! running false)
    (.interrupt thread)
    (reset! worker nil)))
