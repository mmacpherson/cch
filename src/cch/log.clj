(ns cch.log
  "SQLite event logging.

  Two write paths:

    1. **Background writer (preferred).** When `start-writer!` has been
       called (cch serve does this in start!), rows are queued onto a
       bounded LinkedBlockingQueue and a daemon thread inserts them over
       one JDBC connection with bound parameters, in small batches. Per-call
       cost is just the queue offer. Parameters, not SQL text, carry the
       values, so no payload byte (quotes, NUL, newlines) can change the
       statement; failures are reported, not swallowed. WAL lets the
       dashboard's reads run alongside writes.

    2. **Per-call fallback.** When no writer is registered (subprocess
       invocations of hooks, tests without setup), log-event! spawns a
       fresh `sqlite3` per insert via `p/process`. Measured at p50 ~3ms
       / p99 ~8ms — same as before.

  CCH_LOG_SYNC=1 forces synchronous `p/sh` regardless of the writer,
  so tests can assert on rows immediately after a log call."
  (:require [babashka.process :as p]
            [babashka.fs :as fs]
            [cch.db :as db]
            [cch.migrate :as migrate]
            [cch.usage-observation :as usage]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [honey.sql :as sql]
            [next.jdbc :as jdbc]))

(defn ensure-db!
  "Create the database directory if needed, apply the baseline schema,
  then run any pending migrations from cch.migrate. Idempotent —
  CREATE TABLE IF NOT EXISTS handles re-runs; cch.migrate/apply-all!
  skips already-applied deltas."
  [path]
  (let [dir (fs/parent path)]
    (when-not (fs/exists? dir)
      (fs/create-dirs dir))
    (let [schema (slurp (io/resource "schema.sql"))]
      (p/sh ["sqlite3" path schema]))
    (migrate/apply-all! path)))

(def ^:private ensured-paths
  "Set of DB paths we've already run ensure-db! against in this process.
  Skips redundant stat calls for long-running dispatchers (cch serve);
  a no-op for short-lived hook subprocesses."
  (atom #{}))

(defn- ensure-db-once!
  [path]
  (when-not (contains? @ensured-paths path)
    (ensure-db! path)
    (swap! ensured-paths conj path)))

(defn- escape-sql
  "Escape a string for SQLite single-quoted literals on the per-call
  fallback path. NUL is dropped: the sqlite3 CLI and process arguments both
  end a string there, which would unbalance the quotes."
  [s]
  (when s
    (-> (str s) (str/replace "\u0000" "") (str/replace "'" "''"))))

(defn- sql-value
  "Format a value for SQL insertion. nil becomes NULL, strings get single-quoted."
  [v]
  (if (nil? v)
    "NULL"
    (str "'" (escape-sql (str v)) "'")))

;; --- Background writer ---

;; When non-nil, holds {:queue <BlockingQueue> :thread <Thread> :running <atom>}.
;; Set by start-writer!, cleared by stop-writer!. log-event! checks this to
;; decide between queued and per-call modes.
(defonce ^:private writer-state (atom nil))

(def ^:private writer-queue-capacity 4096)
(def ^:private writer-batch-max 200)

(def ^:private insert-sql
  (str "INSERT INTO events (session_id, hook_name, event_type, tool_name, file_path,"
       " cwd, decision, reason, elapsed_ms, extra, agent) VALUES (?,?,?,?,?,?,?,?,?,?,?)"))

(defn- event-statement
  "The bound insert for one event, in insert-sql column order."
  [{:keys [session-id hook-name event-type tool-name file-path cwd
           decision reason elapsed-ms extra agent]}]
  [insert-sql session-id hook-name event-type tool-name file-path cwd
   (when decision (name decision)) reason
   elapsed-ms extra (or agent "claude-code")])

(defonce ^:private writer-failures (atom {:count 0 :last nil}))

(defn writer-health
  "Insert failures since start: {:count n :last {:at ms :error class-name}}."
  []
  @writer-failures)

(defn- open-writer-connection [path]
  (doto (jdbc/get-connection {:dbtype "sqlite" :dbname path})
    (jdbc/execute! ["PRAGMA journal_mode=WAL"])
    (jdbc/execute! ["PRAGMA busy_timeout=5000"])))

(defn- run-item! [conn {:keys [statements]}]
  (doseq [stmt statements] (jdbc/execute! conn stmt)))

(defn- write-batch!
  "Run a batch of queued items in one transaction. On failure, report it and
  retry each item alone, in its own transaction, so one bad item cannot drop
  its neighbours. A transaction is always committed or rolled back here, so
  none can be left open."
  [conn items]
  (try
    (jdbc/with-transaction [tx conn]
      (doseq [item items] (run-item! tx item)))
    (catch Exception _
      (doseq [item items]
        (try (jdbc/with-transaction [tx conn] (run-item! tx item))
             (catch Exception e
               (swap! writer-failures #(-> % (update :count inc)
                                           (assoc :last {:at (System/currentTimeMillis)
                                                         :error (.getName (class e))})))
               (binding [*out* *err*]
                 (println "cch.log: event insert failed:" (.getName (class e))))))))))

(defn- writer-loop
  "Drain the queue in batches until ::stop. Reopens the connection after a
  connection-level failure; never exits on a bad row."
  [path ^java.util.concurrent.BlockingQueue queue]
  (loop [conn nil]
    (let [item (try (.take queue) (catch InterruptedException _ ::stop))]
      (if (identical? item ::stop)
        (some-> ^java.sql.Connection conn .close)
        (let [batch (java.util.ArrayList.)
              _ (.drainTo queue batch (dec writer-batch-max))
              stop? (.remove batch ::stop)
              rows (cons item (vec batch))
              conn (try (or conn (open-writer-connection path))
                        (catch Exception e
                          (binding [*out* *err*]
                            (println "cch.log: cannot open event DB:" (.getName (class e))))
                          nil))]
          (if conn
            (write-batch! conn rows)
            (swap! writer-failures update :count + (count rows)))
          (if stop?
            (some-> ^java.sql.Connection conn .close)
            (recur (when (and conn (not (.isClosed ^java.sql.Connection conn))) conn))))))))

(defn start-writer!
  "Spin up the background writer. Idempotent — no-op if already running.
  Returns the writer-state map for callers that want to introspect."
  []
  (or @writer-state
      (let [path  (db/db-path)
            _     (ensure-db-once! path)
            queue (java.util.concurrent.LinkedBlockingQueue. writer-queue-capacity)
            thread (Thread. ^Runnable #(writer-loop path queue))]
        (.setDaemon thread true)
        (.setName thread "cch-sqlite-writer")
        (.start thread)
        (reset! writer-state {:queue queue :thread thread}))))

(defn stop-writer!
  "Shut the writer down. Sends a stop sentinel so any queued events drain
  first. Idempotent."
  []
  (when-let [{:keys [queue ^Thread thread]} @writer-state]
    (try (.put ^java.util.concurrent.BlockingQueue queue ::stop) (catch Exception _ nil))
    (try (.join thread 2000) (catch Exception _ nil))
    (reset! writer-state nil)))

;; --- log-event! ---

(defn log-event!
  "Insert an event row. Non-blocking.

  event is a map with keys:
    :hook-name, :event-type, :tool-name, :file-path, :cwd,
    :session-id, :decision, :reason, :elapsed-ms, :extra, :agent

  :agent defaults to 'claude-code' for callers that don't supply one
  (legacy code paths, tests). The dispatcher derives it from the
  X-CCH-Agent header on incoming requests.

  Path selection:
    - CCH_LOG_SYNC=1     → synchronous p/sh (test backdoor)
    - writer running     → enqueue SQL onto the background writer (~µs)
    - otherwise          → fire-and-forget per-call sqlite3 subprocess"
  [{:keys [hook-name event-type tool-name file-path cwd
           session-id decision reason elapsed-ms extra agent]}]
  (let [path  (db/db-path)
        sync? (= "1" (System/getenv "CCH_LOG_SYNC"))
        ;; SQL text for the per-call fallback only; the writer binds
        ;; parameters instead.
        insert (format
                 "INSERT INTO events (session_id, hook_name, event_type, tool_name, file_path, cwd, decision, reason, elapsed_ms, extra, agent) VALUES (%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s);"
                 (sql-value session-id)
                 (sql-value hook-name)
                 (sql-value event-type)
                 (sql-value tool-name)
                 (sql-value file-path)
                 (sql-value cwd)
                 (sql-value (when decision (name decision)))
                 (sql-value reason)
                 (sql-value elapsed-ms)
                 (sql-value extra)
                 (sql-value (or agent "claude-code")))
        fallback-sql (str "PRAGMA busy_timeout=5000; " insert)]
    (try
      (ensure-db-once! path)
      (cond
        sync?
        (p/sh ["sqlite3" path fallback-sql])

        @writer-state
        (let [{:keys [^java.util.concurrent.BlockingQueue queue]} @writer-state]
          (when-not (.offer queue {:statements
                                   [(event-statement
                                      {:session-id session-id :hook-name hook-name
                                       :event-type event-type :tool-name tool-name
                                       :file-path file-path :cwd cwd :decision decision
                                       :reason reason :elapsed-ms elapsed-ms
                                       :extra extra :agent agent})]})
            (binding [*out* *err*]
              (println "cch.log: writer queue full; dropping event"))))

        :else
        ;; Discard subprocess stdio — inheriting would corrupt the hook's
        ;; JSON response on stdout that Claude Code parses.
        (p/process ["sqlite3" path fallback-sql]
                   {:out :discard :err :discard}))
      (catch Exception _e
        nil))))


(defn distinct-cwds
  "All distinct cwd values in the events table, in arbitrary order.
  Done as a SQL DISTINCT — avoids pulling the full table into memory
  for the dashboard's Repo dropdown. Returns a seq of strings."
  []
  (->> (db/query (first (sql/format {:select-distinct [:cwd] :from [:events]
                                     :where [:!= :cwd nil]}
                                    {:inline true})))
       (keep :cwd)))

(defn recent-sessions
  "Return up to `limit` most-recently-logged session IDs as
  [{:session_id :timestamp} ...] pairs, optionally filtered to
  sessions whose events landed under `cwd-prefix`.

  The unfiltered dashboard hot path walks events newest-first and uses
  idx_events_session to reject rows that have a newer sibling. It stops
  as soon as it finds `limit` distinct sessions. This avoids grouping
  and sorting the full events table on every /events request.

  A cwd filter can match fewer than `limit` sessions anywhere in history,
  so that case retains the exhaustive grouped query for exact results."
  [& {:keys [limit cwd-prefix] :or {limit 30}}]
  (if (str/blank? cwd-prefix)
    (db/query
      (format
        (str "SELECT e.session_id, e.timestamp"
             " FROM events AS e"
             " WHERE e.session_id IS NOT NULL"
             "   AND NOT EXISTS ("
             "     SELECT 1 FROM events AS newer"
             "     WHERE newer.session_id = e.session_id"
             "       AND newer.id > e.id)"
             " ORDER BY e.id DESC"
             " LIMIT %d")
        (int limit)))
    (let [q {:select   [:session-id [[:max :timestamp] :timestamp]]
             :from     [:events]
             :where    [:and
                        [:is-not :session-id nil]
                        [:like :cwd (str cwd-prefix "%")]]
             :group-by [:session-id]
             :order-by [[:timestamp :desc]]
             :limit    (int limit)}]
      (db/query (first (sql/format q {:inline true}))))))

(defn query-events
  "Query recent events. Returns a seq of maps.
  opts: :limit, :hook, :event, :session, :decision, :since, :cwd-prefix,
        :agent, :q (text search)"
  [& {:keys [limit hook event session decision since cwd-prefix agent node q]}]
  (let [cols [:id :timestamp :agent :node :session-id :hook-name :event-type
              :tool-name :file-path :cwd :decision :reason :elapsed-ms :extra]
        qry  (cond-> {:select   cols
                      :from     [:events]
                      :order-by [[:id :desc]]
                      :limit    (or limit 20)}
               hook       (update :where (fnil conj [:and]) [:= :hook-name hook])
               event      (update :where (fnil conj [:and]) [:= :event-type event])
               session    (update :where (fnil conj [:and]) [:= :session-id session])
               agent      (update :where (fnil conj [:and]) [:= :agent agent])
               node       (update :where (fnil conj [:and]) [:= :node node])
               (= decision "observe")
                          (update :where (fnil conj [:and]) [:is :decision nil])
               (and decision (not= decision "observe"))
                          (update :where (fnil conj [:and]) [:= :decision decision])
               since      (update :where (fnil conj [:and]) [:> :timestamp since])
               cwd-prefix (update :where (fnil conj [:and]) [:like :cwd (str cwd-prefix "%")])
               (not (str/blank? q))
               (update :where (fnil conj [:and])
                       [:or
                        [:like :reason   (str "%" q "%")]
                        [:like :file-path (str "%" q "%")]
                        [:like :extra    (str "%" q "%")]]))]
    (db/query (first (sql/format qry {:inline true})))))

(defn session-trace-rows
  "Rows that make up one session's activity trace, oldest first: every
  event-log row (the full provider payload) plus any hook row that asked or
  denied. At most `limit` of the newest rows."
  [session-id & {:keys [limit] :or {limit 2000}}]
  (->> (db/query
         (first (sql/format
                  {:select   [:id :timestamp :agent :session-id :hook-name :event-type
                              :tool-name :file-path :cwd :decision :reason :elapsed-ms :extra]
                   :from     [:events]
                   :where    [:and
                              [:= :session-id session-id]
                              [:or [:= :hook-name "event-log"]
                                   [:in :decision ["deny" "ask"]]]]
                   :order-by [[:id :desc]]
                   :limit    (int limit)}
                  {:inline true})))
       reverse
       vec))

(defn recent-session-summaries
  "The most recently active sessions with their agent, working directory, and
  last event time: [{:session_id :agent :cwd :timestamp} ...]."
  [& {:keys [limit] :or {limit 40}}]
  (vec
    (for [{:keys [session_id]} (recent-sessions :limit limit)
          :let [row (first (db/query
                             (first (sql/format
                                      {:select [:session-id :agent :cwd :timestamp]
                                       :from [:events]
                                       :where [:= :session-id session_id]
                                       :order-by [[:id :desc]]
                                       :limit 1}
                                      {:inline true}))))]
          :when row]
      row)))

(defn turn-ends-after
  "Turn-ending event-log rows (Stop) with id > after-id, oldest first:
  [{:id :session_id :agent :timestamp} ...]."
  [after-id & {:keys [limit] :or {limit 500}}]
  (db/query
    (first (sql/format
             {:select [:id :session-id :agent :timestamp]
              :from [:events]
              :where [:and [:> :id (long after-id)]
                      [:= :hook-name "event-log"]
                      [:= :event-type "Stop"]
                      [:is-not :session-id nil]]
              :order-by [[:id :asc]]
              :limit (int limit)}
             {:inline true}))))

(defn max-event-id
  "The newest event id, or 0 for an empty table."
  []
  (or (:id (first (db/query "SELECT MAX(id) AS id FROM events"))) 0))

(defn first-event-id-since
  "The id just before the first event at or after an events.timestamp, so a
  cursor set to it starts there; 0 when there is none."
  [timestamp]
  (or (some-> (first (db/query
                       (first (sql/format
                                {:select [[[:min :id] :id]]
                                 :from [:events]
                                 :where [:>= :timestamp timestamp]}
                                {:inline true}))))
              :id dec)
      0))

;; --- Context snapshots ---

(defn log-context-snapshot!
  "Insert a context window snapshot. Non-blocking, same write-path as log-event!.
  :agent defaults to 'claude-code'; pass 'codex' (etc.) for cross-CLI snapshots."
  [{:keys [session-id used-pct current-tokens window-size model-id payload agent]}]
  (let [path        (db/db-path)
        agent       (or agent "claude-code")
        observed-at (System/currentTimeMillis)
        legacy      (format
                      "INSERT INTO context_snapshots (agent, session_id, used_pct, current_tokens, window_size, model_id, payload) VALUES (%s,%s,%s,%s,%s,%s,%s);"
                      (sql-value agent)
                      (sql-value session-id)
                      (if used-pct (str used-pct) "NULL")
                      (if current-tokens (str (long current-tokens)) "NULL")
                      (if window-size (str (long window-size)) "NULL")
                      (sql-value model-id)
                      (sql-value payload))
        observations (usage/from-snapshot {:agent agent
                                           :observed-at observed-at
                                           :payload payload})
        normalized   (apply str
                            (for [{:keys [event-id schema-version observed-at agent
                                         window used-percentage resets-at]}
                                  observations]
                              (format
                                "INSERT OR IGNORE INTO usage_observations (event_id, schema_version, observed_at, agent, window_key, used_percentage, resets_at, publishable) VALUES (%s,%d,%d,%s,%s,%s,%d,1);"
                                (sql-value event-id)
                                schema-version
                                observed-at
                                (sql-value agent)
                                (sql-value window)
                                (str used-percentage)
                                resets-at)))
        insert       (str "BEGIN;" legacy normalized "COMMIT;")
        fallback-sql (str "PRAGMA busy_timeout=5000; " insert)]
    (try
      (ensure-db-once! path)
      (if-let [{:keys [^java.util.concurrent.BlockingQueue queue]} @writer-state]
        (when-not (.offer queue
                          {:statements
                           (into [[(str "INSERT INTO context_snapshots (agent, session_id, used_pct,"
                                        " current_tokens, window_size, model_id, payload)"
                                        " VALUES (?,?,?,?,?,?,?)")
                                   agent session-id used-pct
                                   (some-> current-tokens long) (some-> window-size long)
                                   model-id payload]]
                                 (for [{:keys [event-id schema-version observed-at agent
                                               window used-percentage resets-at]}
                                       observations]
                                   [(str "INSERT OR IGNORE INTO usage_observations (event_id,"
                                         " schema_version, observed_at, agent, window_key,"
                                         " used_percentage, resets_at, publishable)"
                                         " VALUES (?,?,?,?,?,?,?,1)")
                                    event-id schema-version observed-at agent window
                                    used-percentage resets-at]))})
          (binding [*out* *err*]
            (println "cch.log: writer queue full; dropping context snapshot")))
        (p/process ["sqlite3" path fallback-sql]
                   {:out :discard :err :discard}))
      (catch Exception _ nil))))

(defn latest-context-snapshot
  "Most recent context snapshot for a session. Returns a map or nil."
  [session-id]
  (first
    (db/query
      (first (sql/format {:select   [:used-pct :current-tokens :window-size :model-id :timestamp]
                          :from     [:context-snapshots]
                          :where    [:= :session-id session-id]
                          :order-by [[:id :desc]]
                          :limit    1}
                         {:inline true})))))
