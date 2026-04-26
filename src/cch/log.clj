(ns cch.log
  "SQLite event logging.

  Two write paths:

    1. **Background writer (preferred).** When `start-writer!` has been
       called (cch serve does this in start!), inserts are queued onto a
       bounded LinkedBlockingQueue and a daemon thread pipes them to one
       long-lived `sqlite3` subprocess via stdin. Per-call cost is just
       the queue offer + a few-byte write — sub-millisecond. WAL is
       enabled on the writer's connection so the dashboard's read
       queries don't block writes.

    2. **Per-call fallback.** When no writer is registered (legacy
       `bb -m hooks.X` subprocess invocation, tests without setup),
       log-event! spawns a fresh `sqlite3` per insert via `p/process`.
       Measured at p50 ~3ms / p99 ~8ms — same as before.

  CCH_LOG_SYNC=1 forces synchronous `p/sh` regardless of the writer,
  so tests can assert on rows immediately after a log call."
  (:require [babashka.process :as p]
            [babashka.fs :as fs]
            [cch.db :as db]
            [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]))


(defn ensure-db!
  "Create the database directory if needed and apply the schema.
  All CREATE statements use IF NOT EXISTS so this is idempotent —
  safe to run on every startup even when the DB already exists."
  [path]
  (let [dir (fs/parent path)]
    (when-not (fs/exists? dir)
      (fs/create-dirs dir))
    (let [schema (slurp (io/resource "schema.sql"))]
      (p/sh ["sqlite3" path schema]))))

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
  "Escape a string for SQLite single-quoted literals."
  [s]
  (when s
    (str/replace (str s) "'" "''")))

(defn- sql-value
  "Format a value for SQL insertion. nil becomes NULL, strings get single-quoted."
  [v]
  (if (nil? v)
    "NULL"
    (str "'" (escape-sql (str v)) "'")))

;; --- Background writer ---

;; When non-nil, holds {:proc <bb-process> :queue <BlockingQueue> :thread <Thread>}.
;; Set by start-writer!, cleared by stop-writer!. log-event! checks this to
;; decide between queued and per-call modes.
(defonce ^:private writer-state (atom nil))

(def ^:private writer-queue-capacity 4096)

(defn- writer-loop
  "Drain queue → write SQL to sqlite3 stdin. Runs on a daemon thread.
  Sets a one-time PRAGMA before the loop. ::stop sentinel breaks out."
  [^java.io.OutputStream out ^java.util.concurrent.BlockingQueue queue]
  (try
    (.write out (.getBytes "PRAGMA journal_mode=WAL;\n" "UTF-8"))
    (.flush out)
    (loop []
      (let [item (.take queue)]
        (when-not (identical? item ::stop)
          (try
            (.write out (.getBytes (str item "\n") "UTF-8"))
            (.flush out)
            (catch Exception _ nil))
          (recur))))
    (catch InterruptedException _ nil)
    (catch Exception _ nil)))

(defn start-writer!
  "Spin up the background writer. Idempotent — no-op if already running.
  Returns the writer-state map for callers that want to introspect."
  []
  (or @writer-state
      (let [path  (db/db-path)
            _     (ensure-db-once! path)
            proc  (p/process ["sqlite3" path]
                             {:in :write :out :discard :err :discard})
            queue (java.util.concurrent.LinkedBlockingQueue. writer-queue-capacity)
            thread (Thread. ^Runnable #(writer-loop (:in proc) queue))]
        (.setDaemon thread true)
        (.setName thread "cch-sqlite-writer")
        (.start thread)
        (reset! writer-state {:proc proc :queue queue :thread thread}))))

(defn stop-writer!
  "Shut the writer down. Sends a stop sentinel so any queued events
  drain before stdin is closed. Idempotent."
  []
  (when-let [{:keys [proc queue thread]} @writer-state]
    (try (.put queue ::stop) (catch Exception _ nil))
    (try (.join thread 1000) (catch Exception _ nil))
    (try (.close ^java.io.Closeable (:in proc)) (catch Exception _ nil))
    (reset! writer-state nil)))

;; --- log-event! ---

(defn log-event!
  "Insert an event row. Non-blocking.

  event is a map with keys:
    :hook-name, :event-type, :tool-name, :file-path, :cwd,
    :session-id, :decision, :reason, :elapsed-ms, :extra

  Path selection:
    - CCH_LOG_SYNC=1     → synchronous p/sh (test backdoor)
    - writer running     → enqueue SQL onto the background writer (~µs)
    - otherwise          → fire-and-forget per-call sqlite3 subprocess"
  [{:keys [hook-name event-type tool-name file-path cwd
           session-id decision reason elapsed-ms extra]}]
  (let [path  (db/db-path)
        sync? (= "1" (System/getenv "CCH_LOG_SYNC"))
        ;; Insert SQL. PRAGMA busy_timeout matters only on the per-call
        ;; fallback path (concurrent sqlite3 procs); the writer thread
        ;; sets WAL once at startup and is the sole writer.
        insert (format
                 "INSERT INTO events (session_id, hook_name, event_type, tool_name, file_path, cwd, decision, reason, elapsed_ms, extra) VALUES (%s,%s,%s,%s,%s,%s,%s,%s,%s,%s);"
                 (sql-value session-id)
                 (sql-value hook-name)
                 (sql-value event-type)
                 (sql-value tool-name)
                 (sql-value file-path)
                 (sql-value cwd)
                 (sql-value (when decision (name decision)))
                 (sql-value reason)
                 (sql-value elapsed-ms)
                 (sql-value extra))
        fallback-sql (str "PRAGMA busy_timeout=5000; " insert)]
    (try
      (ensure-db-once! path)
      (cond
        sync?
        (p/sh ["sqlite3" path fallback-sql])

        @writer-state
        (let [{:keys [^java.util.concurrent.BlockingQueue queue]} @writer-state]
          (when-not (.offer queue insert)
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
  (let [fmt (requiring-resolve 'honey.sql/format)]
    (->> (db/query (first (fmt {:select-distinct [:cwd] :from [:events]
                                :where [:!= :cwd nil]}
                               {:inline true})))
         (keep :cwd))))

(defn recent-sessions
  "Return up to `limit` most-recently-active session IDs as
  [{:session_id :timestamp} ...] pairs, optionally filtered to
  sessions whose events landed under `cwd-prefix`. Server-side
  grouping + sort, no post-filtering in Clojure."
  [& {:keys [limit cwd-prefix] :or {limit 30}}]
  (let [q (cond-> {:select [:session-id [[:max :timestamp] :timestamp]]
                   :from   [:events]
                   :group-by [:session-id]
                   :order-by [[:timestamp :desc]]
                   :limit (int limit)}
            (not (str/blank? cwd-prefix))
            (assoc :where [:like :cwd (str cwd-prefix "%")]))]
    (let [fmt (requiring-resolve 'honey.sql/format)]
      (->> (db/query (first (fmt q {:inline true})))
           (filter :session_id)))))

(defn query-events
  "Query recent events. Returns a seq of maps.
  opts: :limit, :hook, :event, :session, :decision, :since, :cwd-prefix"
  [& {:keys [limit hook event session decision since cwd-prefix]}]
  (let [cols [:id :timestamp :session-id :hook-name :event-type
              :tool-name :file-path :cwd :decision :reason :elapsed-ms :extra]
        q    (cond-> {:select   cols
                      :from     [:events]
                      :order-by [[:id :desc]]
                      :limit    (or limit 20)}
               hook       (update :where (fnil conj [:and]) [:= :hook-name hook])
               event      (update :where (fnil conj [:and]) [:= :event-type event])
               session    (update :where (fnil conj [:and]) [:= :session-id session])
               decision   (update :where (fnil conj [:and]) [:= :decision decision])
               since      (update :where (fnil conj [:and]) [:> :timestamp since])
               cwd-prefix (update :where (fnil conj [:and]) [:like :cwd (str cwd-prefix "%")]))]
    (let [fmt (requiring-resolve 'honey.sql/format)]
      (db/query (first (fmt q {:inline true}))))))

;; --- Context snapshots ---

(defn log-context-snapshot!
  "Insert a context window snapshot. Non-blocking, same write-path as log-event!."
  [{:keys [session-id used-pct current-tokens window-size model-id payload]}]
  (let [path   (db/db-path)
        insert (format
                 "INSERT INTO context_snapshots (session_id, used_pct, current_tokens, window_size, model_id, payload) VALUES (%s,%s,%s,%s,%s,%s);"
                 (sql-value session-id)
                 (if used-pct (str used-pct) "NULL")
                 (if current-tokens (str (long current-tokens)) "NULL")
                 (if window-size (str (long window-size)) "NULL")
                 (sql-value model-id)
                 (sql-value payload))
        fallback-sql (str "PRAGMA busy_timeout=5000; " insert)]
    (try
      (ensure-db-once! path)
      (if-let [{:keys [^java.util.concurrent.BlockingQueue queue]} @writer-state]
        (when-not (.offer queue insert)
          (binding [*out* *err*]
            (println "cch.log: writer queue full; dropping context snapshot")))
        (p/process ["sqlite3" path fallback-sql]
                   {:out :discard :err :discard}))
      (catch Exception _ nil))))

(defn latest-context-snapshot
  "Most recent context snapshot for a session. Returns a map or nil."
  [session-id]
  (let [fmt (requiring-resolve 'honey.sql/format)]
    (first
      (db/query
        (first (fmt {:select   [:used-pct :current-tokens :window-size :model-id :timestamp]
                     :from     [:context-snapshots]
                     :where    [:= :session-id session-id]
                     :order-by [[:id :desc]]
                     :limit    1}
                    {:inline true}))))))
