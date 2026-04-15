(ns cch.log
  "SQLite event logging via fire-and-forget sqlite3 CLI.

  Every hook invocation is logged as a row in ~/.local/share/cch/events.db.
  The sqlite3 process is spawned with `p/process` (returns immediately, no
  wait on exit) so the INSERT itself doesn't block the hook response.

  What DOES happen on the hot path: `ProcessBuilder.start` to fork+exec the
  sqlite3 binary. Measured warm-JVM cost in bench/log_overhead.clj:
    p50 ~3ms  p95 ~6ms  p99 ~8ms  (per wrap-logging call)
  That's the fork+exec itself; ensure-db! adds ~3μs (warm stat) and is
  cached via delay so repeated calls are free.

  Under sustained load, async sqlite3 processes can pile up and serialize
  on the DB lock (PRAGMA busy_timeout=5000). That's fine for interactive
  hook firing but is a known weakness for bursty high-volume use — a
  future background-queue optimization is tracked separately."
  (:require [babashka.process :as p]
            [babashka.fs :as fs]
            [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(defn db-path
  "Returns the SQLite database path, respecting XDG_DATA_HOME."
  []
  (str (or (System/getenv "XDG_DATA_HOME")
           (str (System/getProperty "user.home") "/.local/share"))
       "/cch/events.db"))

(defn ensure-db!
  "Create the database and schema if they don't exist."
  [path]
  (let [dir (fs/parent path)]
    (when-not (fs/exists? dir)
      (fs/create-dirs dir))
    (when-not (fs/exists? path)
      (let [schema (slurp (io/resource "schema.sql"))]
        (p/sh ["sqlite3" path schema])))))

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

(defn log-event!
  "Fire-and-forget: spawn sqlite3 to insert an event row. Non-blocking.

  event is a map with keys:
    :hook-name, :event-type, :tool-name, :file-path, :cwd,
    :session-id, :decision, :reason, :elapsed-ms, :extra

  When CCH_LOG_SYNC=1 in the environment, runs sqlite3 synchronously via
  p/sh instead of fire-and-forget. Used by tests that need to assert on
  rows immediately after a hook invocation."
  [{:keys [hook-name event-type tool-name file-path cwd
           session-id decision reason elapsed-ms extra]}]
  (let [path (db-path)
        sync? (= "1" (System/getenv "CCH_LOG_SYNC"))
        sql  (format
               ;; busy_timeout lets concurrent hook runs serialize instead of
               ;; SQLITE_BUSY-failing. PRAGMA is per-connection and each
               ;; sqlite3 CLI call gets its own, so set it inline every time.
               "PRAGMA busy_timeout=5000; INSERT INTO events (session_id, hook_name, event_type, tool_name, file_path, cwd, decision, reason, elapsed_ms, extra) VALUES (%s,%s,%s,%s,%s,%s,%s,%s,%s,%s);"
               (sql-value session-id)
               (sql-value hook-name)
               (sql-value event-type)
               (sql-value tool-name)
               (sql-value file-path)
               (sql-value cwd)
               (sql-value (when decision (name decision)))
               (sql-value reason)
               (sql-value elapsed-ms)
               (sql-value extra))]
    (try
      (ensure-db-once! path)
      (if sync?
        (p/sh ["sqlite3" path sql])
        ;; Discard subprocess stdio — inheriting would corrupt the hook's
        ;; JSON response on stdout that Claude Code parses.
        (p/process ["sqlite3" path sql]
                   {:out :discard :err :discard}))
      (catch Exception _e
        nil))))

(defn- sqlite-json
  "Run `sql` against the events DB via `sqlite3 -json`. Returns parsed
  keyword-keyed maps on success, nil when the query produces no rows."
  [sql]
  (let [path   (db-path)
        result (p/sh ["sqlite3" "-json" path sql])]
    (when (zero? (:exit result))
      (let [out (str/trim (:out result))]
        (when-not (str/blank? out)
          (json/parse-string out true))))))

(defn distinct-cwds
  "All distinct cwd values in the events table, in arbitrary order.
  Done as a SQL DISTINCT — avoids pulling the full table into memory
  for the dashboard's Repo dropdown. Returns a seq of strings."
  []
  (->> (sqlite-json "SELECT DISTINCT cwd FROM events WHERE cwd IS NOT NULL;")
       (keep :cwd)))

(defn recent-sessions
  "Return up to `limit` most-recently-active session IDs as
  [{:session_id :timestamp} ...] pairs, optionally filtered to
  sessions whose events landed under `cwd-prefix`. Server-side
  grouping + sort, no post-filtering in Clojure."
  [& {:keys [limit cwd-prefix] :or {limit 30}}]
  (let [where (if (str/blank? cwd-prefix)
                ""
                (str " WHERE cwd LIKE '" (escape-sql cwd-prefix) "%'"))
        sql   (str "SELECT session_id, max(timestamp) AS timestamp "
                   "FROM events"
                   where
                   " GROUP BY session_id "
                   "ORDER BY timestamp DESC "
                   "LIMIT " (int limit) ";")]
    (->> (sqlite-json sql)
         (filter :session_id))))

(defn query-events
  "Query recent events. Returns a seq of maps.
  opts: :limit, :hook, :event, :session, :decision, :since, :cwd-prefix"
  [& {:keys [limit hook event session decision since cwd-prefix]}]
  (let [limit   (or limit 20)
        path    (db-path)
        wheres  (cond-> []
                  hook       (conj (format "hook_name = '%s'" (escape-sql hook)))
                  event      (conj (format "event_type = '%s'" (escape-sql event)))
                  session    (conj (format "session_id = '%s'" (escape-sql session)))
                  decision   (conj (format "decision = '%s'" (escape-sql decision)))
                  since      (conj (format "timestamp > '%s'" (escape-sql since)))
                  cwd-prefix (conj (format "cwd LIKE '%s%%'" (escape-sql cwd-prefix))))
        where   (if (seq wheres)
                  (str " WHERE " (str/join " AND " wheres))
                  "")
        sql     (format "SELECT id,timestamp,session_id,hook_name,event_type,tool_name,file_path,cwd,decision,reason,elapsed_ms,extra FROM events%s ORDER BY id DESC LIMIT %d;"
                        where limit)
        result  (p/sh ["sqlite3" "-json" path sql])]
    (when (zero? (:exit result))
      (let [out (str/trim (:out result))]
        (when-not (str/blank? out)
          (json/parse-string out true))))))
