(ns cch.log
  "SQLite event logging via fire-and-forget sqlite3 CLI.

  Every hook invocation is logged as a row in ~/.local/share/cch/events.db.
  The sqlite3 process is spawned non-blocking (~1.3ms) so it doesn't
  delay the hook response."
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
    :session-id, :decision, :reason, :elapsed-ms"
  [{:keys [hook-name event-type tool-name file-path cwd
           session-id decision reason elapsed-ms]}]
  (let [path (db-path)
        sql  (format
               "INSERT INTO events (session_id, hook_name, event_type, tool_name, file_path, cwd, decision, reason, elapsed_ms) VALUES (%s,%s,%s,%s,%s,%s,%s,%s,%s);"
               (sql-value session-id)
               (sql-value hook-name)
               (sql-value event-type)
               (sql-value tool-name)
               (sql-value file-path)
               (sql-value cwd)
               (sql-value (when decision (name decision)))
               (sql-value reason)
               (sql-value elapsed-ms))]
    (try
      (ensure-db! path)
      (p/process ["sqlite3" path sql]
                 {:out :inherit :err :inherit})
      (catch Exception _e
        nil))))

(defn query-events
  "Query recent events. Returns a seq of maps.
  opts: :limit, :hook, :session, :decision, :since"
  [& {:keys [limit hook session decision since]}]
  (let [limit   (or limit 20)
        path    (db-path)
        wheres  (cond-> []
                  hook     (conj (format "hook_name = '%s'" (escape-sql hook)))
                  session  (conj (format "session_id = '%s'" (escape-sql session)))
                  decision (conj (format "decision = '%s'" (escape-sql decision)))
                  since    (conj (format "timestamp > '%s'" (escape-sql since))))
        where   (if (seq wheres)
                  (str " WHERE " (str/join " AND " wheres))
                  "")
        sql     (format "SELECT id,timestamp,session_id,hook_name,event_type,tool_name,file_path,cwd,decision,reason,elapsed_ms FROM events%s ORDER BY id DESC LIMIT %d;"
                        where limit)
        result  (p/sh ["sqlite3" "-json" path sql])]
    (when (zero? (:exit result))
      (let [out (str/trim (:out result))]
        (when-not (str/blank? out)
          (json/parse-string out true))))))
