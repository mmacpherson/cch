(ns cch.db
  "Persistent read-only SQLite connection for server-context readers.

  Call open-db! once at server startup to hold a go-sqlite3 pod connection.
  All callers (forecast, log queries, config-db reads) share that connection
  via query. Falls back to shelling out to sqlite3 when no connection is open
  (CLI context, tests). The hook hot path never calls this namespace."
  (:require [babashka.process :as p]
            [clojure.string :as str]))

(defn db-path
  "Returns the SQLite database path, respecting XDG_DATA_HOME."
  []
  (str (or (System/getenv "XDG_DATA_HOME")
           (str (System/getProperty "user.home") "/.local/share"))
       "/cch/events.db"))

(def ^:private conn (atom nil))

(defn open-db!
  "Load the go-sqlite3 pod and open a persistent read-only connection.
   Call once at server startup."
  []
  (require '[babashka.pods :as pods])
  ((resolve 'babashka.pods/load-pod) 'org.babashka/go-sqlite3 "0.3.13")
  (require '[pod.babashka.go-sqlite3])
  (let [get-conn (resolve 'pod.babashka.go-sqlite3/get-connection)
        q        (resolve 'pod.babashka.go-sqlite3/query)
        c        (get-conn (str "file:" (db-path) "?mode=ro"))]
    (reset! conn c)
    (q c "SELECT 1")))

(defn close-db!
  "Close the persistent connection. Call at server shutdown."
  []
  (when-let [c @conn]
    (when-let [close-fn (resolve 'pod.babashka.go-sqlite3/close-connection)]
      (close-fn c))
    (reset! conn nil)))

(defn query
  "Run a SQL query. Uses the persistent pod connection if available,
   otherwise falls back to shelling out to sqlite3."
  [sql]
  (if-let [c @conn]
    (let [q (resolve 'pod.babashka.go-sqlite3/query)]
      (q c sql))
    (let [result (p/sh ["sqlite3" "-json" (db-path) sql])]
      (when (and (zero? (:exit result))
                 (not (str/blank? (:out result))))
        (let [parse (requiring-resolve 'cheshire.core/parse-string)]
          (parse (:out result) true))))))
