(ns cch.db
  "SQLite read access for server-context callers (forecast, log queries,
  config-db reads). Uses next.jdbc against a read-only connection to
  events.db. SQLite WAL mode means this read path coexists with the
  cch.log writer subprocess without locking contention."
  (:require [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]))

(defn db-path
  "Returns the SQLite database path, respecting XDG_DATA_HOME."
  []
  (str (or (System/getenv "XDG_DATA_HOME")
           (str (System/getProperty "user.home") "/.local/share"))
       "/cch/events.db"))

(defn- jdbc-spec
  "Build a fresh spec on each call so tests that redef db-path see the
   change. SQLite JDBC datasources are cheap to construct."
  []
  ;; mode=rwc lets the spec also work for a not-yet-created DB during
  ;; test setup before any writer has run; the writer subprocess in
  ;; cch.log remains the actual schema-creator at the application level.
  {:dbtype "sqlite" :dbname (db-path)})

(defn open-db!
  "Reserved for future explicit init (e.g. when this becomes a pooled
   datasource). Currently a no-op — datasources are created per query."
  [])

(defn close-db!
  "Symmetric counterpart to open-db!; also a no-op today."
  [])

(defn query
  "Run a SQL query string and return rows as a vector of unqualified
   keyword-keyed maps (matching the previous shell-out output shape so
   callers don't change). Returns nil on empty results or when the DB
   file is missing."
  [sql]
  (try
    (let [rows (jdbc/execute! (jdbc/get-datasource (jdbc-spec)) [sql]
                              {:builder-fn rs/as-unqualified-maps})]
      (when (seq rows) (vec rows)))
    (catch java.sql.SQLException _ nil)))
