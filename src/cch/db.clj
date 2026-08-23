(ns cch.db
  "SQLite read access for server-context callers (forecast, log queries,
  config-db reads). Uses next.jdbc against a read-only connection to
  events.db. SQLite WAL mode means this read path coexists with the
  cch.log writer subprocess without locking contention.

  --- SQL style: raw strings vs HoneySQL ---

  `query` takes a finished SQL string; how you build that string is a
  deliberate, per-query choice, not an accident of who wrote it:

    * RAW SQL STRINGS for complex, STATIC analytic queries — the forecast
      window CTEs (window functions, monotone/bucketing passes), the
      recent-session NOT EXISTS walk. These are hand-tuned and read far more
      clearly as SQL than as nested HoneySQL data; there is nothing dynamic to
      compose. Interpolate only trusted, escaped values (see agent-clause /
      escape-sql-literal) — never raw user input.

    * HONEYSQL (`honey.sql/format`) for DYNAMIC queries assembled from a
      variable set of filters — cch.stats/stat-counts and cch.log/query-events
      bolt on optional WHERE clauses with `cond->`. Building those by string
      concatenation is exactly the injection-prone tedium HoneySQL removes.

  Rule of thumb: if the SQL shape is fixed, write SQL; if the shape varies with
  the arguments, build it with HoneySQL. Don't port the forecast CTEs into
  HoneySQL (it's worse), and don't hand-concatenate dynamic WHERE clauses."
  (:require [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]))

(def ^:const path-property
  "System property overriding the database location.

  A property rather than an environment variable because the JVM cannot
  set its own env, so a test run has no way to redirect itself — and
  relying on the caller to export XDG_DATA_HOME first means any
  invocation that skips the wrapper writes to the real database. The
  test alias sets this in :jvm-opts, so isolation holds however tests
  are started."
  "cch.db.path")

(defn db-path
  "Returns the SQLite database path.

  Precedence: the cch.db.path system property, then XDG_DATA_HOME, then
  ~/.local/share."
  []
  (or (System/getProperty path-property)
      (str (or (System/getenv "XDG_DATA_HOME")
               (str (System/getProperty "user.home") "/.local/share"))
           "/cch/events.db")))

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
