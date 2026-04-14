(ns cch.config-db
  "CRUD for the hook_config table.

  Rows describe whether a given hook is enabled at a given scope:
    hook_name  — matches a cli.registry entry
    scope      — 'global' or 'repo:<abs-path>'
    enabled    — 0 / 1
    options    — JSON blob, hook-specific (nullable)

  Mirrors cch.log's sqlite3 CLI pattern — synchronous p/sh here because
  config reads/writes are UI-initiated, not hot-path. If CRUD latency
  ever matters we can share cch.log's queue.

  Repo scope format: 'repo:<abs-path>'. The absolute path must already
  be canonicalized by the caller (fs/canonicalize) so equality checks
  don't split hairs on symlinks."
  (:require [babashka.process :as p]
            [cch.log :as log]
            [cheshire.core :as json]
            [clojure.string :as str]))

(def ^:const global-scope "global")

(defn repo-scope
  "Build the scope string for a repo root path."
  [repo-root]
  (str "repo:" repo-root))

(defn- escape [s]
  (when s (str/replace (str s) "'" "''")))

(defn- sql-lit [v]
  (if (nil? v) "NULL" (str "'" (escape v) "'")))

(defn upsert!
  "Insert or update a row for (hook-name, scope). Options is a Clojure
  map serialized to JSON. Synchronous; returns nil.

  Callers should pre-canonicalize repo paths (not done here to keep
  this fn pure about I/O)."
  [{:keys [hook-name scope enabled options]}]
  (let [path    (log/db-path)
        opts    (when options (json/generate-string options))
        enabled (if enabled 1 0)
        sql     (format
                  "INSERT INTO hook_config (hook_name, scope, enabled, options) VALUES (%s, %s, %d, %s) ON CONFLICT(hook_name, scope) DO UPDATE SET enabled=excluded.enabled, options=excluded.options, updated_at=strftime('%%Y-%%m-%%dT%%H:%%M:%%f','now');"
                  (sql-lit hook-name)
                  (sql-lit scope)
                  enabled
                  (sql-lit opts))]
    (log/ensure-db! path)
    (p/sh ["sqlite3" path sql])
    nil))

(defn delete!
  "Remove a config row. No-op if not present."
  [hook-name scope]
  (let [path (log/db-path)
        sql  (format "DELETE FROM hook_config WHERE hook_name = %s AND scope = %s;"
                     (sql-lit hook-name) (sql-lit scope))]
    (log/ensure-db! path)
    (p/sh ["sqlite3" path sql])
    nil))

(defn- parse-row
  "Decode JSON options and normalize enabled to a boolean."
  [{:keys [hook_name scope enabled options updated_at]}]
  {:hook-name  hook_name
   :scope      scope
   :enabled    (= 1 enabled)
   :options    (when (and options (not (str/blank? options)))
                 (try (json/parse-string options true)
                      (catch Exception _ nil)))
   :updated-at updated_at})

(defn list-all
  "All rows, ordered by hook then scope. Returns a seq of maps."
  []
  (let [path   (log/db-path)
        sql    "SELECT hook_name, scope, enabled, options, updated_at FROM hook_config ORDER BY hook_name, scope;"
        result (p/sh ["sqlite3" "-json" path sql])]
    (when (zero? (:exit result))
      (let [out (str/trim (:out result))]
        (when-not (str/blank? out)
          (map parse-row (json/parse-string out true)))))))

(defn list-for-scope
  "Rows belonging to one scope."
  [scope]
  (let [path   (log/db-path)
        sql    (format "SELECT hook_name, scope, enabled, options, updated_at FROM hook_config WHERE scope = %s ORDER BY hook_name;"
                       (sql-lit scope))
        result (p/sh ["sqlite3" "-json" path sql])]
    (when (zero? (:exit result))
      (let [out (str/trim (:out result))]
        (when-not (str/blank? out)
          (map parse-row (json/parse-string out true)))))))

(defn get-row
  "Look up a single row. Returns nil if absent."
  [hook-name scope]
  (let [path   (log/db-path)
        sql    (format "SELECT hook_name, scope, enabled, options, updated_at FROM hook_config WHERE hook_name = %s AND scope = %s LIMIT 1;"
                       (sql-lit hook-name) (sql-lit scope))
        result (p/sh ["sqlite3" "-json" path sql])]
    (when (zero? (:exit result))
      (let [out (str/trim (:out result))]
        (when-not (str/blank? out)
          (first (map parse-row (json/parse-string out true))))))))
