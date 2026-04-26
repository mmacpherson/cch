(ns cch.config-db
  "CRUD for the hook_config table.

  Rows describe whether a given hook is enabled at a given scope:
    hook_name  — matches a cli.registry entry
    scope      — 'global' or 'repo:<abs-path>'
    enabled    — 0 / 1
    options    — JSON blob, hook-specific (nullable)

  Writes (upsert!, delete!) use the sqlite3 CLI. Reads (list-all,
  list-for-scope, get-row) use cch.db/query, which shares the
  persistent pod connection in server context and falls back to the
  sqlite3 CLI otherwise.

  Repo scope format: 'repo:<abs-path>'. The absolute path must already
  be canonicalized by the caller (fs/canonicalize) so equality checks
  don't split hairs on symlinks."
  (:require [babashka.process :as p]
            [cch.db :as db]
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
  (let [path    (db/db-path)
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
  (let [path (db/db-path)
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
  (some->> (db/query "SELECT hook_name, scope, enabled, options, updated_at FROM hook_config ORDER BY hook_name, scope;")
           (map parse-row)))

(defn list-for-scope
  "Rows belonging to one scope."
  [scope]
  (some->> (db/query (format "SELECT hook_name, scope, enabled, options, updated_at FROM hook_config WHERE scope = %s ORDER BY hook_name;"
                             (sql-lit scope)))
           (map parse-row)))

(defn get-row
  "Look up a single row. Returns nil if absent."
  [hook-name scope]
  (some-> (db/query (format "SELECT hook_name, scope, enabled, options, updated_at FROM hook_config WHERE hook_name = %s AND scope = %s LIMIT 1;"
                            (sql-lit hook-name) (sql-lit scope)))
          first
          parse-row))
