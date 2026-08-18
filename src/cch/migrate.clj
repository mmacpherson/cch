(ns cch.migrate
  "Tiny in-code migration registry for the SQLite events DB.

  Why not migratus / honeysql-migrations? cch's schema fits on one page,
  and we already shell out to the `sqlite3` CLI for every write. A 50-line
  in-process registry keeps the dependency footprint at zero and the
  control flow obvious.

  Contract:

    - `schema.sql` is the source of truth for what a fresh table looks
      like — CREATE TABLE IF NOT EXISTS is run on every `ensure-db!`.
    - `migrations` carries ordered deltas that upgrade pre-existing DBs
      to match the current schema.sql. Append-only.
    - Each migration runs at most once per DB; `schema_migrations`
      tracks which `:id`s have been applied.
    - `:baseline-probe` exists so DBs that pre-date this registry — and
      already received the effect via ad-hoc DDL — get marked applied
      without re-running ALTER (which would fail with 'duplicate column').

  Adding a migration:
    1. Update schema.sql so fresh installs get the new shape.
    2. Append `{:id \"NNNN-slug\" :up \"...SQL...\"}` to `migrations`.
       Omit `:baseline-probe` — only the registry-predating ones need it."
  (:require [babashka.process :as p]
            [clojure.string :as str]))

(defn- column-exists?
  "True if the named column exists on the given table in this DB."
  [path table col]
  (let [res (p/sh ["sqlite3" path
                   (format "SELECT 1 FROM pragma_table_info('%s') WHERE name='%s';"
                           table col)])]
    (str/includes? (or (:out res) "") "1")))

(def ^:private migrations
  "Ordered. Append-only."
  [{:id "0001-events-agent"
    :up (str "ALTER TABLE events ADD COLUMN agent TEXT NOT NULL DEFAULT 'claude-code';"
             "CREATE INDEX IF NOT EXISTS idx_events_agent ON events(agent);")
    :baseline-probe (fn [path] (column-exists? path "events" "agent"))}
   {:id "0002-context-snapshots-agent"
    :up (str "ALTER TABLE context_snapshots ADD COLUMN agent TEXT NOT NULL DEFAULT 'claude-code';"
             "CREATE INDEX IF NOT EXISTS idx_ctx_agent ON context_snapshots(agent);")
    :baseline-probe (fn [path] (column-exists? path "context_snapshots" "agent"))}
   ;; 0003/0004 add columns that schema.sql also creates for fresh installs,
   ;; so a fresh DB already has them — the baseline-probe records the
   ;; migration as applied without re-running the ALTER (which would fail
   ;; with 'duplicate column name').
   {:id "0003-events-federation"
    :up (str "ALTER TABLE events ADD COLUMN node TEXT;"
             "ALTER TABLE events ADD COLUMN origin_id INTEGER;"
             "CREATE INDEX IF NOT EXISTS idx_events_node ON events(node);"
             "CREATE UNIQUE INDEX IF NOT EXISTS idx_events_node_origin ON events(node, origin_id);")
    :baseline-probe (fn [path] (column-exists? path "events" "node"))}
   {:id "0004-context-snapshots-federation"
    :up (str "ALTER TABLE context_snapshots ADD COLUMN node TEXT;"
             "ALTER TABLE context_snapshots ADD COLUMN origin_id INTEGER;"
             "CREATE INDEX IF NOT EXISTS idx_ctx_node ON context_snapshots(node);"
             "CREATE UNIQUE INDEX IF NOT EXISTS idx_ctx_node_origin ON context_snapshots(node, origin_id);")
    :baseline-probe (fn [path] (column-exists? path "context_snapshots" "node"))}
   {:id "0005-federation-offsets"
    :up (str "CREATE TABLE IF NOT EXISTS federation_offsets ("
             "  table_name      TEXT PRIMARY KEY,"
             "  last_shipped_id INTEGER NOT NULL DEFAULT 0,"
             "  updated_at      TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%f','now'))"
             ");")}])

(defn migration-ids
  "Ordered ids of every defined migration. Public so tests and
  introspection don't hardcode the set."
  []
  (mapv :id migrations))

(defn- ensure-tracking-table! [path]
  (p/sh ["sqlite3" path
         (str "CREATE TABLE IF NOT EXISTS schema_migrations ("
              "  id         TEXT PRIMARY KEY,"
              "  applied_at TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%f','now'))"
              ");")]))

(defn applied-ids
  "Set of migration `:id`s already recorded in `schema_migrations`."
  [path]
  (let [{:keys [out]} (p/sh ["sqlite3" path "SELECT id FROM schema_migrations;"])]
    (->> (str/split-lines (or out ""))
         (map str/trim)
         (remove str/blank?)
         set)))

(defn- record! [path id]
  (p/sh ["sqlite3" path
         (format "INSERT OR IGNORE INTO schema_migrations(id) VALUES('%s');" id)]))

(defn apply-all!
  "Bring `path`'s schema up to current. Idempotent — runs each unapplied
  migration once. Pre-existing DBs whose effects already match a migration
  are baselined (recorded without re-running) via `:baseline-probe`."
  [path]
  (ensure-tracking-table! path)
  (let [applied (applied-ids path)]
    (doseq [{:keys [id up baseline-probe]} migrations
            :when (not (applied id))]
      (cond
        (and baseline-probe (baseline-probe path))
        (record! path id)

        :else
        (do (p/sh ["sqlite3" path up])
            (record! path id))))))
