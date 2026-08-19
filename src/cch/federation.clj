(ns cch.federation
  "Cross-machine event-log federation — core (see claude-code-hooks-n55).

  Each node keeps its local SQLite as the fast write buffer. A background
  shipper (cch.federation.ship) periodically POSTs new append-only rows to
  a collector node's /ingest endpoint, which idempotently unions them by
  (node, origin_id). This namespace holds the parts with no network deps:
  node identity, config parsing, the collector-side idempotent insert, and
  the shipper watermark.

  Deployment specifics — collector URL, token, node-name override — live in
  ~/.config/cch/config.yaml under a `federation:` section, never in the
  repo. Example (values illustrative):

    federation:
      node: my-laptop                       # optional; defaults to hostname
      collector-url: http://collector:8888  # a node ships here when set
      collector: true                       # this machine accepts /ingest
      token: some-shared-secret             # optional; enforced iff set
      interval-seconds: 60

  Transport is expected to run over a private network (e.g. Tailscale): the
  collector binds `cch serve` to its tailnet interface, so /ingest is
  reachable only by the operator's own devices. The optional bearer token
  is defense-in-depth, not the primary gate.

  Intentionally does NOT require cch.config: cch.config-db requires cch.log,
  and cch.log requires this namespace for node stamping, so routing through
  cch.config would form a load cycle. The federation section lives in the
  same global config file; we just read it directly."
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [cch.db :as db]
            [clj-yaml.core :as yaml]
            [clojure.string :as str]))

;; --- Global config (read directly to avoid a cch.config load cycle) ---

(defn- global-config-path []
  (str (or (System/getenv "XDG_CONFIG_HOME")
           (str (System/getProperty "user.home") "/.config"))
       "/cch/config.yaml"))

(defn- load-global-config
  "Parsed global config map, or nil if missing/malformed. Federation must
  never crash the write path on a bad config, so parse errors → nil."
  []
  (let [path (global-config-path)]
    (when (fs/exists? path)
      (try (yaml/parse-string (slurp path) :keywords true)
           (catch Exception _ nil)))))

(defn- hostname []
  (or (try (.getHostName (java.net.InetAddress/getLocalHost))
           (catch Exception _ nil))
      (some-> (System/getenv "HOSTNAME") not-empty)
      "unknown"))

(defn federation-config
  "Pure: normalize a parsed global-config map into federation settings.

    {:node          this machine's node id (federation.node or hostname)
     :collector-url where a node ships (nil → shipping off)
     :token         optional shared secret
     :interval-ms   ship cadence
     :collector?    true → this machine accepts /ingest
     :enabled?      true → shipping is on (collector-url present & not disabled)}"
  [global-cfg]
  (let [f             (:federation global-cfg)
        collector-url (some-> (:collector-url f) str not-empty)]
    {:node          (or (some-> (:node f) str not-empty) (hostname))
     :collector-url collector-url
     :token         (some-> (:token f) str not-empty)
     :interval-ms   (* 1000 (max 5 (or (:interval-seconds f) 60)))
     :collector?    (true? (:collector f))
     :enabled?      (and (some? collector-url) (not= false (:enabled f)))}))

(defn load-federation-config
  "Read + normalize the federation config from the global config file."
  []
  (federation-config (load-global-config)))

(def node-name
  "This machine's federation node id. Memoized — computed once per process,
  so the write path pays nothing after the first call."
  (memoize #(:node (load-federation-config))))

;; --- Authorization (collector side) ---

(defn authorized?
  "Collector-side check. With no configured token the network is the gate,
  so everything passes. With a token configured, the request must carry a
  matching `Bearer <token>` Authorization header."
  [configured-token auth-header]
  (or (str/blank? configured-token)
      (= auth-header (str "Bearer " configured-token))))

;; --- Idempotent ingest (collector side) ---

(def ^:private ingest-columns
  "Per-table column list carried over the wire, excluding the collector's
  own autoincrement id (which is local) and node/origin_id (handled
  specially). Doubles as the whitelist of shippable table names."
  {"events" [:timestamp :agent :session_id :hook_name :event_type :tool_name
             :file_path :cwd :decision :reason :elapsed_ms :extra]
   "context_snapshots" [:timestamp :agent :session_id :used_pct :current_tokens
                        :window_size :model_id :payload]})

(defn known-table? [table] (contains? ingest-columns table))

(defn- sqlv
  "Format a value as a SQLite literal: nil→NULL, numbers bare, else a
  single-quoted, escaped string."
  [v]
  (cond
    (nil? v)    "NULL"
    (number? v) (str v)
    :else       (str "'" (str/replace (str v) "'" "''") "'")))

(defn ingest-sql
  "Pure: build one idempotent multi-row INSERT for `rows` into `table`.
  node comes from each row's :node; origin_id from its :id (the source's
  local id). INSERT OR IGNORE + UNIQUE(node, origin_id) makes re-sending a
  batch a no-op. Returns nil for an unknown table or empty rows."
  [table rows]
  (when (and (known-table? table) (seq rows))
    (let [cols     (ingest-columns table)
          col-list (str/join ", " (map name (concat [:node :origin_id] cols)))
          row->vals (fn [r]
                      (str "("
                           (str/join ", "
                                     (concat [(sqlv (:node r)) (sqlv (:id r))]
                                             (map #(sqlv (get r %)) cols)))
                           ")"))]
      (str "PRAGMA busy_timeout=5000; INSERT OR IGNORE INTO " table
           " (" col-list ") VALUES "
           (str/join ", " (map row->vals rows)) ";"))))

(defn ingest-rows!
  "Collector side: idempotently insert `rows` received from another node
  into `table` in the local DB. No-op for unknown table / empty rows.
  Returns the count of rows submitted (not necessarily inserted, since
  duplicates are ignored).

  The batch INSERT is fed to sqlite3 over stdin, not as a command-line
  argument: a full batch easily exceeds the OS ARG_MAX and fails with
  E2BIG (Argument list too long) when passed as argv."
  [table rows]
  (if-let [sql (ingest-sql table rows)]
    (do (p/sh ["sqlite3" (db/db-path)] {:in sql})
        (count rows))
    0))

;; --- Shipper watermark (node side) ---

(defn get-watermark
  "Highest local id of `table` already shipped to the collector; 0 if none."
  [table]
  (or (some-> (db/query (str "SELECT last_shipped_id FROM federation_offsets"
                             " WHERE table_name='" table "';"))
              first :last_shipped_id)
      0))

(defn set-watermark!
  "Persist `id` as the high-water mark for `table`."
  [table id]
  (when (known-table? table)
    (p/sh ["sqlite3" (db/db-path)
           (str "PRAGMA busy_timeout=5000;"
                " INSERT INTO federation_offsets(table_name,last_shipped_id) VALUES('"
                table "'," (long id) ")"
                " ON CONFLICT(table_name) DO UPDATE SET last_shipped_id=" (long id)
                ", updated_at=strftime('%Y-%m-%dT%H:%M:%f','now');")])))

(def ship-batch
  "Max rows per POST. Bounds the multi-row INSERT and the request size."
  500)

(defn rows-after
  "Up to `limit` rows of `table` with id greater than `watermark`, ascending."
  [table watermark limit]
  (when (known-table? table)
    (db/query (str "SELECT * FROM " table " WHERE id > " (long watermark)
                   " ORDER BY id ASC LIMIT " (int limit) ";"))))
