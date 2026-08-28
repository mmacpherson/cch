(ns cch.control.store
  "SQLite persistence for the local half of the native control plane.

  Native inbox credentials are deliberately machine-local. Public APIs return
  sanitized session maps, and delivery history stores a content hash rather
  than message text."
  (:require [babashka.fs :as fs]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs])
  (:import [java.math BigInteger]
           [java.nio.charset StandardCharsets]
           [java.security MessageDigest]))

(def ^:const path-property "cch.control.db.path")

(defn db-path
  "Separate operational store; intentionally outside events.db federation."
  []
  (or (System/getProperty path-property)
      (str (or (System/getenv "XDG_DATA_HOME")
               (str (System/getProperty "user.home") "/.local/share"))
           "/cch/control.db")))

(defn- datasource []
  {:dbtype "sqlite" :dbname (db-path)})

(def ^:private schema
  [(str "CREATE TABLE IF NOT EXISTS control_sessions ("
        "route_id TEXT PRIMARY KEY, agent TEXT NOT NULL, native_id TEXT NOT NULL,"
        "cwd TEXT, name TEXT, socket_path TEXT, auth_token TEXT, pid INTEGER,"
        "transcript_path TEXT, native_url TEXT,"
        "registered_at INTEGER NOT NULL, updated_at INTEGER NOT NULL)")
   (str "CREATE UNIQUE INDEX IF NOT EXISTS idx_control_sessions_native "
        "ON control_sessions(agent, native_id)")
   (str "CREATE TABLE IF NOT EXISTS control_deliveries ("
        "message_id TEXT PRIMARY KEY, source TEXT NOT NULL, target TEXT NOT NULL,"
        "content_sha256 TEXT NOT NULL, status TEXT NOT NULL, created_at INTEGER NOT NULL)")
   (str "CREATE TABLE IF NOT EXISTS control_codex_bindings ("
        "tool_use_id TEXT PRIMARY KEY, source TEXT NOT NULL, target TEXT NOT NULL,"
        "message_id TEXT, content_sha256 TEXT NOT NULL, created_at INTEGER NOT NULL,"
        "consumed_at INTEGER)")])

(defonce ^:private ensured-paths (atom #{}))

(defn- ensure-column! [ds table-name column-name column-ddl]
  (let [columns (jdbc/execute! ds [(str "PRAGMA table_info(" table-name ")")]
                               {:builder-fn rs/as-unqualified-maps})]
    (when-not (some #(= column-name (:name %)) columns)
      (jdbc/execute! ds [(str "ALTER TABLE " table-name
                              " ADD COLUMN " column-ddl)]))))

(defn- restrict-db-permissions! [path]
  (when-not (= "Windows" (System/getProperty "os.name"))
    (try
      (java.nio.file.Files/setPosixFilePermissions
        (java.nio.file.Path/of path (make-array String 0))
        (java.util.Set/of java.nio.file.attribute.PosixFilePermission/OWNER_READ
                          java.nio.file.attribute.PosixFilePermission/OWNER_WRITE))
      (catch UnsupportedOperationException _ nil))))

(defn ensure-db!
  "Create the local operational DB and restrict the file to its owner."
  []
  (let [path (db-path)]
    (when-not (contains? @ensured-paths path)
      (when-let [parent (fs/parent path)]
        (fs/create-dirs parent))
      (let [ds (jdbc/get-datasource (datasource))]
        (doseq [ddl schema]
          (jdbc/execute! ds [ddl]))
        ;; control.db predates migration bookkeeping. These additive, nullable
        ;; columns safely upgrade existing owner-local stores in place.
        (ensure-column! ds "control_sessions" "transcript_path"
                        "transcript_path TEXT")
        (ensure-column! ds "control_sessions" "native_url"
                        "native_url TEXT"))
      (restrict-db-permissions! path)
      (swap! ensured-paths conj path))
    path))

(defn- execute! [sql-params]
  (ensure-db!)
  (jdbc/execute! (jdbc/get-datasource (datasource)) sql-params
                 {:builder-fn rs/as-unqualified-maps}))

(defn content-digest
  "SHA-256 for dedupe and source-binding records. Message bodies are never
  persisted in the operational database."
  [value]
  (let [digest (.digest (MessageDigest/getInstance "SHA-256")
                        (.getBytes ^String value StandardCharsets/UTF_8))]
    (format "%064x" (BigInteger. 1 digest))))

(defn upsert-claude!
  [{:keys [session-id cwd name socket-path auth-token pid transcript-path]}]
  (let [now (System/currentTimeMillis)
        route-id (str "claude:" session-id)]
    (execute!
      [(str "INSERT INTO control_sessions "
            "(route_id, agent, native_id, cwd, name, socket_path, auth_token, pid, transcript_path, registered_at, updated_at) "
            "VALUES (?, 'claude', ?, ?, ?, ?, ?, ?, ?, ?, ?) "
            "ON CONFLICT(route_id) DO UPDATE SET "
            "cwd=excluded.cwd, name=COALESCE(excluded.name, control_sessions.name), "
            "socket_path=excluded.socket_path, auth_token=excluded.auth_token, "
            "pid=excluded.pid, "
            "transcript_path=COALESCE(excluded.transcript_path, control_sessions.transcript_path), "
            "updated_at=excluded.updated_at")
       route-id session-id cwd name socket-path auth-token pid transcript-path now now])
    route-id))

(defn set-claude-native-url!
  "Cache one provider-validated deep link in the owner-local operational DB."
  [route-id native-url]
  (execute!
    ["UPDATE control_sessions SET native_url=?, updated_at=? WHERE route_id=? AND agent='claude'"
     native-url (System/currentTimeMillis) route-id])
  native-url)

(defn claude-sessions []
  (or (execute!
        ["SELECT route_id, agent, native_id, cwd, name, socket_path, pid, transcript_path, native_url, registered_at, updated_at FROM control_sessions WHERE agent='claude' ORDER BY updated_at DESC"])
      []))

(defn claude-session
  "Return the internal row for a route, including its local inbox credential."
  [route-id]
  (first
    (execute!
      ["SELECT * FROM control_sessions WHERE route_id=? AND agent='claude'" route-id])))

(defn claude-route-for-socket [socket-path]
  (some->
    (first
      (execute!
        ["SELECT route_id FROM control_sessions WHERE agent='claude' AND socket_path=?"
         socket-path]))
    :route_id))

(defn delivery [message-id]
  (first
    (execute!
      ["SELECT message_id, source, target, content_sha256, status, created_at FROM control_deliveries WHERE message_id=?"
       message-id])))

(defn record-delivery!
  [{:keys [message-id source target content-sha256 status]}]
  (execute!
    ["INSERT OR IGNORE INTO control_deliveries (message_id, source, target, content_sha256, status, created_at) VALUES (?, ?, ?, ?, ?, ?)"
     message-id source target content-sha256 status (System/currentTimeMillis)])
  (delivery message-id))

(defn record-codex-binding!
  "Record the trusted Codex hook observation that precedes one cch MCP call.
  Only routing metadata and a message digest cross this boundary."
  [{:keys [tool-use-id session-id target message-id message]}]
  (let [now (System/currentTimeMillis)]
    ;; Binding rows are immutable: replaying a hook event must never make a
    ;; consumed proof valid again.
    (execute! ["DELETE FROM control_codex_bindings WHERE created_at<?"
               (- now (* 24 60 60 1000))])
    (let [result (first
                   (execute!
                     [(str "INSERT OR IGNORE INTO control_codex_bindings "
                           "(tool_use_id, source, target, message_id, content_sha256, created_at, consumed_at) "
                           "VALUES (?, ?, ?, ?, ?, ?, NULL)")
                      tool-use-id (str "codex:" session-id) target message-id
                      (content-digest message) now]))]
      (if (= 1 (:next.jdbc/update-count result))
        tool-use-id
        (throw (ex-info "Codex source binding already exists"
                        {:type :duplicate-codex-binding}))))))

(defn claim-codex-binding!
  "Atomically consume a recent binding matching the MCP envelope. Returns the
  authoritative Codex route, or nil for a missing, expired, mismatched, or
  replayed proof."
  [{:keys [tool-use-id target message-id message max-age-ms]
    :or {max-age-ms (* 30 60 1000)}}]
  (let [now (System/currentTimeMillis)
        result (first
                 (execute!
                   [(str "UPDATE control_codex_bindings SET consumed_at=? "
                         "WHERE tool_use_id=? AND target=? "
                         "AND ((message_id IS NULL AND ? IS NULL) OR message_id=?) "
                         "AND content_sha256=? AND consumed_at IS NULL AND created_at>=?")
                    now tool-use-id target message-id message-id
                    (content-digest message) (- now max-age-ms)]))]
    (when (= 1 (:next.jdbc/update-count result))
      (:source
        (first
          (execute!
            ["SELECT source FROM control_codex_bindings WHERE tool_use_id=?"
             tool-use-id]))))))
