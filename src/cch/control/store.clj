(ns cch.control.store
  "SQLite persistence for the local half of the native control plane.

  Native inbox credentials are deliberately machine-local. Public APIs return
  sanitized session maps, and delivery history stores a content hash rather
  than message text."
  (:require [babashka.fs :as fs]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]))

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
        "registered_at INTEGER NOT NULL, updated_at INTEGER NOT NULL)")
   (str "CREATE UNIQUE INDEX IF NOT EXISTS idx_control_sessions_native "
        "ON control_sessions(agent, native_id)")
   (str "CREATE TABLE IF NOT EXISTS control_deliveries ("
        "message_id TEXT PRIMARY KEY, source TEXT NOT NULL, target TEXT NOT NULL,"
        "content_sha256 TEXT NOT NULL, status TEXT NOT NULL, created_at INTEGER NOT NULL)")])

(defonce ^:private ensured-paths (atom #{}))

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
          (jdbc/execute! ds [ddl])))
      (restrict-db-permissions! path)
      (swap! ensured-paths conj path))
    path))

(defn- execute! [sql-params]
  (ensure-db!)
  (jdbc/execute! (jdbc/get-datasource (datasource)) sql-params
                 {:builder-fn rs/as-unqualified-maps}))

(defn upsert-claude!
  [{:keys [session-id cwd name socket-path auth-token pid]}]
  (let [now (System/currentTimeMillis)
        route-id (str "claude:" session-id)]
    (execute!
      [(str "INSERT INTO control_sessions "
            "(route_id, agent, native_id, cwd, name, socket_path, auth_token, pid, registered_at, updated_at) "
            "VALUES (?, 'claude', ?, ?, ?, ?, ?, ?, ?, ?) "
            "ON CONFLICT(route_id) DO UPDATE SET "
            "cwd=excluded.cwd, name=COALESCE(excluded.name, control_sessions.name), "
            "socket_path=excluded.socket_path, auth_token=excluded.auth_token, "
            "pid=excluded.pid, updated_at=excluded.updated_at")
       route-id session-id cwd name socket-path auth-token pid now now])
    route-id))

(defn claude-sessions []
  (or (execute!
        ["SELECT route_id, agent, native_id, cwd, name, socket_path, pid, registered_at, updated_at FROM control_sessions WHERE agent='claude' ORDER BY updated_at DESC"])
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
