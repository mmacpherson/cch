(ns cch.control.codex
  "Synchronous JSON-RPC client for Codex's shared app-server daemon.

  The control plane uses only typed app-server methods. It does not spawn or
  wrap agent TUIs and never reads rollout transcript contents."
  (:require [cch.control.unix-websocket :as websocket]
            [cheshire.core :as json]
            [clojure.string :as str])
  (:import [java.io EOFException]))

(defn- codex-home []
  (let [configured (System/getenv "CODEX_HOME")]
    (if (str/blank? configured)
      (str (System/getProperty "user.home") "/.codex")
      configured)))

(defn- default-socket-path []
  (str (codex-home) "/app-server-control/app-server-control.sock"))

(def ^:dynamic *connect!*
  "Unix WebSocket connection boundary. Dynamic for protocol tests."
  websocket/connect!)

(defrecord Client [transport next-id])

(defn- write-json! [^Client client value]
  (websocket/send-text! (:transport client) (json/generate-string value)))

(defn- read-response! [^Client client request-id]
  (loop []
    (let [message (json/parse-string
                    (websocket/read-text! (:transport client)) true)]
      (if (= request-id (:id message))
        (if-let [error (:error message)]
          (throw (ex-info (str "Codex app-server request failed: "
                               (or (:message error) (pr-str error)))
                          {:type :codex-rpc-error
                           :request-id request-id
                           :error error}))
          (:result message))
        ;; Notifications and server requests are intentionally ignored by
        ;; this narrow POC client. Its methods cannot trigger approvals.
        (recur)))))

(defn rpc! [^Client client method params]
  (locking client
    (let [id (swap! (:next-id client) inc)]
      (write-json! client {:id id :method method :params params})
      (read-response! client id))))

(defn start-client!
  ([] (start-client! (default-socket-path)))
  ([socket-path]
   (let [transport (try
                     (*connect!* socket-path)
                     (catch Exception error
                       (throw
                         (ex-info
                           (str "Codex shared app-server daemon is unavailable: "
                                (.getMessage error)
                                ". Run `cch control install` to install and start "
                                "the local systemd user service, or start `codex "
                                "app-server --listen unix://` without Remote Control.")
                           {:type :codex-daemon-unavailable
                            :socket-path socket-path}
                           error))))
         client (->Client transport (atom 0))]
     (try
       (rpc! client "initialize"
             {:clientInfo {:name "cch-control-plane" :version "0.1.0"}
              :capabilities {:experimentalApi true
                             :optOutNotificationMethods []}})
       (write-json! client {:method "initialized" :params {}})
       client
       (catch Exception error
         (websocket/close! transport)
         (if (instance? EOFException error)
           (throw (ex-info "Codex app-server closed during initialization"
                           {:type :codex-daemon-unavailable
                            :socket-path socket-path}
                           error))
           (throw error)))))))

(defn close-client! [^Client client]
  (when client
    (websocket/close! (:transport client))
    nil))

(defn- call-with-client [f]
  (let [client (start-client!)]
    (try (f client) (finally (close-client! client)))))

(defn- status-name [status]
  (cond
    (string? status) status
    (map? status) (or (:type status) "unknown")
    :else "unknown"))

(defn- public-thread [thread]
  (let [direct-input (:canAcceptDirectInput thread)
        ;; Ordinary CLI threads report null for this experimental capability
        ;; but thread/queue/add accepts them. Only false identifies a V2
        ;; subagent owned by its parent and forbidden from direct input.
        available? (not= false direct-input)]
    {:id (str "codex:" (:id thread))
     :agent "codex"
     :native-id (:id thread)
     :name (or (:name thread) (:agentNickname thread))
     :cwd (:cwd thread)
     :status (status-name (:status thread))
     :available available?
     :cli-version (:cliVersion thread)
     :source (:source thread)
     :unavailable-reason
     (when-not available?
       "thread is a parent-owned subagent and rejects direct input")}))

(defn- loaded-thread-ids
  "Enumerate the app-server's currently loaded (live) thread ids. Codex 0.151+
  reports running threads only through `thread/loaded/list`; `thread/list`
  returns state-DB rollouts, which are all `notLoaded` and therefore exclude
  live agent sessions. The listing is cursor-paginated."
  [client]
  (loop [cursor nil ids []]
    (let [{:keys [data nextCursor]}
          (rpc! client "thread/loaded/list"
                (cond-> {} cursor (assoc :cursor cursor)))
          ids (into ids data)]
      (if (and nextCursor (seq data))
        (recur nextCursor ids)
        ids))))

(defn- read-thread
  "Fetch one loaded thread's detail. A thread can unload between listing and
  read; treat that race as gone rather than failing the whole refresh."
  [client id]
  (try
    (:thread (rpc! client "thread/read" {:threadId id}))
    (catch clojure.lang.ExceptionInfo _ nil)))

(defn sessions-with-client [client]
  (->> (loaded-thread-ids client)
       (keep #(read-thread client %))
       (mapv public-thread)))

(defn sessions []
  (call-with-client sessions-with-client))

(def ^:private cch-tool-names
  #{"list_sessions" "get_session" "send_message" "set_session_alias"})

(defn refresh-mcp!
  "Ask the shared Codex app-server to reload its MCP runtimes, then verify that
  cch came back with its deliberately narrow tool surface. The upstream reload
  request is app-server-wide; it does not restart the daemon or attached agent
  clients, but may briefly reconnect other MCP servers owned by that daemon."
  []
  (call-with-client
    (fn [client]
      (rpc! client "config/mcpServer/reload" nil)
      (let [servers (:data (rpc! client "mcpServerStatus/list"
                                 {:detail "toolsAndAuthOnly"}))
            cch-server (some #(when (= "cch" (:name %)) %) servers)
            tools (set (map name (keys (:tools cch-server))))
            missing (sort (remove tools cch-tool-names))
            unexpected (sort (remove cch-tool-names tools))]
        (when-not cch-server
          (throw (ex-info
                   "Codex reloaded MCP servers, but the cch server is unavailable"
                   {:type :codex-mcp-refresh-failed
                    :server "cch"})))
        (when (seq missing)
          (throw (ex-info
                   (str "Codex reloaded cch, but expected tools are missing: "
                        (str/join ", " missing))
                   {:type :codex-mcp-refresh-failed
                    :server "cch"
                    :missing-tools missing})))
        (when (seq unexpected)
          (throw (ex-info
                   (str "Codex reloaded cch, but unexpected tools are present: "
                        (str/join ", " unexpected))
                   {:type :codex-mcp-refresh-failed
                    :server "cch"
                    :unexpected-tools unexpected})))
        {:agent "codex"
         :server "cch"
         :status "refreshed"
         :tools (sort cch-tool-names)}))))

(defn send!
  "Queue normal text input to an active Codex thread. The app-server's
  clientUserMessageId is the caller's stable id, providing native deduping."
  [{:keys [route-id message message-id source]}]
  (let [native-id (some-> route-id (str/replace-first #"^codex:" ""))]
    (when (= native-id route-id)
      (throw (ex-info (str "Invalid Codex route: " route-id)
                      {:type :invalid-route :target route-id})))
    (call-with-client
      (fn [client]
        (let [session (some #(when (= route-id (:id %)) %) (sessions-with-client client))]
          (when-not session
            (throw (ex-info (str "Unknown or unloaded Codex session: " route-id)
                            {:type :unknown-session :target route-id})))
          (when-not (:available session)
            (throw (ex-info (str "Codex session cannot accept direct input: " route-id)
                            {:type :stale-session :target route-id})))
          (rpc! client "thread/queue/add"
                {:threadId native-id
                 :clientUserMessageId message-id
                 :input [{:type "text"
                          :text (str "[cch message from " source
                                     "; id " message-id "]\n" message)}]})
          {:message-id message-id :source source :target route-id
           :transport "codex-app-server" :status "submitted"})))))
