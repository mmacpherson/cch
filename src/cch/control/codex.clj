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

(defn sessions-with-client [client]
  (->> (rpc! client "thread/list"
             {:limit 200
              :sortKey "updated_at"
              :sortDirection "desc"
              :useStateDbOnly true})
       :data
       ;; A saved rollout is not a running agent. Keep only daemon-owned
       ;; threads; unavailable loaded threads remain visible for diagnosis.
       (remove #(= "notLoaded" (status-name (:status %))))
       (mapv public-thread)))

(defn sessions []
  (call-with-client sessions-with-client))

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
