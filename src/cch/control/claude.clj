(ns cch.control.claude
  "Claude Code native session discovery, registration, and inbox delivery."
  (:require [babashka.fs :as fs]
            [cch.control.store :as store]
            [cch.subprocess :as subprocess]
            [cheshire.core :as json]
            [clojure.string :as str])
  (:import [java.net StandardProtocolFamily UnixDomainSocketAddress]
           [java.nio.channels Channels SocketChannel]
           [java.nio.charset StandardCharsets]))

(def ^:const minimum-version "2.1.224")

(defn- socket-pid [socket-path]
  (some->> socket-path
           (re-find #"(?:^|[/\\])(\d+)\.sock$")
           second
           parse-long))

(defn register-from-hook!
  "Read a Claude SessionStart payload and register the session's native inbox.
  The socket and token come from Claude's documented hook environment."
  ([payload] (register-from-hook! payload (System/getenv)))
  ([payload env]
   (let [session-id  (or (:session_id payload) (:sessionId payload))
         cwd         (:cwd payload)
         socket-path (get env "CLAUDE_CODE_MESSAGING_SOCKET")
         auth-token  (get env "CLAUDE_CODE_MESSAGING_TOKEN")]
     (when (str/blank? (str session-id))
       (throw (ex-info "Claude SessionStart payload has no session_id"
                       {:type :invalid-registration})))
     (when (str/blank? socket-path)
       (throw (ex-info
                (str "Claude did not export CLAUDE_CODE_MESSAGING_SOCKET; "
                     "cross-session messaging requires Claude Code " minimum-version "+")
                {:type :unsupported-claude-version :minimum minimum-version})))
     (store/upsert-claude! {:session-id session-id
                            :cwd cwd
                            :name (:name payload)
                            :socket-path socket-path
                            :auth-token auth-token
                            :pid (socket-pid socket-path)}))))

(defn native-sessions
  "Ask Claude's supported session registry for current presence."
  []
  (try
    (let [{:keys [exit out err]} (subprocess/run ["claude" "agents" "--json"])]
      (if (zero? exit)
        (json/parse-string out true)
        (throw (ex-info (str "claude agents failed: " (str/trim err))
                        {:type :claude-discovery-failed :exit exit}))))
    (catch Exception e
      (if (:type (ex-data e))
        (throw e)
        (throw (ex-info "Claude CLI discovery is unavailable"
                        {:type :claude-discovery-failed} e))))))

(defn sessions
  "Merge registered inbox addresses with Claude's live-session view. Secrets
  are omitted. A stale registration remains visible with available=false so a
  restart failure is explicit rather than silently losing the target."
  []
  (let [live-by-id (into {} (map (juxt :sessionId identity)) (native-sessions))]
    (mapv
      (fn [{:keys [route_id native_id cwd name socket_path pid updated_at]}]
        (let [live (get live-by-id native_id)
              socket-live? (and socket_path (fs/exists? socket_path))]
          {:id route_id
           :agent "claude"
           :native-id native_id
           :name (or (:name live) name)
           :cwd (or (:cwd live) cwd)
           :pid (or (:pid live) pid)
           :status (or (:status live) "stale")
           :available (boolean (and live socket-live?))
           :updated-at updated_at
           :unavailable-reason
           (when-not (and live socket-live?)
             (if live "registered inbox socket no longer exists"
                 "session is no longer reported by claude agents"))}))
      (store/claude-sessions))))

(defn send!
  "Inject a normal user message through Claude's native inbox. This function
  has no parameter for control responses, permissions, or raw protocol frames."
  [{:keys [route-id message message-id source]}]
  (let [{:keys [socket_path auth_token native_id] :as session}
        (store/claude-session route-id)]
    (when-not session
      (throw (ex-info (str "Unknown Claude session: " route-id)
                      {:type :unknown-session :target route-id})))
    (when-not (and socket_path (fs/exists? socket_path))
      (throw (ex-info (str "Claude inbox is stale: " route-id)
                      {:type :stale-session :target route-id})))
    (let [frame {:type "user"
                 :session_id native_id
                 :uuid message-id
                 :msg_id message-id
                 :priority "next"
                 :message {:role "user"
                           :content (str "[cch message from " source
                                         "; id " message-id "]\n" message)}}]
      (with-open [channel (SocketChannel/open StandardProtocolFamily/UNIX)]
        (.connect channel (UnixDomainSocketAddress/of socket_path))
        (let [writer (Channels/newWriter channel (.newEncoder StandardCharsets/UTF_8) -1)]
          (when-not (str/blank? auth_token)
            (.write writer (str (json/generate-string {:type "auth" :token auth_token}) "\n")))
          (.write writer (str (json/generate-string frame) "\n"))
          (.flush writer)))
      {:message-id message-id :source source :target route-id
       :transport "claude-native-inbox" :status "submitted"})))
