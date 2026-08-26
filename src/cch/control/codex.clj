(ns cch.control.codex
  "Synchronous JSON-RPC client for Codex's shared app-server daemon.

  The control plane uses only typed app-server methods. It does not spawn or
  wrap agent TUIs and never reads rollout transcript contents."
  (:require [cheshire.core :as json]
            [clojure.string :as str])
  (:import [java.io BufferedReader BufferedWriter InputStreamReader OutputStreamWriter]
           [java.nio.charset StandardCharsets]
           [java.util.concurrent TimeUnit]))

(def ^:dynamic *proxy-command*
  "Command used to connect to the managed shared daemon. Dynamic for tests."
  ["codex" "app-server" "proxy"])

(defrecord Client [^Process process ^BufferedReader reader ^BufferedWriter writer
                   next-id stderr-lines])

(defn- start-stderr-drain! [^Process process stderr-lines]
  (doto
    (Thread.
      ^Runnable
      (fn []
        (try
          (with-open [r (BufferedReader.
                         (InputStreamReader. (.getErrorStream process)
                                             StandardCharsets/UTF_8))]
            (loop []
              (when-let [line (.readLine r)]
                (swap! stderr-lines #(->> (conj % line) (take-last 20) vec))
                (recur))))
          (catch Exception _ nil))))
    (.setDaemon true)
    (.setName "cch-codex-app-server-stderr")
    (.start)))

(defn- write-json! [^Client client value]
  (.write ^BufferedWriter (:writer client) (json/generate-string value))
  (.newLine ^BufferedWriter (:writer client))
  (.flush ^BufferedWriter (:writer client)))

(defn- read-response! [^Client client request-id]
  (loop []
    (if-let [line (.readLine ^BufferedReader (:reader client))]
      (let [message (json/parse-string line true)]
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
          (recur)))
      (let [detail (str/join "\n" @(:stderr-lines client))]
        (throw
          (ex-info
            (str "Codex shared app-server daemon is unavailable"
                 (when-not (str/blank? detail) (str ": " detail))
                 ". Install the standalone Codex distribution and run "
                 "`codex remote-control start` once on this machine.")
            {:type :codex-daemon-unavailable
             :command *proxy-command*}))))))

(defn rpc! [^Client client method params]
  (locking client
    (let [id (swap! (:next-id client) inc)]
      (write-json! client {:id id :method method :params params})
      (read-response! client id))))

(defn start-client!
  ([] (start-client! *proxy-command*))
  ([command]
   (let [process (try
                   (.start (ProcessBuilder. ^java.util.List (vec command)))
                   (catch Exception e
                     (throw (ex-info
                              (str "Cannot start Codex app-server proxy: "
                                   (.getMessage e))
                              {:type :codex-daemon-unavailable
                               :command command}
                              e))))
         stderr-lines (atom [])
         client (->Client process
                          (BufferedReader.
                            (InputStreamReader. (.getInputStream process)
                                                StandardCharsets/UTF_8))
                          (BufferedWriter.
                            (OutputStreamWriter. (.getOutputStream process)
                                                 StandardCharsets/UTF_8))
                          (atom 0) stderr-lines)]
     (start-stderr-drain! process stderr-lines)
     (try
       (rpc! client "initialize"
             {:clientInfo {:name "cch-control-plane" :version "0.1.0"}
              :capabilities {:experimentalApi true
                             :optOutNotificationMethods []}})
       (write-json! client {:method "initialized" :params {}})
       client
       (catch Exception e
         (try (.close ^BufferedWriter (:writer client)) (catch Exception _ nil))
         (.destroy process)
         (throw e))))))

(defn close-client! [^Client client]
  (when client
    (try (.close ^BufferedWriter (:writer client)) (catch Exception _ nil))
    (when-not (.waitFor ^Process (:process client) 500 TimeUnit/MILLISECONDS)
      (.destroy ^Process (:process client)))
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
  {:id (str "codex:" (:id thread))
   :agent "codex"
   :native-id (:id thread)
   :name (or (:name thread) (:agentNickname thread))
   :cwd (:cwd thread)
   :status (status-name (:status thread))
   :available (true? (:canAcceptDirectInput thread))
   :cli-version (:cliVersion thread)
   :source (:source thread)
   :unavailable-reason
   (when-not (true? (:canAcceptDirectInput thread))
     "thread is not loaded by the shared app-server daemon")})

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
