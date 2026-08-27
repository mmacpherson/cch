(ns cch.control.core
  "Agent-agnostic session directory and safe text-message router."
  (:require [cch.control.claude :as claude]
            [cch.control.codex :as codex]
            [cch.control.remote :as remote]
            [cch.control.store :as store]
            [clojure.string :as str])
  (:import [java.nio.charset StandardCharsets]
           [java.util UUID]))

(def ^:const max-message-bytes (* 32 1024))
(defonce ^:private delivery-lock (Object.))

(defn- safe-call [agent f]
  (try
    {:sessions (f)}
    (catch Exception e
      {:sessions []
       :error {:agent agent
               :message (.getMessage e)
               :type (some-> e ex-data :type name)}})))

(defn list-local-sessions
  "Return sanitized native presence. One failed adapter does not hide the
  other agent family; adapter errors are returned alongside the list."
  []
  (let [claude-result (safe-call "claude" claude/sessions)
        codex-result  (safe-call "codex" codex/sessions)]
    {:sessions (vec (concat (:sessions claude-result) (:sessions codex-result)))
     :errors (vec (keep :error [claude-result codex-result]))}))

(defn- remote-presence []
  (when-let [config (remote/config-from-env)]
    (try
      {:config config :sessions (remote/sessions config)}
      (catch Exception error
        {:config config
         :sessions []
         :error {:agent "broker"
                 :message (.getMessage error)
                 :type (some-> error ex-data :type name)}}))))

(defn list-sessions
  "Return local native presence plus active routes leased by other paired
  runners. Broker failure is diagnostic only and never hides local sessions."
  []
  (let [local (list-local-sessions)
        local-ids (set (map :id (:sessions local)))
        remote-result (remote-presence)
        remote-sessions (->> (:sessions remote-result)
                             (remove #(or (contains? local-ids (:id %))
                                          (= (get-in remote-result [:config :runner-id])
                                             (:runner-id %))))
                             (map #(assoc % :location "remote")))]
    {:sessions (vec (concat (:sessions local) remote-sessions))
     :errors (cond-> (:errors local)
               (:error remote-result) (conj (:error remote-result)))}))

(defn get-session [route-id]
  (some #(when (= route-id (:id %)) %) (:sessions (list-sessions))))

(defn inferred-source []
  (cond
    (not (str/blank? (System/getenv "CLAUDE_CODE_SESSION_ID")))
    (str "claude:" (System/getenv "CLAUDE_CODE_SESSION_ID"))

    (not (str/blank? (System/getenv "CLAUDE_CODE_MESSAGING_SOCKET")))
    (or (store/claude-route-for-socket
          (System/getenv "CLAUDE_CODE_MESSAGING_SOCKET"))
        "claude:unregistered")

    (not (str/blank? (System/getenv "CODEX_THREAD_ID")))
    (str "codex:" (System/getenv "CODEX_THREAD_ID"))

    :else "operator"))

(defn- valid-envelope-label? [value]
  (and (string? value)
       (<= 1 (count value) 512)
       (not (re-find #"[\p{Cntrl}]" value))))

(defn- validate-envelope
  [{:keys [target message source message-id]}]
  (when (str/blank? target)
    (throw (ex-info "target is required" {:type :invalid-message})))
  (when (str/blank? message)
    (throw (ex-info "message is required" {:type :invalid-message})))
  (when-not (valid-envelope-label? target)
    (throw (ex-info "target must be 1-512 characters without control characters"
                    {:type :invalid-message})))
  (when (> (alength (.getBytes ^String message StandardCharsets/UTF_8))
           max-message-bytes)
    (throw (ex-info (str "message exceeds " max-message-bytes " UTF-8 bytes")
                    {:type :message-too-large :max-bytes max-message-bytes})))
  (let [message-id (or message-id (str (UUID/randomUUID)))
        source (or (not-empty source) (inferred-source))]
    (when-not (valid-envelope-label? message-id)
      (throw (ex-info "message_id must be 1-512 characters without control characters"
                      {:type :invalid-message})))
    (when-not (valid-envelope-label? source)
      (throw (ex-info "source must be 1-512 characters without control characters"
                      {:type :invalid-message})))
    {:target target :message message :source source :message-id message-id
     :content-sha256 (store/content-digest message)}))

(defn- with-delivery-dedupe!
  [{:keys [target source message-id content-sha256]} deliver!]
  (locking delivery-lock
    (if-let [prior (store/delivery message-id)]
      (if (and (= target (:target prior))
               (= source (:source prior))
               (= content-sha256 (:content_sha256 prior)))
        {:message-id message-id :source source :target target
         :status "duplicate" :original-status (:status prior)}
        (throw (ex-info "message_id was already used with different content or routing"
                        {:type :message-id-conflict :message-id message-id})))
      (let [result (deliver!)]
        (store/record-delivery! {:message-id message-id
                                 :source source
                                 :target target
                                 :content-sha256 content-sha256
                                 :status (:status result)})
        result))))

(defn- with-remote-delivery-dedupe!
  "Validate duplicate identity locally, but still replay an identical envelope
  to the disposable broker. A live broker suppresses it; a restarted in-memory
  broker can accept it again, while destination-side dedupe prevents a second
  native submission."
  [{:keys [target source message-id content-sha256]} deliver!]
  (locking delivery-lock
    (if-let [prior (store/delivery message-id)]
      (if (and (= target (:target prior))
               (= source (:source prior))
               (= content-sha256 (:content_sha256 prior)))
        (deliver!)
        (throw (ex-info "message_id was already used with different content or routing"
                        {:type :message-id-conflict :message-id message-id})))
      (let [result (deliver!)]
        (store/record-delivery! {:message-id message-id
                                 :source source
                                 :target target
                                 :content-sha256 content-sha256
                                 :status (:status result)})
        result))))

(defn send-local-message!
  "Send only through a machine-local native adapter. The paired runner uses
  this entrypoint so a broker delivery can never bounce back to the broker."
  [request]
  (let [{:keys [target message source message-id] :as envelope}
        (validate-envelope request)]
    (with-delivery-dedupe!
      envelope
      #(cond
         (str/starts-with? target "claude:")
         (claude/send! {:route-id target :message message
                        :message-id message-id :source source})

         (str/starts-with? target "codex:")
         (codex/send! {:route-id target :message message
                       :message-id message-id :source source})

         :else
         (throw (ex-info (str "Unsupported target route: " target)
                         {:type :unsupported-agent :target target}))))))

(defn- remote-target
  [config target]
  (try
    (some #(when (and (= target (:id %))
                      (not= (:runner-id config) (:runner-id %)))
             %)
          (remote/sessions config))
    (catch Exception _ nil)))

(defn send-message!
  "Route plain text locally when the target is native to this machine, or
  through the optional paired broker when another runner owns it. There is
  deliberately no raw-frame or approval-response API."
  [request]
  (let [{:keys [target message source message-id] :as envelope}
        (validate-envelope request)
        local-ids (set (map :id (:sessions (list-local-sessions))))
        config (remote/config-from-env)]
    (if (or (contains? local-ids target)
            (nil? config)
            (nil? (remote-target config target)))
      (send-local-message! (select-keys envelope
                                        [:target :message :source :message-id]))
      (with-remote-delivery-dedupe!
        envelope
        #(remote/enqueue! config
                          {:target target :message message
                           :source source :message-id message-id})))))
