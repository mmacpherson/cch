(ns cch.control.core
  "Agent-agnostic session directory and safe text-message router."
  (:require [cch.control.claude :as claude]
            [cch.control.codex :as codex]
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

(defn list-sessions
  "Return sanitized native presence. One failed adapter does not hide the
  other agent family; adapter errors are returned alongside the list."
  []
  (let [claude-result (safe-call "claude" claude/sessions)
        codex-result  (safe-call "codex" codex/sessions)]
    {:sessions (vec (concat (:sessions claude-result) (:sessions codex-result)))
     :errors (vec (keep :error [claude-result codex-result]))}))

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

(defn send-message!
  "Send text through the target's native input channel. There is deliberately
  no raw-frame or approval-response API."
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
        source (or (not-empty source) (inferred-source))
        content-sha256 (store/content-digest message)]
    (when-not (valid-envelope-label? message-id)
      (throw (ex-info "message_id must be 1-512 characters without control characters"
                      {:type :invalid-message})))
    (when-not (valid-envelope-label? source)
      (throw (ex-info "source must be 1-512 characters without control characters"
                      {:type :invalid-message})))
    (locking delivery-lock
      (if-let [prior (store/delivery message-id)]
        (if (and (= target (:target prior))
                 (= source (:source prior))
                 (= content-sha256 (:content_sha256 prior)))
          {:message-id message-id :source source :target target
           :status "duplicate" :original-status (:status prior)}
          (throw (ex-info "message_id was already used with different content or routing"
                          {:type :message-id-conflict :message-id message-id})))
        (let [result (cond
                       (str/starts-with? target "claude:")
                       (claude/send! {:route-id target :message message
                                      :message-id message-id :source source})

                       (str/starts-with? target "codex:")
                       (codex/send! {:route-id target :message message
                                     :message-id message-id :source source})

                       :else
                       (throw (ex-info (str "Unsupported target route: " target)
                                       {:type :unsupported-agent :target target})))]
          (store/record-delivery! {:message-id message-id
                                   :source source
                                   :target target
                                   :content-sha256 content-sha256
                                   :status (:status result)})
          result)))))
