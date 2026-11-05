(ns cch.control.core
  "Agent-agnostic session directory and safe text-message router."
  (:require [cch.control.claude :as claude]
            [cch.control.codex :as codex]
            [cch.control.naming :as naming]
            [cch.control.remote :as remote]
            [cch.control.runner-health :as runner-health]
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
    {:sessions (->> (concat (:sessions claude-result) (:sessions codex-result))
                    (mapv naming/present-session))
     :errors (vec (keep :error [claude-result codex-result]))}))

(defn- remote-presence []
  (when-let [config (remote/config)]
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

(defn set-session-alias!
  "Set a broker-visible presentation alias for one machine-local route. The
  paired broker independently verifies that this runner owns the active route."
  [{:keys [route-id alias]}]
  (let [alias (naming/normalize-alias alias)
        local-ids (set (map :id (:sessions (list-local-sessions))))
        config (remote/config)]
    (when-not (contains? local-ids route-id)
      (throw (ex-info "Only the current machine's active sessions can rename themselves"
                      {:type :unknown-session :target route-id})))
    (when-not config
      (throw (ex-info "Session aliases require a paired control runner"
                      {:type :runner-not-paired})))
    (remote/set-session-alias! config route-id alias)))

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
  [{:keys [target message source message-id seq in-reply-to]}]
  (when (str/blank? target)
    (throw (ex-info "target is required" {:type :invalid-message})))
  (when (str/blank? message)
    (throw (ex-info "message is required" {:type :invalid-message})))
  (when (str/starts-with? (str/triml message) "/")
    (throw (ex-info "command-mode input is not allowed through send_message"
                    {:type :command-mode-not-allowed})))
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
    (when (and (some? in-reply-to) (not (valid-envelope-label? in-reply-to)))
      (throw (ex-info "in_reply_to must be 1-512 characters without control characters"
                      {:type :invalid-message})))
    (when (and (some? seq) (not (pos-int? seq)))
      (throw (ex-info "seq must be a positive integer" {:type :invalid-message})))
    (cond-> {:target target :message message :source source :message-id message-id
             :content-sha256 (store/content-digest message)}
      seq (assoc :seq seq)
      in-reply-to (assoc :in-reply-to in-reply-to))))

(defn- with-delivery-dedupe!
  [{:keys [target source message-id content-sha256 seq in-reply-to]} deliver!]
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
                                 :status (:status result)
                                 :seq seq
                                 :in-reply-to in-reply-to})
        result))))

(defn- with-remote-delivery-dedupe!
  "Validate duplicate identity locally, but still replay an identical envelope
  to the disposable broker. A live broker suppresses it; a restarted in-memory
  broker can accept it again, while destination-side dedupe prevents a second
  native submission."
  [{:keys [target source message-id content-sha256 seq in-reply-to]} deliver!]
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
                                 :status (:status result)
                                 :seq seq
                                 :in-reply-to in-reply-to})
        result))))

(defn send-local-message!
  "Send only through a machine-local native adapter. The paired runner uses
  this entrypoint so a broker delivery can never bounce back to the broker."
  [request]
  (let [{:keys [target] :as envelope} (validate-envelope request)
        native (-> envelope
                   (select-keys [:message :message-id :source :seq :in-reply-to])
                   (assoc :route-id target))]
    (with-delivery-dedupe!
      envelope
      #(cond
         (str/starts-with? target "claude:")
         (cond-> (claude/send! native)
           (:seq envelope) (assoc :seq (:seq envelope)))

         (str/starts-with? target "codex:")
         (cond-> (codex/send! native)
           (:seq envelope) (assoc :seq (:seq envelope)))

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

(defn- registration-guidance
  "How a session of the source's agent becomes live presence. Agent-specific
  and free of local paths or ids."
  [source]
  (cond
    (= "claude:unregistered" source)
    (str " This Claude session's messaging socket has no cch registration. "
         "Claude sessions register through cch's SessionStart hook: run "
         "`cch control install` if it is missing, then restart the session "
         "(`claude --resume`) so the hook runs.")

    (str/starts-with? source "claude:")
    (str " Claude sessions register through cch's SessionStart hook: run "
         "`cch control install` if it is missing, then restart the session "
         "(`claude --resume`) so the hook runs.")

    (str/starts-with? source "codex:")
    (str " Codex sessions are live presence only while attached to the shared "
         "app-server: start or resume them with `codex --remote unix://`, after "
         "`cch control install` has set that server up.")

    :else nil))

(defn- inferred-source-note
  "Name where an inferred source came from, and the explicit alternative for a
  person sending by hand. cch never downgrades an agent source on its own."
  [source]
  (str "cch inferred the source " source " from this "
       (cond (str/starts-with? source "codex:") "Codex"
             :else "Claude")
       " session's environment. If you are sending by hand rather than as that "
       "session, pass `--source operator` explicitly."))

(defn unleased-source-explanation
  "Say why the broker does not know the sending route, from this machine's
  side: either the local runner is not completing exchanges, or the runner is
  fine and the source session itself is not live presence. When the source
  was inferred from the environment rather than given, say so and name the
  explicit operator alternative."
  ([source source-session health]
   (unleased-source-explanation source source-session health nil))
  ([source source-session {:keys [status since consecutive-failures error-type]}
    {:keys [inferred?]}]
   (str "Source route " source " is not leased by this runner at the broker. "
        (case status
          :failing
          (str "The local runner has failed " consecutive-failures
               " consecutive broker exchanges since " since
               (when error-type (str " (" error-type ")"))
               ", so it cannot register this machine's sessions.")
          :stalled
          (str "The local runner's polling tick has been stuck since " since
               ", so it cannot register this machine's sessions.")
          :stopped
          "The local runner is not running, so nothing registers this machine's sessions."
          :unknown
          "The local runner has no recorded status; it may never have started."
          :starting
          "The local runner has started but has not completed a broker exchange yet; retry shortly."
          (cond
            (nil? source-session)
            (str "The local runner is healthy, but this session is not in its local presence."
                 (registration-guidance source))

            (not (:available source-session))
            (str "The local runner is healthy, but this session is not available"
                 (if-let [reason (:unavailable-reason source-session)]
                   (str ": " reason ".")
                   " as live presence.")
                 (registration-guidance source))

            :else
            "The local runner is healthy and the session is live; the broker should list it within a few seconds."))
        (when (and inferred? (not= "operator" source))
          (str " " (inferred-source-note source)))
        " Run `cch control doctor` for details.")))

(defn send-message!
  "Route plain text locally when the target is native to this machine, or
  through the optional paired broker when another runner owns it. There is
  deliberately no raw-frame or approval-response API."
  [request]
  (let [inferred? (str/blank? (:source request))
        envelope (validate-envelope (dissoc request :seq))
        ;; The sending runner owns its routes' outgoing channels, so it alone
        ;; numbers them. Receiving runners deliver the number they are given.
        envelope (assoc envelope :seq (store/assign-seq! envelope))
        {:keys [target message source message-id seq in-reply-to]} envelope
        local-sessions (:sessions (list-local-sessions))
        local-ids (set (map :id local-sessions))
        config (remote/config)]
    (if (or (contains? local-ids target)
            (nil? config)
            (nil? (remote-target config target)))
      (send-local-message! (select-keys envelope
                                        [:target :message :source :message-id
                                         :seq :in-reply-to]))
      (with-remote-delivery-dedupe!
        envelope
        #(try
           (assoc (remote/enqueue! config
                                   (cond-> {:target target :message message
                                            :source source :message-id message-id
                                            :seq seq}
                                     in-reply-to (assoc :in-reply-to in-reply-to)))
                  :seq seq)
           (catch clojure.lang.ExceptionInfo error
             (if (= :invalid-source (:type (ex-data error)))
               (throw (ex-info (unleased-source-explanation
                                 source
                                 (some (fn [session] (when (= source (:id session)) session))
                                       local-sessions)
                                 (runner-health/classify
                                   (runner-health/read-status)
                                   (java.time.Instant/now))
                                 {:inferred? inferred?})
                               (ex-data error)
                               error))
               (throw error))))))))
