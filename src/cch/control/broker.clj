(ns cch.control.broker
  "Disposable in-memory rendezvous for the cross-runner control-plane POC.

  The broker stores short-lived route presence and transient text envelopes.
  Native provider credentials and private machine-local metadata never enter
  this component; presence may include a provider-validated session deep link.
  All mutation is serialized on the Broker value so enqueue, poll,
  acknowledgement, and duplicate detection are atomic."
  (:require [cch.control.broker-api :as api]
            [cch.control.naming :as naming]
            [cch.control.store :as store]
            [clojure.string :as str])
  (:import [java.net URI]
           [java.nio.charset StandardCharsets]
           [java.security MessageDigest]))

(def ^:const default-lease-ms 60000)
(def ^:const default-message-ttl-ms 30000)
(def ^:const default-ack-timeout-ms 2000)
(def ^:const default-max-attempts 3)
(def ^:const max-message-bytes (* 32 1024))

(defrecord Broker [state now-fn options])

(defn new-broker
  "Create an empty broker. runner-tokens is a map of opaque runner id to its
  persistent pairing token. The token map and message bodies remain in memory."
  ([runner-tokens] (new-broker runner-tokens {}))
  ([runner-tokens {:keys [now-fn lease-ms message-ttl-ms ack-timeout-ms
                          max-attempts]
                   :or {now-fn #(System/currentTimeMillis)
                        lease-ms default-lease-ms
                        message-ttl-ms default-message-ttl-ms
                        ack-timeout-ms default-ack-timeout-ms
                        max-attempts default-max-attempts}}]
   (->Broker (atom {:runner-tokens runner-tokens
                    :runners {}
                    :routes {}
                    :aliases {}
                    :messages {}})
             now-fn
             {:lease-ms lease-ms
              :message-ttl-ms message-ttl-ms
              :ack-timeout-ms ack-timeout-ms
              :max-attempts max-attempts})))

(defn- now [^Broker broker]
  ((:now-fn broker)))

(defn secure-equal? [a b]
  (and (string? a)
       (string? b)
       (MessageDigest/isEqual (.getBytes ^String a StandardCharsets/UTF_8)
                              (.getBytes ^String b StandardCharsets/UTF_8))))

(defn authorize!
  "Validate one runner's persistent pairing credential without returning it."
  [^Broker broker runner-id token]
  (let [expected (get-in @(:state broker) [:runner-tokens runner-id])]
    (when-not (secure-equal? expected token)
      (throw (ex-info "Runner authentication failed" {:type :unauthorized})))
    true))

(defn valid-label? [value]
  (and (string? value)
       (<= 1 (count value) 512)
       (not (re-find #"[\p{Cntrl}]" value))))

(defn route-agent [route-id]
  (cond
    (str/starts-with? route-id "claude:") "claude"
    (str/starts-with? route-id "codex:") "codex"
    :else nil))

(defn- sanitized-native-url [agent value]
  (when (and (string? value) (<= (count value) 512))
    (try
      (let [uri (URI. value)
            path (.getPath uri)]
        (when (and (= "https" (.getScheme uri))
                   (nil? (.getUserInfo uri))
                   (= -1 (.getPort uri))
                   (nil? (.getQuery uri))
                   (nil? (.getFragment uri))
                   (= agent "claude")
                   (= "claude.ai" (some-> (.getHost uri) str/lower-case))
                   (re-matches #"/code/session_[A-Za-z0-9_-]{8,128}" path))
          value))
      (catch Exception _ nil))))

(defn sanitize-session [{:keys [id status available native-url]}]
  (when-let [agent (and (valid-label? id) (route-agent id))]
    (let [native-url (sanitized-native-url agent native-url)]
      (cond->
        {:id id
         :agent agent
         :status (if (valid-label? status) status "unknown")
         :available (boolean available)}
        native-url (assoc :native-url native-url)))))

(defn clamp [value lower upper]
  (-> (or value lower) (max lower) (min upper)))

(defn terminal-status? [status]
  (contains? #{"delivered" "failed" "expired"} status))

(defn- expire-state [state timestamp max-attempts]
  (let [expired-runners (->> (:runners state)
                             (keep (fn [[runner-id runner]]
                                     (when (<= (:expires-at runner) timestamp)
                                       runner-id)))
                             set)
        routes (into {}
                     (remove (fn [[_ route]]
                               (contains? expired-runners (:runner-id route))))
                     (:routes state))
        messages
        (reduce-kv
          (fn [result message-id message]
            (let [expired? (<= (:expires-at message) timestamp)
                  exhausted? (and (= "in-flight" (:status message))
                                  (<= (:next-at message) timestamp)
                                  (>= (:attempts message) max-attempts))]
              (assoc result message-id
                     (cond
                       (and expired? (not (terminal-status? (:status message))))
                       (assoc message :status "expired" :body nil)

                       exhausted?
                       (assoc message :status "failed" :body nil
                              :failure "attempts-exhausted")

                       :else message))))
          {}
          (:messages state))]
    (-> state
        (update :runners #(apply dissoc % expired-runners))
        (assoc :routes routes :messages messages))))

(defn- expire! [^Broker broker]
  (swap! (:state broker) expire-state (now broker)
         (get-in broker [:options :max-attempts])))

(defn register!
  "Replace a runner's leased route set with a sanitized presence snapshot.
  Only available Claude/Codex routes are advertised."
  [^Broker broker {:keys [runner-id token sessions lease-ms]}]
  (authorize! broker runner-id token)
  (when-not (valid-label? runner-id)
    (throw (ex-info "runner_id is invalid" {:type :invalid-runner})))
  (let [timestamp (now broker)
        lease-ms (clamp (or lease-ms (get-in broker [:options :lease-ms]))
                        1000 (* 5 60 1000))
        sessions (->> sessions
                      (keep sanitize-session)
                      (filter :available)
                      (map (juxt :id identity))
                      (into {}))]
    (locking broker
      (let [state (expire-state @(:state broker) timestamp
                                (get-in broker [:options :max-attempts]))
            old-route-ids (get-in state [:runners runner-id :route-ids] #{})
            route-ids (set (keys sessions))
            conflicting (some (fn [route-id]
                                (let [owner (get-in state [:routes route-id :runner-id])]
                                  (when (and owner (not= owner runner-id))
                                    route-id)))
                              route-ids)]
        (when conflicting
          (throw (ex-info "Route is already leased by another runner"
                          {:type :route-conflict :route conflicting})))
        (let [without-old (update state :routes #(apply dissoc % old-route-ids))
              runner {:runner-id runner-id
                      :route-ids route-ids
                      :updated-at timestamp
                      :expires-at (+ timestamp lease-ms)}
              routes (reduce-kv
                       (fn [acc route-id session]
                         (assoc acc route-id
                                (assoc session
                                       :runner-id runner-id
                                       :expires-at (+ timestamp lease-ms))))
                       (:routes without-old)
                       sessions)
              next-state (-> without-old
                             (assoc-in [:runners runner-id] runner)
                             (assoc :routes routes))]
          (reset! (:state broker) next-state)
          {:status "registered"
           :runner-id runner-id
           :route-count (count route-ids)
           :expires-at (+ timestamp lease-ms)})))))

(defn sessions
  "Return active sanitized presence. runner-id is an opaque routing label, not
  a hostname."
  [^Broker broker]
  (expire! broker)
  (let [state @(:state broker)
        aliases (:aliases state)]
    (->> (:routes state)
         vals
         (map (fn [session]
                (let [route-id (:id session)
                      alias-record (get aliases route-id)]
                  (-> (cond-> (dissoc session :expires-at)
                        (= (:runner-id session) (:runner-id alias-record))
                        (assoc :alias (:alias alias-record)))
                      naming/present-session))))
         (sort-by :id)
         vec)))

(defn get-session [^Broker broker route-id]
  (some #(when (= route-id (:id %)) %) (sessions broker)))

(defn- set-alias-as!
  [^Broker broker {:keys [route-id alias runner-id require-owned-route?]}]
  (let [alias (naming/normalize-alias alias)]
    (locking broker
      (let [state (expire-state @(:state broker) (now broker)
                                (get-in broker [:options :max-attempts]))
            route (get-in state [:routes route-id])]
        (when-not route
          (throw (ex-info "Session is not currently available"
                          {:type :unknown-session :target route-id})))
        (when (and require-owned-route?
                   (not= runner-id (:runner-id route)))
          (throw (ex-info "Session is not leased by this runner"
                          {:type :forbidden})))
        (let [next-state (if alias
                           (assoc-in state [:aliases route-id]
                                     {:runner-id (:runner-id route)
                                      :alias alias})
                           (update state :aliases dissoc route-id))]
          (reset! (:state broker) next-state)
          (naming/present-session
            (cond-> (dissoc route :expires-at)
              alias (assoc :alias alias))))))))

(defn set-session-alias!
  "Set presentation metadata for a route leased by the authenticated runner."
  [^Broker broker {:keys [runner-id token] :as request}]
  (authorize! broker runner-id token)
  (set-alias-as! broker (assoc request
                               :runner-id runner-id
                               :require-owned-route? true)))

(defn set-operator-session-alias!
  "Set presentation metadata through the separately authenticated web app."
  [^Broker broker request]
  (set-alias-as! broker (assoc request :require-owned-route? false)))

(defn- enqueue-as!
  [^Broker broker
   {:keys [source target message message-id ttl-ms source-runner
           require-owned-source?]}]
  (doseq [[field value] [[:source source] [:target target] [:message-id message-id]]]
    (when-not (valid-label? value)
      (throw (ex-info (str (name field) " is invalid") {:type :invalid-message :field field}))))
  (when (str/blank? message)
    (throw (ex-info "message is required" {:type :invalid-message})))
  (when (str/starts-with? (str/triml message) "/")
    (throw (ex-info "command-mode input is not allowed"
                    {:type :command-mode-not-allowed})))
  (when (> (alength (.getBytes ^String message StandardCharsets/UTF_8)) max-message-bytes)
    (throw (ex-info "message is too large"
                    {:type :message-too-large :max-bytes max-message-bytes})))
  (let [timestamp (now broker)
        digest (store/content-digest message)
        ttl-ms (clamp (or ttl-ms (get-in broker [:options :message-ttl-ms]))
                      1000 (get-in broker [:options :message-ttl-ms]))]
    (locking broker
      (let [state (expire-state @(:state broker) timestamp
                                (get-in broker [:options :max-attempts]))
            source-owner (get-in state [:routes source :runner-id])
            target-owner (get-in state [:routes target :runner-id])
            prior (get-in state [:messages message-id])]
        (when (and require-owned-source?
                   (not (or (= "operator" source)
                            (= source-runner source-owner))))
          (throw (ex-info "Source route is not leased by this runner"
                          {:type :invalid-source :source source})))
        (when-not target-owner
          (throw (ex-info "Target route is not currently available"
                          {:type :unknown-session :target target})))
        (if prior
          (if (and (= source (:source prior))
                   (= target (:target prior))
                   (= digest (:content-sha256 prior)))
            {:message-id message-id :source source :target target
             :status "duplicate" :original-status (:status prior)}
            (throw (ex-info "message_id was already used with different content or routing"
                            {:type :message-id-conflict :message-id message-id})))
          (let [envelope {:message-id message-id
                          :source source
                          :source-runner source-runner
                          :target target
                          :target-runner target-owner
                          :body message
                          :content-sha256 digest
                          :status "queued"
                          :attempts 0
                          :created-at timestamp
                          :next-at timestamp
                          :expires-at (+ timestamp ttl-ms)}]
            (reset! (:state broker) (assoc-in state [:messages message-id] envelope))
            {:message-id message-id :source source :target target
             :transport "broker-memory" :status "queued"}))))))

(defn enqueue!
  "Accept one transient envelope from an authenticated runner. A repeated id
  with the same digest/routing is idempotent; conflicting reuse is rejected."
  [^Broker broker {:keys [runner-id token] :as request}]
  (authorize! broker runner-id token)
  (enqueue-as! broker (assoc request
                             :source-runner runner-id
                             :require-owned-source? true)))

(defn enqueue-operator!
  "Accept one transient envelope from the separately authenticated human web
  boundary. This capability has no runner token and cannot claim an agent
  route as its source."
  [^Broker broker request]
  (enqueue-as! broker (assoc request
                             :source "operator"
                             :source-runner "operator"
                             :require-owned-source? false)))

(defn poll!
  "Lease up to limit due envelopes for one runner. Unacked leases become due
  after ack-timeout-ms, stopping at max-attempts or message expiry."
  [^Broker broker {:keys [runner-id token limit]}]
  (authorize! broker runner-id token)
  (let [timestamp (now broker)
        limit (clamp (or limit 20) 1 100)]
    (locking broker
      (let [state (expire-state @(:state broker) timestamp
                                (get-in broker [:options :max-attempts]))
            runner (get-in state [:runners runner-id])]
        (when-not runner
          (throw (ex-info "Runner lease is absent or expired"
                          {:type :runner-not-registered})))
        (let [due (->> (:messages state)
                       vals
                       (filter #(and (= runner-id (:target-runner %))
                                     (not (terminal-status? (:status %)))
                                     (<= (:next-at %) timestamp)
                                     (< (:attempts %) (get-in broker [:options :max-attempts]))))
                       (sort-by (juxt :created-at :message-id))
                       (take limit)
                       vec)
              leased (mapv #(-> %
                                (update :attempts inc)
                                (assoc :status "in-flight"
                                       :next-at (+ timestamp
                                                   (get-in broker [:options :ack-timeout-ms]))))
                           due)
              next-state (reduce (fn [s message]
                                   (assoc-in s [:messages (:message-id message)] message))
                                 state leased)]
          (reset! (:state broker) next-state)
          {:messages (mapv #(select-keys % [:message-id :source :target :body
                                            :attempts :expires-at])
                           leased)})))))

(defn ack!
  "Acknowledge successful or permanently failed native delivery. Duplicate
  acknowledgements are harmless."
  [^Broker broker {:keys [runner-id token message-id status failure]}]
  (authorize! broker runner-id token)
  (when-not (contains? #{"delivered" "failed"} status)
    (throw (ex-info "ack status must be delivered or failed"
                    {:type :invalid-ack})))
  (locking broker
    (let [state (expire-state @(:state broker) (now broker)
                              (get-in broker [:options :max-attempts]))
          message (get-in state [:messages message-id])]
      (when-not message
        (throw (ex-info "Unknown message" {:type :unknown-message})))
      (when-not (= runner-id (:target-runner message))
        (throw (ex-info "Message belongs to another runner" {:type :forbidden})))
      (if (terminal-status? (:status message))
        {:message-id message-id :status (:status message) :duplicate true}
        (let [updated (cond-> (assoc message :status status :body nil)
                        (and (= "failed" status) failure)
                        (assoc :failure (name failure)))]
          (reset! (:state broker) (assoc-in state [:messages message-id] updated))
          {:message-id message-id :status status})))))

(defn message-status
  "Return routing/status metadata only; never return the transient body or
  content digest. Intended for POC diagnostics and tests."
  [^Broker broker runner-id token message-id]
  (authorize! broker runner-id token)
  (expire! broker)
  (when-let [message (get-in @(:state broker) [:messages message-id])]
    (when-not (or (= runner-id (:target-runner message))
                  (= runner-id (:source-runner message)))
      (throw (ex-info "Message belongs to another runner" {:type :forbidden})))
    (select-keys message [:message-id :source :target :status :attempts
                          :created-at :expires-at :failure])))

(defn operator-message-status
  "Return metadata only for a message created by the authenticated web
  operator capability."
  [^Broker broker message-id]
  (expire! broker)
  (when-let [message (get-in @(:state broker) [:messages message-id])]
    (when (and (= "operator" (:source message))
               (= "operator" (:source-runner message)))
      (select-keys message [:message-id :source :target :status :attempts
                            :created-at :expires-at :failure]))))

(defn summary
  "Non-sensitive broker diagnostics."
  [^Broker broker]
  (expire! broker)
  (let [state @(:state broker)]
    {:status "ok"
     :runner-count (count (:runners state))
     :route-count (count (:routes state))
     :message-count (count (:messages state))}))

(extend-type Broker
  api/ControlBroker
  (authorize-runner! [b runner-id token]
    (authorize! b runner-id token))
  (register-runner! [b request]
    (register! b request))
  (active-sessions [b]
    (sessions b))
  (set-session-alias! [b request]
    (set-session-alias! b request))
  (set-operator-session-alias! [b request]
    (set-operator-session-alias! b request))
  (enqueue-message! [b request]
    (enqueue! b request))
  (enqueue-operator-message! [b request]
    (enqueue-operator! b request))
  (poll-messages! [b request]
    (poll! b request))
  (ack-message! [b request]
    (ack! b request))
  (message-metadata [b runner-id token message-id]
    (message-status b runner-id token message-id))
  (operator-message-metadata [b message-id]
    (operator-message-status b message-id))
  (broker-summary [b]
    (summary b))
  (close-broker! [_] nil))
