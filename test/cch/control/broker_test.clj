(ns cch.control.broker-test
  (:require [cch.control.broker :as broker]
            [cch.control.broker-api :as api]
            [cch.usage-observation :as usage]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]))

(def runner-tokens
  {"runner-a" "synthetic-token-a"
   "runner-b" "synthetic-token-b"})

(def runner-a-sessions
  [{:id "codex:00000000-0000-0000-0000-00000000000a"
    :agent "codex" :native-id "private-native-value" :cwd "/private/path"
    :name "Native panel" :status "idle" :available true}])

(def runner-b-sessions
  [{:id "codex:00000000-0000-0000-0000-00000000000b"
    :agent "codex" :status "idle" :available true
    :native-url "https://chatgpt.com/codex"}
   {:id "claude:00000000-0000-0000-0000-00000000000c"
    :agent "claude" :status "working" :available true
    :native-url "https://claude.ai/code/session_synthetic123"}
   {:id "claude:00000000-0000-0000-0000-00000000000d"
    :agent "claude" :status "stale" :available false}])

(defn- register-pair! [b]
  (broker/register! b {:runner-id "runner-a" :token "synthetic-token-a"
                       :local-ui-url "https://runner-a.invalid/"
                       :sessions runner-a-sessions})
  (broker/register! b {:runner-id "runner-b" :token "synthetic-token-b"
                       :sessions runner-b-sessions}))

(defn- observation
  [agent observed-at window used-percentage resets-at]
  (first
    (usage/from-snapshot
      {:agent agent
       :observed-at observed-at
       :payload {:rate_limits
                 {(keyword window) {:used_percentage used-percentage
                                    :resets_at resets-at}}}})))

(defn- activity [event-id observed-at agent action outcome]
  {:event-id (apply str (repeat 64 event-id))
   :schema-version 1 :observed-at observed-at :agent agent
   :action action :outcome outcome})

(deftest activity-is-idempotent-bounded-and-has-no-runner-attribution
  (let [clock (atom 2000000000000)
        b (broker/new-broker runner-tokens {:now-fn #(deref clock)})
        first-event (activity "a" @clock "claude-code" "turn.started" "observed")
        second-event (activity "b" (inc @clock) "codex" "session.started" "observed")]
    (is (= {:accepted 2 :duplicates 0 :latest-cursor 2}
           (broker/publish-activity!
             b {:runner-id "runner-a" :token "synthetic-token-a"
                :observations [first-event second-event]})))
    (is (= {:accepted 0 :duplicates 1 :latest-cursor 2}
           (broker/publish-activity!
             b {:runner-id "runner-b" :token "synthetic-token-b"
                :observations [first-event]})))
    (is (= [second-event]
           (api/recent-activity-observations b {:limit 10 :agent "codex"})))
    (let [rendered (pr-str (api/recent-activity-observations b {:limit 10}))]
      (is (not (str/includes? rendered "runner-a")))
      (is (not (str/includes? rendered "synthetic-token"))))
    (is (= :invalid-activity-observation
           (try
             (broker/publish-activity!
               b {:runner-id "runner-a" :token "synthetic-token-a"
                  :observations [(assoc first-event :cwd "/private")]})
             (catch clojure.lang.ExceptionInfo error
               (:type (ex-data error))))))))

(deftest registration-publishes-only-sanitized-available-routes
  (let [b (broker/new-broker runner-tokens)]
    (register-pair! b)
    (is (= [{:id "claude:00000000-0000-0000-0000-00000000000c"
             :agent "claude" :status "working" :available true
             :native-url "https://claude.ai/code/session_synthetic123"
             :runner-id "runner-b"}
            {:id "codex:00000000-0000-0000-0000-00000000000a"
             :agent "codex" :status "idle" :available true
             :name "Native panel"
             :runner-id "runner-a"}
            {:id "codex:00000000-0000-0000-0000-00000000000b"
             :agent "codex" :status "idle" :available true
             :runner-id "runner-b"}]
           (mapv #(dissoc % :mnemonic :display-name) (broker/sessions b))))
    (is (every? #(re-matches #"[a-z]+-[a-z]+-[0-9a-f]{4}"
                             (:mnemonic %))
                (broker/sessions b)))
    (is (= (str "Native panel · "
                (:mnemonic (second (broker/sessions b))))
           (:display-name (second (broker/sessions b)))))
    (is (nil? (:cwd (first (broker/sessions b)))))
    (is (nil? (:native-url (last (broker/sessions b))))
        "generic or unrecognized provider links are discarded")
    (is (= :unauthorized
           (try
             (broker/register! b {:runner-id "runner-a" :token "wrong"
                                  :sessions []})
             (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))))))

(deftest active-runners-expose-only-validated-presentation-metadata
  (let [b (broker/new-broker runner-tokens)]
    (register-pair! b)
    (is (= [{:runner-id "runner-a"
             :local-ui-url "https://runner-a.invalid"}
            {:runner-id "runner-b"}]
           (api/active-runners b)))
    (is (= :invalid-runner
           (try
             (broker/register!
               b {:runner-id "runner-a" :token "synthetic-token-a"
                  :local-ui-url "https://user:secret@runner.invalid"
                  :sessions []})
             (catch clojure.lang.ExceptionInfo error
               (:type (ex-data error))))))))

(deftest malformed-native-names-are-not-federated
  (let [b (broker/new-broker runner-tokens)
        route "codex:00000000-0000-0000-0000-00000000000e"]
    (broker/register!
      b {:runner-id "runner-a" :token "synthetic-token-a"
         :sessions [{:id route :agent "codex" :status "idle"
                     :available true :name "forged\nname"}]})
    (let [session (first (broker/sessions b))]
      (is (nil? (:name session)))
      (is (= (:mnemonic session) (:display-name session))))))

(deftest aliases-are-presentation-only-and-owner-bound
  (let [b (broker/new-broker runner-tokens)
        route-id "codex:00000000-0000-0000-0000-00000000000a"]
    (register-pair! b)
    (let [renamed (broker/set-session-alias!
                    b {:runner-id "runner-a" :token "synthetic-token-a"
                       :route-id route-id :alias "  Build pair  "})]
      (is (= route-id (:id renamed)))
      (is (= "Build pair" (:alias renamed)))
      (is (= (str "Build pair · " (:mnemonic renamed))
             (:display-name renamed))))
    (is (= :forbidden
           (try
             (broker/set-session-alias!
               b {:runner-id "runner-b" :token "synthetic-token-b"
                  :route-id route-id :alias "Forged"})
             (catch clojure.lang.ExceptionInfo error
               (:type (ex-data error))))))
    (is (= "Operator name"
           (:alias
             (broker/set-operator-session-alias!
               b {:route-id route-id :alias "Operator name"}))))
    (is (= route-id (:id (first (filter #(= route-id (:id %))
                                        (broker/sessions b))))))
    (is (nil? (:alias
                (broker/set-session-alias!
                  b {:runner-id "runner-a" :token "synthetic-token-a"
                     :route-id route-id :alias ""}))))))

(deftest expired-route-aliases-do-not-cross-runner-ownership
  (let [clock (atom 1000)
        b (broker/new-broker runner-tokens
                             {:now-fn #(deref clock) :lease-ms 1000})
        route-id "codex:00000000-0000-0000-0000-00000000000a"]
    (broker/register! b {:runner-id "runner-a" :token "synthetic-token-a"
                         :sessions runner-a-sessions})
    (broker/set-session-alias!
      b {:runner-id "runner-a" :token "synthetic-token-a"
         :route-id route-id :alias "Old owner"})
    (swap! clock + 1001)
    (broker/register!
      b {:runner-id "runner-b" :token "synthetic-token-b"
         :sessions [{:id route-id :agent "codex" :status "idle"
                     :available true}]})
    (is (nil? (:alias (first (broker/sessions b)))))))

(deftest messages-cross-both-directions-and-deduplicate
  (let [b (broker/new-broker runner-tokens)
        a "codex:00000000-0000-0000-0000-00000000000a"
        c "claude:00000000-0000-0000-0000-00000000000c"]
    (register-pair! b)
    (is (= "queued"
           (:status (broker/enqueue! b {:runner-id "runner-a"
                                        :token "synthetic-token-a"
                                        :source a :target c
                                        :message "Synthetic request"
                                        :message-id "message-forward"}))))
    (let [message (-> (broker/poll! b {:runner-id "runner-b"
                                       :token "synthetic-token-b"})
                      :messages first)]
      (is (= {:message-id "message-forward"
              :source a :target c :body "Synthetic request"
              :attempts 1}
             (dissoc message :expires-at))))
    (is (= "delivered"
           (:status (broker/ack! b {:runner-id "runner-b"
                                    :token "synthetic-token-b"
                                    :message-id "message-forward"
                                    :status "delivered"}))))
    (is (= "duplicate"
           (:status (broker/enqueue! b {:runner-id "runner-a"
                                        :token "synthetic-token-a"
                                        :source a :target c
                                        :message "Synthetic request"
                                        :message-id "message-forward"}))))
    (is (= "queued"
           (:status (broker/enqueue! b {:runner-id "runner-b"
                                        :token "synthetic-token-b"
                                        :source c :target a
                                        :message "Synthetic reply"
                                        :message-id "message-reply"}))))
    (is (= "Synthetic reply"
           (-> (broker/poll! b {:runner-id "runner-a"
                                :token "synthetic-token-a"})
               :messages first :body)))
    (is (= :message-id-conflict
           (try
             (broker/enqueue! b {:runner-id "runner-a"
                                 :token "synthetic-token-a"
                                 :source a :target c
                                 :message "Changed body"
                                 :message-id "message-forward"})
             (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))))))

(deftest unacked-delivery-has-bounded-retries
  (let [clock (atom 10000)
        b (broker/new-broker runner-tokens
                             {:now-fn #(deref clock)
                              :ack-timeout-ms 100
                              :message-ttl-ms 5000
                              :max-attempts 3})
        source "codex:00000000-0000-0000-0000-00000000000a"
        target "codex:00000000-0000-0000-0000-00000000000b"]
    (register-pair! b)
    (broker/enqueue! b {:runner-id "runner-a" :token "synthetic-token-a"
                        :source source :target target :message "Retry me"
                        :message-id "message-retry"})
    (doseq [attempt [1 2 3]]
      (is (= attempt
             (-> (broker/poll! b {:runner-id "runner-b"
                                  :token "synthetic-token-b"})
                 :messages first :attempts)))
      (swap! clock + 100))
    (is (empty? (:messages (broker/poll! b {:runner-id "runner-b"
                                            :token "synthetic-token-b"}))))
    (is (= {:message-id "message-retry" :source source :target target
            :status "failed" :attempts 3 :created-at 10000 :expires-at 15000
            :failure "attempts-exhausted"}
           (broker/message-status b "runner-a" "synthetic-token-a"
                                  "message-retry")))))

(deftest leases-and-messages-expire-and-runners-can-reconnect
  (let [clock (atom 20000)
        b (broker/new-broker runner-tokens
                             {:now-fn #(deref clock)
                              :lease-ms 1000 :message-ttl-ms 1000})
        source "codex:00000000-0000-0000-0000-00000000000a"
        target "claude:00000000-0000-0000-0000-00000000000c"]
    (register-pair! b)
    (broker/enqueue! b {:runner-id "runner-a" :token "synthetic-token-a"
                        :source source :target target :message "Short lived"
                        :message-id "message-expiry"})
    (swap! clock + 1001)
    (is (empty? (broker/sessions b)))
    (is (= "expired" (:status (broker/message-status
                                b "runner-a" "synthetic-token-a"
                                "message-expiry"))))
    (is (= "registered"
           (:status (broker/register! b {:runner-id "runner-b"
                                         :token "synthetic-token-b"
                                         :sessions runner-b-sessions}))))
    (is (= 2 (count (broker/sessions b))))))

(deftest default-lease-outlives-slow-native-discovery
  (let [clock (atom 30000)
        b (broker/new-broker runner-tokens {:now-fn #(deref clock)})]
    (register-pair! b)
    ;; Real Claude + Codex discovery took 13-15 seconds per runner in the
    ;; physical-machine POC. Sequential refreshes must comfortably overlap.
    (swap! clock + 30000)
    (is (= 3 (count (broker/sessions b))))
    (swap! clock + 30001)
    (is (empty? (broker/sessions b)))))

(deftest source-routes-cannot-be-spoofed-by-another-runner
  (let [b (broker/new-broker runner-tokens)]
    (register-pair! b)
    (is (= :invalid-source
           (try
             (broker/enqueue! b {:runner-id "runner-b"
                                 :token "synthetic-token-b"
                                 :source "codex:00000000-0000-0000-0000-00000000000a"
                                 :target "claude:00000000-0000-0000-0000-00000000000c"
                                 :message "Forged" :message-id "message-forged"})
             (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))))))

(deftest web-operator-routes-without-borrowing-a-runner-credential
  (let [b (broker/new-broker runner-tokens)
        source "codex:00000000-0000-0000-0000-00000000000a"
        target "claude:00000000-0000-0000-0000-00000000000c"]
    (register-pair! b)
    (is (= "queued"
           (:status (broker/enqueue-operator!
                      b {:target target
                         :message "Synthetic operator request"
                         :message-id "operator-message"}))))
    (is (= {:message-id "operator-message"
            :source "operator"
            :target target
            :status "queued"
            :attempts 0}
           (select-keys (broker/operator-message-status b "operator-message")
                        [:message-id :source :target :status :attempts])))
    (is (= "Synthetic operator request"
           (-> (broker/poll! b {:runner-id "runner-b"
                                :token "synthetic-token-b"})
               :messages first :body)))
    (broker/enqueue! b {:runner-id "runner-a"
                        :token "synthetic-token-a"
                        :source source :target target
                        :message "Synthetic agent request"
                        :message-id "agent-message"})
    (is (nil? (broker/operator-message-status b "agent-message")))
    (is (= :unknown-session
           (try
             (broker/enqueue-operator!
               b {:target "claude:00000000-0000-0000-0000-0000000000ff"
                  :message "Synthetic unavailable request"
                  :message-id "operator-unknown"})
             (catch clojure.lang.ExceptionInfo error
               (:type (ex-data error))))))))

(deftest usage-observations-are-idempotent-and-cursor-readable
  (let [clock (atom 2000000000000)
        b (broker/new-broker runner-tokens {:now-fn #(deref clock)})
        first-observation (observation "codex" @clock "five_hour" 12.5
                                       (quot (+ @clock 3600000) 1000))
        second-observation (observation "agy" (inc @clock) "seven_day" 34
                                        (quot (+ @clock 86400000) 1000))]
    (is (= {:accepted 2 :duplicates 0 :latest-cursor 2}
           (broker/publish-usage!
             b {:runner-id "runner-a" :token "synthetic-token-a"
                :observations [first-observation second-observation]})))
    (is (= {:accepted 0 :duplicates 1 :latest-cursor 2}
           (broker/publish-usage!
             b {:runner-id "runner-b" :token "synthetic-token-b"
                :observations [first-observation]})))
    (let [page-one (broker/read-usage!
                     b {:runner-id "runner-b" :token "synthetic-token-b"
                        :after-cursor 0 :limit 1})
          page-two (broker/read-usage!
                     b {:runner-id "runner-a" :token "synthetic-token-a"
                        :after-cursor (:next-cursor page-one) :limit 10})]
      (is (= 1 (:next-cursor page-one)))
      (is (= [first-observation]
             (mapv #(dissoc % :cursor) (:observations page-one))))
      (is (= 2 (:next-cursor page-two)))
      (is (= [second-observation]
             (mapv #(dissoc % :cursor) (:observations page-two))))
      (is (every? #(nil? (:runner-id %))
                  (concat (:observations page-one) (:observations page-two)))))))

(deftest hosted-usage-read-model-is-internal-bounded-and-sanitized
  (let [clock (atom 2000000000000)
        b (broker/new-broker runner-tokens {:now-fn #(deref clock)})
        reset (quot (+ @clock 3600000) 1000)
        observations [(observation "codex" (- @clock 60000)
                                           "five_hour" 12 reset)
                      (observation "codex" @clock
                                           "five_hour" 14 reset)]]
    (broker/publish-usage!
      b {:runner-id "runner-a" :token "synthetic-token-a"
         :observations observations})
    (let [model (api/usage-forecast-inputs b)
          input (get-in model [:agents "codex" "five_hour"])]
      (is (= reset (:resets-at input)))
      (is (= 2 (:sample-count input)))
      (is (= [12.0 14.0] (mapv :used-percentage (:samples input))))
      (is (not (str/includes? (pr-str model) "runner-a")))
      (is (not (str/includes? (pr-str model) "synthetic-token-a"))))))

(deftest usage-observation-boundary-rejects-forgery-and-bounds-retention
  (let [clock (atom 2000000000000)
        b (broker/new-broker
            runner-tokens
            {:now-fn #(deref clock)
             :usage-retention-ms 1000
             :usage-future-skew-ms 100
             :max-usage-observations 2})
        valid #(observation "codex" % "five_hour" 10
                            (quot (+ @clock 3600000) 1000))
        publish #(broker/publish-usage!
                   b {:runner-id "runner-a" :token "synthetic-token-a"
                      :observations %})
        error-type (fn [f]
                     (try (f) nil
                          (catch clojure.lang.ExceptionInfo error
                            (:type (ex-data error)))))]
    (is (= :unauthorized
           (error-type #(broker/publish-usage!
                          b {:runner-id "runner-a" :token "wrong"
                             :observations []}))))
    (is (= :invalid-usage-observation
           (error-type #(publish [(assoc (valid @clock)
                                         :session-id "must-not-cross")]))))
    (is (= :invalid-usage-observation
           (error-type #(publish [(update (valid @clock)
                                          :used-percentage inc)]))))
    (is (= :usage-batch-too-large
           (error-type #(publish (vec (repeat 257 (valid @clock)))))))
    (is (= :usage-observation-expired
           (error-type #(publish [(valid (- @clock 1001))]))))
    (is (= :usage-observation-future
           (error-type #(publish [(valid (+ @clock 101))]))))
    (publish [(valid (- @clock 2)) (valid (- @clock 1)) (valid @clock)])
    (let [retained (:observations
                     (broker/read-usage!
                       b {:runner-id "runner-a" :token "synthetic-token-a"
                          :after-cursor 0 :limit 10}))]
      (is (= 2 (count retained)))
      (is (= [(- @clock 1) @clock] (mapv :observed-at retained))))
    (swap! clock + 1001)
    (is (empty? (:observations
                  (broker/read-usage!
                    b {:runner-id "runner-a" :token "synthetic-token-a"
                       :after-cursor 0 :limit 10}))))))
