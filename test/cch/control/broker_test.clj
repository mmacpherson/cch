(ns cch.control.broker-test
  (:require [cch.control.broker :as broker]
            [clojure.test :refer [deftest is]]))

(def runner-tokens
  {"runner-a" "synthetic-token-a"
   "runner-b" "synthetic-token-b"})

(def runner-a-sessions
  [{:id "codex:00000000-0000-0000-0000-00000000000a"
    :agent "codex" :native-id "private-native-value" :cwd "/private/path"
    :name "Private label" :status "idle" :available true}])

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
                       :sessions runner-a-sessions})
  (broker/register! b {:runner-id "runner-b" :token "synthetic-token-b"
                       :sessions runner-b-sessions}))

(deftest registration-publishes-only-sanitized-available-routes
  (let [b (broker/new-broker runner-tokens)]
    (register-pair! b)
    (is (= [{:id "claude:00000000-0000-0000-0000-00000000000c"
             :agent "claude" :status "working" :available true
             :native-url "https://claude.ai/code/session_synthetic123"
             :runner-id "runner-b"}
            {:id "codex:00000000-0000-0000-0000-00000000000a"
             :agent "codex" :status "idle" :available true
             :runner-id "runner-a"}
            {:id "codex:00000000-0000-0000-0000-00000000000b"
             :agent "codex" :status "idle" :available true
             :runner-id "runner-b"}]
           (mapv #(dissoc % :mnemonic :display-name) (broker/sessions b))))
    (is (every? #(re-matches #"[a-z]+-[a-z]+-[0-9a-f]{4}"
                             (:mnemonic %))
                (broker/sessions b)))
    (is (every? #(= (:mnemonic %) (:display-name %))
                (broker/sessions b)))
    (is (nil? (:cwd (first (broker/sessions b)))))
    (is (nil? (:native-url (last (broker/sessions b))))
        "generic or unrecognized provider links are discarded")
    (is (= :unauthorized
           (try
             (broker/register! b {:runner-id "runner-a" :token "wrong"
                                  :sessions []})
             (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))))))

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
