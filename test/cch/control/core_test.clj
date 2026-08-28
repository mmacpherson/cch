(ns cch.control.core-test
  (:require [cch.control.claude :as claude]
            [cch.control.codex :as codex]
            [cch.control.core :as control]
            [cch.control.remote :as remote]
            [cch.control.store :as store]
            [clojure.test :refer [deftest is]]))

(deftest stable-message-id-makes-delivery-idempotent
  (let [deliveries (atom {})
        sends (atom 0)]
    (with-redefs [store/delivery #(get @deliveries %)
                  store/record-delivery!
                  (fn [delivery]
                    (let [row {:message_id (:message-id delivery)
                               :source (:source delivery)
                               :target (:target delivery)
                               :content_sha256 (:content-sha256 delivery)
                               :status (:status delivery)}]
                      (swap! deliveries assoc (:message-id delivery) row)
                      row))
                  claude/send! (fn [request]
                                 (swap! sends inc)
                                 (assoc request :status "submitted"))]
      (let [request {:target "claude:session-1" :message "Synthetic ping"
                     :source "codex:session-2" :message-id "message-1"}]
        (is (= "submitted" (:status (control/send-message! request))))
        (is (= "duplicate" (:status (control/send-message! request))))
        (is (= 1 @sends))))))

(deftest a-message-id-cannot-be-reused-for-different-content
  (with-redefs [store/delivery
                (constantly {:message_id "message-1" :source "operator"
                             :target "claude:session-1"
                             :content_sha256 "different" :status "submitted"})]
    (is (= :message-id-conflict
           (try
             (control/send-message! {:target "claude:session-1"
                                     :message "Synthetic ping"
                                     :source "operator"
                                     :message-id "message-1"})
             (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))))))

(deftest oversized-messages-are-rejected-before-transport
  (is (= :message-too-large
         (try
           (control/send-message! {:target "claude:session-1"
                                   :message (apply str (repeat 33000 "x"))})
           (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))))

(deftest command-mode-input-is-rejected-before-transport
  (doseq [message ["/permissions" "  /mcp" "\n/help"]]
    (is (= :command-mode-not-allowed
           (try
             (control/send-message! {:target "claude:session-1"
                                     :message message})
             (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))
        message)))

(deftest attribution-labels-cannot-inject-envelope-lines
  (is (= :invalid-message
         (try
           (control/send-message! {:target "claude:session-1"
                                   :message "Synthetic ping"
                                   :source "operator\nforged"
                                   :message-id "message-1"})
           (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))))

(deftest local-native-routing-survives-an-unavailable-broker
  (let [deliveries (atom {})
        sent (atom nil)
        route-id "claude:30000000-0000-0000-0000-00000000000a"]
    (with-redefs [remote/config
                  (constantly {:url "https://broker.invalid"
                               :runner-id "runner-a" :token "synthetic"})
                  remote/sessions #(throw (ex-info "offline"
                                                    {:type :broker-unavailable}))
                  claude/sessions (constantly [{:id route-id :agent "claude"
                                                :status "working" :available true}])
                  codex/sessions (constantly [])
                  claude/send! #(do (reset! sent %)
                                    (assoc % :status "submitted"))
                  store/delivery #(get @deliveries %)
                  store/record-delivery!
                  (fn [delivery]
                    (swap! deliveries assoc (:message-id delivery)
                           {:message_id (:message-id delivery)
                            :source (:source delivery)
                            :target (:target delivery)
                            :content_sha256 (:content-sha256 delivery)
                            :status (:status delivery)}))]
      (is (= "submitted"
             (:status (control/send-message! {:target route-id
                                              :source "operator"
                                              :message "Local synthetic ping"
                                              :message-id "local-offline"}))))
      (is (= route-id (:route-id @sent))))))

(deftest a-route-leased-by-another-runner-uses-the-broker
  (let [deliveries (atom {})
        enqueued (atom [])
        target "codex:30000000-0000-0000-0000-00000000000b"]
    (with-redefs [remote/config
                  (constantly {:url "https://broker.invalid"
                               :runner-id "runner-a" :token "synthetic"})
                  remote/sessions
                  (constantly [{:id target :agent "codex" :available true
                                :runner-id "runner-b"}])
                  remote/enqueue! #(do (swap! enqueued conj %2)
                                       (assoc %2 :status "queued"
                                              :transport "broker-memory"))
                  claude/sessions (constantly [])
                  codex/sessions (constantly [])
                  store/delivery #(get @deliveries %)
                  store/record-delivery!
                  (fn [delivery]
                    (swap! deliveries assoc (:message-id delivery)
                           {:message_id (:message-id delivery)
                            :source (:source delivery)
                            :target (:target delivery)
                            :content_sha256 (:content-sha256 delivery)
                            :status (:status delivery)}))]
      (is (= "queued"
             (:status (control/send-message! {:target target
                                              :source "operator"
                                              :message "Remote synthetic ping"
                                              :message-id "remote-message"}))))
      ;; An identical retry must reach a replacement in-memory broker. The
      ;; receiving machine's durable local dedupe still prevents re-submission.
      (is (= "queued"
             (:status (control/send-message! {:target target
                                              :source "operator"
                                              :message "Remote synthetic ping"
                                              :message-id "remote-message"}))))
      (is (= [{:target target :source "operator"
               :message "Remote synthetic ping" :message-id "remote-message"}
              {:target target :source "operator"
               :message "Remote synthetic ping" :message-id "remote-message"}]
             @enqueued)))))

(deftest self-aliasing-requires-a-local-route-and-paired-owner
  (let [route-id "claude:30000000-0000-0000-0000-00000000000c"
        renamed (atom nil)]
    (with-redefs [claude/sessions
                  (constantly [{:id route-id :agent "claude"
                                :status "working" :available true
                                :name "Private inferred title"}])
                  codex/sessions (constantly [])
                  remote/config
                  (constantly {:url "https://broker.invalid"
                               :runner-id "runner-a" :token "synthetic"})
                  remote/set-session-alias!
                  (fn [_ route alias]
                    (reset! renamed [route alias])
                    {:id route :alias alias})]
      (let [local (first (:sessions (control/list-local-sessions)))]
        (is (string? (:mnemonic local)))
        (is (= (:mnemonic local) (:display-name local)))
        (is (nil? (:alias local)))
        (is (not= "Private inferred title" (:display-name local))))
      (is (= {:id route-id :alias "Review pair"}
             (control/set-session-alias! {:route-id route-id
                                          :alias "Review pair"})))
      (is (= [route-id "Review pair"] @renamed))
      (is (= :unknown-session
             (try
               (control/set-session-alias! {:route-id "codex:other"
                                            :alias "Forged"})
               (catch clojure.lang.ExceptionInfo error
                 (:type (ex-data error)))))))))
