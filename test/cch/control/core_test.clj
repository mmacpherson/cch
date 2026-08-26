(ns cch.control.core-test
  (:require [cch.control.claude :as claude]
            [cch.control.core :as control]
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

(deftest attribution-labels-cannot-inject-envelope-lines
  (is (= :invalid-message
         (try
           (control/send-message! {:target "claude:session-1"
                                   :message "Synthetic ping"
                                   :source "operator\nforged"
                                   :message-id "message-1"})
           (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))))
