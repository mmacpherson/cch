(ns cch.control.codex-binding-test
  (:require [cch.control.codex-binding :as binding]
            [cch.control.store :as store]
            [clojure.test :refer [deftest is]]))

(def synthetic-payload
  {:hook_event_name "PreToolUse"
   :session_id "thread-1"
   :turn_id "turn-1"
   :tool_name "mcp__cch__send_message"
   :tool_use_id "tool-call-1"
   :tool_input {:target "claude:session-1"
                :message_id "message-1"
                :message "Synthetic ping"
                ;; A model claim must be replaced by the runtime hook proof.
                :source_proof "untrusted-model-value"}})

(deftest hook-records-native-source-and-overwrites-proof
  (let [recorded (atom nil)]
    (with-redefs [store/record-codex-binding!
                  (fn [value] (reset! recorded value))]
      (let [response (binding/bind! synthetic-payload)]
        (is (= {:tool-use-id "tool-call-1"
                :session-id "thread-1"
                :target "claude:session-1"
                :message-id "message-1"
                :message "Synthetic ping"}
               @recorded))
        (is (= "tool-call-1"
               (get-in response
                       [:hookSpecificOutput :updatedInput :source_proof])))
        (is (= "allow"
               (get-in response
                       [:hookSpecificOutput :permissionDecision])))))))

(deftest hook-normalizes-live-codex-route-alias
  (let [recorded (atom nil)
        payload (-> synthetic-payload
                    (update :tool_input dissoc :target)
                    (assoc-in [:tool_input :route] "claude:session-1"))]
    (with-redefs [store/record-codex-binding!
                  (fn [value] (reset! recorded value))]
      (let [response (binding/bind! payload)
            updated (get-in response [:hookSpecificOutput :updatedInput])]
        (is (= "claude:session-1" (:target @recorded)))
        (is (= "claude:session-1" (:target updated)))
        (is (not (contains? updated :route)))))))

(deftest hook-strips-code-mode-runtime-metadata
  (with-redefs [store/record-codex-binding! (constantly nil)]
    (let [response (binding/bind!
                     (assoc-in synthetic-payload [:tool_input :runtime]
                               {:internal true}))
          updated (get-in response [:hookSpecificOutput :updatedInput])]
      (is (not (contains? updated :runtime)))
      (is (= "tool-call-1" (:source_proof updated))))))

(deftest alias-hook-binds-the-current-codex-session-and-value
  (let [recorded (atom nil)
        payload (-> synthetic-payload
                    (assoc :tool_name "mcp__cch__set_session_alias")
                    (assoc :tool_use_id "alias-call-1")
                    (assoc :tool_input {:alias "Review pair"
                                        :source_proof "untrusted"
                                        :runtime {:internal true}}))]
    (with-redefs [store/record-codex-alias-binding!
                  #(reset! recorded %)]
      (let [updated (get-in (binding/bind! payload)
                            [:hookSpecificOutput :updatedInput])]
        (is (= {:tool-use-id "alias-call-1"
                :session-id "thread-1"
                :alias "Review pair"}
               @recorded))
        (is (= {:alias "Review pair" :source_proof "alias-call-1"}
               updated))))))

(deftest unexpected-hook-shapes-fail-closed
  (doseq [payload [(assoc synthetic-payload :hook_event_name "PostToolUse")
                   (assoc synthetic-payload :tool_name "mcp__other__tool")
                   (assoc synthetic-payload :session_id "")
                   (assoc-in synthetic-payload [:tool_input :route]
                             "claude:ambiguous")]]
    (is (= :invalid-codex-binding-hook
           (try (binding/bind! payload) nil
                (catch clojure.lang.ExceptionInfo error
                  (:type (ex-data error))))))))

(deftest missing-or-replayed-proof-fails-clearly
  (with-redefs [store/claim-codex-binding! (constantly nil)]
    (is (= :unbound-codex-source
           (try
             (binding/claim-source! {:source-proof "missing"
                                     :target "claude:session-1"
                                     :message-id "message-1"
                                     :message "Synthetic ping"})
             (catch clojure.lang.ExceptionInfo error
               (:type (ex-data error))))))))

(deftest alias-proof-is-payload-bound-and-fails-clearly
  (with-redefs [store/claim-codex-alias-binding!
                (fn [request]
                  (when (= {:tool-use-id "alias-proof" :alias "Audit"}
                           request)
                    "codex:thread-1"))]
    (is (= "codex:thread-1"
           (binding/claim-alias-source! {:source-proof "alias-proof"
                                         :alias "Audit"})))
    (is (= :unbound-codex-source
           (try
             (binding/claim-alias-source! {:source-proof "alias-proof"
                                           :alias "Changed"})
             (catch clojure.lang.ExceptionInfo error
               (:type (ex-data error))))))))
