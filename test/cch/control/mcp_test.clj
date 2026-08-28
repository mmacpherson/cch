(ns cch.control.mcp-test
  (:require [cch.control.codex-binding :as codex-binding]
            [cch.control.core :as control]
            [cch.control.mcp :as mcp]
            [clojure.test :refer [deftest is]]))

(deftest codex-mcp-calls-use-hook-bound-source
  (let [sent (atom nil)]
    (with-redefs [mcp/caller-agent (constantly "codex")
                  codex-binding/claim-source!
                  (fn [request]
                    (is (= "proof-1" (:source-proof request)))
                    "codex:thread-1")
                  control/send-message!
                  (fn [request]
                    (reset! sent request)
                    (assoc request :status "submitted"))]
      (mcp/send-message {:target "claude:session-1"
                         :message "Synthetic ping"
                         :message_id "message-1"
                         :source_proof "proof-1"})
      (is (= "codex:thread-1" (:source @sent))))))

(deftest codex-route-alias-is-normalized-before-source-claim
  (let [claimed (atom nil)
        sent (atom nil)]
    (with-redefs [mcp/caller-agent (constantly "codex")
                  codex-binding/claim-source!
                  (fn [request]
                    (reset! claimed request)
                    "codex:thread-1")
                  control/send-message!
                  (fn [request]
                    (reset! sent request)
                    (assoc request :status "submitted"))]
      (mcp/send-message {:route "claude:session-1"
                         :message "Synthetic ping"
                         :source_proof "proof-1"})
      (is (= "claude:session-1" (:target @claimed)))
      (is (= "claude:session-1" (:target @sent))))))

(deftest transport-kebab-aliases-are-normalized-before-source-claim
  (let [claimed (atom nil)
        sent (atom nil)]
    (with-redefs [mcp/caller-agent (constantly "codex")
                  codex-binding/claim-source!
                  (fn [request]
                    (reset! claimed request)
                    "codex:thread-1")
                  control/send-message!
                  (fn [request]
                    (reset! sent request)
                    (assoc request :status "submitted"))]
      ;; PluMCP canonicalizes JSON snake_case names to kebab-case keywords.
      (mcp/send-message {:target "claude:session-1"
                         :message "Synthetic ping"
                         :message-id "message-1"
                         :source-proof "proof-1"})
      (is (= "proof-1" (:source-proof @claimed)))
      (is (= "message-1" (:message-id @claimed)))
      (is (= "message-1" (:message-id @sent))))))

(deftest code-mode-runtime-metadata-is-discarded
  (let [claimed (atom nil)
        sent (atom nil)]
    (with-redefs [mcp/caller-agent (constantly "codex")
                  codex-binding/claim-source!
                  (fn [request]
                    (reset! claimed request)
                    "codex:thread-1")
                  control/send-message!
                  (fn [request]
                    (reset! sent request)
                    (assoc request :status "submitted"))]
      (doseq [runtime-key [:runtime "runtime" 'runtime :bridge/runtime]]
        (mcp/send-message {:target "claude:session-1"
                           :message "Synthetic ping"
                           :source-proof "proof-1"
                           runtime-key {:internal true}})
        (is (not (contains? @claimed runtime-key)))
        (is (not (contains? @sent runtime-key)))))))

(deftest ambiguous-destination-aliases-fail-closed
  (with-redefs [mcp/caller-agent (constantly "codex")]
    (is (= :unsupported-control-input
           (try
             (mcp/send-message {:target "claude:session-1"
                                :route "claude:session-2"
                                :message "Synthetic ping"})
             (catch clojure.lang.ExceptionInfo error
               (:type (ex-data error))))))))

(deftest duplicate-transport-aliases-fail-closed
  (with-redefs [mcp/caller-agent (constantly "codex")]
    (doseq [arguments [{:target "claude:session-1"
                        :message "Synthetic ping"
                        :message_id "one"
                        :message-id "two"}
                       {:target "claude:session-1"
                        :message "Synthetic ping"
                        :source_proof "one"
                        :source-proof "two"}]]
      (is (= :unsupported-control-input
             (try
               (mcp/send-message arguments)
               (catch clojure.lang.ExceptionInfo error
                 (:type (ex-data error)))))))))

(deftest claude-mcp-calls-reject-model-source-claims
  (with-redefs [mcp/caller-agent (constantly "claude")]
    (is (= :unsupported-control-input
           (try
             (mcp/send-message {:target "codex:thread-1"
                                :message "Synthetic ping"
                                :source "codex:forged"})
             (catch clojure.lang.ExceptionInfo error
               (:type (ex-data error))))))))

(deftest generic-message-tool-rejects-control-and-credential-fields
  (with-redefs [mcp/caller-agent (constantly "claude")]
    (doseq [field [:approval :permission :credentials :command :raw_frame]]
      (is (= :unsupported-control-input
             (try
               (mcp/send-message
                 (assoc {:target "codex:thread-1" :message "Synthetic ping"}
                        field "not-a-supported-capability"))
               (catch clojure.lang.ExceptionInfo error
                 (:type (ex-data error)))))
          (name field)))))
