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
