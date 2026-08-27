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
                         :source_proof "proof-1"
                         :source "claude:forged"})
      (is (= "codex:thread-1" (:source @sent))))))

(deftest claude-mcp-calls-ignore-model-source-claims
  (let [sent (atom nil)]
    (with-redefs [mcp/caller-agent (constantly "claude")
                  control/send-message!
                  (fn [request]
                    (reset! sent request)
                    (assoc request :status "submitted"))]
      (mcp/send-message {:target "codex:thread-1"
                         :message "Synthetic ping"
                         :source "codex:forged"})
      (is (nil? (:source @sent))))))
