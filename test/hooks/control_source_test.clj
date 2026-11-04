(ns hooks.control-source-test
  (:require [babashka.fs :as fs]
            [cch.control.core :as control]
            [cch.control.mcp :as mcp]
            [cch.control.store :as store]
            [clojure.test :refer [deftest is use-fixtures]]
            [hooks.control-source :as control-source]))

(use-fixtures :each
  (fn [f]
    (let [dir (str (fs/create-temp-dir {:prefix "cch-control-source-"}))]
      (try
        (with-redefs [store/db-path (constantly (str dir "/control.db"))]
          (f))
        (finally (fs/delete-tree dir))))))

(defn- payload [overrides]
  (merge {:hook_event_name "PreToolUse"
          :session_id "claude-session-1"
          :tool_name "mcp__cch__send_message"
          :tool_use_id "toolu-1"
          :tool_input {:target "codex:thread-1"
                       :message "Synthetic ping"
                       :message_id "message-1"}
          :cch/agent "claude-code"}
         overrides))

(defn- sent-source
  "Run the hook, then the MCP tool with the rewritten input as a Claude
  caller, and return the source the control plane was asked to send from."
  [hook-input]
  (let [sent (atom nil)
        {:keys [updated-input]} (control-source/handler-fn hook-input)]
    (with-redefs [mcp/caller-agent (constantly "claude")
                  control/send-message! #(do (reset! sent %) {:status "queued"})]
      (mcp/send-message updated-input))
    (:source @sent)))

(deftest a-claude-send-is-bound-to-the-calling-session
  (let [{:keys [decision updated-input]}
        (control-source/handler-fn (payload {}))]
    (is (= :allow decision))
    (is (= "toolu-1" (:source_proof updated-input)))
    (is (= "claude:claude-session-1" (sent-source (payload {:tool_use_id "toolu-2"}))))))

(deftest a-proof-is-single-use-and-payload-bound
  (let [{:keys [updated-input]} (control-source/handler-fn (payload {}))]
    (with-redefs [mcp/caller-agent (constantly "claude")
                  control/send-message! (constantly {:status "queued"})]
      (mcp/send-message updated-input)
      (is (thrown? clojure.lang.ExceptionInfo (mcp/send-message updated-input))
          "a replayed proof is rejected")
      (let [{:keys [updated-input]} (control-source/handler-fn
                                      (payload {:tool_use_id "toolu-3"}))]
        (is (thrown? clojure.lang.ExceptionInfo
                     (mcp/send-message (assoc updated-input :message "Altered")))
            "a proof does not cover a different message")))))

(deftest a-claude-caller-cannot-spend-a-codex-proof
  (store/record-codex-binding! {:agent "codex" :tool-use-id "codex-call"
                                :session-id "thread-9" :target "claude:x"
                                :message "Synthetic ping"})
  (with-redefs [mcp/caller-agent (constantly "claude")
                control/send-message! (constantly {:status "queued"})]
    (is (thrown? clojure.lang.ExceptionInfo
                 (mcp/send-message {:target "claude:x" :message "Synthetic ping"
                                    :source_proof "codex-call"})))))

(deftest only-claude-cch-tool-calls-are-bound
  (is (nil? (control-source/handler-fn (payload {:cch/agent "codex"})))
      "Codex runs its own binding hook")
  (is (nil? (control-source/handler-fn (payload {:tool_name "Bash"}))))
  (is (nil? (control-source/handler-fn (payload {:hook_event_name "PostToolUse"})))))

(deftest a-claude-alias-is-bound-to-the-calling-session
  (let [renamed (atom nil)
        {:keys [updated-input]}
        (control-source/handler-fn (payload {:tool_name "mcp__cch__set_session_alias"
                                             :tool_input {:alias "Review pair"}}))]
    (with-redefs [mcp/caller-agent (constantly "claude")
                  control/set-session-alias! #(do (reset! renamed %) {:status "renamed"})]
      (mcp/set-session-alias updated-input))
    (is (= {:route-id "claude:claude-session-1" :alias "Review pair"} @renamed))))
