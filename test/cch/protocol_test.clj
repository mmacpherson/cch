(ns cch.protocol-test
  (:require [clojure.test :refer [deftest is testing]]
            [cch.protocol :as proto]
            [cheshire.core :as json]))

(defn- parsed
  "Helper: render a response and parse it back to a map for assertion."
  [event-name decision]
  (some-> (proto/->response event-name decision) (json/parse-string true)))

(deftest test-extract-file-path
  (testing "extracts from tool_input"
    (is (= "/foo/bar.py"
           (proto/extract-file-path {:tool_input {:file_path "/foo/bar.py"}}))))

  (testing "extracts from tool_params as fallback"
    (is (= "/foo/bar.py"
           (proto/extract-file-path {:tool_params {:file_path "/foo/bar.py"}}))))

  (testing "prefers tool_input over tool_params"
    (is (= "/from/input.py"
           (proto/extract-file-path {:tool_input  {:file_path "/from/input.py"}
                                     :tool_params {:file_path "/from/params.py"}}))))

  (testing "returns nil when neither present"
    (is (nil? (proto/extract-file-path {:tool_input {}})))))

;; --- PreToolUse: nested hookSpecificOutput.permissionDecision ---

(deftest test-pretooluse-shape
  (testing "basic deny"
    (let [p (parsed "PreToolUse" {:decision :deny :reason "blocked"})]
      (is (= "PreToolUse" (get-in p [:hookSpecificOutput :hookEventName])))
      (is (= "deny" (get-in p [:hookSpecificOutput :permissionDecision])))
      (is (= "blocked" (get-in p [:hookSpecificOutput :permissionDecisionReason])))))

  (testing "ask with additional context"
    (let [p (parsed "PreToolUse" {:decision :ask
                                  :reason   "out of scope"
                                  :context  "file is in another repo"})]
      (is (= "ask" (get-in p [:hookSpecificOutput :permissionDecision])))
      (is (= "file is in another repo"
             (get-in p [:hookSpecificOutput :additionalContext])))))

  (testing "allow with updated-input"
    (let [p (parsed "PreToolUse" {:decision      :allow
                                  :reason        "modified"
                                  :updated-input {:command "safe-cmd"}})]
      (is (= "allow" (get-in p [:hookSpecificOutput :permissionDecision])))
      (is (= {:command "safe-cmd"}
             (get-in p [:hookSpecificOutput :updatedInput]))))))

;; --- Top-level decision events (PostToolUse, Stop, etc.) ---

(deftest test-top-level-decision-shape
  (testing "PostToolUse block emits top-level decision/reason"
    (let [p (parsed "PostToolUse" {:decision :block :reason "flagged pattern"})]
      (is (= "block" (:decision p)))
      (is (= "flagged pattern" (:reason p)))
      ;; Not nested under hookSpecificOutput:
      (is (nil? (get-in p [:hookSpecificOutput :permissionDecision])))))

  (testing ":deny is normalized to \"block\" for top-level-decision events"
    ;; cch hooks historically return :deny internally; the renderer
    ;; translates that to Claude Code's expected "block" for events
    ;; that don't support a separate deny/block distinction.
    (let [p (parsed "Stop" {:decision :deny :reason "continue working"})]
      (is (= "block" (:decision p)))))

  (testing "context optionally attached under hookSpecificOutput"
    (let [p (parsed "PostToolUse" {:decision :block
                                   :reason   "flagged"
                                   :context  "matched: rm -rf /"})]
      (is (= "block" (:decision p)))
      (is (= "PostToolUse" (get-in p [:hookSpecificOutput :hookEventName])))
      (is (= "matched: rm -rf /"
             (get-in p [:hookSpecificOutput :additionalContext])))))

  (testing "covers all top-level-decision events"
    (doseq [event ["PostToolUse" "PostToolUseFailure" "Stop" "SubagentStop"
                   "UserPromptSubmit" "ConfigChange" "TaskCreated" "TaskCompleted"]]
      (let [p (parsed event {:decision :block :reason "r"})]
        (is (= "block" (:decision p)) (str event " should emit top-level decision"))
        (is (nil? (get-in p [:hookSpecificOutput :permissionDecision]))
            (str event " must not use PreToolUse-shaped permissionDecision"))))))

;; --- PermissionRequest: nested hookSpecificOutput.decision.behavior ---

(deftest test-permission-request-shape
  (testing "allow"
    (let [p (parsed "PermissionRequest" {:decision :allow})]
      (is (= "allow" (get-in p [:hookSpecificOutput :decision :behavior])))
      (is (= "PermissionRequest" (get-in p [:hookSpecificOutput :hookEventName])))))

  (testing "deny carries message (not reason)"
    (let [p (parsed "PermissionRequest" {:decision :deny :reason "policy violation"})]
      (is (= "deny" (get-in p [:hookSpecificOutput :decision :behavior])))
      (is (= "policy violation" (get-in p [:hookSpecificOutput :decision :message])))))

  (testing "allow with updatedInput"
    (let [p (parsed "PermissionRequest" {:decision      :allow
                                         :updated-input {:command "npm run lint"}})]
      (is (= {:command "npm run lint"}
             (get-in p [:hookSpecificOutput :decision :updatedInput]))))))

;; --- Side-effect events: no output ---

(deftest test-no-output-events
  (testing "SessionStart with :decision only (no :context) still emits nothing —
            SessionStart has no block/allow semantics"
    (is (nil? (proto/->response "SessionStart" {:decision :deny :reason "nope"}))))

  (testing "Notification emits nothing"
    (is (nil? (proto/->response "Notification" {:decision :block :reason "r"}))))

  (testing "unknown event defaults to no-output"
    (is (nil? (proto/->response "SomeFutureEvent" {:decision :block :reason "r"}))))

  (testing "nil decision always produces nil response, regardless of event"
    (is (nil? (proto/->response "PreToolUse" nil)))
    (is (nil? (proto/->response "PostToolUse" nil)))
    (is (nil? (proto/->response "Unknown" nil)))))

;; --- SessionStart additionalContext ---

(deftest test-session-start-additional-context
  (testing ":context emits hookSpecificOutput.additionalContext"
    (let [p (parsed "SessionStart" {:context "project uses Babashka, not clojure"})]
      (is (= "SessionStart" (get-in p [:hookSpecificOutput :hookEventName])))
      (is (= "project uses Babashka, not clojure"
             (get-in p [:hookSpecificOutput :additionalContext])))
      ;; No permissionDecision, no top-level decision
      (is (nil? (get-in p [:hookSpecificOutput :permissionDecision])))
      (is (nil? (:decision p)))))

  (testing "empty decision-map → nil"
    (is (nil? (proto/->response "SessionStart" {}))))

  (testing ":decision alone with no :context → nil (SessionStart can't be denied)"
    (is (nil? (proto/->response "SessionStart" {:decision :deny})))))

;; --- UserPromptSubmit context-only (no decision) ---

(deftest test-user-prompt-submit-context-only
  (testing ":context without :decision emits additionalContext shape"
    (let [p (parsed "UserPromptSubmit" {:context "remember: user prefers terse replies"})]
      (is (= "UserPromptSubmit" (get-in p [:hookSpecificOutput :hookEventName])))
      (is (= "remember: user prefers terse replies"
             (get-in p [:hookSpecificOutput :additionalContext])))
      ;; No top-level decision since none was requested
      (is (nil? (:decision p)))))

  (testing ":decision + :context still routes to top-level-decision shape"
    (let [p (parsed "UserPromptSubmit" {:decision :block
                                        :reason   "prompt looks malicious"
                                        :context  "pattern matched X"})]
      (is (= "block" (:decision p)))
      (is (= "pattern matched X"
             (get-in p [:hookSpecificOutput :additionalContext]))))))

;; --- Generic escape hatch ---

(deftest test-hook-specific-output-escape-hatch
  (testing "passes user-supplied map through verbatim, auto-populates hookEventName"
    (let [p (parsed "FileChanged"
                    {:hook-specific-output {:someExperimentalField "value"
                                            :otherField            42}})]
      (is (= "FileChanged" (get-in p [:hookSpecificOutput :hookEventName])))
      (is (= "value" (get-in p [:hookSpecificOutput :someExperimentalField])))
      (is (= 42 (get-in p [:hookSpecificOutput :otherField])))))

  (testing "explicit hookEventName in escape-hatch map wins over auto-populated"
    (let [p (parsed "FileChanged"
                    {:hook-specific-output {:hookEventName "CustomName"
                                            :field         "v"}})]
      (is (= "CustomName" (get-in p [:hookSpecificOutput :hookEventName])))))

  (testing "takes precedence over :decision when both present"
    (let [p (parsed "PreToolUse"
                    {:decision             :deny
                     :reason               "would normally block"
                     :hook-specific-output {:permissionDecision "allow"
                                            :permissionDecisionReason "overridden"}})]
      ;; Escape hatch rendered — :decision/:reason ignored
      (is (= "allow" (get-in p [:hookSpecificOutput :permissionDecision])))
      (is (= "overridden"
             (get-in p [:hookSpecificOutput :permissionDecisionReason])))))

  (testing "works on otherwise-unmodeled events (forward compat)"
    (let [p (parsed "SomeFutureEvent"
                    {:hook-specific-output {:futureShape "whatever"}})]
      (is (= "SomeFutureEvent" (get-in p [:hookSpecificOutput :hookEventName])))
      (is (= "whatever" (get-in p [:hookSpecificOutput :futureShape]))))))
