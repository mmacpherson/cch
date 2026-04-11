(ns cch.protocol-test
  (:require [clojure.test :refer [deftest is testing]]
            [cch.protocol :as proto]
            [cheshire.core :as json]))

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

(deftest test-response-format
  (testing "basic deny response"
    (let [parsed (json/parse-string
                   (proto/->response "PreToolUse" {:decision :deny :reason "blocked"})
                   true)]
      (is (= "PreToolUse" (get-in parsed [:hookSpecificOutput :hookEventName])))
      (is (= "deny" (get-in parsed [:hookSpecificOutput :permissionDecision])))
      (is (= "blocked" (get-in parsed [:hookSpecificOutput :permissionDecisionReason])))))

  (testing "response with additional context"
    (let [parsed (json/parse-string
                   (proto/->response "PreToolUse" {:decision :ask
                                                    :reason   "out of scope"
                                                    :context  "file is in another repo"})
                   true)]
      (is (= "file is in another repo"
             (get-in parsed [:hookSpecificOutput :additionalContext])))))

  (testing "response with updated input"
    (let [parsed (json/parse-string
                   (proto/->response "PreToolUse" {:decision      :allow
                                                    :reason        "modified"
                                                    :updated-input {:command "safe-cmd"}})
                   true)]
      (is (= {:command "safe-cmd"}
             (get-in parsed [:hookSpecificOutput :updatedInput])))))

  (testing "event name is passed through"
    (let [parsed (json/parse-string
                   (proto/->response "PostToolUse" {:decision :allow :reason "ok"})
                   true)]
      (is (= "PostToolUse" (get-in parsed [:hookSpecificOutput :hookEventName]))))))
