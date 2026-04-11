(ns hooks.scope-lock-test
  (:require [clojure.test :refer [deftest is testing]]
            [hooks.scope-lock :as scope]
            [cch.protocol :as proto]
            [cheshire.core :as json]
            [babashka.fs :as fs]
            [babashka.process :as p]
            [clojure.string :as str]))

;; Use the actual repo root for integration tests
(def repo-root
  (str/trim (:out (p/sh ["git" "rev-parse" "--show-toplevel"]))))

;; --- Unit tests: check-scope with explicit root (no git calls) ---

(deftest test-allows-edit-within-worktree
  (is (nil? (scope/check-scope "/repo/src/main.py" "/repo" "/repo"))))

(deftest test-asks-for-edit-outside-worktree
  (let [result (scope/check-scope "/other/sneaky.py" "/repo" "/repo")]
    (is (= :ask (:decision result)))
    (is (str/includes? (:reason result) "outside worktree"))))

(deftest test-denies-edit-inside-dot-git
  (let [result (scope/check-scope "/repo/.git/config" "/repo" "/repo")]
    (is (= :deny (:decision result)))
    (is (str/includes? (:reason result) ".git/"))))

(deftest test-allows-tmp-writes
  (testing "/tmp is always allowed, even with a worktree root"
    (is (nil? (scope/check-scope "/tmp/scratch.py" "/repo" "/repo")))))

(deftest test-allows-when-no-repo
  (testing "nil root means not in a git repo — should allow everything"
    (is (nil? (scope/check-scope "/anywhere/file.py" "/tmp" nil)))))

(deftest test-allows-when-no-file-path
  (testing "nil file-path — nothing to check"
    (is (nil? (scope/check-scope nil "/repo" "/repo")))))

;; --- Config narrowing tests (use temp directories) ---

(deftest test-config-narrows-scope
  (let [tmp-dir  (str (fs/create-temp-dir {:prefix "scope-test-"}))
        config   (str tmp-dir "/.scope-lock.edn")]
    (try
      (spit config "{:allowed-paths [\"src/\" \".claude/\"]}")

      (testing "file within allowed path passes"
        (is (nil? (scope/check-scope
                    (str repo-root "/src/main.py") tmp-dir repo-root))))

      (testing "file outside allowed paths prompts"
        (let [result (scope/check-scope
                       (str repo-root "/docs/readme.md") tmp-dir repo-root)]
          (is (= :ask (:decision result)))
          (is (str/includes? (:reason result) "outside allowed scope"))
          (is (str/includes? (:reason result) "src/, .claude/"))))

      (testing "second allowed path also works"
        (is (nil? (scope/check-scope
                    (str repo-root "/.claude/settings.json") tmp-dir repo-root))))
      (finally
        (fs/delete-tree tmp-dir)))))

(deftest test-no-config-allows-all-in-worktree
  (let [tmp-dir (str (fs/create-temp-dir {:prefix "scope-noconfig-"}))]
    (try
      (testing "no .scope-lock.edn means any path in worktree is fine"
        (is (nil? (scope/check-scope
                    (str repo-root "/anything/goes.py") tmp-dir repo-root))))
      (finally
        (fs/delete-tree tmp-dir)))))

(deftest test-empty-allowed-paths-allows-all
  (let [tmp-dir (str (fs/create-temp-dir {:prefix "scope-empty-"}))
        config  (str tmp-dir "/.scope-lock.edn")]
    (try
      (spit config "{:allowed-paths []}")
      (testing "empty allowed-paths list means no narrowing"
        (is (nil? (scope/check-scope
                    (str repo-root "/anything.py") tmp-dir repo-root))))
      (finally
        (fs/delete-tree tmp-dir)))))

;; --- Protocol response tests ---

(deftest test-response-ask
  (let [decision {:decision :ask :reason "out of scope"}
        parsed   (json/parse-string (proto/->response decision) true)]
    (is (= "ask" (get-in parsed [:hookSpecificOutput :permissionDecision])))
    (is (= "out of scope" (get-in parsed [:hookSpecificOutput :permissionDecisionReason])))))

(deftest test-response-deny
  (let [decision {:decision :deny :reason "no .git edits"}
        parsed   (json/parse-string (proto/->response decision) true)]
    (is (= "deny" (get-in parsed [:hookSpecificOutput :permissionDecision])))
    (is (= "no .git edits" (get-in parsed [:hookSpecificOutput :permissionDecisionReason])))))

;; --- Integration test: run as subprocess like Claude Code would ---

(deftest test-cli-integration
  (let [run (fn [json-input]
              (p/sh {:dir repo-root
                     :in  json-input}
                    "bb" "-cp" "src" "-m" "hooks.scope-lock"))]

    (testing "allowed edit exits 0, no output"
      (let [input  (format "{\"cwd\":\"%s\",\"tool_input\":{\"file_path\":\"%s/src/foo.py\"}}"
                           repo-root repo-root)
            result (run input)]
        (is (zero? (:exit result)))
        (is (str/blank? (:out result)))))

    (testing "out-of-worktree edit exits 0 with JSON ask response"
      (let [input  (format "{\"cwd\":\"%s\",\"tool_input\":{\"file_path\":\"/etc/passwd\"}}"
                           repo-root)
            result (run input)
            parsed (json/parse-string (:out result) true)]
        (is (zero? (:exit result)))
        (is (= "ask" (get-in parsed [:hookSpecificOutput :permissionDecision])))))

    (testing ".git edit exits 0 with JSON deny response"
      (let [input  (format "{\"cwd\":\"%s\",\"tool_input\":{\"file_path\":\"%s/.git/config\"}}"
                           repo-root repo-root)
            result (run input)
            parsed (json/parse-string (:out result) true)]
        (is (zero? (:exit result)))
        (is (= "deny" (get-in parsed [:hookSpecificOutput :permissionDecision])))))))
