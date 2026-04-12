(ns hooks.scope-lock-test
  (:require [clojure.test :refer [deftest is testing]]
            [hooks.scope-lock :as scope]
            [cch.protocol :as proto]
            [cch.config :as config]
            [cheshire.core :as json]
            [babashka.fs :as fs]
            [babashka.process :as p]
            [clojure.string :as str]))

;; Use the actual repo root for integration tests
(def repo-root
  (str/trim (:out (p/sh ["git" "rev-parse" "--show-toplevel"]))))

;; --- Unit tests: check-scope with explicit args (pure, no I/O) ---

(deftest test-allows-edit-within-worktree
  (is (nil? (scope/check-scope "/repo/src/main.py" "/repo" nil))))

(deftest test-asks-for-edit-outside-worktree
  (let [result (scope/check-scope "/other/sneaky.py" "/repo" nil)]
    (is (= :ask (:decision result)))
    (is (str/includes? (:reason result) "outside worktree"))))

(deftest test-denies-edit-inside-dot-git
  (let [result (scope/check-scope "/repo/.git/config" "/repo" nil)]
    (is (= :deny (:decision result)))
    (is (str/includes? (:reason result) ".git/"))))

(deftest test-allows-tmp-writes
  (testing "/tmp is always allowed, even with a worktree root"
    (is (nil? (scope/check-scope "/tmp/scratch.py" "/repo" nil)))))

(deftest test-tmp-git-bypass-denied
  (testing "/tmp/.git/ is denied: .git check must run before /tmp allow"
    (let [result (scope/check-scope "/tmp/repo/.git/config" "/tmp/repo" nil)]
      (is (= :deny (:decision result)))
      (is (str/includes? (:reason result) ".git/")))))

(deftest test-allows-when-no-repo
  (testing "nil root means not in a git repo — should allow everything"
    (is (nil? (scope/check-scope "/anywhere/file.py" nil nil)))))

(deftest test-allows-when-no-file-path
  (testing "nil file-path — nothing to check"
    (is (nil? (scope/check-scope nil "/repo" nil)))))

;; --- Allowed-paths narrowing tests ---

(deftest test-allowed-paths-permits-matching
  (testing "file within allowed path passes"
    (is (nil? (scope/check-scope
                (str repo-root "/src/main.py") repo-root ["src/"])))))

(deftest test-allowed-paths-blocks-outside
  (testing "file outside allowed paths prompts"
    (let [result (scope/check-scope
                   (str repo-root "/docs/readme.md") repo-root ["src/" ".claude/"])]
      (is (= :ask (:decision result)))
      (is (str/includes? (:reason result) "outside allowed scope")))))

(deftest test-allowed-paths-second-path-works
  (testing "second allowed path also works"
    (is (nil? (scope/check-scope
                (str repo-root "/.claude/settings.json") repo-root ["src/" ".claude/"])))))

(deftest test-nil-allowed-paths-allows-all
  (testing "nil allowed-paths means no narrowing"
    (is (nil? (scope/check-scope
                (str repo-root "/anything/goes.py") repo-root nil)))))

(deftest test-empty-allowed-paths-allows-all
  (testing "empty allowed-paths list means no narrowing"
    (is (nil? (scope/check-scope
                (str repo-root "/anything.py") repo-root [])))))

(deftest test-segment-matching-prevents-prefix-collision
  (testing "src/ should not match src-old/"
    (let [result (scope/check-scope
                   (str repo-root "/src-old/legacy.py") repo-root ["src/"])]
      (is (= :ask (:decision result))))))

;; --- Security edge cases ---

(deftest test-dot-dot-traversal-blocked
  (testing "path traversal via ../ resolves and is caught"
    (let [result (scope/check-scope "/repo/src/../../etc/passwd" "/repo" nil)]
      (is (= :ask (:decision result)))
      (is (str/includes? (:reason result) "outside worktree")))))

(deftest test-dot-git-without-trailing-slash
  (testing ".git directory itself (no trailing slash) is denied"
    (let [result (scope/check-scope "/repo/.git" "/repo" nil)]
      (is (= :deny (:decision result))))))

(deftest test-malformed-yaml-config
  (let [tmp-dir  (str (fs/create-temp-dir {:prefix "scope-bad-yaml-"}))
        config-f (str tmp-dir "/.cch-config.yaml")]
    (try
      (spit config-f "hooks:\n  scope-lock:\n    allowed-paths: [unclosed\n")
      (testing "malformed YAML throws ex-info with ::malformed-config"
        (let [ex (try
                   (config/load-yaml config-f)
                   nil
                   (catch clojure.lang.ExceptionInfo e e))]
          (is (some? ex))
          (is (= :cch.config/malformed-config (:type (ex-data ex))))))
      (finally
        (fs/delete-tree tmp-dir)))))

;; --- Config file loading tests (uses temp directories) ---

(deftest test-config-file-loading
  (let [tmp-dir (str (fs/create-temp-dir {:prefix "scope-test-"}))
        config  (str tmp-dir "/.cch-config.yaml")]
    (try
      (spit config "hooks:\n  scope-lock:\n    allowed-paths:\n      - src/\n      - .claude/\n")
      (testing "config is loaded; hook section reachable via nested path"
        (let [cfg (config/load-yaml config)]
          (is (= ["src/" ".claude/"] (get-in cfg [:hooks :scope-lock :allowed-paths])))))
      (finally
        (fs/delete-tree tmp-dir)))))

;; --- Protocol response tests ---

(deftest test-response-ask
  (let [decision {:decision :ask :reason "out of scope"}
        parsed   (json/parse-string (proto/->response "PreToolUse" decision) true)]
    (is (= "ask" (get-in parsed [:hookSpecificOutput :permissionDecision])))
    (is (= "PreToolUse" (get-in parsed [:hookSpecificOutput :hookEventName])))
    (is (= "out of scope" (get-in parsed [:hookSpecificOutput :permissionDecisionReason])))))

(deftest test-response-deny
  (let [decision {:decision :deny :reason "no .git edits"}
        parsed   (json/parse-string (proto/->response "PreToolUse" decision) true)]
    (is (= "deny" (get-in parsed [:hookSpecificOutput :permissionDecision])))
    (is (= "no .git edits" (get-in parsed [:hookSpecificOutput :permissionDecisionReason])))))

;; Event-name / non-PreToolUse shape coverage lives in cch.protocol-test.

;; --- Integration test: run as subprocess like Claude Code would ---

(deftest test-cli-integration
  (let [run (fn [json-input]
              (p/sh {:dir repo-root
                     :in  json-input}
                    "bb" "-cp" "src:resources" "-m" "hooks.scope-lock"))]

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
        (is (= "deny" (get-in parsed [:hookSpecificOutput :permissionDecision])))))

    (testing "malformed .cch-config.yaml fails closed with deny"
      (let [tmp-repo (str (fs/create-temp-dir {:prefix "scope-malformed-"}))]
        (try
          ;; Init a repo inside /tmp so worktree-root resolves, then drop a
          ;; malformed yaml. /tmp dir is symlinked on some systems; normalize.
          (p/sh {:dir tmp-repo} "git" "init" "-q")
          (let [real-root (str/trim (:out (p/sh {:dir tmp-repo}
                                               "git" "rev-parse" "--show-toplevel")))]
            (spit (str real-root "/.cch-config.yaml")
                  "hooks:\n  scope-lock:\n    allowed-paths: [unclosed\n")
            (fs/create-dirs (str real-root "/src"))
            (let [input  (format "{\"cwd\":\"%s\",\"tool_input\":{\"file_path\":\"%s/src/a.clj\"}}"
                                 real-root real-root)
                  result (p/sh {:dir real-root :in input}
                               "bb" "-cp" (str repo-root "/src:" repo-root "/resources")
                               "-m" "hooks.scope-lock")
                  parsed (json/parse-string (:out result) true)]
              (is (zero? (:exit result)))
              (is (= "deny" (get-in parsed [:hookSpecificOutput :permissionDecision])))
              (is (str/includes? (get-in parsed [:hookSpecificOutput :permissionDecisionReason])
                                 "malformed config"))))
          (finally
            (fs/delete-tree tmp-repo)))))))
