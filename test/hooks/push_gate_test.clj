(ns hooks.push-gate-test
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hooks.push-gate :as pg]))

(def repo-root
  (str/trim (:out (p/sh ["git" "rev-parse" "--show-toplevel"]))))

;; --- is-push? detection (pure) --------------------------------------------

(deftest test-is-push-recognizes-git-push
  (testing "bare and with args"
    (is (pg/is-push? "git push"))
    (is (pg/is-push? "git push origin main"))
    (is (pg/is-push? "git push --force-with-lease"))
    (is (pg/is-push? "  git push  "))
    (is (pg/is-push? "GIT_TRACE=1 git push origin main")))
  (testing "non-push git commands pass through"
    (is (not (pg/is-push? "git pull")))
    (is (not (pg/is-push? "git status")))
    (is (not (pg/is-push? "git push-foo"))
        "must not false-match subcommands that share the `push` prefix"))
  (testing "chained / piped pushes intentionally not detected"
    (is (not (pg/is-push? "foo && git push"))
        "detection is leading-command only; chained pushes are out of scope")
    (is (not (pg/is-push? "echo hi | git push"))))
  (testing "nil / empty"
    (is (not (pg/is-push? nil)))
    (is (not (pg/is-push? "")))))

;; --- check-push gate runner (I/O, small temp dir) ------------------------

(deftest test-check-push-no-gates-allows
  (let [tmp (str (fs/create-temp-dir {:prefix "push-gate-"}))]
    (try
      (is (nil? (pg/check-push tmp nil)))
      (is (nil? (pg/check-push tmp [])))
      (finally (fs/delete-tree tmp)))))

(deftest test-check-push-all-pass
  (let [tmp (str (fs/create-temp-dir {:prefix "push-gate-"}))]
    (try
      (is (nil? (pg/check-push tmp ["true" "echo hi"])))
      (finally (fs/delete-tree tmp)))))

(deftest test-check-push-first-failure-short-circuits
  (let [tmp     (str (fs/create-temp-dir {:prefix "push-gate-"}))
        marker  (str tmp "/should-not-run")]
    (try
      (let [result (pg/check-push tmp ["false"
                                       (str "touch " marker)])]
        (is (= :deny (:decision result)))
        (is (str/includes? (:reason result) "`false` failed"))
        (is (not (fs/exists? marker))
            "later gates must not run after a failure"))
      (finally (fs/delete-tree tmp)))))

(deftest test-check-push-captures-output-tail
  (let [tmp (str (fs/create-temp-dir {:prefix "push-gate-"}))]
    (try
      (let [result (pg/check-push tmp ["sh -c 'echo boom-marker >&2; exit 1'"])]
        (is (= :deny (:decision result)))
        (is (str/includes? (:reason result) "boom-marker")
            "stderr must be surfaced so Claude can see why the gate failed"))
      (finally (fs/delete-tree tmp)))))

;; --- CLI integration: verify PreToolUse response shape -------------------

(defn- run-hook [real-root json-input]
  (p/sh {:dir real-root :in json-input}
        "bb" "-cp" (str repo-root "/src:" repo-root "/resources")
        "-m" "hooks.push-gate"))

(deftest test-cli-integration-non-push-passes-through
  (let [tmp-repo (str (fs/create-temp-dir {:prefix "push-gate-cli-"}))]
    (try
      (p/sh {:dir tmp-repo} "git" "init" "-q")
      (let [real-root (str/trim (:out (p/sh {:dir tmp-repo}
                                            "git" "rev-parse" "--show-toplevel")))
            input     (json/generate-string
                        {:hook_event_name "PreToolUse"
                         :cwd             real-root
                         :tool_name       "Bash"
                         :tool_input      {:command "ls -la"}})
            result    (run-hook real-root input)]
        (is (zero? (:exit result)))
        (is (str/blank? (:out result))
            "non-push commands produce no output"))
      (finally (fs/delete-tree tmp-repo)))))

(deftest test-cli-integration-push-with-no-config-passes-through
  (let [tmp-repo (str (fs/create-temp-dir {:prefix "push-gate-cli-"}))]
    (try
      (p/sh {:dir tmp-repo} "git" "init" "-q")
      (let [real-root (str/trim (:out (p/sh {:dir tmp-repo}
                                            "git" "rev-parse" "--show-toplevel")))
            input     (json/generate-string
                        {:hook_event_name "PreToolUse"
                         :cwd             real-root
                         :tool_name       "Bash"
                         :tool_input      {:command "git push origin main"}})
            result    (run-hook real-root input)]
        (is (zero? (:exit result)))
        (is (str/blank? (:out result))
            "no config → no gates → no denial"))
      (finally (fs/delete-tree tmp-repo)))))

(deftest test-cli-integration-push-with-passing-gates-allows
  (let [tmp-repo (str (fs/create-temp-dir {:prefix "push-gate-cli-"}))]
    (try
      (p/sh {:dir tmp-repo} "git" "init" "-q")
      (let [real-root (str/trim (:out (p/sh {:dir tmp-repo}
                                            "git" "rev-parse" "--show-toplevel")))]
        (spit (str real-root "/.cch-config.yaml")
              "hooks:\n  push-gate:\n    gates:\n      - \"true\"\n      - \"echo ok\"\n")
        (let [input  (json/generate-string
                       {:hook_event_name "PreToolUse"
                        :cwd             real-root
                        :tool_name       "Bash"
                        :tool_input      {:command "git push"}})
              result (run-hook real-root input)]
          (is (zero? (:exit result)))
          (is (str/blank? (:out result)))))
      (finally (fs/delete-tree tmp-repo)))))

(deftest test-cli-integration-failing-gate-denies-with-pretooluse-shape
  (let [tmp-repo (str (fs/create-temp-dir {:prefix "push-gate-cli-"}))]
    (try
      (p/sh {:dir tmp-repo} "git" "init" "-q")
      (let [real-root (str/trim (:out (p/sh {:dir tmp-repo}
                                            "git" "rev-parse" "--show-toplevel")))]
        (spit (str real-root "/.cch-config.yaml")
              "hooks:\n  push-gate:\n    gates:\n      - \"sh -c 'echo gate-stderr >&2; exit 1'\"\n")
        (let [input  (json/generate-string
                       {:hook_event_name "PreToolUse"
                        :cwd             real-root
                        :tool_name       "Bash"
                        :tool_input      {:command "git push origin main"}})
              result (run-hook real-root input)
              parsed (json/parse-string (:out result) true)]
          (is (zero? (:exit result)))
          (is (= "deny"
                 (get-in parsed [:hookSpecificOutput :permissionDecision]))
              "PreToolUse must use hookSpecificOutput.permissionDecision shape")
          (is (str/includes?
                (get-in parsed [:hookSpecificOutput :permissionDecisionReason])
                "gate-stderr"))))
      (finally (fs/delete-tree tmp-repo)))))

(deftest test-cli-integration-malformed-config-denies-fail-closed
  (let [tmp-repo (str (fs/create-temp-dir {:prefix "push-gate-cli-"}))]
    (try
      (p/sh {:dir tmp-repo} "git" "init" "-q")
      (let [real-root (str/trim (:out (p/sh {:dir tmp-repo}
                                            "git" "rev-parse" "--show-toplevel")))]
        (spit (str real-root "/.cch-config.yaml")
              "hooks:\n  push-gate:\n    gates: [unclosed\n")
        (let [input  (json/generate-string
                       {:hook_event_name "PreToolUse"
                        :cwd             real-root
                        :tool_name       "Bash"
                        :tool_input      {:command "git push"}})
              result (run-hook real-root input)
              parsed (json/parse-string (:out result) true)]
          (is (zero? (:exit result)))
          (is (= "deny"
                 (get-in parsed [:hookSpecificOutput :permissionDecision])))
          (is (str/includes?
                (get-in parsed [:hookSpecificOutput :permissionDecisionReason])
                "malformed config"))))
      (finally (fs/delete-tree tmp-repo)))))
