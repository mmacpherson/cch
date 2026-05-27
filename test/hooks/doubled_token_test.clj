(ns hooks.doubled-token-test
  (:require [clojure.test :refer [deftest is testing]]
            [hooks.doubled-token :as dt]
            [cheshire.core :as json]
            [babashka.fs :as fs]
            [babashka.process :as p]
            [clojure.string :as str]
            [test-support :as ts]))

(def repo-root
  (str/trim (:out (p/sh ["git" "rev-parse" "--show-toplevel"]))))

;; --- Unit tests: find-doubled-tokens (pure, no I/O) ---

(deftest test-nil-and-blank-return-nil
  (is (nil? (dt/find-doubled-tokens nil)))
  (is (nil? (dt/find-doubled-tokens "")))
  (is (nil? (dt/find-doubled-tokens "   "))))

(deftest test-clean-code-returns-nil
  (is (nil? (dt/find-doubled-tokens "def get_b2_client():\n    return client")))
  (is (nil? (dt/find-doubled-tokens "observations_latest = compute()")))
  (is (nil? (dt/find-doubled-tokens "foo_bar_baz = 1"))))

(deftest test-separator-doubled-detected
  (testing "underscore doubled"
    (let [hits (dt/find-doubled-tokens "x = latest_latest")]
      (is (some? hits))
      (is (str/includes? (:match (first hits)) "latest_latest"))))
  (testing "hyphen doubled"
    (let [hits (dt/find-doubled-tokens "class=\"btn-btn-extra\"")]
      (is (some? hits))
      (is (str/includes? (:match (first hits)) "btn-btn")))))

(deftest test-concat-doubled-detected
  (testing "concatenated without separator"
    (let [hits (dt/find-doubled-tokens "getget_b2_client = None")]
      (is (some? hits))
      (is (= "getget" (:match (first hits)))))))

(deftest test-compound-doubled-detected
  (testing "multi-part repeated subsequence"
    (let [hits (dt/find-doubled-tokens "x = get_b2_get_b2_client")]
      (is (some? hits))
      (is (str/includes? (:match (first hits)) "get_b2_get_b2")))))

(deftest test-line-numbers-correct
  (let [hits (dt/find-doubled-tokens "clean = 1\nfoo_foo = 2\nbar = 3")]
    (is (= 1 (count hits)))
    (is (= 2 (:line (first hits))))))

(deftest test-multiple-hits-on-one-line
  (let [hits (dt/find-doubled-tokens "x = foo_foo + bar_bar")]
    (is (= 2 (count hits)))))

(deftest test-real-world-patterns
  (testing "observations_latest_latest"
    (is (some? (dt/find-doubled-tokens "self.observations_latest_latest = x"))))
  (testing "trait_observations_latest_latest"
    (is (some? (dt/find-doubled-tokens "trait_observations_latest_latest"))))
  (testing "get_b2_get_b2_client"
    (is (some? (dt/find-doubled-tokens "client = get_b2_get_b2_client()")))))

(deftest test-no-false-positives-on-common-patterns
  (testing "__init__ is not doubled"
    (is (nil? (dt/find-doubled-tokens "def __init__(self):"))))
  (testing "normal snake_case"
    (is (nil? (dt/find-doubled-tokens "get_b2_client = create_session()"))))
  (testing "normal camelCase"
    (is (nil? (dt/find-doubled-tokens "getB2Client = createSession()")))))

;; --- Unit tests: check-file ---

(deftest test-check-file-nonexistent-returns-nil
  (is (nil? (dt/check-file "/tmp/does-not-exist-ever-12345.py"))))

(deftest test-check-file-clean-returns-nil
  (let [tmp (str (fs/create-temp-file {:prefix "dt-clean-" :suffix ".py"}))]
    (try
      (spit tmp "def foo_bar():\n    return 1\n")
      (is (nil? (dt/check-file tmp)))
      (finally
        (fs/delete tmp)))))

(deftest test-check-file-doubled-returns-decision
  (let [tmp (str (fs/create-temp-file {:prefix "dt-bad-" :suffix ".py"}))]
    (try
      (spit tmp "x = latest_latest\ny = foo_bar\n")
      (let [result (dt/check-file tmp)]
        (is (= :block (:decision result)))
        (is (str/includes? (:reason result) "doubled-token"))
        (is (str/includes? (:reason result) "latest_latest")))
      (finally
        (fs/delete tmp)))))

;; --- Integration: subprocess the hook, verify PostToolUse response shape ---

(deftest test-cli-integration-clean-file-no-output
  (let [tmp (str (fs/create-temp-file {:prefix "dt-int-clean-" :suffix ".py"}))]
    (try
      (spit tmp "def get_client():\n    return None\n")
      (let [input  (json/generate-string
                     {:hook_event_name "PostToolUse"
                      :cwd             "/tmp"
                      :tool_name       "Edit"
                      :tool_input      {:file_path   tmp
                                        :old_string  "None"
                                        :new_string  "Client()"
                                        :replace_all false}})
            result (ts/run-hook "hooks.doubled-token" input {:dir repo-root})]
        (is (zero? (:exit result)))
        (is (str/blank? (:out result))))
      (finally
        (fs/delete tmp)))))

(deftest test-cli-integration-doubled-file-emits-block
  (let [tmp (str (fs/create-temp-file {:prefix "dt-int-bad-" :suffix ".py"}))]
    (try
      (spit tmp "self.observations_observations_latest = compute()\n")
      (let [input  (json/generate-string
                     {:hook_event_name "PostToolUse"
                      :cwd             "/tmp"
                      :tool_name       "Edit"
                      :tool_input      {:file_path   tmp
                                        :old_string  "compute()"
                                        :new_string  "fetch()"
                                        :replace_all false}})
            result (ts/run-hook "hooks.doubled-token" input {:dir repo-root})
            parsed (json/parse-string (:out result) true)]
        (is (zero? (:exit result)))
        (is (= "block" (:decision parsed)))
        (is (str/includes? (:reason parsed) "doubled-token"))
        (is (nil? (get-in parsed [:hookSpecificOutput :permissionDecision]))
            "must NOT use PreToolUse-shaped response"))
      (finally
        (fs/delete tmp)))))
