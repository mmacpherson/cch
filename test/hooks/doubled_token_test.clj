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

(deftest test-cli-integration-untouched-artifact-stays-silent
  (let [tmp (str (fs/create-temp-file {:prefix "dt-int-bad-" :suffix ".py"}))]
    (try
      (spit tmp "self.observations_observations_latest = compute()\n")
      ;; The file already contains a doubled token, but this edit does not
      ;; touch it — it swaps compute() for fetch(). Blocking here is what
      ;; the hook used to do and is exactly the defect: a pre-existing
      ;; match made every later edit to the file unlandable.
      (let [input  (json/generate-string
                     {:hook_event_name "PostToolUse"
                      :cwd             "/tmp"
                      :tool_name       "Edit"
                      :tool_input      {:file_path   tmp
                                        :old_string  "compute()"
                                        :new_string  "fetch()"
                                        :replace_all false}})
            result (ts/run-hook "hooks.doubled-token" input {:dir repo-root})]
        (is (zero? (:exit result)))
        (is (str/blank? (:out result))
            "an edit that introduces nothing must stay silent"))
      (finally
        (fs/delete tmp)))))

(deftest test-cli-integration-introduced-artifact-emits-block
  (let [tmp (str (fs/create-temp-file {:prefix "doubled-" :suffix ".py"}))]
    (try
      (spit tmp "x = 1\n")
      (let [input  (json/generate-string
                     {:hook_event_name "PostToolUse"
                      :cwd             "/tmp"
                      :tool_name       "Edit"
                      :tool_input      {:file_path   tmp
                                        :old_string  "get_b2_client()"
                                        :new_string  "get_b2_b2_client()"
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

;; --- Unit tests: check-edit (only what the edit introduces) ---

(deftest check-edit-ignores-pre-existing-matches
  (testing "a match already in the file is not the edit's fault"
    (is (nil? (dt/check-edit "/tmp/x.md"
                             "see sample-project here"
                             "see sample-project there")))))

(deftest check-edit-flags-introduced-artifact
  (let [r (dt/check-edit "/tmp/x.py" "get_b2_client()" "get_b2_b2_client()")]
    (is (= :block (:decision r)))
    (is (str/includes? (:reason r) "get_b2_b2_client"))
    (is (str/includes? (:reason r) "introduced"))))

(deftest check-edit-allows-removing-an-artifact
  (testing "cleaning one up must not be mistaken for adding one"
    (is (nil? (dt/check-edit "/tmp/x.py" "get_b2_b2_client()" "get_b2_client()")))))

(deftest check-edit-handles-absent-old-string
  (testing "with no prior text every match is introduced"
    (let [r (dt/check-edit "/tmp/x.py" nil "observations_observations_latest = 1")]
      (is (= :block (:decision r))))))

(deftest check-edit-clean-edit-is-silent
  (is (nil? (dt/check-edit "/tmp/x.py" "a = 1" "b = 2"))))
