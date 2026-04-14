(ns cli.registry-test
  (:require [clojure.test :refer [deftest is testing]]
            [cli.registry :as registry]))

(deftest test-hook-type-defaults-to-code
  (is (= :code (registry/hook-type {})))
  (is (= :code (registry/hook-type {:ns "hooks.foo" :event "PreToolUse"})))
  (is (= :prompt (registry/hook-type {:type :prompt})))
  (is (= :agent  (registry/hook-type {:type :agent}))))

(deftest test-hook-events-normalization
  (testing "single-event entry → single [{:event :matcher}] vector"
    (is (= [{:event "PreToolUse" :matcher "Edit|Write"}]
           (registry/hook-events {:event "PreToolUse" :matcher "Edit|Write"}))))

  (testing "multi-event entry → returns :events as-is"
    (let [evs [{:event "PreToolUse" :matcher ".*"}
               {:event "SessionStart" :matcher nil}]]
      (is (= evs (registry/hook-events {:events evs})))))

  (testing "no event — returns nil (legal for nothing-registered test fixtures)"
    (is (nil? (registry/hook-events {})))))

(deftest test-dispatcher-events-dedups-and-keeps-matchers
  (let [pairs (registry/dispatcher-events)
        events (map :event pairs)]
    (testing "each event appears at most once"
      (is (= (count events) (count (distinct events)))))

    (testing "PreToolUse uses '.*' because tool hooks subscribe to it"
      (let [{:keys [matcher]} (first (filter #(= "PreToolUse" (:event %)) pairs))]
        (is (= ".*" matcher))))

    (testing "SessionStart has nil matcher (non-tool event)"
      (let [{:keys [matcher]} (first (filter #(= "SessionStart" (:event %)) pairs))]
        (is (nil? matcher))))))

(deftest test-matcher-matches?
  (testing "nil matcher matches everything (non-tool event)"
    (is (true? (registry/matcher-matches? nil nil)))
    (is (true? (registry/matcher-matches? nil "Edit"))))
  (testing "nil tool-name matches (no filtering possible)"
    (is (true? (registry/matcher-matches? "Edit" nil))))
  (testing "regex matches substring"
    (is (true? (registry/matcher-matches? "Edit|Write" "Edit")))
    (is (true? (registry/matcher-matches? "Edit|Write" "Write"))))
  (testing "non-match returns false"
    (is (false? (registry/matcher-matches? "Edit|Write" "Read")))))

;; --- Validator ---

(defn- with-registry [fake-hooks f]
  (with-redefs [registry/hooks fake-hooks]
    (f)))

(deftest test-validate-accepts-valid-code-entry
  (with-registry
    {"ok" {:type :code :ns "hooks.ok" :event "PreToolUse"
           :description "fine"}}
    #(is (nil? (do (registry/validate-registry!) nil)))))

(deftest test-validate-rejects-code-without-ns
  (with-registry
    {"bad" {:type :code :event "PreToolUse" :description "missing ns"}}
    (fn []
      (let [ex (try (registry/validate-registry!) nil
                    (catch clojure.lang.ExceptionInfo e e))]
        (is (some? ex))
        (is (= :cli.registry/invalid-registry (:type (ex-data ex))))
        (is (some #(re-find #"requires :ns" %) (:errors (ex-data ex))))))))

(deftest test-validate-rejects-prompt-without-template
  (with-registry
    {"bad" {:type :prompt :event "PreToolUse" :description "no template"}}
    (fn []
      (let [ex (try (registry/validate-registry!) nil
                    (catch clojure.lang.ExceptionInfo e e))]
        (is (some? ex))
        (is (some #(re-find #"requires :prompt-template" %) (:errors (ex-data ex))))))))

(deftest test-validate-rejects-mixing-code-and-prompt-fields
  (with-registry
    {"bad" {:type :code :ns "hooks.x" :event "PreToolUse"
            :description "mixed" :prompt-template "nope"}}
    (fn []
      (let [ex (try (registry/validate-registry!) nil
                    (catch clojure.lang.ExceptionInfo e e))]
        (is (some? ex))))))

(deftest test-validate-rejects-unknown-type
  (with-registry
    {"bad" {:type :quantum :description "invalid type"}}
    (fn []
      (let [ex (try (registry/validate-registry!) nil
                    (catch clojure.lang.ExceptionInfo e e))]
        (is (some? ex))
        (is (some #(re-find #"unknown :type" %) (:errors (ex-data ex))))))))

(deftest test-real-registry-is-valid
  (testing "the built-in registry passes validation"
    (is (nil? (do (registry/validate-registry!) nil)))))
