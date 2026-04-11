(ns cli.settings-test
  (:require [clojure.test :refer [deftest is testing]]
            [cli.settings :as settings]
            [babashka.fs :as fs]))

(deftest test-read-write-settings
  (let [tmp (str (fs/create-temp-file {:prefix "settings-" :suffix ".json"}))]
    (try
      (testing "write then read round-trips"
        (let [data {:permissions {:allow ["Read" "Edit"]}
                    :hooks {:PreToolUse [{:matcher "Edit" :hooks [{:type "command" :command "echo hi"}]}]}}]
          (settings/write-settings! tmp data)
          (is (= data (settings/read-settings tmp)))))

      (testing "read nonexistent returns empty map"
        (is (= {} (settings/read-settings "/nonexistent/settings.json"))))
      (finally
        (fs/delete tmp)))))

(deftest test-add-hook
  (let [tmp (str (fs/create-temp-file {:prefix "settings-" :suffix ".json"}))]
    (try
      (spit tmp "{}")

      (testing "adds a hook entry"
        (settings/add-hook! tmp "PreToolUse" "Edit|Write" "hooks.scope-lock")
        (let [s (settings/read-settings tmp)
              hooks (get-in s [:hooks :PreToolUse])]
          (is (= 1 (count hooks)))
          (is (= "Edit|Write" (:matcher (first hooks))))
          (is (re-find #"hooks\.scope-lock" (get-in (first hooks) [:hooks 0 :command])))))

      (testing "replaces existing hook with same tag"
        (settings/add-hook! tmp "PreToolUse" "Edit" "hooks.scope-lock")
        (let [s (settings/read-settings tmp)
              hooks (get-in s [:hooks :PreToolUse])]
          (is (= 1 (count hooks)))
          (is (= "Edit" (:matcher (first hooks))))))
      (finally
        (fs/delete tmp)))))

(deftest test-remove-hook
  (let [tmp (str (fs/create-temp-file {:prefix "settings-" :suffix ".json"}))]
    (try
      (spit tmp "{}")
      (settings/add-hook! tmp "PreToolUse" "Edit|Write" "hooks.scope-lock")

      (testing "removes a hook by tag name"
        (settings/remove-hook! tmp "PreToolUse" "scope-lock")
        (let [s (settings/read-settings tmp)
              hooks (get-in s [:hooks :PreToolUse])]
          (is (empty? hooks))))
      (finally
        (fs/delete tmp)))))

(deftest test-preserves-co-located-hooks
  (let [tmp (str (fs/create-temp-file {:prefix "settings-" :suffix ".json"}))]
    (try
      ;; Start with a non-cch hook already in place
      (let [existing {:hooks
                      {:PreToolUse
                       [{:matcher "Edit|Write"
                         :hooks [{:type "command" :command "ruff format --quiet $FILE"}
                                 {:type "command" :command "bb -m hooks.scope-lock # cch:scope-lock"}]}]}}]
        (settings/write-settings! tmp existing)

        (testing "reinstalling cch hook preserves non-cch hooks"
          (settings/add-hook! tmp "PreToolUse" "Edit|Write" "hooks.scope-lock")
          (let [s (settings/read-settings tmp)
                hooks (get-in s [:hooks :PreToolUse])]
            ;; Should have 2 entries: the original (with ruff only) + the new cch entry
            (is (= 2 (count hooks)))
            ;; First entry should still have ruff
            (is (re-find #"ruff" (get-in (first hooks) [:hooks 0 :command])))
            ;; Second entry should be the cch hook
            (is (re-find #"cch:scope-lock" (get-in (second hooks) [:hooks 0 :command])))))

        (testing "uninstalling cch hook preserves non-cch hooks"
          (settings/remove-hook! tmp "PreToolUse" "scope-lock")
          (let [s (settings/read-settings tmp)
                hooks (get-in s [:hooks :PreToolUse])]
            ;; Should still have the ruff entry
            (is (= 1 (count hooks)))
            (is (re-find #"ruff" (get-in (first hooks) [:hooks 0 :command]))))))
      (finally
        (fs/delete tmp)))))

(deftest test-hook-command-format
  (let [cmd (settings/hook-command "hooks.scope-lock")]
    (testing "includes framework classpath"
      (is (re-find #"cch/repo/src" cmd)))
    (testing "includes resources classpath"
      (is (re-find #"cch/repo/resources" cmd)))
    (testing "project classpath comes first (for overrides)"
      (is (re-find #"^bb -cp \"\$CLAUDE_PROJECT_DIR" cmd)))
    (testing "includes namespace"
      (is (re-find #"-m hooks\.scope-lock" cmd)))
    (testing "includes cch tag"
      (is (re-find #"# cch:scope-lock" cmd)))))
