(ns cli.settings-test
  (:require [clojure.test :refer [deftest is testing]]
            [cli.install]
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

(deftest test-server-reachable
  (testing "returns true when something accepts on the port"
    (with-open [srv (java.net.ServerSocket. 0)]
      (let [port (.getLocalPort srv)]
        (is (= true (#'cli.install/server-reachable? "127.0.0.1" port))))))
  (testing "returns false on a port nothing is listening on (with a short timeout)"
    ;; Pick a port, close the listener, then probe — connect refused.
    (let [port (with-open [srv (java.net.ServerSocket. 0)]
                 (.getLocalPort srv))]
      (is (= false (#'cli.install/server-reachable? "127.0.0.1" port))))))

(deftest test-add-hook-http-mode
  (let [tmp (str (fs/create-temp-file {:prefix "settings-" :suffix ".json"}))]
    (try
      (spit tmp "{}")

      (testing "http install writes type:http with URL and timeout"
        (settings/add-hook! tmp "PreToolUse" "Edit|Write" "hooks.scope-lock" :mode :http)
        (let [s     (settings/read-settings tmp)
              entry (first (get-in s [:hooks :PreToolUse]))
              hook  (first (:hooks entry))]
          (is (= "http" (:type hook)))
          (is (= "http://127.0.0.1:8888/hooks/scope-lock" (:url hook)))
          (is (= 5 (:timeout hook)))
          (is (= "Edit|Write" (:matcher entry)))))

      (testing "reinstall with same hook replaces the prior HTTP entry (not duplicate)"
        (settings/add-hook! tmp "PreToolUse" "Edit|Write" "hooks.scope-lock" :mode :http)
        (let [hooks (get-in (settings/read-settings tmp) [:hooks :PreToolUse])]
          (is (= 1 (count hooks)))))

      (testing "switching from http back to command replaces the HTTP entry"
        (settings/add-hook! tmp "PreToolUse" "Edit|Write" "hooks.scope-lock" :mode :command)
        (let [hooks (get-in (settings/read-settings tmp) [:hooks :PreToolUse])
              hook  (first (:hooks (first hooks)))]
          (is (= 1 (count hooks)))
          (is (= "command" (:type hook)))
          (is (re-find #"# cch:scope-lock" (:command hook)))))
      (finally
        (fs/delete tmp)))))
