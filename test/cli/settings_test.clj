(ns cli.settings-test
  (:require [clojure.test :refer [deftest is testing]]
            [cli.install]
            [cli.settings :as settings]
            [babashka.fs :as fs]))

(defn- with-tmp-settings [f]
  (let [tmp (str (fs/create-temp-file {:prefix "settings-" :suffix ".json"}))]
    (try
      (spit tmp "{}")
      (f tmp)
      (finally
        (fs/delete tmp)))))

(deftest test-read-write-settings
  (with-tmp-settings
    (fn [tmp]
      (testing "write then read round-trips"
        (let [data {:permissions {:allow ["Read" "Edit"]}
                    :hooks {:PreToolUse [{:matcher "Edit" :hooks [{:type "command" :command "echo hi"}]}]}}]
          (settings/write-settings! tmp data)
          (is (= data (settings/read-settings tmp)))))

      (testing "read nonexistent returns empty map"
        (is (= {} (settings/read-settings "/nonexistent/settings.json")))))))

;; --- Dispatch entries ---

(deftest test-add-dispatch-entry
  (with-tmp-settings
    (fn [tmp]
      (testing "adds universal dispatch entry for a tool event"
        (settings/add-dispatch-entry! tmp "PreToolUse" :matcher ".*")
        (let [s     (settings/read-settings tmp)
              entry (first (get-in s [:hooks :PreToolUse]))
              hook  (first (:hooks entry))]
          (is (= ".*" (:matcher entry)))
          (is (= "http" (:type hook)))
          (is (= "http://127.0.0.1:8888/dispatch/PreToolUse" (:url hook)))
          (is (= 30 (:timeout hook)))))

      (testing "idempotent — reinstall replaces, doesn't duplicate"
        (settings/add-dispatch-entry! tmp "PreToolUse" :matcher ".*")
        (let [hooks (get-in (settings/read-settings tmp) [:hooks :PreToolUse])]
          (is (= 1 (count hooks))))))))

(deftest test-add-dispatch-entry-non-tool-event
  (with-tmp-settings
    (fn [tmp]
      (testing "non-tool event omits :matcher"
        (settings/add-dispatch-entry! tmp "SessionStart")
        (let [entry (first (get-in (settings/read-settings tmp) [:hooks :SessionStart]))]
          (is (nil? (:matcher entry))))))))

(deftest test-dispatch-preserves-co-located
  (with-tmp-settings
    (fn [tmp]
      (let [existing {:hooks
                      {:PreToolUse
                       [{:matcher "Edit|Write"
                         :hooks [{:type "command" :command "ruff format --quiet $FILE"}]}]}}]
        (settings/write-settings! tmp existing))

      (testing "adding dispatch entry preserves the non-cch ruff entry"
        (settings/add-dispatch-entry! tmp "PreToolUse" :matcher ".*")
        (let [hooks (get-in (settings/read-settings tmp) [:hooks :PreToolUse])]
          (is (= 2 (count hooks)))
          ;; ruff entry still there
          (is (some (fn [entry]
                      (some #(re-find #"ruff" (or (:command %) ""))
                            (:hooks entry)))
                    hooks))))

      (testing "removing dispatch entry leaves the ruff entry intact"
        (settings/remove-dispatch-entry! tmp "PreToolUse")
        (let [hooks (get-in (settings/read-settings tmp) [:hooks :PreToolUse])]
          (is (= 1 (count hooks)))
          (is (re-find #"ruff"
                       (get-in (first hooks) [:hooks 0 :command]))))))))

;; --- Prompt entries ---

(deftest test-add-prompt-entry
  (with-tmp-settings
    (fn [tmp]
      (testing "writes native prompt hook with :__cch tag"
        (settings/add-prompt-entry! tmp "PreToolUse" "Bash"
                                    "bash-safety"
                                    {:prompt-template "Review: $ARGUMENTS"
                                     :model           "claude-haiku-4-5-20251001"
                                     :status-message  "Checking…"})
        (let [entry (first (get-in (settings/read-settings tmp) [:hooks :PreToolUse]))
              hook  (first (:hooks entry))]
          (is (= "Bash" (:matcher entry)))
          (is (= "prompt" (:type hook)))
          (is (= "Review: $ARGUMENTS" (:prompt hook)))
          (is (= "claude-haiku-4-5-20251001" (:model hook)))
          (is (= "Checking…" (:statusMessage hook)))
          (is (= "prompt:bash-safety" (:__cch hook)))))

      (testing "reinstall replaces rather than duplicates"
        (settings/add-prompt-entry! tmp "PreToolUse" "Bash" "bash-safety"
                                    {:prompt-template "v2: $ARGUMENTS"})
        (let [hooks (get-in (settings/read-settings tmp) [:hooks :PreToolUse])]
          (is (= 1 (count hooks)))
          (is (= "v2: $ARGUMENTS"
                 (get-in (first hooks) [:hooks 0 :prompt]))))))))

;; --- Agent entries ---

(deftest test-add-agent-entry
  (with-tmp-settings
    (fn [tmp]
      (testing "writes native agent hook with :__cch tag"
        (settings/add-agent-entry! tmp "PreToolUse" "Edit"
                                   "edit-review"
                                   {:agent-spec {:agentName "reviewer"
                                                 :prompt    "review this edit"}
                                    :timeout    120})
        (let [hook (first (:hooks (first (get-in (settings/read-settings tmp) [:hooks :PreToolUse]))))]
          (is (= "agent" (:type hook)))
          (is (= "reviewer" (:agentName hook)))
          (is (= 120 (:timeout hook)))
          (is (= "agent:edit-review" (:__cch hook))))))))

;; --- Full cleanup ---

(deftest test-remove-all-cch
  (with-tmp-settings
    (fn [tmp]
      ;; Drop in mixed cch + non-cch entries
      (settings/write-settings!
        tmp
        {:hooks
         {:PreToolUse [{:matcher "Edit|Write"
                        :hooks [{:type "command" :command "ruff format --quiet"}
                                {:type "http" :url "http://127.0.0.1:8888/dispatch/PreToolUse" :timeout 30}]}]
          :SessionStart [{:hooks [{:type "prompt"
                                   :prompt "Brief me"
                                   :__cch "prompt:briefing"}]}]
          :Stop [{:hooks [{:type "command" :command "bash -c ':'"}]}]}})

      (testing "removes dispatch, prompt, and agent entries"
        (settings/remove-all-cch! tmp)
        (let [s (settings/read-settings tmp)]
          ;; PreToolUse: only ruff survives
          (let [hooks (get-in s [:hooks :PreToolUse])]
            (is (= 1 (count hooks)))
            (is (re-find #"ruff" (get-in (first hooks) [:hooks 0 :command]))))
          ;; SessionStart: cch prompt entry was the only one → empty after prune
          (is (empty? (get-in s [:hooks :SessionStart])))
          ;; Stop: non-cch entry untouched
          (is (= 1 (count (get-in s [:hooks :Stop])))))))))

(deftest test-remove-all-cch-cleans-legacy-entries
  (with-tmp-settings
    (fn [tmp]
      ;; Simulate a pre-dispatcher install: per-hook command + http entries
      ;; sharing matcher groups with non-cch hooks.
      (settings/write-settings!
        tmp
        {:hooks
         {:PermissionRequest
          [{:hooks [{:type "command"
                     :command "bb -cp x -m hooks.event-log # cch:event-log"}]}]
          :PreToolUse
          [{:matcher "Edit|Write"
            :hooks [{:type "command" :command "ruff format --quiet"}
                    {:type "command"
                     :command "bb -m hooks.scope-lock # cch:scope-lock"}
                    {:type "http"
                     :url "http://127.0.0.1:8888/hooks/protect-files"
                     :timeout 5}]}]}})

      (testing "recognizes both legacy command tags and legacy /hooks/ URLs"
        (settings/remove-all-cch! tmp)
        (let [s (settings/read-settings tmp)]
          ;; PermissionRequest: only cch-tagged entry was present → empty
          (is (empty? (get-in s [:hooks :PermissionRequest])))
          ;; PreToolUse: ruff survives, both legacy cch entries removed
          (let [hooks (get-in s [:hooks :PreToolUse])]
            (is (= 1 (count hooks)))
            (is (re-find #"ruff" (get-in (first hooks) [:hooks 0 :command])))))))))

;; --- URL helper ---

(deftest test-dispatch-url
  (is (= "http://127.0.0.1:8888/dispatch/PreToolUse"
         (settings/dispatch-url "PreToolUse")))
  (is (= "http://localhost:9999/dispatch/SessionStart"
         (settings/dispatch-url "SessionStart" :host "localhost" :port 9999))))

;; --- Install / uninstall basics (via reachability helper too) ---

(deftest test-server-reachable
  (testing "returns true when something accepts on the port"
    (with-open [srv (java.net.ServerSocket. 0)]
      (let [port (.getLocalPort srv)]
        (is (= true (cli.install/server-reachable? "127.0.0.1" port))))))
  (testing "returns false on a port nothing is listening on"
    (let [port (with-open [srv (java.net.ServerSocket. 0)]
                 (.getLocalPort srv))]
      (is (= false (cli.install/server-reachable? "127.0.0.1" port))))))
