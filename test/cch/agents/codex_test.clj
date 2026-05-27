(ns cch.agents.codex-test
  (:require [clojure.test :refer [deftest is testing]]
            [cch.agents.codex :as codex]
            [cli.codex-settings :as cs]
            [babashka.fs :as fs]
            [clojure.string :as str]))

(defn- with-tmp [f]
  (let [tmp (str (fs/create-temp-file {:prefix "codex-agent-" :suffix ".toml"}))]
    (try (f tmp) (finally (fs/delete tmp)))))

(deftest dispatch-command-uses-cch-dispatcher-url
  (is (= "curl -s -X POST --data-binary @- http://127.0.0.1:8888/dispatch/PreToolUse"
         (codex/dispatch-command "PreToolUse"))))

(deftest dispatch-command-respects-host-port
  (is (= "curl -s -X POST --data-binary @- http://10.0.0.1:9999/dispatch/SessionStart"
         (codex/dispatch-command "SessionStart" :host "10.0.0.1" :port 9999))))

(deftest registry->entries-defaults-matcher-to-wildcard
  (let [out (codex/registry->entries [{:event "PreCompact" :matcher nil}])]
    (is (= 1 (count out)))
    (is (= ".*" (:matcher (first out))))
    (is (= "PreCompact" (:event (first out))))
    (is (= 30 (:timeout (first out))))))

(deftest registry->entries-preserves-explicit-matcher
  (let [out (codex/registry->entries [{:event "PreToolUse" :matcher "Bash|Edit"}])]
    (is (= "Bash|Edit" (:matcher (first out))))))

(deftest registry->entries-drops-events-codex-does-not-support
  (testing "Claude-only events (e.g. WorktreeRemove, TaskCreated) are filtered out"
    (let [events [{:event "PreToolUse"     :matcher ".*"}
                  {:event "WorktreeRemove" :matcher ".*"}
                  {:event "TaskCreated"    :matcher ".*"}
                  {:event "SessionStart"   :matcher nil}]
          out (codex/registry->entries events)]
      (is (= 2 (count out)) "only PreToolUse and SessionStart survive")
      (is (= #{"PreToolUse" "SessionStart"} (set (map :event out)))))))

(deftest unsupported-events-reports-dropped-event-names
  (let [events [{:event "PreToolUse"     :matcher ".*"}
                {:event "WorktreeRemove" :matcher ".*"}
                {:event "TaskCreated"    :matcher ".*"}]]
    (is (= ["TaskCreated" "WorktreeRemove"] (codex/unsupported-events events)))))

(deftest install-returns-result-map-with-counts-and-skipped
  (with-tmp
    (fn [tmp]
      (spit tmp "")
      (let [result (codex/install! :config-path tmp)]
        (is (= tmp (:path result)))
        (is (pos? (:written result)))
        (is (every? string? (:skipped result)))
        (is (not-any? codex/supported-events (:skipped result))
            "skipped list must not contain any Codex-supported event")))))

(deftest install-writes-block-with-all-dispatcher-events
  (with-tmp
    (fn [tmp]
      (spit tmp "")
      (codex/install! :config-path tmp)
      (let [contents (slurp tmp)]
        (is (str/includes? contents (str "# cch:begin " codex/block-name)))
        (is (str/includes? contents (str "# cch:end " codex/block-name)))
        (is (str/includes? contents "[[hooks.PreToolUse]]"))
        (is (str/includes? contents "[[hooks.PostToolUse]]"))
        (is (str/includes? contents "curl -s -X POST --data-binary @- http://127.0.0.1:8888/dispatch/PreToolUse"))))))

(deftest install-preserves-user-content
  (with-tmp
    (fn [tmp]
      (spit tmp "model = \"gpt-5.5\"\n")
      (codex/install! :config-path tmp)
      (is (str/starts-with? (slurp tmp) "model = \"gpt-5.5\"\n")))))

(deftest install-is-idempotent
  (with-tmp
    (fn [tmp]
      (spit tmp "")
      (codex/install! :config-path tmp)
      (codex/install! :config-path tmp)
      (let [contents (slurp tmp)
            begin-count (count (re-seq (re-pattern (str "# cch:begin " codex/block-name))
                                       contents))]
        (is (= 1 begin-count) "second install replaces, doesn't duplicate")))))

(deftest uninstall-strips-cch-block-only
  (with-tmp
    (fn [tmp]
      (let [user "model = \"gpt-5.5\"\n"]
        (spit tmp user)
        (codex/install! :config-path tmp)
        (codex/uninstall! :config-path tmp)
        (is (= user (slurp tmp)))))))
