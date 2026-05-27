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
