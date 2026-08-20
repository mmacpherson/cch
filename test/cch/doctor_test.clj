(ns cch.doctor-test
  (:require [cch.agents.agy :as agy]
            [cch.agents.codex :as codex]
            [cch.doctor :as doctor]
            [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]))

;; ---------------------------------------------------------------------------
;; Pure detectors — no I/O, fixture content only.
;; ---------------------------------------------------------------------------

(deftest claude-dispatch-installed?-detects-dispatch-url
  (testing "true when a hook points at /dispatch/"
    (is (doctor/claude-dispatch-installed?
          {:hooks {:PreToolUse [{:matcher "*"
                                 :hooks [{:url "http://127.0.0.1:8888/dispatch/PreToolUse"}]}]}})))
  (testing "false for settings with only unrelated hooks"
    (is (not (doctor/claude-dispatch-installed?
               {:hooks {:PreToolUse [{:matcher "*"
                                      :hooks [{:command "echo hi"}]}]}}))))
  (testing "false for empty settings"
    (is (not (doctor/claude-dispatch-installed? {})))))

(deftest codex-block-installed?-detects-sentinel
  (testing "sentinel block form"
    (is (doctor/codex-block-installed?
          (str "foo\n# cch:begin " codex/block-name "\nbar\n# cch:end " codex/block-name))))
  (testing "raw dispatch-command form (pre-sentinel installs)"
    (is (doctor/codex-block-installed?
          (str "[[hooks.PreToolUse.hooks]]\ntype = \"command\"\n"
               "command = \"curl -s -X POST -H 'X-CCH-Agent: codex' "
               "--data-binary @- http://127.0.0.1:8888/dispatch/PreToolUse\""))))
  (testing "a dispatch URL for a different agent is not a codex install"
    (is (not (doctor/codex-block-installed?
               "command = \"curl ... -H 'X-CCH-Agent: agy' .../dispatch/Stop\""))))
  (is (not (doctor/codex-block-installed? "no cch here")))
  (is (not (doctor/codex-block-installed? nil))))

(deftest agy-hooks-installed?-detects-key
  (is (doctor/agy-hooks-installed? {agy/hook-key {"PreToolUse" []}}))
  (is (not (doctor/agy-hooks-installed? {"user-hook" {}})))
  (is (not (doctor/agy-hooks-installed? nil))))

(deftest codex-trusted-projects-parses-toml
  (let [toml (str "[projects.\"/home/u/a\"]\n"
                  "trust_level = \"trusted\"\n\n"
                  "[projects.\"/home/u/b\"]\n"
                  "trust_level = \"untrusted\"\n\n"
                  "[projects.\"/home/u/c\"]\n"
                  "trust_level = \"trusted\"\n")
        trusted (doctor/codex-trusted-projects toml)]
    (is (= #{"/home/u/a" "/home/u/c"} trusted)
        "only trust_level=trusted projects are collected")
    (is (empty? (doctor/codex-trusted-projects ""))))
  (testing "a non-projects section between does not leak trust to a prior project"
    (let [toml (str "[projects.\"/home/u/a\"]\n"
                    "[hooks.PreToolUse]\n"
                    "trust_level = \"trusted\"\n")]
      (is (empty? (doctor/codex-trusted-projects toml))
          "trust_level under [hooks...] must not attach to the project header"))))

(deftest path-covered-by?-matches-self-and-descendants
  (let [roots #{"/home/u/proj"}]
    (is (doctor/path-covered-by? roots "/home/u/proj"))
    (is (doctor/path-covered-by? roots "/home/u/proj/src/x"))
    (is (not (doctor/path-covered-by? roots "/home/u/project")) "prefix but not a child")
    (is (not (doctor/path-covered-by? roots "/home/u")))))

(deftest enabled-code-hooks-filters-to-enabled-code-hooks
  (let [rows [{:hook-name "command-guard" :enabled true}    ; real code hook
              {:hook-name "scope-lock"    :enabled false}   ; code hook, disabled
              {:hook-name "not-a-hook"    :enabled true}]]  ; unknown → excluded
    (let [enabled (doctor/enabled-code-hooks rows)]
      (is (contains? enabled "command-guard"))
      (is (not (contains? enabled "scope-lock")) "disabled excluded")
      (is (not (contains? enabled "not-a-hook")) "non-code excluded"))))

;; ---------------------------------------------------------------------------
;; Report model.
;; ---------------------------------------------------------------------------

(deftest agent-note-covers-codex-trust-states
  (is (= "not on this box"
         (doctor/agent-note {:agent "codex" :present? false :installed? false})))
  (is (str/includes?
        (doctor/agent-note {:agent "codex" :present? true :installed? true
                            :extra {:trusted-projects 3 :cwd-trusted? true}})
        "trusted codex project"))
  (is (str/includes?
        (doctor/agent-note {:agent "codex" :present? true :installed? true
                            :extra {:trusted-projects 3 :cwd-trusted? false}})
        "NOT a trusted"))
  (is (str/includes?
        (doctor/agent-note {:agent "codex" :present? true :installed? true
                            :extra {:trusted-projects 0 :cwd-trusted? false}})
        "no trusted codex projects")))

(deftest agent-note-flags-unwired-present-agent
  (is (str/includes?
        (doctor/agent-note {:agent "agy" :present? true :installed? false})
        "cch install")))

(deftest problems-fails-on-down-server-and-unwired-agent
  (let [box    {:server {:reachable? false} :hooks {:enabled #{}}}
        agents [{:agent "codex" :present? true :installed? false}]]
    (let [probs (doctor/problems box agents)]
      (is (some #(str/includes? % "not reachable") probs))
      (is (some #(str/includes? % "codex present but cch not wired") probs))))
  (testing "no problems when server up and everything present is wired"
    (let [box    {:server {:reachable? true} :hooks {:enabled #{}}}
          agents [{:agent "codex" :present? true :installed? true
                   :extra {:trusted-projects 0 :cwd-trusted? false}}
                  {:agent "agy" :present? false :installed? false}]]
      (is (empty? (doctor/problems box agents))
          "codex trust is advisory — must NOT fail the exit code"))))

(deftest render-produces-a-table
  (let [box    {:server {:host "127.0.0.1" :port 8888 :reachable? true}
                :hooks {:enabled #{"command-guard" "scope-lock"}}}
        agents [{:agent "claude-code" :present? true :installed? true}
                {:agent "codex" :present? true :installed? true
                 :extra {:trusted-projects 2 :cwd-trusted? true}}
                {:agent "agy" :present? false :installed? false}]
        out    (doctor/render box agents)]
    (is (str/includes? out "cch doctor"))
    (is (str/includes? out "2 code hook(s) enabled"))
    (is (str/includes? out "claude-code"))
    (is (str/includes? out "codex"))
    (is (str/includes? out "agy"))))
