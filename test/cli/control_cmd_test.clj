(ns cli.control-cmd-test
  (:require [cch.subprocess :as subprocess]
            [cch.control.remote :as remote]
            [cli.control-cmd :as control-cmd]
            [clojure.test :refer [deftest is testing]]))

(deftest install-mcp-reconciles-provider-environment
  (doseq [[agent expected-remove expected-add]
          [[:claude
            ["claude" "mcp" "remove" "cch" "--scope" "user"]
            ["claude" "mcp" "add" "--scope" "user" "cch"
             "--env" "CODEX_HOME=/home/example/.config/codex"
             "--env" "CCH_MCP_CALLER=claude"
             "--" "cch" "control" "mcp"]]
           [:codex
            ["codex" "mcp" "remove" "cch"]
            ["codex" "mcp" "add" "--env"
             "CODEX_HOME=/home/example/.config/codex"
             "--env" "CCH_MCP_CALLER=codex"
             "cch" "--" "cch" "control" "mcp"]]]]
    (testing (name agent)
      (let [calls (atom [])]
        (with-redefs [subprocess/run
                      (fn [argv]
                        (swap! calls conj argv)
                        {:exit 0 :out "" :err ""})]
          (is (= :updated
                 (#'control-cmd/install-mcp!
                   agent "/home/example/.config/codex")))
          (is (= [[(name agent) "--version"]
                  [(name agent) "mcp" "get" "cch"]
                  expected-remove
                  expected-add]
                 @calls)))))))

(deftest install-mcp-adds-when-not-configured
  (let [calls (atom [])]
    (with-redefs [subprocess/run
                  (fn [argv]
                    (swap! calls conj argv)
                    {:exit (if (= ["codex" "mcp" "get" "cch"] argv) 1 0)
                     :out "" :err "not found"})]
      (is (= :installed
             (#'control-cmd/install-mcp! :codex "/home/example/.codex")))
      (is (= [["codex" "--version"]
              ["codex" "mcp" "get" "cch"]
              ["codex" "mcp" "add" "--env"
               "CODEX_HOME=/home/example/.codex"
               "--env" "CCH_MCP_CALLER=codex"
               "cch" "--" "cch" "control" "mcp"]]
             @calls)))))

(deftest paired-runner-config-is-captured-once-by-agent-mcp-registration
  (with-redefs [remote/config-from-env
                (constantly {:url "https://broker.invalid"
                             :runner-id "runner-a"
                             :token "synthetic-token"})]
    (let [add (get (#'control-cmd/mcp-commands
                     :codex "/home/example/.config/codex") :add)]
      (is (= ["--env" "CCH_CONTROL_BROKER_URL=https://broker.invalid"
              "--env" "CCH_CONTROL_RUNNER_ID=runner-a"
              "--env" "CCH_CONTROL_RUNNER_TOKEN=synthetic-token"]
             (subvec add 7 13)))
      (is (= ["cch" "--" "cch" "control" "mcp"]
             (subvec add 13))))))
