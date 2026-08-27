(ns cli.control-cmd-test
  (:require [cch.subprocess :as subprocess]
            [cli.codex-settings :as codex-settings]
            [cli.control-cmd :as control-cmd]
            [clojure.test :refer [deftest is testing]]))

(deftest install-mcp-reconciles-provider-environment
  (doseq [[agent expected-remove expected-add]
          [[:claude
            ["claude" "mcp" "remove" "cch" "--scope" "user"]
            ["claude" "mcp" "add" "--scope" "user" "cch"
             "--env" "CODEX_HOME=/home/example/.config/codex"
             "--env" "CCH_MCP_CALLER=claude"
             "--env" "CCH_CONTROL_PAIRING_PATH=/home/example/.config/cch/control-runner.json"
             "--" "/opt/cch/bin/cch" "control" "mcp"]]
           [:codex
            ["codex" "mcp" "remove" "cch"]
            nil]]]
    (testing (name agent)
      (let [calls (atom [])
            installed (atom nil)]
        (with-redefs [subprocess/run
                      (fn [argv]
                        (swap! calls conj argv)
                        {:exit 0 :out "" :err ""})
                      codex-settings/install-control-mcp!
                      (fn [path config]
                        (reset! installed [path config]))]
          (is (= :updated
                 (#'control-cmd/install-mcp!
                   agent "/home/example/.config/codex" "/opt/cch/bin/cch"
                   "/home/example/.config/cch/control-runner.json")))
          (is (= (cond-> [[(name agent) "--version"]
                          [(name agent) "mcp" "get" "cch"]
                          expected-remove]
                   expected-add (conj expected-add)
                   (= :codex agent) (conj ["codex" "mcp" "get" "cch"]))
                 @calls))
          (if (= :codex agent)
            (is (= ["/home/example/.config/codex/config.toml"
                    {:command "/opt/cch/bin/cch"
                     :args ["control" "mcp"]
                     :env {"CODEX_HOME" "/home/example/.config/codex"
                           "CCH_MCP_CALLER" "codex"
                           "CCH_CONTROL_PAIRING_PATH"
                           "/home/example/.config/cch/control-runner.json"}}]
                   @installed))
            (is (nil? @installed))))))))

(deftest install-mcp-adds-when-not-configured
  (let [calls (atom [])
        installed (atom nil)
        get-count (atom 0)]
    (with-redefs [subprocess/run
                  (fn [argv]
                    (swap! calls conj argv)
                    {:exit (if (and (= ["codex" "mcp" "get" "cch"] argv)
                                    (= 1 (swap! get-count inc)))
                             1
                             0)
                     :out "" :err "not found"})
                  codex-settings/install-control-mcp!
                  (fn [path config]
                    (reset! installed [path config]))]
      (is (= :installed
             (#'control-cmd/install-mcp!
               :codex "/home/example/.codex" "/opt/cch/bin/cch"
               "/home/example/.config/cch/control-runner.json")))
      (is (= [["codex" "--version"]
              ["codex" "mcp" "get" "cch"]
              ["codex" "mcp" "get" "cch"]]
             @calls))
      (is (= "/home/example/.codex/config.toml" (first @installed))))))

(deftest provider-mcp-uses-absolute-cch-and-does-not-copy-runner-token
  (let [add (get (#'control-cmd/mcp-commands
                   :claude "/home/example/.config/codex" "/opt/cch/bin/cch"
                   "/home/example/.config/cch/control-runner.json") :add)]
    (is (= ["--" "/opt/cch/bin/cch" "control" "mcp"]
           (subvec add 12)))
    (is (not-any? #(re-find #"TOKEN|synthetic-token" %) add))))
