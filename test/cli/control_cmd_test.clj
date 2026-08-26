(ns cli.control-cmd-test
  (:require [cch.subprocess :as subprocess]
            [cli.control-cmd :as control-cmd]
            [clojure.test :refer [deftest is testing]]))

(deftest install-mcp-reconciles-provider-environment
  (doseq [[agent expected-remove expected-add]
          [[:claude
            ["claude" "mcp" "remove" "cch" "--scope" "user"]
            ["claude" "mcp" "add" "--scope" "user" "cch"
             "--env" "CODEX_HOME=/home/example/.config/codex"
             "--" "cch" "control" "mcp"]]
           [:codex
            ["codex" "mcp" "remove" "cch"]
            ["codex" "mcp" "add" "--env"
             "CODEX_HOME=/home/example/.config/codex"
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
               "cch" "--" "cch" "control" "mcp"]]
             @calls)))))
