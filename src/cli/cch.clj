(ns cli.cch
  "cch — Common Craft Hall CLI.

  The command surface is DATA: `commands` is an ordered table mapping each
  subcommand to its description, handler, and (optional) tools.cli option spec.
  One generic dispatcher interprets it — parsing options, generating per-command
  --help from the spec, reporting unknown-flag/parse errors uniformly, and
  routing to the handler. Adding a command or flag is a table edit; the top-level
  usage text is derived from the table, so it can't drift.

  Handler contract: each `:fn` is called as (handler options arguments), where
  options is the parsed option map and arguments the leftover positionals.
  A `:raw true` command bypasses parsing and receives the raw arg strings
  (used by `serve`, which delegates to the server's own arg handling)."
  (:require [cch.attention :as attention]
            [cch.doctor :as doctor]
            [cch.server :as server]
            [cli.control-cmd :as control-cmd]
            [cli.broker-service :as broker-service]
            [cli.init :as init]
            [cli.install :as install]
            [cli.list-cmd :as list-cmd]
            [cli.log-cmd :as log-cmd]
            [cli.service-cmd :as service-cmd]
            [clojure.tools.cli :as cli])
  (:gen-class))

(defn- attention-cmd
  "Handler for `cch attention`: render the blocked-on-you report."
  [options _arguments]
  (println (attention/report options)))

(def ^:private commands
  "Ordered subcommand table. Each entry: [name {:desc :fn :opts? :raw?}].
  :opts is a tools.cli option spec (a vector of [short long desc & kvs])."
  [["init"      {:desc "Set up cch in the current project"
                 :fn   #'init/run}]
   ["install"   {:desc "Bootstrap cch"
                 :fn   #'install/run
                 :opts [[nil "--all"    "Detect and provision every agent on this box"]
                        [nil "--global" "Write to the global Claude settings.json"]
                        [nil "--codex"  "Write Codex entries to $CODEX_HOME/config.toml"]
                        [nil "--agy"    "Configure the AGY statusLine feed for quota capture"]]}]
   ["uninstall" {:desc "Remove cch-owned entries"
                 :fn   #'install/run-uninstall
                 :opts [[nil "--global" "Remove from the global Claude settings.json"]
                        [nil "--codex"  "Remove the cch block from $CODEX_HOME/config.toml"]
                        [nil "--agy"    "Restore the previous AGY status line"]]}]
   ["list"      {:desc "Show available and installed hooks"
                 :fn   #'list-cmd/run}]
   ["log"       {:desc "Query event history"
                 :fn   #'log-cmd/run
                 :opts [[nil "--limit N"      "Max rows to show" :parse-fn parse-long]
                        [nil "--hook HOOK"    "Filter by hook name"]
                        [nil "--event EVENT"  "Filter by event type"]
                        [nil "--session ID"   "Filter by session id"]
                        [nil "--decision DEC" "Filter by decision (allow/ask/deny)"]
                        [nil "--since TS"     "Only events at or after TS"]]}]
   ["attention" {:desc "Time agents spent blocked on you"
                 :fn   #'attention-cmd
                 :opts [[nil "--days N"  "Look back N days (default: all history)" :parse-fn parse-long]
                        [nil "--limit N" "Max rows" :parse-fn parse-long]]}]
   ["doctor"    {:desc "Report per-agent federation wiring on this box"
                 :fn   #'doctor/run
                 :opts [[nil "--cwd DIR" "Scope the codex trust check to DIR"]]}]
   ["control"   {:desc "Native multi-agent session routing"
                 :fn   #'control-cmd/run
                 :raw  true}]
   ["serve"     {:desc "Run the HTTP dispatcher + web dashboard"
                 :fn   #'server/-main
                 :raw  true}]
   ["install-service"   {:desc "Install OS-native auto-start for `cch serve`"
                         :fn   #'service-cmd/run}]
   ["uninstall-service" {:desc "Remove the auto-start unit/plist"
                         :fn   #'service-cmd/run-uninstall}]
   ["install-broker-service"   {:desc "Install a systemd user unit for `cch control broker`"
                                :fn   #'broker-service/run
                                :opts [[nil "--host HOST" "Loopback listener host"]
                                       [nil "--port PORT" "Listener port" :parse-fn parse-long]]}]
   ["uninstall-broker-service" {:desc "Remove the broker systemd user unit"
                                :fn   #'broker-service/run-uninstall}]])

(defn- lookup [cmd]
  (some (fn [[n spec]] (when (= n cmd) spec)) commands))

(def ^:private help-opt
  [nil "--help" "Show this command's options"])

(defn print-usage []
  (println "cch — Common Craft Hall")
  (println)
  (println "Usage: cch <command> [args]")
  (println)
  (println "Commands:")
  (doseq [[name {:keys [desc]}] commands]
    (println (format "  %-24s %s" name desc)))
  (println)
  (println "Run 'cch <command> --help' for details."))

(defn -main [& args]
  (let [[cmd & rest-args] args
        spec (lookup cmd)]
    (cond
      (nil? spec)
      (print-usage)

      (:raw spec)
      (apply (:fn spec) rest-args)

      :else
      (let [opt-spec (conj (vec (:opts spec)) help-opt)
            {:keys [options arguments errors summary]} (cli/parse-opts rest-args opt-spec)]
        (cond
          (:help options)
          (do (println (format "cch %s — %s" cmd (:desc spec)))
              (println)
              (println "Options:")
              (println summary))

          (seq errors)
          (do (run! println errors)
              (println)
              (println summary)
              (System/exit 2))

          :else
          ((:fn spec) options arguments))))))
