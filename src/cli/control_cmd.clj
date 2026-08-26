(ns cli.control-cmd
  "CLI entrypoint for the native control-plane proof of concept."
  (:require [cch.control.claude :as claude]
            [cch.control.core :as control]
            [cch.subprocess :as subprocess]
            [cheshire.core :as json]
            [cli.codex-app-server-service :as codex-service]
            [cli.settings :as settings]
            [clojure.string :as str]
            [clojure.tools.cli :as cli]))

(def ^:private send-options
  [[nil "--to ROUTE" "Destination route id"]
   [nil "--message TEXT" "Plain-text message"]
   [nil "--source ROUTE" "Source route id (otherwise inferred)"]
   [nil "--message-id ID" "Stable id for an idempotent retry"]])

(defn- command-available? [command]
  (try
    (zero? (:exit (subprocess/run [command "--version"])))
    (catch Exception _ false)))

(defn- configured? [command args]
  (try
    (zero? (:exit (subprocess/run (into [command] args))))
    (catch Exception _ false)))

(defn- install-mcp! [agent]
  (case agent
    :claude
    (when (command-available? "claude")
      (if (configured? "claude" ["mcp" "get" "cch"])
        :present
        (let [result (subprocess/run
                       ["claude" "mcp" "add" "--scope" "user"
                        "cch" "--" "cch" "control" "mcp"])]
          (if (zero? (:exit result)) :installed
              (throw (ex-info (str "Could not install Claude MCP config: "
                                   (str/trim (:err result)))
                              {:type :mcp-install-failed :agent "claude"}))))))

    :codex
    (when (command-available? "codex")
      (if (configured? "codex" ["mcp" "get" "cch"])
        :present
        (let [result (subprocess/run
                       ["codex" "mcp" "add" "cch" "--"
                        "cch" "control" "mcp"])]
          (if (zero? (:exit result)) :installed
              (throw (ex-info (str "Could not install Codex MCP config: "
                                   (str/trim (:err result)))
                              {:type :mcp-install-failed :agent "codex"}))))))))

(defn- install! []
  (let [path (settings/global-settings-path)]
    (settings/add-control-registration-entry! path)
    (println "Installed automatic Claude session registration in" path)
    (doseq [agent [:claude :codex]
            :let [status (install-mcp! agent)]]
      (println (format "%-7s MCP: %s" (name agent)
                       (case status
                         :installed "installed"
                         :present "already configured"
                         nil "CLI not found; skipped"))))
    (println)
    (if (command-available? "codex")
      (let [{:keys [status path]} (codex-service/install!)]
        (case status
          :installed
          (do
            (println "Codex app-server: installed and started local systemd user service")
            (println "  " path)
            (println "Codex Remote Control remains disabled for this POC."))

          :unsupported
          (do
            (println "Codex app-server: automatic supervision currently requires Linux/systemd.")
            (println "Start the local socket without Remote Control:")
            (println "  codex app-server --listen unix://"))))
      (println "Codex app-server: CLI not found; skipped"))
    (println "No per-session cch pairing or login is required after this install.")))

(defn- print-json [value]
  (println (json/generate-string value {:pretty true})))

(defn- register-claude! []
  (let [payload (json/parse-string (slurp *in*) true)]
    (claude/register-from-hook! payload)
    nil))

(defn- send! [args]
  (let [{:keys [options errors summary]} (cli/parse-opts args send-options)]
    (when (seq errors)
      (throw (ex-info (str (str/join "; " errors) "\n" summary)
                      {:type :invalid-cli-options})))
    (print-json
      (control/send-message! {:target (:to options)
                              :message (:message options)
                              :source (:source options)
                              :message-id (:message-id options)}))))

(defn print-usage []
  (println "cch control — native Claude/Codex routing POC")
  (println)
  (println "Usage: cch control <command>")
  (println)
  (println "Commands:")
  (println "  install          One-time automatic registration + MCP setup")
  (println "  sessions         List sanitized native session presence")
  (println "  get ROUTE        Get one session")
  (println "  send [options]   Send a native text message")
  (println "  mcp              Run the PluMCP stdio server")
  (println "  register-claude  Internal SessionStart hook"))

(defn run [& args]
  (let [[command & more] args]
    (case command
      "install" (install!)
      "sessions" (print-json (control/list-sessions))
      "get" (print-json {:session (control/get-session (first more))})
      "send" (send! more)
      "mcp" ((requiring-resolve 'cch.control.mcp/-main))
      "register-claude" (register-claude!)
      (print-usage))))
