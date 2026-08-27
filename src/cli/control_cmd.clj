(ns cli.control-cmd
  "CLI entrypoint for the native control-plane proof of concept."
  (:require [cch.control.claude :as claude]
            [cch.control.codex-binding :as codex-binding]
            [cch.control.core :as control]
            [cch.subprocess :as subprocess]
            [cheshire.core :as json]
            [cli.codex-app-server-service :as codex-service]
            [cli.codex-settings :as codex-settings]
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

(defn- mcp-commands [agent codex-home]
  (case agent
    :claude
    {:get ["claude" "mcp" "get" "cch"]
     :remove ["claude" "mcp" "remove" "cch" "--scope" "user"]
     :add ["claude" "mcp" "add" "--scope" "user" "cch"
           "--env" (str "CODEX_HOME=" codex-home)
           "--env" "CCH_MCP_CALLER=claude"
           "--" "cch" "control" "mcp"]}

    :codex
    {:get ["codex" "mcp" "get" "cch"]
     :remove ["codex" "mcp" "remove" "cch"]
     :add ["codex" "mcp" "add" "--env" (str "CODEX_HOME=" codex-home)
           "--env" "CCH_MCP_CALLER=codex"
           "cch" "--" "cch" "control" "mcp"]}))

(def ^:private codex-binding-block "cch-control-plane")

(defn- install-codex-binding-hook! [codex-home]
  (codex-settings/install-hook!
    (str codex-home "/config.toml")
    codex-binding-block
    [{:event "PreToolUse"
      :matcher codex-binding/tool-name
      :command "cch control bind-codex-source"
      :timeout 30}]))

(defn- run-mcp-command! [agent action argv]
  (let [result (subprocess/run argv)]
    (when-not (zero? (:exit result))
      (throw (ex-info (str "Could not " (name action) " " (name agent)
                           " MCP config: " (str/trim (:err result)))
                      {:type :mcp-install-failed
                       :agent (name agent)
                       :action action})))
    result))

(defn- install-mcp! [agent codex-home]
  (let [command (name agent)]
    (when (command-available? command)
      (let [{:keys [get remove add]} (mcp-commands agent codex-home)
            present? (configured? command (rest get))]
        ;; Provider CLIs may sanitize inherited environment variables before
        ;; spawning MCP servers. Reconcile the cch-owned entry so both agents
        ;; can always resolve the shared Codex socket, including relocated
        ;; CODEX_HOME installations.
        (when present?
          (run-mcp-command! agent :remove remove))
        (run-mcp-command! agent :install add)
        (if present? :updated :installed)))))

(defn- install! []
  (let [path (settings/global-settings-path)
        codex-home (codex-settings/codex-home)]
    (settings/add-control-registration-entry! path)
    (println "Installed automatic Claude session registration in" path)
    (doseq [agent [:claude :codex]
            :let [status (install-mcp! agent codex-home)]]
      (println (format "%-7s MCP: %s" (name agent)
                       (case status
                         :installed "installed"
                         :updated "configuration reconciled"
                         nil "CLI not found; skipped"))))
    (when (command-available? "codex")
      (install-codex-binding-hook! codex-home)
      (println "Codex caller binding hook: installed"))
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

(defn- bind-codex-source! []
  (-> (json/parse-string (slurp *in*) true)
      codex-binding/bind!
      print-json))

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
  (println "  register-claude  Internal SessionStart hook")
  (println "  bind-codex-source Internal PreToolUse hook"))

(defn run [& args]
  (let [[command & more] args]
    (case command
      "install" (install!)
      "sessions" (print-json (control/list-sessions))
      "get" (print-json {:session (control/get-session (first more))})
      "send" (send! more)
      "mcp" ((requiring-resolve 'cch.control.mcp/-main))
      "register-claude" (register-claude!)
      "bind-codex-source" (bind-codex-source!)
      (print-usage))))
