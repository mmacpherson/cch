(ns cli.control-cmd
  "CLI entrypoint for the native control-plane proof of concept."
  (:require [cch.control.claude :as claude]
            [cch.control.broker :as broker]
            [cch.control.broker-http :as broker-http]
            [cch.control.codex-binding :as codex-binding]
            [cch.control.core :as control]
            [cch.control.remote :as remote]
            [cch.control.runner :as runner]
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

(def ^:private broker-options
  [[nil "--host HOST" "Loopback listener for private HTTPS proxy"
    :default "127.0.0.1"]
   [nil "--port PORT" "Listener port" :default 8787 :parse-fn parse-long]])

(def ^:private runner-options
  [[nil "--poll-ms MS" "Polling interval" :default runner/default-poll-ms
    :parse-fn parse-long]])

(defn- command-available? [command]
  (try
    (zero? (:exit (subprocess/run [command "--version"])))
    (catch Exception _ false)))

(defn- configured? [command args]
  (try
    (zero? (:exit (subprocess/run (into [command] args))))
    (catch Exception _ false)))

(defn- broker-mcp-env []
  (when-let [{:keys [url runner-id token]} (remote/config-from-env)]
    ["--env" (str "CCH_CONTROL_BROKER_URL=" url)
     "--env" (str "CCH_CONTROL_RUNNER_ID=" runner-id)
     "--env" (str "CCH_CONTROL_RUNNER_TOKEN=" token)]))

(defn- mcp-commands [agent codex-home]
  (case agent
    :claude
    {:get ["claude" "mcp" "get" "cch"]
     :remove ["claude" "mcp" "remove" "cch" "--scope" "user"]
     :add (vec (concat ["claude" "mcp" "add" "--scope" "user" "cch"
                        "--env" (str "CODEX_HOME=" codex-home)
                        "--env" "CCH_MCP_CALLER=claude"]
                       (broker-mcp-env)
                       ["--" "cch" "control" "mcp"]))}

    :codex
    {:get ["codex" "mcp" "get" "cch"]
     :remove ["codex" "mcp" "remove" "cch"]
     :add (vec (concat ["codex" "mcp" "add"
                        "--env" (str "CODEX_HOME=" codex-home)
                        "--env" "CCH_MCP_CALLER=codex"]
                       (broker-mcp-env)
                       ["cch" "--" "cch" "control" "mcp"]))}))

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

          :started
          (println "Codex app-server: existing local systemd user service started")

          :unchanged
          (println "Codex app-server: active local systemd user service left untouched")

          :updated-restart-required
          (do
            (println "Codex app-server: unit updated; active service left untouched")
            (println "Restart it later, when no Codex clients are attached:")
            (println "  systemctl --user restart" codex-service/service-name))

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

(defn- parse-command-options [args option-spec]
  (let [{:keys [options errors summary]} (cli/parse-opts args option-spec)]
    (when (seq errors)
      (throw (ex-info (str (str/join "; " errors) "\n" summary)
                      {:type :invalid-cli-options})))
    options))

(defn- wait-until-shutdown! [stop!]
  (let [latch (java.util.concurrent.CountDownLatch. 1)
        hook (Thread. ^Runnable (fn []
                                  (try (stop!) (finally (.countDown latch)))))]
    (.addShutdownHook (Runtime/getRuntime) hook)
    (.await latch)))

(defn- run-broker! [args]
  (let [{:keys [host port]} (parse-command-options args broker-options)
        tokens (broker-http/runner-tokens-from-env)
        state (broker/new-broker tokens)
        server (broker-http/start! state {:host host :port port})]
    (println (format "Disposable control broker listening on http://%s:%d" host port))
    (println "Terminate TLS with the private overlay; no provider credentials are accepted.")
    (println (format "%d paired runner credential(s) loaded from the environment."
                     (count tokens)))
    (wait-until-shutdown! #((:stop server) :timeout 100))))

(defn- run-runner! [args]
  (let [{:keys [poll-ms]} (parse-command-options args runner-options)
        config (or (remote/config-from-env)
                   (throw (ex-info "Runner pairing environment is not configured"
                                   {:type :missing-runner-config})))
        process (runner/start! (assoc config :poll-ms poll-ms))]
    (println (format "Control runner '%s' polling its paired HTTPS broker every %d ms."
                     (:runner-id config) poll-ms))
    (println "Local native sessions continue to operate if this process or broker stops.")
    (wait-until-shutdown! (:stop process))))

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
  (println "  broker [options] Run the disposable in-memory POC broker")
  (println "  runner [options] Run one outbound paired POC runner")
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
      "broker" (run-broker! more)
      "runner" (run-runner! more)
      "mcp" ((requiring-resolve 'cch.control.mcp/-main))
      "register-claude" (register-claude!)
      "bind-codex-source" (bind-codex-source!)
      (print-usage))))
