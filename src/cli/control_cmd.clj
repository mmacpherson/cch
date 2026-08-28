(ns cli.control-cmd
  "CLI entrypoint for native multi-agent session routing."
  (:require [cch.control.claude :as claude]
            [cch.control.broker :as broker]
            [cch.control.broker-api :as broker-api]
            [cch.control.broker-http :as broker-http]
            [cch.control.broker-postgres :as broker-postgres]
            [cch.control.codex-binding :as codex-binding]
            [cch.control.codex :as codex]
            [cch.control.core :as control]
            [cch.control.doctor :as control-doctor]
            [cch.control.remote :as remote]
            [cch.control.runner :as runner]
            [cch.control.web-auth :as web-auth]
            [cch.subprocess :as subprocess]
            [cheshire.core :as json]
            [cli.codex-app-server-service :as codex-service]
            [cli.codex-settings :as codex-settings]
            [cli.control-runner-service :as runner-service]
            [cli.settings :as settings]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.cli :as cli])
  (:import [java.math BigInteger]
           [java.net JarURLConnection]
           [java.nio.charset StandardCharsets]
           [java.nio.file Files]
           [java.security MessageDigest]))

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

(defn- deployment-files []
  (let [resource (io/resource "cch/control/mcp.clj")]
    (case (.getProtocol resource)
      "jar"
      [(-> ^JarURLConnection (.openConnection resource)
           .getJarFileURL
           .toURI
           io/file)]

      "file"
      (let [source-file (io/file (.toURI resource))
            source-root (nth (iterate #(.getParentFile %) source-file) 3)
            repo-root (.getParentFile source-root)
            roots [source-root (io/file repo-root "resources")]
            files (concat [(io/file repo-root "deps.edn")]
                          (mapcat file-seq roots))]
        (->> files (filter #(.isFile %)) (sort-by #(.getPath %))))

      (throw (ex-info "Cannot identify the deployed cch code artifact"
                      {:type :cch-revision-unavailable
                       :protocol (.getProtocol resource)})))))

(defn- deployment-revision []
  (let [digest (MessageDigest/getInstance "SHA-256")]
    (doseq [file (deployment-files)]
      ;; Separators prevent two differently partitioned files from producing
      ;; the same concatenated byte stream. Only the digest enters provider
      ;; config; local paths and source contents never do.
      (.update digest (.getBytes (.getName file) StandardCharsets/UTF_8))
      (.update digest (byte-array [(byte 0)]))
      (.update digest (Files/readAllBytes (.toPath file)))
      (.update digest (byte-array [(byte 0)])))
    (format "%064x" (BigInteger. 1 (.digest digest)))))

(defn- mcp-commands [agent codex-home cch-bin pairing-path revision]
  (case agent
    :claude
    {:get ["claude" "mcp" "get" "cch"]
     :remove ["claude" "mcp" "remove" "cch" "--scope" "user"]
     :add (vec (concat ["claude" "mcp" "add" "--scope" "user" "cch"
                        "--env" (str "CODEX_HOME=" codex-home)
                        "--env" "CCH_MCP_CALLER=claude"
                        "--env" (str "CCH_CONTROL_PAIRING_PATH=" pairing-path)
                        "--env" (str "CCH_MCP_REVISION=" revision)]
                       ["--" cch-bin "control" "mcp"]))}

    :codex
    {:get ["codex" "mcp" "get" "cch"]
     :remove ["codex" "mcp" "remove" "cch"]}))

(def ^:private codex-binding-block "cch-control-plane")

(defn- shell-quote [value]
  (str "'" (str/replace value "'" "'\"'\"'") "'"))

(defn- install-codex-binding-hook! [codex-home cch-bin]
  (codex-settings/install-hook!
    (str codex-home "/config.toml")
    codex-binding-block
    [{:event "PreToolUse"
      :matcher codex-binding/tool-name
      :command (str (shell-quote cch-bin) " control bind-codex-source")
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

(defn- install-mcp! [agent codex-home cch-bin pairing-path revision]
  (let [command (name agent)]
    (when (command-available? command)
      (let [{:keys [get remove add]}
            (mcp-commands agent codex-home cch-bin pairing-path revision)
            present? (configured? command (rest get))]
        ;; Provider CLIs may sanitize inherited environment variables before
        ;; spawning MCP servers. Reconcile the cch-owned entry so both agents
        ;; can always resolve the shared Codex socket, including relocated
        ;; CODEX_HOME installations.
        (when present?
          (run-mcp-command! agent :remove remove))
        (if (= :codex agent)
          (do
            ;; `codex mcp add` does not expose the upstream allowlist and
            ;; approval settings. Own the complete cch table so no broad
            ;; provider-level approval policy is needed.
            (codex-settings/install-control-mcp!
              (str codex-home "/config.toml")
              {:command cch-bin
               :args ["control" "mcp"]
               :env {"CODEX_HOME" codex-home
                     "CCH_MCP_CALLER" "codex"
                     "CCH_CONTROL_PAIRING_PATH" pairing-path
                     "CCH_MCP_REVISION" revision}})
            (run-mcp-command! agent :validate get))
          (run-mcp-command! agent :install add))
        (if present? :updated :installed)))))

(defn- print-runner-service-status! [{:keys [status path]}]
  (case status
    :installed (println "Control runner: installed and started user service at" path)
    :started (println "Control runner: existing user service started")
    :unchanged (println "Control runner: active user service left untouched")
    :updated-restart-required
    (println "Control runner: service definition updated; active runner left untouched")
    :unsupported
    (println "Control runner: automatic supervision requires systemd or launchd.")))

(defn- install-runner-if-paired! []
  (if (remote/config)
    (print-runner-service-status! (runner-service/install!))
    (println "Control runner: not paired; local native routing remains available")))

(defn- install! []
  (let [path (settings/global-settings-path)
        codex-home (codex-settings/codex-home)
        cch-bin (or (codex-service/resolve-executable
                      "cch" (System/getenv "PATH"))
                    (throw (ex-info "Cannot install native control hooks: cch is not on PATH"
                                    {:type :cch-cli-unavailable})))
        pairing-path (remote/pairing-path)
        revision (deployment-revision)
        env-pairing (remote/config-from-env)]
    (when env-pairing
      (remote/save-config! env-pairing))
    (settings/add-control-registration-entry!
      path (str (shell-quote cch-bin) " control register-claude"))
    (settings/add-control-mcp-permissions! path)
    (println "Installed automatic Claude session registration in" path)
    (println "Pre-authorized only cch's three native routing tools for Claude")
    (doseq [agent [:claude :codex]
            :let [status (install-mcp! agent codex-home cch-bin pairing-path
                                       revision)]]
      (println (format "%-7s MCP: %s" (name agent)
                       (case status
                         :installed "installed"
                         :updated "configuration reconciled"
                         nil "CLI not found; skipped"))))
    (when (command-available? "codex")
      (install-codex-binding-hook! codex-home cch-bin)
      (println "Codex caller binding hook: installed"))
    (println)
    (if (command-available? "codex")
      (let [{:keys [status path]} (codex-service/install!)]
        (case status
          :installed
          (do
            (println "Codex app-server: installed and started local user service")
            (println "  " path)
            (println "cch does not change Codex Remote Control configuration."))

          :started
          (println "Codex app-server: existing local user service started")

          :unchanged
          (println "Codex app-server: active local user service left untouched")

          :updated-restart-required
          (do
            (println "Codex app-server: service definition updated; active service left untouched")
            (println "Restart it later with the OS user-service manager, when no Codex clients are attached."))

          :unsupported
          (do
            (println "Codex app-server: automatic supervision requires systemd or launchd.")
            (println "Start the local socket without Remote Control:")
            (println "  codex app-server --listen unix://"))))
      (println "Codex app-server: CLI not found; skipped"))
    (install-runner-if-paired!)
    (println "No per-session cch pairing or login is required after this install.")))

(defn- pair! []
  (let [config (or (remote/config-from-env)
                   (throw
                     (ex-info
                       (str "Set CCH_CONTROL_BROKER_URL, CCH_CONTROL_RUNNER_ID, "
                            "and CCH_CONTROL_RUNNER_TOKEN for this one-time pairing")
                       {:type :missing-runner-config})))
        path (remote/save-config! config)]
    (println "Saved owner-only runner pairing at" path)
    (println "Paired runner id:" (:runner-id config))
    (print-runner-service-status! (runner-service/install!))
    (println "Future agent sessions require no cch pairing step.")))

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

(defn- refresh-mcp! [args]
  (case (first args)
    "codex"
    (let [{:keys [tools]} (codex/refresh-mcp!)]
      (println "Codex cch MCP: refreshed and verified through the shared app-server")
      (println "Verified tools:" (str/join ", " tools))
      (println "No Codex agent or app-server process was restarted."))

    "claude"
    (do
      (println "Claude MCP refresh is local to each active session.")
      (println "Run this inside the Claude Code session that needs the new cch version:")
      (println "  /mcp reconnect cch")
      (println "This preserves the session and restarts only its cch MCP subprocess."))

    (throw (ex-info
             "Usage: cch control refresh-mcp <claude|codex>"
             {:type :invalid-cli-options}))))

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
        database (broker-postgres/database-config-from-env)
        web-config (web-auth/config-from-env)
        state (if database
                (broker-postgres/new-broker tokens database)
                (broker/new-broker tokens))
        server (broker-http/start! state {:host host :port port
                                          :web-config web-config})]
    (println (format "Control broker listening on http://%s:%d" host port))
    (println (if database
               "Postgres route directory and message metadata enabled."
               "Disposable in-memory route directory enabled."))
    (println "Terminate TLS with the private overlay; no provider credentials are accepted.")
    (println (if web-config
               "Google-protected human switchboard enabled."
               "Human switchboard disabled; configure its Google OIDC environment to enable it."))
    (println (format "%d paired runner credential(s) loaded from the environment."
                     (count tokens)))
    (wait-until-shutdown!
      #(do ((:stop server) :timeout 100)
           (broker-api/close-broker! state)))))

(defn- run-runner! [args]
  (let [{:keys [poll-ms]} (parse-command-options args runner-options)
        config (or (remote/config)
                   (throw (ex-info "Runner pairing environment is not configured"
                                   {:type :missing-runner-config})))
        process (runner/start! (assoc config :poll-ms poll-ms))]
    (println (format "Control runner '%s' polling its paired HTTPS broker every %d ms."
                     (:runner-id config) poll-ms))
    (println "Local native sessions continue to operate if this process or broker stops.")
    (wait-until-shutdown! (:stop process))))

(defn print-usage []
  (println "cch control — native multi-agent session routing")
  (println)
  (println "Usage: cch control <command>")
  (println)
  (println "Commands:")
  (println "  install          One-time automatic registration + MCP setup")
  (println "  pair             Persist one-time runner environment + install service")
  (println "  doctor           Check pairing, supervision, and native capabilities")
  (println "  sessions         List sanitized native session presence")
  (println "  get ROUTE        Get one session")
  (println "  send [options]   Send a native text message")
  (println "  refresh-mcp AGENT Refresh cch tools in active Claude or Codex sessions")
  (println "  broker [options] Run the private broker (Postgres when configured)")
  (println "  runner [options] Run one outbound paired runner")
  (println "  mcp              Run the PluMCP stdio server")
  (println "  register-claude  Internal SessionStart hook")
  (println "  bind-codex-source Internal PreToolUse hook"))

(defn run [& args]
  (let [[command & more] args]
    (case command
      "install" (install!)
      "pair" (pair!)
      "doctor" (print-json (control-doctor/report))
      "sessions" (print-json (control/list-sessions))
      "get" (print-json {:session (control/get-session (first more))})
      "send" (send! more)
      "refresh-mcp" (refresh-mcp! more)
      "broker" (run-broker! more)
      "runner" (run-runner! more)
      "mcp" ((requiring-resolve 'cch.control.mcp/-main))
      "register-claude" (register-claude!)
      "bind-codex-source" (bind-codex-source!)
      (print-usage))))
