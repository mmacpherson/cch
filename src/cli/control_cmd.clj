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
            [cch.control.mcp :as mcp]
            [cch.control.mcp-http :as mcp-http]
            [cch.control.mcp-tokens :as mcp-tokens]
            [cch.control.remote :as remote]
            [cch.control.runner :as runner]
            [cch.control.web-auth :as web-auth]
            [cch.doctor :as doctor]
            [cch.subprocess :as subprocess]
            [babashka.fs :as fs]
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
   [nil "--message-id ID" "Stable id for an idempotent retry"]
   [nil "--in-reply-to ID" "Id of the cch message this one answers"]])

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

(defn claude-http-add-args
  "Argv to register cch as an HTTP MCP server for Claude Code, carrying the
  bearer token inline as an Authorization header. The shared-endpoint
  equivalent of the stdio `--` command form."
  [url token]
  ["claude" "mcp" "add" "--scope" "user" "--transport" "http" "cch" url
   "--header" (str "Authorization: Bearer " token)])

(def ^:private claude-mcp-remove-command
  ["claude" "mcp" "remove" "cch" "--scope" "user"])

(defn- mcp-commands [agent codex-home cch-bin pairing-path revision]
  (case agent
    :claude
    {:get ["claude" "mcp" "get" "cch"]
     :remove claude-mcp-remove-command
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
      :matcher codex-binding/tool-matcher
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

(defn- install-mcp! [agent codex-home cch-bin pairing-path revision tokens]
  (let [command (name agent)]
    (when (command-available? command)
      (let [{:keys [get remove]}
            (mcp-commands agent codex-home cch-bin pairing-path revision)
            present? (configured? command (rest get))
            url (mcp-http/endpoint-url)]
        ;; Reconcile the cch-owned entry: agents now share one HTTP MCP endpoint
        ;; on `cch serve` instead of each spawning a stdio JVM. The stdio path
        ;; (cch control mcp) remains as a documented fallback.
        (when present?
          (run-mcp-command! agent :remove remove))
        (if (= :codex agent)
          (do
            ;; `codex mcp add` does not expose the upstream allowlist and
            ;; approval settings. Own the complete cch table so no broad
            ;; provider-level approval policy is needed. Codex reads the bearer
            ;; token from CCH_MCP_TOKEN in the shared app-server's environment.
            (codex-settings/install-control-mcp-http!
              (str codex-home "/config.toml")
              {:url url})
            (run-mcp-command! agent :validate get))
          (run-mcp-command! agent :install
                            (claude-http-add-args
                              url (mcp-tokens/token-for tokens "claude"))))
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
    (println "Pre-authorized only cch's four native control tools for Claude")
    ;; Mint (or reuse) the per-agent bearer tokens once; both agents point at
    ;; the shared HTTP MCP endpoint on `cch serve`.
    (let [tokens (mcp-tokens/ensure-tokens!)]
      (doseq [agent [:claude :codex]
              :let [status (install-mcp! agent codex-home cch-bin pairing-path
                                         revision tokens)]]
        (println (format "%-7s MCP: %s" (name agent)
                         (case status
                           :installed "installed"
                           :updated "configuration reconciled"
                           nil "CLI not found; skipped")))))
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

(defn- owned-claude-mcp? [entry]
  (let [command (:command entry)
        url (:url entry)
        uri (when (string? url) (try (java.net.URI. url) (catch Exception _ nil)))]
    (or (some? (:__cch entry))
        (and (string? command)
             (re-find #"(?i)(?:^|[/\\])cch(?:\.exe)?$" command)
             (= ["control" "mcp"] (vec (:args entry))))
        (and uri (= "/mcp" (.getPath uri))
             (or (= url (mcp-http/endpoint-url))
                 (and (#{"http" "https"} (.getScheme uri))
                      (#{"127.0.0.1" "localhost" "::1" "[::1]"} (.getHost uri))))))))

(defn- strip-control-settings [data]
  (let [data (if (contains? (:hooks data) :SessionStart)
               (update-in data [:hooks :SessionStart]
                          #(vec (keep (fn [entry]
                                        (let [kept (vec (remove (fn [h] (= "control-plane:claude-registration" (:__cch h)))
                                                               (:hooks entry)))]
                                          (when (seq kept) (assoc entry :hooks kept)))) %)))
               data)]
    (if (contains? (:permissions data) :allow)
      (update-in data [:permissions :allow] #(vec (remove (set settings/control-mcp-permissions) %)))
      data)))

(defn- write-control-json! [path data]
  (codex-settings/write-config! path (json/generate-string data {:pretty true})))

(defn- remove-claude-mcp! []
  (when (command-available? "claude")
    (let [result (subprocess/run claude-mcp-remove-command)]
      (when-not (zero? (:exit result))
        (throw (ex-info "Could not remove Claude MCP configuration through its CLI"
                        {:type :mcp-uninstall-failed})))
      true)))

(defn- read-control-json [path]
  (try
    (let [data (settings/read-settings path)]
      (when-not (map? data)
        (throw (ex-info "Invalid control config" {})))
      data)
    (catch Exception _
      (throw (ex-info "Cannot parse control JSON configuration; files left unchanged"
                      {:type :invalid-control-config})))))

(defn uninstall-control!
  "Remove only control-owned MCP entries, registration and exact tool grants.
  Paths are injectable; services, dispatcher hooks and unrelated config remain."
  ([] (uninstall-control! {}))
  ([{:keys [codex-path claude-path settings-path env-path]}]
   (let [claude-path (or claude-path (doctor/claude-mcp-config-path))
         data (read-control-json claude-path)
         settings-path (or settings-path (settings/global-settings-path))
         settings-data (read-control-json settings-path)
         entry (get-in data [:mcpServers :cch])
         owned? (boolean (and entry (owned-claude-mcp? entry)))
         claude-removed? (boolean (and owned? (remove-claude-mcp!)))
         codex-result (codex-settings/remove-control-config! (or codex-path (codex-settings/codex-config-path)))
         cleaned (strip-control-settings settings-data)
         _ (when-not (= settings-data cleaned) (write-control-json! settings-path cleaned))
         env-path (or env-path (codex-service/env-file-path (System/getProperty "user.home")))
         env-existed? (fs/exists? env-path)]
     (codex-service/remove-env-file! env-path)
     {:codex codex-result
      :claude (cond-> {:mcp-removed? claude-removed? :preserved-mcp? (boolean (and entry (not claude-removed?)))}
                (and owned? (not claude-removed?)) (assoc :reason :cli-unavailable))
      :settings-updated? (not= settings-data cleaned)
      :environment-file-removed? env-existed?})))

(defn- uninstall! []
  (print-json (uninstall-control!))
  (println "Existing sessions keep their current connections; reconnect MCP sessions to apply removal."))

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
                              :message-id (:message-id options)
                              :in-reply-to (:in-reply-to options)}))))

(defn- refresh-mcp! [args]
  (case (first args)
    "codex"
    (let [{:keys [tools]} (codex/refresh-mcp! (mcp/tool-signatures))]
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
        server (broker-http/start! state {:host host :port port})
        web-server (try
                     (when web-config
                       (broker-http/start-web! state web-config))
                     (catch Exception error
                       ((:stop server) :timeout 100)
                       (broker-api/close-broker! state)
                       (throw error)))]
    (println (format "Control broker listening on http://%s:%d" host port))
    (println (if database
               "Postgres route directory and message metadata enabled."
               "Disposable in-memory route directory enabled."))
    (println "Terminate TLS with the private overlay; no provider credentials are accepted.")
    (if web-server
      (println (format
                 "Cloudflare Access switchboard listening separately on http://%s:%d."
                 (:host web-server) (:port web-server)))
      (println "Human switchboard disabled; configure its Cloudflare Access environment to enable it."))
    (println (format "%d paired runner credential(s) loaded from the environment."
                     (count tokens)))
    (wait-until-shutdown!
      #(do (when web-server ((:stop web-server) :timeout 100))
           ((:stop server) :timeout 100)
           (broker-api/close-broker! state)))))

(defn- exit-for-supervisor-restart!
  "Both service definitions restart a runner that exits non-zero (launchd
  KeepAlive, systemd Restart=on-failure). A fresh process discards whatever
  wedged state a long-lived client accumulated. Halt if shutdown hooks hang."
  [{:keys [reason]}]
  (runner/log! "exiting for a supervisor restart (" (name reason) ")")
  (doto (Thread. ^Runnable #(do (Thread/sleep 10000)
                                (.halt (Runtime/getRuntime) 1)))
    (.setDaemon true)
    (.start))
  (System/exit 1))

(defn- run-runner! [args]
  (let [{:keys [poll-ms]} (parse-command-options args runner-options)
        config (or (remote/config)
                   (throw (ex-info "Runner pairing environment is not configured"
                                   {:type :missing-runner-config})))
        process (runner/start! (assoc config :poll-ms poll-ms)
                               (assoc (runner/default-dependencies config)
                                      :on-unhealthy! exit-for-supervisor-restart!))]
    (runner/log! (format "runner '%s' polling its paired HTTPS broker every %d ms."
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
  (println "  uninstall        Remove control MCP config and token environment file")
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

(def ^:private long-running-commands
  "Services and hooks whose failures belong in supervisor logs with traces."
  #{"broker" "runner" "mcp" "register-claude" "bind-codex-source"})

(defn- exit! [code] (System/exit code))

(defn- report-cli-error!
  "Print a typed cch error as one line on stderr and exit non-zero. Anything
  untyped is a bug and keeps its stack trace."
  [^clojure.lang.ExceptionInfo error]
  (if (:type (ex-data error))
    (do (binding [*out* *err*]
          (println (str "Error: " (ex-message error))))
        (exit! 1))
    (throw error)))

(declare dispatch)

(defn run [& args]
  (if (contains? long-running-commands (first args))
    (apply dispatch args)
    (try
      (apply dispatch args)
      (catch clojure.lang.ExceptionInfo error
        (report-cli-error! error)))))

(defn- dispatch [& args]
  (let [[command & more] args]
    (case command
      "install" (install!)
      "uninstall" (cond
                    (empty? more) (uninstall!)
                    (contains? #{["--help"] ["-h"]} (vec more))
                    (println "Usage: cch control uninstall")
                    :else (throw (ex-info "Usage: cch control uninstall"
                                          {:type :invalid-cli-options})))
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
