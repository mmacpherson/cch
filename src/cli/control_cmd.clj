(ns cli.control-cmd
  "CLI entrypoint for the native control-plane proof of concept."
  (:require [babashka.process :as p]
            [cch.control.claude :as claude]
            [cch.control.core :as control]
            [cheshire.core :as json]
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
    (zero? (:exit (p/sh [command "--version"] {:continue true})))
    (catch Exception _ false)))

(defn- configured? [command args]
  (try
    (zero? (:exit (p/sh (into [command] args) {:continue true})))
    (catch Exception _ false)))

(defn- install-mcp! [agent]
  (case agent
    :claude
    (when (command-available? "claude")
      (if (configured? "claude" ["mcp" "get" "cch"])
        :present
        (let [result (p/sh ["claude" "mcp" "add" "--scope" "user"
                            "cch" "--" "cch" "control" "mcp"]
                           {:continue true})]
          (if (zero? (:exit result)) :installed
              (throw (ex-info (str "Could not install Claude MCP config: "
                                   (str/trim (:err result)))
                              {:type :mcp-install-failed :agent "claude"}))))))

    :codex
    (when (command-available? "codex")
      (if (configured? "codex" ["mcp" "get" "cch"])
        :present
        (let [result (p/sh ["codex" "mcp" "add" "cch" "--"
                            "cch" "control" "mcp"]
                           {:continue true})]
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
    (println "Codex also needs its shared native daemon. Run once:")
    (println "  codex remote-control start")
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
