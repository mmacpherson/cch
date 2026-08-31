(ns cch.control.doctor
  "Sanitized, actionable diagnostics for native control-plane prerequisites."
  (:require [babashka.fs :as fs]
            [cch.control.core :as control]
            [cch.control.remote :as remote]
            [cch.subprocess :as subprocess]
            [cheshire.core :as json]
            [cli.control-runner-service :as runner-service]
            [clojure.string :as str]))

(def required-codex-methods
  #{"thread/loaded/list" "thread/read" "thread/queue/add"})

(defn- command-version [run-command command]
  (try
    (let [{:keys [exit out]} (run-command [command "--version"])]
      (if (zero? exit)
        {:status :ok :version (str/trim out)}
        {:status :unavailable
         :action (str "Install or repair the " command " CLI on PATH.")}))
    (catch Exception _
      {:status :unavailable
       :action (str "Install or repair the " command " CLI on PATH.")})))

(defn- codex-protocol
  [{:keys [run-command create-temp-dir delete-tree]}]
  (let [directory (str (create-temp-dir))]
    (try
      (let [{:keys [exit]} (run-command
                             ["codex" "app-server" "generate-json-schema"
                              "--experimental" "--out" directory])]
        (if-not (zero? exit)
          {:status :unavailable
           :action "Update the package-managed Codex CLI; app-server schema generation failed."}
          (let [schema-path (str directory "/ClientRequest.json")
                schema (if (fs/exists? schema-path)
                         (json/parse-string (slurp schema-path)) {})
                values (filter string? (tree-seq coll? seq schema))
                supported (set (filter (set values) required-codex-methods))
                missing (sort (remove supported required-codex-methods))]
            (if (empty? missing)
              {:status :ok :methods (sort supported)
               :source :generated-local-cli-schema}
              {:status :unsupported :missing-methods missing
               :action "Update Codex before enabling native routing; required app-server methods are missing."}))))
      (catch Exception _
        {:status :unavailable
         :action "Install a Codex CLI that can generate the experimental app-server schema."})
      (finally
        (delete-tree directory)))))

(defn- provider-report [agent version sessions errors]
  (let [error (some #(when (= agent (:agent %)) %) errors)]
    (cond-> (assoc version :sessions (count (filter #(= agent (:agent %)) sessions)))
      error (assoc :status :unavailable
                   :error-type (:type error)
                   :action (case agent
                             "codex" "Run `cch control install`, then verify the shared Codex app-server socket."
                             "claude" "Update Claude Code and rerun `cch control install` to register native inboxes.")))))

(defn report
  "Return diagnostics without route ids, paths, tokens, transcripts, or message
  bodies. Dependencies are injectable so tests do not inspect operator state."
  ([]
   (report {:run-command subprocess/run
            :create-temp-dir #(fs/create-temp-dir
                                {:prefix "cch-codex-schema-check-"})
            :delete-tree fs/delete-tree
            :list-local-sessions control/list-local-sessions
            :pairing-config remote/config
            :runner-status runner-service/status}))
  ([{:keys [run-command create-temp-dir delete-tree list-local-sessions
            pairing-config runner-status]}]
   (let [{:keys [sessions errors]} (list-local-sessions)
         pairing (try
                   (if (pairing-config)
                     {:status :configured :credential-storage :owner-only-local-file}
                     {:status :unpaired
                      :action "Set the three CCH_CONTROL_* values once and run `cch control pair`."})
                   (catch Exception error
                     {:status :invalid
                      :error-type (some-> error ex-data :type)
                      :action "Repair or replace the local runner pairing with `cch control pair`."}))
         claude (provider-report "claude" (command-version run-command "claude")
                                 sessions errors)
         codex (assoc (provider-report "codex" (command-version run-command "codex")
                                       sessions errors)
                      :protocol (codex-protocol
                                  {:run-command run-command
                                   :create-temp-dir create-temp-dir
                                   :delete-tree delete-tree}))
         supervision (runner-status)
         attention? (or (contains? #{:invalid} (:status pairing))
                        (= :unavailable (:status claude))
                        (= :unavailable (:status codex))
                        (not= :ok (get-in codex [:protocol :status]))
                        (and (= :configured (:status pairing))
                             (not= :active (:status supervision))))]
     {:status (if attention? :attention :ok)
      :pairing pairing
      :runner supervision
      :providers {:claude claude :codex codex}})))
