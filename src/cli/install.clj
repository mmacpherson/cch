(ns cli.install
  "cch install / uninstall — bootstrap and cleanup under the dispatcher model.

  `cch install [--global]` writes universal dispatch entries to settings.json
  (one per event cch code hooks care about), plus native settings.json
  entries for every prompt/agent hook in the registry. Enables every
  :code hook in the DB at global scope so they run by default.

  `cch uninstall [--global]` removes every cch-owned entry from settings.json
  and clears the hook_config table.

  Per-hook enable/disable (at global or per-repo scope) is handled via
  the web UI's config CRUD, not via install/uninstall."
  (:require [babashka.process :as p]
            [cch.agents.agy :as agy]
            [cch.agents.codex :as codex]
            [cch.config-db :as cdb]
            [cch.db :as db]
            [cch.log :as log]
            [cli.registry :as registry]
            [cli.settings :as settings]
            [clojure.string :as str]))

(defn server-reachable?
  "Fast TCP connect probe. Returns true if `cch serve` appears to be up
  at host:port within a short timeout. Used for a post-install warning."
  [host port]
  (try
    (with-open [s (java.net.Socket.)]
      (.connect s (java.net.InetSocketAddress. ^String host (int port)) 500)
      true)
    (catch Exception _ false)))

(defn parse-flags
  "Parse --flag and --key=value forms from args.
  Returns [flag-set kv-map positional-vec]."
  [args]
  (reduce (fn [[flags kvs pos] arg]
            (cond
              (re-matches #"--([\w-]+)=(.+)" arg)
              (let [[_ k v] (re-matches #"--([\w-]+)=(.+)" arg)]
                [flags (assoc kvs (keyword k) v) pos])

              (str/starts-with? arg "--")
              [(conj flags arg) kvs pos]

              :else
              [flags kvs (conj pos arg)]))
          [#{} {} []]
          args))

(defn- clear-hook-config!
  "Delete every row from hook_config. Used by uninstall."
  []
  (let [path (db/db-path)]
    (log/ensure-db! path)
    (p/sh ["sqlite3" path "DELETE FROM hook_config;"])
    nil))

(defn- install-dispatch-entries!
  "For every event cch handles, write a universal dispatch entry."
  [settings-path]
  (registry/validate-registry!)
  (doseq [{:keys [event matcher]} (registry/dispatcher-events)]
    (settings/add-dispatch-entry! settings-path event :matcher matcher)))

(defn- install-prompt-and-agent-entries!
  "For every :prompt / :agent registry entry, write its native settings.json entry."
  [settings-path]
  (doseq [[hook-name entry] (registry/list-hooks)
          :let [t (registry/hook-type entry)]
          :when (contains? #{:prompt :agent} t)]
    (case t
      :prompt (settings/add-prompt-entry! settings-path (:event entry) (:matcher entry)
                                          hook-name entry)
      :agent  (settings/add-agent-entry!  settings-path (:event entry) (:matcher entry)
                                          hook-name entry))))

(defn- enable-code-hooks-globally!
  "Flip enabled=true in hook_config at global scope for every :code hook.
  Idempotent (upsert)."
  []
  (doseq [[hook-name entry] (registry/list-hooks)
          :when (= :code (registry/hook-type entry))]
    (cdb/upsert! {:hook-name hook-name
                  :scope     cdb/global-scope
                  :enabled   true})))

(defn- print-server-warning-if-down []
  (when-not (server-reachable? "127.0.0.1" 8888)
    (println)
    (println "⚠  cch serve is not reachable at http://127.0.0.1:8888.")
    (println "   Code-hook dispatch will fail with ECONNREFUSED until the server is running.")
    (println "   For persistent setup:")
    (println "       cch install-service")
    (println "   Or start a one-off session with:")
    (println "       cch serve &")))

(defn- run-claude-install! [global?]
  (let [path (if global?
               (settings/global-settings-path)
               (settings/project-settings-path "."))]
    (install-dispatch-entries! path)
    (install-prompt-and-agent-entries! path)
    (enable-code-hooks-globally!)
    (println (format "Installed cch to %s" path))
    (println (format "  %d dispatcher entries written"
                     (count (registry/dispatcher-events))))
    (let [by-type   (group-by #(registry/hook-type (second %)) (registry/list-hooks))
          n-code    (count (:code   by-type))
          n-prompt  (count (:prompt by-type))
          n-agent   (count (:agent  by-type))]
      (println (format "  %d code hook(s) enabled globally (via dispatcher)" n-code))
      (when (pos? n-prompt)
        (println (format "  %d native prompt entries written" n-prompt)))
      (when (pos? n-agent)
        (println (format "  %d native agent entries written" n-agent))))
    (print-server-warning-if-down)))

(defn- run-codex-install! []
  (let [{:keys [path written skipped]} (codex/install!)]
    (enable-code-hooks-globally!)
    (println (format "Installed cch to %s" path))
    (println (format "  %d codex hook entries written" written))
    (when (seq skipped)
      (println (format "  skipped %d event(s) Codex doesn't support: %s"
                       (count skipped) (str/join ", " skipped))))
    (println "  (Codex events route to the same /dispatch/<event> endpoints")
    (println "   used by Claude Code)")
    (print-server-warning-if-down)))

(defn- run-agy-install! []
  (let [{:keys [path script]} (agy/install!)]
    (println (format "Installed cch AGY usage capture to %s" path))
    (println (format "  status-line adapter: %s" script))
    (println "  AGY quota snapshots will appear under Source → AGY on /usage")
    (print-server-warning-if-down)))

(defn run
  "cch install [--global] [--codex|--agy] — bootstrap cch.

  Default: writes Claude Code dispatcher entries to the current repo's
  settings.local.json. With --global, writes the global Claude
  settings.json. With --codex, writes Codex entries to the user's
  ~/.codex/config.toml instead (Codex has no project-vs-global split, so
  --codex and --global are mutually exclusive). With --agy, configures the
  documented AGY statusLine feed for quota capture.

  All paths enable :code hooks at global scope."
  [& args]
  (let [[flags _kvs _pos] (parse-flags args)
        codex?  (contains? flags "--codex")
        agy?    (contains? flags "--agy")
        global? (contains? flags "--global")]
    (cond
      (> (count (filter true? [codex? agy? global?])) 1)
      (do (println "Error: --global, --codex, and --agy are mutually exclusive")
          (System/exit 2))

      codex?
      (run-codex-install!)

      agy?
      (run-agy-install!)

      :else
      (run-claude-install! global?))))

(defn run-uninstall
  "cch uninstall [--global] [--codex|--agy] — remove cch-owned entries.

  Default removes them from the repo's Claude settings.local.json;
  --global removes from the global Claude settings.json; --codex removes
  the cch sentinel block from ~/.codex/config.toml. --codex and --global
  are mutually exclusive. Always clears the hook_config table."
  [& args]
  (let [[flags _kvs _pos] (parse-flags args)
        codex?  (contains? flags "--codex")
        agy?    (contains? flags "--agy")
        global? (contains? flags "--global")]
    (cond
      (> (count (filter true? [codex? agy? global?])) 1)
      (do (println "Error: --global, --codex, and --agy are mutually exclusive")
          (System/exit 2))

      codex?
      (let [path (codex/uninstall!)]
        (clear-hook-config!)
        (println (format "Uninstalled cch from %s" path))
        (println "  hook_config table cleared"))

      agy?
      (let [{:keys [path restored]} (agy/uninstall!)]
        (println (format "Uninstalled cch AGY usage capture from %s" path))
        (println (if restored
                   "  previous AGY status line restored"
                   "  current AGY status line left unchanged")))

      :else
      (let [path (if global?
                   (settings/global-settings-path)
                   (settings/project-settings-path "."))]
        (settings/remove-all-cch! path)
        (clear-hook-config!)
        (println (format "Uninstalled cch from %s" path))
        (println "  hook_config table cleared")))))
