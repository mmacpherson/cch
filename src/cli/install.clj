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
            [cch.doctor :as doctor]
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

(def ^:private known-flags
  "Flags accepted by both `cch install` and `cch uninstall`."
  #{"--codex" "--agy" "--global" "--all"})

(defn- help? [args]
  (boolean (some #{"--help" "-h"} args)))

(defn- unknown-flags
  "Flags in `flag-set` that are neither a known flag nor --help."
  [flag-set]
  (seq (remove (conj known-flags "--help") flag-set)))

(defn- print-install-help []
  (println "cch install [--global] [--all|--codex|--agy]")
  (println)
  (println "Bootstrap cch. Default target is the current repo's settings.local.json.")
  (println "  --all      Detect every agent on this box and provision each one")
  (println "  --global   Write to the global Claude settings.json instead")
  (println "  --codex    Write Codex entries to $CODEX_HOME/config.toml")
  (println "  --agy      Configure the AGY statusLine feed for quota capture")
  (println)
  (println "--codex and --agy are mutually exclusive with each other and --global.")
  (println "--all provisions all present agents; combine with --global to target")
  (println "the global Claude settings.json. Afterward, run `cch doctor` to verify."))

(defn- print-uninstall-help []
  (println "cch uninstall [--global] [--codex|--agy]")
  (println)
  (println "Remove cch-owned entries. Default target is the repo's settings.local.json.")
  (println "  --global   Remove from the global Claude settings.json instead")
  (println "  --codex    Remove the cch sentinel block from $CODEX_HOME/config.toml")
  (println "  --agy      Restore the previous AGY status line")
  (println)
  (println "--global, --codex, and --agy are mutually exclusive.")
  (println "Always clears the hook_config table."))

(defn- reject-unknown!
  "Print an error for unknown flags and exit non-zero. `cmd` is \"install\"
  or \"uninstall\" (used in the follow-up hint)."
  [unknown cmd]
  (println (format "Error: unknown flag(s): %s" (str/join ", " unknown)))
  (println (format "Run 'cch %s --help' for usage." cmd))
  (System/exit 2))

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
    (println "  AGY quota snapshots will appear under Source → AGY on /usage"))
  (let [{:keys [path events]} (agy/install-hooks!)]
    (println (format "Installed cch AGY lifecycle hooks to %s" path))
    (println (format "  %d event(s) routed to /dispatch: %s"
                     (count events) (str/join ", " events)))
    (println "  AGY tool events will appear in the event log tagged agent=agy"))
  (print-server-warning-if-down))

(defn- run-all-install!
  "Detect which agents are on this box and provision cch into each present
  one in a single pass. `global?` selects the Claude target (global
  settings.json vs the repo's settings.local.json). Absent agents are reported
  and skipped. Prints the codex interactive-trust reminder when codex is
  provisioned, since that step can't be automated headlessly."
  [global?]
  (let [agents  (doctor/detect-agents (System/getProperty "user.dir"))
        present (filter :present? agents)]
    (println "cch install --all — detecting agents on this box")
    (doseq [{:keys [agent present?]} agents]
      (println (format "  %-13s %s" agent (if present? "found" "not found — skipped"))))
    (println)
    (doseq [{:keys [agent]} present]
      (println (format "── %s ──" agent))
      (case agent
        "claude-code" (run-claude-install! global?)
        "codex"       (run-codex-install!)
        "agy"         (run-agy-install!))
      (println))
    (when (some #(= "codex" (:agent %)) present)
      (println "⚠  codex requires a one-time INTERACTIVE trust before headless")
      (println "   'codex exec' will fire the hooks. Run codex interactively once")
      (println "   in this box and trust it, then confirm with:")
      (println "       cch doctor")
      (println))
    (println "Verify wiring across all agents with:  cch doctor")))

(defn run
  "cch install [--global] [--codex|--agy] — bootstrap cch.

  Default: writes Claude Code dispatcher entries to the current repo's
  settings.local.json. With --global, writes the global Claude
  settings.json. With --codex, writes Codex entries to the user's
  $CODEX_HOME/config.toml instead (Codex has no project-vs-global split, so
  --codex and --global are mutually exclusive). With --agy, configures the
  documented AGY statusLine feed for quota capture.

  All paths enable :code hooks at global scope."
  [& args]
  (let [[flags _kvs _pos] (parse-flags args)
        all?    (contains? flags "--all")
        codex?  (contains? flags "--codex")
        agy?    (contains? flags "--agy")
        global? (contains? flags "--global")]
    (cond
      (help? args)
      (print-install-help)

      (unknown-flags flags)
      (reject-unknown! (unknown-flags flags) "install")

      (and all? (or codex? agy?))
      (do (println "Error: --all cannot be combined with --codex or --agy")
          (System/exit 2))

      (> (count (filter true? [codex? agy? global?])) 1)
      (do (println "Error: --global, --codex, and --agy are mutually exclusive")
          (System/exit 2))

      all?
      (run-all-install! global?)

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
  the cch sentinel block from $CODEX_HOME/config.toml. --codex and --global
  are mutually exclusive. Always clears the hook_config table."
  [& args]
  (let [[flags _kvs _pos] (parse-flags args)
        codex?  (contains? flags "--codex")
        agy?    (contains? flags "--agy")
        global? (contains? flags "--global")]
    (cond
      (help? args)
      (print-uninstall-help)

      (unknown-flags flags)
      (reject-unknown! (unknown-flags flags) "uninstall")

      (> (count (filter true? [codex? agy? global?])) 1)
      (do (println "Error: --global, --codex, and --agy are mutually exclusive")
          (System/exit 2))

      codex?
      (let [path (codex/uninstall!)]
        (clear-hook-config!)
        (println (format "Uninstalled cch from %s" path))
        (println "  hook_config table cleared"))

      agy?
      (do
        (let [{:keys [path restored]} (agy/uninstall!)]
          (println (format "Uninstalled cch AGY usage capture from %s" path))
          (println (if restored
                     "  previous AGY status line restored"
                     "  current AGY status line left unchanged")))
        (let [{:keys [path removed]} (agy/uninstall-hooks!)]
          (println (format "Removed cch AGY lifecycle hooks from %s" path))
          (when-not removed
            (println "  (no cch hook block was present)"))))

      :else
      (let [path (if global?
                   (settings/global-settings-path)
                   (settings/project-settings-path "."))]
        (settings/remove-all-cch! path)
        (clear-hook-config!)
        (println (format "Uninstalled cch from %s" path))
        (println "  hook_config table cleared")))))
