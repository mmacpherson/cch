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
            [cch.config-db :as cdb]
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
  (let [path (log/db-path)]
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

(defn run
  "cch install [--global] — bootstrap cch in the current repo (default)
  or globally. Writes dispatcher entries, prompt/agent entries, and
  enables all :code hooks at global scope."
  [& args]
  (let [[flags _kvs _pos] (parse-flags args)
        global? (contains? flags "--global")
        path    (if global?
                  (settings/global-settings-path)
                  (settings/project-settings-path "."))]
    (install-dispatch-entries! path)
    (install-prompt-and-agent-entries! path)
    (enable-code-hooks-globally!)
    (println (format "Installed cch to %s" path))
    (println (format "  %d dispatcher entries written"
                     (count (registry/dispatcher-events))))
    (let [n-code   (count (filter #(= :code   (registry/hook-type (second %)))
                                  (registry/list-hooks)))
          n-prompt (count (filter #(= :prompt (registry/hook-type (second %)))
                                  (registry/list-hooks)))
          n-agent  (count (filter #(= :agent  (registry/hook-type (second %)))
                                  (registry/list-hooks)))]
      (println (format "  %d code hook(s) enabled globally (via dispatcher)" n-code))
      (when (pos? n-prompt)
        (println (format "  %d native prompt entries written" n-prompt)))
      (when (pos? n-agent)
        (println (format "  %d native agent entries written" n-agent))))
    (when-not (server-reachable? "127.0.0.1" 8888)
      (println)
      (println "⚠  cch serve is not reachable at http://127.0.0.1:8888.")
      (println "   Code-hook dispatch will fail with ECONNREFUSED until the server is running.")
      (println "   For persistent setup:")
      (println "       cch install-service")
      (println "   Or start a one-off session with:")
      (println "       cch serve &"))))

(defn run-uninstall
  "cch uninstall [--global] — remove every cch-owned settings.json entry
  and clear the hook_config table."
  [& args]
  (let [[flags _kvs _pos] (parse-flags args)
        global? (contains? flags "--global")
        path    (if global?
                  (settings/global-settings-path)
                  (settings/project-settings-path "."))]
    (settings/remove-all-cch! path)
    (clear-hook-config!)
    (println (format "Uninstalled cch from %s" path))
    (println "  hook_config table cleared")))
