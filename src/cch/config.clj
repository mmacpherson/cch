(ns cch.config
  "Two-tier configuration loading plus effective-config resolution for
  the HTTP dispatcher.

  Legacy per-hook loader (scope-lock, command-audit use this directly):
    Global:   ~/.config/cch/config.yaml       (per-user, optional)
    Project:  .cch-config.yaml                (walks up from cwd to worktree root)

  Schema:

    log:
      enabled: true
    hooks:
      <hook-name>:
        enabled: true            # optional; defaults true when section present
        <hook-specific config>

  Each hook reads its own section via (get-in cfg [:hooks :<name>]).

  Missing files return nil. Malformed YAML throws ex-info with
  {:type ::malformed-config} — callers decide policy.

  --- Dispatcher-side effective config ---

  `load-effective-config` returns the resolved enable/options state per
  hook for a given cwd. Precedence (later wins, per hook entry):

    1. hardcoded defaults (all registered hooks enabled, no options)
    2. DB global-scope rows
    3. DB repo-scope rows                 ← skipped if repo YAML present
    4. repo .cch-config.yaml (if present)

  When a repo commits a YAML, DB per-repo rows are ignored for that repo
  — the YAML is authoritative for team agreement. DB global still
  contributes baseline enablement, since team agreements override user
  defaults. See claude-code-hooks-nkp."
  (:require [clj-yaml.core :as yaml]
            [babashka.fs :as fs]
            [babashka.process :as p]
            [clojure.string :as str]
            [cch.config-db :as cdb]
            [cli.registry :as registry]))

(defn global-config-path
  "Returns the global config path, respecting XDG_CONFIG_HOME."
  []
  (str (or (System/getenv "XDG_CONFIG_HOME")
           (str (System/getProperty "user.home") "/.config"))
       "/cch/config.yaml"))

(defn find-config-up
  "Walk up from dir looking for filename. Returns path or nil.
  Optional boundary stops the walk (inclusive — checks boundary dir, but not above)."
  ([dir filename]
   (find-config-up dir filename nil))
  ([dir filename boundary]
   (loop [d (fs/path dir)]
     (when d
       (let [candidate (fs/path d filename)]
         (if (fs/exists? candidate)
           (str candidate)
           (when-not (and boundary (= (str d) (str (fs/path boundary))))
             (recur (fs/parent d)))))))))

(defn load-yaml
  "Load a YAML file. Returns nil if missing, throws ex-info with
  {:type ::malformed-config} on parse failure, returns the parsed map
  (keyword-keyed) on success.

  Distinguishes missing from malformed so callers whose correctness
  depends on the config (e.g. security-narrowing hooks) can fail closed
  rather than silently proceed as if no config existed."
  [path]
  (when (and path (fs/exists? path))
    (try
      (yaml/parse-string (slurp path) :keywords true)
      (catch Exception e
        (throw (ex-info (str "malformed YAML at " path ": " (.getMessage e))
                        {:type ::malformed-config :path path}
                        e))))))

(defn deep-merge
  "Recursively merge maps. Later values win for non-map keys."
  [& maps]
  (reduce (fn [acc m]
            (reduce-kv (fn [a k v]
                         (if (and (map? (get a k)) (map? v))
                           (assoc a k (deep-merge (get a k) v))
                           (assoc a k v)))
                       acc m))
          {} (remove nil? maps)))

(defn load-config
  "Load and merge global + project configuration.
  Optional boundary limits how far up the tree project config discovery walks.
  Throws ex-info on malformed YAML in either tier; caller decides policy."
  ([cwd] (load-config cwd nil))
  ([cwd boundary]
   (let [global  (load-yaml (global-config-path))
         project (some-> (find-config-up cwd ".cch-config.yaml" boundary) load-yaml)]
     (deep-merge global project))))

;; --- Effective config (nkp dispatcher) ---

(defn worktree-root
  "Git worktree root for cwd, or cwd itself if not in a repo.
  Reused from hook-level helpers; centralized here since the dispatcher
  needs it to locate repo-scoped config (YAML + DB rows)."
  [cwd]
  (let [result (p/sh ["git" "-C" (or cwd ".") "rev-parse" "--show-toplevel"])]
    (if (zero? (:exit result))
      (str/trim (:out result))
      (or cwd "."))))

(defn- default-entry [_hook-name]
  {:enabled? true :options nil :source :default})

(defn- hardcoded-defaults
  "All registered hooks, enabled, no options."
  []
  (into {} (for [[n _] (registry/list-hooks)]
             [n (default-entry n)])))

(defn- db-rows->entries
  "Convert cch.config-db row maps to the layer shape. `source` is the
  kw to stamp into each entry so dispatchers/UI can show where a
  setting came from."
  [rows source]
  (into {} (for [{:keys [hook-name enabled options]} rows]
             [hook-name {:enabled? enabled
                         :options  options
                         :source   source}])))

(defn- yaml-hooks->entries
  "Pluck :hooks from a parsed YAML map and render as layer entries.
  Semantics: presence of a section means enabled unless :enabled is
  explicitly false. Options are the rest of the section."
  [yaml-map]
  (when-let [hooks (:hooks yaml-map)]
    (into {} (for [[k v] hooks
                   :let [hname (name k)
                         enabled (not= false (:enabled v))
                         opts    (not-empty (dissoc v :enabled))]]
               [hname {:enabled? enabled
                       :options  opts
                       :source   :repo-yaml}]))))

(defn- merge-layers
  "Combine layer maps in precedence order (later wins). Each layer maps
  hook-name → entry. Later-layer entries fully replace earlier ones —
  no per-key merging of options, so overrides are unambiguous."
  [& layers]
  (reduce (fn [acc layer] (merge acc layer)) {} (remove nil? layers)))

(defn load-effective-config
  "Resolve effective hook config for a cwd.

  Returns a map shaped:
    {:hooks     {hook-name → {:enabled? bool :options map-or-nil :source kw}}
     :repo-root \"/abs/path\"
     :yaml-path \"/abs/.cch-config.yaml\" or nil
     :yaml-error \"...\" or nil}

  :source values: :default :db-global :db-repo :repo-yaml

  Malformed repo YAML does not throw — :yaml-error is set and the layer
  is skipped. Dispatcher caller decides whether to fail closed (block
  everything) or fall through to DB layers. This matches the posture
  tracked in scope-lock (fail closed at the hook level) while leaving
  the policy choice explicit at the dispatcher layer."
  [cwd]
  (let [root       (worktree-root cwd)
        yaml-path  (find-config-up (or cwd root) ".cch-config.yaml" root)
        [yaml err] (try
                     [(load-yaml yaml-path) nil]
                     (catch clojure.lang.ExceptionInfo e
                       [nil (.getMessage e)]))
        defaults   (hardcoded-defaults)
        global     (db-rows->entries (cdb/list-for-scope cdb/global-scope) :db-global)
        repo       (when-not yaml
                     (db-rows->entries (cdb/list-for-scope (cdb/repo-scope root)) :db-repo))
        yaml-ents  (when yaml (yaml-hooks->entries yaml))
        effective  (merge-layers defaults global repo yaml-ents)]
    {:hooks      effective
     :repo-root  root
     :yaml-path  (when yaml yaml-path)
     :yaml-error err}))
