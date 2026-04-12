(ns cch.config
  "Two-tier configuration loading.

  Merge order (later wins):
    1. Global:   ~/.config/cch/config.yaml       (per-user, optional)
    2. Project:  .cch-config.yaml                (walks up from cwd to worktree root)

  Schema:

    log:
      enabled: true
    hooks:
      <hook-name>:
        <hook-specific config>

  Each hook reads its own section via (get-in cfg [:hooks :<name>]).

  Missing files return nil. Malformed YAML throws ex-info with
  {:type ::malformed-config} — callers decide policy (security-sensitive
  hooks should translate this into a deny decision rather than silently
  proceeding)."
  (:require [clj-yaml.core :as yaml]
            [babashka.fs :as fs]))

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
