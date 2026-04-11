(ns cch.config
  "Three-tier configuration loading.

  Merge order (most specific wins):
    1. Global:       ~/.config/cch/config.edn
    2. Per-project:  .claude-hooks.edn (walks up from cwd)
    3. Per-hook:     .<hook-name>.edn (walks up from cwd)"
  (:require [clojure.edn :as edn]
            [babashka.fs :as fs]))

(defn global-config-path
  "Returns the global config path, respecting XDG_CONFIG_HOME."
  []
  (str (or (System/getenv "XDG_CONFIG_HOME")
           (str (System/getProperty "user.home") "/.config"))
       "/cch/config.edn"))

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

(defn load-edn
  "Load an EDN file, returning nil if it doesn't exist."
  [path]
  (when (and path (fs/exists? path))
    (edn/read-string (slurp path))))

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
  "Load and merge configuration for a hook invocation.
  Optional boundary limits how far up the tree config discovery walks."
  ([cwd]
   (load-config cwd nil nil))
  ([cwd hook-name]
   (load-config cwd hook-name nil))
  ([cwd hook-name boundary]
   (let [global   (load-edn (global-config-path))
         project  (some-> (find-config-up cwd ".claude-hooks.edn" boundary) load-edn)
         hook-cfg (when hook-name
                    (some-> (find-config-up cwd (str "." hook-name ".edn") boundary)
                            load-edn))]
     (deep-merge global project hook-cfg))))
