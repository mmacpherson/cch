(ns cli.install
  "cch install/uninstall — manage hooks in settings.json."
  (:require [cli.registry :as registry]
            [cli.settings :as settings]
            [clojure.string :as str]))

(defn parse-flags
  "Parse --global and other flags from args. Returns [flags rest-args]."
  [args]
  (let [flags     (set (filter #(str/starts-with? % "--") args))
        rest-args (remove #(str/starts-with? % "--") args)]
    [flags (vec rest-args)]))

(defn run [& args]
  (let [[flags rest-args] (parse-flags args)
        hook-name         (first rest-args)
        global?           (contains? flags "--global")]

    (when-not hook-name
      (println "Usage: cch install <hook-name> [--global]")
      (println)
      (println "Available hooks:")
      (doseq [[name {:keys [description]}] (registry/list-hooks)]
        (println (format "  %-16s %s" name description)))
      (System/exit 1))

    (if-let [hook (registry/get-hook hook-name)]
      (let [path (if global?
                   (settings/global-settings-path)
                   (settings/project-settings-path "."))]
        (settings/add-hook! path (:event hook) (:matcher hook) (:ns hook))
        (println (format "Installed '%s' in %s" hook-name path))
        (println (format "  Event:   %s" (:event hook)))
        (println (format "  Matcher: %s" (:matcher hook))))
      (do
        (println (format "Unknown hook: '%s'" hook-name))
        (println "Run 'cch list' to see available hooks.")
        (System/exit 1)))))

(defn run-uninstall [& args]
  (let [[flags rest-args] (parse-flags args)
        hook-name         (first rest-args)
        global?           (contains? flags "--global")]

    (when-not hook-name
      (println "Usage: cch uninstall <hook-name> [--global]")
      (System/exit 1))

    ;; Look up in registry for event type, but fall back to scanning all
    ;; event types if the hook has been removed from the registry.
    (let [hook (registry/get-hook hook-name)
          path (if global?
                 (settings/global-settings-path)
                 (settings/project-settings-path "."))]
      (if hook
        (do
          (settings/remove-hook! path (:event hook) hook-name)
          (println (format "Uninstalled '%s' from %s" hook-name path)))
        ;; Hook not in registry — scan all event types for the cch tag
        (let [s (settings/read-settings path)
              event-types (keys (:hooks s))]
          (doseq [et event-types]
            (settings/remove-hook! path (name et) hook-name))
          (println (format "Uninstalled '%s' from %s (scanned all event types)" hook-name path)))))))
