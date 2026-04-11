(ns cli.list-cmd
  "cch list — show available and installed hooks."
  (:require [cli.registry :as registry]
            [cli.settings :as settings]))

(defn installed-hooks
  "Return set of hook names currently installed in a settings file."
  [path]
  (let [s (settings/read-settings path)]
    (->> (vals (:hooks s))
         (mapcat identity)
         (mapcat :hooks)
         (keep :command)
         (keep #(second (re-find #"# cch:(\S+)" %)))
         set)))

(defn run [& _args]
  (let [global-path  (settings/global-settings-path)
        project-path (settings/project-settings-path ".")
        global-installed  (installed-hooks global-path)
        project-installed (installed-hooks project-path)]
    (println "Available hooks:")
    (println)
    (doseq [[name {:keys [event matcher description]}] (registry/list-hooks)]
      (let [status (cond
                     (project-installed name) "[project]"
                     (global-installed name)  "[global] "
                     :else                    "         ")]
        (println (format "  %s %-16s %s" status name description))
        (println (format "             %s on %s" event matcher))))
    (println)
    (println "Install: cch install <hook-name> [--global]")))
