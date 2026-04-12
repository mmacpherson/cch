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

(defn- event-summary
  "One-line summary of a hook's event subscription(s)."
  [{:keys [event matcher events]}]
  (cond
    events (format "%d events" (count events))
    matcher (format "%s on %s" event matcher)
    :else event))

(defn run [& _args]
  (let [global-path  (settings/global-settings-path)
        project-path (settings/project-settings-path ".")
        global-installed  (installed-hooks global-path)
        project-installed (installed-hooks project-path)]
    (println "Available hooks:")
    (println)
    (doseq [[name hook] (registry/list-hooks)]
      (let [status (cond
                     (project-installed name) "[project]"
                     (global-installed name)  "[global] "
                     :else                    "         ")]
        (println (format "  %s %-16s %s" status name (:description hook)))
        (println (format "             %s" (event-summary hook)))))
    (println)
    (println "Install: cch install <hook-name> [--global] [--exclude=Event1,...]")))
