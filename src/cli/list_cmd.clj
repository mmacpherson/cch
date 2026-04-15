(ns cli.list-cmd
  "cch list — show registered hooks and their global enablement state.

  Under the dispatcher model, hooks are not 'installed per-project' anymore;
  they're registered in code (cli.registry) and enabled per scope in the
  hook_config DB table (toggled by `cch install` / the web UI's matrix
  page). This command is a quick at-a-glance view of what cch knows
  about and which ones are on at the global default."
  (:require [cch.config-db :as cdb]
            [cli.registry :as registry]))

(defn- event-summary
  "One-line summary of a hook's event subscription(s)."
  [{:keys [event matcher events]}]
  (cond
    events  (format "%d events (observer)" (count events))
    matcher (format "%s on %s" event matcher)
    :else   event))

(defn- enabled-globally?
  [hook-name global-rows]
  (let [row (first (filter #(= hook-name (:hook-name %)) global-rows))]
    (cond
      (nil? row)               true   ; default-on (hardcoded-defaults)
      (true? (:enabled row))   true
      :else                    false)))

(defn run [& _args]
  (let [global-rows (cdb/list-for-scope cdb/global-scope)
        hooks       (registry/list-hooks)]
    (println (format "Registered hooks (%d):" (count hooks)))
    (println)
    (doseq [[name hook] hooks
            :let [t       (registry/hook-type hook)
                  on?     (enabled-globally? name global-rows)
                  marker  (if on? "✓" "·")]]
      (println (format "  %s  %-15s  [%s]  %s"
                       marker name (clojure.core/name t) (:description hook)))
      (println (format "                       %s" (event-summary hook))))
    (println)
    (println "  ✓ enabled globally   · disabled globally")
    (println)
    (println "Bootstrap:    cch install [--global]")
    (println "Per-repo:     enable/disable in the dashboard at /hooks")))
