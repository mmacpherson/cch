(ns cli.install
  "cch install/uninstall — manage hooks in settings.json."
  (:require [cli.registry :as registry]
            [cli.settings :as settings]
            [clojure.string :as str]))

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

(defn- select-events
  "Resolve the events this install should subscribe to.
  Single-event hook: [{:event ... :matcher ...}].
  Multi-event hook with optional exclusion list: filter :events."
  [hook exclude-set]
  (if-let [events (:events hook)]
    (remove #(contains? exclude-set (:event %)) events)
    [{:event (:event hook) :matcher (:matcher hook)}]))

(defn run [& args]
  (let [[flags kvs rest-args] (parse-flags args)
        hook-name             (first rest-args)
        global?               (contains? flags "--global")
        http?                 (contains? flags "--http")
        mode                  (if http? :http :command)
        exclude-set           (->> (or (:exclude kvs) "")
                                   (#(str/split % #","))
                                   (remove str/blank?)
                                   set)]

    (when-not hook-name
      (println "Usage: cch install <hook-name> [--global] [--http] [--exclude=Event1,Event2]")
      (println)
      (println "Available hooks:")
      (doseq [[name {:keys [description]}] (registry/list-hooks)]
        (println (format "  %-16s %s" name description)))
      (System/exit 1))

    (if-let [hook (registry/get-hook hook-name)]
      (let [path   (if global?
                     (settings/global-settings-path)
                     (settings/project-settings-path "."))
            events (select-events hook exclude-set)]
        (doseq [{:keys [event matcher]} events]
          (settings/add-hook! path event matcher (:ns hook) :mode mode))
        (println (format "Installed '%s' in %s (%s mode)"
                         hook-name path (name mode)))
        (when http?
          (println "  Make sure `cch serve` is running, or events will fail silently."))
        (if (:events hook)
          (do
            (println (format "  Subscribed to %d event(s):" (count events)))
            (doseq [{:keys [event matcher]} events]
              (println (format "    %s%s" event (if matcher (str "  [" matcher "]") ""))))
            (when (seq exclude-set)
              (println (format "  Excluded: %s" (str/join ", " (sort exclude-set))))))
          (do
            (println (format "  Event:   %s" (:event hook)))
            (println (format "  Matcher: %s" (:matcher hook))))))
      (do
        (println (format "Unknown hook: '%s'" hook-name))
        (println "Run 'cch list' to see available hooks.")
        (System/exit 1)))))

(defn run-uninstall [& args]
  (let [[flags _kvs rest-args] (parse-flags args)
        hook-name              (first rest-args)
        global?                (contains? flags "--global")]

    (when-not hook-name
      (println "Usage: cch uninstall <hook-name> [--global]")
      (System/exit 1))

    (let [hook (registry/get-hook hook-name)
          path (if global?
                 (settings/global-settings-path)
                 (settings/project-settings-path "."))]
      (cond
        ;; Multi-event registered hook: remove from every declared event.
        (:events hook)
        (do
          (doseq [{:keys [event]} (:events hook)]
            (settings/remove-hook! path event hook-name))
          (println (format "Uninstalled '%s' from %s (%d event(s))"
                           hook-name path (count (:events hook)))))

        ;; Legacy single-event hook.
        hook
        (do
          (settings/remove-hook! path (:event hook) hook-name)
          (println (format "Uninstalled '%s' from %s" hook-name path)))

        ;; Hook no longer in registry — scan all event types for the cch tag.
        :else
        (let [s           (settings/read-settings path)
              event-types (keys (:hooks s))]
          (doseq [et event-types]
            (settings/remove-hook! path (name et) hook-name))
          (println (format "Uninstalled '%s' from %s (scanned all event types)"
                           hook-name path)))))))
