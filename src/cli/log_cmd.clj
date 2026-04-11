(ns cli.log-cmd
  "cch log — query event history from SQLite."
  (:require [cch.log :as log]
            [clojure.string :as str]))

(defn parse-opts
  "Parse --key=value flags from args."
  [args]
  (reduce (fn [m arg]
            (if-let [[_ k v] (re-matches #"--(\w+)=(.+)" arg)]
              (assoc m (keyword k) v)
              (if-let [[_ k] (re-matches #"--(\w+)" arg)]
                (assoc m (keyword k) true)
                m)))
          {} args))

(defn format-event [e]
  (let [ts   (:timestamp e)
        hook (:hook_name e)
        tool (:tool_name e)
        dec  (:decision e)
        file (:file_path e)
        ms   (:elapsed_ms e)]
    (format "%s  %-14s %-8s %-6s %s%s"
            (or ts "?")
            (or hook "?")
            (or tool "?")
            (or dec "allow")
            (or file "")
            (if ms (format "  (%.1fms)" (double ms)) ""))))

(defn run [& args]
  (let [opts   (parse-opts args)
        events (log/query-events
                 :limit    (some-> (:limit opts) parse-long)
                 :hook     (:hook opts)
                 :session  (:session opts)
                 :decision (:decision opts)
                 :since    (:since opts))]
    (if (seq events)
      (do
        (println "Timestamp              Hook           Tool     Decision  File")
        (println (str/join (repeat 90 "─")))
        (doseq [e (reverse events)]
          (println (format-event e))))
      (println "No events found."))))
