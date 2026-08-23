(ns cli.log-cmd
  "cch log — query event history from SQLite."
  (:require [cch.log :as log]
            [clojure.string :as str]))

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

(defn run [{:keys [limit hook event session decision since]} _arguments]
  (let [events (log/query-events
                 :limit    limit
                 :hook     hook
                 :event    event
                 :session  session
                 :decision decision
                 :since    since)]
    (if (seq events)
      (do
        (println "Timestamp              Hook           Tool     Decision  File")
        (println (str/join (repeat 90 "─")))
        (doseq [e (reverse events)]
          (println (format-event e))))
      (println "No events found."))))
