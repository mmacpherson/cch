(ns cli.retention-cmd
  "`cch retention`: show what the retention rules would change, or run them
  now. `cch serve` already runs them daily; this is for a first look, a
  manual pass, and reclaiming disk space afterwards."
  (:require [babashka.fs :as fs]
            [cch.db :as db]
            [cch.retention :as retention]
            [clojure.string]))

(defn- describe [days]
  (if (pos? days) (format "after %s days" days) "off"))

(defn- report [{:keys [vacuum] run? :run}]
  (let [{:keys [enabled event-payload-days event-days context-snapshot-days] :as s}
        (retention/load-settings)]
    (println (format "Retention for %s%s" (db/db-path)
                     (if enabled "" " (scheduled passes are disabled in the config)")))
    (println (format "  event payloads cleared  %s" (describe event-payload-days)))
    (println (format "  events deleted          %s" (describe event-days)))
    (println (format "  context snapshots       %s" (describe context-snapshot-days)))
    (println (format "  compaction while idle   %s"
                     (let [{:keys [compact-interval-days compact-free-fraction compact-idle-minutes]} s
                           triggers (cond-> []
                                      (pos? compact-interval-days)
                                      (conj (format "every %s days" compact-interval-days))
                                      (pos? compact-free-fraction)
                                      (conj (format "at %.0f%% free" (* 100.0 compact-free-fraction))))]
                       (if (seq triggers)
                         (format "%s, after %s idle minutes"
                                 (clojure.string/join " or " triggers) compact-idle-minutes)
                         "off"))))
    (if run?
      (let [{:keys [event-payloads events context-snapshots]} (retention/prune! s)]
        (println (format "Cleared %d event payloads, deleted %d events and %d context snapshots."
                         event-payloads events context-snapshots)))
      (let [{:keys [event-payloads events context-snapshots]} (retention/pending s)]
        (println (format "A pass now would clear %d event payloads, delete %d events and %d context snapshots."
                         event-payloads events context-snapshots))
        (println "Run it with `cch retention --run`.")))
    (when vacuum
      (println "Rebuilding the database file (stop `cch serve` first; this can take minutes)...")
      (println (if (retention/vacuum!)
                 "Done."
                 (str "Rebuilt, but another process has the database open, so the file has not"
                      " shrunk yet. It will after that process's next complete checkpoint."))))))

(defn run [options _arguments]
  (if (fs/exists? (db/db-path))
    (report options)
    (println "No event database at" (db/db-path))))
