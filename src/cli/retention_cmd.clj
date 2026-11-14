(ns cli.retention-cmd
  "`cch retention`: show what the retention rules would change, or run them
  now. `cch serve` already runs them daily; this is for a first look, a
  manual pass, and reclaiming disk space afterwards."
  (:require [babashka.fs :as fs]
            [cch.db :as db]
            [cch.retention :as retention]))

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
      (retention/vacuum!)
      (println "Done."))))

(defn run [options _arguments]
  (if (fs/exists? (db/db-path))
    (report options)
    (println "No event database at" (db/db-path))))
