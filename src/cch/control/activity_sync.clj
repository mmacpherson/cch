(ns cch.control.activity-sync
  "Crash-safe one-way publication of normalized agent activity."
  (:require [cch.activity-observation :as activity]
            [cch.control.remote :as remote]
            [cch.log :as log]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs])
  (:import [java.time Instant]))

(def ^:const default-batch-size 200)
(def ^:const publish-retention-buffer-ms (* 6 24 60 60 1000))
(def ^:const publish-future-buffer-ms (* 60 1000))
(defonce ^:private ensured-paths (atom #{}))

(defn- datasource [path] {:dbtype "sqlite" :dbname path})

(defn- ensure-db! [path]
  (when-not (contains? @ensured-paths path)
    (log/ensure-db! path)
    (swap! ensured-paths conj path)))

(defn- row [connectable statement]
  (jdbc/execute-one! connectable statement
                     {:builder-fn rs/as-unqualified-maps}))

(defn- rows [connectable statement]
  (jdbc/execute! connectable statement
                 {:builder-fn rs/as-unqualified-maps}))

(defn tick!
  "Publish one source-local page and advance past every inspected raw row only
  after broker acknowledgement. Rows that do not map to the allowlist are
  intentionally skipped but still advance the local cursor."
  ([config path] (tick! config path default-batch-size))
  ([config path batch-size]
   (ensure-db! path)
   (let [ds (jdbc/get-datasource (datasource path))
         after (or (:last_event_id
                     (row ds ["SELECT last_event_id FROM activity_sync_state WHERE singleton_id=1"]))
                   0)
         now (System/currentTimeMillis)
         cutoff (- now publish-retention-buffer-ms)
         ;; A long-lived local event database can contain millions of rows.
         ;; Jump the private source cursor over history that the broker would
         ;; reject instead of walking it a few hundred rows per daemon tick.
         stale-through (or (:stale_through
                             (row ds [(str "SELECT coalesce("
                                           "(SELECT min(id)-1 FROM events "
                                           " WHERE id>? AND timestamp>=?),"
                                           "(SELECT max(id) FROM events)) AS stale_through")
                                      after (str (Instant/ofEpochMilli cutoff))]))
                           after)
         start-after (max after stale-through)
         source (rows ds [(str "SELECT id,timestamp,agent,hook_name,event_type,"
                               "tool_name,decision,elapsed_ms FROM events "
                               "WHERE id>? AND origin_id IS NULL ORDER BY id LIMIT ?")
                          start-after batch-size])
         observations (->> source
                           (keep activity/from-local-event)
                           (filter #(<= cutoff
                                        (:observed-at %)
                                        (+ now publish-future-buffer-ms)))
                           vec)
         through (or (:id (peek source)) start-after)
         result (if (seq observations)
                  (remote/publish-activity-observations! config observations)
                  {:accepted 0 :duplicates 0})]
     (when (> through after)
       (jdbc/execute!
         ds
         [(str "INSERT INTO activity_sync_state(singleton_id,last_event_id) "
               "VALUES (1,?) ON CONFLICT(singleton_id) DO UPDATE SET "
               "last_event_id=excluded.last_event_id,"
               "updated_at=strftime('%Y-%m-%dT%H:%M:%f','now')")
          through]))
     {:inspected (count source)
      :sent (count observations)
      :accepted (:accepted result)
      :duplicates (:duplicates result)
      :cursor through})))
