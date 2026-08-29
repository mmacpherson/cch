(ns cch.control.usage-sync
  "Crash-safe normalized usage exchange for a paired outbound runner.

  Local observations are published by SQLite row id. Broker observations are
  pulled by broker cursor and materialized with publishable=0, preventing echo.
  Neither direction runs on a hook or native-message delivery thread."
  (:require [cch.control.remote :as remote]
            [cch.log :as log]
            [cch.usage-observation :as usage]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs])
  (:import [java.time Instant]))

(def ^:const default-batch-size 200)
(def ^:const default-backfill-batch-size 1000)
;; Keep one day inside the broker's 91-day retention boundary. A historical
;; replay can take long enough that rows exactly on the broker boundary expire
;; between local selection and upload; the transport buffer prevents a stale
;; first row from wedging an otherwise valid batch.
(def ^:const default-backfill-retention-ms (* 90 24 60 60 1000))
(def ^:const default-publish-retention-ms default-backfill-retention-ms)

(defonce ^:private ensured-paths (atom #{}))

(defn- datasource [path]
  {:dbtype "sqlite" :dbname path})

(defn- ensure-db! [path]
  (when-not (contains? @ensured-paths path)
    (log/ensure-db! path)
    (swap! ensured-paths conj path)))

(defn- rows [connectable statement]
  (jdbc/execute! connectable statement
                 {:builder-fn rs/as-unqualified-maps}))

(defn- row [connectable statement]
  (jdbc/execute-one! connectable statement
                     {:builder-fn rs/as-unqualified-maps}))

(defn- cursor [connectable direction]
  (or (:cursor
        (row connectable
             ["SELECT cursor FROM usage_sync_state WHERE direction=?"
              direction]))
      0))

(defn- set-cursor! [connectable direction value]
  (jdbc/execute!
    connectable
    [(str "INSERT INTO usage_sync_state(direction,cursor) VALUES (?,?) "
          "ON CONFLICT(direction) DO UPDATE SET cursor=excluded.cursor,"
          "updated_at=strftime('%Y-%m-%dT%H:%M:%f','now')")
     direction value]))

(defn- local-row->observation
  [{:keys [event_id schema_version observed_at agent window_key
           used_percentage resets_at]}]
  {:event-id event_id
   :schema-version schema_version
   :observed-at observed_at
   :agent agent
   :window window_key
   :used-percentage used_percentage
   :resets-at resets_at})

(defn backfill-once!
  "Import one bounded batch of source-local legacy snapshots. The derived event
  ids are deterministic, so interruption and replay are harmless. Rows copied
  into the legacy table from another node (`origin_id` non-null) are excluded."
  ([path]
   (backfill-once! path (System/currentTimeMillis)
                   default-backfill-batch-size))
  ([path now-ms batch-size]
   (ensure-db! path)
   (let [ds (jdbc/get-datasource (datasource path))
         state (or (:last_context_id
                     (row ds ["SELECT last_context_id FROM usage_backfill_state WHERE singleton_id=1"]))
                   0)
         cutoff (str (Instant/ofEpochMilli
                       (- now-ms default-backfill-retention-ms)))
         snapshots
         (rows ds [(str "SELECT id,timestamp,agent,payload FROM context_snapshots "
                        "WHERE id>? AND origin_id IS NULL AND timestamp>=? "
                        "ORDER BY id LIMIT ?")
                   state cutoff batch-size])
         observations
         (mapcat (fn [{:keys [timestamp agent payload]}]
                   (usage/from-snapshot {:agent agent
                                         :observed-at timestamp
                                         :payload payload}))
                 snapshots)
         inserted (atom 0)
         through (or (:id (peek snapshots)) state)]
     (when (seq snapshots)
       (jdbc/with-transaction [tx ds]
         (jdbc/execute! tx ["PRAGMA busy_timeout=5000"])
         (doseq [{:keys [event-id schema-version observed-at agent window
                         used-percentage resets-at]}
                 observations]
           (let [result
                 (first
                   (rows
                     tx
                     [(str "INSERT OR IGNORE INTO usage_observations "
                           "(event_id,schema_version,observed_at,agent,window_key,"
                           "used_percentage,resets_at,publishable) "
                           "VALUES (?,?,?,?,?,?,?,1)")
                      event-id schema-version observed-at agent window
                      used-percentage resets-at]))]
             (when (= 1 (:next.jdbc/update-count result))
               (swap! inserted inc))))
         (jdbc/execute!
           tx
           [(str "INSERT INTO usage_backfill_state(singleton_id,last_context_id) "
                 "VALUES (1,?) ON CONFLICT(singleton_id) DO UPDATE SET "
                 "last_context_id=excluded.last_context_id,"
                 "updated_at=strftime('%Y-%m-%dT%H:%M:%f','now')")
            through])))
     {:snapshots (count snapshots)
      :observations (count observations)
      :inserted @inserted
      :cursor through})))

(defn publish-once!
  "Publish the next local batch and advance only after broker acknowledgement."
  ([config path] (publish-once! config path default-batch-size))
  ([config path batch-size]
   (ensure-db! path)
   (let [ds (jdbc/get-datasource (datasource path))
         after (cursor ds "publish")
         cutoff (- (System/currentTimeMillis) default-publish-retention-ms)
         local-rows
         (rows ds [(str "SELECT id,event_id,schema_version,observed_at,agent,"
                        "window_key,used_percentage,resets_at "
                        "FROM usage_observations "
                        "WHERE publishable=1 AND id>? AND observed_at>=? "
                        "ORDER BY id LIMIT ?")
                   after cutoff batch-size])]
     (if (empty? local-rows)
       {:sent 0 :accepted 0 :duplicates 0 :cursor after}
       (let [result (remote/publish-usage-observations!
                      config (mapv local-row->observation local-rows))
             through (:id (peek local-rows))]
         (jdbc/with-transaction [tx ds]
           (jdbc/execute! tx ["PRAGMA busy_timeout=5000"])
           (set-cursor! tx "publish" through))
         {:sent (count local-rows)
          :accepted (:accepted result)
          :duplicates (:duplicates result)
          :cursor through})))))

(defn- validate-pull-response!
  [after {:keys [observations next-cursor]}]
  (when-not (and (vector? observations)
                 (integer? next-cursor)
                 (<= after next-cursor))
    (throw (ex-info "Broker returned an invalid usage page"
                    {:type :invalid-usage-page})))
  (loop [prior after
         remaining observations
         validated []]
    (if-let [observation (first remaining)]
      (let [broker-cursor (:cursor observation)
            canonical (usage/validate-observation! (dissoc observation :cursor))]
        (when-not (and (integer? broker-cursor) (< prior broker-cursor))
          (throw (ex-info "Broker returned a non-monotonic usage cursor"
                          {:type :invalid-usage-page})))
        (recur broker-cursor (next remaining)
               (conj validated (assoc canonical :cursor broker-cursor))))
      (let [derived (or (:cursor (peek validated)) after)]
        (when-not (= derived next-cursor)
          (throw (ex-info "Broker usage page cursor does not match its rows"
                          {:type :invalid-usage-page})))
        validated))))

(defn pull-once!
  "Pull one broker page and atomically materialize it with the new cursor."
  ([config path] (pull-once! config path default-batch-size))
  ([config path batch-size]
   (ensure-db! path)
   (let [ds (jdbc/get-datasource (datasource path))
         after (cursor ds "pull")
         response (remote/read-usage-observations! config after batch-size)
         observations (validate-pull-response! after response)
         inserted (atom 0)]
     (jdbc/with-transaction [tx ds]
       (jdbc/execute! tx ["PRAGMA busy_timeout=5000"])
       (doseq [{:keys [event-id schema-version observed-at agent window
                       used-percentage resets-at]}
               observations]
         (let [result
               (first
                 (rows
                   tx
                   [(str "INSERT OR IGNORE INTO usage_observations "
                         "(event_id,schema_version,observed_at,agent,window_key,"
                         "used_percentage,resets_at,publishable) "
                         "VALUES (?,?,?,?,?,?,?,0)")
                    event-id schema-version observed-at agent window
                    used-percentage resets-at]))]
           (when (= 1 (:next.jdbc/update-count result))
             (swap! inserted inc))))
       (set-cursor! tx "pull" (:next-cursor response)))
     {:received (count observations)
      :inserted @inserted
      :cursor (:next-cursor response)})))

(defn tick!
  "Attempt both directions independently so one malformed/local failure cannot
  prevent the other direction from making progress."
  ([config path] (tick! config path default-batch-size))
  ([config path batch-size]
   (reduce
     (fn [result [operation run]]
       (try
         (assoc result operation (run))
         (catch Exception error
           (update result :errors conj {:operation operation :error error}))))
     {:errors []}
     [[:backfill #(backfill-once! path (System/currentTimeMillis)
                                  default-backfill-batch-size)]
      [:publish #(publish-once! config path batch-size)]
      [:pull #(pull-once! config path batch-size)]])))
