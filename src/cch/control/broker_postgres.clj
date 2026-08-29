(ns cch.control.broker-postgres
  "Postgres-backed route leases and message metadata for the hosted broker.

  Message bodies and runner pairing tokens are deliberately absent from the
  schema. Bodies live only in this process; after a broker restart an identical
  enqueue rehydrates the body while the durable digest prevents conflicting
  reuse of a message id."
  (:require [cch.control.broker :as memory]
            [cch.control.broker-api :as api]
            [cch.control.naming :as naming]
            [cch.control.store :as store]
            [clojure.string :as str]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs])
  (:import [com.zaxxer.hikari HikariConfig HikariDataSource]
           [java.nio.charset StandardCharsets]
           [java.sql SQLException Timestamp]
           [java.time Instant OffsetDateTime ZoneOffset]))

(def ^:const default-schema "cch_control")
(def ^:const default-pool-size 4)
(def ^:const default-metadata-retention-ms (* 24 60 60 1000))
(def ^:private migration-lock-id 620240826001)

(defn- schema-name [value]
  (let [value (or (not-empty value) default-schema)]
    (when-not (re-matches #"[a-z][a-z0-9_]{0,62}" value)
      (throw (ex-info "Postgres schema name is invalid"
                      {:type :invalid-broker-config})))
    value))

(defn- qschema [schema]
  (str "\"" schema "\""))

(defn- table [schema name]
  (str (qschema schema) ".\"" name "\""))

(defn migration-statements
  "Return the public, credential-free schema DDL for one isolated schema."
  [schema]
  (let [schema (schema-name schema)
        runners (table schema "runners")
        sessions (table schema "sessions")
        messages (table schema "messages")]
    [(str "CREATE TABLE IF NOT EXISTS " runners " ("
          "runner_id text PRIMARY KEY,"
          "updated_at timestamptz NOT NULL,"
          "lease_expires_at timestamptz NOT NULL)")
     (str "CREATE TABLE IF NOT EXISTS " sessions " ("
          "route_id text PRIMARY KEY,"
          "runner_id text NOT NULL REFERENCES " runners
          "(runner_id) ON DELETE CASCADE,"
          "agent text NOT NULL CHECK (agent IN ('claude','codex')),"
          "native_status text NOT NULL,"
          "available boolean NOT NULL,"
          "lease_expires_at timestamptz NOT NULL)")
     (str "CREATE INDEX IF NOT EXISTS sessions_lease_expires_idx ON "
          sessions " (lease_expires_at)")
     (str "CREATE INDEX IF NOT EXISTS sessions_runner_idx ON "
          sessions " (runner_id)")
     (str "CREATE TABLE IF NOT EXISTS " messages " ("
          "message_id text PRIMARY KEY,"
          "source_route text NOT NULL,"
          "source_runner text NOT NULL,"
          "target_route text NOT NULL,"
          "target_runner text NOT NULL,"
          "content_sha256 char(64) NOT NULL,"
          "status text NOT NULL CHECK (status IN "
          "('queued','in-flight','awaiting-replay','delivered','failed','expired')),"
          "attempts integer NOT NULL DEFAULT 0 CHECK (attempts >= 0),"
          "failure text,"
          "created_at timestamptz NOT NULL,"
          "next_attempt_at timestamptz NOT NULL,"
          "expires_at timestamptz NOT NULL)")
     (str "CREATE INDEX IF NOT EXISTS messages_poll_idx ON " messages
          " (target_runner, status, next_attempt_at, created_at)")
     (str "CREATE INDEX IF NOT EXISTS messages_expires_idx ON " messages
          " (expires_at)")]))

(defn- migration-2-statements [schema]
  [(str "ALTER TABLE " (table schema "sessions")
        " ADD COLUMN IF NOT EXISTS native_url text")])

(defn migration-3-statements
  "Create bounded presentation metadata independently of ephemeral leases."
  [schema]
  [(str "CREATE TABLE IF NOT EXISTS " (table (schema-name schema)
                                                "session_aliases") " ("
        "route_id text PRIMARY KEY,"
        "runner_id text NOT NULL,"
        "alias text NOT NULL CHECK (char_length(alias) BETWEEN 1 AND "
        naming/max-alias-length "),"
        "updated_at timestamptz NOT NULL)")])

(defn migration-4-statements
  "Persist the bounded provider-advertised presentation name on its lease."
  [schema]
  [(str "ALTER TABLE " (table (schema-name schema) "sessions")
        " ADD COLUMN IF NOT EXISTS native_name text"
        " CHECK (native_name IS NULL OR char_length(native_name) BETWEEN 1 AND "
        naming/max-native-name-length ")")])

(defn migration-5-statements
  "Create the narrow forecast-observation stream. Authentication authorizes a
  write but runner identity is intentionally absent from durable rows."
  [schema]
  (let [observations (table (schema-name schema) "usage_observations")]
    [(str "CREATE TABLE IF NOT EXISTS " observations " ("
          "cursor bigserial PRIMARY KEY,"
          "event_id char(64) NOT NULL UNIQUE,"
          "schema_version smallint NOT NULL CHECK (schema_version=1),"
          "observed_at bigint NOT NULL,"
          "agent varchar(64) NOT NULL,"
          "window_key text NOT NULL CHECK (window_key IN ('five_hour','seven_day')),"
          "used_percentage double precision NOT NULL CHECK "
          "(used_percentage >= 0 AND used_percentage <= 100),"
          "resets_at bigint NOT NULL,"
          "received_at timestamptz NOT NULL DEFAULT now())")
     (str "CREATE INDEX IF NOT EXISTS usage_observations_forecast_idx ON "
          observations " (agent,window_key,resets_at,observed_at)")
     (str "CREATE INDEX IF NOT EXISTS usage_observations_observed_idx ON "
          observations " (observed_at)")]))

(defn- timestamp [millis]
  (OffsetDateTime/ofInstant (Instant/ofEpochMilli millis) ZoneOffset/UTC))

(defn- epoch-millis [value]
  (cond
    (nil? value) nil
    (instance? OffsetDateTime value) (.toEpochMilli (.toInstant ^OffsetDateTime value))
    (instance? Instant value) (.toEpochMilli ^Instant value)
    (instance? Timestamp value) (.getTime ^Timestamp value)
    :else (throw (ex-info "Unsupported Postgres timestamp value"
                          {:type :invalid-database-value}))))

(defn- rows [connectable statement]
  (jdbc/execute! connectable statement
                 {:builder-fn rs/as-unqualified-lower-maps}))

(defn- row [connectable statement]
  (jdbc/execute-one! connectable statement
                     {:builder-fn rs/as-unqualified-lower-maps}))

(defn migrate!
  "Apply serialized, transactional schema migrations. The advisory lock is
  scoped to the transaction and prevents concurrent application instances
  from racing DDL."
  [datasource schema]
  (let [schema (schema-name schema)
        migrations (table schema "schema_migrations")]
    (jdbc/with-transaction [tx datasource]
      (jdbc/execute! tx ["SELECT pg_advisory_xact_lock(?)" migration-lock-id])
      (jdbc/execute!
        tx [(str "CREATE TABLE IF NOT EXISTS " migrations
                 " (version integer PRIMARY KEY, applied_at timestamptz NOT NULL DEFAULT now())")])
      (let [applied (set (map :version (rows tx [(str "SELECT version FROM " migrations)])))]
        (when-not (contains? applied 1)
          (doseq [statement (migration-statements schema)]
            (jdbc/execute! tx [statement]))
          (jdbc/execute! tx [(str "INSERT INTO " migrations " (version) VALUES (1)")]))
        (when-not (contains? applied 2)
          (doseq [statement (migration-2-statements schema)]
            (jdbc/execute! tx [statement]))
          (jdbc/execute! tx [(str "INSERT INTO " migrations " (version) VALUES (2)")]))
        (when-not (contains? applied 3)
          (doseq [statement (migration-3-statements schema)]
            (jdbc/execute! tx [statement]))
          (jdbc/execute! tx [(str "INSERT INTO " migrations " (version) VALUES (3)")]))
        (when-not (contains? applied 4)
          (doseq [statement (migration-4-statements schema)]
            (jdbc/execute! tx [statement]))
          (jdbc/execute! tx [(str "INSERT INTO " migrations " (version) VALUES (4)")]))
        (when-not (contains? applied 5)
          (doseq [statement (migration-5-statements schema)]
            (jdbc/execute! tx [statement]))
          (jdbc/execute! tx [(str "INSERT INTO " migrations " (version) VALUES (5)")]))))
    true))

(defn datasource
  "Build a small least-privilege connection pool. Credentials may be supplied
  separately from the JDBC URL and are never retained in broker metadata."
  [{:keys [jdbc-url username password pool-size]
    :or {pool-size default-pool-size}}]
  (when (str/blank? jdbc-url)
    (throw (ex-info "CCH_CONTROL_DATABASE_URL is required"
                    {:type :missing-broker-database})))
  (when-not (str/starts-with? jdbc-url "jdbc:postgresql:")
    (throw (ex-info "CCH_CONTROL_DATABASE_URL must be a PostgreSQL JDBC URL"
                    {:type :invalid-broker-config})))
  (let [pool-size (memory/clamp pool-size 1 8)
        config (doto (HikariConfig.)
                 (.setJdbcUrl jdbc-url)
                 (.setMaximumPoolSize pool-size)
                 (.setMinimumIdle 0)
                 (.setConnectionTimeout 5000)
                 (.setValidationTimeout 3000)
                 (.setPoolName "cch-control-postgres")
                 (.addDataSourceProperty "tcpKeepAlive" "true"))]
    (when-not (str/blank? username) (.setUsername config username))
    (when-not (str/blank? password) (.setPassword config password))
    (HikariDataSource. config)))

(defrecord PostgresBroker
  [datasource runner-tokens bodies now-fn schema options]
  java.io.Closeable
  (close [_] (.close ^HikariDataSource datasource)))

(defn- now [^PostgresBroker broker]
  ((:now-fn broker)))

(defn- transact [datasource f]
  (letfn [(run-attempt [attempt]
            (try
              (jdbc/with-transaction [tx datasource {:isolation :serializable}]
                (f tx))
              (catch SQLException error
                (if (and (< attempt 4)
                         (contains? #{"40001" "40P01"} (.getSQLState error)))
                  (run-attempt (inc attempt))
                  (throw error)))))]
    (run-attempt 1)))

(defn- authorize! [^PostgresBroker broker runner-id token]
  (let [expected (get (:runner-tokens broker) runner-id)]
    (when-not (memory/secure-equal? expected token)
      (throw (ex-info "Runner authentication failed" {:type :unauthorized})))
    true))

(defn- expire-metadata!
  [^PostgresBroker broker tx current-ms]
  (let [messages (table (:schema broker) "messages")
        runners (table (:schema broker) "runners")
        max-attempts (get-in broker [:options :max-attempts])
        retention (get-in broker [:options :metadata-retention-ms])
        expired (rows tx [(str "UPDATE " messages
                               " SET status='expired', failure=NULL"
                               " WHERE status NOT IN ('delivered','failed','expired')"
                               " AND expires_at <= ? RETURNING message_id")
                          (timestamp current-ms)])
        failed (rows tx [(str "UPDATE " messages
                              " SET status='failed', failure='attempts-exhausted'"
                              " WHERE status='in-flight' AND next_attempt_at <= ?"
                              " AND attempts >= ? RETURNING message_id")
                         (timestamp current-ms) max-attempts])
        deleted (rows tx [(str "DELETE FROM " messages
                               " WHERE status IN ('delivered','failed','expired')"
                               " AND created_at < ? RETURNING message_id")
                          (timestamp (- current-ms retention))])]
    (jdbc/execute! tx [(str "DELETE FROM " runners " WHERE lease_expires_at <= ?")
                       (timestamp current-ms)])
    (mapv :message_id (concat expired failed deleted))))

(defn- discard-bodies! [broker message-ids]
  (when (seq message-ids)
    (swap! (:bodies broker) #(apply dissoc % message-ids))))

(defn- row->session
  [{:keys [route_id runner_id agent native_status available native_url
           native_name alias]}]
  (naming/present-session
    (cond-> {:id route_id :runner-id runner_id :agent agent
             :status native_status :available available}
      native_url (assoc :native-url native_url)
      native_name (assoc :name native_name)
      alias (assoc :alias alias))))

(defn- row->metadata
  [{:keys [message_id source_route target_route status attempts created_at
           expires_at failure]}]
  (cond-> {:message-id message_id :source source_route :target target_route
           :status status :attempts attempts
           :created-at (epoch-millis created_at)
           :expires-at (epoch-millis expires_at)}
    failure (assoc :failure failure)))

(defn- register! [^PostgresBroker broker {:keys [runner-id token sessions lease-ms]}]
  (authorize! broker runner-id token)
  (when-not (memory/valid-label? runner-id)
    (throw (ex-info "runner_id is invalid" {:type :invalid-runner})))
  (let [timestamp-value (now broker)
        lease-ms (memory/clamp (or lease-ms (get-in broker [:options :lease-ms]))
                               1000 (* 5 60 1000))
        expires (+ timestamp-value lease-ms)
        sanitized (->> sessions
                       (keep memory/sanitize-session)
                       (filter :available)
                       (map (juxt :id identity))
                       (into {}))
        runners (table (:schema broker) "runners")
        routes (table (:schema broker) "sessions")]
    (locking broker
      (try
        (let [{:keys [value discard]}
              (transact
                (:datasource broker)
                (fn [tx]
                  (let [discard (expire-metadata! broker tx timestamp-value)
                        conflict (some
                                   (fn [route-id]
                                     (some->
                                       (row tx [(str "SELECT route_id FROM " routes
                                                     " WHERE route_id=? AND runner_id<>?"
                                                     " AND lease_expires_at>? LIMIT 1")
                                                route-id runner-id
                                                (timestamp timestamp-value)])
                                       :route_id))
                                   (keys sanitized))]
                    (when conflict
                      (throw (ex-info "Route is already leased by another runner"
                                      {:type :route-conflict :route conflict})))
                    (jdbc/execute!
                      tx [(str "INSERT INTO " runners
                               " (runner_id,updated_at,lease_expires_at) VALUES (?,?,?)"
                               " ON CONFLICT (runner_id) DO UPDATE SET"
                               " updated_at=EXCLUDED.updated_at,"
                               " lease_expires_at=EXCLUDED.lease_expires_at")
                          runner-id (timestamp timestamp-value) (timestamp expires)])
                    (jdbc/execute! tx [(str "DELETE FROM " routes " WHERE runner_id=?")
                                       runner-id])
                    (doseq [[route-id session] sanitized]
                      (jdbc/execute!
                        tx [(str "INSERT INTO " routes
                                 " (route_id,runner_id,agent,native_status,available,native_url,native_name,lease_expires_at)"
                                 " VALUES (?,?,?,?,?,?,?,?)")
                            route-id runner-id (:agent session) (:status session)
                            true (:native-url session) (:name session)
                            (timestamp expires)]))
                    {:discard discard
                     :value {:status "registered" :runner-id runner-id
                             :route-count (count sanitized) :expires-at expires}})))]
          (discard-bodies! broker discard)
          value)
        (catch SQLException error
          (if (= "23505" (.getSQLState error))
            (throw (ex-info "Route is already leased by another runner"
                            {:type :route-conflict} error))
            (throw error)))))))

(defn- sessions [^PostgresBroker broker]
  (let [timestamp-value (now broker)
        routes (table (:schema broker) "sessions")
        aliases (table (:schema broker) "session_aliases")
        {:keys [value discard]}
        (transact
          (:datasource broker)
          (fn [tx]
            {:discard (expire-metadata! broker tx timestamp-value)
             :value (mapv row->session
                          (rows tx [(str "SELECT r.route_id,r.runner_id,r.agent,"
                                         "r.native_status,r.available,r.native_url,"
                                         "r.native_name,a.alias FROM "
                                         routes " r LEFT JOIN " aliases
                                         " a ON a.route_id=r.route_id"
                                         " AND a.runner_id=r.runner_id"
                                         " WHERE r.lease_expires_at>? ORDER BY r.route_id")
                                    (timestamp timestamp-value)]))}))]
    (discard-bodies! broker discard)
    value))

(defn- set-alias-as!
  [^PostgresBroker broker
   {:keys [route-id alias runner-id require-owned-route?]}]
  (let [alias (naming/normalize-alias alias)
        timestamp-value (now broker)
        routes (table (:schema broker) "sessions")
        aliases (table (:schema broker) "session_aliases")]
    (transact
      (:datasource broker)
      (fn [tx]
        (let [route (row tx [(str "SELECT route_id,runner_id,agent,native_status,"
                                  "available,native_url,native_name FROM " routes
                                  " WHERE route_id=? AND lease_expires_at>?")
                             route-id (timestamp timestamp-value)])]
          (when-not route
            (throw (ex-info "Session is not currently available"
                            {:type :unknown-session :target route-id})))
          (when (and require-owned-route?
                     (not= runner-id (:runner_id route)))
            (throw (ex-info "Session is not leased by this runner"
                            {:type :forbidden})))
          (if alias
            (jdbc/execute!
              tx [(str "INSERT INTO " aliases
                       " (route_id,runner_id,alias,updated_at) VALUES (?,?,?,?)"
                       " ON CONFLICT (route_id) DO UPDATE SET"
                       " runner_id=EXCLUDED.runner_id,alias=EXCLUDED.alias,"
                       "updated_at=EXCLUDED.updated_at")
                  route-id (:runner_id route) alias
                  (timestamp timestamp-value)])
            (jdbc/execute! tx [(str "DELETE FROM " aliases " WHERE route_id=?")
                               route-id]))
          (row->session (cond-> route alias (assoc :alias alias))))))))

(defn- set-session-alias!
  [^PostgresBroker broker {:keys [runner-id token] :as request}]
  (authorize! broker runner-id token)
  (set-alias-as! broker (assoc request
                               :runner-id runner-id
                               :require-owned-route? true)))

(defn- set-operator-session-alias! [^PostgresBroker broker request]
  (set-alias-as! broker (assoc request :require-owned-route? false)))

(defn- validate-envelope!
  [{:keys [source target message message-id]}]
  (doseq [[field value] [[:source source] [:target target] [:message-id message-id]]]
    (when-not (memory/valid-label? value)
      (throw (ex-info (str (name field) " is invalid")
                      {:type :invalid-message :field field}))))
  (when (str/blank? message)
    (throw (ex-info "message is required" {:type :invalid-message})))
  (when (str/starts-with? (str/triml message) "/")
    (throw (ex-info "command-mode input is not allowed"
                    {:type :command-mode-not-allowed})))
  (when (> (alength (.getBytes ^String message StandardCharsets/UTF_8))
           memory/max-message-bytes)
    (throw (ex-info "message is too large"
                    {:type :message-too-large
                     :max-bytes memory/max-message-bytes}))))

(defn- enqueue-as!
  [^PostgresBroker broker
   {:keys [source target message message-id ttl-ms source-runner
           require-owned-source?] :as request}]
  (validate-envelope! request)
  (let [timestamp-value (now broker)
        digest (store/content-digest message)
        ttl-ms (memory/clamp (or ttl-ms (get-in broker [:options :message-ttl-ms]))
                             1000 (get-in broker [:options :message-ttl-ms]))
        expires (+ timestamp-value ttl-ms)
        routes (table (:schema broker) "sessions")
        messages (table (:schema broker) "messages")]
    (locking broker
      (let [{:keys [value discard store-body?]}
            (transact
              (:datasource broker)
              (fn [tx]
                (let [discard (expire-metadata! broker tx timestamp-value)
                      source-owner (some->
                                     (row tx [(str "SELECT runner_id FROM " routes
                                                   " WHERE route_id=? AND lease_expires_at>?")
                                              source (timestamp timestamp-value)])
                                     :runner_id)
                      target-owner (some->
                                     (row tx [(str "SELECT runner_id FROM " routes
                                                   " WHERE route_id=? AND lease_expires_at>?")
                                              target (timestamp timestamp-value)])
                                     :runner_id)
                      prior (row tx [(str "SELECT * FROM " messages
                                          " WHERE message_id=? FOR UPDATE")
                                     message-id])]
                  (when (and require-owned-source?
                             (not (or (= "operator" source)
                                      (= source-runner source-owner))))
                    (throw (ex-info "Source route is not leased by this runner"
                                    {:type :invalid-source :source source})))
                  (when-not target-owner
                    (throw (ex-info "Target route is not currently available"
                                    {:type :unknown-session :target target})))
                  (if prior
                    (if (and (= source (:source_route prior))
                             (= target (:target_route prior))
                             (= digest (:content_sha256 prior)))
                      (let [rehydrate? (and (not (memory/terminal-status? (:status prior)))
                                            (nil? (get @(:bodies broker) message-id)))]
                        (when rehydrate?
                          (jdbc/execute!
                            tx [(str "UPDATE " messages
                                     " SET status='queued', next_attempt_at=?"
                                     " WHERE message_id=?")
                                (timestamp timestamp-value) message-id]))
                        {:discard discard :store-body? rehydrate?
                         :value {:message-id message-id :source source :target target
                                 :status "duplicate"
                                 :original-status (:status prior)
                                 :replayed rehydrate?}})
                      (throw
                        (ex-info "message_id was already used with different content or routing"
                                 {:type :message-id-conflict :message-id message-id})))
                    (do
                      (jdbc/execute!
                        tx [(str "INSERT INTO " messages
                                 " (message_id,source_route,source_runner,target_route,target_runner,"
                                 "content_sha256,status,attempts,created_at,next_attempt_at,expires_at)"
                                 " VALUES (?,?,?,?,?,?,'queued',0,?,?,?)")
                            message-id source source-runner target target-owner digest
                            (timestamp timestamp-value) (timestamp timestamp-value)
                            (timestamp expires)])
                      {:discard discard :store-body? true
                       :value {:message-id message-id :source source :target target
                               :transport "broker-postgres" :status "queued"}})))))]
        (discard-bodies! broker discard)
        (when store-body?
          (swap! (:bodies broker) assoc message-id message))
        value))))

(defn- enqueue!
  [^PostgresBroker broker {:keys [runner-id token] :as request}]
  (authorize! broker runner-id token)
  (enqueue-as! broker (assoc request
                             :source-runner runner-id
                             :require-owned-source? true)))

(defn- enqueue-operator!
  [^PostgresBroker broker request]
  (enqueue-as! broker (assoc request
                             :source "operator"
                             :source-runner "operator"
                             :require-owned-source? false)))

(defn- poll!
  [^PostgresBroker broker {:keys [runner-id token limit]}]
  (authorize! broker runner-id token)
  (let [timestamp-value (now broker)
        limit (memory/clamp (or limit 20) 1 100)
        runners (table (:schema broker) "runners")
        messages (table (:schema broker) "messages")]
    (locking broker
      (let [{:keys [value discard]}
            (transact
              (:datasource broker)
              (fn [tx]
                (let [discard (expire-metadata! broker tx timestamp-value)
                      runner (row tx [(str "SELECT runner_id FROM " runners
                                           " WHERE runner_id=? AND lease_expires_at>?")
                                      runner-id (timestamp timestamp-value)])]
                  (when-not runner
                    (throw (ex-info "Runner lease is absent or expired"
                                    {:type :runner-not-registered})))
                  (let [candidates
                        (rows tx [(str "SELECT message_id,source_route,target_route,attempts,expires_at"
                                       " FROM " messages
                                       " WHERE target_runner=? AND status IN ('queued','in-flight')"
                                       " AND next_attempt_at<=? AND attempts<?"
                                       " ORDER BY created_at,message_id FOR UPDATE SKIP LOCKED LIMIT 100")
                                  runner-id (timestamp timestamp-value)
                                  (get-in broker [:options :max-attempts])])
                        [present missing]
                        ((juxt #(filter (fn [candidate]
                                         (contains? @(:bodies broker)
                                                    (:message_id candidate))) %)
                               #(remove (fn [candidate]
                                          (contains? @(:bodies broker)
                                                     (:message_id candidate))) %))
                         candidates)
                        leased (vec (take limit present))]
                    (doseq [{:keys [message_id]} missing]
                      (jdbc/execute!
                        tx [(str "UPDATE " messages
                                 " SET status='awaiting-replay' WHERE message_id=?")
                            message_id]))
                    (doseq [{:keys [message_id]} leased]
                      (jdbc/execute!
                        tx [(str "UPDATE " messages
                                 " SET status='in-flight', attempts=attempts+1,"
                                 " next_attempt_at=? WHERE message_id=?")
                            (timestamp (+ timestamp-value
                                          (get-in broker [:options :ack-timeout-ms])))
                            message_id]))
                    {:discard discard
                     :value {:messages
                             (mapv
                               (fn [{:keys [message_id source_route target_route
                                            attempts expires_at]}]
                                 {:message-id message_id :source source_route
                                  :target target_route
                                  :body (get @(:bodies broker) message_id)
                                  :attempts (inc attempts)
                                  :expires-at (epoch-millis expires_at)})
                               leased)}}))))]
        (discard-bodies! broker discard)
        value))))

(defn- ack!
  [^PostgresBroker broker {:keys [runner-id token message-id status failure]}]
  (authorize! broker runner-id token)
  (when-not (contains? #{"delivered" "failed"} status)
    (throw (ex-info "ack status must be delivered or failed"
                    {:type :invalid-ack})))
  (let [messages (table (:schema broker) "messages")
        timestamp-value (now broker)]
    (locking broker
      (let [{:keys [value discard]}
            (transact
              (:datasource broker)
              (fn [tx]
                (let [discard (expire-metadata! broker tx timestamp-value)
                      message (row tx [(str "SELECT * FROM " messages
                                            " WHERE message_id=? FOR UPDATE")
                                       message-id])]
                  (when-not message
                    (throw (ex-info "Unknown message" {:type :unknown-message})))
                  (when-not (= runner-id (:target_runner message))
                    (throw (ex-info "Message belongs to another runner"
                                    {:type :forbidden})))
                  (if (memory/terminal-status? (:status message))
                    {:discard discard
                     :value {:message-id message-id :status (:status message)
                             :duplicate true}}
                    (do
                      (jdbc/execute!
                        tx [(str "UPDATE " messages
                                 " SET status=?, failure=? WHERE message_id=?")
                            status (when (and (= "failed" status) failure)
                                     (name failure))
                            message-id])
                      {:discard (conj discard message-id)
                       :value {:message-id message-id :status status}})))))]
        (discard-bodies! broker discard)
        value))))

(defn- message-status
  [^PostgresBroker broker runner-id token message-id]
  (authorize! broker runner-id token)
  (let [messages (table (:schema broker) "messages")
        timestamp-value (now broker)
        {:keys [value discard]}
        (transact
          (:datasource broker)
          (fn [tx]
            (let [discard (expire-metadata! broker tx timestamp-value)
                  message (row tx [(str "SELECT * FROM " messages
                                        " WHERE message_id=?") message-id])]
              (when (and message
                         (not (or (= runner-id (:target_runner message))
                                  (= runner-id (:source_runner message)))))
                (throw (ex-info "Message belongs to another runner"
                                {:type :forbidden})))
              {:discard discard
               :value (some-> message row->metadata)})))]
    (discard-bodies! broker discard)
    value))

(defn- operator-message-status
  [^PostgresBroker broker message-id]
  (let [messages (table (:schema broker) "messages")
        timestamp-value (now broker)
        {:keys [value discard]}
        (transact
          (:datasource broker)
          (fn [tx]
            {:discard (expire-metadata! broker tx timestamp-value)
             :value (some->
                      (row tx [(str "SELECT * FROM " messages
                                    " WHERE message_id=? AND source_route='operator'"
                                    " AND source_runner='operator'")
                               message-id])
                      row->metadata)}))]
    (discard-bodies! broker discard)
    value))

(defn- prune-usage!
  [^PostgresBroker broker tx timestamp-value]
  (let [observations (table (:schema broker) "usage_observations")
        oldest (- timestamp-value (get-in broker [:options :usage-retention-ms]))
        max-count (get-in broker [:options :max-usage-observations])]
    (jdbc/execute! tx [(str "DELETE FROM " observations " WHERE observed_at < ?")
                       oldest])
    (jdbc/execute!
      tx [(str "DELETE FROM " observations " WHERE cursor IN ("
               "SELECT cursor FROM " observations
               " ORDER BY cursor DESC OFFSET ?)")
          max-count])))

(defn- row->usage-observation
  [{:keys [cursor event_id schema_version observed_at agent window_key
           used_percentage resets_at]}]
  {:cursor cursor
   :event-id (str/trim event_id)
   :schema-version schema_version
   :observed-at observed_at
   :agent agent
   :window window_key
   :used-percentage used_percentage
   :resets-at resets_at})

(defn- publish-usage!
  [^PostgresBroker broker {:keys [runner-id token observations]}]
  (authorize! broker runner-id token)
  (let [timestamp-value (now broker)
        observations (memory/validate-usage-batch!
                       observations timestamp-value
                       (get-in broker [:options :usage-retention-ms])
                       (get-in broker [:options :usage-future-skew-ms]))
        table-name (table (:schema broker) "usage_observations")]
    (transact
      (:datasource broker)
      (fn [tx]
        (prune-usage! broker tx timestamp-value)
        (let [accepted
              (reduce
                (fn [accepted-count observation]
                  (+ accepted-count
                     (count
                       (rows
                         tx
                         [(str "INSERT INTO " table-name
                               " (event_id,schema_version,observed_at,agent,window_key,"
                               "used_percentage,resets_at) VALUES (?,?,?,?,?,?,?)"
                               " ON CONFLICT (event_id) DO NOTHING RETURNING cursor")
                          (:event-id observation)
                          (:schema-version observation)
                          (:observed-at observation)
                          (:agent observation)
                          (:window observation)
                          (:used-percentage observation)
                          (:resets-at observation)]))))
                0
                observations)]
          (prune-usage! broker tx timestamp-value)
          {:accepted accepted
           :duplicates (- (count observations) accepted)
           :latest-cursor (or (:cursor
                                (row tx [(str "SELECT max(cursor) AS cursor FROM "
                                              table-name)]))
                              0)})))))

(defn- read-usage!
  [^PostgresBroker broker {:keys [runner-id token after-cursor limit]}]
  (authorize! broker runner-id token)
  (let [after-cursor (or after-cursor 0)
        limit (or limit 500)]
    (when-not (and (integer? after-cursor) (<= 0 after-cursor))
      (throw (ex-info "after_cursor must be a non-negative integer"
                      {:type :invalid-usage-cursor})))
    (when-not (and (integer? limit)
                   (<= 1 limit memory/max-usage-read-limit))
      (throw (ex-info "limit is invalid" {:type :invalid-usage-cursor})))
    (let [observations-table (table (:schema broker) "usage_observations")
          observations
          (transact
            (:datasource broker)
            (fn [tx]
              (prune-usage! broker tx (now broker))
              (mapv row->usage-observation
                    (rows tx [(str "SELECT cursor,event_id,schema_version,"
                                   "observed_at,agent,window_key,used_percentage,resets_at "
                                   "FROM " observations-table
                                   " WHERE cursor>? ORDER BY cursor LIMIT ?")
                              after-cursor limit]))))]
      {:observations observations
       :next-cursor (or (:cursor (peek observations)) after-cursor)})))

(defn- summary [^PostgresBroker broker]
  (let [timestamp-value (now broker)
        runners (table (:schema broker) "runners")
        routes (table (:schema broker) "sessions")
        messages (table (:schema broker) "messages")
        {:keys [value discard]}
        (transact
          (:datasource broker)
          (fn [tx]
            {:discard (expire-metadata! broker tx timestamp-value)
             :value {:status "ok" :storage "postgres"
                     :runner-count (:count (row tx [(str "SELECT count(*) FROM " runners)]))
                     :route-count (:count (row tx [(str "SELECT count(*) FROM " routes)]))
                     :message-count (:count (row tx [(str "SELECT count(*) FROM " messages)]))}}))]
    (discard-bodies! broker discard)
    value))

(defn new-broker
  "Create and migrate a Postgres broker. The caller owns the returned pool and
  must close the broker during shutdown."
  ([runner-tokens database]
   (new-broker runner-tokens database {}))
  ([runner-tokens database
    {:keys [now-fn lease-ms message-ttl-ms ack-timeout-ms max-attempts
            metadata-retention-ms usage-retention-ms usage-future-skew-ms
            max-usage-observations schema]
     :or {now-fn #(System/currentTimeMillis)
          lease-ms memory/default-lease-ms
          message-ttl-ms memory/default-message-ttl-ms
          ack-timeout-ms memory/default-ack-timeout-ms
          max-attempts memory/default-max-attempts
          metadata-retention-ms default-metadata-retention-ms
          usage-retention-ms memory/default-usage-retention-ms
          usage-future-skew-ms memory/default-usage-future-skew-ms
          max-usage-observations memory/default-max-usage-observations}}]
   (let [schema (schema-name (or schema (:schema database)))
         datasource (datasource database)]
     (try
       (migrate! datasource schema)
       ;; Bodies do not survive a process restart. Durable nonterminal rows wait
       ;; for an identical source retry to rehydrate them.
       (jdbc/execute!
         datasource
         [(str "UPDATE " (table schema "messages")
               " SET status='awaiting-replay'"
               " WHERE status IN ('queued','in-flight')")])
       (->PostgresBroker datasource runner-tokens (atom {}) now-fn schema
                         {:lease-ms lease-ms
                          :message-ttl-ms message-ttl-ms
                          :ack-timeout-ms ack-timeout-ms
                          :max-attempts max-attempts
                          :metadata-retention-ms metadata-retention-ms
                          :usage-retention-ms usage-retention-ms
                          :usage-future-skew-ms usage-future-skew-ms
                          :max-usage-observations max-usage-observations})
       (catch Exception error
         (.close ^HikariDataSource datasource)
         (throw error))))))

(defn database-config-from-env
  "Return nil for explicit in-memory mode or a sanitized Postgres connection
  config. Values are consumed only by the JDBC pool and never printed."
  ([] (database-config-from-env (System/getenv)))
  ([env]
   (when-let [jdbc-url (not-empty (get env "CCH_CONTROL_DATABASE_URL"))]
     (let [pool-raw (not-empty (get env "CCH_CONTROL_DATABASE_POOL_SIZE"))
           pool-size (if pool-raw
                       (or (parse-long pool-raw)
                           (throw (ex-info "Database pool size must be an integer"
                                           {:type :invalid-broker-config})))
                       default-pool-size)]
       {:jdbc-url jdbc-url
        :username (get env "CCH_CONTROL_DATABASE_USER")
        :password (get env "CCH_CONTROL_DATABASE_PASSWORD")
        :schema (schema-name (get env "CCH_CONTROL_DATABASE_SCHEMA"))
        :pool-size pool-size}))))

(extend-type PostgresBroker
  api/ControlBroker
  (authorize-runner! [broker runner-id token]
    (authorize! broker runner-id token))
  (register-runner! [broker request]
    (register! broker request))
  (active-sessions [broker]
    (sessions broker))
  (set-session-alias! [broker request]
    (set-session-alias! broker request))
  (set-operator-session-alias! [broker request]
    (set-operator-session-alias! broker request))
  (enqueue-message! [broker request]
    (enqueue! broker request))
  (enqueue-operator-message! [broker request]
    (enqueue-operator! broker request))
  (poll-messages! [broker request]
    (poll! broker request))
  (ack-message! [broker request]
    (ack! broker request))
  (publish-usage-observations! [broker request]
    (publish-usage! broker request))
  (read-usage-observations! [broker request]
    (read-usage! broker request))
  (message-metadata [broker runner-id token message-id]
    (message-status broker runner-id token message-id))
  (operator-message-metadata [broker message-id]
    (operator-message-status broker message-id))
  (broker-summary [broker]
    (summary broker))
  (close-broker! [broker]
    (.close ^java.io.Closeable broker)))
