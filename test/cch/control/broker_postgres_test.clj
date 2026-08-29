(ns cch.control.broker-postgres-test
  (:require [cch.control.broker-api :as api]
            [cch.control.broker-postgres :as postgres]
            [cch.control.broker :as memory]
            [cch.usage-observation :as usage]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]))

(def runner-tokens
  {"runner-a" "synthetic-token-a"
   "runner-b" "synthetic-token-b"})

(deftest migration-schema-contains-metadata-but-no-hosted-message-content
  (let [ddl (str/join "\n" (concat
                              (postgres/migration-statements "cch_control")
                              (postgres/migration-3-statements "cch_control")
                              (postgres/migration-4-statements "cch_control")
                              (postgres/migration-5-statements "cch_control")
                              (postgres/migration-6-statements "cch_control")))]
    (is (str/includes? ddl "content_sha256"))
    (is (str/includes? ddl "lease_expires_at"))
    (is (str/includes? ddl "awaiting-replay"))
    (is (str/includes? ddl "session_aliases"))
    (is (str/includes? ddl "native_name"))
    (is (str/includes? ddl "usage_observations"))
    (is (str/includes? ddl "usage_observations_latest_idx"))
    (is (str/includes? ddl "used_percentage"))
    (is (not (re-find #"(?i)\b(session_id|runner_id|account|hostname|path|payload)\b"
                      (str/join "\n" (postgres/migration-5-statements
                                        "cch_control"))))
        "normalized usage rows contain no source or provider identity")
    (is (not (re-find #"(?i)\\b(body|token|credential|transcript)\\b" ddl))
        "provider credentials, transcripts, and message bodies have no columns")))

(deftest database-environment-is-explicit-and-pool-is-small
  (is (nil? (postgres/database-config-from-env {})))
  (is (= {:jdbc-url "jdbc:postgresql://127.0.0.1/control_test"
          :username "control_app"
          :password "synthetic-password"
          :schema "isolated_control"
          :pool-size 3}
         (postgres/database-config-from-env
           {"CCH_CONTROL_DATABASE_URL" "jdbc:postgresql://127.0.0.1/control_test"
            "CCH_CONTROL_DATABASE_USER" "control_app"
            "CCH_CONTROL_DATABASE_PASSWORD" "synthetic-password"
            "CCH_CONTROL_DATABASE_SCHEMA" "isolated_control"
            "CCH_CONTROL_DATABASE_POOL_SIZE" "3"})))
  (is (= :invalid-broker-config
         (try
           (postgres/migration-statements "unsafe;drop schema public")
           (catch clojure.lang.ExceptionInfo error
             (:type (ex-data error))))))
  (is (= :invalid-broker-config
         (try
           (postgres/database-config-from-env
             {"CCH_CONTROL_DATABASE_URL" "jdbc:postgresql://127.0.0.1/test"
              "CCH_CONTROL_DATABASE_POOL_SIZE" "many"})
           (catch clojure.lang.ExceptionInfo error
             (:type (ex-data error)))))))

(defn- integration-url []
  (System/getenv "CCH_TEST_POSTGRES_URL"))

(deftest postgres-publish-execution-counts-inserts-without-a-live-database
  (let [now 2000000000000
        observation
        (first
          (usage/from-snapshot
            {:agent "codex"
             :observed-at now
             :payload {:rate_limits
                       {:five_hour {:used_percentage 4
                                    :resets_at 2000003600}}}}))
        b (postgres/->PostgresBroker
            nil runner-tokens (atom {}) (constantly now) "cch_control"
            {:usage-retention-ms memory/default-usage-retention-ms
             :usage-future-skew-ms memory/default-usage-future-skew-ms
             :max-usage-observations memory/default-max-usage-observations})
        transact-var (ns-resolve 'cch.control.broker-postgres 'transact)
        prune-var (ns-resolve 'cch.control.broker-postgres 'prune-usage!)
        rows-var (ns-resolve 'cch.control.broker-postgres 'rows)
        row-var (ns-resolve 'cch.control.broker-postgres 'row)
        publish-var (ns-resolve 'cch.control.broker-postgres 'publish-usage!)]
    (with-redefs-fn
      {transact-var (fn [_ run] (run :synthetic-transaction))
       prune-var (fn [& _] nil)
       rows-var (fn [_ _] [{:cursor 7}])
       row-var (fn [_ _] {:cursor 7})}
      (fn []
        (is (= {:accepted 1 :duplicates 0 :latest-cursor 7}
               (publish-var
                 b {:runner-id "runner-a" :token "synthetic-token-a"
                    :observations [observation]})))))))

(deftest postgres-usage-read-model-is-bounded-without-a-live-database
  (let [now 2000000000000
        reset (quot (+ now 3600000) 1000)
        b (postgres/->PostgresBroker
            nil runner-tokens (atom {}) (constantly now) "cch_control"
            {:usage-retention-ms memory/default-usage-retention-ms
             :usage-future-skew-ms memory/default-usage-future-skew-ms
             :max-usage-observations memory/default-max-usage-observations})
        transact-var (ns-resolve 'cch.control.broker-postgres 'transact)
        prune-var (ns-resolve 'cch.control.broker-postgres 'prune-usage!)
        rows-var (ns-resolve 'cch.control.broker-postgres 'rows)
        inputs-var (ns-resolve 'cch.control.broker-postgres 'usage-inputs)
        statements (atom [])]
    (with-redefs-fn
      {transact-var (fn [_ run] (run :synthetic-transaction))
       prune-var (fn [& _] nil)
       rows-var
       (fn [_ [sql & _]]
         (swap! statements conj sql)
         (cond
           (str/includes? sql "DISTINCT ON")
           [{:agent "claude-code" :window_key "five_hour"
             :resets_at reset}]

           (str/includes? sql "WITH source")
           [{:observed_at (- now 60000) :used_percentage 12.0
             :sample_count 2}
            {:observed_at now :used_percentage 14.0 :sample_count 2}]

           (str/includes? sql "SELECT final_pct")
           [{:final_pct 81.0} {:final_pct 76.0}]

           :else []))}
      (fn []
        (let [model (inputs-var b)
              input (get-in model [:agents "claude-code" "five_hour"])]
          (is (= 2 (:sample-count input)))
          (is (= [12.0 14.0] (mapv :used-percentage (:samples input))))
          (is (= [81.0 76.0] (:historical-finals input)))
          (is (some #(str/includes? % "row_number() OVER") @statements)
              "samples are time-bucketed in Postgres, not loaded wholesale"))))))

(deftest postgres-directory-reconnect-replay-and-migrations
  (if (str/blank? (integration-url))
    (is true "Set CCH_TEST_POSTGRES_URL to exercise the real Postgres integration")
    (let [schema (str "cch_test_" (str/replace (str (random-uuid)) "-" ""))
          database {:jdbc-url (integration-url) :pool-size 2 :schema schema}
          admin (postgres/datasource {:jdbc-url (integration-url) :pool-size 1})
          clock (atom 100000)
          source "codex:00000000-0000-0000-0000-00000000000a"
          target "claude:00000000-0000-0000-0000-00000000000b"
          brokers (atom [])]
      (try
        (jdbc/execute! admin [(str "CREATE SCHEMA \"" schema "\"")])
        (let [b1 (postgres/new-broker runner-tokens database
                                      {:now-fn #(deref clock)
                                       :schema schema
                                       :lease-ms 5000
                                       :message-ttl-ms 5000
                                       :ack-timeout-ms 100
                                       :max-attempts 3})]
          (swap! brokers conj b1)
          (api/register-runner!
            b1 {:runner-id "runner-a" :token "synthetic-token-a"
                :sessions [{:id source :agent "codex" :status "idle"
                            :available true :cwd "/never-federated"
                            :name "Native panel"}]})
          (api/register-runner!
            b1 {:runner-id "runner-b" :token "synthetic-token-b"
                :sessions [{:id target :agent "claude" :status "working"
                            :available true
                            :native-url "https://claude.ai/code/session_synthetic123"}]})
          (is (= #{source target} (set (map :id (api/active-sessions b1)))))
          (is (= "https://claude.ai/code/session_synthetic123"
                 (:native-url (some #(when (= target (:id %)) %)
                                    (api/active-sessions b1)))))
          (is (every? #(nil? (:cwd %)) (api/active-sessions b1)))
          (is (= "Native panel"
                 (:name (some #(when (= source (:id %)) %)
                              (api/active-sessions b1)))))
          (is (= "Build pair"
                 (:alias
                   (api/set-session-alias!
                     b1 {:runner-id "runner-a" :token "synthetic-token-a"
                         :route-id source :alias "Build pair"}))))
          (is (= :forbidden
                 (try
                   (api/set-session-alias!
                     b1 {:runner-id "runner-b" :token "synthetic-token-b"
                         :route-id source :alias "Forged"})
                   (catch clojure.lang.ExceptionInfo error
                     (:type (ex-data error))))))
          (api/set-operator-session-alias!
            b1 {:route-id target :alias "Review pair"})

          (is (= "queued"
                 (:status
                   (api/enqueue-message!
                     b1 {:runner-id "runner-a" :token "synthetic-token-a"
                         :source source :target target
                         :message "Synthetic durable-metadata ping"
                         :message-id "postgres-message-1"}))))
          (is (= "Synthetic durable-metadata ping"
                 (-> (api/poll-messages!
                       b1 {:runner-id "runner-b" :token "synthetic-token-b"})
                     :messages first :body)))
          (is (= "delivered"
                 (:status
                   (api/ack-message!
                     b1 {:runner-id "runner-b" :token "synthetic-token-b"
                         :message-id "postgres-message-1" :status "delivered"}))))

          (is (= "queued"
                 (:status
                   (api/enqueue-operator-message!
                     b1 {:target target :message "Synthetic operator ping"
                         :message-id "postgres-operator-message"}))))
          (is (= "operator"
                 (-> (api/poll-messages!
                       b1 {:runner-id "runner-b" :token "synthetic-token-b"})
                     :messages first :source)))
          (api/ack-message!
            b1 {:runner-id "runner-b" :token "synthetic-token-b"
                :message-id "postgres-operator-message" :status "delivered"})
          (is (= "delivered"
                 (:status (api/operator-message-metadata
                            b1 "postgres-operator-message"))))
          (is (nil? (api/operator-message-metadata b1 "postgres-message-1")))

          ;; Leave a second body queued, then restart the broker. Only its
          ;; digest/route metadata survives; an identical retry rehydrates it.
          (api/enqueue-message!
            b1 {:runner-id "runner-a" :token "synthetic-token-a"
                :source source :target target :message "Replay after restart"
                :message-id "postgres-message-replay"})
          (api/close-broker! b1)
          (swap! brokers pop)

          (let [b2 (postgres/new-broker runner-tokens database
                                        {:now-fn #(deref clock)
                                         :schema schema
                                         :lease-ms 5000
                                         :message-ttl-ms 5000
                                         :ack-timeout-ms 100
                                         :max-attempts 3})]
            (swap! brokers conj b2)
            (is (= {source "Build pair" target "Review pair"}
                   (into {} (map (juxt :id :alias)
                                 (api/active-sessions b2)))))
            (is (empty? (:messages
                          (api/poll-messages!
                            b2 {:runner-id "runner-b"
                                :token "synthetic-token-b"}))))
            (is (true? (:replayed
                         (api/enqueue-message!
                           b2 {:runner-id "runner-a" :token "synthetic-token-a"
                               :source source :target target
                               :message "Replay after restart"
                               :message-id "postgres-message-replay"}))))
            (is (= "Replay after restart"
                   (-> (api/poll-messages!
                         b2 {:runner-id "runner-b" :token "synthetic-token-b"})
                       :messages first :body)))
            (is (= :message-id-conflict
                   (try
                     (api/enqueue-message!
                       b2 {:runner-id "runner-a" :token "synthetic-token-a"
                           :source source :target target :message "Changed"
                           :message-id "postgres-message-replay"})
                     (catch clojure.lang.ExceptionInfo error
                       (:type (ex-data error))))))

            (testing "unacknowledged delivery and route leases are bounded"
              (swap! clock + 100)
              (is (= 2 (-> (api/poll-messages!
                              b2 {:runner-id "runner-b"
                                  :token "synthetic-token-b"})
                            :messages first :attempts)))
              (swap! clock + 100)
              (is (= 3 (-> (api/poll-messages!
                              b2 {:runner-id "runner-b"
                                  :token "synthetic-token-b"})
                            :messages first :attempts)))
              (swap! clock + 100)
              (is (empty? (:messages
                            (api/poll-messages!
                              b2 {:runner-id "runner-b"
                                  :token "synthetic-token-b"}))))
              (is (= "failed"
                     (:status
                       (api/message-metadata
                         b2 "runner-a" "synthetic-token-a"
                         "postgres-message-replay"))))
              (swap! clock + 5001)
              (is (empty? (api/active-sessions b2)))
              (is (= "registered"
                     (:status
                       (api/register-runner!
                         b2 {:runner-id "runner-b" :token "synthetic-token-b"
                             :sessions [{:id target :agent "claude"
                                         :status "working" :available true}]}))))
              (is (= [target] (mapv :id (api/active-sessions b2))))
              (is (= "Review pair" (:alias (first (api/active-sessions b2)))))))

          (testing "schema and migrations serialize without content columns"
            (let [results (doall (map deref
                                      (repeatedly 4
                                                  #(future
                                                     (postgres/migrate! admin schema)))))
                  columns (jdbc/execute!
                            admin
                            ["SELECT column_name FROM information_schema.columns WHERE table_schema=? AND table_name='messages' ORDER BY column_name"
                             schema]
                            {:builder-fn rs/as-unqualified-lower-maps})
                  versions (jdbc/execute-one!
                             admin
                             [(str "SELECT count(*) AS count FROM \"" schema
                                   "\".schema_migrations")]
                             {:builder-fn rs/as-unqualified-lower-maps})]
              (is (every? true? results))
              (is (= 6 (:count versions)))
              (is (not-any? #{"body" "token" "credential" "transcript"}
                            (map :column_name columns))))))
        (finally
          (doseq [broker @brokers]
            (api/close-broker! broker))
          (jdbc/execute! admin [(str "DROP SCHEMA IF EXISTS \"" schema
                                    "\" CASCADE")])
          (.close ^java.io.Closeable admin))))))
