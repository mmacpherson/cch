(ns cch.control.broker-postgres-test
  (:require [cch.control.broker-api :as api]
            [cch.control.broker-postgres :as postgres]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]))

(def runner-tokens
  {"runner-a" "synthetic-token-a"
   "runner-b" "synthetic-token-b"})

(deftest migration-schema-contains-metadata-but-no-hosted-message-content
  (let [ddl (str/join "\n" (postgres/migration-statements "cch_control"))]
    (is (str/includes? ddl "content_sha256"))
    (is (str/includes? ddl "lease_expires_at"))
    (is (str/includes? ddl "awaiting-replay"))
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
                            :available true :cwd "/never-federated"}]})
          (api/register-runner!
            b1 {:runner-id "runner-b" :token "synthetic-token-b"
                :sessions [{:id target :agent "claude" :status "working"
                            :available true}]})
          (is (= #{source target} (set (map :id (api/active-sessions b1)))))
          (is (every? #(nil? (:cwd %)) (api/active-sessions b1)))

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
              (is (= [target] (mapv :id (api/active-sessions b2))))))

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
              (is (= 1 (:count versions)))
              (is (not-any? #{"body" "token" "credential" "transcript"}
                            (map :column_name columns))))))
        (finally
          (doseq [broker @brokers]
            (api/close-broker! broker))
          (jdbc/execute! admin [(str "DROP SCHEMA IF EXISTS \"" schema
                                    "\" CASCADE")])
          (.close ^java.io.Closeable admin))))))
