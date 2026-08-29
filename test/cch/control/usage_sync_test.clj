(ns cch.control.usage-sync-test
  (:require [babashka.fs :as fs]
            [cch.control.broker :as broker]
            [cch.control.broker-http :as broker-http]
            [cch.control.remote :as remote]
            [cch.control.usage-sync :as usage-sync]
            [cch.log :as log]
            [cch.usage-observation :as usage]
            [cheshire.core]
            [clojure.test :refer [deftest is testing]]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs])
  (:import [java.net ServerSocket]))

(defn- free-port []
  (with-open [socket (ServerSocket. 0)]
    (.getLocalPort socket)))

(defn- observation [agent observed-at window pct resets-at]
  (first
    (usage/from-snapshot
      {:agent agent
       :observed-at observed-at
       :payload {:rate_limits
                 {(keyword window) {:used_percentage pct
                                    :resets_at resets-at}}}})))

(defn- insert-local! [path observation]
  (log/ensure-db! path)
  (jdbc/execute!
    {:dbtype "sqlite" :dbname path}
    [(str "INSERT OR IGNORE INTO usage_observations "
          "(event_id,schema_version,observed_at,agent,window_key,"
          "used_percentage,resets_at,publishable) VALUES (?,?,?,?,?,?,?,1)")
     (:event-id observation)
     (:schema-version observation)
     (:observed-at observation)
     (:agent observation)
     (:window observation)
     (:used-percentage observation)
     (:resets-at observation)]))

(defn- local-rows [path]
  (jdbc/execute!
    {:dbtype "sqlite" :dbname path}
    [(str "SELECT event_id,agent,window_key,publishable "
          "FROM usage_observations ORDER BY id")]
    {:builder-fn rs/as-unqualified-maps}))

(defn- sync-cursors [path]
  (jdbc/execute!
    {:dbtype "sqlite" :dbname path}
    ["SELECT direction,cursor FROM usage_sync_state ORDER BY direction"]
    {:builder-fn rs/as-unqualified-maps}))

(deftest bounded-backfill-is-local-only-and-idempotent
  (let [directory (str (fs/create-temp-dir {:prefix "usage-backfill-test-"}))
        path (str directory "/events.db")
        now 2000000000000
        timestamp (str (java.time.Instant/ofEpochMilli (- now 1000)))
        payload (cheshire.core/generate-string
                  {:rate_limits
                   {:five_hour {:used_percentage 11 :resets_at 2000003600}
                    :seven_day {:used_percentage 22 :resets_at 2000604800}}})]
    (try
      (log/ensure-db! path)
      (jdbc/execute!
        {:dbtype "sqlite" :dbname path}
        [(str "INSERT INTO context_snapshots "
              "(timestamp,agent,session_id,payload,node,origin_id) VALUES "
              "(?,'codex','synthetic-local',?,NULL,NULL),"
              "(?,'codex','synthetic-remote',?,'synthetic-node',7)")
         timestamp payload timestamp payload])
      (is (= {:snapshots 1 :observations 2 :inserted 2 :cursor 1}
             (usage-sync/backfill-once! path now 10)))
      (is (= {:snapshots 0 :observations 0 :inserted 0 :cursor 1}
             (usage-sync/backfill-once! path now 10)))
      (let [rows (local-rows path)]
        (is (= 2 (count rows)))
        (is (every? #(= 1 (:publishable %)) rows))
        (is (= #{"five_hour" "seven_day"} (set (map :window_key rows)))))
      (finally
        (fs/delete-tree directory)))))

(deftest bounded-backfill-covers-the-twelve-week-prior-horizon
  (let [directory (str (fs/create-temp-dir {:prefix "usage-prior-backfill-test-"}))
        path (str directory "/events.db")
        now 2000000000000
        within-retention (str (java.time.Instant/ofEpochMilli
                                (- now (* 84 24 60 60 1000))))
        outside-retention (str (java.time.Instant/ofEpochMilli
                                 (- now (* 92 24 60 60 1000))))
        payload (cheshire.core/generate-string
                  {:rate_limits
                   {:seven_day {:used_percentage 81 :resets_at 2000604800}}})]
    (try
      (log/ensure-db! path)
      (jdbc/execute!
        {:dbtype "sqlite" :dbname path}
        [(str "INSERT INTO context_snapshots "
              "(timestamp,agent,session_id,payload) VALUES "
              "(?,'claude-code','synthetic-within',?),"
              "(?,'claude-code','synthetic-outside',?)")
         within-retention payload outside-retention payload])
      (is (= 1 (:inserted (usage-sync/backfill-once! path now 10))))
      (is (= 1 (count (local-rows path))))
      (finally
        (fs/delete-tree directory)))))

(deftest publisher-skips-rows-too-close-to-the-broker-retention-boundary
  (let [directory (str (fs/create-temp-dir {:prefix "usage-publish-buffer-test-"}))
        path (str directory "/events.db")
        now (System/currentTimeMillis)
        stale (observation "codex" (- now (* 91 24 60 60 1000))
                           "five_hour" 8 (quot (+ now 3600000) 1000))
        fresh (observation "codex" now "five_hour" 9
                           (quot (+ now 3600000) 1000))
        published (atom nil)]
    (try
      (insert-local! path stale)
      (insert-local! path fresh)
      (with-redefs [remote/publish-usage-observations!
                    (fn [_ observations]
                      (reset! published observations)
                      {:accepted (count observations) :duplicates 0})]
        (is (= 1 (:sent (usage-sync/publish-once! {} path 10))))
        (is (= [(:event-id fresh)] (mapv :event-id @published)))
        (is (= [{:direction "publish" :cursor 2}]
               (sync-cursors path))))
      (finally
        (fs/delete-tree directory)))))

(deftest paired-runners-recover-and-exchange-without-echo
  (let [directory (str (fs/create-temp-dir {:prefix "usage-sync-test-"}))
        db-a (str directory "/a.db")
        db-b (str directory "/b.db")
        port (free-port)
        config-a {:url (str "http://127.0.0.1:" port)
                  :runner-id "runner-a" :token "synthetic-token-a"}
        config-b {:url (str "http://127.0.0.1:" port)
                  :runner-id "runner-b" :token "synthetic-token-b"}
        now (System/currentTimeMillis)
        from-a (observation "codex" now "five_hour" 8
                            (quot (+ now 3600000) 1000))
        from-b (observation "agy" (inc now) "seven_day" 27.5
                            (quot (+ now 86400000) 1000))]
    (try
      (insert-local! db-a from-a)
      (log/ensure-db! db-b)
      (testing "an outage advances neither durable cursor"
        (let [result (usage-sync/tick! config-a db-a 10)]
          (is (= #{:publish :pull}
                 (set (map :operation (:errors result)))))
          (is (empty? (sync-cursors db-a)))))
      (let [b (broker/new-broker {"runner-a" "synthetic-token-a"
                                  "runner-b" "synthetic-token-b"})
            server (broker-http/start! b {:port port})]
        (try
          (testing "recovery publishes A and lets a quiet B materialize it"
            (let [a-result (usage-sync/tick! config-a db-a 10)
                  b-result (usage-sync/tick! config-b db-b 10)]
              (is (empty? (:errors a-result)))
              (is (= 1 (get-in a-result [:publish :accepted])))
              (is (= 0 (get-in a-result [:pull :inserted]))
                  "a node ignores its own event-id on pull")
              (is (empty? (:errors b-result)))
              (is (= 1 (get-in b-result [:pull :inserted])))
              (is (= [{:direction "pull" :cursor 1}]
                     (sync-cursors db-b)))
              (is (= 0 (:publishable (first (local-rows db-b)))))))
          (testing "B publishes its local row and A receives it"
            (insert-local! db-b from-b)
            (is (= 1 (get-in (usage-sync/tick! config-b db-b 10)
                             [:publish :accepted])))
            (is (= 1 (get-in (usage-sync/tick! config-a db-a 10)
                             [:pull :inserted])))
            (is (= 2 (count (local-rows db-a))))
            (is (= 2 (count (local-rows db-b))))
            (is (= [1 0] (mapv :publishable (local-rows db-a))))
            (is (= [0 1] (mapv :publishable (local-rows db-b)))))
          (testing "additional ticks neither republish remote rows nor duplicate"
            (let [a-result (usage-sync/tick! config-a db-a 10)
                  b-result (usage-sync/tick! config-b db-b 10)
                  broker-page (remote/read-usage-observations! config-a 0 10)]
              (is (= 0 (get-in a-result [:publish :sent])))
              (is (= 0 (get-in b-result [:publish :sent])))
              (is (= 2 (count (:observations broker-page))))))
          (finally
            ((:stop server) :timeout 100))))
      (finally
        (fs/delete-tree directory)))))
