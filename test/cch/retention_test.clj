(ns cch.retention-test
  (:require [babashka.fs :as fs]
            [cch.db :as db]
            [cch.log :as log]
            [cch.retention :as retention]
            [clojure.test :refer [deftest is testing]]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]))

(def ^:private day-ms 86400000)
(def ^:private now-ms 1800000000000)

(deftest settings-fall-back-to-defaults
  (is (= retention/defaults (retention/settings nil)))
  (is (= 7 (:event-payload-days (retention/settings {:retention {:event-payload-days 7}}))))
  (is (= 0 (:event-days (retention/settings {:retention {:event-days 0}})))
      "0 is a valid horizon: it turns the rule off")
  (is (false? (:enabled (retention/settings {:retention {:enabled false}}))))
  (is (= retention/defaults
         (retention/settings {:retention {:event-days -1 :event-payload-days "soon" :enabled "no"}}))))

(deftest cutoff-uses-the-stored-timestamp-spelling
  (is (= "2027-01-14T08:00:00.000" (retention/cutoff now-ms 1)))
  (is (nil? (retention/cutoff now-ms 0))))

(defn- with-db [f]
  (let [tmp-dir (str (fs/create-temp-dir {:prefix "retention-"}))
        db-path (str tmp-dir "/test.db")]
    (try
      (with-redefs [db/db-path (constantly db-path)]
        (log/ensure-db! db-path)
        (f (jdbc/get-datasource {:dbtype "sqlite" :dbname db-path})))
      (finally (fs/delete-tree tmp-dir)))))

(defn- insert-aged! [ds days-old]
  (let [ts (retention/cutoff now-ms days-old)]
    (jdbc/execute! ds ["INSERT INTO events (timestamp, session_id, hook_name, event_type, extra) VALUES (?,?,?,?,?)"
                       ts "s1" "event-log" "PostToolUse" "{\"tool_response\":\"output\"}"])
    (jdbc/execute! ds ["INSERT INTO context_snapshots (timestamp, session_id, payload) VALUES (?,?,?)"
                       ts "s1" "{}"])))

(defn- ages
  "Days-old of each remaining row, with whether its payload survives."
  [ds]
  (mapv (juxt :timestamp (comp some? :extra))
        (jdbc/execute! ds ["SELECT timestamp, extra FROM events ORDER BY id"]
                       {:builder-fn rs/as-unqualified-maps})))

(deftest prune-applies-each-horizon
  (with-db
    (fn [ds]
      (doseq [days [400 200 100 40 20 1]] (insert-aged! ds days))
      (let [s (assoc (retention/settings nil) :batch-rows 2 :pause-ms 0)
            ts #(retention/cutoff now-ms %)]
        (is (= {:events 1 :context-snapshots 3 :event-payloads 3} (retention/pending s now-ms)))
        (is (= {:events 1 :context-snapshots 3 :event-payloads 3}
               (retention/prune! s now-ms (constantly true))))
        (is (= [[(ts 200) false] [(ts 100) false] [(ts 40) false] [(ts 20) true] [(ts 1) true]]
               (ages ds))
            "old rows go, aged payloads are cleared, recent rows are untouched")
        (is (= 3 (:n (jdbc/execute-one! ds ["SELECT COUNT(*) AS n FROM context_snapshots"]
                                        {:builder-fn rs/as-unqualified-maps}))))
        (testing "a second pass has nothing to do"
          (is (= {:events 0 :context-snapshots 0 :event-payloads 0} (retention/pending s now-ms)))
          (is (= {:events 0 :context-snapshots 0 :event-payloads 0}
                 (retention/prune! s now-ms (constantly true)))))
        (testing "rows that age in later are picked up from the cursor"
          (is (= {:events 0 :context-snapshots 0 :event-payloads 1}
                 (retention/prune! s (+ now-ms (* 15 day-ms)) (constantly true)))))))))

(deftest disabled-rules-and-early-stop
  (with-db
    (fn [ds]
      (doseq [days [400 300 200 100]] (insert-aged! ds days))
      (let [off (assoc (retention/settings {:retention {:event-payload-days 0 :event-days 0
                                                        :context-snapshot-days 0}})
                       :pause-ms 0)]
        (is (= {:events 0 :context-snapshots 0 :event-payloads 0}
               (retention/prune! off now-ms (constantly true))))
        (is (= 4 (count (ages ds)))))
      (testing "a stop between transactions leaves the rest for the next pass"
        (let [s (assoc (retention/settings nil) :batch-rows 1 :pause-ms 0 :event-days 0
                       :context-snapshot-days 0)]
          (is (= 1 (:event-payloads (retention/prune! s now-ms (constantly false)))))
          (is (= 3 (:event-payloads (retention/prune! s now-ms (constantly true))))))))))

(deftest prune-runs-alongside-a-writer
  (with-db
    (fn [ds]
      ;; As the server's event writer leaves it.
      (jdbc/execute! ds ["PRAGMA journal_mode=WAL"])
      (doseq [days (repeat 300 100)] (insert-aged! ds days))
      (let [writing (atom true)
            writer (future
                     (with-open [conn (jdbc/get-connection ds)]
                       (jdbc/execute! conn ["PRAGMA busy_timeout=5000"])
                       (loop [n 0]
                         (if @writing
                           (do (jdbc/execute! conn ["INSERT INTO events (session_id, hook_name, event_type) VALUES ('s2','event-log','Stop')"])
                               (recur (inc n)))
                           n))))
            s (assoc (retention/settings nil) :batch-rows 1 :pause-ms 0)
            result (try (retention/prune! s now-ms (constantly true))
                        (finally (reset! writing false)))]
        (is (= {:events 0 :context-snapshots 300 :event-payloads 300} result))
        (is (pos? @writer) "the writer kept inserting throughout")))))

(deftest vacuum-returns-freed-space
  (with-db
    (fn [ds]
      (jdbc/execute! ds ["PRAGMA journal_mode=WAL"])
      (let [payload (apply str (repeat 20000 "x"))]
        (dotimes [_ 200]
          (jdbc/execute! ds ["INSERT INTO events (timestamp, hook_name, event_type, extra) VALUES (?,?,?,?)"
                             (retention/cutoff now-ms 100) "event-log" "PostToolUse" payload])))
      (retention/prune! (assoc (retention/settings nil) :pause-ms 0) now-ms (constantly true))
      (let [size #(fs/size (db/db-path))
            before (do (jdbc/execute! ds ["PRAGMA wal_checkpoint(TRUNCATE)"]) (size))]
        (is (true? (retention/vacuum!)))
        (is (< (size) (quot before 4)) "cleared payloads no longer occupy the file")))))
