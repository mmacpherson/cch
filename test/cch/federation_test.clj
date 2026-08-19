(ns cch.federation-test
  "Tests for cross-machine event-log federation (claude-code-hooks-n55).
  Deployment specifics are never hardcoded — these use fake node names,
  loopback-free fakes, and tmp DBs."
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [cch.db :as db]
            [cch.federation :as fed]
            [cch.federation.ship :as ship]
            [cch.log :as log]
            [cch.server]
            [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

;; --- Pure: config parsing ---

(deftest federation-config-parsing
  (testing "no federation section → shipping off, not a collector"
    (let [c (fed/federation-config nil)]
      (is (false? (:enabled? c)))
      (is (false? (:collector? c)))
      (is (nil? (:collector-url c)))
      (is (string? (:node c)) "node always resolves (hostname fallback)")))
  (testing "collector-url present → shipping enabled"
    (let [c (fed/federation-config {:federation {:collector-url "http://collector:8888"
                                                 :node "node-a"}})]
      (is (true? (:enabled? c)))
      (is (= "http://collector:8888" (:collector-url c)))
      (is (= "node-a" (:node c)))
      (is (= 60000 (:interval-ms c)) "default 60s")))
  (testing "explicit enabled:false overrides a present collector-url"
    (let [c (fed/federation-config {:federation {:collector-url "http://x" :enabled false}})]
      (is (false? (:enabled? c)))))
  (testing "collector flag, token, and custom interval"
    (let [c (fed/federation-config {:federation {:collector true :token "secret"
                                                 :interval-seconds 30}})]
      (is (true? (:collector? c)))
      (is (= "secret" (:token c)))
      (is (= 30000 (:interval-ms c)))))
  (testing "interval floored to avoid a hot spin loop on a bad config"
    (let [c (fed/federation-config {:federation {:interval-seconds 1}})]
      (is (= 5000 (:interval-ms c))))))

;; --- Pure: authorization ---

(deftest authorization
  (testing "no configured token → open (the network is the gate)"
    (is (true? (fed/authorized? nil "anything")))
    (is (true? (fed/authorized? "" nil))))
  (testing "configured token must match a Bearer header exactly"
    (is (true?  (fed/authorized? "secret" "Bearer secret")))
    (is (false? (fed/authorized? "secret" "Bearer wrong")))
    (is (false? (fed/authorized? "secret" nil)))
    (is (false? (fed/authorized? "secret" "secret")) "raw token without Bearer is rejected")))

;; --- Pure: ingest SQL ---

(deftest ingest-sql-shape
  (testing "unknown table or empty rows → nil"
    (is (nil? (fed/ingest-sql "bogus" [{:id 1}])))
    (is (nil? (fed/ingest-sql "events" []))))
  (testing "idempotent insert carries node + origin_id and escapes quotes"
    (let [sql (fed/ingest-sql "events" [{:id 7 :node "node-a" :hook_name "it's"}])]
      (is (str/includes? sql "INSERT OR IGNORE INTO events"))
      (is (str/includes? sql "node, origin_id, timestamp"))
      (is (str/includes? sql "'node-a', 7,") "node then origin_id")
      (is (str/includes? sql "'it''s'") "single quote escaped"))))

;; --- DB-backed: ingest idempotency, watermark, shipper read ---

(defn- with-tmp-db [f]
  (let [dir (str (fs/create-temp-dir {:prefix "fed-test-"}))
        dbf (str dir "/events.db")]
    (with-redefs [db/db-path (fn [] dbf)]
      (log/ensure-db! dbf)
      (try (f dbf) (finally (fs/delete-tree dir))))))

(def ^:private sample-event
  {:id 5 :node "node-a" :timestamp "2026-01-01T00:00:00.000"
   :agent "claude-code" :session_id "s1" :hook_name "scope-lock"
   :event_type "PreToolUse" :tool_name "Edit" :file_path "/x"
   :cwd "/r" :decision nil :reason nil :elapsed_ms 1.0 :extra nil})

(deftest ingest-is-idempotent
  (with-tmp-db
    (fn [_]
      (is (= 1 (fed/ingest-rows! "events" [sample-event])))
      (fed/ingest-rows! "events" [sample-event]) ; re-send: dup ignored
      (is (= 1 (-> (db/query "SELECT count(*) AS c FROM events WHERE node='node-a';")
                   first :c))
          "re-sending the same (node, origin_id) does not duplicate")
      (let [r (first (db/query "SELECT node, origin_id, hook_name FROM events WHERE node='node-a';"))]
        (is (= "node-a" (:node r)))
        (is (= 5 (:origin_id r)) "origin_id preserves the source row id")
        (is (= "scope-lock" (:hook_name r)))))))

(deftest ingest-large-batch-fed-via-stdin
  (testing "a batch whose INSERT exceeds the OS ARG_MAX still ingests"
    ;; Regression: ingest-rows! must feed sqlite3 over stdin, not argv.
    ;; 600 rows each padded so the combined INSERT is well over ~128KB.
    (with-tmp-db
      (fn [_]
        (let [pad  (apply str (repeat 400 "x"))
              rows (mapv (fn [i]
                           {:id i :node "bulk" :timestamp "2026-01-01T00:00:00.000"
                            :agent "claude-code" :session_id "s" :hook_name "h"
                            :event_type "PreToolUse" :extra pad})
                         (range 600))]
          (is (= 600 (fed/ingest-rows! "events" rows)))
          (is (= 600 (-> (db/query "SELECT count(*) AS c FROM events WHERE node='bulk';")
                         first :c))
              "all rows land — no E2BIG"))))))

(deftest ingest-different-nodes-same-origin-id-coexist
  (with-tmp-db
    (fn [_]
      (fed/ingest-rows! "events" [(assoc sample-event :node "node-a" :id 5)])
      (fed/ingest-rows! "events" [(assoc sample-event :node "node-b" :id 5)])
      (is (= 2 (-> (db/query "SELECT count(*) AS c FROM events;") first :c))
          "(node,origin_id) is the key — same id from different nodes is distinct"))))

(deftest watermark-roundtrip
  (with-tmp-db
    (fn [_]
      (is (= 0 (fed/get-watermark "events")) "absent watermark reads as 0")
      (fed/set-watermark! "events" 42)
      (is (= 42 (fed/get-watermark "events")))
      (fed/set-watermark! "events" 100)
      (is (= 100 (fed/get-watermark "events")) "upsert advances in place"))))

(deftest rows-after-respects-watermark
  (with-tmp-db
    (fn [dbf]
      (p/sh ["sqlite3" dbf
             (str "INSERT INTO events (hook_name,event_type,node) VALUES "
                  "('h','E','node-a'),('h','E','node-a'),('h','E','node-a');")])
      (is (= 3 (count (fed/rows-after "events" 0 10))))
      (is (= 2 (count (fed/rows-after "events" 1 10))) "id > watermark")
      (is (empty? (fed/rows-after "events" 99 10))))))

;; --- Shipper: byte-aware batching (guards against oversized POSTs) ---

(deftest byte-batching-splits-large-payloads
  (let [big     (apply str (repeat 500000 "x"))            ; ~0.5MB per row
        rows    (mapv (fn [i] {:id i :extra big}) (range 20)) ; ~10MB total
        batches (ship/into-byte-batches rows)]
    (testing "a payload over the per-POST cap splits into multiple groups"
      (is (> (count batches) 1)))
    (testing "every row is preserved, in id order"
      (is (= (map :id rows) (mapcat #(map :id %) batches))))
    (testing "each multi-row group stays within a sane bound"
      (doseq [b batches]
        (is (or (= 1 (count b))
                (<= (count (json/generate-string b)) (* 5 1024 1024))))))))

(deftest byte-batching-keeps-small-rows-together
  (let [rows    (mapv (fn [i] {:id i :extra "x"}) (range 100))
        batches (ship/into-byte-batches rows)]
    (testing "many tiny rows fit in a single POST"
      (is (= 1 (count batches)))
      (is (= 100 (count (first batches)))))))

;; --- HTTP handler: collector gating (end-to-end through handle-ingest) ---

(defn- ingest-req [headers body-str]
  {:headers headers :body (io/input-stream (.getBytes ^String body-str "UTF-8"))})

(deftest ingest-handler-gating
  (with-tmp-db
    (fn [_]
      (let [handle @#'cch.server/handle-ingest]
        (testing "a plain (non-collector) node returns 404, accepts nothing"
          (with-redefs [fed/load-federation-config (fn [] {:collector? false})]
            (is (= 404 (:status (handle (ingest-req {} "{}")))))))
        (testing "collector with a token rejects a missing/wrong token with 401"
          (with-redefs [fed/load-federation-config (fn [] {:collector? true :token "s"})]
            (is (= 401 (:status (handle (ingest-req {} "{}")))))
            (is (= 401 (:status (handle (ingest-req {"authorization" "Bearer nope"} "{}")))))))
        (testing "collector, authorized, valid payload → 200 and rows land"
          (with-redefs [fed/load-federation-config (fn [] {:collector? true :token "s"})]
            (let [payload (json/generate-string
                            {:table "events"
                             :rows [{:id 9 :node "nz" :hook_name "h"
                                     :event_type "PreToolUse" :agent "claude-code"
                                     :timestamp "2026-01-01T00:00:00.000"}]})
                  resp    (handle (ingest-req {"authorization" "Bearer s"} payload))]
              (is (= 200 (:status resp)))
              (is (= 1 (-> (db/query "SELECT count(*) AS c FROM events WHERE node='nz';")
                           first :c))))))
        (testing "collector rejects an unknown table with 400"
          (with-redefs [fed/load-federation-config (fn [] {:collector? true})]
            (is (= 400 (:status (handle (ingest-req {} (json/generate-string
                                                        {:table "secrets" :rows []}))))))))))))
