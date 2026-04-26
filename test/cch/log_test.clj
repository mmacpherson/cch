(ns cch.log-test
  (:require [clojure.test :refer [deftest is testing]]
            [cch.db :as db]
            [cch.log :as log]
            [cheshire.core :as json]
            [babashka.fs :as fs]
            [babashka.process :as p]
            [clojure.string]))

(deftest test-ensure-db
  (let [tmp-dir (str (fs/create-temp-dir {:prefix "log-test-"}))
        db-path (str tmp-dir "/test.db")]
    (try
      (testing "creates database and schema"
        (log/ensure-db! db-path)
        (is (fs/exists? db-path))
        ;; Verify tables exist
        (let [result (p/sh ["sqlite3" db-path ".tables"])]
          (is (re-find #"events" (:out result)))))

      (testing "idempotent — second call doesn't error"
        (log/ensure-db! db-path))
      (finally
        (fs/delete-tree tmp-dir)))))

(deftest test-log-and-query
  (let [tmp-dir (str (fs/create-temp-dir {:prefix "log-test-"}))
        db-path (str tmp-dir "/test.db")]
    (try
      (log/ensure-db! db-path)

      (testing "can insert and query events"
        ;; Insert directly via sqlite3 to avoid fire-and-forget timing
        (p/sh ["sqlite3" db-path
               "INSERT INTO events (session_id, hook_name, event_type, tool_name, file_path, cwd, decision, reason) VALUES ('sess1', 'scope-lock', 'PreToolUse', 'Edit', '/repo/foo.py', '/repo', 'allow', 'in scope');"])
        (p/sh ["sqlite3" db-path
               "INSERT INTO events (session_id, hook_name, event_type, tool_name, file_path, cwd, decision, reason) VALUES ('sess1', 'scope-lock', 'PreToolUse', 'Write', '/etc/passwd', '/repo', 'ask', 'outside worktree');"])

        (let [result (p/sh ["sqlite3" "-json" db-path
                            "SELECT * FROM events ORDER BY id;"])
              events (json/parse-string (:out result) true)]
          (is (= 2 (count events)))
          (is (= "allow" (:decision (first events))))
          (is (= "ask" (:decision (second events))))))
      (finally
        (fs/delete-tree tmp-dir)))))

(deftest test-nil-fields-become-sql-null
  (let [tmp-dir (str (fs/create-temp-dir {:prefix "log-null-test-"}))
        db-path (str tmp-dir "/test.db")]
    (try
      (log/ensure-db! db-path)

      (testing "nil values are stored as SQL NULL, not string 'null'"
        ;; Insert with nil session_id and tool_name via sql-value
        (p/sh ["sqlite3" db-path
               "INSERT INTO events (session_id, hook_name, event_type, tool_name, file_path, cwd, decision, reason) VALUES (NULL, 'scope-lock', 'PreToolUse', NULL, '/repo/foo.py', '/repo', 'allow', NULL);"])

        (let [result (p/sh ["sqlite3" "-json" db-path
                            "SELECT session_id, tool_name, reason FROM events LIMIT 1;"])
              event  (first (json/parse-string (:out result) true))]
          ;; JSON null, not the string "null"
          (is (nil? (:session_id event)))
          (is (nil? (:tool_name event)))
          (is (nil? (:reason event)))))
      (finally
        (fs/delete-tree tmp-dir)))))

(deftest test-escape-sql
  (testing "escapes single quotes"
    (is (= "it''s a test" (#'log/escape-sql "it's a test"))))
  (testing "handles nil"
    (is (nil? (#'log/escape-sql nil)))))

(deftest test-sql-value
  (testing "nil becomes NULL"
    (is (= "NULL" (#'log/sql-value nil))))
  (testing "string gets quoted"
    (is (= "'hello'" (#'log/sql-value "hello"))))
  (testing "quotes are escaped"
    (is (= "'it''s'" (#'log/sql-value "it's")))))

;; --- Background writer ---

(deftest test-writer-roundtrip
  (testing "queued events land in the DB after stop-writer! drains"
    (let [tmp-dir (str (fs/create-temp-dir {:prefix "log-writer-"}))
          db      (str tmp-dir "/test.db")]
      (try
        (with-redefs [db/db-path (fn [] db)]
          (log/ensure-db! db)
          (log/start-writer!)
          (try
            (dotimes [i 25]
              (log/log-event!
                {:hook-name "writer-test"
                 :event-type "PreToolUse"
                 :tool-name "Edit"
                 :file-path (str "/tmp/" i ".txt")
                 :cwd "/tmp"
                 :session-id "s1"
                 :decision nil
                 :reason nil
                 :elapsed-ms nil
                 :extra "{}"}))
            (finally
              ;; stop-writer! sends ::stop and joins the drain thread,
              ;; so by the time it returns sqlite3 has consumed everything.
              (log/stop-writer!)))
          (let [n (-> (p/sh ["sqlite3" db "SELECT count(*) FROM events WHERE hook_name = 'writer-test';"])
                      :out
                      str
                      clojure.string/trim
                      Long/parseLong)]
            (is (= 25 n) "all queued inserts must reach the DB after stop-writer!")))
        (finally
          (fs/delete-tree tmp-dir))))))

(deftest test-writer-start-is-idempotent
  (testing "calling start-writer! twice doesn't spawn a second writer"
    (let [tmp-dir (str (fs/create-temp-dir {:prefix "log-writer-idem-"}))
          db      (str tmp-dir "/test.db")]
      (try
        (with-redefs [db/db-path (fn [] db)]
          (log/ensure-db! db)
          (let [w1 (log/start-writer!)
                w2 (log/start-writer!)]
            (try
              (is (identical? w1 w2)
                  "second start-writer! should return the existing writer-state map")
              (finally
                (log/stop-writer!)))))
        (finally
          (fs/delete-tree tmp-dir))))))

(deftest test-writer-stop-is-idempotent
  (testing "calling stop-writer! when no writer is running is a no-op"
    (is (nil? (log/stop-writer!)))))
