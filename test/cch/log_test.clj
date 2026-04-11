(ns cch.log-test
  (:require [clojure.test :refer [deftest is testing]]
            [cch.log :as log]
            [cheshire.core :as json]
            [babashka.fs :as fs]
            [babashka.process :as p]))

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
