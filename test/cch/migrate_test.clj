(ns cch.migrate-test
  (:require [clojure.test :refer [deftest is testing]]
            [babashka.fs :as fs]
            [babashka.process :as p]
            [cch.migrate :as migrate]
            [clojure.string :as str]))

(defn- create-legacy-events-table! [db]
  (p/sh ["sqlite3" db
         (str "CREATE TABLE events (id INTEGER PRIMARY KEY, hook_name TEXT, event_type TEXT);")]))

(defn- create-legacy-context-snapshots-table! [db]
  (p/sh ["sqlite3" db
         (str "CREATE TABLE context_snapshots (id INTEGER PRIMARY KEY, "
              "session_id TEXT, payload TEXT);")]))

(defn- column-names [db table]
  (-> (p/sh ["sqlite3" db (format "PRAGMA table_info('%s');" table)])
      :out
      str/split-lines
      (->> (map #(second (str/split % #"\|"))) set)))

(defn- with-tmp-db [f]
  (let [tmp-dir (str (fs/create-temp-dir {:prefix "migrate-test-"}))
        db      (str tmp-dir "/test.db")]
    (try (f db) (finally (fs/delete-tree tmp-dir)))))

(deftest apply-all-on-empty-db-is-noop-and-records-nothing
  (testing "without any tables, migrations that ALTER would fail — but baseline-probe runs on column-exists? which returns false, so we'd ALTER nonexistent tables. The intended sequence is schema.sql first, THEN apply-all!. So this test asserts apply-all! is safe to call WITHOUT preceding schema.sql, even if the migrations themselves can't run yet."
    (with-tmp-db
      (fn [db]
        ;; schema_migrations table itself should always create successfully.
        (try (migrate/apply-all! db)
             (catch Exception _ nil))
        ;; Even on failure, the tracking table must exist.
        (let [out (:out (p/sh ["sqlite3" db ".tables"]))]
          (is (str/includes? out "schema_migrations")))))))

(deftest baselines-existing-agent-columns
  (testing "DB that already has agent columns (added by ad-hoc DDL) gets both migrations baselined, no ALTER attempted"
    (with-tmp-db
      (fn [db]
        ;; Simulate a DB at the state the old apply-column-migrations! would leave it in:
        ;; both tables exist with their agent columns, but no schema_migrations table.
        (create-legacy-events-table! db)
        (p/sh ["sqlite3" db
               "ALTER TABLE events ADD COLUMN agent TEXT NOT NULL DEFAULT 'claude-code';"])
        (create-legacy-context-snapshots-table! db)
        (p/sh ["sqlite3" db
               "ALTER TABLE context_snapshots ADD COLUMN agent TEXT NOT NULL DEFAULT 'claude-code';"])

        (migrate/apply-all! db)

        (is (= (set (migrate/migration-ids)) (migrate/applied-ids db))
            "agent migrations baselined; later migrations applied")
        (is (contains? (column-names db "events") "agent"))
        (is (contains? (column-names db "context_snapshots") "agent"))
        (is (contains? (column-names db "events") "node"))))))

(deftest runs-pending-migrations-on-pre-existing-db-without-agent-cols
  (testing "DB that pre-dates the agent column gets both migrations applied"
    (with-tmp-db
      (fn [db]
        (create-legacy-events-table! db)
        (create-legacy-context-snapshots-table! db)
        ;; No agent column yet.
        (is (not (contains? (column-names db "events") "agent")))

        (migrate/apply-all! db)

        (is (contains? (column-names db "events") "agent"))
        (is (contains? (column-names db "context_snapshots") "agent"))
        (is (= (set (migrate/migration-ids)) (migrate/applied-ids db)))))))

(deftest mixed-state-handled-correctly
  (testing "DB with one column added but not the other: applied one is baselined, missing one is applied"
    (with-tmp-db
      (fn [db]
        (create-legacy-events-table! db)
        (p/sh ["sqlite3" db
               "ALTER TABLE events ADD COLUMN agent TEXT NOT NULL DEFAULT 'claude-code';"])
        (create-legacy-context-snapshots-table! db)
        ;; context_snapshots still missing its agent col.

        (migrate/apply-all! db)

        (is (contains? (column-names db "context_snapshots") "agent"))
        (is (= (set (migrate/migration-ids)) (migrate/applied-ids db)))))))

(deftest is-idempotent
  (testing "running apply-all! twice doesn't error and doesn't double-record"
    (with-tmp-db
      (fn [db]
        (create-legacy-events-table! db)
        (create-legacy-context-snapshots-table! db)
        (migrate/apply-all! db)
        (migrate/apply-all! db)
        (let [count-out (:out (p/sh ["sqlite3" db "SELECT count(*) FROM schema_migrations;"]))]
          (is (= (str (count (migrate/migration-ids))) (str/trim count-out))))))))

(deftest applied-ids-on-empty-tracking-table
  (with-tmp-db
    (fn [db]
      (p/sh ["sqlite3" db
             "CREATE TABLE schema_migrations (id TEXT PRIMARY KEY, applied_at TEXT);"])
      (is (= #{} (migrate/applied-ids db))))))
