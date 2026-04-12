(ns hooks.event-log-test
  "Subprocess tests for the universal observer. Verifies that:
   - the hook emits no stdout regardless of event type (protocol split)
   - every input lands in SQLite with event_type correctly populated
   - the `extra` column carries the full input payload as JSON
   - install subscribes to the expected event count; uninstall cleans up

   Uses CCH_LOG_SYNC=1 to run sqlite3 synchronously so we can assert on
   rows immediately after a hook invocation (no sleep-polling)."
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(def repo-root
  (str/trim (:out (p/sh ["git" "rev-parse" "--show-toplevel"]))))

(defn- run-with-db
  "Subprocess hooks.event-log against a test-scoped DB, synchronously."
  [db-path json-input]
  (p/sh {:dir    repo-root
         :in     json-input
         :extra-env {"CCH_LOG_SYNC" "1"
                     "XDG_DATA_HOME" (str (fs/parent (fs/parent db-path)))}}
        "bb" "-cp" "src:resources" "-m" "hooks.event-log"))

(defn- query-last
  "Read the most recent events row as a map."
  [db-path]
  (let [result (p/sh ["sqlite3" "-json" db-path
                      "SELECT * FROM events ORDER BY id DESC LIMIT 1;"])]
    (when-not (str/blank? (:out result))
      (first (json/parse-string (:out result) true)))))

(defn- fresh-db []
  (let [tmp (str (fs/create-temp-dir {:prefix "event-log-"}))]
    ;; XDG_DATA_HOME is resolved as parent of parent — matches db-path layout.
    (fs/create-dirs (str tmp "/cch"))
    (str tmp "/cch/events.db")))

;; --- Protocol: observer never emits stdout, regardless of event ---

(deftest test-no-stdout-for-any-event
  (let [db (fresh-db)]
    (try
      (testing "SessionStart → no output"
        (let [result (run-with-db db
                       (json/generate-string {:hook_event_name "SessionStart"
                                              :session_id "s-1"
                                              :source "startup"}))]
          (is (zero? (:exit result)))
          (is (str/blank? (:out result)))))

      (testing "PostToolUse (Bash) → no output"
        (let [result (run-with-db db
                       (json/generate-string {:hook_event_name "PostToolUse"
                                              :session_id "s-1"
                                              :tool_name "Bash"
                                              :tool_input {:command "ls"}}))]
          (is (zero? (:exit result)))
          (is (str/blank? (:out result)))))

      (testing "Stop → no output"
        (let [result (run-with-db db
                       (json/generate-string {:hook_event_name "Stop"
                                              :session_id "s-1"
                                              :last_assistant_message "done"}))]
          (is (zero? (:exit result)))
          (is (str/blank? (:out result)))))
      (finally
        (fs/delete-tree (fs/parent (fs/parent db)))))))

;; --- SQLite: every invocation lands as a row ---

(deftest test-event-type-populated-per-event
  (let [db (fresh-db)]
    (try
      (testing "SessionStart row has event_type=SessionStart, hook_name=event-log"
        (run-with-db db
          (json/generate-string {:hook_event_name "SessionStart"
                                 :session_id "sess-1"}))
        (let [row (query-last db)]
          (is (= "event-log" (:hook_name row)))
          (is (= "SessionStart" (:event_type row)))
          (is (= "sess-1" (:session_id row)))))

      (testing "PreCompact row has event_type=PreCompact"
        (run-with-db db
          (json/generate-string {:hook_event_name "PreCompact"
                                 :trigger "manual"}))
        (is (= "PreCompact" (:event_type (query-last db)))))
      (finally
        (fs/delete-tree (fs/parent (fs/parent db)))))))

;; --- `extra` column carries the full payload ---

(deftest test-extra-column-captures-payload
  (let [db (fresh-db)]
    (try
      (testing "extra column holds the input JSON (minus cch internals)"
        (run-with-db db
          (json/generate-string {:hook_event_name "UserPromptSubmit"
                                 :session_id "sess-2"
                                 :prompt "write a haiku"}))
        (let [row    (query-last db)
              parsed (json/parse-string (:extra row) true)]
          (is (= "UserPromptSubmit" (:hook_event_name parsed)))
          (is (= "write a haiku" (:prompt parsed)))
          ;; cch's internal marker key is stripped before serialization
          (is (not (contains? parsed :cch/hook-name)))))

      (testing "event-specific fields (PreCompact.trigger) survive"
        (run-with-db db
          (json/generate-string {:hook_event_name "PreCompact"
                                 :trigger "auto"
                                 :custom_instructions ""}))
        (let [row    (query-last db)
              parsed (json/parse-string (:extra row) true)]
          (is (= "auto" (:trigger parsed)))))
      (finally
        (fs/delete-tree (fs/parent (fs/parent db)))))))
