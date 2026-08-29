(ns cch.control.activity-sync-test
  (:require [babashka.fs :as fs]
            [cch.control.activity-sync :as sync]
            [cch.control.remote :as remote]
            [cch.log :as log]
            [clojure.test :refer [deftest is]]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]))

(defn- cursor [path]
  (:last_event_id
    (jdbc/execute-one!
      {:dbtype "sqlite" :dbname path}
      ["SELECT last_event_id FROM activity_sync_state WHERE singleton_id=1"]
      {:builder-fn rs/as-unqualified-maps})))

(deftest exporter-advances-only-after-ack-and-never-sends-raw-fields
  (let [directory (str (fs/create-temp-dir {:prefix "activity-sync-test-"}))
        path (str directory "/events.db")
        published (atom nil)]
    (try
      (log/ensure-db! path)
      (jdbc/execute!
        {:dbtype "sqlite" :dbname path}
        [(str "INSERT INTO events(timestamp,agent,session_id,hook_name,event_type,"
              "tool_name,file_path,cwd,decision,reason,elapsed_ms,extra) VALUES "
              "('2026-08-29T12:00:00Z','claude-code','private-session',"
              "'event-log','PostToolUse','Edit','/private/file','/private',"
              "'allow','private reason',2.5,'{\"prompt\":\"private\"}'),"
              "('2026-08-29T12:00:01Z','claude-code','private-session',"
              "'scope-lock','PreToolUse','Edit','/private/file','/private',"
              "'allow','private reason',1.0,'{}')")])
      (with-redefs [remote/publish-activity-observations!
                    (fn [_ observations]
                      (reset! published observations)
                      {:accepted (count observations) :duplicates 0})]
        (is (= {:inspected 2 :sent 1 :accepted 1 :duplicates 0 :cursor 2}
               (sync/tick! {} path 10)))
        (is (= 2 (cursor path)))
        (is (= #{:event-id :schema-version :observed-at :agent :action
                 :tool-category :outcome :duration-ms}
               (set (keys (first @published)))))
        (is (= 0 (:sent (sync/tick! {} path 10)))))
      (finally
        (fs/delete-tree directory)))))

(deftest failed-publication-does-not-advance-the-source-cursor
  (let [directory (str (fs/create-temp-dir {:prefix "activity-retry-test-"}))
        path (str directory "/events.db")]
    (try
      (log/ensure-db! path)
      (jdbc/execute!
        {:dbtype "sqlite" :dbname path}
        ["INSERT INTO events(timestamp,agent,hook_name,event_type) VALUES ('2026-08-29T12:00:00Z','claude-code','event-log','SessionStart')"])
      (with-redefs [remote/publish-activity-observations!
                    (fn [& _] (throw (ex-info "offline" {:type :offline})))]
        (is (= :offline
               (try (sync/tick! {} path 10)
                    (catch clojure.lang.ExceptionInfo error
                      (:type (ex-data error)))))))
      (is (nil? (cursor path)))
      (finally
        (fs/delete-tree directory)))))

(deftest old-history-is-jumped-before-the-bounded-page
  (let [directory (str (fs/create-temp-dir {:prefix "activity-history-test-"}))
        path (str directory "/events.db")
        now (str (java.time.Instant/now))
        sent (atom nil)]
    (try
      (log/ensure-db! path)
      (jdbc/execute!
        {:dbtype "sqlite" :dbname path}
        [(str "INSERT INTO events(timestamp,agent,hook_name,event_type) VALUES "
              "('2020-01-01T00:00:00Z','claude-code','event-log','SessionStart'),"
              "(?,'codex','event-log','UserPromptSubmit')")
         now])
      (with-redefs [remote/publish-activity-observations!
                    (fn [_ observations]
                      (reset! sent observations)
                      {:accepted (count observations) :duplicates 0})]
        (let [result (sync/tick! {} path 1)]
          (is (= 1 (:inspected result)))
          (is (= 1 (:sent result)))
          (is (= "codex" (:agent (first @sent))))
          (is (= 2 (cursor path)))))
      (finally
        (fs/delete-tree directory)))))
