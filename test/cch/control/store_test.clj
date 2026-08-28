(ns cch.control.store-test
  (:require [babashka.fs :as fs]
            [cch.control.store :as store]
            [clojure.test :refer [deftest is]]
            [next.jdbc :as jdbc]))

(defn- with-temp-store [f]
  (let [dir (str (fs/create-temp-dir {:prefix "cch-control-store-"}))
        path (str dir "/events.db")]
    (try
      (with-redefs [store/db-path (constantly path)]
        (f path))
      (finally (fs/delete-tree dir)))))

(deftest claude-registration-is-idempotent-and-sanitized
  (with-temp-store
    (fn [_]
      (let [route (store/upsert-claude!
                    {:session-id "session-1" :cwd "/synthetic/project"
                     :transcript-path "/synthetic/transcript.jsonl"
                     :socket-path "/tmp/synthetic.sock" :auth-token "secret"
                     :pid 123})]
        (is (= "claude:session-1" route))
        (store/upsert-claude!
          {:session-id "session-1" :cwd "/synthetic/project-2"
           :socket-path "/tmp/synthetic-2.sock" :auth-token "new-secret"
           :pid 456})
        (let [public (first (store/claude-sessions))
              internal (store/claude-session route)]
          (is (= 1 (count (store/claude-sessions))))
          (is (= "/synthetic/project-2" (:cwd public)))
          (is (= "/synthetic/transcript.jsonl" (:transcript_path public)))
          (is (not (contains? public :auth_token)))
          (is (= route (store/claude-route-for-socket "/tmp/synthetic-2.sock")))
          (is (= "new-secret" (:auth_token internal)))
          (store/set-claude-native-url!
            route "https://claude.ai/code/session_synthetic123")
          (is (= "https://claude.ai/code/session_synthetic123"
                 (:native_url (store/claude-session route)))))))))

(deftest existing-local-session-schema-gains-deep-link-columns
  (with-temp-store
    (fn [path]
      (jdbc/execute!
        {:dbtype "sqlite" :dbname path}
        [(str "CREATE TABLE control_sessions ("
              "route_id TEXT PRIMARY KEY, agent TEXT NOT NULL, "
              "native_id TEXT NOT NULL, cwd TEXT, name TEXT, "
              "socket_path TEXT, auth_token TEXT, pid INTEGER, "
              "registered_at INTEGER NOT NULL, updated_at INTEGER NOT NULL)")])
      (store/upsert-claude!
        {:session-id "legacy-session"
         :transcript-path "/synthetic/legacy.jsonl"})
      (let [row (store/claude-session "claude:legacy-session")]
        (is (= "/synthetic/legacy.jsonl" (:transcript_path row)))
        (is (contains? row :native_url))))))

(deftest delivery-records-store-hash-not-content
  (with-temp-store
    (fn [_]
      (store/record-delivery!
        {:message-id "message-1" :source "operator" :target "claude:session-1"
         :content-sha256 "abc123" :status "submitted"})
      (let [row (store/delivery "message-1")]
        (is (= "abc123" (:content_sha256 row)))
        (is (not-any? #(re-find #"(?i)message|content" (name %))
                      (remove #{:message_id :content_sha256} (keys row))))))))

(deftest codex-bindings-are-envelope-bound-and-one-time
  (with-temp-store
    (fn [_]
      (let [binding {:tool-use-id "tool-call-1"
                     :session-id "thread-1"
                     :target "claude:session-1"
                     :message-id "message-1"
                     :message "Synthetic ping"}]
        (store/record-codex-binding! binding)
        (is (nil? (store/claim-codex-binding!
                    {:tool-use-id "tool-call-1"
                     :target "claude:session-1"
                     :message-id "message-1"
                     :message "Different content"})))
        (is (= "codex:thread-1"
               (store/claim-codex-binding!
                 {:tool-use-id "tool-call-1"
                  :target "claude:session-1"
                  :message-id "message-1"
                  :message "Synthetic ping"})))
        (is (nil? (store/claim-codex-binding!
                    {:tool-use-id "tool-call-1"
                     :target "claude:session-1"
                     :message-id "message-1"
                     :message "Synthetic ping"})))
        (is (= :duplicate-codex-binding
               (try
                 (store/record-codex-binding! binding)
                 nil
                 (catch clojure.lang.ExceptionInfo error
                   (:type (ex-data error))))))))))
