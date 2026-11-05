(ns cch.control.store-test
  (:require [babashka.fs :as fs]
            [cch.control.store :as store]
            [clojure.test :refer [deftest is]]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]))

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

(deftest codex-alias-bindings-store-only-a-digest-and-are-one-time
  (with-temp-store
    (fn [path]
      (let [binding {:tool-use-id "alias-call-1"
                     :session-id "thread-1"
                     :alias "Review pair"}]
        (store/record-codex-alias-binding! binding)
        (let [row (first
                    (jdbc/execute!
                      {:dbtype "sqlite" :dbname path}
                      ["SELECT * FROM control_codex_alias_bindings WHERE tool_use_id=?"
                       "alias-call-1"]
                      {:builder-fn rs/as-unqualified-maps}))]
          (is (not (some #{"Review pair"} (vals row))))
          (is (= (store/content-digest "Review pair") (:alias_sha256 row))))
        (is (nil? (store/claim-codex-alias-binding!
                    {:tool-use-id "alias-call-1" :alias "Changed"})))
        (is (= "codex:thread-1"
               (store/claim-codex-alias-binding!
                 {:tool-use-id "alias-call-1" :alias "Review pair"})))
        (is (nil? (store/claim-codex-alias-binding!
                    {:tool-use-id "alias-call-1" :alias "Review pair"})))))))

(deftest deliveries-for-route-joins-sequence-numbers
  (with-temp-store
    (fn [_]
      (store/record-delivery! {:message-id "m1" :source "claude:a" :target "codex:b"
                               :content-sha256 "x" :status "submitted"})
      (store/assign-seq! {:message-id "m1" :source "claude:a" :target "codex:b"})
      (store/record-delivery! {:message-id "m2" :source "codex:b" :target "claude:a"
                               :content-sha256 "y" :status "delivered"})
      (store/record-delivery! {:message-id "m3" :source "codex:b" :target "codex:c"
                               :content-sha256 "z" :status "delivered"})
      (let [rows (store/deliveries-for-route "claude:a" 10)]
        (is (= #{"m1" "m2"} (set (map :message_id rows))))
        (is (= 1 (:seq (first (filter #(= "m1" (:message_id %)) rows)))))
        (is (nil? (:seq (first (filter #(= "m2" (:message_id %)) rows)))))))))

(deftest summaries-usage-and-cursor-round-trip
  (with-temp-store
    (fn [_]
      (is (nil? (store/summarizer-cursor)))
      (store/save-summarizer-cursor! 41)
      (store/save-summarizer-cursor! 42)
      (is (= 42 (store/summarizer-cursor)))
      (store/save-session-summary! {:route-id "codex:a" :status "s1" :auto-name "n1"
                                    :repo "r" :turn-key "t1"})
      (store/save-session-summary! {:route-id "codex:a" :status "s2" :auto-name "n1"
                                    :repo "r" :turn-key "t2"})
      (is (= {:status "s2" :auto-name "n1" :repo "r" :turn-key "t2"}
             (dissoc (get (store/session-summaries) "codex:a") :updated-at)))
      (is (= 0.0 (:cost_usd (store/summarizer-usage "2026-01-01"))))
      (store/record-summarizer-usage! "2026-01-01" {:input-tokens 100 :output-tokens 10 :cost-usd 0.25})
      (store/record-summarizer-usage! "2026-01-01" {:input-tokens 50 :output-tokens 5 :cost-usd 0.5})
      (let [u (store/summarizer-usage "2026-01-01")]
        (is (= 2 (:calls u)))
        (is (= 150 (:input_tokens u)))
        (is (= 0.75 (:cost_usd u)))))))
