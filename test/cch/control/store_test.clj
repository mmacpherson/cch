(ns cch.control.store-test
  (:require [babashka.fs :as fs]
            [cch.control.store :as store]
            [clojure.test :refer [deftest is]]))

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
          (is (not (contains? public :auth_token)))
          (is (= route (store/claude-route-for-socket "/tmp/synthetic-2.sock")))
          (is (= "new-secret" (:auth_token internal))))))))

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
