(ns cch.control.broker-http-test
  (:require [cch.control.broker :as broker]
            [cch.control.broker-http :as broker-http]
            [cch.control.remote :as remote]
            [babashka.fs :as fs]
            [clojure.test :refer [deftest is testing]])
  (:import [java.net ServerSocket]
           [java.nio.file Files LinkOption]
           [java.nio.file.attribute PosixFilePermissions]))

(defn- free-port []
  (with-open [socket (ServerSocket. 0)]
    (.getLocalPort socket)))

(defn- config [port runner-id token]
  {:url (str "http://127.0.0.1:" port)
   :runner-id runner-id
   :token token})

(def sessions-a
  [{:id "codex:10000000-0000-0000-0000-00000000000a"
    :agent "codex" :status "idle" :available true :cwd "/not-shared"}])

(def sessions-b
  [{:id "claude:10000000-0000-0000-0000-00000000000b"
    :agent "claude" :status "working" :available true}])

(deftest http-client-crosses-the-authenticated-broker-boundary
  (let [port (free-port)
        b (broker/new-broker {"runner-a" "synthetic-token-a"
                              "runner-b" "synthetic-token-b"})
        server (broker-http/start! b {:port port})
        a (config port "runner-a" "synthetic-token-a")
        runner-b (config port "runner-b" "synthetic-token-b")]
    (try
      (remote/register! a sessions-a)
      (remote/register! runner-b sessions-b)
      (is (= #{"codex:10000000-0000-0000-0000-00000000000a"
               "claude:10000000-0000-0000-0000-00000000000b"}
             (set (map :id (remote/sessions a)))))
      (is (every? #(nil? (:cwd %)) (remote/sessions a)))
      (is (= "Build pair"
             (:alias
               (remote/set-session-alias!
                 a "codex:10000000-0000-0000-0000-00000000000a"
                 "Build pair"))))
      (is (= "Build pair"
             (:alias
               (some #(when (= "codex:10000000-0000-0000-0000-00000000000a"
                               (:id %))
                        %)
                     (remote/sessions runner-b)))))
      (is (= :forbidden
             (try
               (remote/set-session-alias!
                 runner-b "codex:10000000-0000-0000-0000-00000000000a"
                 "Forged")
               (catch clojure.lang.ExceptionInfo error
                 (:type (ex-data error))))))
      (is (= "queued"
             (:status
               (remote/enqueue! a
                                {:source "codex:10000000-0000-0000-0000-00000000000a"
                                 :target "claude:10000000-0000-0000-0000-00000000000b"
                                 :message "Synthetic network ping"
                                 :message-id "http-message-1"}))))
      (is (= "Synthetic network ping"
             (:body (first (remote/poll! runner-b)))))
      (remote/ack! runner-b "http-message-1" "delivered")
      (is (= "delivered" (:status (remote/message-status a "http-message-1"))))
      (is (= :unauthorized
             (try
               (remote/sessions (assoc a :token "wrong"))
               (catch clojure.lang.ExceptionInfo error
                 (:type (ex-data error))))))
      (finally
        ((:stop server) :timeout 100)))))

(deftest runner-config-requires-complete-encrypted-or-loopback-transport
  (testing "unset is an intentional local-only mode"
    (is (nil? (remote/config-from-env {}))))
  (testing "partial pairing is rejected"
    (is (= :invalid-runner-config
           (try
             (remote/config-from-env {"CCH_CONTROL_BROKER_URL" "https://broker.invalid"})
             (catch clojure.lang.ExceptionInfo error
               (:type (ex-data error)))))))
  (testing "a token cannot be sent over non-loopback cleartext"
    (is (= :insecure-broker-url
           (try
             (remote/config-from-env
               {"CCH_CONTROL_BROKER_URL" "http://broker.invalid"
                "CCH_CONTROL_RUNNER_ID" "runner-a"
                "CCH_CONTROL_RUNNER_TOKEN" "synthetic-token"})
             (catch clojure.lang.ExceptionInfo error
               (:type (ex-data error)))))))
  (is (= {:url "http://127.0.0.1:8787"
          :runner-id "runner-a" :token "synthetic-token"}
         (remote/config-from-env
           {"CCH_CONTROL_BROKER_URL" "http://127.0.0.1:8787/"
            "CCH_CONTROL_RUNNER_ID" "runner-a"
            "CCH_CONTROL_RUNNER_TOKEN" "synthetic-token"}))))

(deftest broker-token-config-is-explicit-and-never-defaulted
  (is (= {"runner-a" "synthetic-token"}
         (broker-http/runner-tokens-from-env
           {"CCH_CONTROL_RUNNER_TOKENS"
            "{\"runner-a\":\"synthetic-token\"}"})))
  (is (= :missing-broker-config
         (try
           (broker-http/runner-tokens-from-env {})
           (catch clojure.lang.ExceptionInfo error
             (:type (ex-data error)))))))

(deftest pairing-config-is-durable-owner-only-and-env-takes-precedence
  (let [directory (fs/create-temp-dir {:prefix "cch-control-pairing-test-"})
        path (str directory "/control-runner.json")
        stored {:url "https://broker.invalid"
                :runner-id "runner-file"
                :token "synthetic-file-token"}
        from-env {:url "https://other-broker.invalid"
                  :runner-id "runner-env"
                  :token "synthetic-env-token"}]
    (try
      (is (= path (remote/save-config! path stored)))
      (is (= stored (remote/config-from-file path)))
      (is (= (PosixFilePermissions/fromString "rw-------")
             (Files/getPosixFilePermissions
               (.toPath (fs/file path)) (make-array LinkOption 0))))
      (with-redefs [remote/config-from-env (constantly from-env)
                    remote/config-from-file (constantly stored)]
        (is (= from-env (remote/config))))
      (finally
        (fs/delete-tree directory)))))
