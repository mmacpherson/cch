(ns cch.server-test
  "End-to-end tests for the HTTP dispatcher + dashboard.

  Starts the server on a free port, hits real routes, asserts responses.
  The equivalence check (command-mode JSON == HTTP-mode JSON for the same
  input) is the correctness backbone — it's what makes --http install
  safe to flip on."
  (:require [babashka.http-client :as http]
            [cch.server :as server]
            [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]))

;; --- Fixture: spin up a real server on a random free port ---

(def ^:dynamic *port* nil)
(def ^:dynamic *stop* nil)

(defn- free-port []
  (with-open [s (java.net.ServerSocket. 0)]
    (.getLocalPort s)))

(defn with-server [f]
  (let [p (free-port)
        {:keys [stop]} (server/start! {:port p :host "127.0.0.1"})]
    (binding [*port* p
              *stop* stop]
      (try (f) (finally (stop :timeout 100))))))

(use-fixtures :once with-server)

(defn- url [path]
  (format "http://127.0.0.1:%d%s" *port* path))

;; --- Health + routing ---

(deftest test-health-lists-registered-hooks
  (let [resp (http/get (url "/health"))
        body (json/parse-string (:body resp) true)]
    (is (= 200 (:status resp)))
    (is (= "ok" (:status body)))
    (let [names (set (map :name (:hooks body)))]
      (is (contains? names "scope-lock"))
      (is (contains? names "command-audit"))
      (is (contains? names "event-log")))))

(deftest test-unknown-hook-404
  (let [resp (http/post (url "/hooks/nope")
                        {:body "{}"
                         :headers {"Content-Type" "application/json"}
                         :throw false})]
    (is (= 404 (:status resp)))))

;; --- Equivalence: HTTP response matches command-mode shape ---

(deftest test-scope-lock-pretooluse-allow
  (testing "allowed path returns empty body (matches command-mode nil)"
    (let [input (json/generate-string
                  {:hook_event_name "PreToolUse"
                   :cwd             "/home/example/projects/claude-code-hooks"
                   :tool_input      {:file_path
                                     "/home/example/projects/claude-code-hooks/src/foo.clj"}})
          resp  (http/post (url "/hooks/scope-lock")
                           {:body input
                            :headers {"Content-Type" "application/json"}})]
      (is (= 200 (:status resp)))
      (is (str/blank? (:body resp))))))

(deftest test-scope-lock-pretooluse-outside-worktree
  (testing "out-of-worktree path returns ask JSON in PreToolUse shape"
    (let [input (json/generate-string
                  {:hook_event_name "PreToolUse"
                   :cwd             "/home/example/projects/claude-code-hooks"
                   :tool_input      {:file_path "/etc/passwd"}})
          resp  (http/post (url "/hooks/scope-lock")
                           {:body input
                            :headers {"Content-Type" "application/json"}})
          body  (json/parse-string (:body resp) true)]
      (is (= 200 (:status resp)))
      (is (= "ask" (get-in body [:hookSpecificOutput :permissionDecision])))
      (is (str/includes? (get-in body [:hookSpecificOutput :permissionDecisionReason])
                         "outside worktree")))))

(deftest test-command-audit-posttooluse-shape
  (testing "PostToolUse response uses top-level decision/reason (not hookSpecificOutput)"
    ;; command-audit without a .cch-config.yaml → no flag-patterns → no match → nil
    (let [input (json/generate-string
                  {:hook_event_name "PostToolUse"
                   :cwd             "/tmp"
                   :tool_name       "Bash"
                   :tool_input      {:command "ls -la"}})
          resp  (http/post (url "/hooks/command-audit")
                           {:body input
                            :headers {"Content-Type" "application/json"}})]
      (is (= 200 (:status resp)))
      (is (str/blank? (:body resp))))))

(deftest test-event-log-no-op
  (testing "event-log returns empty body on every event; just logs"
    (let [input (json/generate-string
                  {:hook_event_name "SessionStart"
                   :session_id      "http-test-session"
                   :source          "startup"})
          resp  (http/post (url "/hooks/event-log")
                           {:body input
                            :headers {"Content-Type" "application/json"}})]
      (is (= 200 (:status resp)))
      (is (str/blank? (:body resp))))))

;; --- Dashboard renders ---

(deftest test-dashboard-renders
  (let [resp (http/get (url "/"))]
    (is (= 200 (:status resp)))
    (is (str/includes? (get-in resp [:headers "content-type"]) "text/html"))
    (is (str/includes? (:body resp) "cch · events"))
    (is (str/includes? (:body resp) "Roboto+Condensed"))
    (is (str/includes? (:body resp) "picocss/pico"))
    (is (str/includes? (:body resp) "<form"))
    ;; Events render as a list of collapsible article cards (not a table).
    (is (str/includes? (:body resp) "event-list"))))

(deftest test-dashboard-filters-applied
  (testing "filter query params flow through to query-events"
    (let [resp (http/get (url "/?hook=event-log&limit=5"))]
      (is (= 200 (:status resp)))
      ;; The selected hook should appear in the select; limit value should be in the input
      (is (str/includes? (:body resp) "selected=\"selected\" value=\"event-log\""))
      (is (str/includes? (:body resp) "value=\"5\"")))))
