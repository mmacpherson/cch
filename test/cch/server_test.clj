(ns cch.server-test
  "End-to-end tests for the dispatcher + dashboard.

  Starts the server on a free port, hits /dispatch/<event>, asserts
  reconciled responses match the per-hook composed-handler output.
  Uses a tmp DB so hook_config state is isolated per run."
  (:require [babashka.fs :as fs]
            [babashka.http-client :as http]
            [babashka.process :as p]
            [cch.config-db :as cdb]
            [cch.log :as log]
            [cch.server :as server]
            [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]))

(def ^:dynamic *port* nil)
(def ^:dynamic *tmp-db* nil)

(defn- free-port []
  (with-open [s (java.net.ServerSocket. 0)]
    (.getLocalPort s)))

(defn with-server [f]
  (let [tmp-dir (str (fs/create-temp-dir {:prefix "server-test-db-"}))
        db      (str tmp-dir "/events.db")
        p       (free-port)]
    (with-redefs [log/db-path (fn [] db)]
      (log/ensure-db! db)
      ;; Enable all code hooks at global scope so dispatch fan-out works.
      ;; Individual tests can disable specific hooks as needed.
      (doseq [hook-name ["scope-lock" "protect-files" "command-audit" "event-log"]]
        (cdb/upsert! {:hook-name hook-name :scope cdb/global-scope :enabled true}))
      (let [{:keys [stop]} (server/start! {:port p :host "127.0.0.1"})]
        (binding [*port* p
                  *tmp-db* db]
          (try (f) (finally
                     (stop :timeout 100)
                     (fs/delete-tree tmp-dir))))))))

(use-fixtures :once with-server)

(defn- url [path]
  (format "http://127.0.0.1:%d%s" *port* path))

(defn- dispatch!
  "POST a JSON body to /dispatch/<event>. Returns the parsed response map
  {:status n :body string :parsed-json or-nil}."
  [event payload]
  (let [body  (json/generate-string payload)
        resp  (http/post (url (str "/dispatch/" event))
                         {:body     body
                          :headers  {"Content-Type" "application/json"}
                          :throw    false})
        parsed (when-not (str/blank? (:body resp))
                 (try (json/parse-string (:body resp) true)
                      (catch Exception _ nil)))]
    {:status (:status resp)
     :body   (:body resp)
     :parsed parsed}))

;; --- Health + unknown event ---

(deftest test-health-lists-registered-hooks
  (let [resp (http/get (url "/health"))
        body (json/parse-string (:body resp) true)]
    (is (= 200 (:status resp)))
    (is (= "ok" (:status body)))
    (let [names (set (map :name (:hooks body)))]
      (is (contains? names "scope-lock"))
      (is (contains? names "protect-files"))
      (is (contains? names "command-audit"))
      (is (contains? names "event-log")))))

(deftest test-legacy-hooks-route-gone
  (testing "POST /hooks/<name> returns 404 (dispatcher model only)"
    (let [resp (http/post (url "/hooks/scope-lock")
                          {:body "{}" :headers {"Content-Type" "application/json"} :throw false})]
      (is (= 404 (:status resp))))))

(deftest test-unknown-event-returns-empty
  (testing "dispatch on an event with no subscribers returns 200 empty"
    (let [{:keys [status body]} (dispatch! "NobodyHandlesThis" {:cwd "/tmp"})]
      (is (= 200 status))
      (is (str/blank? body)))))

;; --- Dispatch routing + reconciliation ---

(deftest test-dispatch-allow-returns-empty
  (testing "PreToolUse with in-scope, non-sensitive file → empty response"
    (let [repo-root (str/trim (:out (p/sh ["git" "rev-parse" "--show-toplevel"])))
          {:keys [status body]}
          (dispatch! "PreToolUse"
                     {:hook_event_name "PreToolUse"
                      :cwd             repo-root
                      :tool_name       "Edit"
                      :tool_input      {:file_path (str repo-root "/src/cch/core.clj")}})]
      (is (= 200 status))
      (is (str/blank? body)))))

(deftest test-dispatch-protect-files-denies-env
  (testing "Edit on .env → protect-files denies; dispatcher returns the deny"
    (let [repo-root (str/trim (:out (p/sh ["git" "rev-parse" "--show-toplevel"])))
          {:keys [status parsed]}
          (dispatch! "PreToolUse"
                     {:hook_event_name "PreToolUse"
                      :cwd             repo-root
                      :tool_name       "Edit"
                      :tool_input      {:file_path (str repo-root "/.env")}})]
      (is (= 200 status))
      (is (= "deny" (get-in parsed [:hookSpecificOutput :permissionDecision])))
      (is (str/includes? (get-in parsed [:hookSpecificOutput :permissionDecisionReason])
                         "sensitive")))))

(deftest test-dispatch-scope-lock-asks-out-of-worktree
  (testing "Edit on /etc/passwd → scope-lock asks; no protect-files match"
    (let [repo-root (str/trim (:out (p/sh ["git" "rev-parse" "--show-toplevel"])))
          {:keys [status parsed]}
          (dispatch! "PreToolUse"
                     {:hook_event_name "PreToolUse"
                      :cwd             repo-root
                      :tool_name       "Edit"
                      :tool_input      {:file_path "/etc/passwd"}})]
      (is (= 200 status))
      (is (= "ask" (get-in parsed [:hookSpecificOutput :permissionDecision]))))))

(deftest test-disabling-hook-in-db-silences-it
  (testing "toggling protect-files off in DB → .env edit no longer denied"
    (let [repo-root (str/trim (:out (p/sh ["git" "rev-parse" "--show-toplevel"])))]
      (try
        (cdb/upsert! {:hook-name "protect-files" :scope cdb/global-scope :enabled false})
        ;; With protect-files disabled and the path outside cwd's worktree,
        ;; scope-lock's .git check still fires for .git paths, but .env is
        ;; in-repo → scope-lock allows. Net: empty body.
        (let [{:keys [status body]}
              (dispatch! "PreToolUse"
                         {:hook_event_name "PreToolUse"
                          :cwd             repo-root
                          :tool_name       "Edit"
                          :tool_input      {:file_path (str repo-root "/.env")}})]
          (is (= 200 status))
          (is (str/blank? body)))
        (finally
          ;; Restore for other tests
          (cdb/upsert! {:hook-name "protect-files" :scope cdb/global-scope :enabled true}))))))

(deftest test-matcher-filters-by-tool-name
  (testing "scope-lock matcher 'Edit|Write' — dispatcher doesn't invoke it for Read"
    (let [repo-root (str/trim (:out (p/sh ["git" "rev-parse" "--show-toplevel"])))
          {:keys [status body]}
          (dispatch! "PreToolUse"
                     {:hook_event_name "PreToolUse"
                      :cwd             repo-root
                      :tool_name       "Read"
                      :tool_input      {:file_path "/etc/passwd"}})]
      ;; Read doesn't match Edit|Write; scope-lock not invoked; no other hook on
      ;; PreToolUse/Read except event-log (observer, nil response). Empty.
      (is (= 200 status))
      (is (str/blank? body)))))

;; --- Non-tool events still dispatch (event-log observes silently) ---

(deftest test-session-start-observed-silently
  (testing "SessionStart goes through event-log, which returns nil (empty body)"
    (let [{:keys [status body]}
          (dispatch! "SessionStart"
                     {:hook_event_name "SessionStart"
                      :session_id      "http-test-session"
                      :source          "startup"})]
      (is (= 200 status))
      (is (str/blank? body)))))

;; --- Reconciliation unit-ish test via pure fn ---

(deftest test-reconcile-precedence
  (let [reconcile (ns-resolve 'cch.server 'reconcile)]
    (testing "deny beats ask beats allow beats context"
      (is (= :deny (:decision (reconcile [{:decision :allow}
                                          {:decision :ask}
                                          {:decision :deny :reason "nope"}]))))
      (is (= :ask  (:decision (reconcile [{:decision :allow}
                                          {:decision :ask :reason "check"}])))))
    (testing "contexts concatenate when no decision present"
      (is (= "a\n\nb" (:context (reconcile [{:context "a"}
                                            {:context "b"}
                                            nil])))))
    (testing "all-nil → nil"
      (is (nil? (reconcile [nil nil]))))))

;; --- Config CRUD API ---

(deftest test-config-api-list
  (testing "GET /api/config returns an array"
    (let [resp (http/get (url "/api/config"))
          body (json/parse-string (:body resp) true)]
      (is (= 200 (:status resp)))
      (is (sequential? body))
      ;; Fixture enables the four code hooks at global scope
      (is (<= 4 (count body))))))

(deftest test-config-api-upsert-and-delete
  (testing "POST /api/config upserts a row; DELETE removes it"
    (let [upsert-resp (http/post (url "/api/config")
                                 {:body (json/generate-string
                                          {:hook-name "scope-lock"
                                           :scope     "repo:/tmp/test-crud"
                                           :enabled   false
                                           :options   {:note "ui-crud-test"}})
                                  :headers {"Content-Type" "application/json"}})]
      (is (= 200 (:status upsert-resp)))
      (let [all (json/parse-string (:body (http/get (url "/api/config"))) true)
            row (first (filter #(and (= "scope-lock" (:hook-name %))
                                     (= "repo:/tmp/test-crud" (:scope %)))
                               all))]
        (is (false? (:enabled row)))
        (is (= {:note "ui-crud-test"} (:options row))))

      (let [del-resp (http/delete (url "/api/config?hook=scope-lock&scope=repo%3A%2Ftmp%2Ftest-crud")
                                  {:throw false})]
        (is (= 200 (:status del-resp))))

      (let [all (json/parse-string (:body (http/get (url "/api/config"))) true)]
        (is (nil? (first (filter #(and (= "scope-lock" (:hook-name %))
                                       (= "repo:/tmp/test-crud" (:scope %)))
                                 all))))))))

(deftest test-config-api-upsert-form-body
  (testing "POST /api/config accepts form-encoded body (for UI)"
    (let [resp (http/post (url "/api/config")
                          {:body    "hook=protect-files&scope=repo%3A%2Ftmp%2Fformtest&enabled=true"
                           :headers {"Content-Type" "application/x-www-form-urlencoded"}})]
      (is (= 200 (:status resp)))
      (let [row (cdb/get-row "protect-files" "repo:/tmp/formtest")]
        (is (true? (:enabled row)))))
    ;; cleanup
    (cdb/delete! "protect-files" "repo:/tmp/formtest")))

(deftest test-config-api-upsert-missing-fields
  (testing "POST /api/config without required fields → 400"
    (let [resp (http/post (url "/api/config")
                          {:body (json/generate-string {:hook-name "x"})
                           :headers {"Content-Type" "application/json"}
                           :throw false})]
      (is (= 400 (:status resp))))))

;; --- Hook matrix page ---

(deftest test-hooks-matrix-renders
  (let [resp (http/get (url "/hooks"))]
    (is (= 200 (:status resp)))
    (is (str/includes? (get-in resp [:headers "content-type"]) "text/html"))
    (is (str/includes? (:body resp) "cch · hooks"))
    (is (str/includes? (:body resp) "scope-lock"))
    (is (str/includes? (:body resp) "protect-files"))
    (is (str/includes? (:body resp) "global"))
    (testing "uses Bulma table classes"
      (is (str/includes? (:body resp) "table is-hoverable is-fullwidth")))
    (testing "type badges rendered as Bulma tags"
      (is (str/includes? (:body resp) "tag")))))

(deftest test-hooks-toggle-form-post
  (testing "POST /hooks/toggle upserts the row"
    (http/post (url "/hooks/toggle")
               {:body    "hook=protect-files&scope=global&enabled=false"
                :headers {"Content-Type" "application/x-www-form-urlencoded"}
                :throw   false})
    ;; bb http-client follows 303s by default; we can't easily observe the
    ;; Location header. The visible behavior is that the row is updated.
    (let [row (cdb/get-row "protect-files" cdb/global-scope)]
      (is (false? (:enabled row))))
    ;; restore so later tests aren't affected
    (cdb/upsert! {:hook-name "protect-files" :scope cdb/global-scope :enabled true})))

;; --- Dashboard renders ---

(deftest test-dashboard-renders
  (let [resp (http/get (url "/"))]
    (is (= 200 (:status resp)))
    (is (str/includes? (get-in resp [:headers "content-type"]) "text/html"))
    (is (str/includes? (:body resp) "cch · events"))
    (is (str/includes? (:body resp) "event-list"))
    (testing "uses Bulma + Inter/JetBrains Mono"
      (is (str/includes? (:body resp) "bulma@"))
      (is (str/includes? (:body resp) "Inter"))
      (is (str/includes? (:body resp) "JetBrains+Mono"))
      (is (str/includes? (:body resp) "class=\"section\"")
          "outer wrapper uses Bulma section"))))

(deftest test-dashboard-filters-applied
  (testing "filter query params flow through to query-events"
    (let [resp (http/get (url "/?hook=event-log&limit=5"))]
      (is (= 200 (:status resp)))
      (is (str/includes? (:body resp) "selected=\"selected\" value=\"event-log\""))
      (is (str/includes? (:body resp) "value=\"5\"")))))
