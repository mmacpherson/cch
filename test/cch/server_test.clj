(ns cch.server-test
  "End-to-end tests for the dispatcher + dashboard.

  Starts the server on a free port, hits /dispatch/<event>, asserts
  reconciled responses match the per-hook composed-handler output.
  Uses a tmp DB so hook_config state is isolated per run."
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [hato.client :as http]
            [cch.config-db :as cdb]
            [cch.db :as db]
            [cch.events :as cch-events]
            [cch.log :as log]
            [cch.server :as server]
            [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [hooks.scope-lock]))

(def ^:dynamic *port* nil)
(def ^:dynamic *tmp-db* nil)

(defn- free-port []
  (with-open [s (java.net.ServerSocket. 0)]
    (.getLocalPort s)))

(defn- delete-tree-tolerantly
  "fs/delete-tree races with SQLite WAL cleanup: SQLite can unlink
   events.db-wal between our directory listing and our unlink call,
   producing NoSuchFileException on a file that's already gone (which
   is the desired end state). Walk the tree ourselves and treat
   already-gone as success."
  [dir]
  (let [path (java.nio.file.Paths/get dir (into-array String []))]
    (when (java.nio.file.Files/exists path (into-array java.nio.file.LinkOption []))
      (java.nio.file.Files/walkFileTree
        path
        (proxy [java.nio.file.SimpleFileVisitor] []
          (visitFile [file _attrs]
            (try (java.nio.file.Files/delete file)
                 (catch java.nio.file.NoSuchFileException _ nil))
            java.nio.file.FileVisitResult/CONTINUE)
          (postVisitDirectory [d _exc]
            (try (java.nio.file.Files/delete d)
                 (catch java.nio.file.NoSuchFileException _ nil))
            java.nio.file.FileVisitResult/CONTINUE))))))

(defn with-server [f]
  (let [tmp-dir (str (fs/create-temp-dir {:prefix "server-test-db-"}))
        db      (str tmp-dir "/events.db")
        p       (free-port)]
    (with-redefs [db/db-path (fn [] db)]
      (log/ensure-db! db)
      ;; Enable all code hooks at global scope so dispatch fan-out works.
      ;; Individual tests can disable specific hooks as needed.
      (doseq [hook-name ["scope-lock" "protect-files" "command-guard" "event-log"]]
        (cdb/upsert! {:hook-name hook-name :scope cdb/global-scope :enabled true}))
      (let [{:keys [stop]} (server/start! {:port p :host "127.0.0.1"})]
        (binding [*port* p
                  *tmp-db* db]
          (try (f) (finally
                     (stop :timeout 100)
                     (delete-tree-tolerantly tmp-dir))))))))

(use-fixtures :once with-server)

(defn- url [path]
  (format "http://127.0.0.1:%d%s" *port* path))

(deftest git-root-on-disk-finds-nearest-worktree
  (let [tmp-dir (str (fs/create-temp-dir {:prefix "server-git-root-"}))
        repo     (str tmp-dir "/repo")
        nested   (str repo "/src/deep")]
    (try
      (fs/create-dirs nested)
      (fs/create-file (str repo "/.git"))
      (is (= repo (#'server/git-root-on-disk nested)))
      (is (nil? (#'server/git-root-on-disk (str tmp-dir "/missing"))))
      (finally
        (fs/delete-tree tmp-dir)))))

(defn- dispatch!
  "POST a JSON body to /dispatch/<event>. Optional :headers override the
  default Content-Type. Returns {:status :body :parsed}."
  [event payload & {:keys [headers]}]
  (let [body  (json/generate-string payload)
        resp  (http/post (url (str "/dispatch/" event))
                         {:body     body
                          :headers  (merge {"Content-Type" "application/json"}
                                           headers)
                          :throw-exceptions?    false})
        parsed (when-not (str/blank? (:body resp))
                 (try (json/parse-string (:body resp) true)
                      (catch Exception _ nil)))]
    {:status (:status resp)
     :body   (:body resp)
     :parsed parsed}))

(defn- no-op-body?
  "The dispatcher emits valid empty JSON ({}) for events where no hook
  produced output (19d9dbf: 'return valid JSON for no-output hook events'),
  rather than a blank body."
  [body]
  (= "{}" (str/trim (str body))))

;; --- Health + unknown event ---

(deftest test-health-lists-registered-hooks
  (let [resp (http/get (url "/health"))
        body (json/parse-string (:body resp) true)]
    (is (= 200 (:status resp)))
    (is (= "ok" (:status body)))
    (let [names (set (map :name (:hooks body)))]
      (is (contains? names "scope-lock"))
      (is (contains? names "protect-files"))
      (is (contains? names "event-log")))))

(deftest test-legacy-hooks-route-gone
  (testing "POST /hooks/<name> returns 404 (dispatcher model only)"
    (let [resp (http/post (url "/hooks/scope-lock")
                          {:body "{}" :headers {"Content-Type" "application/json"} :throw-exceptions? false})]
      (is (= 404 (:status resp))))))

(deftest test-unknown-event-returns-empty
  (testing "dispatch on an event with no subscribers returns 200 empty"
    (let [{:keys [status body]} (dispatch! "NobodyHandlesThis" {:cwd "/tmp"})]
      (is (= 200 status))
      (is (no-op-body? body)))))

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
      (is (no-op-body? body)))))

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
          (is (no-op-body? body)))
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
      (is (no-op-body? body)))))

;; --- Live re-eval (nREPL-style hot-reload) ---

(deftest test-hook-redef-is-live
  (testing "redefining a hook's composed var takes effect on the next dispatch"
    (let [repo-root (str/trim (:out (p/sh ["git" "rev-parse" "--show-toplevel"])))
          payload {:hook_event_name "PreToolUse"
                   :cwd             repo-root
                   :tool_name       "Edit"
                   :tool_input      {:file_path "/etc/passwd"}}]
      (testing "baseline: scope-lock asks for /etc/passwd (out of worktree)"
        (let [{:keys [status parsed]} (dispatch! "PreToolUse" payload)]
          (is (= 200 status))
          (is (= "ask" (get-in parsed [:hookSpecificOutput :permissionDecision])))))

      (testing "after with-redefs, the new behavior shows up immediately"
        ;; If the dispatcher captured `composed` by value at startup, this
        ;; test fails: the old fn keeps returning :ask. With the var
        ;; lookup happening per-dispatch, the redef wins and dispatch
        ;; returns empty (no other hook denies /etc/passwd).
        (with-redefs [hooks.scope-lock/composed (fn [_input] nil)]
          (let [{:keys [status body]} (dispatch! "PreToolUse" payload)]
            (is (= 200 status))
            (is (no-op-body? body)
                "redefined composed should be picked up on next dispatch"))))

      (testing "original behavior restored after with-redefs scope ends"
        (let [{:keys [parsed]} (dispatch! "PreToolUse" payload)]
          (is (= "ask" (get-in parsed [:hookSpecificOutput :permissionDecision]))))))))

;; --- Non-tool events still dispatch (event-log observes silently) ---

(deftest test-session-start-observed-silently
  (testing "SessionStart goes through event-log, which returns nil (empty body)"
    (let [{:keys [status body]}
          (dispatch! "SessionStart"
                     {:hook_event_name "SessionStart"
                      :session_id      "http-test-session"
                      :source          "startup"})]
      (is (= 200 status))
      (is (no-op-body? body)))))

;; --- Agent attribution via X-CCH-Agent header ---

(deftest dispatch-tags-event-with-x-cch-agent-header
  (testing "events posted with X-CCH-Agent: codex land in the DB with agent=codex"
    (let [repo-root (str/trim (:out (p/sh ["git" "rev-parse" "--show-toplevel"])))]
      (dispatch! "PreToolUse"
                 {:hook_event_name "PreToolUse"
                  :cwd             repo-root
                  :session_id      "agent-tag-codex"
                  :tool_name       "Edit"
                  :tool_input      {:file_path (str repo-root "/src/cch/core.clj")}}
                 :headers {"X-CCH-Agent" "codex"})
      ;; event-log logs synchronously via CCH_LOG_SYNC? No — but the fixture's
      ;; ensure-db! created the writer-less path; log-event! spawns a sqlite3
      ;; per call. Give it a brief beat to land.
      (Thread/sleep 200)
      (let [out (p/sh ["sqlite3" "-json" *tmp-db*
                       "SELECT agent FROM events WHERE session_id='agent-tag-codex' LIMIT 1;"])
            rows (json/parse-string (:out out) true)]
        (is (= "codex" (:agent (first rows))))))))

(deftest dispatch-defaults-to-claude-code-when-no-header
  (testing "events posted with no X-CCH-Agent header default to agent=claude-code"
    (let [repo-root (str/trim (:out (p/sh ["git" "rev-parse" "--show-toplevel"])))]
      (dispatch! "PreToolUse"
                 {:hook_event_name "PreToolUse"
                  :cwd             repo-root
                  :session_id      "agent-tag-default"
                  :tool_name       "Edit"
                  :tool_input      {:file_path (str repo-root "/src/cch/core.clj")}})
      (Thread/sleep 200)
      (let [out (p/sh ["sqlite3" "-json" *tmp-db*
                       "SELECT agent FROM events WHERE session_id='agent-tag-default' LIMIT 1;"])
            rows (json/parse-string (:out out) true)]
        (is (= "claude-code" (:agent (first rows))))))))

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
                                  {:throw-exceptions? false})]
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
                           :throw-exceptions? false})]
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
    (testing "uses custom CSS with matrix table and hook badges"
      (is (str/includes? (:body resp) "cch.css"))
      (is (str/includes? (:body resp) "hook-badge")))))

;; --- /context-snapshot — schema variations of context_window.current_usage ---

(deftest test-coerce-current-tokens-shapes
  (testing "passes a number through unchanged"
    (is (= 1234 (#'server/coerce-current-tokens 1234))))
  (testing "sums a per-source breakdown map (current Claude Code schema)"
    (is (= (+ 1 122 800 191721)
           (#'server/coerce-current-tokens
             {:input_tokens 1
              :output_tokens 122
              :cache_creation_input_tokens 800
              :cache_read_input_tokens 191721}))))
  (testing "ignores non-numeric values inside the map"
    (is (= 5 (#'server/coerce-current-tokens
               {:input_tokens 5 :note "ignored"}))))
  (testing "nil and unsupported types collapse to nil"
    (is (nil? (#'server/coerce-current-tokens nil)))
    (is (nil? (#'server/coerce-current-tokens "1234")))
    (is (nil? (#'server/coerce-current-tokens [1 2 3])))))

(deftest test-context-snapshot-accepts-realistic-payload
  (testing "POST /context-snapshot with the post-2026 schema (current_usage as map) returns 204"
    (let [body {:session_id "ctx-snap-test"
                :model {:id "claude-opus-4-7"}
                :context_window
                {:total_input_tokens 3325
                 :total_output_tokens 95734
                 :context_window_size 1000000
                 :current_usage {:input_tokens 1
                                 :output_tokens 122
                                 :cache_creation_input_tokens 800
                                 :cache_read_input_tokens 191721}
                 :used_percentage 19
                 :remaining_percentage 81}
                :rate_limits {:seven_day {:used_percentage 29
                                          :resets_at 1777518000}}}
          resp (http/post (url "/context-snapshot")
                          {:body (json/generate-string body)
                           :headers {"Content-Type" "application/json"}
                           :throw-exceptions? false})]
      (is (= 204 (:status resp)))
      (testing "row landed with both indexed columns populated"
        (let [r (-> (p/sh ["sqlite3" "-json" *tmp-db*
                           "SELECT used_pct, current_tokens FROM context_snapshots WHERE session_id='ctx-snap-test' ORDER BY id DESC LIMIT 1;"])
                    :out
                    str/trim
                    (json/parse-string true)
                    first)]
          (is (= 19.0 (:used_pct r)))
          (is (= (+ 1 122 800 191721) (:current_tokens r))))))))

(deftest test-context-snapshot-normalizes-agy-quota
  (testing "AGY's documented weekly quota shape becomes canonical 7d data"
    (let [body {:session_id "agy-ctx-snap-test"
                :model {:id "Gemini 3.1 Pro (High)"}
                :context_window {:context_window_size 1048576
                                 :used_percentage 14.24}
                :quota {:gemini-weekly
                        {:remaining_fraction 0.9378
                         :reset_time "2026-08-02T07:50:32Z"
                         :reset_in_seconds 560580}}}
          resp (http/post (url "/context-snapshot")
                          {:body (json/generate-string body)
                           :headers {"Content-Type" "application/json"
                                     "X-CCH-Agent" "agy"}
                           :throw-exceptions? false})]
      (is (= 204 (:status resp)))
      (Thread/sleep 200)
      (let [r (-> (p/sh ["sqlite3" "-json" *tmp-db*
                         (str "SELECT agent, "
                              "json_extract(payload, '$.rate_limits.seven_day.used_percentage') AS used, "
                              "json_extract(payload, '$.rate_limits.seven_day.resets_at') AS resets_at "
                              "FROM context_snapshots "
                              "WHERE session_id='agy-ctx-snap-test' "
                              "ORDER BY id DESC LIMIT 1;")])
                  :out
                  str/trim
                  (json/parse-string true)
                  first)]
        (is (= "agy" (:agent r)))
        (is (< (Math/abs (- 6.22 (:used r))) 0.000001))
        (is (= (.getEpochSecond
                 (java.time.Instant/parse "2026-08-02T07:50:32Z"))
               (:resets_at r)))))))

(deftest test-context-snapshot-coalesces-identical-agy-payloads
  (testing "repeated status-line emissions are acknowledged but stored once"
    (let [session-id "agy-coalesce-http-test"
          body {:session_id session-id
                :agent_state "working"
                :model {:id "Gemini 3.1 Pro (High)"}
                :context_window {:used_percentage 3.0}
                :quota {:gemini-weekly
                        {:remaining_fraction 0.98
                         :reset_time "2026-08-04T00:00:00Z"}}}
          post! #(http/post (url "/context-snapshot")
                            {:body (json/generate-string body)
                             :headers {"Content-Type" "application/json"
                                       "X-CCH-Agent" "agy"}
                             :throw-exceptions? false})
          responses (repeatedly 5 post!)]
      (is (every? #(= 204 (:status %)) responses))
      (is (= "true" (get-in (first responses)
                             [:headers "x-cch-captured"])))
      (is (every? #(= "false"
                       (get-in % [:headers "x-cch-captured"]))
                  (rest responses)))
      (Thread/sleep 200)
      (let [count-row (-> (p/sh ["sqlite3" "-json" *tmp-db*
                                 (str "SELECT COUNT(*) AS n "
                                      "FROM context_snapshots "
                                      "WHERE agent='agy' AND session_id='"
                                      session-id "';")])
                          :out
                          str/trim
                          (json/parse-string true)
                          first)]
        (is (= 1 (:n count-row)))))))

;; --- /forecast ---

(deftest test-forecast-endpoint
  (testing "GET /forecast returns JSON with five_hour and seven_day keys"
    (let [resp (http/get (url "/forecast") {:throw-exceptions? false})
          body (json/parse-string (:body resp) true)]
      (is (= 200 (:status resp)))
      (is (map? body))
      (is (contains? body :five_hour))
      (is (contains? body :seven_day)))))

(deftest test-hooks-toggle-form-post
  (testing "POST /hooks/toggle upserts the row"
    (http/post (url "/hooks/toggle")
               {:body    "hook=protect-files&scope=global&enabled=false"
                :headers {"Content-Type" "application/x-www-form-urlencoded"}
                :throw-exceptions?   false})
    ;; hato follows 303s by default; we can't easily observe the
    ;; Location header. The visible behavior is that the row is updated.
    (let [row (cdb/get-row "protect-files" cdb/global-scope)]
      (is (false? (:enabled row))))
    ;; restore so later tests aren't affected
    (cdb/upsert! {:hook-name "protect-files" :scope cdb/global-scope :enabled true})))

;; --- Dashboard renders ---

;; --- Live event stream (Datastar + SSE) ---

(deftest test-event-stream-sse-headers
  (testing "GET /events/stream returns text/event-stream and keeps the conn open"
    (let [conn   (java.net.Socket. "127.0.0.1" (int *port*))
          out    (.getOutputStream conn)
          in     (.getInputStream conn)
          req    (str "GET /events/stream HTTP/1.1\r\n"
                      "Host: 127.0.0.1\r\n"
                      "Connection: keep-alive\r\n\r\n")]
      (try
        (.write out (.getBytes req "UTF-8"))
        (.flush out)
        ;; Read a bit of the response and look for SSE content-type.
        (Thread/sleep 200)
        (let [buf     (byte-array 4096)
              n       (.read in buf)
              header  (String. buf 0 n "UTF-8")]
          (is (re-find #"200 OK" header))
          (is (re-find #"(?i)content-type:\s*text/event-stream" header)))
        (finally
          (.close conn))))))

(deftest test-event-stream-receives-published-fragment
  (testing "publishing an event reaches the SSE subscriber as a datastar frame"
    (let [conn (java.net.Socket. "127.0.0.1" (int *port*))
          out  (.getOutputStream conn)
          in   (.getInputStream conn)
          req  (str "GET /events/stream HTTP/1.1\r\n"
                    "Host: 127.0.0.1\r\n"
                    "Connection: keep-alive\r\n\r\n")]
      (try
        (.write out (.getBytes req "UTF-8"))
        (.flush out)
        (Thread/sleep 150)
        ;; Drain the headers
        (let [buf (byte-array 4096)]
          (.read in buf))
        ;; Publish an event; the subscriber should receive a Datastar
        ;; merge-fragments frame for .event-list.
        (cch-events/publish!
          {:id 123 :timestamp "2026-04-14T00:00:00"
           :hook_name "scope-lock" :event_type "PreToolUse"
           :tool_name "Edit" :file_path "/tmp/x"
           :decision "allow" :reason "ok"})
        (Thread/sleep 150)
        (let [buf  (byte-array 8192)
              n    (.read in buf)
              body (String. buf 0 n "UTF-8")]
          (is (re-find #"event: datastar-patch-elements" body))
          (is (re-find #"data: selector \.event-rows" body))
          (is (re-find #"data: mode prepend" body))
          (is (re-find #"scope-lock" body)))
        (finally
          (.close conn))))))

(deftest test-dashboard-renders
  (testing "/ renders the overview page"
    (let [resp (http/get (url "/"))]
      (is (= 200 (:status resp)))
      (is (str/includes? (:body resp) "overview"))))
  (testing "/events renders the new events page"
    (let [resp (http/get (url "/events"))]
      (is (= 200 (:status resp)))
      (is (str/includes? (get-in resp [:headers "content-type"]) "text/html"))
      (is (str/includes? (:body resp) "cch · events"))
      (is (str/includes? (:body resp) "dense-table"))
      (is (str/includes? (:body resp) "cch.css"))
      (is (str/includes? (:body resp) "JetBrains")))))

(deftest test-dashboard-filters-applied
  (testing "filter query params flow through to query-events"
    (let [resp (http/get (url "/events?hook=event-log&limit=5"))]
      (is (= 200 (:status resp)))
      (is (str/includes? (:body resp) "selected=\"selected\""))
      (is (str/includes? (:body resp) "event-log")))))

;; ---------------------------------------------------------------------------
;; bundle->fc-shape — falls back to data-bundle stats for non-Claude tiles.
;; The statusLine cache is Claude-only; without this helper, /usage tiles for
;; other agents render `—` even when their chart has data.
;; ---------------------------------------------------------------------------

(deftest bundle->fc-shape-returns-nil-on-empty-bundle
  (let [f @#'cch.server/bundle->fc-shape]
    (is (nil? (f nil)))
    (is (nil? (f {})) "no :last-pct means nothing to render")))

(deftest bundle->fc-shape-fills-tiles-from-data
  (let [f       @#'cch.server/bundle->fc-shape
        result  (f {:last-pct   42.3
                    :projection {:proj 57.45 :band {:lo 50.2 :hi 64.8}}
                    :resets-at  2000
                    :now        500
                    :rate-phr   11.93})]
    (is (= 42 (:current_pct result))      "current_pct rounds last-pct")
    (is (= 57.5 (:projected_pct result))  "projected_pct rounds to 1 decimal")
    (is (= {:lo 50 :hi 65} (:band result)) "band lo/hi round to ints")
    (is (= 11.9 (:local_rate_phr result)) "rate-phr rounds to 1 decimal")
    (is (= 1500 (:secs_left result))      "secs_left = resets-at − now")))

(deftest bundle->fc-shape-omits-fields-without-source
  (let [f      @#'cch.server/bundle->fc-shape
        result (f {:last-pct  21.0
                   :resets-at 1000
                   :now       400})]
    (is (= 21 (:current_pct result)))
    (is (= 600 (:secs_left result)))
    (is (not (contains? result :projected_pct)))
    (is (not (contains? result :band)))
    (is (not (contains? result :local_rate_phr)))))

(deftest bundle->fc-shape-secs-left-floors-at-zero
  (let [f      @#'cch.server/bundle->fc-shape
        result (f {:last-pct  10.0
                   :resets-at 100
                   :now       500})]
    (is (= 0 (:secs_left result))
        "negative seconds (window already reset) clamps to 0, not exposed as negative")))

(deftest usage-page-offers-agy-source
  (let [resp (http/get (url "/usage?agent=agy"))]
    (is (= 200 (:status resp)))
    (is (str/includes? (:body resp) ">AGY</a>"))
    (is (str/includes? (:body resp)
                       "class=\"filter-tab active\" href=\"/usage?agent=agy\"")))
  (testing "the product name is accepted as an alias"
    (is (= "agy" (#'server/parse-agent {:agent "antigravity"})))))
