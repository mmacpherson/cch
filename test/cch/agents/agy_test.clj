(ns cch.agents.agy-test
  (:require [babashka.fs :as fs]
            [cch.agents.agy :as agy]
            [cheshire.core :as json]
            [clojure.test :refer [deftest is testing]])
  (:import (java.time Instant)))

(deftest normalizes-documented-weekly-quota
  (let [reset "2026-07-06T07:50:32Z"
        out   (agy/normalize-status-payload
                {:session_id "agy-session"
                 :quota {:gemini-weekly
                         {:remaining_fraction 0.9378
                          :reset_time reset
                          :reset_in_seconds 560580}}}
                0)]
    (is (< (Math/abs
             (- 6.22
                (-> out :rate_limits :seven_day :used_percentage)))
           0.000001))
    (is (= (.getEpochSecond (Instant/parse reset))
           (get-in out [:rate_limits :seven_day :resets_at])))
    (is (= 0.9378
           (get-in out [:quota :gemini-weekly :remaining_fraction]))
        "the original AGY quota payload remains available")))

(deftest reset-seconds-fallback-and-explicit-five-hour-bucket
  (let [out (agy/normalize-status-payload
              {:quota {"gemini-5h" {:remaining_fraction 0.25
                                    :reset_in_seconds 600}}}
              1000)]
    (is (= {:used_percentage 75.0 :resets_at 1600}
           (get-in out [:rate_limits :five_hour])))))

(deftest same-window-uses-most-consumed-bucket
  (let [limits (agy/agy-rate-limits
                 {:flash-weekly {:remaining_fraction 0.8
                                 :reset_time "2026-07-30T00:00:00Z"}
                  :pro-weekly   {:remaining_fraction 0.2
                                 :reset_time "2026-07-30T00:00:00Z"}}
                 0)]
    (is (= 80.0 (get-in limits [:seven_day :used_percentage])))))

(deftest unknown-period-is-not-misclassified
  (is (= {}
         (agy/agy-rate-limits
           {:future-daily {:remaining_fraction 0.5
                           :reset_time "2026-07-30T00:00:00Z"}}
           0))))

(deftest coalesces-repeated-status-line-payloads
  (agy/reset-capture-state!)
  (let [working {:session_id "coalesce-session"
                 :agent_state "working"
                 :context_window {:used_percentage 3.0}
                 :rate_limits {:seven_day {:used_percentage 1.7
                                           :resets_at 200000}}}
        idle    (assoc working
                       :agent_state "idle"
                       :context_window {:used_percentage 3.3})
        changed (assoc-in working
                          [:rate_limits :seven_day :used_percentage]
                          2.1)]
    (is (true? (agy/should-capture? working 1000))
        "first payload is retained")
    (is (false? (agy/should-capture? working 1300))
        "exact 300ms repeat is coalesced")
    (is (true? (agy/should-capture? idle 1600))
        "transition to idle retains final context")
    (is (false? (agy/should-capture? idle 1900))
        "repeated idle payload is coalesced")
    (is (true? (agy/should-capture? changed 2200))
        "quota changes are retained immediately")
    (is (false? (agy/should-capture? changed 31999)))
    (is (true? (agy/should-capture? changed 32200))
        "unchanged activity gets a 30-second heartbeat")))

(deftest payload-without-session-is-not-coalesced
  (agy/reset-capture-state!)
  (is (true? (agy/should-capture? {:agent_state "working"} 1000)))
  (is (true? (agy/should-capture? {:agent_state "working"} 1001))))

(deftest install-and-uninstall-round-trip-status-line
  (let [dir          (str (fs/create-temp-dir {:prefix "agy-agent-"}))
        config-path  (str dir "/settings.json")
        adapter-path (str dir "/cch-agy-statusline.sh")
        saved-path   (str dir "/backup.json")
        previous     {:type "command" :command "/opt/user/status.sh"}]
    (try
      (spit config-path
            (json/generate-string
              {:model "Gemini 3.1 Pro (High)"
               :statusLine previous}))
      (agy/install! :config-path config-path
                    :adapter-path adapter-path
                    :saved-path saved-path)
      (let [installed (json/parse-string (slurp config-path) true)]
        (is (= {:type "command"
                :command adapter-path
                :enabled true}
               (:statusLine installed)))
        (is (= "Gemini 3.1 Pro (High)" (:model installed)))
        (is (fs/executable? adapter-path))
        (is (re-find #"X-CCH-Agent: agy" (slurp adapter-path))))
      (testing "a second install is idempotent and retains the original backup"
        (agy/install! :config-path config-path
                      :adapter-path adapter-path
                      :saved-path saved-path)
        (is (= previous
               (:status_line
                 (json/parse-string (slurp saved-path) true)))))
      (let [{:keys [restored]}
            (agy/uninstall! :config-path config-path
                            :adapter-path adapter-path
                            :saved-path saved-path)
            uninstalled (json/parse-string (slurp config-path) true)]
        (is restored)
        (is (= previous (:statusLine uninstalled)))
        (is (not (fs/exists? adapter-path)))
        (is (not (fs/exists? saved-path))))
      (finally
        (fs/delete-tree dir)))))

;; --- Lifecycle hook dispatch ---

(deftest dispatcher-hooks-has-correct-shape
  (let [block (agy/dispatcher-hooks :host "127.0.0.1" :port 8888)
        cch   (get block agy/hook-key)]
    (testing "cch owns a single named hook key"
      (is (= [agy/hook-key] (keys block))))
    (testing "Pre/PostToolUse use the grouped matcher+hooks shape"
      (doseq [ev ["PreToolUse" "PostToolUse"]]
        (let [grp (first (get cch ev))]
          (is (= "*" (get grp "matcher")))
          (is (= "command" (get-in grp ["hooks" 0 "type"])))
          (is (re-find #"X-CCH-Agent: agy" (get-in grp ["hooks" 0 "command"])))
          (is (re-find (re-pattern (str "/dispatch/" ev))
                       (get-in grp ["hooks" 0 "command"]))))))
    (testing "Stop uses the flat handler-list shape (no matcher wrapper)"
      (let [h (first (get cch "Stop"))]
        (is (nil? (get h "matcher")))
        (is (= "command" (get h "type")))
        (is (re-find #"/dispatch/Stop" (get h "command")))))))

(deftest install-hooks-preserves-user-hooks-and-round-trips
  (let [dir  (str (fs/create-temp-dir {:prefix "agy-hooks-"}))
        path (str dir "/hooks.json")]
    (try
      ;; Pre-existing user hook that must survive install + uninstall.
      (spit path (json/generate-string {"user-lint" {"PostToolUse" []}}))
      (agy/install-hooks! :config-path path)
      (let [after (json/parse-string (slurp path))]
        (is (contains? after "user-lint") "user hook preserved")
        (is (contains? after agy/hook-key) "cch hook added"))
      ;; Re-install is idempotent (still exactly the two keys).
      (agy/install-hooks! :config-path path)
      (is (= #{"user-lint" agy/hook-key}
             (set (keys (json/parse-string (slurp path))))))
      ;; Uninstall strips only cch's key.
      (let [{:keys [removed]} (agy/uninstall-hooks! :config-path path)]
        (is removed)
        (is (= {"user-lint" {"PostToolUse" []}}
               (json/parse-string (slurp path)))))
      (finally (fs/delete-tree dir)))))

(deftest uninstall-hooks-deletes-file-when-only-cch-remains
  (let [dir  (str (fs/create-temp-dir {:prefix "agy-hooks-"}))
        path (str dir "/hooks.json")]
    (try
      (agy/install-hooks! :config-path path)
      (is (fs/exists? path))
      (agy/uninstall-hooks! :config-path path)
      (is (not (fs/exists? path)) "file removed once cch's key is the last one")
      (testing "uninstall on an absent file is a no-op"
        (is (false? (:removed (agy/uninstall-hooks! :config-path path)))))
      (finally (fs/delete-tree dir)))))

(deftest normalize-event-payload-maps-agy-shape-to-cch
  (let [out (agy/normalize-event-payload
              "PreToolUse"
              {:conversationId "conv-123"
               :workspacePaths ["/home/example/proj" "/tmp"]
               :stepIdx 19
               :toolCall {:name "run_command" :args {:CommandLine "npm test"}}})]
    (is (= "PreToolUse" (:hook_event_name out)))
    (is (= "conv-123"   (:session_id out)))
    (is (= "/home/example/proj" (:cwd out)) "first workspace path becomes cwd")
    (is (= "run_command" (:tool_name out)))
    (is (= {:CommandLine "npm test"} (:tool_input out)))
    (is (= 19 (:stepIdx out)) "original AGY fields retained for the extra blob"))
  (testing "a payload with no toolCall (e.g. Stop) omits tool keys"
    (let [out (agy/normalize-event-payload
                "Stop" {:conversationId "c" :executionNum 1 :terminationReason "model_stop"})]
      (is (= "Stop" (:hook_event_name out)))
      (is (= "c" (:session_id out)))
      (is (nil? (:tool_name out)))
      (is (nil? (:tool_input out))))))

(deftest hook-response-matches-agy-contract
  (testing "PreToolUse with no decision returns explicit allow (empty = deny in AGY)"
    (is (= {"decision" "allow"}
           (json/parse-string (agy/->hook-response "PreToolUse" nil)))))
  (testing "PreToolUse maps cch :deny/:ask to AGY vocab, carrying reason"
    (is (= {"decision" "deny" "reason" "nope"}
           (json/parse-string (agy/->hook-response "PreToolUse" {:decision :deny :reason "nope"}))))
    (is (= {"decision" "ask"}
           (json/parse-string (agy/->hook-response "PreToolUse" {:decision :ask})))))
  (testing "PostToolUse and Stop return a neutral empty object"
    (is (= {} (json/parse-string (agy/->hook-response "PostToolUse" nil))))
    (is (= {} (json/parse-string (agy/->hook-response "Stop" nil))))))

(deftest find-config-up-is-nil-safe
  ;; Regression: agy payloads can lack workspacePaths, so cwd resolves to nil;
  ;; find-config-up must not NPE on a nil dir (was (fs/path nil)).
  (is (nil? (@(requiring-resolve 'cch.config/find-config-up) nil ".cch-config.yaml"))))
