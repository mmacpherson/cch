(ns hooks.codex-usage-capture-test
  (:require [clojure.test :refer [deftest is testing]]
            [cheshire.core :as json]
            [hooks.codex-usage-capture :as h]))

(def sample-token-count-line
  (json/generate-string
    {:timestamp "2026-05-27T21:39:57.206Z"
     :type "event_msg"
     :payload {:type "token_count"
               :info  {:total_token_usage {:total_tokens 11009}
                       :model_context_window 258400}
               :rate_limits {:limit_id "codex"
                             :primary   {:used_percent 6.0   :window_minutes 300   :resets_at 1779930704}
                             :secondary {:used_percent 21.0  :window_minutes 10080 :resets_at 1780174277}
                             :plan_type "plus"}}}))

(def sample-non-rate-line
  (json/generate-string {:type "response_item" :payload {:role "assistant"}}))

(deftest parse-jsonl-handles-blank-and-corrupt-lines
  (let [content (str sample-token-count-line "\n"
                     "{ not valid json\n"
                     "\n"
                     sample-non-rate-line "\n")
        out (h/parse-jsonl-lines content)]
    (is (= 2 (count out)) "blank + corrupt lines are dropped, valid lines kept")
    (is (every? map? out))))

(deftest latest-token-count-picks-most-recent
  (let [earlier  (-> (json/parse-string sample-token-count-line true)
                     (assoc-in [:payload :rate_limits :primary :used_percent] 5.0))
        later    (-> (json/parse-string sample-token-count-line true)
                     (assoc-in [:payload :rate_limits :primary :used_percent] 12.0))
        non-tc   (json/parse-string sample-non-rate-line true)
        result   (h/latest-token-count [earlier non-tc later])]
    (is (= 12.0 (get-in result [:payload :rate_limits :primary :used_percent])))))

(deftest latest-token-count-returns-nil-when-absent
  (is (nil? (h/latest-token-count [{:type "response_item"}]))))

(deftest codex->claude-rate-limits-maps-by-window-minutes
  (testing "primary (300m) → five_hour; secondary (10080m) → seven_day"
    (let [out (h/codex->claude-rate-limits
                {:primary   {:used_percent 6.0  :window_minutes 300   :resets_at 1779930704}
                 :secondary {:used_percent 21.0 :window_minutes 10080 :resets_at 1780174277}})]
      (is (= {:used_percentage 6.0  :resets_at 1779930704} (get out "five_hour")))
      (is (= {:used_percentage 21.0 :resets_at 1780174277} (get out "seven_day"))))))

(deftest codex->claude-rate-limits-swapped-positions
  (testing "if codex ever sent the windows in the opposite slots, mapping still works"
    (let [out (h/codex->claude-rate-limits
                {:primary   {:used_percent 21.0 :window_minutes 10080 :resets_at 1780174277}
                 :secondary {:used_percent 6.0  :window_minutes 300   :resets_at 1779930704}})]
      (is (= {:used_percentage 6.0  :resets_at 1779930704} (get out "five_hour")))
      (is (= {:used_percentage 21.0 :resets_at 1780174277} (get out "seven_day"))))))

(deftest codex->claude-rate-limits-drops-unknown-windows
  (testing "an unrecognized window_minutes value is dropped, not mapped"
    (let [out (h/codex->claude-rate-limits
                {:primary {:used_percent 5.0 :window_minutes 60 :resets_at 12345}
                 :secondary {:used_percent 21.0 :window_minutes 10080 :resets_at 67890}})]
      (is (= #{"seven_day"} (set (keys out)))))))

(deftest codex->claude-rate-limits-drops-zero-pct
  (testing "0% windows are dropped — they carry no info and their rolling resets_at poisons forecasts"
    (let [out (h/codex->claude-rate-limits
                {:primary   {:used_percent 0.0 :window_minutes 300   :resets_at 99999}
                 :secondary {:used_percent 21.0 :window_minutes 10080 :resets_at 67890}})]
      (is (= #{"seven_day"} (set (keys out)))))
    (let [out (h/codex->claude-rate-limits
                {:primary   {:used_percent 0.0 :window_minutes 300   :resets_at 99999}
                 :secondary {:used_percent 0.0 :window_minutes 10080 :resets_at 99998}})]
      (is (empty? out)))))

(deftest build-snapshot-emits-claude-shaped-payload
  (let [input  {:session_id "sess-abc"
                :model      "gpt-5.5"}
        parsed [(json/parse-string sample-token-count-line true)]
        args   (h/build-snapshot input parsed)]
    (is (= "codex"     (:agent args)))
    (is (= "sess-abc"  (:session-id args)))
    (is (= "gpt-5.5"   (:model-id args)))
    (let [payload (json/parse-string (:payload args) true)]
      (is (= "sess-abc" (:session_id payload)))
      (is (= 6.0  (get-in payload [:rate_limits :five_hour :used_percentage])))
      (is (= 21.0 (get-in payload [:rate_limits :seven_day :used_percentage])))
      (is (= 1779930704 (get-in payload [:rate_limits :five_hour :resets_at]))))))

(deftest build-snapshot-returns-nil-when-no-token-count
  (is (nil? (h/build-snapshot {:session_id "s"} [{:type "response_item"}]))))

(deftest build-snapshot-returns-nil-when-rate-limits-empty
  (testing "edge case: token_count line present but no recognized windows"
    (let [tc {:type "event_msg"
              :payload {:type "token_count"
                        :rate_limits {:primary {:used_percent 5.0
                                                :window_minutes 999
                                                :resets_at 1}}}}]
      (is (nil? (h/build-snapshot {:session_id "s"} [tc])))))
  (testing "all-zero windows are dropped → nil snapshot"
    (let [tc {:type "event_msg"
              :payload {:type "token_count"
                        :rate_limits {:primary   {:used_percent 0.0
                                                  :window_minutes 300
                                                  :resets_at 99999}
                                      :secondary {:used_percent 0.0
                                                  :window_minutes 10080
                                                  :resets_at 99998}}}}]
      (is (nil? (h/build-snapshot {:session_id "s"} [tc]))))))

(deftest hook-noop-for-non-codex-agent
  (testing "handler returns nil and does no logging when :cch/agent isn't 'codex'"
    ;; No exception, no logging. The hook returns nil regardless.
    (is (nil? (h/handler-fn {:cch/agent "claude-code"
                             :transcript_path "/nonexistent/path.jsonl"})))))
