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
