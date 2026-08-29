(ns cch.control.usage-forecast-test
  (:require [cch.control.usage-forecast :as usage-forecast]
            [cch.projections :as projections]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(deftest projects-bounded-read-model-with-shared-priors
  (let [now 2000000000000
        reset (+ (quot now 1000) 3600)
        captured (atom nil)
        model {:generated-at now
               :agents
               {"claude-code"
                {"five_hour"
                 {:resets-at reset
                  :sample-count 2
                  :samples [{:observed-at (- now 60000)
                             :used-percentage 12.0}
                            {:observed-at now :used-percentage 14.0}]
                  :historical-finals []}}}}]
    (with-redefs [projections/rate-bayes-projection
                  (fn [observed window-info]
                    (reset! captured {:observed observed :window-info window-info})
                    {:proj 22.5 :band {:lo 18.2 :hi 27.8}})]
      (let [result (usage-forecast/from-read-model model)
            projected (get-in result [:agents "claude-code" "five_hour"])]
        (is (= 14 (:current-pct projected)))
        (is (= 22.5 (:projected-pct projected)))
        (is (= {:lo 18 :hi 28} (:band projected)))
        (is (= {:mu 3.75 :sigma 1.3} (:prior projected)))
        (is (= [{:ts (- (quot now 1000) 60) :pct 12.0}
                {:ts (quot now 1000) :pct 14.0}]
               (:observed @captured)))
        (is (= 3.75 (get-in @captured [:window-info :prior-mu])))
        (testing "the hosted result contains aggregates, not source identity"
          (is (= #{:current-pct :projected-pct :resets-at :seconds-left
                   :sample-count :prior :band :page-data}
                 (set (keys projected)))))))))

(deftest stale-window-without-a-new-observation-does-not-project
  (let [now 2000000000000
        result (usage-forecast/from-read-model
                 {:generated-at now
                  :agents
                  {"codex"
                   {"five_hour"
                    {:resets-at (dec (quot now 1000))
                     :sample-count 1
                     :samples [{:observed-at (- now 60000)
                                :used-percentage 90.0}]
                     :historical-finals [90.0 80.0]}}}})]
    (is (= {} (get-in result [:agents "codex"])))
    (is (not (str/includes? (pr-str result) "90.0")))))
