(ns cch.settings-test
  (:require [cch.settings :as settings]
            [clojure.test :refer [deftest testing is]]))

(deftest forecast-refresh-interval-defaults
  (testing "nil/empty config → default cadence"
    (is (= (* 1000 settings/default-forecast-refresh-seconds)
           (settings/forecast-refresh-interval-ms nil)))
    (is (= (* 1000 settings/default-forecast-refresh-seconds)
           (settings/forecast-refresh-interval-ms {})))))

(deftest forecast-refresh-interval-reads-config
  (testing "explicit seconds are honored"
    (is (= 60000 (settings/forecast-refresh-interval-ms
                   {:forecast {:refresh-interval-seconds 60}}))))
  (testing "floored at the minimum to prevent a hot loop"
    (is (= (* 1000 settings/min-forecast-refresh-seconds)
           (settings/forecast-refresh-interval-ms
             {:forecast {:refresh-interval-seconds 1}})))))
