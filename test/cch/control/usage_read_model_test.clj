(ns cch.control.usage-read-model-test
  (:require [cch.control.usage-read-model :as read-model]
            [clojure.test :refer [deftest is testing]]))

(defn- observation [cursor observed-at window pct reset]
  {:cursor cursor
   :observed-at observed-at
   :agent "claude-code"
   :window window
   :used-percentage pct
   :resets-at reset})

(deftest builds-bounded-monotone-privacy-safe-inputs
  (let [now 2000000000000
        now-seconds (quot now 1000)
        current-reset (+ now-seconds 3600)
        prior-resets (map #(- now-seconds (* % 18000)) (range 1 15))
        current [(observation 3 (- now 120000) "five_hour" 10 current-reset)
                 ;; Replayed later by cursor, but older by observation time.
                 (observation 99 (- now 180000) "five_hour" 9 current-reset)
                 ;; A stale decrease is dropped by the monotone pass.
                 (observation 4 (- now 60000) "five_hour" 8 current-reset)
                 (observation 5 (- now 30000) "five_hour" 12 current-reset)]
        history (map-indexed
                  (fn [index reset]
                    (observation (+ 100 index) (- now (* (inc index) 18000000))
                                 "five_hour" (+ 20 index) reset))
                  prior-resets)
        result (read-model/from-observations (concat current history) now)
        input (get-in result [:agents "claude-code" "five_hour"])]
    (is (= current-reset (:resets-at input)))
    (is (= 4 (:sample-count input)))
    (is (= [9.0 10.0 12.0] (mapv :used-percentage (:samples input))))
    (is (= 12 (count (:historical-finals input))))
    (is (= #{:generated-at :agents} (set (keys result))))
    (is (= #{:resets-at :sample-count :samples :historical-finals}
           (set (keys input))))
    (is (every? #(= #{:observed-at :used-percentage} (set (keys %)))
                (:samples input)))
    (testing "no source identity can enter the read model"
      (is (not-any? #(contains? (set (tree-seq coll? seq result)) %)
                    ["runner-a" "session-a" "/private/path"])))))
