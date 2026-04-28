(ns cch.forecast-test
  (:require [clojure.test :refer [deftest testing is are]]
            [cch.forecast :refer [weighted-prior-params]]))

;; ---------------------------------------------------------------------------
;; weighted-prior-params — pure fn, no DB required
;; ---------------------------------------------------------------------------

(deftest weighted-prior-params-requires-two-windows
  (testing "nil with zero windows"
    (is (nil? (weighted-prior-params []))))
  (testing "nil with one window"
    (is (nil? (weighted-prior-params [{:final_pct 80.0}])))))

(deftest weighted-prior-params-two-identical-weeks
  (testing "mu is the common rate; sigma is at the floor"
    (let [{:keys [mu sigma]} (weighted-prior-params [{:final_pct 92.4}
                                                     {:final_pct 92.4}])
          expected-mu (/ 92.4 (* 7.0 24.0))]
      (is (< (Math/abs (- mu expected-mu)) 1e-6))
      ;; variance is zero → clamped to prior-sigma-floor
      (is (= sigma 0.03)))))

(deftest weighted-prior-params-recency-weighting
  (testing "most-recent week (index 0) is weighted highest"
    ;; Two weeks: recent=100%, older=0%. With decay 0.85, recent has weight 1,
    ;; older has weight 0.85. Weighted mean should be closer to 100%/hr.
    (let [{:keys [mu]} (weighted-prior-params [{:final_pct 100.0}
                                               {:final_pct 0.0}])
          unweighted-mean (/ (/ (+ 100.0 0.0) 2.0) (* 7.0 24.0))]
      (is (> mu unweighted-mean) "recent 100% week should pull mu above unweighted mean"))))

(deftest weighted-prior-params-sigma-reflects-spread
  (testing "wider spread → larger sigma"
    (let [tight (weighted-prior-params [{:final_pct 90.0} {:final_pct 88.0}
                                        {:final_pct 91.0} {:final_pct 89.0}])
          wide  (weighted-prior-params [{:final_pct 100.0} {:final_pct 50.0}
                                        {:final_pct 95.0}  {:final_pct 40.0}])]
      (is (< (:sigma tight) (:sigma wide))))))

(deftest weighted-prior-params-mu-in-plausible-range
  (testing "typical 75-95% weeks produce a mu near 0.5-0.6 %/hr"
    (let [{:keys [mu sigma]}
          (weighted-prior-params [{:final_pct 92.0} {:final_pct 85.0}
                                  {:final_pct 78.0} {:final_pct 90.0}
                                  {:final_pct 95.0}])]
      (is (< 0.4 mu 0.7))
      (is (pos? sigma)))))

(deftest weighted-prior-params-up-to-12-weeks
  (testing "accepts and processes 12 rows without error"
    (let [rows (repeat 12 {:final_pct 88.0})
          {:keys [mu sigma]} (weighted-prior-params rows)]
      (is (some? mu))
      (is (= sigma 0.03)))))
