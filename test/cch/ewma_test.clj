(ns cch.ewma-test
  (:require [clojure.test :refer [deftest is testing]]
            [cch.ewma :as ewma]))

(deftest ewma-step-seeding
  (testing "first sample seeds with the observed rate"
    (is (= 2.5 (ewma/ewma-step nil 2.5 1.0 1.0)))))

(deftest ewma-step-relaxation
  (testing "with dt = tau, new weight is 1 - 1/e ≈ 0.632"
    (let [result (ewma/ewma-step 0.0 1.0 1.0 1.0)]
      (is (< 0.63 result 0.64))))
  (testing "with dt ≪ tau, the estimate barely moves"
    (let [result (ewma/ewma-step 0.0 10.0 0.01 1.0)]
      (is (< result 0.11))))
  (testing "with dt ≫ tau, the estimate snaps to the new rate"
    (let [result (ewma/ewma-step 0.0 5.0 100.0 1.0)]
      (is (< 4.99 result 5.01)))))

(defn- snap [ts pct] {:ts ts :pct pct :resets-at (+ ts (* 7 86400))})

(deftest projection
  (testing "on-pace extrapolation lands near 100% at reset"
    ;; starting at 50% with 7d/2 = 84h left, at the target rate (0.595 %/hr)
    ;; you land at 50 + 0.595*84 ≈ 100%
    (is (< 99.0 (ewma/project-end-of-window 0 50.0 (/ 100.0 (* 7 24)) (* 7 86400 0.5)) 101.0)))
  (testing "zero pace means no further burn — projection = current"
    (is (= 42.0 (ewma/project-end-of-window 0 42.0 0.0 (* 5 3600)))))
  (testing "projection clamps at 0 (negative rate shouldn't appear, but defend)"
    (is (= 0.0 (ewma/project-end-of-window 0 5.0 -10.0 (* 3 3600)))))
  (testing "past-reset resets_at collapses to current pct"
    (is (= 30.0 (ewma/project-end-of-window 1000 30.0 5.0 500)))))

(deftest fold-empty-and-short-input
  (testing "empty input returns nil"
    (is (nil? (ewma/fold-ewma []))))
  (testing "single snapshot returns nil (no transitions)"
    (is (nil? (ewma/fold-ewma [(snap 0 10)]))))
  (testing "two identical-pct snapshots return nil (no real transitions)"
    (is (nil? (ewma/fold-ewma [(snap 0 10) (snap 3600 10)])))))

(deftest fold-basic-rate
  (testing "constant 1 %/hr for several hours ends near 1 %/hr"
    (let [snaps (for [i (range 8)] (snap (* i 3600) (double i)))
          {:keys [fast slow samples]} (ewma/fold-ewma snaps)]
      (is (= 7 samples))
      (is (< 0.99 fast 1.01))
      (is (< 0.85 slow 1.01)))))

(deftest fold-reset-handling
  (testing "a negative Δpct is treated as a window reset, not a huge negative rate"
    (let [snaps [(snap 0 95)
                 (snap 3600 96)    ; +1 %/hr
                 (snap 7200 2)     ; reset rolled forward
                 (snap 10800 3)]   ; +1 %/hr
          {:keys [fast slow samples]} (ewma/fold-ewma snaps)]
      (is (= 2 samples))
      (is (pos? fast))
      (is (pos? slow)))))

(deftest fold-folds-close-samples
  (testing "samples closer than min-gap-hr (15min) get folded forward"
    (let [snaps [(snap 0 5)
                 (snap 1 5)          ; 1s — fold
                 (snap 60 5)         ; 1min — fold
                 (snap 3600 5)       ; 1hr from anchor, zero Δpct → real rate-0 sample
                 (snap 7200 6)       ; +1/hr
                 (snap 10800 7)]     ; +1/hr
          {:keys [samples]} (ewma/fold-ewma snaps)]
      (is (= 3 samples))))

  (testing "long quiet stretches drag the EWMAs down toward 0"
    (let [snaps (concat
                  ;; 4 hours of steady 1%/hr burn
                  (for [i (range 5)] (snap (* i 3600) (double i)))
                  ;; then 8 hours of nothing
                  [(snap (+ (* 4 3600) (* 8 3600)) 4.0)])
          {:keys [slow]} (ewma/fold-ewma snaps)]
      (is (< slow 0.5) "8h of zero burn after 1%/hr should pull slow EWMA below 0.5"))))
