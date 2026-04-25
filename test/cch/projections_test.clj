(ns cch.projections-test
  (:require [clojure.test :refer [deftest is testing]]
            [cch.projections :as p]))

;; --- helpers ---

(defn- snap [ts pct] {:ts ts :pct (double pct)})

(defn- linear-samples
  "n samples spaced 1h apart, starting at pct=start with a constant
  rate of `rate-per-hr` %/hr."
  [n start rate-per-hr]
  (mapv (fn [i] (snap (* i 3600) (+ start (* i rate-per-hr))))
        (range n)))

(defn- window-info
  "Synthetic window info: now == last sample, reset is `hours-left`
  hours later. last-pct is the last observed pct."
  [observed hours-left]
  (let [last (last observed)]
    {:window-start 0
     :now          (:ts last)
     :resets-at    (+ (:ts last) (long (* hours-left 3600)))
     :last-pct     (:pct last)}))

;; --- rate-samples ---

(deftest rate-samples-coalesces-and-skips-resets
  (testing "anchor carries through tight (<15min) pairs; window-roll jumps anchor"
    (let [obs [(snap 0 5)
               (snap 60 5)        ; 1 min — coalesce, anchor stays at (0,5)
               (snap 3600 6)      ; 1 hr → +1/hr
               (snap 7200 5)      ; Δpct<0 — anchor jumps to (7200,5), no emit
               (snap 10800 7)]    ; vs new anchor: 1hr → +2/hr
          rs (p/rate-samples obs)]
      (is (= 2 (count rs)))
      (is (< 0.99 (:rate (first rs))  1.01))
      (is (< 1.99 (:rate (second rs)) 2.01)))))

;; --- linear-projection (constrained, b≥0) ---

(deftest linear-recovers-linear-trend
  (testing "a perfectly linear series projects forward at the same rate"
    ;; 10 samples at 1h spacing, rate 0.5 %/hr → t∈[0,9]hr, pct∈[0,4.5]
    ;; reset is 24h after the last sample → x_new = 33hr → pred = 16.5
    (let [obs (linear-samples 10 0 0.5)
          win (window-info obs 24)
          {:keys [proj band]} (p/linear-projection obs win)]
      (is (< 16.0 proj 17.0))
      (is (<= (:lo band) proj (:hi band)))
      (testing "perfect fit → tight band"
        (is (< (- (:hi band) (:lo band)) 0.5))))))

(deftest linear-monotone-on-decreasing-data
  (testing "if unconstrained slope is negative, NNLS pins rate at 0"
    (let [obs (mapv (fn [i] (snap (* i 3600) (double (- 10 i)))) (range 5))
          win (window-info obs 24)
          {:keys [rate proj band]} (p/linear-projection obs win)]
      (is (= 0.0 rate) "monotone constraint pins rate at 0 when slope<0")
      (is (>= proj (:last-pct win)) "projection never below current pct")
      (is (>= (:lo band) (:last-pct win)) "band lo clamped at current pct"))))

(deftest linear-needs-three-points
  (is (nil? (p/linear-projection [(snap 0 0) (snap 3600 1)]
                                 (window-info [(snap 0 0) (snap 3600 1)] 24)))))

;; --- bayes-projection ---

(deftest bayes-shrinks-strongly-toward-empirical-prior
  (testing "with the tight empirical prior (μ₀=0.55, σ₀=0.12), a few samples way above prior get pulled hard toward the prior"
    (let [obs (linear-samples 3 0 5.0)             ;; rate 5 %/hr — way above prior
          win (window-info obs 24)
          {:keys [rate]} (p/bayes-projection obs win)]
      (is (< 0.55 rate 2.0)
          "posterior should sit between prior (0.55) and 2.0; old loose prior would let it run to ~5"))))

(deftest bayes-band-stays-bounded-at-long-horizon
  (testing "a 30-day horizon doesn't blow up the band — Brownian variance grows linearly, not quadratically"
    (let [obs (linear-samples 12 0 0.6)
          win (window-info obs (* 30 24))           ;; 30 days out
          {:keys [band proj]} (p/bayes-projection obs win)
          half-width (- (:hi band) proj)]
      (is (< half-width 200.0)
          "old σ²·Δt² model would produce a band wider than 1000% on the high side at 30d"))))

(deftest bayes-band-widens-with-noisy-rates
  (testing "noisy rates produce a wider credible interval than steady rates"
    (let [steady [(snap 0 0) (snap 3600 1) (snap 7200 2) (snap 10800 3)
                  (snap 14400 4) (snap 18000 5)]
          noisy  [(snap 0 0) (snap 3600 1) (snap 7200 5) (snap 10800 6)
                  (snap 14400 6) (snap 18000 12)]
          win-s (window-info steady 24)
          win-n (window-info noisy 24)
          band-s (:band (p/bayes-projection steady win-s))
          band-n (:band (p/bayes-projection noisy win-n))]
      (is (< (- (:hi band-s) (:lo band-s))
             (- (:hi band-n) (:lo band-n)))))))


;; --- loess-smooth ---

(deftest loess-smooth-passes-through-linear
  (testing "a linear input is returned ~unchanged after smoothing"
    (let [obs (linear-samples 20 0 0.5)
          smoothed (p/loess-smooth obs 40 0.2)]
      (is (= 40 (count smoothed)))
      (testing "endpoints close to the truth at first/last sample times"
        (is (< (Math/abs (- (:pct (first smoothed)) 0.0)) 0.5))
        (is (< (Math/abs (- (:pct (last smoothed))
                            (:pct (last obs))))
               0.5))))))

(deftest loess-smooth-flattens-noise
  (testing "the smoothed curve has smaller adjacent jumps than the raw observations"
    (let [;; jagged but trending up — alternating fast/slow 1h gaps
          obs (mapv (fn [i]
                      (let [base (* i 0.5)
                                noise (if (even? i) 0.0 1.0)]
                        (snap (* i 3600) (+ base noise))))
                    (range 16))
          smoothed (p/loess-smooth obs 60 0.2)
          max-jump (fn [pts get-y]
                     (apply max
                            (map (fn [a b] (Math/abs (- (get-y b) (get-y a))))
                                 pts (rest pts))))]
      (is (< (max-jump smoothed :pct) (max-jump obs :pct))))))

(deftest loess-smooth-needs-three-points
  (is (nil? (p/loess-smooth [] 10 0.2)))
  (is (nil? (p/loess-smooth [(snap 0 0)] 10 0.2)))
  (is (nil? (p/loess-smooth [(snap 0 0) (snap 3600 1)] 10 0.2))))

(deftest loess-smooth-is-monotone
  (testing "smoothed curve never decreases — used_percentage can only go up"
    (let [;; A curve with a real plateau followed by a rise — the kernel
          ;; smoother on its own would bow downward at the boundary.
          obs [(snap 0      0.0)
               (snap 3600   1.0)
               (snap 7200   2.0)
               (snap 10800  2.0)
               (snap 14400  2.0)
               (snap 18000  2.0)
               (snap 21600  2.0)
               (snap 25200  3.0)
               (snap 28800  4.5)
               (snap 32400  6.0)]
          smoothed (p/loess-smooth obs 60 0.2)
          ys       (mapv :pct smoothed)]
      (is (every? (fn [[a b]] (>= b a)) (partition 2 1 ys))
          "every adjacent pair satisfies y_{i+1} >= y_i"))))

(deftest isotonic-pav-known-cases
  (testing "already-monotone input returns unchanged"
    (is (= [1.0 2.0 3.0] (p/isotonic-pav [1.0 2.0 3.0]))))
  (testing "single violating pair gets pooled to its mean"
    (is (= [1.5 1.5 3.0] (p/isotonic-pav [2.0 1.0 3.0]))))
  (testing "L2-closest non-decreasing fit on a small classic example"
    ;; pool 4,2,3 → mean 3, then with 5,6 → 3,3,3,5,6
    (is (= [3.0 3.0 3.0 5.0 6.0]
           (p/isotonic-pav [4.0 2.0 3.0 5.0 6.0])))))

;; --- thin-by-time ---

(deftest thin-by-time-collapses-bursts
  (testing "many samples within one bucket collapse to one"
    (let [obs (mapv #(snap (+ 1000 %) (double %)) (range 50)) ; 50 samples in 50s
          thinned (p/thin-by-time obs 900)]                   ; 15-min buckets
      (is (= 1 (count thinned)))))
  (testing "samples spread across buckets keep one each"
    (let [obs [(snap 0 0) (snap 1 0)         ; bucket 0
               (snap 901 1) (snap 902 1)     ; bucket 1
               (snap 1801 2)]                ; bucket 2
          thinned (p/thin-by-time obs 900)]
      (is (= 3 (count thinned)))
      (is (= [0 901 1801] (mapv :ts thinned))))))

;; --- aggregator ---

(deftest all-projections-filters-empty-methods
  (testing "with too few samples, all-projections returns empty (no errors)"
    (is (= [] (p/all-projections [(snap 0 0)]
                                 {:window-start 0 :now 0 :resets-at 86400 :last-pct 0})))))

(deftest all-projections-includes-both-methods-on-rich-data
  (let [obs (linear-samples 12 0 0.6)
        win (window-info obs 48)
        methods (set (map :method (p/all-projections obs win)))]
    (is (contains? methods :linear))
    (is (contains? methods :bayes))
    (is (= 2 (count methods)) "EWMA / trailing-* should no longer appear")))
