(ns cch.usage-model-test
  (:require [cch.numeric :as num]
            [cch.usage-model :as m]
            [clojure.test :refer [deftest is testing]])
  (:import (java.time ZoneId)))

(def ^:private utc (ZoneId/of "UTC"))
(def ^:private h 3600)
(def ^:private day 86400)
;; Monday 2026-06-01 00:00 UTC — hour-of-week 0 in UTC.
(def ^:private t0 1780272000)
(def ^:private seven (m/specs :seven-day))

(defn- rows
  "Hourly rows for a window ending at `end` with cumulative pct by hour offset."
  [end start pcts-by-hour]
  (for [[k p] pcts-by-hour] {:resets-at end :hour (+ start (* k h)) :pct p}))

(deftest hour-of-week-is-local
  (is (= 0 (m/hour-of-week utc t0)))
  (is (= 25 (m/hour-of-week utc (+ t0 day h))))
  (testing "a zone offset shifts the local hour (Monday 09:00 JST)"
    (is (= 9 (m/hour-of-week (ZoneId/of "Asia/Tokyo") t0)))))

(deftest windows-cluster-jittered-resets-and-track-monotone-max
  (let [end (+ t0 (* 7 day))
        start (- end (* 7 day))
        ws (m/windows (concat (rows end start {0 1.0 1 5.0})
                              ;; same window, reset reported 7s later
                              (rows (+ end 7) start {2 4.0 3 9.0}))
                      seven)]
    (is (= 1 (count ws)))
    (is (= [[start 1.0] [(+ start h) 5.0] [(+ start (* 2 h)) 5.0] [(+ start (* 3 h)) 9.0]]
           (:cum (first ws)))
        "a stale lower report does not pull the running max down")))

(deftest windows-detect-early-reset-and-cap
  (let [end1 (+ t0 (* 7 day))
        start1 (- end1 (* 7 day))
        ;; second window starts (first use) two days into the first
        start2 (+ start1 (* 2 day))
        end2 (+ start2 (* 7 day))
        [w1 w2] (m/windows (concat (rows end1 start1 {0 10.0 5 60.0 6 100.0})
                                   (rows end2 start2 {0 3.0}))
                           seven)]
    (is (= start2 (:eff-end w1)) "early reset ends the first window at the second's first use")
    (is (= (+ start1 (* 7 h)) (:cap w1)) "cap is the end of the first hour at 100%")
    (is (= (:cap w1) (m/live-end w1)))
    (is (= end2 (:eff-end w2)))))

(deftest hour-series-gaps-are-live-zero-and-capped-hours-excluded
  (let [end (+ t0 (* 7 day))
        start (- end (* 7 day))
        ws (m/windows (rows end start {0 10.0 1 100.0}) seven)
        s (m/hour-series ws (+ start (* 4 h) 1800))]
    (is (= [1.0 10.0] (get s start)))
    (is (= [1.0 90.0] (get s (+ start h))))
    (is (= 0.0 (first (get s (+ start (* 2 h))))) "hours after the cap carry no exposure")
    (let [open-ws (m/windows (rows end start {0 10.0}) seven)
          s2 (m/hour-series open-ws (+ start (* 4 h) 1800))]
      (is (= [1.0 0.0] (get s2 (+ start (* 2 h)))) "hours without reports are live with zero usage")
      (is (= 0.5 (first (get s2 (+ start (* 4 h))))) "the current hour is live for its elapsed fraction"))))

(deftest profile-normalizes-and-follows-activity
  (let [series (into (sorted-map)
                     (for [k (range (* 28 24))
                           :let [t (+ t0 (* k h))
                                 hod (mod k 24)]]
                       [t [1.0 (if (<= 9 hod 16) 2.0 0.0)]]))
        prof (m/profile series utc)]
    (is (= 168 (count prof)))
    (is (< (Math/abs (- 1.0 (/ (reduce + prof) 168.0))) 1e-9) "mean 1")
    (is (> (nth prof 12) (* 5 (nth prof 3))) "midday outweighs 3am")
    (is (every? pos? prof) "the floor keeps every hour possible")))

(defn- simulate
  "Daily usage from the model's own generative process."
  [[kappa al0 be0 a0 b0 d] days seed]
  (let [r (num/rng seed)]
    (loop [i 0 lam (num/gamma-sample r al0 be0) out []]
      (if (= i days)
        out
        (let [lam (if (< (.nextDouble r) (- 1.0 d)) (num/gamma-sample r al0 be0) lam)
              on? (< (.nextDouble r) (/ a0 (+ a0 b0)))
              y (if on? (Math/rint (num/gamma-sample r (* kappa 24.0) lam)) 0.0)]
          (recur (inc i) lam (conj out [(+ t0 (* i day)) 24.0 y])))))))

(deftest fit-prefers-true-parameters-over-perturbed
  (let [truth [0.05 6.0 30.0 3.0 1.0 0.8]
        blks (simulate truth 400 11)
        ll (fn [th] (:loglik (m/run-filter blks th seven true)))
        {:keys [theta]} (m/fit blks seven (:x0 seven))]
    (is (> (ll theta) (ll [0.2 6.0 30.0 3.0 1.0 0.8])))
    (is (>= (ll theta) (- (ll truth) 1.0)) "the optimizer reaches at least the truth's likelihood")))

(deftest predict-properties
  (let [theta [0.05 6.0 30.0 3.0 1.0 0.8]
        st [6.0 30.0 3.0 1.0]
        fut [[10.0 true] [24.0 false] [24.0 false]]
        p (m/predict st theta 20.0 fut true)]
    (testing "demand never falls below current usage"
      (is (<= 20.0 (:lo p) (:median p) (:hi p))))
    (testing "no time left means the forecast is the current value"
      (is (= 20.0 (:median (m/predict st theta 20.0 [] false)))))
    (testing "a heavier intensity state (larger beta, so smaller rate lambda) raises the forecast"
      (is (> (:median (m/predict [6.0 90.0 3.0 1.0] theta 20.0 fut true)) (:median p))))
    (testing "seeded, so identical inputs give identical output"
      (is (= (dissoc p :draws) (dissoc (m/predict st theta 20.0 fut true) :draws))))))

(deftest crps-matches-closed-forms
  (testing "a point forecast scores its absolute error"
    (is (< (Math/abs (- 3.0 (m/crps (double-array (repeat 100 10.0)) 13.0 100.0))) 1e-12)))
  (testing "uniform draws on [0,1] against 0.5: 1/4 - 1/6 = 1/12"
    (let [n 2001 xs (double-array (map #(/ (double %) (dec n)) (range n)))]
      (is (< (Math/abs (- (/ 1.0 12.0) (m/crps xs 0.5 100.0))) 1e-3))))
  (testing "draws beyond the cap score as the cap"
    (is (< (m/crps (double-array (repeat 10 250.0)) 100.0 100.0) 1e-12))))

(deftest shrinkage-interpolates-between-cell-and-target
  (let [stats {:usage (vec (repeat 168 2.0)) :exposure (vec (repeat 168 1.0))}
        spiky (assoc stats :usage (assoc (vec (repeat 168 0.0)) 10 336.0))
        target (vec (repeat 168 1.0))]
    (is (= target (m/shrunk-rates stats target 5.0)) "a flat cell stays flat")
    (is (= 168.0 (nth (m/shrunk-rates spiky target 0.0) 10)) "k = 0 is the cell alone")
    (is (< (nth (m/shrunk-rates spiky target 1000.0) 10) 1.2) "large k approaches the target")
    (is (= target (m/pooled-rates [stats stats])))))

(deftest future-blocks-split-at-anchor-hour
  (let [prof (vec (repeat 168 1.0))
        ;; 22:00 Monday UTC -> blocks break at 04:00
        t (+ t0 (* 22 h))
        fb (m/future-blocks prof utc seven t (+ t (* 36 h)))]
    (is (= [[6.0 true] [24.0 false] [6.0 false]] fb))))

(deftest future-pieces-respect-steps-hours-and-blocks
  (let [prof (vec (repeat 168 1.0))
        t (+ t0 (* 3 h) 1800)                 ; 03:30 Monday UTC
        ps (m/future-pieces prof utc seven t (+ t (* 3 h)) h)]
    (is (= [(+ t0 (* 4 h)) (+ t0 (* 5 h)) (+ t0 (* 6 h)) (+ t0 (* 6 h) 1800)] (mapv first ps)))
    (is (= [0.5 1.0 1.0 0.5] (mapv second ps)))
    (is (= [0 1 1 1] (mapv #(nth % 2) ps)) "04:00 starts the next 7d block")))

(deftest predict-path-is-monotone-and-ends-at-the-forecast
  (let [theta [0.05 6.0 30.0 3.0 1.0 0.8]
        st [6.0 30.0 3.0 1.0]
        prof (vec (repeat 168 1.0))
        t (+ t0 (* 22 h))
        path (m/predict-path st theta 20.0 (m/future-pieces prof utc seven t (+ t (* 36 h)) h) true)]
    (is (= 36 (count path)))
    (is (every? (fn [[a b]] (<= (:median a) (:median b))) (partition 2 1 path)) "cumulative demand never falls")
    (is (every? #(<= 20.0 (:lo %) (:q25 %) (:median %) (:q75 %) (:hi %)) path))
    (is (<= (:p-cap (first path)) (:p-cap (peek path))))))

(deftest eb-rates-pools-noise-and-keeps-real-differences
  (let [target (vec (repeat 168 1.0))
        r (num/rng 5)
        noisy-week (fn [mean] (* mean (num/gamma-sample r 2.0 2.0)))]
    (testing "a cell that is only noise around the target pools almost fully"
      (let [weekly (vec (for [_ (range 168)] (vec (repeatedly 12 #(noisy-week 1.0)))))
            {:keys [rates k]} (m/eb-rates weekly target)]
        (is (> k 20.0))
        (is (< (reduce max (map #(Math/abs (- % 1.0)) rates)) 0.35))))
    (testing "a cell whose profile really differs keeps its shape"
      (let [true-rate (fn [h] (if (< (mod h 24) 12) 1.8 0.2))
            weekly (vec (for [h (range 168)] (vec (repeatedly 12 #(noisy-week (true-rate h))))))
            {:keys [rates k]} (m/eb-rates weekly target)]
        (is (< k 5.0))
        (is (> (nth rates 6) 1.4))
        (is (< (nth rates 18) 0.6))))))

(deftest fit-can-hold-parameters-fixed
  (let [blks (simulate [0.05 6.0 30.0 3.0 1.0 0.8] 200 3)
        {:keys [x theta]} (m/fit blks seven (:x0 seven) :fixed {5 -30.0})]
    (is (= -30.0 (nth x 5)))
    (is (< (nth theta 5) 1e-9) "d pinned at ~0: no memory between blocks")))

(deftest interval-prob-keeps-precision-in-the-upper-tail
  (testing "log P matches a 40-digit reference where CDF differencing cancels"
    ;; [y kappa*A alpha beta log-P]; the naive difference returned 1e-300 here.
    (doseq [[y k al be expected] [[8 0.0170749 39.9094 6.66726 -37.22500457]
                                  [2 3.28059 42.9453 0.803522 -38.48696716]
                                  [21 1.35711 10.4581 0.518352 -38.70770448]
                                  [3 4.43641 64.1322 2.16049 -39.34885232]]]
      (is (< (Math/abs (- expected (Math/log (m/interval-prob y 1.0 al be k)))) 1e-3)
          (str [y k al be])))))
