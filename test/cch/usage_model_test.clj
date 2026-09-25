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
      (is (= p (m/predict st theta 20.0 fut true))))))

(deftest future-blocks-split-at-anchor-hour
  (let [prof (vec (repeat 168 1.0))
        ;; 22:00 Monday UTC -> blocks break at 04:00
        t (+ t0 (* 22 h))
        fb (m/future-blocks prof utc seven t (+ t (* 36 h)))]
    (is (= [[6.0 true] [24.0 false] [6.0 false]] fb))))
