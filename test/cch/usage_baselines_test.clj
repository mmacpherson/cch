(ns cch.usage-baselines-test
  (:require [cch.numeric :as num]
            [cch.usage-baselines :as b]
            [cch.usage-model :as m]
            [clojure.test :refer [deftest is testing]])
  (:import (java.time ZoneId)))

(def ^:private zone (ZoneId/of "UTC"))

(def ^:private prof
  "Unit-mean daily cycle."
  (let [p (vec (for [h (range 168)] (Math/exp (Math/cos (/ (* 2 Math/PI (- (mod h 24) 14)) 24)))))
        mu (/ (reduce + p) 168.0)]
    (mapv #(/ % mu) p)))

(defn- simulate
  "Rung-3 hourly counts with mean-per-mass c and burstiness kappa."
  [c kappa hours seed]
  (let [r (num/rng seed)]
    (vec (for [h (range hours)
               :let [a (nth prof (mod h 168))]]
           [1.0 a (double (num/poisson-sample r (num/gamma-sample r (* kappa a) (/ kappa c))))]))))

(deftest fits-recover-parameters
  (let [stps (simulate 2.0 0.4 20000 5)]
    (testing "rungs 1 and 2 estimate the mean rate"
      (is (< (Math/abs (- 2.0 (:rate (b/fit 1 stps)))) 0.1))
      (is (< (Math/abs (- 2.0 (:c (b/fit 2 stps)))) 0.1)))
    (testing "rung 3 also recovers the burstiness"
      (let [{:keys [c kappa]} (b/fit 3 stps)]
        (is (< (Math/abs (- 2.0 c)) 0.1))
        (is (< (Math/abs (- 1.0 (/ kappa 0.4))) 0.1))))))

(deftest forecast-mean-and-spread
  (let [spec (m/specs :seven-day)
        now 1780272000
        resets-at (+ now (* 3 86400))
        f2 (b/forecast {:rung 2 :c 1.0} prof zone spec now resets-at 10.0 :n 4000)
        f3 (b/forecast {:rung 3 :c 1.0 :kappa 0.2} prof zone spec now resets-at 10.0 :n 4000)
        mean (fn [^doubles d] (/ (reduce + d) (alength d)))]
    (testing "both rungs center on x + c * future mass (72 unit-mean hours)"
      (is (< (Math/abs (- 82.0 (mean (:draws f2)))) 1.0))
      (is (< (Math/abs (- 82.0 (mean (:draws f3)))) 1.5)))
    (testing "burstiness widens the predictive"
      (is (> (- (:hi f3) (:lo f3)) (* 1.5 (- (:hi f2) (:lo f2))))))
    (is (every? #(>= % 10.0) (:draws f2)) "never below the current reading")))

(deftest weekly-pace-fit-and-update
  (let [r (num/rng 9)
        wtots (vec (for [_ (range 400)]
                     (let [theta (num/gamma-sample r 3.0 3.0)]
                       [168.0 (double (num/poisson-sample r (* 0.4 theta 168.0)))])))
        {:keys [c alpha] :as fit} (b/fit-weekly wtots)]
    (testing "recovers the mean rate and the week-to-week dispersion"
      (is (< (Math/abs (- 1.0 (/ c 0.4))) 0.08))
      (is (< (Math/abs (- 1.0 (/ alpha 3.0))) 0.3)))
    (testing "a light start pulls the forecast below the historical pace"
      (let [spec (m/specs :seven-day)
            start 1780272000
            now (+ start (* 3 86400))
            end (+ start (* 7 86400))
            slow (b/forecast-weekly fit prof zone spec start now end 5.0)
            usual (b/forecast-weekly fit prof zone spec start now end (* c 72.0))]
        (is (< (:median slow) (- (:median usual) (* c 72.0 0.4))))))))
