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

(deftest bursts-and-weekly-pace
  (let [r (num/rng 21)
        ;; 150 weeks of 168 unit-mass hours: theta ~ Gamma(4, 4), bursts kappa 0.5, c 0.5
        windows (vec (for [_ (range 150)]
                       (let [theta (num/gamma-sample r 4.0 4.0)
                             hours (vec (for [_ (range 168)]
                                          [1.0 (double (num/poisson-sample r (num/gamma-sample r 0.5 (/ 0.5 (* 0.5 theta)))))]))]
                         {:mass 168.0 :total (reduce + (map second hours)) :hours hours})))
        {:keys [c kappa alpha] :as fit} (b/fit-bursts-weekly windows)]
    (testing "recovers rate, burstiness, and week-to-week spread"
      (is (< (Math/abs (- 1.0 (/ c 0.5))) 0.1))
      (is (< (Math/abs (- 1.0 (/ kappa 0.5))) 0.15))
      ;; week-to-week spread from 150 weeks is noisy: across seeds the
      ;; estimate spans ~3.3-5.6 around the true 4
      (is (< (Math/abs (- 1.0 (/ alpha 4.0))) 0.5)))
    (testing "the week's pace moves the forecast, and bursts widen it vs rung 5"
      (let [spec (m/specs :seven-day)
            start 1780272000 now (+ start (* 3 86400)) end (+ start (* 7 86400))
            slow (b/forecast-bursts-weekly fit prof zone spec start now end 5.0)
            usual (b/forecast-bursts-weekly fit prof zone spec start now end (* c 72.0))
            r5 (b/forecast-weekly {:c c :alpha alpha} prof zone spec start now end (* c 72.0))]
        (is (< (:median slow) (:median usual)))
        (is (> (- (:hi usual) (:lo usual)) (- (:hi r5) (:lo r5))))))))

(defn- simulate-onoff
  "Weeks of 7 daily blocks x 24 unit-mass hours: each day on with prob pi,
  theta ~ Gamma(alpha, alpha), on-day hours rung-6 with rate c / pi."
  [n c kappa alpha pi seed]
  (let [r (num/rng seed)]
    (vec (for [_ (range n)]
           (let [theta (num/gamma-sample r alpha alpha)]
             {:blocks (vec (for [_ (range 7)]
                             (let [on? (< (.nextDouble r) pi)]
                               (vec (for [_ (range 24)]
                                      [1.0 (if on?
                                             (double (num/poisson-sample r (num/gamma-sample r kappa (/ kappa (* (/ c pi) theta)))))
                                             0.0)])))))})))))

(deftest onoff-fit-and-idle-days
  (let [windows (simulate-onoff 200 0.5 0.5 4.0 0.7 31)
        {:keys [c kappa pi alpha] :as fit} (b/fit-onoff windows)]
    (testing "recovers rate, burstiness, and the on-day probability"
      (is (< (Math/abs (- 1.0 (/ c 0.5))) 0.1))
      (is (< (Math/abs (- 1.0 (/ kappa 0.5))) 0.15))
      (is (< (Math/abs (- pi 0.7)) 0.05))
      (is (< 2.0 alpha 8.0)))
    (testing "idle days make the week's total more uncertain than rung 6 at the same mean"
      (let [spec (m/specs :seven-day)
            start 1780272000 now (+ start (* 3 86400)) end (+ start (* 7 86400))
            on-day (vec (repeat 24 [1.0 1.0]))
            f7 (b/forecast-onoff fit prof zone spec [on-day on-day on-day] [] now end 72.0)
            f6 (b/forecast-bursts-weekly {:c c :kappa kappa :alpha alpha} prof zone spec start now end 72.0)]
        (is (> (- (:hi f7) (:lo f7)) (- (:hi f6) (:lo f6))))))))
