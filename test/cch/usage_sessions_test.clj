(ns cch.usage-sessions-test
  (:require [cch.numeric :as num]
            [cch.usage-sessions :as us]
            [clojure.test :refer [deftest is testing]]))

(deftest spells-split-at-gaps
  (let [evs [[100 "a"] [200 "a"] [2000 "a"] [150 "b"] [160 "b"]]]
    (is (= [[100 200] [150 160] [2000 2000]] (us/spells evs 600)))
    (is (= 2 (us/active-at (us/spells evs 600) 700 600)) "both last events within 600 s of 700")
    (is (= 1 (us/active-at (us/spells evs 600) 780 600)) "b's last event (160) is now 620 s back")
    (is (= 1 (us/active-at [[100 5000]] 3000 600)) "a spell spanning t is active")))

(deftest sessions-drive-the-tick-rate
  (let [zone (java.time.ZoneId/of "UTC")
        prof (vec (repeat 168 1.0))
        r (num/rng 77)
        b 0.2 c 1.5
        ;; 20 days: sessions arrive at 0.5/h, last ~1.5 h; ticks at b + c * N
        spells (vec (for [_ (range (num/poisson-sample r (* 0.5 480)))]
                      (let [s (* 3600 480 (.nextDouble r))]
                        [(long s) (long (+ s (* 3600 (/ (- (Math/log (- 1.0 (.nextDouble r)))) (/ 1 1.5)))))])))
        hours (vec (for [hh (range 480)]
                     (let [t0 (* 3600 hh) t1 (+ t0 3600)
                           sess (reduce + 0.0 (map (fn [[s e]] (/ (max 0 (- (min e t1) (max s t0))) 3600.0)) spells))]
                       [(double (num/poisson-sample r (+ b (* c sess)))) 1.0 sess])))
        fit (us/fit-p9 hours)]
    (testing "recovers the per-session tick rate and the baseline"
      (is (< 1.1 (:c fit) 2.0))
      (is (< 0.05 (:b fit) 0.5)))
    (testing "and the session process"
      (let [{:keys [a mu]} (us/fit-sessions spells 0 (* 3600 480) prof zone)]
        (is (< 0.3 a 0.8))
        (is (< 0.45 mu 0.9))))
    (testing "more sessions running now means a higher forecast"
      (let [p (merge fit (us/fit-sessions spells 0 (* 3600 480) prof zone))
            arr (us/arrival-hours spells 0 (* 3600 480) prof zone)
            busy (us/forecast-p9 p prof zone hours arr 6 (* 3600 480) (* 3600 486) 10.0)
            idle (us/forecast-p9 p prof zone hours arr 0 (* 3600 480) (* 3600 486) 10.0)]
        (is (> (:median busy) (+ 3 (:median idle))))))))

(deftest away-available-arrivals
  (let [r (num/rng 88)
        r1 0.04 r2 0.06 a 3.0
        ;; 30 days hourly: away/available chain (~day-long spells), starts NegBin(a) when available
        states (loop [k 0 st 1 out []]
                 (if (= k 720) out
                     (let [st (if (< (.nextDouble r) (if (= st 1) r2 r1)) (- 1 st) st)]
                       (recur (inc k) st (conj out st)))))
        hours (mapv (fn [st] [(double (if (= st 1) (num/poisson-sample r (num/gamma-sample r 2.0 (/ 2.0 a))) 0)) 1.0 0.0]) states)
        fit (us/fit-arrivals hours)]
    (testing "recovers day-scale switching and the available rate"
      (is (< 0.01 (:r1 fit) 0.15))
      (is (< 0.02 (:r2 fit) 0.2))
      (is (< 2.0 (:a fit) 4.5)))
    (testing "explains the starts better than a drifting rate alone (P9's arrivals)"
      (is (> (:loglik (us/filter-arrivals hours fit))
             (+ 10.0 (:loglik (us/filter-p9 hours (us/fit-p9 hours :fix-c true)))))))))
