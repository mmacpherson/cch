(ns cch.usage-ticks-test
  (:require [cch.numeric :as num]
            [cch.usage-model :as m]
            [cch.usage-ticks :as tk]
            [clojure.test :refer [deftest is testing]]))

(deftest intervals-from-readings
  (let [r1 1000000 r2 (+ r1 604800)
        readings [[100 5.0 r1] [160 6.0 r1]
                  [170 4.0 r1]                   ; stale: another session's cached state
                  [400 9.0 r1]                   ; a 3-tick jump over a gap
                  [500 100.0 r1] [900 100.0 r1]  ; capped: no exposure after
                  [1000 1.0 r2]
                  [1050 100.0 r1]                ; a session still reporting the old window
                  [1100 2.0 (+ r2 2)]]           ; new window (reset jitter 2s)
        ;; a 604000 s window length puts the new window's start at 950 (r2 - span)
        ivs (tk/intervals readings 300 (- r2 950))]
    (is (= [{:t0 100 :t1 160 :k 1.0} {:t0 160 :t1 400 :k 3.0} {:t0 400 :t1 500 :k 91.0}
            {:t0 950 :t1 950 :k 0.0} {:t0 950 :t1 1000 :k 1.0} {:t0 1000 :t1 1100 :k 1.0}]
           (mapv #(dissoc % :known) ivs)))
    (testing "what is known before t is a prefix, and never uses a later reading"
      (is (= (mapv #(dissoc % :known) (tk/intervals (filter #(< (first %) 1000) readings) 300 (- r2 950)))
             (mapv #(dissoc % :known) (tk/known-before ivs 1000)))))
    (testing "the capped span until the old reset is not exposure"
      (is (= 550.0 (:secs (tk/exposure ivs)))))))

(deftest idle-gap-between-windows-is-exposure
  (let [ivs (tk/intervals [[100 5.0 10000] [200 7.0 10000] [9000 1.0 20000]] 300 15000)]
    (is (= [{:t0 100 :t1 200 :k 2.0} {:t0 200 :t1 5000 :k 0.0} {:t0 5000 :t1 9000 :k 1.0}] (mapv #(dissoc % :known) ivs))
        "zero-tick gap until the new start (20000-15000), then the new window's tick"))
  (testing "a window start after its first reading (drifting reset times) never makes overlapping intervals"
    (let [ivs (tk/intervals [[100 5.0 10000] [200 7.0 10000] [9000 1.0 20000] [9500 2.0 20000]] 300 5000)]
      (is (every? #(<= (:t0 %) (:t1 %)) ivs))
      (is (apply <= (map :t0 ivs))))))

(deftest p0-recovers-a-rate
  (let [r (num/rng 3)
        rate (/ 0.4 3600)                        ; 0.4 ticks an hour, ~67 a week
        ;; readings every 10 minutes for 8 weekly windows, ticks from a Poisson
        ;; process, the meter restarting at each reset
        readings (loop [t 0 pct 0.0 out []]
                   (if (> t (* 56 86400))
                     out
                     (let [reset (* 604800 (inc (quot t 604800)))
                           pct (if (zero? (mod t 604800)) 0.0 pct)
                           pct (+ pct (num/poisson-sample r (* rate 600)))]
                       (recur (+ t 600) pct (conj out [t pct reset])))))
        ivs (tk/intervals readings 300 604800)
        {fit :rate} (tk/fit-p0 ivs)]
    (is (< (Math/abs (- 1.0 (/ fit rate))) 0.1))
    (is (> (tk/loglik-p0 ivs fit) (tk/loglik-p0 ivs (* 2 fit))) "the fit beats a doubled rate")))

(deftest p1-follows-the-profile
  (let [zone (java.time.ZoneId/of "UTC")
        ;; unit-mean profile: busy 09:00-17:00, quiet otherwise
        raw (vec (for [h (range 168)] (if (<= 9 (mod h 24) 16) 3.0 0.2)))
        mu (/ (reduce + raw) 168.0)
        prof (mapv #(/ % mu) raw)
        r (num/rng 5)
        c 0.4
        ;; readings every 10 minutes, ticks with intensity c * profile
        readings (loop [t 0 pct 0.0 out []]
                   (if (> t (* 56 86400))
                     out
                     (let [reset (* 604800 (inc (quot t 604800)))
                           pct (if (zero? (mod t 604800)) 0.0 pct)
                           pct (+ pct (num/poisson-sample r (* c (m/mass prof zone t (+ t 600)))))]
                       (recur (+ t 600) pct (conj out [t pct reset])))))
        ivs (tk/intervals readings 300 604800)
        {fit :c} (tk/fit-p1 ivs prof zone)]
    (is (< (Math/abs (- 1.0 (/ fit c))) 0.1))
    (testing "the profile explains the data better than a constant rate"
      (is (> (tk/loglik-p1 ivs fit prof zone)
             (tk/loglik-p0 ivs (:rate (tk/fit-p0 ivs))))))
    (testing "a forecast over busy hours expects more than one over quiet hours"
      (let [busy (tk/forecast-p1 {:c fit} prof zone (* 9 3600) (* 17 3600) 0.0)
            quiet (tk/forecast-p1 {:c fit} prof zone (* 17 3600) (* 25 3600) 0.0)]
        (is (> (:median busy) (* 3 (:median quiet))))))))

(deftest p2-weekly-pace
  (let [zone (java.time.ZoneId/of "UTC")
        prof (vec (repeat 168 1.0))
        r (num/rng 8)
        c 0.3
        ;; 60 calendar weeks, each with pace theta ~ Gamma(3, 3); readings hourly
        start (tk/week-start zone 1780272000)
        readings (vec (for [w (range 60)
                            :let [theta (num/gamma-sample r 3.0 3.0)
                                  ws (+ start (* w 604800))
                                  counts (reductions + (repeatedly 168 #(num/poisson-sample r (* c theta))))]
                            [h cum] (map-indexed vector counts)]
                        [(+ ws (* 3600 (inc h))) (min 99.0 (double cum)) (+ ws 604800)]))
        ivs (tk/intervals readings 300 604800)
        {:keys [alpha] fc :c :as fit} (tk/fit-p2 ivs prof zone)]
    (testing "recovers the rate and the week-to-week spread"
      (is (< (Math/abs (- 1.0 (/ fc c))) 0.15))
      (is (< 1.5 alpha 6.0)))
    (testing "a busy start to the week raises the rest-of-week forecast"
      (let [ws (+ start (* 70 604800))
            now (+ ws (* 48 3600))
            end (+ ws (* 120 3600))
            busy [{:t0 ws :t1 now :k (* 2.0 fc 48)}]
            quiet [{:t0 ws :t1 now :k (* 0.3 fc 48)}]]
        (is (> (:median (tk/forecast-p2 fit prof zone busy now end 0.0))
               (* 2 (:median (tk/forecast-p2 fit prof zone quiet now end 0.0)))))))))

(deftest drift-only-pace
  (let [zone (java.time.ZoneId/of "UTC")
        prof (vec (repeat 168 1.0))
        r (num/rng 12)
        c 0.25 s 0.8 h 36.0
        phi (Math/pow 2.0 (/ -1.0 h))
        innov (* s (Math/sqrt (- 1.0 (* phi phi))))
        ;; 60 days of hourly readings, pace an hourly AR(1); the meter restarts
        ;; when its window (reset time) changes
        readings (loop [k 0 u 0.0 pct 0.0 prev-reset nil out []]
                   (if (= k (* 60 24))
                     out
                     (let [t (* 3600 (inc k))
                           reset (* 604800 (inc (quot t 604800)))
                           pct (if (= reset prev-reset) pct 0.0)
                           pct (+ pct (num/poisson-sample r (* c (Math/exp u))))]
                       (recur (inc k) (+ (* phi u) (* innov (.nextGaussian r))) pct reset
                              (conj out [t pct reset])))))
        ivs (tk/intervals readings 300 604800)
        hours (tk/hourly-obs ivs prof zone (* 3600 (inc (* 60 24))))
        fit (tk/fit-drift hours)]
    (testing "recovers a drifting pace of the right size and persistence"
      (is (< 0.4 (:s fit) 1.4))
      (is (< 12.0 (:h fit) 120.0)))
    (testing "a drifting pace explains the ticks better than a constant one"
      (is (> (:loglik (tk/drift-filter hours fit))
             (:loglik (tk/drift-filter hours (assoc fit :s 1e-3))))))
    (testing "forecasts are finite and never below the current reading"
      (let [now (* 3600 (* 60 24)) f (tk/forecast-drift fit prof zone hours now (+ now (* 48 3600)) 30.0)]
        (is (every? #(and (Double/isFinite %) (>= % 30.0)) (:draws f)))))))

(deftest hours-before-matches-a-rebuild
  (testing "the incremental hourly view equals rebuilding from the readings before t"
    (let [zone (java.time.ZoneId/of "UTC")
          prof (vec (for [h (range 168)] (+ 0.5 (mod h 3))))
          r (num/rng 4)
          ;; two windows with an idle gap between them (the second starts at first use)
          readings (concat (for [k (range 1 60)] [(* 1000 k) (double (quot k 7)) 200000])
                           (for [k (range 1 40)] [(+ 150000 (* 900 k)) (double (quot k 5)) 400000]))
          ivs (tk/intervals readings 300 250000)
          base (tk/hourly-base ivs)]
      (doseq [t [30000 100000 149000 152000 170000 190000]]
        (let [rebuilt (tk/hourly-obs (tk/intervals (filter #(< (first %) t) readings) 300 250000) prof zone (* 3600 (quot t 3600)))
              fast (tk/hours-before base ivs prof zone t 0)
              near (fn [a b] (every? true? (map (fn [[x1 y1] [x2 y2]] (and (< (Math/abs (- x1 x2)) 1e-9) (< (Math/abs (- y1 y2)) 1e-9))) a b)))]
          (is (near (take (count fast) rebuilt) fast) (str "t=" t)))))))

(deftest session-interval-matrix
  (testing "summed over tick counts, rows are transition probabilities"
    (let [ms (map #(tk/interval-matrix 0.4 1.2 3.0 2.5 %) (range 60))
          [m00 m01 m10 m11] (apply mapv + ms)]
      (is (< (Math/abs (- 1.0 (+ m00 m01))) 1e-6))
      (is (< (Math/abs (- 1.0 (+ m10 m11))) 1e-6))))
  (testing "with no switching, ticks while on are Poisson(lam dt)"
    (doseq [k [0 1 4]]
      (let [[_ _ _ m11] (tk/interval-matrix 1e-12 1e-12 2.0 1.5 k)
            pois (Math/exp (- (* k (Math/log 3.0)) 3.0 (num/log-gamma (+ k 1.0))))]
        (is (< (Math/abs (- m11 pois)) 1e-6) (str "k=" k))))))

(deftest sessions-fit
  (let [zone (java.time.ZoneId/of "UTC")
        prof (vec (repeat 168 1.0))
        r (num/rng 21)
        a 0.25 b 1.0 c 3.0                        ; sessions ~1 h, gaps ~4 h, 3 ticks/h in session
        ;; simulate the chain in continuous time over 40 days; readings every 5 minutes
        horizon (* 40 86400)
        switches (loop [t 0.0 on? false out []]
                   (if (> t horizon) out
                       (let [hold (* 3600 (/ (- (Math/log (- 1.0 (.nextDouble r)))) (if on? b a)))]
                         (recur (+ t hold) (not on?) (conj out [t (+ t hold) on?])))))
        tick-times (sort (for [[t0 t1 on?] switches :when on?
                               :let [nk (num/poisson-sample r (* c (/ (- t1 t0) 3600.0)))]
                               _ (range nk)]
                           (+ t0 (* (.nextDouble r) (- t1 t0)))))
        readings (loop [t 300 ticks tick-times pct 0.0 out []]
                   (if (> t horizon) out
                       (let [[before after] (split-with #(< % t) ticks)
                             reset (* 604800 (inc (quot t 604800)))
                             pct (if (and (seq out) (not= reset (nth (peek out) 2))) (double (count before)) (+ pct (count before)))]
                         (recur (+ t 300) after pct (conj out [t pct reset])))))
        ivs (tk/intervals readings 300 604800)
        units (tk/mmpp-units ivs prof zone)
        fit (tk/fit-p3 units)]
    (testing "recovers the session rates and the in-session tick rate"
      (is (< 0.12 (:a fit) 0.5))
      (is (< 0.5 (:b fit) 2.0))
      (is (< 2.0 (:c fit) 4.5)))
    (testing "sessions explain the ticks better than a constant rate"
      (is (> (:loglik (tk/p3-filter units fit))
             (tk/loglik-p1 ivs (:c (tk/fit-p1 ivs prof zone)) prof zone))))
    (testing "forecasts are finite and never below the current reading"
      (let [f (tk/forecast-p3 fit prof zone units horizon (+ horizon (* 24 3600)) 20.0)]
        (is (every? #(and (Double/isFinite %) (>= % 20.0)) (:draws f)))))))

(deftest closed-forms-match-uniformization
  (testing "no-tick matrix: closed form equals the series"
    (doseq [[a b lam dt] [[0.4 1.2 3.0 2.5] [0.01 0.02 0.5 0.001] [2.0 2.0 0.0 1.0] [0.3 0.3 1e-9 10.0]]]
      (is (every? #(< (Math/abs %) 1e-8)
                  (map - (tk/zero-tick-matrix a b lam dt) (tk/interval-matrix a b lam dt 0)))
          (str [a b lam dt]))))
  (testing "a precisely timed tick: the point-process form matches the exact count to first order"
    (let [dt (/ 1.0 3600) exact (tk/interval-matrix 0.4 1.2 3.0 dt 1)
          fast (#'tk/unit-matrix 0.4 1.2 3.0 dt 1)]
      (is (every? #(< (Math/abs %) 1e-6) (map - exact fast))))))

(deftest same-second-ticks-have-finite-likelihood
  (testing "a tick between two readings in the same second (a zero-length interval) keeps the likelihood finite"
    (let [zone (java.time.ZoneId/of "UTC")
          prof (vec (repeat 168 1.0))
          ivs (tk/intervals [[100 1.0 604800] [100 2.0 604800] [4000 3.0 604800]] 300 604800)
          units (tk/mmpp-units ivs prof zone)]
      (is (Double/isFinite (:loglik (tk/p3-filter units {:c 2.0 :a 0.3 :b 1.0})))))))
