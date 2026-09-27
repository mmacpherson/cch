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
           ivs))
    (testing "the capped span until the old reset is not exposure"
      (is (= 550.0 (:secs (tk/exposure ivs)))))))

(deftest idle-gap-between-windows-is-exposure
  (let [ivs (tk/intervals [[100 5.0 10000] [200 7.0 10000] [9000 1.0 20000]] 300 15000)]
    (is (= [{:t0 100 :t1 200 :k 2.0} {:t0 200 :t1 5000 :k 0.0} {:t0 5000 :t1 9000 :k 1.0}] ivs)
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
