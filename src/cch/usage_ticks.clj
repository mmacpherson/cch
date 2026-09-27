(ns cch.usage-ticks
  "Continuous-time models of the quota meter's 1% ticks.

  The meter is observed whenever a session reports it, so the data are the
  ticks' arrival times, known to within the gap between readings. Each pair
  of consecutive readings gives an interval (t0, t1] with a count k of ticks
  that arrived inside it; for a Poisson-type process the likelihood of these
  interval counts is exact, whether the gap is seconds (a precisely timed
  tick) or hours (a multi-tick jump). Time spent capped is not exposure:
  usage is blocked, not zero. Across a reset, the new window's start is
  known (its reset time minus the window length, which also dates a
  provider-granted reset), so the stretch from that start to the first new
  reading carries a known count. The stretch before it counts as exposure
  with no ticks, as the hourly models assume: the readings caught the old
  window's final, and without an open window there is no usage. That is
  near-certain for Claude (readings every ~20 s while active) and weaker
  for Codex's sparse readings.

  Integer-tick providers only (Claude, Codex); AGY reports fractional
  percents and needs a compound (continuous-increment) process.

  Rungs, from the ground floor:
    P0  homogeneous Poisson process: constant tick rate lambda

  Pure functions."
  (:require [cch.numeric :as num]))

(defn intervals
  "Tick-count intervals from raw readings [[ts pct resets-at] ...] (any
  order): [{:t0 :t1 :k}], between consecutive accepted readings, and from a
  window's start (resets-at - `span-secs`) to its first reading. Readings
  below their window's running maximum are stale (another session's cached
  state) and ignored; `cluster-secs` groups a window's jittered reset times."
  [readings cluster-secs span-secs]
  (let [sorted (sort-by first readings)]
    (loop [[[ts pct r] & more] sorted
           prev nil        ; {:ts :pct :r} of the last accepted reading
           out (transient [])]
      (if (nil? ts)
        (persistent! out)
        (let [pct (double pct)
              same-window? (and prev (<= (Math/abs (double (- r (:r prev)))) cluster-secs))]
          (cond
            ;; stale report within the current window, or from an earlier
            ;; window (a session still caching the old reset): windows only
            ;; move forward
            (or (and same-window? (< pct (:pct prev)))
                (and prev (not same-window?) (< r (:r prev))))
            (recur more prev out)

            same-window?
            (recur more {:ts ts :pct pct :r (:r prev)}
                   (if (>= (:pct prev) 100.0)
                     out                                   ; capped: no exposure
                     (conj! out {:t0 (:ts prev) :t1 ts :k (Math/rint (- pct (:pct prev)))})))

            ;; first reading, or a new window: restart the counter; the new
            ;; window's ticks so far arrived after its start
            :else
            (let [start (min ts (max (- r span-secs) (if prev (:ts prev) Long/MIN_VALUE)))
                  out (cond-> out
                        ;; between the old window's last reading and the new
                        ;; window's start: no usage (the readings caught the old
                        ;; window's final, and no window means no usage); a
                        ;; capped stretch until the old reset stays blocked
                        (and prev (< (:ts prev) start))
                        (conj! (let [from (if (>= (:pct prev) 100.0) (min start (max (:ts prev) (:r prev))) (:ts prev))]
                                 {:t0 from :t1 start :k 0.0})))]
              (recur more {:ts ts :pct pct :r r}
                     (if (and prev (< start ts))
                       (conj! out {:t0 start :t1 ts :k (Math/rint pct)})
                       out)))))))))

(defn exposure
  "Total exposed seconds and ticks of `ivs`."
  [ivs]
  {:secs (reduce + 0.0 (map #(- (:t1 %) (:t0 %)) ivs))
   :ticks (reduce + 0.0 (map :k ivs))})

;; --- P0: homogeneous Poisson process ---

(defn fit-p0
  "Maximum-likelihood constant rate (ticks per second): total ticks over
  total exposed time."
  [ivs]
  (let [{:keys [secs ticks]} (exposure ivs)]
    {:rung :P0 :rate (/ ticks (max 1.0 secs))}))

(defn loglik-p0
  "Log likelihood of interval counts under rate `rate` (per second)."
  [ivs rate]
  (reduce + 0.0 (for [{:keys [t0 t1 k]} ivs
                      :let [mu (* rate (- t1 t0))]]
                  (- (* k (Math/log (max mu 1e-300))) mu (num/log-gamma (+ k 1.0))))))

(defn forecast-p0
  "Meter at `end` from reading `x` at `now`: x + Poisson(rate * (end - now))."
  [{:keys [rate]} now end x & {:keys [n seed] :or {n 3000 seed 1}}]
  (let [r (num/rng seed)
        mu (* rate (max 0 (- end now)))
        draws (double-array n)]
    (dotimes [i n] (aset draws i (+ (double x) (num/poisson-sample r mu))))
    (java.util.Arrays/sort draws)
    {:median (num/quantile draws 0.5) :lo (num/quantile draws 0.05)
     :q25 (num/quantile draws 0.25) :q75 (num/quantile draws 0.75)
     :hi (num/quantile draws 0.95)
     :p-cap (/ (double (count (filter #(>= % 100.0) draws))) n)
     :draws draws}))
