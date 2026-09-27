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
    P1  inhomogeneous Poisson process: lambda(t) = c * profile(time of week)
    P2  mixed Poisson process: lambda(t) = c * theta_w * profile(t), a gamma
        pace theta_w ~ Gamma(alpha, alpha) per calendar week (Monday 04:00
        local), the user's week rather than the provider's quota window

  Pure functions."
  (:require [cch.numeric :as num]
            [cch.usage-baselines :as base]
            [cch.usage-model :as m])
  (:import (java.time DayOfWeek Instant ZoneId ZonedDateTime)
           (java.time.temporal TemporalAdjusters)))

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

;; --- P1: inhomogeneous Poisson process ---
;;
;; The intensity follows the shared fleet profile (hour units, piecewise
;; constant by hour of week): lambda(t) = c * profile(t), so an interval's
;; expected count is c times the profile mass over it.

(defn fit-p1
  "Maximum-likelihood scale c (ticks per unit profile mass): total ticks over
  total profile mass of the intervals."
  [ivs prof zone]
  (let [mass (reduce + 0.0 (map #(m/mass prof zone (:t0 %) (:t1 %)) ivs))
        ticks (reduce + 0.0 (map :k ivs))]
    {:rung :P1 :c (/ ticks (max 1e-9 mass))}))

(defn loglik-p1
  "Log likelihood of interval counts under scale `c` and profile `prof`."
  [ivs c prof zone]
  (reduce + 0.0 (for [{:keys [t0 t1 k]} ivs
                      :let [mu (* c (m/mass prof zone t0 t1))]]
                  (- (* k (Math/log (max mu 1e-300))) mu (num/log-gamma (+ k 1.0))))))

(defn forecast-p1
  "Meter at `end` from reading `x` at `now`: x + Poisson(c * profile mass)."
  [{:keys [c]} prof zone now end x & {:keys [n seed] :or {n 3000 seed 1}}]
  (forecast-p0 {:rate (/ (* c (m/mass prof zone now (max now end))) (max 1 (- end now)))} now end x :n n :seed seed))

;; --- P2: a gamma pace per calendar week ---

(defn week-start
  "Epoch second of the calendar week containing `t`: Monday 04:00 local."
  ^long [^ZoneId zone t]
  (let [z (ZonedDateTime/ofInstant (Instant/ofEpochSecond t) zone)
        monday (-> z (.minusHours 4) (.with (TemporalAdjusters/previousOrSame DayOfWeek/MONDAY))
                   (.toLocalDate) (.atTime 4 0) (.atZone zone))]
    (.toEpochSecond monday)))

(defn weekly-totals
  "Per calendar week, [profile-mass ticks] of the intervals, each interval
  assigned to the week containing its midpoint: {week-start [mass ticks]}."
  [ivs prof zone]
  (reduce (fn [acc {:keys [t0 t1 k]}]
            (update acc (week-start zone (quot (+ t0 t1) 2))
                    (fn [[mass ticks]] [(+ (or mass 0.0) (m/mass prof zone t0 t1)) (+ (or ticks 0.0) k)])))
          {} ivs))

(defn fit-p2
  "Maximum-likelihood {:c :alpha}: weekly totals are negative binomial given
  their profile mass (the weekly-pace likelihood of cch.usage-baselines)."
  [ivs prof zone]
  (assoc (base/fit-weekly (vec (vals (weekly-totals ivs prof zone)))) :rung :P2))

(defn forecast-p2
  "Meter at `end` from reading `x` at `now`. `week-ivs` are this calendar
  week's intervals so far: they update its pace theta ~ Gamma(alpha + ticks,
  alpha + c * mass); later weeks the horizon reaches draw a fresh pace."
  [{:keys [c alpha]} prof zone week-ivs now end x & {:keys [n seed] :or {n 3000 seed 1}}]
  (let [ticks (reduce + 0.0 (map :k week-ivs))
        m-obs (reduce + 0.0 (map #(m/mass prof zone (:t0 %) (:t1 %)) week-ivs))
        ;; future mass split by calendar week: the current week, then each later one
        weeks (loop [t now out []]
                (if (>= t end)
                  out
                  (let [nxt (min end (+ (week-start zone t) (* 7 86400)))]
                    (recur nxt (conj out (m/mass prof zone t nxt))))))
        r (num/rng seed)
        draws (double-array n)]
    (dotimes [i n]
      (aset draws i
            (+ (double x)
               (reduce + 0.0
                       (map-indexed
                         (fn [j mass]
                           (let [theta (if (zero? j)
                                         (num/gamma-sample r (+ alpha ticks) (+ alpha (* c m-obs)))
                                         (num/gamma-sample r alpha alpha))]
                             (if (pos? mass) (double (num/poisson-sample r (* c theta mass))) 0.0)))
                         weeks)))))
    (java.util.Arrays/sort draws)
    {:median (num/quantile draws 0.5) :lo (num/quantile draws 0.05)
     :q25 (num/quantile draws 0.25) :q75 (num/quantile draws 0.75)
     :hi (num/quantile draws 0.95)
     :p-cap (/ (double (count (filter #(>= % 100.0) draws))) n)
     :draws draws}))
