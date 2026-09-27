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
    --  Cox process with a drifting pace alone (negative result: without
        sessions the pace is forced to explain bursts)
    P3  on/off sessions: a Markov-modulated Poisson process; a hidden chain
        switches off->on at rate a and on->off at rate b (per hour); ticks
        arrive at c * profile(t) while on and not at all while off

  Pure functions."
  (:require [cch.numeric :as num]
            [cch.usage-baselines :as base]
            [cch.usage-model :as m])
  (:import (java.time DayOfWeek Instant ZoneId ZonedDateTime)
           (java.time.temporal TemporalAdjusters)))

(defn intervals
  "Tick-count intervals from raw readings [[ts pct resets-at] ...] (any
  order): [{:t0 :t1 :k :known}], between consecutive accepted readings, and
  from a window's start (resets-at - `span-secs`) to its first reading.
  `:known` is the time of the reading that reveals the interval, so the
  intervals come out ordered by it and what is known before any time t is a
  prefix (see `known-before`). Readings below their window's running
  maximum are stale (another session's cached state) and ignored;
  `cluster-secs` groups a window's jittered reset times."
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
                     (conj! out {:t0 (:ts prev) :t1 ts :k (Math/rint (- pct (:pct prev))) :known ts})))

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
                                 {:t0 from :t1 start :k 0.0 :known ts})))]
              (recur more {:ts ts :pct pct :r r}
                     (if (and prev (< start ts))
                       (conj! out {:t0 start :t1 ts :k (Math/rint pct) :known ts})
                       out)))))))))

(defn known-before
  "The intervals of `ivs` (as returned by `intervals`) revealed before `t`,
  optionally only those starting at or after `since`: a prefix by :known."
  ([ivs t] (known-before ivs t nil))
  ([ivs t since]
   (let [n (count ivs)
         end (loop [lo 0 hi n]                    ; first index with :known >= t
               (if (>= lo hi) lo
                   (let [mid (quot (+ lo hi) 2)]
                     (if (< (:known (nth ivs mid)) t) (recur (inc mid) hi) (recur lo mid)))))
         pre (subvec ivs 0 end)]
     (if since (filterv #(>= (:t0 %) since) pre) pre))))

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

;; --- P3: a continuously drifting pace (Cox process) ---
;;
;; u(t) is a stationary Ornstein-Uhlenbeck process, held constant within each
;; clock hour (the standard discretization): u' = phi u + e, phi = 2^(-1/h),
;; e ~ N(0, s^2 (1 - phi^2)). Tick intervals roll up per hour: exposure (profile
;; mass) splits exactly across hours; an interval's ticks go to the hour it
;; ends in (exact for second-scale intervals, approximate for multi-tick
;; gaps spanning hours). The likelihood is the forward filter on the grid in
;; u of cch.usage-baselines, with Poisson hourly observations.

(defn hourly-obs
  "[[ticks mass] ...] per clock hour from the first interval's hour to the
  hour before `t-end` (hours without exposure have mass 0)."
  [ivs prof zone t-end]
  (if (empty? ivs)
    []
    (let [h0 (* 3600 (quot (:t0 (first ivs)) 3600))
          n (max 0 (quot (- t-end h0) 3600))
          ticks (double-array n) mass (double-array n)]
      (doseq [{:keys [t0 t1 k]} ivs]
        (loop [t t0]
          (when (< t (min t1 t-end))
            (let [hi (quot (- t h0) 3600)
                  nxt (min t1 (+ h0 (* 3600 (inc hi))))]
              (when (< -1 hi n) (aset mass hi (+ (aget mass hi) (m/mass prof zone t nxt))))
              (recur nxt))))
        (let [hi (quot (- (dec t1) h0) 3600)]
          (when (< -1 hi n) (aset ticks hi (+ (aget ticks hi) (double k))))))
      (mapv vector ticks mass))))

(defn hourly-base
  "Per clock hour over all of `ivs`: ticks (at the hour each interval ends
  in) and exposed fraction of the hour, profile-free. {:h0 :ticks :live}."
  [ivs]
  (if (empty? ivs)
    {:h0 0 :ticks (double-array 0) :live (double-array 0)}
    (let [h0 (* 3600 (quot (reduce min (map :t0 ivs)) 3600))
          n (inc (quot (- (reduce max (map :t1 ivs)) h0) 3600))
          ticks (double-array n) live (double-array n)]
      (doseq [{:keys [t0 t1 k]} ivs]
        (loop [t t0]
          (when (< t t1)
            (let [hi (quot (- t h0) 3600)
                  nxt (min t1 (+ h0 (* 3600 (inc hi))))]
              (aset live hi (+ (aget live hi) (/ (double (- nxt t)) 3600.0)))
              (recur nxt))))
        (let [hi (quot (- (dec (max t1 (inc t0))) h0) 3600)]
          (aset ticks hi (+ (aget ticks hi) (double k)))))
      {:h0 h0 :ticks ticks :live live})))

(defn hours-before
  "[[ticks mass] ...] for the clock hours in [since, t) (whole hours; the hour
  in progress is left out), from `base` (built on all of `ivs`) minus the
  intervals revealed at or after t, so nothing later leaks in."
  [{:keys [h0 ticks live]} ivs prof zone t since]
  (let [^doubles ticks ticks ^doubles live live
        n (alength ticks)
        from (max 0 (quot (- (* 3600 (quot since 3600)) h0) 3600))
        to (min n (quot (- (* 3600 (quot t 3600)) h0) 3600))]
    (if (<= to from)
      []
      (let [tk (java.util.Arrays/copyOfRange ticks (int from) (int to))
            lv (java.util.Arrays/copyOfRange live (int from) (int to))
            late (drop (count (known-before ivs t)) ivs)]
        ;; remove what was revealed later (the gap before a window's first
        ;; reading, which only that reading reveals)
        (doseq [{:keys [t0 t1 k]} (take-while #(< (:t0 %) t) late)]
          (loop [u t0]
            (when (< u t1)
              (let [hi (- (quot (- u h0) 3600) from)
                    nxt (min t1 (+ h0 (* 3600 (+ from hi 1))))]
                (when (< -1 hi (alength lv)) (aset lv hi (- (aget lv hi) (/ (double (- nxt u)) 3600.0))))
                (recur nxt))))
          (let [hi (- (quot (- (dec (max t1 (inc t0))) h0) 3600) from)]
            (when (< -1 hi (alength tk)) (aset tk hi (- (aget tk hi) (double k))))))
        (vec (for [i (range (alength tk))
                   :let [h (+ h0 (* 3600 (+ from i)))]]
               [(aget tk i) (* (max 0.0 (aget lv i)) (nth prof (m/hour-of-week zone h)))]))))))

(defn- hour-obs
  "log P(y | u) for one hour on the drift grid."
  ^doubles [^double c ^double y ^double mass]
  (let [grid ^doubles base/drift-grid n (alength grid) out (double-array n)]
    (when (pos? mass)
      (let [lc (+ (Math/log c) (Math/log mass)) lf (num/log-gamma (+ y 1.0))]
        (dotimes [i n]
          (let [u (aget grid i)]
            (aset out i (- (* y (+ lc u)) (* c mass (Math/exp u)) lf))))))
    out))

(defn drift-filter
  "Forward filter over hourly observations: {:p posterior :loglik L}."
  [hours {:keys [c s h]}]
  (let [phi (Math/pow 2.0 (/ -1.0 h))
        innov (max 1e-3 (* s (Math/sqrt (- 1.0 (* phi phi)))))
        kern (base/kernel phi innov)]
    (loop [[[y mass] & more :as hs] hours ^doubles p (base/stationary s) ll 0.0 first? true]
      (if (empty? hs)
        {:p p :loglik ll}
        (let [^doubles p (if first? p (base/transition p kern))]
          (if (zero? mass)
            (recur more p ll false)
            (let [^doubles lo (hour-obs c y mass)
                  mx (areduce lo i mm Double/NEGATIVE_INFINITY (max mm (aget lo i)))
                  post (double-array (alength p))
                  z (loop [i 0 z 0.0]
                      (if (= i (alength p)) z
                          (let [v (* (aget p i) (Math/exp (- (aget lo i) mx)))]
                            (aset post i v) (recur (inc i) (+ z v)))))]
              (dotimes [i (alength post)] (aset post i (/ (aget post i) z)))
              (recur more post (+ ll mx (Math/log z)) false))))))))

(defn fit-drift
  "Penalized ML {:c :s :h} (weak priors on the unconstrained scale, sd 1 for
  the spread and half-life, as rung 8: beyond the grid the likelihood is flat
  in them)."
  [hours]
  (let [ticks (reduce + 0.0 (map first hours)) mass (reduce + 0.0 (map second hours))
        c0 (/ ticks (max 1e-9 mass))
        x0 [(Math/log (max 1e-6 c0)) (Math/log 0.7) (Math/log 48.0)]
        sd [3.0 1.0 1.0]
        unpack (fn [[lc ls lh]] {:c (Math/exp lc) :s (Math/exp ls) :h (Math/exp lh)})
        penalty (fn [x] (* 0.5 (reduce + (map (fn [xi mi d] (Math/pow (/ (- xi mi) d) 2)) x x0 sd))))
        {x :x} (num/nelder-mead (fn [x] (let [v (:loglik (drift-filter hours (unpack x)))]
                                          (if (Double/isFinite v) (+ (- v) (penalty x)) 1e300)))
                                x0 :tol 1e-6 :max-iter 2000)]
    (assoc (unpack x) :rung :drift)))

(defn forecast-drift
  "Meter at `end` from reading `x` at `now`: filter the hourly observations
  up to now, then simulate the pace hour by hour over the future profile."
  [{:keys [c s h] :as params} prof zone hours now end x & {:keys [n seed] :or {n 3000 seed 1}}]
  (let [phi (Math/pow 2.0 (/ -1.0 h))
        innov (max 1e-3 (* s (Math/sqrt (- 1.0 (* phi phi)))))
        ^doubles p (:p (drift-filter hours params))
        grid ^doubles base/drift-grid
        cum (double-array (reductions + p))
        total (aget cum (dec (alength cum)))
        ;; future masses per clock hour
        fut (loop [t now out []]
              (if (>= t end) out
                  (let [nxt (min end (* 3600 (inc (quot t 3600))))]
                    (recur nxt (conj out (m/mass prof zone t nxt))))))
        r (num/rng seed)
        draws (double-array n)]
    (dotimes [i n]
      (let [v (* total (.nextDouble r))
            j (min (dec (alength cum)) (let [k (java.util.Arrays/binarySearch cum v)] (if (neg? k) (- (inc k)) k)))
            u0 (+ (aget grid j) (* 0.05 (- (.nextDouble r) 0.5)))]
        ;; the meter stops at 100, so a path that reaches it needs no more steps
        (aset draws i (+ (double x)
                         (loop [[mh & more] fut u u0 acc 0.0]
                           (if (or (nil? mh) (>= (+ x acc) 100.0)) acc
                               (let [u (+ (* phi u) (* innov (.nextGaussian r)))]
                                 (recur more u (+ acc (if (pos? mh) (double (num/poisson-sample r (* c (Math/exp u) mh))) 0.0))))))))))
    (java.util.Arrays/sort draws)
    {:median (num/quantile draws 0.5) :lo (num/quantile draws 0.05)
     :q25 (num/quantile draws 0.25) :q75 (num/quantile draws 0.75)
     :hi (num/quantile draws 0.95)
     :p-cap (/ (double (count (filter #(>= % 100.0) draws))) n)
     :draws draws}))

;; --- P3: on/off sessions (Markov-modulated Poisson process) ---
;;
;; State 0 off, 1 on. Generator Q = [[-a a] [b -b]]; intensity Lambda =
;; diag(0, lam). Over an interval of length dt with k ticks, the matrix of
;; P(k ticks, end state | start state) is the z^k coefficient of
;; exp((Q - Lambda + z Lambda) dt), computed by uniformization: with
;; nu >= max exit rate, exp(G dt) = sum_n Pois(n; nu dt) (I + G / nu)^n,
;; tracking polynomial coefficients in z up to degree k. lam within an
;; interval is c times its mean profile (exact for short intervals).
;; Consecutive zero-tick intervals within an hour are merged (exact).

(defn mmpp-units
  "Observation units for the session model: [{:dt hours :mass m :k ticks}],
  zero-tick intervals split at clock hours and merged within each hour,
  ticked intervals kept whole."
  [ivs prof zone]
  (let [units (transient [])
        flush (fn [units acc] (if acc (conj! units acc) units))]
    (loop [[{:keys [t0 t1 k] :as iv} & more] ivs acc nil units units]
      (if (nil? iv)
        (persistent! (flush units acc))
        (if (pos? k)
          ;; readings have one-second resolution: a tick between two readings
          ;; in the same second happened within that second
          (let [t0 (min t0 (dec t1))]
            (recur more nil (conj! (flush units acc)
                                   {:dt (/ (- t1 t0) 3600.0) :mass (m/mass prof zone t0 t1) :k k :hour nil})))
          ;; zero-tick: split at hours, merge pieces into the running hour unit
          (let [[acc units] (loop [t t0 acc acc units units]
                              (if (>= t t1) [acc units]
                                  (let [h (* 3600 (quot t 3600)) nxt (min t1 (+ h 3600))
                                        piece {:dt (/ (- nxt t) 3600.0) :mass (m/mass prof zone t nxt) :k 0.0 :hour h}]
                                    (if (and acc (= (:hour acc) h))
                                      (recur nxt (-> acc (update :dt + (:dt piece)) (update :mass + (:mass piece))) units)
                                      (recur nxt piece (flush units acc))))))]
            (recur more acc units)))))))

(defn- poly-mul
  "Product of 2x2 matrix polynomials (vectors of [m00 m01 m10 m11] by degree), truncated at degree kmax."
  [p q kmax]
  (let [out (vec (repeat (inc kmax) [0.0 0.0 0.0 0.0]))]
    (reduce (fn [out [i [a00 a01 a10 a11]]]
              (reduce (fn [out [j [b00 b01 b10 b11]]]
                        (let [d (+ i j)]
                          (if (> d kmax) out
                              (update out d (fn [[c00 c01 c10 c11]]
                                              [(+ c00 (* a00 b00) (* a01 b10)) (+ c01 (* a00 b01) (* a01 b11))
                                               (+ c10 (* a10 b00) (* a11 b10)) (+ c11 (* a10 b01) (* a11 b11))])))))
                      out (map-indexed vector q)))
            out (map-indexed vector p))))

(defn interval-matrix
  "[m00 m01 m10 m11] = P(k ticks over dt hours, end state | start state)
  for rates a, b and on-intensity lam (per hour)."
  [a b lam dt k]
  (let [k (long k)
        nu (max 1e-9 (* 1.05 (max a (+ b lam))))
        ;; P = I + (Q - Lambda)/nu (degree 0) and Lambda/nu (degree 1)
        p0 [(- 1.0 (/ a nu)) (/ a nu) (/ b nu) (- 1.0 (/ (+ b lam) nu))]
        step (cond-> [p0] (pos? k) (conj [0.0 0.0 0.0 (/ lam nu)]))
        x (* nu dt)
        nmax (long (+ x (* 8 (Math/sqrt (max x 1.0))) 20))]
    (loop [n 0 pw [[1.0 0.0 0.0 1.0]]            ; (P + z B)^n as a polynomial
           w (Math/exp (- x))                     ; Pois(n; x)
           acc [0.0 0.0 0.0 0.0]]
      (if (> n nmax)
        acc
        (let [coef (get pw k [0.0 0.0 0.0 0.0])
              acc (mapv + acc (mapv #(* w %) coef))]
          (recur (inc n) (poly-mul pw step k) (* w (/ x (inc n))) acc))))))

(defn zero-tick-matrix
  "[m00 m01 m10 m11] = P(no ticks over dt hours, end state | start state),
  exp((Q - Lambda) dt) for Q = [[-a a] [b -b]], Lambda = diag(0, lam), in
  closed form from the two eigenvalues of the 2x2 generator."
  [a b lam dt]
  (let [g00 (- a) g01 a g10 b g11 (- (+ b lam))
        tr (+ g00 g11) det (- (* g00 g11) (* g01 g10))
        disc (Math/sqrt (max 0.0 (- (* 0.25 tr tr) det)))
        l1 (+ (* 0.5 tr) disc) l2 (- (* 0.5 tr) disc)
        e1 (Math/exp (* l1 dt)) e2 (Math/exp (* l2 dt))]
    (if (< disc 1e-12)
      ;; repeated eigenvalue: exp(G dt) = e^(l dt) (I + (G - l I) dt)
      [(* e1 (+ 1.0 (* (- g00 l1) dt))) (* e1 g01 dt) (* e1 g10 dt) (* e1 (+ 1.0 (* (- g11 l1) dt)))]
      ;; Sylvester: exp(G dt) = (e1 (G - l2 I) - e2 (G - l1 I)) / (l1 - l2)
      (let [d (- l1 l2)]
        [(/ (- (* e1 (- g00 l2)) (* e2 (- g00 l1))) d) (/ (* (- e1 e2) g01) d)
         (/ (* (- e1 e2) g10) d) (/ (- (* e1 (- g11 l2)) (* e2 (- g11 l1))) d)]))))

(defn- unit-matrix
  "The unit's transition-with-ticks matrix: closed form for no ticks; for
  one tick over a short interval (a precisely timed tick), the point-process
  form exp((Q - Lambda) dt) Lambda dt / ... evaluated as no-event then an
  event at the end; otherwise (several ticks at unknown times) the general
  uniformization count."
  [a b lam dt k]
  (cond
    (zero? k) (zero-tick-matrix a b lam dt)
    (and (== k 1) (< dt (/ 1.0 60)))
    (let [[m00 m01 m10 m11] (zero-tick-matrix a b lam dt)]
      ;; a tick can only come from the on state
      [0.0 (* m01 lam dt) 0.0 (* m11 lam dt)])
    :else (interval-matrix a b lam dt k)))

(defn p3-filter
  "Forward filter over session units: {:p [P(off) P(on)] :loglik L}."
  [units {:keys [c a b]}]
  (loop [[{:keys [dt mass k]} & more :as us] units p0 (/ b (+ a b)) p1 (/ a (+ a b)) ll 0.0]
    (if (empty? us)
      {:p [p0 p1] :loglik ll}
      (let [lam (if (pos? dt) (/ (* c mass) dt) 0.0)
            [m00 m01 m10 m11] (unit-matrix a b lam dt k)
            q0 (+ (* p0 m00) (* p1 m10)) q1 (+ (* p0 m01) (* p1 m11))
            z (+ q0 q1)]
        (if (and (pos? z) (Double/isFinite z))
          (recur more (/ q0 z) (/ q1 z) (+ ll (Math/log z)))
          {:p [p0 p1] :loglik Double/NEGATIVE_INFINITY})))))

(defn fit-p3
  "Penalized ML session parameters {:c :a :b} (per hour; weak normal priors
  on the log scale, sd 3)."
  [units]
  (let [ticks (reduce + 0.0 (map :k units)) mass (reduce + 0.0 (map :mass units))
        x0 [(Math/log (max 1e-6 (* 3 (/ ticks (max 1e-9 mass))))) (Math/log 0.3) (Math/log 1.0)]
        unpack (fn [[lc la lb]] {:c (Math/exp lc) :a (Math/exp la) :b (Math/exp lb)})
        penalty (fn [x] (* 0.5 (reduce + (map (fn [xi mi] (Math/pow (/ (- xi mi) 3.0) 2)) x x0))))
        {x :x} (num/nelder-mead (fn [x] (let [v (:loglik (p3-filter units (unpack x)))]
                                          (if (Double/isFinite v) (+ (- v) (penalty x)) 1e300)))
                                x0 :tol 1e-6 :max-iter 1500)]
    (assoc (unpack x) :rung :P3)))

(defn forecast-p3
  "Meter at `end` from reading `x` at `now`: the session state filtered up
  to now, then on/off switching and ticks simulated over the future profile."
  [{:keys [c a b] :as params} prof zone units now end x & {:keys [n seed] :or {n 3000 seed 1}}]
  (let [[_ p-on] (:p (p3-filter units params))
        ;; future (duration h, mass) per clock hour
        fut (loop [t now out []]
              (if (>= t end) out
                  (let [nxt (min end (* 3600 (inc (quot t 3600))))]
                    (recur nxt (conj out [(/ (- nxt t) 3600.0) (m/mass prof zone t nxt)])))))
        r (num/rng seed)
        draws (double-array n)]
    (dotimes [i n]
      (aset draws i
            (+ (double x)
               (loop [[[dt mass] & more] fut on? (Boolean/valueOf (< (.nextDouble r) p-on)) acc 0.0]
                 (if (or (nil? dt) (>= (+ x acc) 100.0))
                   acc
                   ;; time on within this hour, switching as a continuous-time chain
                   (let [[t-on on?] (loop [t 0.0 on? on? t-on 0.0]
                                      (let [rate (if on? b a)
                                            hold (/ (- (Math/log (- 1.0 (.nextDouble r)))) rate)]
                                        (if (>= (+ t hold) dt)
                                          [(+ t-on (if on? (- dt t) 0.0)) on?]
                                          (recur (+ t hold) (not on?) (+ t-on (if on? hold 0.0))))))
                         lam (if (pos? dt) (/ (* c mass) dt) 0.0)]
                     (recur more on? (+ acc (double (num/poisson-sample r (* lam t-on)))))))))))
    (java.util.Arrays/sort draws)
    {:median (num/quantile draws 0.5) :lo (num/quantile draws 0.05)
     :q25 (num/quantile draws 0.25) :q75 (num/quantile draws 0.75)
     :hi (num/quantile draws 0.95)
     :p-cap (/ (double (count (filter #(>= % 100.0) draws))) n)
     :draws draws}))
