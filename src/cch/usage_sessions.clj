(ns cch.usage-sessions
  "Quota-meter ticks driven by concurrent agent sessions (P9).

  The meter's tick rate scales with how many agent sessions are working at
  once (Claude, last 28 days: ~0.3 ticks/h with one session active in an
  hour, ~3.9 with five or more), which no small set of latent states can
  express. The local hook events carry session ids, so concurrency is
  observed: a session's events split into activity spells at gaps longer
  than `gap-secs`, and N(t) is the number of spells covering t.

    ticks in hour h ~ NegBin(mean exp(u_h) * (b * M_h + c * S_h), size kappa)

  M_h is the hour's exposure times the time-of-week profile (a baseline for
  usage the hooks cannot see: other machines, the web), S_h its session
  hours times the profile, u a slow pace (stationary AR(1) over hours,
  half-life >= 24 h), and kappa the hourly burstiness (a gamma-mixed
  Poisson: without it, hour-to-hour overdispersion given the session count
  is pushed into the pace). For forecasting, sessions follow an M/M/inf process:
  spells start at a * profile(t) and last an exponential time (rate mu),
  both fitted in closed form from the spells.

  Pure functions; only counts of distinct session ids per time are used."
  (:require [cch.numeric :as num]
            [cch.usage-baselines :as base]
            [cch.usage-model :as m]))

;; --- activity spells ---

(defn spells
  "Activity spells from events [[ts session-id] ...] (any order): a
  session's events split at gaps longer than `gap-secs`, each spell
  [start end] from its first to its last event. Sorted by start."
  [events gap-secs]
  (->> (group-by second events)
       (mapcat (fn [[_ evs]]
                 (let [ts (sort (map first evs))]
                   (reduce (fn [acc t]
                             (let [[s e] (peek acc)]
                               (if (and e (<= (- t e) gap-secs))
                                 (conj (pop acc) [s t])
                                 (conj acc [t t]))))
                           [] ts))))
       (sort-by first)
       vec))

(defn active-at
  "Spells active at `t` given only events before t: a session had an event
  within `gap-secs` before t. A spell spanning t qualifies too (its events
  are never more than gap-secs apart, so one fell in that stretch)."
  [spells t gap-secs]
  (count (filter (fn [[s e]] (and (< s t) (or (>= e t) (<= (- t e) gap-secs)))) spells)))

;; --- hourly observations ---

(defn hourly-obs
  "[[ticks base-mass session-mass] ...] per clock hour in [since, t) from the
  tick model's hourly base (cch.usage-ticks/hours-before shape: [[ticks
  mass] ...] with mass = exposure * profile) and spells (session hours in
  each hour, times its profile value and exposed fraction)."
  [tick-hours since t spells prof zone]
  (let [h0 (* 3600 (quot since 3600))
        n (count tick-hours)
        sess (double-array n)]
    (doseq [[s e] spells
            :let [s (max s h0) e (min e t)]
            :when (< s e)]
      (loop [u s]
        (when (< u e)
          (let [hi (quot (- u h0) 3600)
                nxt (min e (+ h0 (* 3600 (inc hi))))]
            (when (< -1 hi n)
              (aset sess hi (+ (aget sess hi) (* (/ (- nxt u) 3600.0) (nth prof (m/hour-of-week zone (+ h0 (* 3600 hi))))))))
            (recur nxt)))))
    (vec (map-indexed (fn [i [y mass]]
                        ;; session hours only count while exposed (not capped)
                        (let [p (nth prof (m/hour-of-week zone (+ h0 (* 3600 i))))
                              live (if (pos? p) (/ mass p) 0.0)]
                          [y mass (* (aget sess i) (min 1.0 live))]))
                      tick-hours))))

;; --- the tick model given concurrency ---

(defn filter-p9
  "Forward filter of the pace over hourly observations [[y M S] ...] on the
  rung-8 grid: {:p posterior :loglik L}."
  [hours {:keys [b c s h kappa]}]
  (let [grid ^doubles base/drift-grid ng (alength grid)
        phi (Math/pow 2.0 (/ -1.0 h))
        kern (base/kernel phi (max 1e-3 (* s (Math/sqrt (- 1.0 (* phi phi))))))]
    (loop [[[y mm ss] & more :as hs] hours ^doubles p (base/stationary s) ll 0.0 first? true]
      (if (empty? hs)
        {:p p :loglik ll}
        (let [^doubles p (if first? p (base/transition p kern))
              mu (+ (* b mm) (* c ss))]
          (if (<= mu 0.0)
            (recur more p ll false)
            (let [lo (double-array ng) k (double kappa)
                  lconst (- (num/log-gamma (+ y k)) (num/log-gamma k) (num/log-gamma (+ y 1.0)))
                  _ (dotimes [i ng]
                      (let [m-i (* mu (Math/exp (aget grid i)))]
                        ;; negative binomial: size kappa, mean m-i
                        (aset lo i (+ lconst (* k (Math/log (/ k (+ k m-i)))) (* y (Math/log (/ m-i (+ k m-i))))))))
                  mx (areduce lo i m Double/NEGATIVE_INFINITY (max m (aget lo i)))
                  post (double-array ng)
                  z (loop [i 0 z 0.0]
                      (if (= i ng) z
                          (let [v (* (aget p i) (Math/exp (- (aget lo i) mx)))]
                            (aset post i v) (recur (inc i) (+ z v)))))]
              (dotimes [i ng] (aset post i (/ (aget post i) z)))
              (recur more post (+ ll mx (Math/log z)) false))))))))

(defn fit-p9
  "Penalized ML {:b :c :s :h :kappa} of the tick model given observed
  concurrency (weak priors, sd 3; sd 1 for the pace spread and half-life,
  h >= 24 h). With :fix-c, the session term is off (c = 0): a drifting
  negative-binomial rate b * mass, used for session arrivals."
  [hours & {:keys [fix-c]}]
  (let [ys (reduce + 0.0 (map first hours))
        ss (reduce + 0.0 (map #(nth % 2) hours))
        mm (reduce + 0.0 (map second hours))
        x0 [(Math/log (max 1e-4 (* (if fix-c 1.0 0.2) (/ ys (max 1e-9 mm))))) (Math/log (max 1e-4 (* 0.8 (/ ys (max 1e-9 ss)))))
            (Math/log 0.4) (Math/log 48.0) 0.0]
        sd [3.0 3.0 1.0 1.0 3.0]
        unpack (fn [[lb lc ls lh lk]] {:b (Math/exp lb) :c (if fix-c 0.0 (Math/exp lc)) :s (Math/exp ls) :h (+ 24.0 (Math/exp lh))
                                       :kappa (Math/exp lk)})
        penalty (fn [x] (* 0.5 (reduce + (map (fn [xi mi d] (Math/pow (/ (- xi mi) d) 2)) x x0 sd))))
        {x :x} (num/nelder-mead (fn [x] (let [v (:loglik (filter-p9 hours (unpack x)))]
                                          (if (Double/isFinite v) (+ (- v) (penalty x)) 1e300)))
                                x0 :tol 1e-6 :max-iter 1500)]
    (unpack x)))

;; --- the session process (M/M/inf) ---

(defn arrival-hours
  "[[starts mass 0.0] ...] per clock hour in [since, t): spell starts and the
  hour's profile mass (the filter-p9 shape with no session term)."
  [spells since t prof zone]
  (let [h0 (* 3600 (quot since 3600))
        n (max 0 (quot (- t h0) 3600))
        starts (double-array n)]
    (doseq [[s _] spells :when (and (>= s h0) (< s t))]
      (let [hi (quot (- s h0) 3600)] (when (< -1 hi n) (aset starts hi (inc (aget starts hi))))))
    (vec (for [i (range n)]
           [(aget starts i) (double (nth prof (m/hour-of-week zone (+ h0 (* 3600 i))))) 0.0]))))

(defn fit-sessions
  "The session process from spells in [since, t): arrivals NegBin(a *
  profile * exp(u_a), size kappa-a) per hour with a drifting arrival pace
  u_a (spread s-a, half-life h-a >= 24 h), fitted by the same filter as the
  ticks; durations exponential with rate mu per hour (closed form). The
  filtered arrival pace at t is kept for forecasting (:arrival-hours)."
  [spells since t prof zone]
  (let [in (filter (fn [[s _]] (and (>= s since) (< s t))) spells)
        dur (reduce + 0.0 (map (fn [[s e]] (/ (- (min e t) s) 3600.0)) in))
        ah (arrival-hours spells since t prof zone)
        {:keys [b s h kappa]} (fit-p9 (mapv (fn [[y mm _]] [y mm 0.0]) ah) :fix-c true)]
    {:a b :s-a s :h-a h :kappa-a kappa
     :mu (/ (max 1 (count in)) (max 1e-3 dur))}))

(defn forecast-p9
  "Meter at `end` from reading `x` at `now`: the pace filtered through the
  hourly observations, `n-active` sessions running now, and future
  sessions and ticks simulated hour by hour (pace clamped to the grid)."
  [{:keys [b c s h a mu kappa s-a h-a kappa-a]} prof zone hours arrivals n-active now end x & {:keys [n seed] :or {n 3000 seed 1}}]
  (let [grid ^doubles base/drift-grid
        {^doubles p :p} (filter-p9 hours {:b b :c c :s s :h h :kappa kappa})
        cum (double-array (reductions + p))
        total (aget cum (dec (alength cum)))
        ;; the arrival pace, filtered through the observed session starts
        {^doubles pa :p} (filter-p9 arrivals {:b a :c 0.0 :s s-a :h h-a :kappa kappa-a})
        cum-a (double-array (reductions + pa))
        total-a (aget cum-a (dec (alength cum-a)))
        phi-a (Math/pow 2.0 (/ -1.0 h-a)) innov-a (* s-a (Math/sqrt (- 1.0 (* phi-a phi-a))))
        phi (Math/pow 2.0 (/ -1.0 h)) innov (* s (Math/sqrt (- 1.0 (* phi phi))))
        lo-g (aget grid 0) hi-g (aget grid (dec (alength grid)))
        fut (loop [t now out []]
              (if (>= t end) out
                  (let [nxt (min end (* 3600 (inc (quot t 3600))))]
                    (recur nxt (conj out [(/ (- nxt t) 3600.0) (m/mass prof zone t nxt)])))))
        r (num/rng seed)
        draws (double-array n)]
    (dotimes [i n]
      (let [v (* total (.nextDouble r))
            j (min (dec (alength cum)) (let [q (java.util.Arrays/binarySearch cum v)] (if (neg? q) (- (inc q)) q)))
            u0 (aget grid j)
            ua0 (aget grid (min (dec (alength cum-a))
                                (let [v (* total-a (.nextDouble r)) q (java.util.Arrays/binarySearch cum-a v)]
                                  (if (neg? q) (- (inc q)) q))))
            ;; remaining lives of the sessions running now (memoryless)
            active0 (vec (repeatedly n-active #(/ (- (Math/log (- 1.0 (.nextDouble r)))) mu)))]
        (aset draws i
              (+ (double x)
                 (loop [[[dt mass] & more] fut u u0 ua ua0 active active0 acc 0.0 first? true]
                   (if (or (nil? dt) (>= (+ x acc) 100.0))
                     acc
                     (let [u (if first? u (max lo-g (min hi-g (+ (* phi u) (* innov (.nextGaussian r))))))
                           ua (if first? ua (max lo-g (min hi-g (+ (* phi-a ua) (* innov-a (.nextGaussian r))))))
                           prof-rate (if (pos? dt) (/ mass dt) 0.0)
                           ;; new sessions this piece: NegBin(a * e^ua * mass), uniform start, exponential life
                           arr-mean (* a (Math/exp ua) mass)
                           arrivals (vec (for [_ (range (if (pos? arr-mean) (num/poisson-sample r (num/gamma-sample r kappa-a (/ kappa-a arr-mean))) 0))]
                                           (let [st (* dt (.nextDouble r))]
                                             [st (/ (- (Math/log (- 1.0 (.nextDouble r)))) mu)])))
                           sess-hours (+ (reduce + 0.0 (map #(min dt %) active))
                                         (reduce + 0.0 (map (fn [[st life]] (min (- dt st) life)) arrivals)))
                           active' (vec (concat (keep #(when (> % dt) (- % dt)) active)
                                                (keep (fn [[st life]] (when (> (+ st life) dt) (- (+ st life) dt))) arrivals)))
                           mean (* (Math/exp u) (+ (* b mass) (* c sess-hours prof-rate)))]
                       ;; bursts: the hour's rate is gamma-distributed around its mean
                       (recur more u ua active'
                              (+ acc (if (pos? mean) (double (num/poisson-sample r (num/gamma-sample r kappa (/ kappa mean)))) 0.0))
                              false))))))))
    (java.util.Arrays/sort draws)
    {:median (num/quantile draws 0.5) :lo (num/quantile draws 0.05)
     :q25 (num/quantile draws 0.25) :q75 (num/quantile draws 0.75)
     :hi (num/quantile draws 0.95)
     :p-cap (/ (double (count (filter #(>= % 100.0) draws))) n)
     :draws draws}))

;; --- P10: away / available on the observed session starts ---
;;
;; Hourly session starts follow a two-state hidden Markov chain (away,
;; available; switching rates r1 away->available, r2 available->away per
;; hour, exact hourly steps) with a slow pace u (half-life >= 24 h) on the
;; available rate: starts ~ NegBin(mean a * profile * exp(u) (available) or
;; a * f * profile (away), size kappa). Day-scale structure (working vs idle
;; days) then lives in the chain instead of the pace.

(defn- hour-step
  "2x2 hourly transition [p00 p01 p10 p11] of the away/available chain."
  [r1 r2]
  (let [tot (+ r1 r2) e (Math/exp (- tot))
        p01 (* (/ r1 tot) (- 1.0 e)) p10 (* (/ r2 tot) (- 1.0 e))]
    [(- 1.0 p01) p01 p10 (- 1.0 p10)]))

(defn- nb-log
  "log NegBin(y; size k, mean m)."
  ^double [^double y ^double k ^double m]
  (+ (num/log-gamma (+ y k)) (- (num/log-gamma k)) (- (num/log-gamma (+ y 1.0)))
     (* k (Math/log (/ k (+ k m)))) (* y (Math/log (/ (max m 1e-300) (+ k m))))))

(defn filter-arrivals
  "Joint filter (away/available x pace grid) over arrival hours [[starts
  mass _] ...]: {:away grid-vector :avail grid-vector :loglik L}."
  [hours {:keys [a f r1 r2 s h kappa]}]
  (let [grid ^doubles base/drift-grid ng (alength grid)
        phi (Math/pow 2.0 (/ -1.0 h))
        kern (base/kernel phi (max 1e-3 (* s (Math/sqrt (- 1.0 (* phi phi))))))
        [p00 p01 p10 p11] (hour-step r1 r2)
        ^doubles stat (base/stationary s)
        pi1 (/ r1 (+ r1 r2))]
    (loop [[[y mass] & more :as hs] hours
           ^doubles aw (amap stat i _ (* (- 1.0 pi1) (aget stat i)))
           ^doubles av (amap stat i _ (* pi1 (aget stat i)))
           ll 0.0 first? true]
      (if (empty? hs)
        {:away aw :avail av :loglik ll}
        (let [;; hour step: chain switch, then pace drift
              ^doubles aw1 (if first? aw (base/transition (amap aw i _ (+ (* (aget aw i) p00) (* (aget av i) p10))) kern))
              ^doubles av1 (if first? av (base/transition (amap av i _ (+ (* (aget aw i) p01) (* (aget av i) p11))) kern))
              y (double y) mass (double mass)
              la (nb-log y kappa (* a f mass))
              lv (double-array ng)
              _ (dotimes [i ng] (aset lv i (nb-log y kappa (* a mass (Math/exp (aget grid i))))))
              mx (max la (areduce lv i mm Double/NEGATIVE_INFINITY (max mm (aget lv i))))
              aw2 (double-array ng) av2 (double-array ng)
              z (loop [i 0 z 0.0]
                  (if (= i ng) z
                      (let [x0 (* (aget aw1 i) (Math/exp (- la mx))) x1 (* (aget av1 i) (Math/exp (- (aget lv i) mx)))]
                        (aset aw2 i x0) (aset av2 i x1) (recur (inc i) (+ z x0 x1)))))]
          (dotimes [i ng] (aset aw2 i (/ (aget aw2 i) z)) (aset av2 i (/ (aget av2 i) z)))
          (recur more aw2 av2 (+ ll mx (Math/log z)) false))))))

(defn fit-arrivals
  "Penalized ML P10 arrival parameters {:a :f :r1 :r2 :s :h :kappa}."
  [hours]
  (let [sig (fn [x] (/ 1.0 (+ 1.0 (Math/exp (- x))))) logit (fn [p] (Math/log (/ p (- 1.0 p))))
        ys (reduce + 0.0 (map first hours)) mm (reduce + 0.0 (map second hours))
        x0 [(Math/log (max 1e-4 (* 2 (/ ys (max 1e-9 mm))))) (logit 0.02) (Math/log 0.05) (Math/log 0.1)
            (Math/log 0.4) (Math/log 48.0) 0.0]
        sd [3.0 3.0 3.0 3.0 1.0 1.0 3.0]
        unpack (fn [[la lf l1 l2 ls lh lk]] {:a (Math/exp la) :f (sig lf) :r1 (Math/exp l1) :r2 (Math/exp l2)
                                             :s (Math/exp ls) :h (+ 24.0 (Math/exp lh)) :kappa (Math/exp lk)})
        penalty (fn [x] (* 0.5 (reduce + (map (fn [xi mi d] (Math/pow (/ (- xi mi) d) 2)) x x0 sd))))
        {x :x} (num/nelder-mead (fn [x] (let [v (:loglik (filter-arrivals hours (unpack x)))]
                                          (if (Double/isFinite v) (+ (- v) (penalty x)) 1e300)))
                                x0 :tol 1e-6 :max-iter 1500)]
    (unpack x)))

(defn forecast-p10
  "P9's forecast with P10's arrivals: the away/available state and the
  arrival pace sampled from the arrival filter and simulated forward."
  [{:keys [b c s h mu kappa] :as params} arrival-params prof zone hours arrivals n-active now end x
   & {:keys [n seed] :or {n 3000 seed 1}}]
  (let [grid ^doubles base/drift-grid ng (alength grid)
        lo-g (aget grid 0) hi-g (aget grid (dec ng))
        {^doubles p :p} (filter-p9 hours {:b b :c c :s s :h h :kappa kappa})
        cum (double-array (reductions + p)) total (aget cum (dec (alength cum)))
        phi (Math/pow 2.0 (/ -1.0 h)) innov (* s (Math/sqrt (- 1.0 (* phi phi))))
        {:keys [a f r1 r2] sa :s ha :h ka :kappa} arrival-params
        {aw :away av :avail} (filter-arrivals arrivals arrival-params)
        cum-a (double-array (reductions + (concat (seq aw) (seq av)))) total-a (aget cum-a (dec (alength cum-a)))
        phi-a (Math/pow 2.0 (/ -1.0 ha)) innov-a (* sa (Math/sqrt (- 1.0 (* phi-a phi-a))))
        [p00 p01 p10 p11] (hour-step r1 r2)
        pick (fn [^doubles cm tot r] (min (dec (alength cm)) (let [q (java.util.Arrays/binarySearch cm (* tot (.nextDouble ^java.util.SplittableRandom r)))] (if (neg? q) (- (inc q)) q))))
        fut (loop [t now out []]
              (if (>= t end) out
                  (let [nxt (min end (* 3600 (inc (quot t 3600))))]
                    (recur nxt (conj out [(/ (- nxt t) 3600.0) (m/mass prof zone t nxt)])))))
        r (num/rng seed)
        draws (double-array n)]
    (dotimes [i n]
      (let [u0 (aget grid (pick cum total r))
            ja (pick cum-a total-a r)
            avail0 (>= ja ng) ua0 (aget grid (mod ja ng))
            active0 (vec (repeatedly n-active #(/ (- (Math/log (- 1.0 (.nextDouble r)))) mu)))]
        (aset draws i
              (+ (double x)
                 (loop [[[dt mass] & more] fut u u0 ua ua0 avail? avail0 active active0 acc 0.0 first? true]
                   (if (or (nil? dt) (>= (+ x acc) 100.0))
                     acc
                     (let [u (if first? u (max lo-g (min hi-g (+ (* phi u) (* innov (.nextGaussian r))))))
                           ua (if first? ua (max lo-g (min hi-g (+ (* phi-a ua) (* innov-a (.nextGaussian r))))))
                           avail? (if first? avail? (< (.nextDouble r) (if avail? p11 p01)))
                           prof-rate (if (pos? dt) (/ mass dt) 0.0)
                           arr-mean (* a mass (if avail? (Math/exp ua) f))
                           arrivals (vec (for [_ (range (if (pos? arr-mean) (num/poisson-sample r (num/gamma-sample r ka (/ ka arr-mean))) 0))]
                                           (let [st (* dt (.nextDouble r))]
                                             [st (/ (- (Math/log (- 1.0 (.nextDouble r)))) mu)])))
                           sess-hours (+ (reduce + 0.0 (map #(min dt %) active))
                                         (reduce + 0.0 (map (fn [[st life]] (min (- dt st) life)) arrivals)))
                           active' (vec (concat (keep #(when (> % dt) (- % dt)) active)
                                                (keep (fn [[st life]] (when (> (+ st life) dt) (- (+ st life) dt))) arrivals)))
                           mean (* (Math/exp u) (+ (* b mass) (* c sess-hours prof-rate)))]
                       (recur more u ua avail? active'
                              (+ acc (if (pos? mean) (double (num/poisson-sample r (num/gamma-sample r kappa (/ kappa mean)))) 0.0))
                              false))))))))
    (java.util.Arrays/sort draws)
    {:median (num/quantile draws 0.5) :lo (num/quantile draws 0.05)
     :q25 (num/quantile draws 0.25) :q75 (num/quantile draws 0.75)
     :hi (num/quantile draws 0.95)
     :p-cap (/ (double (count (filter #(>= % 100.0) draws))) n)
     :draws draws}))
