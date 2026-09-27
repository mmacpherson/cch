(ns cch.usage-baselines
  "Count-process baselines for the usage forecast: a ladder of nested models,
  each adding one ingredient, to show what the production model's
  ingredients buy.

  The meter moves in 1% ticks, so hourly usage y is a count:

    rung 1  homogeneous Poisson       y_h ~ Poisson(lambda * live_h)
    rung 2  inhomogeneous Poisson     y_h ~ Poisson(c * A_h)
    rung 3  negative binomial         y_h ~ Poisson(L_h), L_h ~ Gamma(kappa A_h, kappa / c)
    rung 5  weekly pace               y_h ~ Poisson(c theta_w A_h), theta_w ~ Gamma(alpha, alpha)
    rung 6  bursts + weekly pace      y_h ~ Poisson(L_h), L_h ~ Gamma(kappa A_h, kappa / (c theta_w))
    rung 7  + on/off blocks           each model block (a day for 7d, an hour for 5h) is on
                                      with probability pi; off blocks emit nothing, on blocks
                                      follow rung 6 with rate c / pi (so the mean is unchanged)
    rung 8  + drifting pace           rung 7 with log theta drifting block to block across
                                      windows: AR(1), stationary sd s, half-life h blocks

  where live_h is the fraction of the hour the collector observed and
  A_h = live_h * profile(hour of week) its activity mass. Rung 3 is rung 2
  with gamma overdispersion (bursts), independent across hours: the
  production model without on/off states or a drifting rate. Parameters
  are maximum likelihood; forecasts are plug-in Monte Carlo sums over the
  future hours. Rungs 1-3 ignore how the current window is going; rung 5
  gives each window its own rate multiplier theta_w (mean 1), learned from
  the window's usage so far by the conjugate gamma update: the simplest
  model that adapts to this week's pace. Pure functions."
  (:require [cch.numeric :as num]
            [cch.usage-model :as m]))

(defn training-steps
  "[[live mass count] ...] from an hour series {hour [live usage]}."
  [series prof zone]
  (mapv (fn [[h [live y]]] [live (* live (nth prof (m/hour-of-week zone h))) (Math/rint y)]) series))

(defn- nb-loglik
  "Log likelihood of counts under rung 3 with mean-per-mass c and
  burstiness kappa (size kappa * A, mean c * A per hour)."
  ^double [stps ^double c ^double kappa]
  (reduce (fn [^double acc [_ a y]]
            (let [a (double a) y (double y)]
              (if (< a 1e-9)
                acc
                (let [size (* kappa a) mu (* c a)]
                  (+ acc
                     (num/log-gamma (+ y size)) (- (num/log-gamma size)) (- (num/log-gamma (+ y 1.0)))
                     (* size (Math/log (/ size (+ size mu))))
                     (if (pos? y) (* y (Math/log (/ mu (+ size mu)))) 0.0))))))
          0.0 stps))

(defn fit
  "Maximum-likelihood parameters of `rung` (1, 2, or 3) from training steps."
  [rung stps]
  (let [sum (fn [f] (reduce + 0.0 (map f stps)))
        total (sum #(nth % 2))]
    (case (long rung)
      1 {:rung 1 :rate (/ total (max 1e-9 (sum first)))}
      2 {:rung 2 :c (/ total (max 1e-9 (sum second)))}
      3 (let [c0 (/ total (max 1e-9 (sum second)))
              ;; the Poisson mean is the ML c for any kappa only in the limit;
              ;; fit both on the log scale
              {[lc lk] :x} (num/nelder-mead (fn [[lc lk]] (- (nb-loglik stps (Math/exp lc) (Math/exp lk))))
                                            [(Math/log (max 1e-6 c0)) 0.0] :tol 1e-8 :max-iter 2000)]
          {:rung 3 :c (Math/exp lc) :kappa (Math/exp lk)}))))

(defn forecast
  "Predictive of the meter at `resets-at` from current reading `x`: Monte
  Carlo over the future hours [now, resets-at) with profile `prof`."
  [{:keys [rung rate c kappa]} prof zone spec now resets-at x & {:keys [n seed] :or {n 3000 seed 1}}]
  (let [pieces (m/future-pieces prof zone spec now resets-at 3600)
        hours (/ (max 0 (- resets-at now)) 3600.0)
        mass (reduce + 0.0 (map second pieces))
        r (num/rng seed)
        draws (double-array n)]
    (dotimes [i n]
      (aset draws i
            (+ (double x)
               (double
                 (case (long rung)
                   1 (num/poisson-sample r (* rate hours))
                   2 (num/poisson-sample r (* c mass))
                   ;; independent gamma rates per hour sum to one gamma over the mass
                   3 (if (pos? mass)
                       (num/poisson-sample r (num/gamma-sample r (* kappa mass) (/ kappa c)))
                       0))))))
    (java.util.Arrays/sort draws)
    {:median (num/quantile draws 0.5) :lo (num/quantile draws 0.05)
     :q25 (num/quantile draws 0.25) :q75 (num/quantile draws 0.75)
     :hi (num/quantile draws 0.95)
     :p-cap (/ (double (count (filter #(>= % 100.0) draws))) n)
     :draws draws}))

;; --- rung 5: a per-window rate multiplier (gamma-Poisson random effect) ---

(defn- window-nb-loglik
  "Log likelihood of window totals [[mass total] ...] with Y_w ~ NB(size
  alpha, mean c * mass): Poisson(c theta mass) with theta ~ Gamma(alpha, alpha)."
  ^double [wtots ^double c ^double alpha]
  (reduce (fn [^double acc [mass y]]
            (let [mu (* c (double mass)) y (double y)]
              (if (< mu 1e-9)
                acc
                (+ acc
                   (num/log-gamma (+ y alpha)) (- (num/log-gamma alpha)) (- (num/log-gamma (+ y 1.0)))
                   (* alpha (Math/log (/ alpha (+ alpha mu))))
                   (if (pos? y) (* y (Math/log (/ mu (+ alpha mu)))) 0.0)))))
          0.0 wtots))

(defn fit-weekly
  "Maximum-likelihood rung-5 parameters from finished windows' [[mass total] ...]."
  [wtots]
  (let [c0 (/ (reduce + 0.0 (map second wtots)) (max 1e-9 (reduce + 0.0 (map first wtots))))
        {[lc la] :x} (num/nelder-mead (fn [[lc la]] (- (window-nb-loglik wtots (Math/exp lc) (Math/exp la))))
                                      [(Math/log (max 1e-6 c0)) 0.0] :tol 1e-8 :max-iter 2000)]
    {:rung 5 :c (Math/exp lc) :alpha (Math/exp la)}))

(defn forecast-weekly
  "Rung-5 predictive of the meter at `resets-at`: the window's multiplier
  updated by its usage so far (`x` over activity mass since `start`)."
  [{:keys [c alpha]} prof zone spec start now resets-at x & {:keys [n seed] :or {n 3000 seed 1}}]
  (let [m-obs (reduce + 0.0 (map second (m/future-pieces prof zone spec start now 3600)))
        m-fut (reduce + 0.0 (map second (m/future-pieces prof zone spec now resets-at 3600)))
        shape (+ alpha (double x))
        rate (+ alpha (* c m-obs))
        r (num/rng seed)
        draws (double-array n)]
    (dotimes [i n]
      (aset draws i (+ (double x)
                       (if (pos? m-fut)
                         (double (num/poisson-sample r (* c m-fut (num/gamma-sample r shape rate))))
                         0.0))))
    (java.util.Arrays/sort draws)
    {:median (num/quantile draws 0.5) :lo (num/quantile draws 0.05)
     :q25 (num/quantile draws 0.25) :q75 (num/quantile draws 0.75)
     :hi (num/quantile draws 0.95)
     :p-cap (/ (double (count (filter #(>= % 100.0) draws))) n)
     :draws draws}))

;; --- rung 6: bursts and a per-window multiplier ---
;;
;; Given theta, every hour of a window has the same negative-binomial success
;; probability p = c theta / (kappa + c theta), so the window total is exactly
;; NB(kappa M, p) and the hourly likelihood factors into a theta-free term (which
;; identifies kappa from hourly dispersion) and (1 - p)^(kappa M) p^Y. The
;; integral over theta ~ Gamma(alpha, alpha) is done on a grid in log theta.

(def ^:private log-theta-grid
  "Grid in log theta, fine enough for windows with ~100 ticks (posterior width
  ~0.1 in log theta)."
  (vec (range -12.0 5.0 0.02)))

(defn- window-log-terms
  "Per grid point: log[(1 - p)^(kappa m) p^y Gamma(theta; alpha, alpha) theta],
  the integrand over log theta."
  [c kappa alpha m y]
  (let [lnorm (- (* alpha (Math/log alpha)) (num/log-gamma alpha))]
    (mapv (fn [u]
            (let [theta (Math/exp u)
                  ct (* c theta)
                  lp (- (Math/log ct) (Math/log (+ kappa ct)))
                  l1p (- (Math/log kappa) (Math/log (+ kappa ct)))]
              (+ (* kappa m l1p) (* y lp) lnorm (* alpha u) (- (* alpha theta)))))
          log-theta-grid)))

(defn- log-sum-exp [xs]
  (let [mx (apply max xs)]
    (+ mx (Math/log (reduce + (map #(Math/exp (- % mx)) xs))))))

(defn- rung6-loglik
  "Log likelihood of finished windows [{:mass :total :hours [[a y] ...]}]."
  [windows c kappa alpha]
  (let [du 0.02]
    (reduce + (for [{:keys [mass total hours]} windows]
                (+ (reduce + (for [[a y] hours :when (> a 1e-9)
                                   :let [r (* kappa a)]]
                               (- (+ (num/log-gamma (+ y r))) (num/log-gamma r) (num/log-gamma (+ y 1.0)))))
                   (log-sum-exp (window-log-terms c kappa alpha mass total))
                   (Math/log du))))))

(defn window-training
  "Rung-6 training data at time `t`: finished windows (within `lookback`
  seconds, or all when nil) as {:mass :total :hours [[mass count] ...]},
  each counting only the hours it covered."
  [wins t prof zone lookback]
  (let [series (m/hour-series wins t)]
    (vec (for [w wins
               :when (and (<= (:eff-end w) t) (or (nil? lookback) (>= (:start w) (- t lookback))))
               :let [hours (vec (for [[h [live y]] (subseq series >= (* 3600 (quot (:start w) 3600)) < (:eff-end w))]
                                  [(* live (nth prof (m/hour-of-week zone h))) (Math/rint y)]))]]
           {:mass (reduce + 0.0 (map first hours))
            :total (reduce + 0.0 (map second hours))
            :hours hours}))))

(defn fit-bursts-weekly
  "Maximum-likelihood rung-6 parameters from finished windows
  [{:mass :total :hours [[mass count] ...]}]."
  [windows]
  (let [c0 (/ (reduce + 0.0 (map :total windows)) (max 1e-9 (reduce + 0.0 (map :mass windows))))
        {[lc lk la] :x} (num/nelder-mead (fn [[lc lk la]]
                                           (let [v (rung6-loglik windows (Math/exp lc) (Math/exp lk) (Math/exp la))]
                                             (if (Double/isFinite v) (- v) 1e300)))
                                         [(Math/log (max 1e-6 c0)) 0.0 0.0] :tol 1e-7 :max-iter 3000)]
    {:rung 6 :c (Math/exp lc) :kappa (Math/exp lk) :alpha (Math/exp la)}))

(defn forecast-bursts-weekly
  "Rung-6 predictive of the meter at `resets-at`: theta's posterior on the
  grid given the window's usage so far, then bursty usage over the future
  mass."
  [{:keys [c kappa alpha]} prof zone spec start now resets-at x & {:keys [n seed] :or {n 3000 seed 1}}]
  (let [m-obs (reduce + 0.0 (map second (m/future-pieces prof zone spec start now 3600)))
        m-fut (reduce + 0.0 (map second (m/future-pieces prof zone spec now resets-at 3600)))
        lw (window-log-terms c kappa alpha m-obs (Math/rint x))
        mx (apply max lw)
        cum (double-array (reductions + (map #(Math/exp (- % mx)) lw)))
        total (aget cum (dec (alength cum)))
        r (num/rng seed)
        draws (double-array n)]
    (dotimes [i n]
      (let [u (* total (.nextDouble r))
            j (let [k (java.util.Arrays/binarySearch cum u)] (if (neg? k) (- (inc k)) k))
            ;; uniform within the grid cell
            theta (Math/exp (+ (nth log-theta-grid (min j (dec (count log-theta-grid)))) (* 0.02 (- (.nextDouble r) 0.5))))]
        (aset draws i (+ (double x)
                         (if (pos? m-fut)
                           (double (num/poisson-sample r (num/gamma-sample r (* kappa m-fut) (/ kappa (* c theta)))))
                           0.0)))))
    (java.util.Arrays/sort draws)
    {:median (num/quantile draws 0.5) :lo (num/quantile draws 0.05)
     :q25 (num/quantile draws 0.25) :q75 (num/quantile draws 0.75)
     :hi (num/quantile draws 0.95)
     :p-cap (/ (double (count (filter #(>= % 100.0) draws))) n)
     :draws draws}))

;; --- rung 7: rung 6 with on/off model blocks ---
;;
;; Blocks follow the production model's structure (m/block-start?). Given
;; theta and the on-block rate c' = c / pi, an on block's hourly likelihood is
;; exp(C_b) (1 - p)^(kappa M_b) p^(Y_b), p = c' theta / (kappa + c' theta), with
;; C_b the theta-free sum over its hours; an off block has likelihood 1 when
;; empty and 0 otherwise. A window's likelihood is the product over blocks,
;; integrated over theta on the log grid.

(defn block-training
  "Rung-7 training data: finished windows (as `window-training`) with their
  hours grouped into model blocks: [{:blocks [[[mass count] ...] ...]}]."
  [wins t prof zone spec lookback]
  (let [series (m/hour-series wins t)]
    (vec (for [w wins
               :when (and (<= (:eff-end w) t) (or (nil? lookback) (>= (:start w) (- t lookback))))
               :let [hrs (subseq series >= (* 3600 (quot (:start w) 3600)) < (:eff-end w))
                     blocks (reduce (fn [acc [h [live y]]]
                                      (let [hour [(* live (nth prof (m/hour-of-week zone h))) (Math/rint y)]]
                                        (if (or (empty? acc) (m/block-start? zone spec h))
                                          (conj acc [hour])
                                          (conj (pop acc) (conj (peek acc) hour)))))
                                    [] hrs)]
               :when (seq blocks)]
           {:blocks blocks}))))

(defn- hours-c
  "The theta-free hourly term of an on block."
  ^double [hours ^double kappa]
  (reduce + 0.0 (for [[a y] hours :when (> a 1e-9) :let [r (* kappa a)]]
                  (- (num/log-gamma (+ y r)) (num/log-gamma r) (num/log-gamma (+ y 1.0))))))

(defn- block-log-terms
  "Per grid point: log P(block | theta) for one block summarized as
  [mass total C], mixing on (prob pi) and off."
  [c-on kappa pi [mass total cterm]]
  (let [lpi (Math/log pi) l1pi (Math/log (- 1.0 pi))]
    (mapv (fn [u]
            (let [ct (* c-on (Math/exp u))
                  lp (- (Math/log ct) (Math/log (+ kappa ct)))
                  l1p (- (Math/log kappa) (Math/log (+ kappa ct)))
                  on (+ lpi cterm (* kappa mass l1p) (* total lp))]
              (if (pos? total)
                on
                (let [mx (max on l1pi)]
                  (+ mx (Math/log (+ (Math/exp (- on mx)) (Math/exp (- l1pi mx)))))))))
          log-theta-grid)))

(defn- theta-prior-terms
  "log[Gamma(theta; alpha, alpha) theta] per grid point (the integrand over log theta)."
  [alpha]
  (let [lnorm (- (* alpha (Math/log alpha)) (num/log-gamma alpha))]
    (mapv (fn [u] (+ lnorm (* alpha u) (- (* alpha (Math/exp u))))) log-theta-grid)))

(defn- summarize-block [hours kappa]
  [(reduce + 0.0 (map first hours)) (reduce + 0.0 (map second hours)) (hours-c hours kappa)])

(defn- rung7-loglik [windows c kappa alpha pi]
  (let [c-on (/ c pi)
        prior (theta-prior-terms alpha)]
    (reduce + (for [{:keys [blocks]} windows]
                (+ (log-sum-exp (reduce (fn [acc b] (mapv + acc (block-log-terms c-on kappa pi (summarize-block b kappa))))
                                        prior blocks))
                   (Math/log 0.02))))))

(defn fit-onoff
  "Maximum-likelihood rung-7 parameters {:c :kappa :alpha :pi} from `block-training` data."
  [windows]
  (let [tot (reduce + 0.0 (for [{:keys [blocks]} windows b blocks [_ y] b] y))
        mass (reduce + 0.0 (for [{:keys [blocks]} windows b blocks [a _] b] a))
        c0 (/ tot (max 1e-9 mass))
        sig (fn [x] (/ 1.0 (+ 1.0 (Math/exp (- x)))))
        {[lc lk la lpi] :x} (num/nelder-mead
                              (fn [[lc lk la lpi]]
                                (let [v (rung7-loglik windows (Math/exp lc) (Math/exp lk) (Math/exp la) (sig lpi))]
                                  (if (Double/isFinite v) (- v) 1e300)))
                              [(Math/log (max 1e-6 c0)) 0.0 0.0 0.0] :tol 1e-7 :max-iter 4000)]
    {:rung 7 :c (Math/exp lc) :kappa (Math/exp lk) :alpha (Math/exp la) :pi (sig lpi)}))

(defn blocks-so-far
  "The current window's hours before `now`, grouped into model blocks:
  [done-blocks current], each block [[mass count] ...]; `current` is the block
  in progress (empty when `now` falls on a block boundary)."
  [series start now prof zone spec]
  (let [hrs (subseq series >= (* 3600 (quot start 3600)) < now)
        groups (reduce (fn [acc [h [live y]]]
                         (let [hour [(* live (nth prof (m/hour-of-week zone h))) (Math/rint y)]]
                           (if (or (empty? acc) (m/block-start? zone spec h))
                             (conj acc [hour])
                             (conj (pop acc) (conj (peek acc) hour)))))
                       [] hrs)
        boundary? (and (zero? (mod now 3600)) (m/block-start? zone spec now))]
    (if (or boundary? (empty? groups))
      [groups []]
      [(pop groups) (peek groups)])))

(defn forecast-onoff
  "Rung-7 predictive of the meter at `resets-at`. `done-blocks` are the
  window's finished blocks and `current` the hours so far of the block in
  progress (each [[mass count] ...]); the future comes from the profile."
  [{:keys [c kappa alpha pi]} prof zone spec done-blocks current now resets-at x
   & {:keys [n seed] :or {n 3000 seed 1}}]
  (let [c-on (/ c pi)
        pieces (m/future-pieces prof zone spec now resets-at 3600)
        m-cur (reduce + 0.0 (keep (fn [[_ mass idx]] (when (zero? idx) mass)) pieces))
        m-later (mapv (fn [[_ g]] (reduce + 0.0 (map second g)))
                      (group-by #(nth % 2) (remove #(zero? (nth % 2)) pieces)))
        base (reduce (fn [acc b] (mapv + acc (block-log-terms c-on kappa pi (summarize-block b kappa))))
                     (theta-prior-terms alpha) done-blocks)
        cur (summarize-block current kappa)
        cur-terms (block-log-terms c-on kappa pi cur)
        lw (mapv + base cur-terms)
        mx (apply max lw)
        cum (double-array (reductions + (map #(Math/exp (- % mx)) lw)))
        total (aget cum (dec (alength cum)))
        r (num/rng seed)
        draws (double-array n)]
    (dotimes [i n]
      (let [u (* total (.nextDouble r))
            j (min (dec (count log-theta-grid))
                   (let [k (java.util.Arrays/binarySearch cum u)] (if (neg? k) (- (inc k)) k)))
            theta (Math/exp (+ (nth log-theta-grid j) (* 0.02 (- (.nextDouble r) 0.5))))
            ;; P(current block on | theta, its usage so far)
            [cm ct cc] cur
            ctheta (* c-on theta)
            lon (+ (Math/log pi) cc (* kappa cm (- (Math/log kappa) (Math/log (+ kappa ctheta))))
                   (* ct (- (Math/log ctheta) (Math/log (+ kappa ctheta)))))
            p-on (if (pos? ct) 1.0 (/ 1.0 (+ 1.0 (Math/exp (- (Math/log (- 1.0 pi)) lon)))))
            m-on (+ (if (< (.nextDouble r) p-on) m-cur 0.0)
                    (reduce + 0.0 (filter (fn [_] (< (.nextDouble r) pi)) m-later)))]
        (aset draws i (+ (double x)
                         (if (pos? m-on)
                           (double (num/poisson-sample r (num/gamma-sample r (* kappa m-on) (/ kappa ctheta))))
                           0.0)))))
    (java.util.Arrays/sort draws)
    {:median (num/quantile draws 0.5) :lo (num/quantile draws 0.05)
     :q25 (num/quantile draws 0.25) :q75 (num/quantile draws 0.75)
     :hi (num/quantile draws 0.95)
     :p-cap (/ (double (count (filter #(>= % 100.0) draws))) n)
     :draws draws}))

;; --- rung 8: rung 7 with a drifting pace ---
;;
;; log theta follows a stationary AR(1) over model blocks, across window
;; boundaries: u' = phi u + e, e ~ N(0, s^2 (1 - phi^2)), phi = 2^(-1/h). The
;; likelihood is an exact forward filter on a grid in u: a banded Gaussian
;; transition per block, then rung 7's on/off block likelihood.

(def ^:private drift-grid (double-array (range -6.0 4.0 0.05)))

(defn training-blocks
  "All hours of the series before `t` (within `lookback`), grouped into model
  blocks, in time order: [[[mass count] ...] ...]."
  [series t prof zone spec lookback]
  (let [hrs (if lookback (subseq series >= (- t lookback) < t) (subseq series < t))]
    (reduce (fn [acc [h [live y]]]
              (let [hour [(* live (nth prof (m/hour-of-week zone h))) (Math/rint y)]]
                (if (or (empty? acc) (m/block-start? zone spec h))
                  (conj acc [hour])
                  (conj (pop acc) (conj (peek acc) hour)))))
            [] hrs)))

(defn- block-obs
  "log P(block | u) on the drift grid for a block summarized as [mass total C]."
  ^doubles [c-on kappa pi [mass total cterm]]
  (let [lpi (Math/log pi) l1pi (Math/log (- 1.0 pi)) n (alength ^doubles drift-grid)
        out (double-array n)]
    (dotimes [i n]
      (let [ct (* c-on (Math/exp (aget ^doubles drift-grid i)))
            lp (- (Math/log ct) (Math/log (+ kappa ct)))
            l1p (- (Math/log kappa) (Math/log (+ kappa ct)))
            on (+ lpi cterm (* kappa mass l1p) (* total lp))]
        (aset out i (if (pos? total)
                      on
                      (let [mx (max on l1pi)]
                        (+ mx (Math/log (+ (Math/exp (- on mx)) (Math/exp (- l1pi mx))))))))))
    out))

(defn- kernel
  "The AR(1) transition on the drift grid as, per source point j, the first
  target index and normalized weights: computed once per parameter set."
  [^double phi ^double innov]
  (let [n (alength ^doubles drift-grid) du 0.05
        band (long (Math/ceil (/ (* 5.0 innov) du)))]
    (vec (for [j (range n)]
           (let [mu (* phi (aget ^doubles drift-grid j))
                 center (long (Math/round (/ (- mu -6.0) du)))
                 lo (max 0 (- center band)) hi (min (dec n) (+ center band))
                 ws (double-array (for [i (range lo (inc hi))]
                                    (let [z (/ (- (aget ^doubles drift-grid i) mu) innov)] (Math/exp (* -0.5 z z)))))
                 tot (reduce + ws)]
             (when (pos? tot) (dotimes [k (alength ws)] (aset ws k (/ (aget ws k) tot))))
             [lo ws])))))

(defn- transition
  "Probability vector p (on the drift grid) after one step of kernel `kern`."
  ^doubles [^doubles p kern]
  (let [n (alength p) out (double-array n)]
    (dotimes [j n]
      (let [pj (aget p j)]
        (when (> pj 1e-300)
          (let [[lo ^doubles ws] (nth kern j)]
            (dotimes [k (alength ws)]
              (let [i (+ (long lo) k)]
                (aset out i (+ (aget out i) (* pj (aget ws k))))))))))
    out))

(defn- stationary [^double s]
  (let [n (alength ^doubles drift-grid)
        w (double-array (map #(Math/exp (* -0.5 (Math/pow (/ % s) 2))) drift-grid))
        tot (reduce + w)]
    (dotimes [i n] (aset w i (/ (aget w i) tot)))
    w))

(defn- drift-filter
  "Forward filter over `blocks`; returns {:p posterior-vector :loglik L}."
  [blocks {:keys [c kappa pi s h]}]
  (let [c-on (/ c pi) phi (Math/pow 2.0 (/ -1.0 h))
        innov (max 1e-3 (* s (Math/sqrt (- 1.0 (* phi phi)))))
        kern (kernel phi innov)]
    (loop [[b & more] blocks p (stationary s) ll 0.0 first? true]
      (if (nil? b)
        {:p p :loglik ll}
        (let [^doubles p (if first? p (transition p kern))
              lo (block-obs c-on kappa pi (summarize-block b kappa))
              mx (areduce lo i m Double/NEGATIVE_INFINITY (max m (aget lo i)))
              post (double-array (alength p))
              z (loop [i 0 z 0.0]
                  (if (= i (alength p)) z
                      (let [v (* (aget p i) (Math/exp (- (aget lo i) mx)))]
                        (aset post i v) (recur (inc i) (+ z v)))))]
          (dotimes [i (alength post)] (aset post i (/ (aget post i) z)))
          (recur more post (+ ll mx (Math/log z)) false))))))

(defn fit-drift
  "Maximum-likelihood rung-8 parameters {:c :kappa :pi :s :h} from `training-blocks`."
  [blocks]
  (let [tot (reduce + 0.0 (for [b blocks [_ y] b] y))
        mass (reduce + 0.0 (for [b blocks [a _] b] a))
        c0 (/ tot (max 1e-9 mass))
        sig (fn [x] (/ 1.0 (+ 1.0 (Math/exp (- x)))))
        unpack (fn [[lc lk lpi ls lh]] {:c (Math/exp lc) :kappa (Math/exp lk) :pi (sig lpi)
                                        :s (Math/exp ls) :h (Math/exp lh)})
        {x :x} (num/nelder-mead (fn [x] (let [v (:loglik (drift-filter blocks (unpack x)))]
                                          (if (Double/isFinite v) (- v) 1e300)))
                                [(Math/log (max 1e-6 c0)) 0.0 0.0 (Math/log 0.5) (Math/log 7.0)]
                                :tol 1e-6 :max-iter 3000)]
    (assoc (unpack x) :rung 8)))

(defn forecast-drift
  "Rung-8 predictive of the meter at `resets-at`: filter the history's
  finished blocks, condition on the block in progress, then simulate the
  pace drifting over the remaining blocks."
  [{:keys [c kappa pi s h] :as params} prof zone spec done-blocks current now resets-at x
   & {:keys [n seed] :or {n 3000 seed 1}}]
  (let [c-on (/ c pi) phi (Math/pow 2.0 (/ -1.0 h))
        innov (max 1e-3 (* s (Math/sqrt (- 1.0 (* phi phi)))))
        {p :p} (drift-filter done-blocks params)
        p (if (seq done-blocks) (transition p (kernel phi innov)) (stationary s))
        cur (summarize-block current kappa)
        lo (block-obs c-on kappa pi cur)
        mx (areduce lo i m Double/NEGATIVE_INFINITY (max m (aget lo i)))
        cum (double-array (reductions + (map (fn [pi_ li] (* pi_ (Math/exp (- li mx)))) p lo)))
        total (aget cum (dec (alength cum)))
        pieces (m/future-pieces prof zone spec now resets-at 3600)
        m-cur (reduce + 0.0 (keep (fn [[_ mass idx]] (when (zero? idx) mass)) pieces))
        m-later (mapv (fn [[_ g]] (reduce + 0.0 (map second g)))
                      (sort-by key (group-by #(nth % 2) (remove #(zero? (nth % 2)) pieces))))
        r (num/rng seed)
        draws (double-array n)
        [cm ct cc] cur]
    (dotimes [i n]
      (let [u0 (let [v (* total (.nextDouble r))
                     j (min (dec (alength cum)) (let [k (java.util.Arrays/binarySearch cum v)] (if (neg? k) (- (inc k)) k)))]
                 (+ (aget ^doubles drift-grid j) (* 0.05 (- (.nextDouble r) 0.5))))
            burst (fn [mass u] (if (pos? mass)
                                 (double (num/poisson-sample r (num/gamma-sample r (* kappa mass) (/ kappa (* c-on (Math/exp u))))))
                                 0.0))
            ctheta (* c-on (Math/exp u0))
            lon (+ (Math/log pi) cc (* kappa cm (- (Math/log kappa) (Math/log (+ kappa ctheta))))
                   (* ct (- (Math/log ctheta) (Math/log (+ kappa ctheta)))))
            p-on (if (pos? ct) 1.0 (/ 1.0 (+ 1.0 (Math/exp (- (Math/log (- 1.0 pi)) lon)))))
            now-use (if (< (.nextDouble r) p-on) (burst m-cur u0) 0.0)]
        (aset draws i (+ (double x) now-use
                         (loop [[mb & more] m-later u u0 acc 0.0]
                           (if (nil? mb) acc
                               (let [u (+ (* phi u) (* innov (.nextGaussian r)))]
                                 (recur more u (+ acc (if (< (.nextDouble r) pi) (burst mb u) 0.0))))))))))
    (java.util.Arrays/sort draws)
    {:median (num/quantile draws 0.5) :lo (num/quantile draws 0.05)
     :q25 (num/quantile draws 0.25) :q75 (num/quantile draws 0.75)
     :hi (num/quantile draws 0.95)
     :p-cap (/ (double (count (filter #(>= % 100.0) draws))) n)
     :draws draws}))
