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
