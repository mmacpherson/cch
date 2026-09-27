(ns cch.usage-baselines
  "Count-process baselines for the usage forecast: a ladder of nested models,
  each adding one ingredient, to show what the production model's
  ingredients buy.

  The meter moves in 1% ticks, so hourly usage y is a count:

    rung 1  homogeneous Poisson       y_h ~ Poisson(lambda * live_h)
    rung 2  inhomogeneous Poisson     y_h ~ Poisson(c * A_h)
    rung 3  negative binomial         y_h ~ Poisson(L_h), L_h ~ Gamma(kappa A_h, kappa / c)
    rung 5  weekly pace               y_h ~ Poisson(c theta_w A_h), theta_w ~ Gamma(alpha, alpha)

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
