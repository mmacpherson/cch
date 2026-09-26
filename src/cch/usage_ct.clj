(ns cch.usage-ct
  "Continuous-time variant of the usage model (claude-code-hooks-lbz, arms B
  and C). Experimental: used by the backtest, not by production forecasts.

  The production model (cch.usage-model) updates its states once per
  calendar block: 24 hours for the 7d meter, 1 hour for the 5h meter. Here
  both meters use hourly steps with continuous-time dynamics, so there is no
  block length to choose:

  * On/off is a two-state continuous-time Markov chain with switching rates
    q-on (off -> on) and q-off (on -> off). Over dt the probability of being on
    relaxes toward p = q-on/(q-on + q-off) at rate q-on + q-off, and the chain
    is filtered exactly by the forward algorithm.
  * Intensity has a fast and a slow gamma belief. The fast belief relaxes
    toward the slow one with half-life h-fast (hours); the slow belief relaxes
    toward the prior with half-life h-slow. Each hour's usage updates the fast
    belief fully and the slow belief with weight w. With w = 0 the slow belief
    stays at the prior, which is arm B (one timescale). This is a
    discount-type approximate filter, not an exact posterior; the backtest
    judges it.

  Observation model, profile, windows, and quantization are shared with
  cch.usage-model."
  (:require [cch.numeric :as num]
            [cch.usage-model :as m])
  (:import (java.time ZoneId)))

(def ^:private ln2 (Math/log 2.0))

(defn- sigmoid ^double [^double x] (/ 1.0 (+ 1.0 (Math/exp (- x)))))

(defn unpack
  "Unconstrained vector -> parameters
  {:kappa :alpha0 :beta0 :q-on :q-off :h-fast :h-slow :w}."
  [[lk la lb lqon lqoff lhf lhs lw]]
  {:kappa (Math/exp lk) :alpha0 (Math/exp la) :beta0 (Math/exp lb)
   :q-on (Math/exp lqon) :q-off (Math/exp lqoff)
   :h-fast (Math/exp lhf) :h-slow (Math/exp lhs) :w (sigmoid lw)})

(def x0
  "Starting point: burstiness and intensity prior near the 5h fit, sessions of
  a few hours, a fast half-life of 3 hours, a slow one of 3 days, w = 0.3."
  [-1.2 1.6 0.7 (Math/log 0.2) (Math/log 0.3) (Math/log 3.0) (Math/log 72.0) (Math/log (/ 0.3 0.7))])

(def arms
  "Parameters held fixed per arm (unconstrained index -> value). Arm B pins
  w at ~0, and the slow half-life is then irrelevant, so it is pinned too."
  {:B {6 (Math/log 72.0) 7 -30.0}
   :C {}})

(defn steps
  "Hourly steps from an hour series: [[hour profile-mass usage] ...]."
  [series prof ^ZoneId zone]
  (mapv (fn [[h [live y]]] [h (* live (nth prof (m/hour-of-week zone h))) y]) series))

(defn- on-transition
  "P(on after dt | on-probability p before), for the two-state chain."
  ^double [^double p q-on q-off ^double dt]
  (let [s (+ q-on q-off)
        stat (/ q-on s)]
    (+ stat (* (- p stat) (Math/exp (- (* s dt)))))))

(defn run-filter
  "Filter the on/off chain and the two intensity beliefs through hourly
  `stps`. Returns {:fast [a b] :slow [a b] :p-on p :loglik L}."
  [stps {:keys [kappa alpha0 beta0 q-on q-off h-fast h-slow w]} score?]
  (let [df (Math/exp (- (/ ln2 h-fast)))
        ds (Math/exp (- (/ ln2 h-slow)))]
    (loop [[s & more] stps af alpha0 bf beta0 as alpha0 bs beta0
           p (/ q-on (+ q-on q-off)) ll 0.0]
      (if (nil? s)
        {:fast [af bf] :slow [as bs] :p-on p :loglik ll}
        (let [[_ A y] s
              as (+ (* ds as) (* (- 1.0 ds) alpha0))
              bs (+ (* ds bs) (* (- 1.0 ds) beta0))
              af (+ (* df af) (* (- 1.0 df) as))
              bf (+ (* df bf) (* (- 1.0 df) bs))
              pp (on-transition p q-on q-off 1.0)
              pg (m/interval-prob y A af bf kappa)
              tot (+ (* pp pg) (* (- 1.0 pp) (if (< y 0.5) 1.0 0.0)))
              r (/ (* pp pg) tot)]
          (recur more
                 (+ af (* r kappa A)) (+ bf (* r y))
                 (+ as (* w r kappa A)) (+ bs (* w r y))
                 r
                 (if score? (+ ll (Math/log (max tot 1e-300))) ll)))))))

(defn fit
  "Maximum-likelihood parameters for `stps` (with a weak prior), holding the
  arm's fixed indices."
  [stps arm x-start]
  (let [fixed (arms arm)
        free (vec (remove (set (keys fixed)) (range (count x-start))))
        full (fn [xf] (reduce-kv assoc (reduce (fn [v [i xi]] (assoc v i xi)) (vec x-start) (map vector free xf))
                                 fixed))
        ;; A weak Gaussian prior (sd 3) around the starting point keeps the
        ;; optimizer out of degenerate corners (e.g. alpha0 -> infinity with a
        ;; vanishing rate); it is negligible against real data.
        obj (fn [xf] (+ (- (:loglik (run-filter stps (unpack (full xf)) true)))
                        ;; free parameters only: fixed ones are not estimated
                        (* 0.5 (reduce + (map (fn [xi i] (Math/pow (/ (- xi (nth x-start i)) 3.0) 2)) xf free)))))
        start (mapv #(nth x-start %) free)
        r1 (num/nelder-mead obj start :tol 1e-6 :max-iter 2000)
        r2 (num/nelder-mead obj (:x r1) :step 0.2 :tol 1e-6 :max-iter 2000)
        best (if (<= (:fx r2) (:fx r1)) r2 r1)
        x (full (:x best))]
    {:x x :params (unpack x) :nll (:fx best)}))

(def ^:private fit-lookback-secs (* 42 86400))

(declare fit-c x0-c)

(defn fit-model
  "Fit arm `arm` (:B or :C) from hourly aggregate rows observed before
  `now`, with the given activity profile. Hourly steps for either meter."
  [rows spec zone now arm profile & {:keys [x-start]}]
  (let [series (m/hour-series (m/windows rows spec) now)
        recent (into (sorted-map) (filter #(>= (key %) (- now fit-lookback-secs)) series))
        stps (steps recent profile zone)
        {:keys [x params nll]} (if (= arm :C)
                                 (fit-c stps (or x-start x0-c))
                                 (fit stps arm (or x-start x0)))]
    {:x x :params params :nll nll :profile profile :arm arm :fitted-at now}))

(declare forecast forecast-c)

(defn forecast-arm
  "Forecast with a fitted arm-B or arm-C model."
  [model rows spec zone now resets-at x]
  (if (= :C (:arm model))
    (forecast-c model rows spec zone now resets-at x)
    (forecast model rows spec zone now resets-at x)))

(declare forecast-steps forecast-steps-c)

(defn forecast
  "Monte Carlo predictive of demand at `resets-at` for window pct `x`.
  Returns {:median :lo :q25 :q75 :hi :p-cap :draws} like
  cch.usage-model/predict."
  [{:keys [params profile]} rows spec zone now resets-at x & {:keys [n seed] :or {n 3000 seed 1}}]
  (let [series (into (sorted-map) (filter #(>= (key %) (- now (* 14 86400)))
                                          (m/hour-series (m/windows rows spec) now)))]
    (forecast-steps params (steps series profile zone)
                    (mapv second (m/future-pieces profile zone spec now resets-at 3600))
                    x :n n :seed seed)))

(defn forecast-steps
  "Arm-B predictive of `x` plus usage over future hourly masses `fut`,
  after filtering history `stps`."
  [params stps fut x & {:keys [n seed] :or {n 3000 seed 1}}]
  (let [{:keys [kappa alpha0 beta0 q-on q-off h-fast h-slow]} params
        {[af bf] :fast [as bs] :slow p-on :p-on} (run-filter stps params false)
        df (Math/exp (- (/ ln2 h-fast)))
        ds (Math/exp (- (/ ln2 h-slow)))
        stay-on (on-transition 1.0 q-on q-off 1.0)
        turn-on (on-transition 0.0 q-on q-off 1.0)
        r (num/rng seed)
        draws (double-array n)]
    (dotimes [i n]
      ;; Mean usage per unit mass is 1/lambda; blend fast -> slow -> prior.
      (let [mf (/ 1.0 (num/gamma-sample r af bf))
            ms (/ 1.0 (num/gamma-sample r as bs))
            m0 (/ 1.0 (num/gamma-sample r alpha0 beta0))]
        (aset draws i
              (loop [j 0 acc (double x) on? (< (.nextDouble r) p-on) wf df ws ds]
                (if (= j (count fut))
                  acc
                  (let [on? (< (.nextDouble r) (if on? stay-on turn-on))
                        mean (+ (* wf mf) (* (- 1.0 wf) (+ (* ws ms) (* (- 1.0 ws) m0))))
                        A (nth fut j)
                        use (if (and on? (pos? A)) (num/gamma-sample r (* kappa A) (/ 1.0 mean)) 0.0)]
                    (recur (inc j) (+ acc use) on? (* wf df) (* ws ds))))))))
    (java.util.Arrays/sort draws)
    {:median (num/quantile draws 0.5) :lo (num/quantile draws 0.05)
     :q25 (num/quantile draws 0.25) :q75 (num/quantile draws 0.75)
     :hi (num/quantile draws 0.95)
     :p-cap (/ (double (count (filter #(>= % 100.0) draws))) n)
     :draws draws}))

;; --- simulation, for parameter-recovery checks ---

(defn simulate
  "Hourly usage from a two-timescale generative process with known
  parameters: on/off from the continuous-time chain; a slow log-rate
  mean-reverting with half-life h-slow and a fast log-rate around it with
  half-life h-fast (both Gaussian AR(1) in hourly steps); usage in an on hour
  ~ Gamma(kappa * a_h, rate lambda). Returns [[hour mass usage] ...] with mass
  from `prof`. The filter is an approximation to this process, so recovery
  is checked for the identifiable parts (on/off rates, the fast half-life,
  and the ranking of B and C)."
  [{:keys [kappa q-on q-off h-fast h-slow sd-fast sd-slow mean-log-rate]} prof ^ZoneId zone t0 hours seed]
  (let [r (num/rng seed)
        phi-f (Math/exp (- (/ ln2 h-fast)))
        phi-s (Math/exp (- (/ ln2 h-slow)))
        stay-on (on-transition 1.0 q-on q-off 1.0)
        turn-on (on-transition 0.0 q-on q-off 1.0)]
    (loop [k 0 on? true us 0.0 uf 0.0 out (transient [])]
      (if (= k hours)
        (persistent! out)
        (let [h (+ t0 (* k 3600))
              on? (< (.nextDouble r) (if on? stay-on turn-on))
              us (+ (* phi-s us) (* sd-slow (Math/sqrt (- 1.0 (* phi-s phi-s))) (.nextGaussian r)))
              uf (+ (* phi-f uf) (* sd-fast (Math/sqrt (- 1.0 (* phi-f phi-f))) (.nextGaussian r)))
              lam (Math/exp (- (+ mean-log-rate us uf)))
              A (nth prof (m/hour-of-week zone h))
              y (if on? (Math/rint (num/gamma-sample r (* kappa A) lam)) 0.0)]
          (recur (inc k) on? us uf (conj! out [h A y])))))))

;; --- arm C: two log-rate Ornstein-Uhlenbeck components, Laplace-Kalman ---
;;
;; Latent log mean usage per unit profile mass: eta = mu + u-fast + u-slow,
;; where each u is a stationary Ornstein-Uhlenbeck process (Gaussian, AR(1)
;; in hourly steps, phi = 2^(-1/h), stationary sd sigma). Usage in an on hour
;; is Gamma(kappa * A, rate kappa * exp(-eta)), so its mean is A * exp(eta).
;; Per hour: predict the 2-d Gaussian state exactly; integrate the quantized
;; likelihood over eta with Gauss-Hermite for the predictive probability;
;; find the Laplace posterior of eta, and condition the 2-d state on it (the
;; Kalman gain splits the surprise by the components' variances); mix the
;; on and off branches by the responsibility.

(defn unpack-c
  "Unconstrained vector -> arm-C parameters."
  [[lk mu lsf lss lhf lhs lqon lqoff]]
  {:kappa (Math/exp lk) :mu mu :sd-fast (Math/exp lsf) :sd-slow (Math/exp lss)
   :h-fast (Math/exp lhf) :h-slow (Math/exp lhs) :q-on (Math/exp lqon) :q-off (Math/exp lqoff)})

(def x0-c
  "Start: burstiness 0.3, mean 3% per unit mass, fast sd 0.5 over 3 h, slow sd
  0.5 over 3 days, sessions of a few hours."
  [(Math/log 0.3) (Math/log 3.0) (Math/log 0.5) (Math/log 0.5) (Math/log 3.0) (Math/log 72.0)
   (Math/log 0.2) (Math/log 0.3)])

(defn- interval-prob-given-rate
  "P(Y in [y - 1/2, y + 1/2]) for Y ~ Gamma(k, rate r), in the smaller tail."
  ^double [^double y ^double k ^double r]
  (let [hi (* r (+ y 0.5))
        lo (* r (max 0.0 (- y 0.5)))
        p (if (> (num/gamma-inc k lo) 0.5)
            (- (num/gamma-inc-upper k lo) (num/gamma-inc-upper k hi))
            (- (num/gamma-inc k hi) (num/gamma-inc k lo)))]
    (min 1.0 (max 1e-300 p))))

(defn- obs-prob
  "P(y | eta) for an on hour with mass A."
  [y A kappa eta]
  (let [k (* kappa A)]
    (if (<= k 1e-9)
      (if (< y 0.5) 1.0 1e-300)
      (interval-prob-given-rate y k (* kappa (Math/exp (- eta)))))))

(defn- predictive-on
  "P(y | past) for an on hour: the observation probability integrated over
  eta ~ N(m, v) by 10-point Gauss-Hermite."
  [y A kappa m v]
  (let [{xs :x ws :w} num/gauss-hermite-10
        s (Math/sqrt (* 2.0 v))]
    (/ (reduce + (map (fn [x w] (* w (obs-prob y A kappa (+ m (* s x))))) xs ws))
       (Math/sqrt Math/PI))))

(defn- laplace-eta
  "Mode and variance of p(eta | y) proportional to P(y | eta) N(eta; m, v),
  by Newton with numerical derivatives."
  [y A kappa m v]
  (let [g (fn [e] (- (Math/log (obs-prob y A kappa e)) (/ (Math/pow (- e m) 2) (* 2.0 v))))
        h 1e-4]
    (loop [e m i 0]
      (let [g0 (g e) gp (g (+ e h)) gm (g (- e h))
            d1 (/ (- gp gm) (* 2 h))
            d2 (min -1e-9 (/ (+ gp gm (* -2 g0)) (* h h)))
            e2 (- e (/ d1 d2))
            e2 (max (- m (* 10 (Math/sqrt v))) (min (+ m (* 10 (Math/sqrt v))) e2))]
        (if (or (> i 30) (< (Math/abs (- e2 e)) 1e-7))
          [e2 (/ -1.0 d2)]
          (recur e2 (inc i)))))))

(defn run-filter-c
  "Filter arm C through hourly `stps`. Returns
  {:m [m-fast m-slow] :P [[pff pfs] [pfs pss]] :p-on p :loglik L}."
  [stps {:keys [kappa mu sd-fast sd-slow h-fast h-slow q-on q-off]} score?]
  (let [pf (Math/exp (- (/ ln2 h-fast))) ps (Math/exp (- (/ ln2 h-slow)))
        qf (* sd-fast sd-fast (- 1.0 (* pf pf))) qs (* sd-slow sd-slow (- 1.0 (* ps ps)))]
    (loop [[s & more] stps mf 0.0 ms 0.0
           pff (* sd-fast sd-fast) pss (* sd-slow sd-slow) pfs 0.0
           p (/ q-on (+ q-on q-off)) ll 0.0]
      (if (nil? s)
        {:m [mf ms] :P [[pff pfs] [pfs pss]] :p-on p :loglik ll}
        (let [[_ A y] s
              ;; predict
              mf (* pf mf) ms (* ps ms)
              pff (+ (* pf pf pff) qf) pss (+ (* ps ps pss) qs) pfs (* pf ps pfs)
              pp (on-transition p q-on q-off 1.0)
              m (+ mu mf ms)
              v (+ pff pss (* 2 pfs))
              pon (predictive-on y A kappa m v)
              tot (+ (* pp pon) (* (- 1.0 pp) (if (< y 0.5) 1.0 0.0)))
              r (/ (* pp pon) tot)
              ;; on branch: condition the state on the Laplace posterior of eta
              [e-hat v-hat] (laplace-eta y A kappa m v)
              gf (/ (+ pff pfs) v) gs (/ (+ pss pfs) v)
              shrink (/ (- v v-hat) v)
              mf-on (+ mf (* gf (- e-hat m))) ms-on (+ ms (* gs (- e-hat m)))
              cf (+ pff pfs) cs (+ pss pfs)
              pff-on (- pff (* cf cf (/ shrink v))) pss-on (- pss (* cs cs (/ shrink v)))
              pfs-on (- pfs (* cf cs (/ shrink v)))
              ;; mix on (weight r) and off (the prediction), matching moments
              mf2 (+ (* r mf-on) (* (- 1.0 r) mf)) ms2 (+ (* r ms-on) (* (- 1.0 r) ms))
              dff (fn [a b] (* (- a mf2) (- b mf2))) dss (fn [a b] (* (- a ms2) (- b ms2)))
              pff2 (+ (* r (+ pff-on (dff mf-on mf-on))) (* (- 1.0 r) (+ pff (dff mf mf))))
              pss2 (+ (* r (+ pss-on (dss ms-on ms-on))) (* (- 1.0 r) (+ pss (dss ms ms))))
              pfs2 (+ (* r (+ pfs-on (* (- mf-on mf2) (- ms-on ms2))))
                      (* (- 1.0 r) (+ pfs (* (- mf mf2) (- ms ms2)))))]
          (recur more mf2 ms2 pff2 pss2 pfs2 r
                 (if score? (+ ll (Math/log (max tot 1e-300))) ll)))))))

(defn fit-c
  "Maximum-likelihood arm-C parameters for `stps` (weak sd-3 prior around the
  start on the unconstrained scale, as in `fit`)."
  [stps x-start]
  (let [obj (fn [x] (+ (- (:loglik (run-filter-c stps (unpack-c x) true)))
                       (* 0.5 (reduce + (map (fn [xi si] (Math/pow (/ (- xi si) 3.0) 2)) x x-start)))))
        r1 (num/nelder-mead obj x-start :tol 1e-6 :max-iter 2000)
        r2 (num/nelder-mead obj (:x r1) :step 0.2 :tol 1e-6 :max-iter 2000)
        best (if (<= (:fx r2) (:fx r1)) r2 r1)]
    {:x (:x best) :params (unpack-c (:x best)) :nll (:fx best)}))

(defn forecast-c
  "Monte Carlo predictive of demand at `resets-at` for arm C."
  [{:keys [params profile]} rows spec zone now resets-at x & {:keys [n seed] :or {n 3000 seed 1}}]
  (let [series (into (sorted-map) (filter #(>= (key %) (- now (* 14 86400)))
                                          (m/hour-series (m/windows rows spec) now)))]
    (forecast-steps-c params (steps series profile zone)
                      (mapv second (m/future-pieces profile zone spec now resets-at 3600))
                      x :n n :seed seed)))

(defn forecast-steps-c
  "Arm-C predictive of `x` plus usage over future hourly masses `fut`,
  after filtering history `stps`."
  [params stps fut x & {:keys [n seed] :or {n 3000 seed 1}}]
  (let [{:keys [kappa mu sd-fast sd-slow h-fast h-slow q-on q-off]} params
        {[mf ms] :m [[pff pfs] [_ pss]] :P p-on :p-on} (run-filter-c stps params false)
        pf (Math/exp (- (/ ln2 h-fast))) ps (Math/exp (- (/ ln2 h-slow)))
        nf (* sd-fast (Math/sqrt (- 1.0 (* pf pf)))) ns (* sd-slow (Math/sqrt (- 1.0 (* ps ps))))
        ;; Cholesky of the 2x2 state covariance
        l11 (Math/sqrt (max 1e-12 pff)) l21 (/ pfs l11) l22 (Math/sqrt (max 1e-12 (- pss (* l21 l21))))
        stay-on (on-transition 1.0 q-on q-off 1.0)
        turn-on (on-transition 0.0 q-on q-off 1.0)
        r (num/rng seed)
        draws (double-array n)]
    (dotimes [i n]
      (let [z1 (.nextGaussian r) z2 (.nextGaussian r)]
        (aset draws i
              (loop [j 0 acc (double x) on? (< (.nextDouble r) p-on)
                     uf (+ mf (* l11 z1)) us (+ ms (* l21 z1) (* l22 z2))]
                (if (= j (count fut))
                  acc
                  (let [on? (< (.nextDouble r) (if on? stay-on turn-on))
                        uf (+ (* pf uf) (* nf (.nextGaussian r)))
                        us (+ (* ps us) (* ns (.nextGaussian r)))
                        A (nth fut j)
                        use (if (and on? (pos? A))
                              (num/gamma-sample r (* kappa A) (* kappa (Math/exp (- (+ mu uf us)))))
                              0.0)]
                    (recur (inc j) (+ acc use) on? uf us)))))))
    (java.util.Arrays/sort draws)
    {:median (num/quantile draws 0.5) :lo (num/quantile draws 0.05)
     :q25 (num/quantile draws 0.25) :q75 (num/quantile draws 0.75)
     :hi (num/quantile draws 0.95)
     :p-cap (/ (double (count (filter #(>= % 100.0) draws))) n)
     :draws draws}))
