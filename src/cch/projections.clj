(ns cch.projections
  "Forward projections of seven-day rate-limit usage.

  Pure functions over a sequence of observed `{:ts :pct}` samples
  (oldest first, ts in epoch seconds, pct in 0–100). Each projection
  method returns a map describing where it expects usage to land at the
  next reset, with optional uncertainty band.

  None of these methods require external libraries — they ship with bb
  out of the box. Heavier-weight methods (Stan, Prophet, BSTS) belong
  in a separate experiment that shells out to a Python runtime.

  Methods:
    :ewma         exponential-weighted moving average of inter-sample rate
    :ols          ordinary least squares linear regression of (t, pct),
                  Gaussian prediction interval
    :bayes        conjugate Gaussian on the rate parameter, posterior
                  credible interval propagated forward
    :trailing-Nh  mean rate over the last N hours (dumb baseline)")

(def ^:private z-90 1.645) ; standard-normal quantile for ~90% CI/PI

;; --- shared sample preparation ---

;; --- observed-curve smoothing (poor-man's LOESS) ---

(defn- weighted-local-linear
  "WLS fit y = a + b*x at the data, evaluated at x*.
   Returns the predicted y, or nil if the local design is degenerate."
  [xs ys ws x*]
  (let [sw (reduce + 0.0 ws)]
    (when (pos? sw)
      (let [swx  (reduce + 0.0 (map * ws xs))
            swy  (reduce + 0.0 (map * ws ys))
            mx   (/ swx sw)
            my   (/ swy sw)
            swxx (reduce + 0.0 (map (fn [w x] (* w x x)) ws xs))
            swxy (reduce + 0.0 (map (fn [w x y] (* w x y)) ws xs ys))
            sxx  (- swxx (* sw mx mx))
            sxy  (- swxy (* sw mx my))
            b    (if (pos? sxx) (/ sxy sxx) 0.0)
            a    (- my (* b mx))]
        (+ a (* b x*))))))

(defn- tricube
  "Tricube kernel: (1 - u^3)^3 for |u|<1, else 0."
  [u]
  (let [au (Math/abs (double u))]
    (if (>= au 1.0)
      0.0
      (let [v (- 1.0 (* au au au))] (* v v v)))))

(defn- pav-merge
  "If the top two PAV blocks violate monotonicity (top.mean < below.mean),
   pop both and push the merged block; recurse until stable."
  [stack]
  (loop [s stack]
    (if (< (count s) 2)
      s
      (let [top   (peek s)
            below (peek (pop s))]
        (if (< (:mean top) (:mean below))
          (let [c (+ (:count below) (:count top))
                m (/ (+ (* (:mean below) (:count below))
                        (* (:mean top)   (:count top)))
                     c)]
            (recur (conj (pop (pop s)) {:count c :mean m})))
          s)))))

(defn isotonic-pav
  "Pool Adjacent Violators: closest non-decreasing fit to `ys` in L2.
   O(n). Used to constrain a kernel smoother of cumulative usage to be
   monotone non-decreasing (used_percentage can only go up)."
  [ys]
  (let [yv (vec ys)
        n  (count yv)]
    (if (<= n 1)
      yv
      (->> (range n)
           (reduce
             (fn [stack i]
               (pav-merge (conj stack {:count 1 :mean (double (yv i))})))
             [])
           (mapcat (fn [{:keys [count mean]}] (repeat count mean)))
           vec))))

(defn loess-smooth
  "Poor-man's LOESS: local-linear regression with tricube weights at
   `n-eval` evenly-spaced eval points, post-processed with isotonic
   regression (PAV) so the output is monotone non-decreasing.
   `bandwidth-frac` is the kernel half-width as a fraction of the
   observed span (0.2 ~= each fit sees ~40% of the data).
   Returns [{:ts :pct} ...] or nil with too few points."
  [observed n-eval bandwidth-frac]
  (when (>= (count observed) 3)
    (let [xs   (mapv :ts observed)
          ys   (mapv :pct observed)
          xmin (apply min xs)
          xmax (apply max xs)
          span (double (max 1 (- xmax xmin)))
          h    (max 1.0 (* (double bandwidth-frac) span))
          step (/ span (max 1 (dec n-eval)))
          raw  (vec
                 (for [i (range n-eval)
                       :let [x* (+ xmin (* i step))
                             ws (mapv #(tricube (/ (- % x*) h)) xs)
                             y* (weighted-local-linear xs ys ws x*)]
                       :when y*]
                   {:ts (long x*) :raw (max 0.0 y*)}))
          mono (isotonic-pav (mapv :raw raw))]
      (mapv (fn [pt y] {:ts (:ts pt) :pct y}) raw mono))))

(defn thin-by-time
  "Reduce a sequence of `{:ts :pct}` snapshots to one representative
   per `bucket-secs`-wide bucket. Helps when the snapshot stream is
   bursty (e.g. statusLine fires 100 times in a few seconds during
   active use): the OLS fit and chart density don't benefit from
   counting tightly-clustered samples as independent observations.

   Picks the first sample in each bucket — preserves the 'leading
   edge' of each time slice, which for cumulative usage is the
   conservative choice."
  [observed bucket-secs]
  (->> observed
       (group-by #(quot (:ts %) bucket-secs))
       (sort-by key)
       (mapv (comp first val))))

(defn rate-samples
  "Inter-sample rates within the window, %/hr. Drops:
   - sub-15-minute gaps (anchor carries forward — no rate info at
     integer-percent resolution; consecutive close samples coalesce
     into one transition with the next far-enough sample)
   - negative Δpct (window roll-forward; the anchor jumps to the new
     low point, no rate emitted for the discontinuity)

   Returns [{:t :rate :dt-hr} ...] with `t` at the *end* of each
   interval — useful for OLS-on-rates and trailing-window plots."
  [observed]
  (loop [xs observed, anchor nil, out (transient [])]
    (if-let [s (first xs)]
      (cond
        (nil? anchor)
        (recur (rest xs) s out)

        (< (:pct s) (:pct anchor))      ; window roll: jump anchor, no sample
        (recur (rest xs) s out)

        :else
        (let [dt-hr (/ (- (:ts s) (:ts anchor)) 3600.0)]
          (if (< dt-hr 0.25)
            (recur (rest xs) anchor out)
            (let [dpct (- (:pct s) (:pct anchor))
                  rate (/ dpct dt-hr)]
              (recur (rest xs) s
                     (conj! out {:t (:ts s) :rate rate :dt-hr dt-hr}))))))
      (persistent! out))))

;; --- Constrained linear regression on (t, pct) ---

(defn- lag1-autocorr
  "Lag-1 autocorrelation of a residual sequence.
   ρ̂_1 = Σ(e_i · e_{i+1}) / Σ(e_i²)."
  [residuals]
  (let [es (vec residuals)
        n  (count es)]
    (if (< n 2)
      0.0
      (let [sum-prod (reduce + 0.0
                             (map (fn [a b] (* a b)) es (rest es)))
            sum-sq   (reduce + 0.0 (map #(* % %) es))]
        (if (pos? sum-sq) (/ sum-prod sum-sq) 0.0)))))

(defn- hac-variance-factor
  "Newey-West-style autocorrelation correction for OLS variance.
   Uses the (1+ρ)/(1-ρ) Bartlett-kernel limit at lag 1, capped to keep
   the band sensible when adjacent residuals are nearly identical
   (cumulative-usage data routinely has ρ → 0.95+).

   Cumulative pct points are highly autocorrelated by construction, so
   plain OLS underestimates σ² and produces overconfident bands. The
   factor ranges from ~1 (independent residuals) to ~20 (very strong
   positive autocorrelation) to inflate σ² accordingly."
  [residuals]
  (let [rho (lag1-autocorr residuals)]
    (cond
      (>= rho 0.95) 20.0
      (<= rho 0.0)  1.0
      :else         (/ (+ 1.0 rho) (- 1.0 rho)))))

(defn- ols-fit
  "OLS fit of y = a + b*x. Returns coefficients + dispersion stats,
   including a HAC autocorrelation correction factor for the variance."
  [pts]
  (let [n      (count pts)
        xs     (mapv first pts)
        ys     (mapv second pts)
        x-bar  (/ (reduce + 0.0 xs) n)
        y-bar  (/ (reduce + 0.0 ys) n)
        sxx    (reduce + 0.0 (map #(let [d (- % x-bar)] (* d d)) xs))
        sxy    (reduce + 0.0 (map (fn [x y] (* (- x x-bar) (- y y-bar))) xs ys))
        b      (if (pos? sxx) (/ sxy sxx) 0.0)
        a      (- y-bar (* b x-bar))
        resid  (mapv (fn [x y] (- y (+ a (* b x)))) xs ys)
        sse    (reduce + 0.0 (map #(* % %) resid))
        sigma2 (if (> n 2) (/ sse (- n 2)) 0.0)
        hac    (hac-variance-factor resid)]
    {:a a :b b :sxx sxx :x-bar x-bar :n n :sigma2 sigma2 :hac hac}))

(defn- ols-prediction
  "Point estimate + 90% prediction-interval half-width at x_new.
   The variance is inflated by the HAC factor stored on the fit
   to account for autocorrelation in cumulative-pct residuals."
  [{:keys [a b sxx x-bar n sigma2 hac] :or {hac 1.0}} x-new]
  (let [y-pred (+ a (* b x-new))
        se     (when (and (> n 2) (pos? sigma2) (pos? sxx))
                 (Math/sqrt
                   (* sigma2 hac
                      (+ 1.0
                         (/ 1.0 n)
                         (/ (Math/pow (- x-new x-bar) 2) sxx)))))]
    {:pred y-pred
     :half-width (or (some-> se (* z-90)) 0.0)}))

(defn- monotone-linear-fit
  "Least squares fit of y = a + b*x with b constrained to be non-negative
   (cumulative usage can only go up). When unconstrained b ≥ 0 the result
   is identical to OLS; when unconstrained b < 0 we collapse to b=0, a=ȳ —
   a horizontal fit at the sample mean. Residuals are recomputed against
   the constrained line so the prediction interval reflects the actual
   model used.

   Strictly: this is NNLS (non-negative least squares) on the slope. Not
   OLS — OLS is unconstrained — but the constraint almost never binds in
   practice for cumulative data, so the fit numerically equals OLS unless
   the data is genuinely non-monotone."
  [pts]
  (let [{:keys [b sxx x-bar n] :as raw} (ols-fit pts)]
    (if (>= b 0)
      raw
      (let [ys     (mapv second pts)
            y-bar  (/ (reduce + 0.0 ys) n)
            resid  (mapv (fn [y] (- y y-bar)) ys)
            sse    (reduce + 0.0 (map #(* % %) resid))
            sigma2 (if (> n 2) (/ sse (- n 2)) 0.0)
            hac    (hac-variance-factor resid)]
        {:a y-bar :b 0.0 :sxx sxx :x-bar x-bar :n n :sigma2 sigma2 :hac hac}))))

(defn linear-projection
  "Constrained linear regression with built-in monotonicity.
   Fits y = a + b·x via NNLS-on-slope (b ≥ 0), forward-projects to
   resets_at with a Gaussian 90% prediction interval. When unconstrained
   slope is negative (synthetic decreasing data), collapses to b=0 and
   reports rate=0; otherwise this is numerically equal to plain OLS.

   This is the frequentist twin of bayes-projection without the prior:
   no shrinkage toward a baseline, just whatever the data implies."
  [observed {:keys [resets-at window-start last-pct]}]
  (when (>= (count observed) 3)
    (let [to-x (fn [ts] (/ (- ts window-start) 3600.0))
          pts  (mapv (fn [{:keys [ts pct]}] [(to-x ts) pct]) observed)
          fit  (monotone-linear-fit pts)
          {:keys [pred half-width]} (ols-prediction fit (to-x resets-at))
          proj (max last-pct pred)
          lo   (max last-pct (- pred half-width))
          hi   (max last-pct (+ pred half-width))]
      {:method :linear
       :name   "Linear (frequentist, b≥0)"
       ;; Report the constrained slope directly. For a horizontal fit
       ;; this is 0, even when proj > last_pct (the fit's intercept can
       ;; be above the most recent sample on synthetic decreasing data).
       :rate   (max 0.0 (:b fit))
       :proj   proj
       :band   {:lo lo :hi hi}})))

;; --- 3. Bayesian conjugate Gaussian on the rate ---

;; Empirical prior on the average weekly rate.
;; User reports typical end-of-week pct 80-95%, occasionally 100%:
;;   mean_rate ≈ 87.5/(7·24) = 0.521 %/hr
;;   σ on average rate ≈ (15pp/2)/(7·24) ≈ 0.045 %/hr (across-week)
;; This is a strong prior — the user has a confident sense of their
;; baseline. Tightness here is what stops the posterior from running
;; off to whatever the most recent few rate samples suggest.
(def ^:private bayes-prior-mu 0.55)
(def ^:private bayes-prior-sigma 0.045)

;; Floor on observed rate noise: pct is quantized to integer percent,
;; so a 1-hour gap carries ±0.5%/hr quantization noise. Without this
;; floor, synthetic data with identical rates makes σ_ε → 0 and the
;; data overwhelms the prior.
(def ^:private rate-noise-floor-sigma2 0.25)

(defn- median
  "Median of a numeric collection."
  [xs]
  (let [s (vec (sort xs))
        n (count s)]
    (cond
      (zero? n) 0.0
      (odd? n)  (double (nth s (quot n 2)))
      :else     (/ (+ (double (nth s (quot n 2)))
                      (double (nth s (dec (quot n 2)))))
                   2.0))))

(defn- robust-rate-variance
  "MAD-based robust estimate of rate variance.
   σ̂ ≈ 1.4826·MAD for Gaussian data, so σ̂² ≈ 2.198·MAD².
   Much less sensitive than sample variance to bursty outlier rates
   (a single hot session shouldn't blow up the credible interval over
   a multi-day projection horizon)."
  [rates]
  (if (< (count rates) 2)
    0.0
    (let [med (median rates)
          mad (median (map #(Math/abs (- (double %) med)) rates))]
      (* 2.198 mad mad))))

(defn- bayes-rate-posterior
  "Conjugate update for the *average* week-rate R ~ N(μ₀, σ₀²) given
  observed inter-sample rates with robust variance σ_ε² (MAD-based).
  Returns posterior {:mu :sigma2 :sigma-eps2 :tau-avg-hr}."
  [rates dts mu0 sigma0]
  (let [n (count rates)
        r-mean (/ (reduce + 0.0 rates) (max 1 n))
        r-var  (if (> n 1)
                 (robust-rate-variance rates)
                 (* sigma0 sigma0))
        sigma-eps2 (max r-var rate-noise-floor-sigma2)
        tau-avg    (if (zero? n) 1.0 (/ (reduce + 0.0 dts) n))
        tau-0      (/ 1.0 (* sigma0 sigma0))
        tau-data   (if (zero? n) 0.0 (/ n sigma-eps2))
        tau-post   (+ tau-0 tau-data)
        mu-post    (/ (+ (* tau-0 mu0) (* tau-data r-mean)) tau-post)
        sigma2-R   (/ 1.0 tau-post)]
    {:mu mu-post :sigma2 sigma2-R
     :sigma-eps2 sigma-eps2 :tau-avg-hr tau-avg}))

(defn bayes-projection
  "Bayesian Gaussian model with two improvements over the original:

   1. Empirical prior. Centered at 0.55 %/hr (≈ what's needed to land
      at 92% over 7 days), σ₀ = 0.12 %/hr — much tighter than a flat
      'target rate' prior, so a few hours of data barely move the
      posterior unless they're substantially off-prior.

   2. Brownian-motion-style variance. Originally Var[pct(reset)]
      grew as σ²·Δt², which assumes 'whatever rate I infer now is locked
      in for the entire 105-hour horizon' and produces absurd bands.
      Replace with σ²_R·Δt² + σ²_BM·Δt, where σ²_BM ≈ σ²_ε · τ_avg
      (within-week noise integrated over the projection horizon, like
      Brownian motion). At long horizons the linear Δt term dominates
      and the band scales like √Δt rather than Δt.

   Monotonicity comes from clamping the posterior rate at 0 and the
   band lower edge at last_pct."
  [observed {:keys [now resets-at last-pct prior-mu prior-sigma]
             :or   {prior-mu    bayes-prior-mu
                    prior-sigma bayes-prior-sigma}}]
  (let [rs    (rate-samples observed)
        rates (mapv :rate rs)
        dts   (mapv :dt-hr rs)]
    (when (>= (count rates) 2)
      (let [{:keys [mu sigma2 sigma-eps2 tau-avg-hr]}
            (bayes-rate-posterior rates dts prior-mu prior-sigma)
            dt-hr     (max 0.0 (/ (- resets-at now) 3600.0))
            mu*       (max 0.0 mu)            ; non-negative average rate
            sigma-bm2 (* sigma-eps2 tau-avg-hr)
            var-proj  (+ (* sigma2 dt-hr dt-hr)    ; uncertainty in mean R
                         (* sigma-bm2 dt-hr))      ; cumulative within-week noise
            sd-proj   (Math/sqrt var-proj)
            mean-proj (+ last-pct (* mu* dt-hr))
            lo        (max last-pct (- mean-proj (* z-90 sd-proj)))
            hi        (max last-pct (+ mean-proj (* z-90 sd-proj)))]
        {:method :bayes
         :name   "Bayesian (empirical prior, BM)"
         :rate   mu*
         :proj   mean-proj
         :band   {:lo lo :hi hi}}))))

;; --- aggregation ---

(defn all-projections
  "Compute every projection method on the same data. Returns a vector
  of method maps (in display order), filtering out methods that lacked
  enough data."
  [observed window-info]
  (->> [(linear-projection observed window-info)
        (bayes-projection observed window-info)]
       (filterv some?)))
