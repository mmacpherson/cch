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

(def ^:private target-pct-per-hr
  "Linear pace for the 7-day window: 100% ÷ (7 days × 24 hours)."
  (/ 100.0 (* 7 24)))

(def ^:private z-90 1.645) ; standard-normal quantile for ~90% CI/PI

;; --- shared sample preparation ---

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

;; --- 1. EWMA (point estimate, fast/slow band as cheap proxy) ---

(defn- ewma-fold
  "Single-pass EWMA over rate samples with given tau (in hours)."
  [tau-hr rate-samples]
  (reduce
    (fn [prev {:keys [rate dt-hr]}]
      (if (nil? prev)
        rate
        (let [alpha (- 1.0 (Math/exp (- (/ dt-hr tau-hr))))]
          (+ (* alpha rate) (* (- 1.0 alpha) prev)))))
    nil
    rate-samples))

(defn ewma-projection
  "Slow EWMA point estimate. Band is fast/slow spread (heuristic, not a
  real CI). Returns nil if fewer than 2 usable rate samples."
  [observed {:keys [now resets-at last-pct]}]
  (let [rs (rate-samples observed)]
    (when (>= (count rs) 2)
      (let [slow (ewma-fold 6.0 rs)
            fast (ewma-fold 1.0 rs)
            dt-hr (max 0.0 (/ (- resets-at now) 3600.0))
            proj (max 0.0 (+ last-pct (* slow dt-hr)))
            band-lo (max 0.0 (+ last-pct (* (min slow fast) dt-hr)))
            band-hi (max 0.0 (+ last-pct (* (max slow fast) dt-hr)))]
        {:method  :ewma
         :name    "EWMA (slow τ=6h)"
         :rate    slow
         :proj    proj
         :band    {:lo band-lo :hi band-hi}}))))

;; --- 2. OLS linear regression on (t, pct) ---

(defn- ols-fit
  "OLS fit of y = a + b*x. Returns coefficients + dispersion stats."
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
        resid  (map (fn [x y] (- y (+ a (* b x)))) xs ys)
        sse    (reduce + 0.0 (map #(* % %) resid))
        sigma2 (if (> n 2) (/ sse (- n 2)) 0.0)]
    {:a a :b b :sxx sxx :x-bar x-bar :n n :sigma2 sigma2}))

(defn- ols-prediction
  "Point estimate + 90% prediction-interval half-width at x_new."
  [{:keys [a b sxx x-bar n sigma2]} x-new]
  (let [y-pred (+ a (* b x-new))
        se     (when (and (> n 2) (pos? sigma2) (pos? sxx))
                 (Math/sqrt
                   (* sigma2
                      (+ 1.0
                         (/ 1.0 n)
                         (/ (Math/pow (- x-new x-bar) 2) sxx)))))]
    {:pred y-pred
     :half-width (or (some-> se (* z-90)) 0.0)}))

(defn ols-projection
  "OLS line through (ts→hours-from-window-start, pct). Forward-projects
  to resets_at with a Gaussian 90% prediction interval. Floors at the
  current pct (a downward fit shouldn't claim usage will go down)."
  [observed {:keys [now resets-at window-start last-pct]}]
  (when (>= (count observed) 3)
    (let [;; convert to hours-since-window-start to keep numbers small
          to-x (fn [ts] (/ (- ts window-start) 3600.0))
          pts  (mapv (fn [{:keys [ts pct]}] [(to-x ts) pct]) observed)
          fit  (ols-fit pts)
          {:keys [pred half-width]} (ols-prediction fit (to-x resets-at))
          proj (max last-pct pred)
          lo   (max last-pct (- pred half-width))
          hi   (max last-pct (+ pred half-width))
          dt-hr (max 1e-9 (/ (- resets-at now) 3600.0))]
      {:method :ols
       :name   "OLS linear"
       :rate   (max 0.0 (/ (- proj last-pct) dt-hr))
       :proj   proj
       :band   {:lo lo :hi hi}})))

;; --- 3. Bayesian conjugate Gaussian on the rate ---

(defn- bayes-rate-posterior
  "Conjugate update for r ~ N(mu0, sigma0^2) given rate observations
  with empirical variance. Returns posterior {:mu :sigma2}."
  [rates mu0 sigma0]
  (let [n (count rates)]
    (if (zero? n)
      {:mu mu0 :sigma2 (* sigma0 sigma0)}
      (let [r-mean      (/ (reduce + 0.0 rates) n)
            r-var       (if (> n 1)
                          (/ (reduce + 0.0
                                     (map #(let [d (- % r-mean)] (* d d)) rates))
                             (dec n))
                          (* sigma0 sigma0))
            sigma-obs2  (max r-var 1e-6)
            tau-0       (/ 1.0 (* sigma0 sigma0))
            tau-obs     (/ n sigma-obs2)
            tau-n       (+ tau-0 tau-obs)
            mu-n        (/ (+ (* tau-0 mu0) (* tau-obs r-mean)) tau-n)
            sigma2-n    (/ 1.0 tau-n)]
        {:mu mu-n :sigma2 sigma2-n}))))

(defn bayes-projection
  "Bayesian conjugate Gaussian on rate. Prior centered at the linear
  weekly target (0.595 %/hr), with σ₀ at the same scale (loose enough
  that ~5 observations dominate). Posterior projects forward to
  resets_at; 90% credible interval comes from Var(r·Δt)=Var(r)·Δt²."
  [observed {:keys [now resets-at last-pct]}]
  (let [rs    (rate-samples observed)
        rates (mapv :rate rs)]
    (when (>= (count rates) 2)
      (let [{:keys [mu sigma2]} (bayes-rate-posterior rates target-pct-per-hr 1.0)
            dt-hr (max 0.0 (/ (- resets-at now) 3600.0))
            proj  (max last-pct (+ last-pct (* mu dt-hr)))
            sd    (Math/sqrt (* sigma2 dt-hr dt-hr))
            lo    (max last-pct (+ last-pct (* mu dt-hr) (* -1.0 z-90 sd)))
            hi    (max last-pct (+ last-pct (* mu dt-hr) (*  1.0 z-90 sd)))]
        {:method :bayes
         :name   "Bayesian (conj. Gaussian on rate)"
         :rate   mu
         :proj   proj
         :band   {:lo lo :hi hi}}))))

;; --- 4. Trailing-window mean rate ---

(defn trailing-rate-projection
  "Mean rate over the last `window-hours`. Crude baseline — no band.
  Returns nil if there aren't two samples within the window."
  [observed {:keys [now resets-at last-pct]} window-hours]
  (let [cutoff (- now (* window-hours 3600))
        recent (filterv #(>= (:ts %) cutoff) observed)]
    (when (>= (count recent) 2)
      (let [a (first recent)
            b (last recent)
            dt-hr (/ (- (:ts b) (:ts a)) 3600.0)]
        (when (pos? dt-hr)
          (let [r        (max 0.0 (/ (- (:pct b) (:pct a)) dt-hr))
                forward  (max 0.0 (/ (- resets-at now) 3600.0))
                proj     (max last-pct (+ last-pct (* r forward)))]
            {:method (keyword (str "trailing-" window-hours "h"))
             :name   (format "Trailing %dh rate" window-hours)
             :rate   r
             :proj   proj
             :band   nil}))))))

;; --- aggregation ---

(defn all-projections
  "Compute every projection method on the same data. Returns a vector
  of method maps (in display order), filtering out methods that lacked
  enough data."
  [observed window-info]
  (->> [(ewma-projection observed window-info)
        (ols-projection observed window-info)
        (bayes-projection observed window-info)
        (trailing-rate-projection observed window-info 6)
        (trailing-rate-projection observed window-info 24)]
       (filterv some?)))
