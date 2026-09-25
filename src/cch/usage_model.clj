(ns cch.usage-model
  "Zero-inflated dynamic gamma model of agent quota usage.

  Usage is a process in absolute (local clock) time; rate-limit windows are
  only accounting on top of it. The model has three parts:

  * Activity profile a(h): the relative usage rate for each of the 168
    local hours of the week, estimated from history and smoothed.
  * Latent intensity: usage over a block with profile mass A is
    Gamma(kappa*A, lambda), with lambda ~ Gamma(alpha, beta). Blocks are
    24h for the 7d window and 1h for the 5h window.
  * On/off: each block is 'on' with probability pi ~ Beta(a, b), else its
    usage is exactly zero. Many days have no usage at all, which one gamma
    cannot represent alongside heavy days.

  Between blocks both states relax toward their priors with discount d,
  so intensity carries across resets but adapts within days. Each block
  updates the states conjugately, weighted by the posterior probability
  that it was 'on'. Percentages are quantized to 1%, so block likelihoods
  are interval probabilities of the compound-gamma predictive
  y/(y+beta) ~ Beta(kappa*A, alpha).

  Parameters theta = [kappa alpha0 beta0 a0 b0 d] are fit by maximizing
  the one-step-ahead predictive likelihood (Nelder–Mead). The forecast is
  a seeded Monte Carlo simulation of the demand path to the scheduled
  reset, which can exceed 100% (the cap stops the meter, not the demand).
  The path is simulated in sub-block pieces; a gamma process splits
  exactly, so the pieces of one block sum to the block's distribution.

  Everything here is pure; cch.forecast owns the SQL and caching."
  (:require [cch.numeric :as num])
  (:import (java.time Instant ZoneId ZonedDateTime)))

(def ^:const hour-secs 3600)

(def specs
  "Per-window model settings. `x0` is the unconstrained starting point for
  the fit: [ln kappa, ln alpha0, ln beta0, ln on-strength, logit on-mean,
  logit d]."
  {:seven-day {:span-secs (* 7 86400) :block-hours 24 :anchor-hour 4
               :min-mass 2.0 :cluster-secs 300 :fit-lookback-secs nil
               :filter-lookback-secs nil :path-step-secs 3600
               :x0 [-3.0 2.3 4.1 1.4 0.85 1.4]}
   :five-hour {:span-secs (* 5 3600) :block-hours 1 :anchor-hour 0
               :min-mass 0.05 :cluster-secs 120
               :fit-lookback-secs (* 42 86400)
               ;; With d ~ 0.9 per hour, state older than a week has no weight.
               :filter-lookback-secs (* 7 86400) :path-step-secs 300
               :x0 [-1.2 1.6 0.7 1.4 -0.85 2.2]}})

;; --- clock ---

(defn hour-of-week
  "Local hour-of-week index, 0..167 with Monday 00:00 = 0."
  ^long [^ZoneId zone ^long ts]
  (let [z (ZonedDateTime/ofInstant (Instant/ofEpochSecond ts) zone)]
    (+ (* 24 (dec (.getValue (.getDayOfWeek z)))) (.getHour z))))

(defn- floor-hour ^long [^long ts] (* hour-secs (quot ts hour-secs)))

;; --- windows ---

(defn- canonical-resets
  "Map each resets_at to its cluster's first value. Providers jitter a
  window's reset time by seconds between reports."
  [resets cluster-secs]
  (loop [[r & more] (sort (distinct resets)) prev nil canon nil m {}]
    (if (nil? r)
      m
      (let [c (if (and prev (<= (- r prev) cluster-secs)) canon r)]
        (recur more r c (assoc m r c))))))

(defn windows
  "Group hourly aggregate rows [{:resets-at :hour :pct}] (pct = max observed
  in that clock hour) into windows, oldest first:

    {:start :end :eff-end :cap :final :cum [[hour cum-pct] ...]}

  `:cum` is the running maximum, which drops stale sessions still reporting
  lower values. `:eff-end` is the earlier of the scheduled reset and the
  next window's first activity (early and granted resets). `:cap` is the
  end of the first hour that reached 100%, after which demand is hidden."
  [rows {:keys [span-secs cluster-secs]}]
  (let [canon (canonical-resets (map :resets-at rows) cluster-secs)
        grouped (->> rows
                     (keep (fn [{:keys [resets-at hour pct]}]
                             (let [c (canon resets-at)]
                               (when (and (< (- c span-secs) (+ hour hour-secs))
                                          (< hour c))
                                 [c hour (double pct)]))))
                     (group-by first))
        base (->> grouped
                  (keep (fn [[c xs]]
                          (let [by-hour (reduce (fn [m [_ h p]] (update m h (fnil max 0.0) p))
                                                (sorted-map) xs)
                                cum (second
                                      (reduce (fn [[mx out] [h p]]
                                                (let [mx (max mx p)] [mx (conj out [h mx])]))
                                              [0.0 []] by-hour))
                                final (second (peek cum))]
                            (when (>= final 1.0)
                              {:start (- c span-secs) :end c :cum cum :final final
                               :first-hour (ffirst cum)
                               :cap (some (fn [[h p]] (when (>= p 100.0) (+ h hour-secs))) cum)}))))
                  (sort-by :end)
                  vec)]
    (vec (map-indexed
           (fn [i w]
             (let [later (keep #(when (> (:first-hour %) (:start w)) (:first-hour %))
                               (subvec base (inc i)))]
               (-> w
                   (assoc :eff-end (reduce min (:end w) later))
                   (dissoc :first-hour))))
           base))))

(defn live-end
  "End of the span in which the window's meter reflects demand."
  [{:keys [eff-end cap]}]
  (if cap (min cap eff-end) eff-end))

(defn cum-at
  "Window pct as of the end of the last hour starting before `t`."
  [{:keys [cum]} t]
  (reduce (fn [v [h p]] (if (< h t) p (reduced v))) 0.0 cum))

;; --- hour series ---

(defn hour-series
  "Sorted map hour -> [live-fraction usage] over absolute time up to `t-end`.
  Hours inside a window carry its increments; hours between windows are
  live with zero usage (windows start at first use, or are contiguous);
  hours after a cap are excluded. The hour containing `t-end` is live for
  its elapsed fraction."
  [wins t-end]
  (if (empty? wins)
    (sorted-map)
    (let [first-h (floor-hour (reduce min (map :start wins)))
          init (into (sorted-map)
                     (for [h (range first-h t-end hour-secs)]
                       [h [(min 1.0 (/ (double (- t-end h)) hour-secs)) 0.0]]))
          with-usage (reduce
                       (fn [m w]
                         (second
                           (reduce (fn [[prev m] [h p]]
                                     [p (if (and (< h t-end) (contains? m h))
                                          (update-in m [h 1] + (max 0.0 (- p prev)))
                                          m)])
                                   [0.0 m] (:cum w))))
                       init wins)]
      (reduce (fn [m {:keys [cap eff-end]}]
                (if cap
                  (reduce (fn [m h] (if (contains? m h) (assoc-in m [h 0] 0.0) m))
                          m (range cap (min eff-end t-end) hour-secs))
                  m))
              with-usage wins))))

;; --- activity profile ---

(defn bin-stats
  "Per hour-of-week bin totals from an hour series:
  {:usage [168 doubles] :exposure [168 doubles]}, exposure in live hours."
  [series ^ZoneId zone]
  (let [usage (double-array 168) expo (double-array 168)]
    (doseq [[h [live y]] series
            :let [k (hour-of-week zone h)]]
      (aset usage k (+ (aget usage k) (double y)))
      (aset expo k (+ (aget expo k) (double live))))
    {:usage (vec usage) :exposure (vec expo)}))

(defn smooth-profile
  "Circularly smooth (Gaussian, sigma 1.5h) a 168-bin rate vector, floor it
  so no hour is impossible, and normalize to mean 1."
  [rate]
  (let [kern (let [ws (mapv #(Math/exp (* -0.5 (Math/pow (/ % 1.5) 2))) (range -6 7))
                   s (reduce + ws)]
               (mapv #(/ % s) ws))
        sm (mapv (fn [k] (reduce + (map-indexed (fn [j w] (* w (nth rate (mod (+ k j -6) 168)))) kern)))
                 (range 168))
        mean (/ (reduce + sm) 168.0)]
    (if (pos? mean)
      (let [floored (mapv #(max % (* 0.02 mean)) sm)
            m2 (/ (reduce + floored) 168.0)]
        (mapv #(/ % m2) floored))
      (vec (repeat 168 1.0)))))

(defn- normalized-usage
  "Bin usage divided by the cell's overall rate, so cells measured in
  different units (5h vs 7d percent, providers) are comparable."
  [{:keys [usage exposure]}]
  (let [rate (/ (reduce + usage) (max 1e-9 (reduce + exposure)))]
    (if (pos? rate) (mapv #(/ % rate) usage) (vec (repeat 168 0.0)))))

(defn pooled-rates
  "Exposure-weighted mean normalized rate per bin across cells' bin-stats:
  a shared shape (mean ~1) to shrink toward."
  [stats]
  (let [num (apply mapv + (map normalized-usage stats))
        den (apply mapv + (map :exposure stats))]
    (mapv (fn [n d] (if (pos? d) (/ n d) 0.0)) num den)))

(defn shrunk-rates
  "Empirical-Bayes shrinkage of one cell's normalized bin rates toward
  `target` (mean ~1), with pseudo-exposure `k` hours per bin: k = 0 is the
  cell alone, large k is the target."
  [stats target k]
  (mapv (fn [u e f] (let [d (+ e k)] (if (pos? d) (/ (+ u (* k f)) d) f)))
        (normalized-usage stats) (:exposure stats) target))

(defn profile
  "Hour-of-week activity profile (length 168, mean 1): usage per live hour
  in each local hour-of-week bin, circularly smoothed (Gaussian, sigma 1.5h)
  and floored so no hour is impossible."
  [series ^ZoneId zone]
  (let [{:keys [usage exposure]} (bin-stats series zone)]
    (smooth-profile (mapv (fn [u e] (if (pos? e) (/ u e) 0.0)) usage exposure))))

(defn mass
  "Profile mass (hour units) over [t0, t1)."
  [prof ^ZoneId zone t0 t1]
  (loop [t t0 acc 0.0]
    (if (>= t t1)
      acc
      (let [nxt (min t1 (+ (floor-hour t) hour-secs))]
        (recur nxt (+ acc (* (nth prof (hour-of-week zone t))
                             (/ (double (- nxt t)) hour-secs))))))))

;; --- blocks ---

(defn- block-start? [zone {:keys [block-hours anchor-hour]} h]
  (zero? (mod (- (mod (hour-of-week zone h) 24) anchor-hour) block-hours)))

(defn blocks
  "Aggregate an hour series into model blocks: [[start mass usage] ...]."
  [series prof zone spec]
  (let [out (reduce (fn [acc [h [live y]]]
                      (let [A (* (nth prof (hour-of-week zone h)) live)]
                        (if (or (empty? acc) (block-start? zone spec h))
                          (conj acc [h A y])
                          (let [[s A0 y0] (peek acc)]
                            (conj (pop acc) [s (+ A0 A) (+ y0 y)])))))
                    [] series)]
    out))

;; --- filter ---

(defn- interval-prob
  "P(y in [y-q, y+q]) under y/(y+beta) ~ Beta(kappa*A, alpha)."
  [y A al be kappa]
  (let [k (* kappa A) q 0.5]
    (if (<= k 1e-9)
      (if (< y q) 1.0 1e-300)
      (let [hi (+ y q) lo (max 0.0 (- y q))]
        (max 1e-300 (- (num/beta-inc k al (/ hi (+ hi be)))
                       (num/beta-inc k al (/ lo (+ lo be)))))))))

(defn run-filter
  "Filter the intensity and on/off states through `blks`. Returns
  {:state [alpha beta a b] :loglik L :on-last? bool} where loglik is the sum
  of one-step-ahead predictive log probabilities (when `score?`)."
  [blks [kappa al0 be0 a0 b0 d] {:keys [min-mass]} score?]
  (let [relax (fn [s p] (+ (* d s) (* (- 1.0 d) p)))]
    (reduce
      (fn [{[al be a b] :state ll :loglik} [_ A y]]
        (let [al (relax al al0) be (relax be be0) a (relax a a0) b (relax b b0)]
          (if (< A min-mass)
            {:state [(+ al (* kappa A)) (+ be y) a b] :loglik ll :on-last? (>= y 0.5)}
            (let [pi (/ a (+ a b))
                  pg (interval-prob y A al be kappa)
                  off (* (- 1.0 pi) (if (< y 0.5) 1.0 0.0))
                  tot (+ (* pi pg) off)
                  r (/ (* pi pg) tot)]
              {:state [(+ al (* r kappa A)) (+ be (* r y)) (+ a r) (+ b (- 1.0 r))]
               :loglik (if score? (+ ll (Math/log (max tot 1e-300))) ll)
               :on-last? (>= y 0.5)}))))
      {:state [al0 be0 a0 b0] :loglik 0.0 :on-last? false}
      blks)))

;; --- fit ---

(defn- sigmoid ^double [^double x] (/ 1.0 (+ 1.0 (Math/exp (- x)))))

(defn unpack
  "Unconstrained vector -> theta [kappa alpha0 beta0 a0 b0 d]."
  [[lk lal lbe ls lp ld]]
  (let [s (Math/exp ls) p (sigmoid lp)]
    [(Math/exp lk) (Math/exp lal) (Math/exp lbe) (* s p) (* s (- 1.0 p)) (sigmoid ld)]))

(defn fit
  "Maximum-likelihood theta for `blks`, starting from unconstrained `x0`
  (the spec default, or yesterday's fit). Nelder–Mead with one restart
  from the best point guards against a stalled simplex."
  [blks spec x0]
  (let [obj (fn [x] (- (:loglik (run-filter blks (unpack x) spec true))))
        r1 (num/nelder-mead obj x0 :tol 1e-6 :max-iter 1500)
        r2 (num/nelder-mead obj (:x r1) :step 0.2 :tol 1e-6 :max-iter 1500)
        best (if (<= (:fx r2) (:fx r1)) r2 r1)]
    {:x (:x best) :theta (unpack (:x best)) :nll (:fx best)}))

;; --- forecast ---

(defn future-blocks
  "Remaining model blocks from `t` to `t-end`: [[mass current-block?] ...]."
  [prof zone spec t t-end]
  (loop [t t first? true out []]
    (if (>= t t-end)
      out
      (let [nxt (loop [h (+ (floor-hour t) hour-secs)]
                  (if (or (>= h t-end) (block-start? zone spec h)) (min h t-end) (recur (+ h hour-secs))))]
        (recur nxt false (conj out [(mass prof zone t nxt) first?]))))))

(defn predict
  "Monte Carlo predictive of demand at the end of `fut` given current window
  pct `x`. Returns {:median :lo :q25 :q75 :hi :p-cap :draws}: a 90% band
  (5th–95th), a 50% band, and the sorted draws."
  [[al be a b] [kappa] x fut on-now? & {:keys [n seed] :or {n 4000 seed 1}}]
  (let [r (num/rng seed)
        draws (double-array n)]
    (dotimes [i n]
      (let [lam (num/gamma-sample r al be)
            pi (num/beta-sample r a b)]
        (aset draws i
              (+ (double x)
                 (reduce (fn [acc [A current?]]
                           (if (and (pos? A) (or (and current? on-now?) (< (.nextDouble r) pi)))
                             (+ acc (num/gamma-sample r (* kappa A) lam))
                             acc))
                         0.0 fut)))))
    (java.util.Arrays/sort draws)
    {:median (num/quantile draws 0.5)
     :lo (num/quantile draws 0.05)
     :q25 (num/quantile draws 0.25)
     :q75 (num/quantile draws 0.75)
     :hi (num/quantile draws 0.95)
     :p-cap (/ (double (count (filter #(>= % 100.0) draws))) n)
     :draws draws}))

(defn crps
  "Continuous ranked probability score of sorted Monte Carlo `draws`
  against observation `y` (lower is better), after capping draws at `cap`.
  The meter stops at the cap, so a capped window is scored on the capped
  variable, which keeps the score proper for censored outcomes."
  [^doubles draws y cap]
  (let [n (alength draws)
        x (double-array (map #(min (double cap) %) draws))
        e-xy (/ (areduce x i acc 0.0 (+ acc (Math/abs (- (aget x i) (double y))))) n)
        ;; E|X - X'| for sorted samples: (2/n^2) * sum (2i - n - 1) x_i, 1-based i
        e-xx (/ (* 2.0 (areduce x i acc 0.0 (+ acc (* (- (* 2.0 (inc i)) n 1.0) (aget x i)))))
                (* (double n) n))]
    (- e-xy (* 0.5 e-xx))))

(defn future-pieces
  "Sub-intervals of [t, t-end) at `step-secs` resolution (a divisor or
  multiple of an hour), never crossing an hour boundary, tagged with the
  model block they belong to (0 = the current block):
  [[piece-end mass block-index] ...]."
  [prof zone spec t t-end step-secs]
  (loop [t t idx 0 out []]
    (if (>= t t-end)
      out
      (let [nxt (min t-end
                     (* step-secs (inc (quot t step-secs)))
                     (+ (floor-hour t) hour-secs))
            idx (if (and (seq out) (zero? (mod t hour-secs)) (block-start? zone spec t))
                  (inc idx)
                  idx)]
        (recur nxt idx (conj out [nxt (mass prof zone t nxt) idx]))))))

(defn predict-path
  "Monte Carlo demand path over `pieces` (see future-pieces) from current
  pct `x`. Each draw samples the intensity and on-probability once, each
  block's on/off once (the current block is on if it has usage), and each
  piece's usage. Returns one summary per piece end:
  {:ts :mean :lo :q25 :median :q75 :hi :p-cap}, where lo/hi bound the 90%
  band and p-cap is P(demand >= 100 by ts)."
  [[al be a b] [kappa] x pieces on-now? & {:keys [n seed] :or {n 3000 seed 1}}]
  (let [k (count pieces)
        r (num/rng seed)
        cols (vec (repeatedly k #(double-array n)))
        masses (double-array (map second pieces))
        blks (long-array (map #(nth % 2) pieces))]
    (dotimes [i n]
      (let [lam (num/gamma-sample r al be)
            pi (num/beta-sample r a b)]
        (loop [j 0 cum (double x) blk -1 on? false]
          (when (< j k)
            (let [b-idx (aget blks j)
                  on? (if (= b-idx blk)
                        on?
                        (or (and (zero? b-idx) on-now?) (< (.nextDouble r) pi)))
                  A (aget masses j)
                  cum (if (and on? (pos? A)) (+ cum (num/gamma-sample r (* kappa A) lam)) cum)]
              (aset ^doubles (nth cols j) i cum)
              (recur (inc j) cum b-idx on?))))))
    (mapv (fn [j]
            (let [^doubles col (nth cols j)
                  mean (/ (areduce col i acc 0.0 (+ acc (aget col i))) n)
                  capped (areduce col i acc 0.0 (if (>= (aget col i) 100.0) (inc acc) acc))]
              (java.util.Arrays/sort col)
              {:ts (first (nth pieces j))
               :mean mean
               :lo (num/quantile col 0.05)
               :q25 (num/quantile col 0.25)
               :median (num/quantile col 0.5)
               :q75 (num/quantile col 0.75)
               :hi (num/quantile col 0.95)
               :p-cap (/ capped n)}))
          (range k))))

(defn forecast
  "Forecast the window ending at `resets-at` from hourly aggregate `rows` of
  all windows up to `now`, given a fitted model {:theta :profile}. `x` is
  the latest observed pct of the current window. Returns
  {:median :lo :hi :p-cap :path}, summarizing demand at the reset; `:path`
  holds the per-step summaries unless `path?` is false (the backtest skips
  it and draws whole blocks, which is equivalent at the endpoint)."
  [{:keys [theta profile]} rows spec zone now resets-at x & {:keys [path?] :or {path? true}}]
  (let [wins (windows rows spec)
        lookback (:filter-lookback-secs spec)
        series (cond->> (hour-series wins now)
                 lookback (into (sorted-map) (filter #(>= (key %) (- now lookback)))))
        {:keys [state on-last?]} (run-filter (blocks series profile zone spec) theta spec false)]
    (if path?
      (let [path (predict-path state theta x
                               (future-pieces profile zone spec now resets-at (:path-step-secs spec))
                               on-last?)]
        (if-let [end (peek path)]
          (assoc (select-keys end [:median :lo :hi :p-cap]) :path path)
          {:median x :lo x :hi x :p-cap (if (>= x 100.0) 1.0 0.0) :path []}))
      (predict state theta x (future-blocks profile zone spec now resets-at) on-last?))))

(defn fit-model
  "Fit the activity profile and theta from hourly aggregate `rows` observed
  before `now`. `x0` warm-starts the optimizer; `profile-override` supplies
  a profile estimated elsewhere (e.g. pooled across agents)."
  [rows spec zone now & {:keys [x0 profile-override]}]
  (let [wins (windows rows spec)
        series (hour-series wins now)
        prof (or profile-override (profile series zone))
        lookback (:fit-lookback-secs spec)
        fit-series (if lookback
                     (into (sorted-map) (filter #(>= (key %) (- now lookback)) series))
                     series)
        blks (blocks fit-series prof zone spec)
        {:keys [x theta nll]} (fit blks spec (or x0 (:x0 spec)))]
    {:theta theta :x x :nll nll :profile prof :fitted-at now :blocks (count blks)}))
