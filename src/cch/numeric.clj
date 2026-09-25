(ns cch.numeric
  "Small numerical toolkit for the usage model: special functions, seeded
  samplers, sample quantiles, and a derivative-free minimizer.

  Dependency-free on purpose — cch ships as a jlink image, and the model
  needs only a handful of well-known routines.")

;; --- special functions ---

(def ^:private lanczos-coefs
  [0.99999999999980993 676.5203681218851 -1259.1392167224028
   771.32342877765313 -176.61502916214059 12.507343278686905
   -0.13857109526572012 9.9843695780195716e-6 1.5056327351493116e-7])

(defn log-gamma
  "ln Γ(x) for x > 0 (Lanczos, g=7; reflection below 0.5)."
  ^double [^double x]
  (if (< x 0.5)
    (- (Math/log (/ Math/PI (Math/abs (Math/sin (* Math/PI x)))))
       (log-gamma (- 1.0 x)))
    (let [x (- x 1.0)
          t (+ x 7.5)
          s (reduce-kv (fn [acc i c] (if (zero? i) acc (+ acc (/ c (+ x i)))))
                       (double (first lanczos-coefs))
                       lanczos-coefs)]
      (+ (* 0.5 (Math/log (* 2.0 Math/PI)))
         (* (+ x 0.5) (Math/log t))
         (- t)
         (Math/log s)))))

(defn- beta-cf
  "Continued fraction for the incomplete beta (modified Lentz)."
  ^double [^double a ^double b ^double x]
  (let [tiny 1e-300
        qab (+ a b) qap (+ a 1.0) qam (- a 1.0)
        d0 (- 1.0 (/ (* qab x) qap))
        d0 (/ 1.0 (if (< (Math/abs d0) tiny) tiny d0))]
    (loop [m 1 c 1.0 d d0 h d0]
      (if (> m 500)
        h
        (let [m2 (* 2 m)
              aa (/ (* m (- b m) x) (* (+ qam m2) (+ a m2)))
              d (+ 1.0 (* aa d)) d (/ 1.0 (if (< (Math/abs d) tiny) tiny d))
              c (+ 1.0 (/ aa c)) c (if (< (Math/abs c) tiny) tiny c)
              h (* h d c)
              aa (/ (- (* (+ a m) (+ qab m) x)) (* (+ a m2) (+ qap m2)))
              d (+ 1.0 (* aa d)) d (/ 1.0 (if (< (Math/abs d) tiny) tiny d))
              c (+ 1.0 (/ aa c)) c (if (< (Math/abs c) tiny) tiny c)
              del (* d c)
              h (* h del)]
          (if (< (Math/abs (- del 1.0)) 1e-15)
            h
            (recur (inc m) c d h)))))))

(defn beta-inc
  "Regularized incomplete beta I_x(a, b) for a, b > 0 and x in [0, 1]."
  ^double [^double a ^double b ^double x]
  (cond
    (<= x 0.0) 0.0
    (>= x 1.0) 1.0
    :else
    (let [ln-bt (+ (- (log-gamma (+ a b)) (log-gamma a) (log-gamma b))
                   (* a (Math/log x))
                   (* b (Math/log (- 1.0 x))))
          bt (Math/exp ln-bt)]
      (if (< x (/ (+ a 1.0) (+ a b 2.0)))
        (/ (* bt (beta-cf a b x)) a)
        (- 1.0 (/ (* bt (beta-cf b a (- 1.0 x))) b))))))

;; --- seeded sampling ---

(defn rng
  "Deterministic generator, so a forecast recomputed from unchanged data
  renders the same numbers instead of flickering by Monte Carlo noise."
  ^java.util.SplittableRandom [seed]
  (java.util.SplittableRandom. (long seed)))

(defn gamma-sample
  "One draw from Gamma(shape, rate) (Marsaglia–Tsang; boosted below shape 1)."
  ^double [^java.util.SplittableRandom r ^double shape ^double rate]
  (if (< shape 1.0)
    (let [u (.nextDouble r)]
      (* (gamma-sample r (+ shape 1.0) rate) (Math/pow u (/ 1.0 shape))))
    (let [d (- shape (/ 1.0 3.0))
          c (/ 1.0 (Math/sqrt (* 9.0 d)))]
      (loop []
        (let [z (.nextGaussian r)
              v (Math/pow (+ 1.0 (* c z)) 3)]
          (if (<= v 0.0)
            (recur)
            (let [u (.nextDouble r)]
              (if (< (Math/log u) (+ (* 0.5 z z) (* d (- 1.0 v (- (Math/log v))))))
                (/ (* d v) rate)
                (recur)))))))))

(defn beta-sample
  "One draw from Beta(a, b)."
  ^double [^java.util.SplittableRandom r ^double a ^double b]
  (let [x (gamma-sample r a 1.0)
        y (gamma-sample r b 1.0)
        s (+ x y)]
    (if (pos? s) (/ x s) 0.5)))

(defn quantile
  "Sample quantile of a sorted double array (linear interpolation)."
  ^double [^doubles sorted ^double q]
  (let [n (alength sorted)
        pos (* q (dec n))
        lo (int (Math/floor pos))
        hi (min (dec n) (inc lo))
        frac (- pos lo)]
    (+ (* (- 1.0 frac) (aget sorted lo)) (* frac (aget sorted hi)))))

;; --- minimization ---

(defn nelder-mead
  "Minimize `f` over R^n from `x0` (a vector of doubles). Derivative-free,
  which suits likelihoods built from incomplete-beta interval
  probabilities. Returns {:x best-vector :fx best-value :iterations n}."
  [f x0 & {:keys [step max-iter tol] :or {step 0.5 max-iter 2000 tol 1e-7}}]
  (let [n (count x0)
        x0 (mapv double x0)
        add (fn [a b] (mapv + a b))
        sub (fn [a b] (mapv - a b))
        scale (fn [k a] (mapv #(* k %) a))
        evaluate (fn [x] [(f x) x])
        init (cons x0 (for [i (range n)] (update x0 i + step)))]
    (loop [simplex (vec (sort-by first (map evaluate init)))
           iter 0]
      (let [[fb xb] (first simplex)
            [fw xw] (peek simplex)
            spread (- fw fb)]
        (if (or (>= iter max-iter) (< (Math/abs spread) tol))
          {:x xb :fx fb :iterations iter}
          (let [others (pop simplex)
                centroid (scale (/ 1.0 n) (reduce add (map second others)))
                [fsw _] (peek others)
                reflect (evaluate (add centroid (sub centroid xw)))
                replace-worst (fn [p] (vec (sort-by first (conj others p))))]
            (recur
              (cond
                (< (first reflect) fb)
                (let [expand (evaluate (add centroid (scale 2.0 (sub centroid xw))))]
                  (replace-worst (if (< (first expand) (first reflect)) expand reflect)))

                (< (first reflect) fsw)
                (replace-worst reflect)

                :else
                (let [contract (evaluate (add centroid (scale 0.5 (sub xw centroid))))]
                  (if (< (first contract) fw)
                    (replace-worst contract)
                    (vec (sort-by first
                                  (cons (first simplex)
                                        (for [[_ x] (rest simplex)]
                                          (evaluate (add xb (scale 0.5 (sub x xb)))))))))))
              (inc iter))))))))
