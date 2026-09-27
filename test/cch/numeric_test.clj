(ns cch.numeric-test
  (:require [cch.numeric :as s]
            [clojure.test :refer [deftest is testing]]))

(defn- close? [a b tol] (< (Math/abs (- (double a) (double b))) tol))

(deftest log-gamma-matches-reference
  ;; Reference values from scipy.special.gammaln.
  (is (close? (s/log-gamma 0.5) 0.5723649429247 1e-10))
  (is (close? (s/log-gamma 10.3) 13.482036786138359 1e-10))
  (is (close? (s/log-gamma 1e-3) 6.907178885383853 1e-9)))

(deftest beta-inc-matches-reference
  ;; Reference values from scipy.special.betainc, including the tiny-shape
  ;; regime the usage model hits (kappa * mass << 1).
  (doseq [[a b x expected] [[0.5 0.5 0.3 0.36901011956554536]
                            [2.0 3.0 0.4 0.5248]
                            [0.04 16.0 0.2 0.9996850542908551]
                            [50.0 20.0 0.7 0.3825092483812333]
                            [3.2 4.4 0.9 0.9993048294675259]]]
    (is (close? (s/beta-inc a b x) expected 1e-10) (str [a b x])))
  (testing "boundaries"
    (is (= 0.0 (s/beta-inc 2.0 3.0 0.0)))
    (is (= 1.0 (s/beta-inc 2.0 3.0 1.0)))))

(deftest samplers-match-moments
  (let [r (s/rng 42)
        n 40000
        mean (fn [xs] (/ (reduce + 0.0 xs) (count xs)))]
    (testing "Gamma(shape, rate) mean = shape/rate, above and below shape 1"
      (is (close? (mean (repeatedly n #(s/gamma-sample r 3.0 2.0))) 1.5 0.03))
      (is (close? (mean (repeatedly n #(s/gamma-sample r 0.3 1.0))) 0.3 0.02)))
    (testing "Beta(a, b) mean = a/(a+b)"
      (is (close? (mean (repeatedly n #(s/beta-sample r 2.0 6.0))) 0.25 0.01)))))

(deftest seeded-rng-is-deterministic
  (is (= (vec (repeatedly 5 (let [r (s/rng 7)] #(s/gamma-sample r 2.0 1.0))))
         (vec (repeatedly 5 (let [r (s/rng 7)] #(s/gamma-sample r 2.0 1.0)))))))

(deftest quantile-interpolates
  (let [xs (double-array [0 10 20 30 40])]
    (is (= 20.0 (s/quantile xs 0.5)))
    (is (= 5.0 (s/quantile xs 0.125)))))

(deftest nelder-mead-finds-rosenbrock-minimum
  (let [rosen (fn [[x y]] (+ (Math/pow (- 1 x) 2) (* 100 (Math/pow (- y (* x x)) 2))))
        {:keys [x fx]} (s/nelder-mead rosen [-1.2 1.0] :tol 1e-12 :max-iter 5000)]
    (is (< fx 1e-8))
    (is (close? (first x) 1.0 1e-3))
    (is (close? (second x) 1.0 1e-3))))

(deftest solve-small-systems
  (let [x (s/solve [[2.0 1.0 0.0] [1.0 3.0 1.0] [0.0 1.0 4.0]] [3.0 5.0 5.0])]
    (is (every? true? (map #(< (Math/abs (- %1 %2)) 1e-12) x [1.0 1.0 1.0]))))
  (testing "needs pivoting"
    (is (= [2.0 1.0] (mapv #(Math/rint %) (s/solve [[0.0 1.0] [1.0 0.0]] [1.0 2.0]))))))

(deftest gamma-inc-matches-reference
  ;; scipy.special.gammainc
  (doseq [[a x expected] [[0.5 0.3 0.5614219739190003] [2.0 1.5 0.4421745996289252]
                          [0.02 0.01 0.9221194412397666] [0.3 5.0 0.9993486812492816]
                          [10.0 12.0 0.7576078383294875] [50.0 40.0 0.07033506665939494]
                          [0.1 0.0001 0.41846137523796295]]]
    (is (< (Math/abs (- (s/gamma-inc a x) expected)) 1e-10) (str [a x])))
  (is (= 0.0 (s/gamma-inc 2.0 0.0))))

(deftest hessian-cholesky-inverse
  (let [f (fn [[x y]] (+ (* 2 x x) (* 3 x y) (* 5 y y)))
        h (s/hessian f [0.3 -0.2])]
    (is (every? true? (map #(< (Math/abs (- %1 %2)) 1e-6) (flatten h) [4.0 3.0 3.0 10.0]))))
  (let [a [[4.0 2.0] [2.0 3.0]]
        l (s/cholesky a)]
    (is (< (Math/abs (- 2.0 (get-in l [0 0]))) 1e-12))
    (is (< (Math/abs (- (Math/sqrt 2.0) (get-in l [1 1]))) 1e-12))
    (is (nil? (s/cholesky [[1.0 2.0] [2.0 1.0]])) "indefinite")
    (is (every? true? (map #(< (Math/abs (- %1 %2)) 1e-12)
                           (flatten (s/inverse a)) [0.375 -0.25 -0.25 0.5])))))

(deftest poisson-sample-moments
  (let [r (s/rng 1)]
    (is (every? zero? (repeatedly 100 #(s/poisson-sample r 0.0))) "zero mean")
    (is (every? #(>= % 0) (repeatedly 1000 #(s/poisson-sample r 1e-20))) "never negative"))
  (doseq [mu [0.3 4.0 60.0]]
    (let [r (s/rng 3)
          xs (vec (repeatedly 20000 #(s/poisson-sample r mu)))
          m (/ (reduce + xs) 20000.0)
          v (/ (reduce + (map #(Math/pow (- % m) 2) xs)) 20000.0)]
      (is (< (Math/abs (- m mu)) (* 4 (Math/sqrt (/ mu 20000.0)))) (str "mean " mu))
      (is (< (Math/abs (- 1.0 (/ v mu))) 0.05) (str "variance " mu)))))

(deftest poisson-sample-large-means-are-fast-and-right
  (let [r (s/rng 9)
        xs (vec (repeatedly 20000 #(s/poisson-sample r 1e6)))
        m (/ (reduce + xs) 20000.0)]
    (is (< (Math/abs (- 1.0 (/ m 1e6))) 1e-3))))
