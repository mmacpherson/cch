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
