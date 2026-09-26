(ns cch.usage-ct-test
  (:require [cch.usage-ct :as ct]
            [clojure.test :refer [deftest is testing]]))

(defn- brute-moments
  "Evidence, mean, and variance of P(y | eta) N(eta; m, v) on a fine grid."
  [y A kappa m v]
  (let [obs @#'ct/obs-prob
        de 1e-3
        es (range (- m (* 12 (Math/sqrt v))) (+ m (* 12 (Math/sqrt v))) de)
        ts (map (fn [e] (* de (obs y A kappa e)
                           (/ (Math/exp (- (/ (Math/pow (- e m) 2) (* 2.0 v))))
                              (Math/sqrt (* 2.0 Math/PI v)))))
                es)
        z (reduce + ts)
        mean (/ (reduce + (map * es ts)) z)]
    [z mean (/ (reduce + (map (fn [e t] (* t (Math/pow (- e mean) 2))) es ts)) z)]))

(deftest posterior-moments-match-quadrature
  (testing "evidence, mean, and variance of the log-rate posterior, including
  small gamma shapes where the posterior is skewed and its mode is not its mean"
    (doseq [[y A kappa m v] [[0.0 1.0 0.3 1.6 0.8] [1.0 0.5 0.3 1.6 0.8] [7.0 1.0 0.3 1.6 0.8]
                             [30.0 3.0 0.3 1.0 0.5] [2.0 1.0 2.0 2.0 0.05]]]
      (let [[z mean var] (@#'ct/posterior-moments y A kappa m v)
            [bz bmean bvar] (brute-moments y A kappa m v)]
        (is (< (Math/abs (- 1.0 (/ z bz))) 0.01) (str "evidence " [y A kappa m v]))
        (is (< (Math/abs (- mean bmean)) (* 0.02 (Math/sqrt v))) (str "mean " [y A kappa m v]))
        (is (< (Math/abs (- 1.0 (/ var bvar))) 0.05) (str "variance " [y A kappa m v]))))))
