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
      (let [[lz mean var] (@#'ct/posterior-moments y A kappa m v)
            z (Math/exp lz)
            [bz bmean bvar] (brute-moments y A kappa m v)]
        (is (< (Math/abs (- 1.0 (/ z bz))) 0.01) (str "evidence " [y A kappa m v]))
        (is (< (Math/abs (- mean bmean)) (* 0.02 (Math/sqrt v))) (str "mean " [y A kappa m v]))
        (is (< (Math/abs (- 1.0 (/ var bvar))) 0.05) (str "variance " [y A kappa m v]))))))

(deftest on-off-mix-is-stable
  (testing "usage means on, whatever the evidence"
    (is (= 1.0 (first (@#'ct/on-off-mix 0.3 -2000.0 5.0))))
    (is (Double/isFinite (second (@#'ct/on-off-mix 0.3 -2000.0 5.0)))))
  (testing "a zero hour mixes the branches"
    (let [[r lp] (@#'ct/on-off-mix 0.5 (Math/log 0.2) 0.0)]
      (is (< (Math/abs (- r (/ 0.1 0.6))) 1e-12))
      (is (< (Math/abs (- lp (Math/log 0.6))) 1e-12)))))

(deftest filter-survives-an-implausible-hour
  (testing "a large usage hour after a long quiet spell, under a sharp fitted
  state, leaves the state and log likelihood finite (0/0 regression)"
    (let [params {:kappa 6.36 :mu 1.25 :sd-fast 0.74 :sd-slow 0.53 :h-fast 0.55 :h-slow 10.2
                  :q-on 0.93 :q-off 0.22}
          stps (vec (concat (for [h (range 48)] [(* h 3600) 1.0 (if (even? h) 3.0 4.0)])
                            [[(* 48 3600) 0.02 95.0]]
                            (for [h (range 49 60)] [(* h 3600) 1.0 3.0])))
          {:keys [m P p-on loglik]} (ct/run-filter-c stps params true)]
      (is (every? #(Double/isFinite %) (concat m (flatten P) [p-on loglik]))))))

(deftest state-variance-stays-bounded-on-uninformative-hours
  (testing "an hour with usage but near-zero profile mass has a nearly flat
  likelihood in the log rate: the state variance must not exceed its
  stationary value (covariance-inflation regression)"
    (let [p (ct/unpack-c ct/x0-c)
          bound (+ (Math/pow (:sd-fast p) 2) (Math/pow (:sd-slow p) 2))
          stps (vec (for [h (range 400)]
                      (cond (= h 300) [(* h 3600) 1.29e-7 1.0]
                            (< (mod h 24) 8) [(* h 3600) 2.4e-7 0.0]
                            :else [(* h 3600) 1.0 (if (odd? h) 3.0 0.0)])))
          {[[pff _] [_ pss]] :P :keys [loglik]} (ct/run-filter-c stps p true)]
      (is (Double/isFinite loglik))
      (is (<= (+ pff pss) (* 1.01 bound))))))

(deftest laplace-on-a-near-flat-likelihood
  (testing "usage in an hour with profile mass ~1e-7: the likelihood is a
  plateau in the log rate, so the posterior is essentially the prior (a real
  hour that once drove the state variance to 5e8)"
    (let [m 1.3117 v 0.3742
          [e-hat v-hat] (@#'ct/laplace-eta 1.0 1.29e-7 0.3 m v)
          [_ mean var] (@#'ct/posterior-moments 1.0 1.29e-7 0.3 m v)]
      (is (< (Math/abs (- e-hat m)) 0.1))
      (is (< 0.9 (/ v-hat v) 1.0001))
      (is (< (Math/abs (- mean m)) 0.1))
      (is (< 0.9 (/ var v) 1.0001)))))
