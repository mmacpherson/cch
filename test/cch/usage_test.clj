(ns cch.usage-test
  (:require [clojure.test :refer [deftest is testing]]
            [cch.usage :as u]))

(defn- make-data
  "Synthetic data bundle for chart tests — simulates a 7d window
   that's halfway through, with observed samples and a multi-method
   projections list."
  [& {:keys [observed projections]
      :or {observed [{:ts 0      :pct 0.0}
                     {:ts 86400  :pct 8.0}
                     {:ts 172800 :pct 16.0}
                     {:ts 259200 :pct 24.0}]
           projections [{:method :ewma  :name "EWMA"
                         :rate 0.5 :proj 80.0 :band {:lo 70 :hi 92}}
                        {:method :ols   :name "OLS linear"
                         :rate 0.55 :proj 78.0 :band {:lo 72 :hi 84}}
                        {:method :bayes :name "Bayesian"
                         :rate 0.5 :proj 75.0 :band {:lo 60 :hi 90}}
                        {:method :trailing-24h :name "Trailing 24h"
                         :rate 0.45 :proj 72.0 :band nil}]}}]
  {:observed     observed
   :resets-at    (* 7 86400)
   :window-start 0
   :now          (* 3 86400)
   :last-pct     (:pct (last observed))
   :samples      (count observed)
   :projections  projections})

(deftest chart-svg-empty-data
  (testing "no data → human-readable fallback, not an empty <svg>"
    (let [out (u/chart-svg nil)]
      (is (re-find #"^:p" (str (first out))) "tag is :p (with optional class)")
      (is (re-find #"Not enough" (str out))))
    (let [out (u/chart-svg (assoc (make-data) :observed []))]
      (is (re-find #"^:p" (str (first out)))))))

(deftest chart-svg-renders-svg
  (testing "with data, returns a [:svg ...] tree"
    (let [out (u/chart-svg (make-data))]
      (is (= :svg (first out)))
      (is (re-find #"viewBox" (str out))))))

(deftest chart-svg-includes-key-elements
  (testing "renders gridlines, projection band, observed polyline, and 'now' guide"
    (let [s (str (u/chart-svg (make-data)))]
      (is (re-find #"polyline"          s) "polyline elements present")
      (is (re-find #"\bpath\b"          s) "Bayesian band path")
      (is (re-find #"stroke-dasharray"  s) "dashed projection lines")
      (is (re-find #"now"               s) "now label")
      (is (re-find #"100%"              s) "100% reference label"))))

(deftest chart-svg-renders-line-per-method
  (testing "one projection polyline per method, each with its own class"
    (let [s (str (u/chart-svg (make-data)))]
      (is (re-find #"proj-ewma"         s))
      (is (re-find #"proj-ols"          s))
      (is (re-find #"proj-bayes"        s))
      (is (re-find #"proj-trailing-24h" s)))))

(deftest chart-svg-bands-per-method
  (testing "every projection with a band draws a path tagged with data-method"
    (let [s (str (u/chart-svg (make-data)))]
      (is (re-find #"band-region" s))
      ;; Clojure-printed form of [:path {:data-method "ewma"}] etc.
      (is (re-find #":data-method \"ewma\""  s))
      (is (re-find #":data-method \"ols\""   s))
      (is (re-find #":data-method \"bayes\"" s)))))

(deftest chart-svg-coords-are-numeric-not-ratios
  (testing "polyline points must not contain Clojure ratios"
    (let [s (str (u/chart-svg (make-data)))]
      (doseq [[_ body] (re-seq #"points=\"([^\"]+)\"" s)]
        (is (not (re-find #"/" body))
            (str "ratio leaked into points=\"" body "\""))))))

(deftest projection-above-100-is-clamped-to-y-top
  (testing "a runaway projection still fits inside viewBox"
    (let [data (assoc (make-data)
                      :projections
                      [{:method :ewma :name "EWMA" :rate 5.0
                        :proj 350.0 :band {:lo 300 :hi 400}}])
          out  (str (u/chart-svg data))
          ys   (->> (re-seq #"y[12]?=\"(-?\d+\.?\d*)\"" out)
                    (map (comp #(Double/parseDouble %) second)))]
      (is (every? #(<= -1.0 % 281.0) ys)))))

(deftest summary-stats-renders-each-method
  (testing "right-rail stats list every method's projected % and band when available"
    (let [out (str (u/summary-stats (make-data)))]
      (is (re-find #"24%" out)  "current pct")
      (is (re-find #"EWMA"      out))
      (is (re-find #"OLS"       out))
      (is (re-find #"Bayesian"  out))
      (is (re-find #"Trailing"  out))
      (is (re-find #"60.*90"    out) "Bayes band rendered as range")
      (is (re-find #"80%"       out) "EWMA proj")
      (is (re-find #"75%"       out) "Bayes proj"))))

(deftest summary-stats-rows-have-data-method
  (testing "each method-row carries a data-method attribute so hover triggers band reveal"
    (let [out (str (u/summary-stats (make-data)))]
      (is (re-find #":data-method \"ewma\""  out))
      (is (re-find #":data-method \"ols\""   out))
      (is (re-find #":data-method \"bayes\"" out)))))

(deftest summary-stats-no-band-skips-band-text
  (testing "a method without a band shows just the point estimate"
    (let [data (make-data :projections [{:method :trailing-24h
                                         :name "Trailing 24h"
                                         :rate 0.45 :proj 72.0 :band nil}])
          out  (str (u/summary-stats data))]
      (is (re-find #"72%" out))
      (is (not (re-find #"band" out))))))

(deftest legend-lists-each-method
  (let [out (str (u/legend (make-data)))]
    (is (re-find #"EWMA"     out))
    (is (re-find #"OLS"      out))
    (is (re-find #"Bayesian" out))
    (is (re-find #"observed" out))
    (is (re-find #"100%"     out))))

(deftest page-body-no-data
  (testing "no-data path renders the chart fallback but no stats panel"
    (let [out (str (u/page-body nil))]
      (is (re-find #"Not enough" out))
      (is (not (re-find #"usage-stats" out))))))

(deftest page-body-with-data
  (testing "data path renders chart + stats grid"
    (let [out (str (u/page-body (make-data)))]
      (is (re-find #"usage-grid"  out))
      (is (re-find #"usage-stats" out))
      (is (re-find #"svg"         out)))))
