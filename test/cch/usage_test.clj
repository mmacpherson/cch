(ns cch.usage-test
  (:require [clojure.test :refer [deftest is testing]]
            [cch.usage :as u]))

(defn- make-data
  "Synthetic data bundle for chart tests — simulates a 7d window
   that's halfway through, with observed samples and a Bayesian
   projection."
  [& {:keys [observed projection]
      :or {observed [{:ts 0      :pct 0.0}
                     {:ts 86400  :pct 8.0}
                     {:ts 172800 :pct 16.0}
                     {:ts 259200 :pct 24.0}]
           projection {:method :rate-bayes :name "Rate, Bayesian"
                       :rate 0.5 :proj 75.0 :band {:lo 60 :hi 90}}}}]
  {:observed     observed
   :resets-at    (* 7 86400)
   :window-start 0
   :now          (* 3 86400)
   :last-pct     (:pct (last observed))
   :samples      (count observed)
   :projection   projection})

(deftest chart-svg-empty-data
  (testing "nil data → human-readable fallback"
    (let [out (u/chart-svg nil)]
      (is (re-find #"^:p" (str (first out))) "tag is :p (with optional class)")
      (is (re-find #"Not enough" (str out)))))
  (testing "empty observed → still renders SVG scaffolding"
    (let [out (u/chart-svg (assoc (make-data) :observed [] :projection nil))]
      (is (= :svg (first out))))))

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
      (is (re-find #"stroke-dasharray"  s) "dashed projection line")
      (is (re-find #"now"               s) "now label")
      (is (re-find #"100%"              s) "100% reference label"))))

(deftest chart-svg-renders-projection-line
  (testing "single projection polyline rendered"
    (let [s (str (u/chart-svg (make-data)))]
      (is (re-find #"proj-line" s)))))

(deftest chart-svg-renders-band
  (testing "credible-interval band path is present"
    (let [s (str (u/chart-svg (make-data)))]
      (is (re-find #"band-region" s)))))

(deftest chart-svg-no-projection-omits-band-and-line
  (testing "when projection is nil, no band or proj-line is rendered"
    (let [s (str (u/chart-svg (assoc (make-data) :projection nil)))]
      (is (not (re-find #"band-region" s)))
      (is (not (re-find #"proj-line"   s))))))

(deftest chart-svg-coords-are-numeric-not-ratios
  (testing "polyline points must not contain Clojure ratios"
    (let [s (str (u/chart-svg (make-data)))]
      (doseq [[_ body] (re-seq #"points=\"([^\"]+)\"" s)]
        (is (not (re-find #"/" body))
            (str "ratio leaked into points=\"" body "\""))))))

(deftest projection-above-100-is-clamped-to-y-top
  (testing "a runaway projection still fits inside viewBox"
    (let [data (assoc (make-data)
                      :projection
                      {:method :rate-bayes :name "Rate, Bayesian" :rate 5.0
                       :proj 350.0 :band {:lo 300 :hi 400}})
          out  (str (u/chart-svg data))
          ys   (->> (re-seq #"y[12]?=\"(-?\d+\.?\d*)\"" out)
                    (map (comp #(Double/parseDouble %) second)))]
      (is (every? #(<= -1.0 % 281.0) ys)))))

(deftest summary-stats-renders-projection
  (testing "right-rail stats include projected % and credible interval"
    (let [out (str (u/summary-stats (make-data)))]
      (is (re-find #"24%"   out) "current pct")
      (is (re-find #"75%"   out) "projected pct")
      (is (re-find #"60.*90" out) "band rendered as range")
      (is (re-find #"projected at reset" out)))))

(deftest summary-stats-without-projection
  (testing "no projection → no projection stat row"
    (let [out (str (u/summary-stats (assoc (make-data) :projection nil)))]
      (is (not (re-find #"projected at reset" out)))
      (is (re-find #"current" out) "core stats still present"))))

(deftest legend-shows-observed-and-projected
  (let [out (str (u/legend (make-data)))]
    (is (re-find #"observed" out))
    (is (re-find #"projected" out))))

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
