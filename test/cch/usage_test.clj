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

(deftest legend-shows-observed-and-projected
  (let [out (str (u/legend (make-data)))]
    (is (re-find #"observed" out))
    (is (re-find #"projected" out))))

(deftest page-body-no-data
  (testing "no-data path renders the chart fallback"
    (let [out (str (u/page-body nil))]
      (is (re-find #"Not enough" out)))))

(deftest page-body-with-data
  (testing "data path renders chart + legend"
    (let [out (str (u/page-body (make-data)))]
      (is (re-find #"usage-chart-block" out))
      (is (re-find #"svg" out)))))

(defn- make-5h-data []
  ;; Halfway through a 5h window, four observed samples at 30-min cadence.
  (let [now (* 2 3600)]
    {:window-key   :five-hour
     :span-secs    (* 5 3600)
     :observed     [{:ts 0    :pct 0.0}
                    {:ts 1800 :pct 12.0}
                    {:ts 3600 :pct 25.0}
                    {:ts 5400 :pct 38.0}]
     :rate-samples [{:ts 0    :pct 0.0  :resets-at (* 5 3600)}
                    {:ts 1800 :pct 12.0 :resets-at (* 5 3600)}
                    {:ts 3600 :pct 25.0 :resets-at (* 5 3600)}
                    {:ts 5400 :pct 38.0 :resets-at (* 5 3600)}]
     :rate-scale   1.0
     :resets-at    (* 5 3600)
     :window-start 0
     :now          now
     :last-pct     38.0
     :samples      4
     :projection   {:method :rate-bayes :name "Rate, Bayesian"
                    :proj 75.0 :band {:lo 60 :hi 90}}}))

(deftest chart-svg-5h-uses-hour-ticks
  (testing "5h-window chart renders HH:mm tick labels, not 'MMM d'"
    (let [s (str (u/chart-svg (make-5h-data)))]
      ;; chart-svg returns hiccup; tick labels appear as quoted strings.
      (is (re-find #"\"\d\d:\d\d\"" s)
          "expected at least one HH:mm tick label")
      ;; The 7d view emits 'MMM d' strings; 5h should not.
      (is (not (re-find #"\"[A-Z][a-z]{2} \d" s))
          "month-name tick should be absent in 5h view"))))

(deftest chart-svg-5h-skips-reset-cycle-ticks
  (testing "5h view omits the per-24h reset-cycle marker lines"
    (let [s (str (u/chart-svg (make-5h-data)))]
      (is (not (re-find #"reset-cycle-tick" s))))))
