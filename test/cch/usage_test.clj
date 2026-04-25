(ns cch.usage-test
  (:require [clojure.test :refer [deftest is testing]]
            [cch.usage :as u]))

(defn- make-data
  "Synthetic data bundle for chart tests — simulates a 7d window
   that's halfway through, with a few observed samples and a slow
   EWMA implying we'll land near 100% at reset."
  [& {:keys [obs proj-pct proj-lo proj-hi]
      :or   {obs      [{:ts 0      :pct 0.0}
                       {:ts 86400  :pct 8.0}
                       {:ts 172800 :pct 16.0}
                       {:ts 259200 :pct 24.0}]
             proj-pct 80.0
             proj-lo  60.0
             proj-hi  95.0}}]
  {:observed     obs
   :resets-at    (* 7 86400)
   :window-start 0
   :now          (* 3 86400)
   :last-pct     (:pct (last obs))
   :slow_x       1.0
   :fast_x       1.2
   :proj-pct     proj-pct
   :proj-lo      proj-lo
   :proj-hi      proj-hi
   :samples      (count obs)})

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
      (is (re-find #"polyline"     s) "observed line")
      (is (re-find #"\bpath\b"     s) "projection band path")
      (is (re-find #"stroke-dasharray" s) "dashed elements present")
      (is (re-find #"now"          s) "now label")
      (is (re-find #"100%"         s) "100% reference label"))))

(deftest projection-above-100-is-clamped-to-y-top
  (testing "a runaway projection still fits inside viewBox"
    (let [data (make-data :proj-pct 350.0 :proj-hi 400.0)
          out  (str (u/chart-svg data))
          ;; viewBox height is 280; nothing should land below 0 or above 280
          ys (->> (re-seq #"y[12]?=\"(-?\d+\.?\d*)\"" out)
                  (map (comp #(Double/parseDouble %) second)))]
      (is (every? #(<= -1.0 % 281.0) ys)))))

(deftest summary-stats-is-data-driven
  (testing "renders the headline numbers we want users to see"
    (let [out (str (u/summary-stats (make-data)))]
      (is (re-find #"24%"   out) "current usage")
      (is (re-find #"80%"   out) "projection")
      (is (re-find #"60.*95" out) "band low–high")
      (is (re-find #"1\.00" out) "slow_x")
      (is (re-find #"1\.20" out) "fast_x"))))

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
