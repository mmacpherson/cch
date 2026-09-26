(ns cch.usage-stan-test
  (:require [cch.usage-model :as m]
            [cch.usage-stan :as us]
            [clojure.test :refer [deftest is testing]])
  (:import (java.time ZoneId)))

(def ^:private utc (ZoneId/of "UTC"))
(def ^:private t0 1780272000) ; Monday 2026-06-01 00:00 UTC

(defn- weekly-rows
  "Seven completed 7d windows plus a current one, 10% per day of use."
  []
  (for [w (range 8)
        :let [end (+ t0 (* (inc w) 7 86400))
              start (- end (* 7 86400))]
        d (range 5)]
    {:resets-at end :hour (+ start (* d 86400) (* 10 3600)) :pct (* 10.0 (inc d))}))

(deftest fit-data-is-consistent-csr
  (let [now (+ t0 (* 7 7 86400) (* 2 86400))
        thin (take 3 (weekly-rows))                ; one short window
        {:keys [data cells]} (us/fit-data {["claude-code" :seven-day] (weekly-rows)
                                           ["agy" :seven-day] thin
                                           ["codex" :seven-day] []}
                                          now utc)
        {:keys [C A NB NW y cell_start cell_len block_ptr hbin live g_init ctype agent]} data]
    (testing "thin cells are kept for pooling; cells without usage are dropped"
      (is (= [["agy" :seven-day] ["claude-code" :seven-day]] cells))
      (is (= 2 C A))
      (is (= [1 2] agent)))
    (testing "block arrays line up across cells"
      (is (= NB (count y) (reduce + cell_len)))
      (is (= [1 (inc (first cell_len))] cell_start))
      (is (= (inc NB) (count block_ptr)))
      (is (= NW (count hbin) (count live) (dec (peek block_ptr))))
      (is (every? #(<= 1 % 168) hbin)))
    (testing "each cell's blocks carry the same usage as the maximum-likelihood fit's blocks"
      (let [spec (m/specs :seven-day)
            ml-blocks (fn [rows] (m/blocks (m/hour-series (m/windows rows spec) now)
                                           (vec (repeat 168 1.0)) utc spec))
            [n1 n2] cell_len]
        (is (= (mapv #(nth % 2) (ml-blocks thin)) (subvec y 0 n1)))
        (is (= (mapv #(nth % 2) (ml-blocks (weekly-rows))) (subvec y n1 (+ n1 n2))))))
    (testing "the fleet log-profile has 168 bins; the basis sizes are data"
      (is (= 168 (count g_init)))
      (is (pos-int? (:n_daily data)))
      (is (every? nat-int? [(:n_weekly data) (:n_weekend data)]))
      (is (= [1 1] ctype)))))
