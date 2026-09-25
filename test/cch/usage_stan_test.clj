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
        {:keys [data cells]} (us/fit-data {["claude-code" :seven-day] (weekly-rows)
                                           ["agy" :seven-day] (take 3 (weekly-rows))}
                                          now utc)
        {:keys [C NB NW y cell_start cell_len block_ptr hbin live S g_init ctype]} data]
    (testing "cells with too little history are left out"
      (is (= [["claude-code" :seven-day]] cells))
      (is (= 1 C)))
    (testing "block arrays line up"
      (is (= NB (count y) (reduce + cell_len)))
      (is (= [1] cell_start))
      (is (= (inc NB) (count block_ptr)))
      (is (= NW (count hbin) (count live) (dec (peek block_ptr))))
      (is (every? #(<= 1 % 168) hbin)))
    (testing "blocks carry the same usage as the in-JVM model's blocks"
      (let [spec (m/specs :seven-day)
            series (m/hour-series (m/windows (weekly-rows) spec) now)
            jvm (m/blocks series (vec (repeat 168 1.0)) utc spec)]
        (is (= (count jvm) NB))
        (is (= (mapv #(nth % 2) jvm) y))))
    (testing "smoothing rows sum to one; the fleet log-profile has 168 bins"
      (is (every? #(< (Math/abs (- 1.0 (reduce + %))) 1e-12) S))
      (is (= 168 (count g_init)))
      (is (= [1] ctype)))))
