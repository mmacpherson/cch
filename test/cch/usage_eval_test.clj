(ns cch.usage-eval-test
  (:require [cch.usage-eval :as e]
            [clojure.test :refer [deftest is testing]]))

(deftest strata-by-meter
  (is (= [1 1 2 7 7] (map #(e/stratum :seven-day %) [0.5 24 25 150 400])))
  (is (= [1 1 2 5] (map #(e/stratum :five-hour %) [0.5 1 1.5 9]))))

(deftest windows-weigh-equally-whatever-their-checkpoint-count
  (let [rows (concat
               ;; window 1: one checkpoint, difference 10
               [{:win 1 :week 0 :hours 12 :d 10.0}]
               ;; window 2: nine checkpoints on day 1, difference 0
               (for [h (range 1 10)] {:win 2 :week 1 :hours h :d 0.0}))
        wv (e/window-values rows :seven-day :d)]
    (is (= 5.0 (e/estimate wv)) "not (10 + 0*9)/10 = 1")))

(deftest strata-weigh-equally-within-a-window
  (let [rows (concat (for [h (range 1 24)] {:win 1 :week 0 :hours h :d 0.0})   ; day 1: many
                     [{:win 1 :week 0 :hours 30 :d 6.0}])                          ; day 2: one
        [{:keys [value]}] (e/window-values rows :seven-day :d)]
    (is (= 3.0 value))))

(deftest paired-difference-and-bootstrap
  (let [base (for [w (range 8) h [12 36]] {:win w :week w :hours h :crps 10.0})
        better (map #(update % :crps - 2.0) base)
        {:keys [delta se lo hi]} (e/paired better base :seven-day :crps)]
    (is (= -2.0 delta))
    (is (< se 1e-9) "a constant difference has no sampling spread")
    (is (<= lo -2.0 hi)))
  (testing "a single calendar week gives no interval"
    (let [rows [{:win 0 :week 3 :hours 1 :crps 1.0}]]
      (is (Double/isNaN (:se (e/paired rows rows :seven-day :crps)))))))
