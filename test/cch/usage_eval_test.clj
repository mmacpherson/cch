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
  (testing "too few calendar weeks give no interval"
    (let [rows (for [w (range 4)] {:win w :week w :hours 1 :crps (double w)})]
      (is (Double/isNaN (:se (e/paired rows rows :seven-day :crps))))))
  (testing "a single calendar week gives no interval"
    (let [rows [{:win 0 :week 3 :hours 1 :crps 1.0}]]
      (is (Double/isNaN (:se (e/paired rows rows :seven-day :crps)))))))

(deftest horizon-estimate-lets-short-windows-inform-only-early-days
  (let [rows (concat
               ;; a full week: day-1 error 1, day-7 error 9
               [{:win 1 :week 0 :hours 12 :d 1.0} {:win 1 :week 0 :hours 160 :d 9.0}]
               ;; three two-day stubs (granted resets): day-1 error 1
               (for [w [2 3 4]] {:win w :week w :hours 12 :d 1.0}))
        wv (e/window-values rows :seven-day :d)]
    (is (= 5.0 (e/horizon-estimate wv)) "day 1 -> 1, day 7 -> 9")
    (is (= 2.0 (e/estimate wv)) "equal-window weight lets the stubs dominate: (5+1+1+1)/4")))

(deftest agent-weights-combine-agents
  (let [rows (concat (for [w (range 3)] {:win [:a w] :week w :agent "claude-code" :hours 12 :d 2.0})
                     (for [w (range 9)] {:win [:c w] :week w :agent "codex" :hours 12 :d 8.0}))
        est (e/agent-weighted {"claude-code" 0.75 "codex" 0.25 "agy" 0.5})]
    (is (< (Math/abs (- (+ (* 0.75 2.0) (* 0.25 8.0)) (est (e/window-values rows :seven-day :d)))) 1e-12)
        "absent agents drop out and the rest renormalize")))
