(ns cch.usage-ledger-test
  (:require [babashka.fs :as fs]
            [cch.db :as db]
            [cch.log :as log]
            [cch.usage-ledger :as ledger]
            [cheshire.core :as json]
            [clojure.test :refer [deftest is testing]]
            [next.jdbc :as jdbc]))

(deftest quantile-score-approximates-crps
  (testing "a sharp forecast at the outcome scores ~0; misses cost ~their distance"
    (is (< (ledger/quantile-score (repeat 19 40.0) 40.0) 1e-12))
    (is (< (Math/abs (- 10.0 (ledger/quantile-score (repeat 19 50.0) 40.0))) 1e-9)))
  (testing "quantiles above the cap are scored as the cap"
    (is (< (ledger/quantile-score (repeat 19 180.0) 100.0) 1e-12))))

(deftest gaussian-quantiles-match-the-band
  (let [qs (ledger/gaussian-quantiles 60.0 40.0 80.0 0.0)]
    (is (= 19 (count qs)))
    (is (< (Math/abs (- 40.0 (first qs))) 1e-9) "5th percentile is the band's low end")
    (is (< (Math/abs (- 60.0 (nth qs 9))) 1e-9) "median")
    (is (< (Math/abs (- 80.0 (peek qs))) 1e-9)))
  (is (every? #(>= % 55.0) (ledger/gaussian-quantiles 60.0 40.0 80.0 55.0))
      "floored at the current reading"))

(deftest score-row-coverage-and-brier
  (let [qs (vec (range 10.0 105.0 5.0))]           ; 10, 15, ..., 100
    (is (:cov90 (ledger/score-row {:quantiles qs :p-cap 0.1} 50.0 false)))
    (is (not (:cov50 (ledger/score-row {:quantiles qs :p-cap 0.1} 90.0 false))))
    (is (< (Math/abs (- 0.81 (:brier (ledger/score-row {:quantiles qs :p-cap 0.1} 100.0 true)))) 1e-9))))

(deftest record-and-score-round-trip
  (let [tmp (str (fs/create-temp-dir {:prefix "ledger-test-"}))
        path (str tmp "/events.db")
        ds {:dbtype "sqlite" :dbname path}
        start 1780272000
        end (+ start (* 7 86400))
        now (+ end (* 3 86400))]
    (try
      (with-redefs [db/db-path (fn [] path)]
        (log/ensure-db! path)
        ;; a completed 7d window ending at 40%
        (doseq [[k pct] (map-indexed vector [5.0 15.0 25.0 40.0])]
          (jdbc/execute! ds [(str "INSERT INTO usage_observations (event_id, schema_version, observed_at, agent, "
                                  "window_key, used_percentage, resets_at) VALUES (?,1,?,'claude-code','seven_day',?,?)")
                             (str "t" k) (* 1000 (+ start (* (inc k) 86400))) pct end]))
        (ledger/record! {:model "ml-harmonic-v1" :agent "claude-code" :window-key :seven-day
                         :resets-at end :now (+ start 86400 60) :current-pct 5.0
                         :quantiles (repeat 19 40.0) :p-cap 0.0})
        (testing "one row per model, window, and hour"
          (ledger/record! {:model "ml-harmonic-v1" :agent "claude-code" :window-key :seven-day
                           :resets-at end :now (+ start 86400 120) :current-pct 5.0
                           :quantiles (repeat 19 99.0) :p-cap 0.0})
          (is (= 1 (:n (first (db/query "SELECT COUNT(*) AS n FROM usage_forecast_ledger"))))))
        (is (= (repeat 19 40.0)
               (json/parse-string (:quantiles (first (db/query "SELECT quantiles FROM usage_forecast_ledger"))))))
        (let [[r & more] (ledger/scored-records now)]
          (is (nil? more))
          (is (= "ml-harmonic-v1" (:model r)))
          (is (< (:crps r) 1e-9) "forecast 40, final 40")
          (is (:cov90 r))))
      (finally (fs/delete-tree tmp)))))
