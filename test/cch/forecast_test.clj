(ns cch.forecast-test
  (:require [babashka.fs :as fs]
            [cch.db :as db]
            [cch.forecast :refer [weighted-prior-params signal-new-data!
                                   start-bg-refresh! stop-bg-refresh!
                                   statusline-stats]]
            [cch.log :as log]
            [clojure.test :refer [deftest testing is]]))

;; ---------------------------------------------------------------------------
;; weighted-prior-params — pure fn, no DB required
;; ---------------------------------------------------------------------------

(deftest weighted-prior-params-requires-two-windows
  (testing "nil with zero windows"
    (is (nil? (weighted-prior-params []))))
  (testing "nil with one window"
    (is (nil? (weighted-prior-params [{:final_pct 80.0}])))))

(deftest weighted-prior-params-two-identical-weeks
  (testing "mu is the common rate; sigma is at the floor"
    (let [{:keys [mu sigma]} (weighted-prior-params [{:final_pct 92.4}
                                                     {:final_pct 92.4}])
          expected-mu (/ 92.4 (* 7.0 24.0))]
      (is (< (Math/abs (- mu expected-mu)) 1e-6))
      ;; variance is zero → clamped to prior-sigma-floor
      (is (= sigma 0.03)))))

(deftest weighted-prior-params-recency-weighting
  (testing "most-recent week (index 0) is weighted highest"
    ;; Two weeks: recent=100%, older=0%. With decay 0.85, recent has weight 1,
    ;; older has weight 0.85. Weighted mean should be closer to 100%/hr.
    (let [{:keys [mu]} (weighted-prior-params [{:final_pct 100.0}
                                               {:final_pct 0.0}])
          unweighted-mean (/ (/ (+ 100.0 0.0) 2.0) (* 7.0 24.0))]
      (is (> mu unweighted-mean) "recent 100% week should pull mu above unweighted mean"))))

(deftest weighted-prior-params-sigma-reflects-spread
  (testing "wider spread → larger sigma"
    (let [tight (weighted-prior-params [{:final_pct 90.0} {:final_pct 88.0}
                                        {:final_pct 91.0} {:final_pct 89.0}])
          wide  (weighted-prior-params [{:final_pct 100.0} {:final_pct 50.0}
                                        {:final_pct 95.0}  {:final_pct 40.0}])]
      (is (< (:sigma tight) (:sigma wide))))))

(deftest weighted-prior-params-mu-in-plausible-range
  (testing "typical 75-95% weeks produce a mu near 0.5-0.6 %/hr"
    (let [{:keys [mu sigma]}
          (weighted-prior-params [{:final_pct 92.0} {:final_pct 85.0}
                                  {:final_pct 78.0} {:final_pct 90.0}
                                  {:final_pct 95.0}])]
      (is (< 0.4 mu 0.7))
      (is (pos? sigma)))))

(deftest weighted-prior-params-up-to-12-weeks
  (testing "accepts and processes 12 rows without error"
    (let [rows (repeat 12 {:final_pct 88.0})
          {:keys [mu sigma]} (weighted-prior-params rows)]
      (is (some? mu))
      (is (= sigma 0.03)))))

;; ---------------------------------------------------------------------------
;; bg-refresh lifecycle + signal/debounce — requires a real (empty) DB
;;
;; Uses a private-var ref hack to watch and reset the cache atom without
;; exposing it in the public API.  Short debounce-ms keeps wall-clock time
;; manageable: 100 ms debounce, 300-500 ms sleeps for timing slack.
;; ---------------------------------------------------------------------------

(defn- with-fresh-bg
  "Run `f` against a throw-away SQLite DB, then stop the bg thread and
  reset global atoms so tests don't bleed into each other."
  [debounce-ms f]
  (let [tmp     (str (fs/create-temp-dir {:prefix "forecast-bg-test-"}))
        db-path (str tmp "/events.db")]
    (with-redefs [db/db-path (fn [] db-path)]
      (log/ensure-db! db-path)
      (try
        (f debounce-ms)
        (finally
          (stop-bg-refresh!)
          (reset! @#'cch.forecast/forecast-cache nil)
          (reset! @#'cch.forecast/pending?       false)
          (fs/delete-tree tmp))))))

(deftest bg-refresh-seeds-cache-on-startup
  (with-fresh-bg 100
    (fn [debounce-ms]
      (start-bg-refresh! :debounce-ms debounce-ms)
      (Thread/sleep 300)
      (testing "statusline-stats returns a map immediately after startup"
        (is (map? (statusline-stats))
            "cache should be a map (windows may be nil-valued on empty DB)")))))

(deftest signal-triggers-recompute
  (with-fresh-bg 100
    (fn [debounce-ms]
      (start-bg-refresh! :debounce-ms debounce-ms)
      (Thread/sleep 300) ; let initial compute settle
      (let [updates (atom 0)
            cache   @#'cch.forecast/forecast-cache]
        (add-watch cache ::signal-test (fn [_ _ _ _] (swap! updates inc)))
        (try
          (signal-new-data!)
          (Thread/sleep 400) ; debounce (100 ms) + compute + margin
          (is (pos? @updates) "a signal should trigger at least one cache update")
          (finally
            (remove-watch cache ::signal-test)))))))

(deftest signal-debounces-burst
  (with-fresh-bg 200
    (fn [debounce-ms]
      (start-bg-refresh! :debounce-ms debounce-ms)
      (Thread/sleep 500) ; let initial compute settle
      (let [updates (atom 0)
            cache   @#'cch.forecast/forecast-cache]
        (add-watch cache ::burst-test (fn [_ _ _ _] (swap! updates inc)))
        (try
          ;; 8 signals spaced 15 ms apart — all within a single debounce window.
          (dotimes [_ 8] (signal-new-data!) (Thread/sleep 15))
          (Thread/sleep 800) ; debounce (200 ms) + compute + generous margin
          (is (<= @updates 2)
              "8 rapid signals within one debounce window should coalesce to ≤2 computes")
          (finally
            (remove-watch cache ::burst-test)))))))
