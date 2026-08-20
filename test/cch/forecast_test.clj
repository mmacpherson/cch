(ns cch.forecast-test
  (:require [babashka.fs :as fs]
            [cch.db :as db]
            [cch.forecast :refer [weighted-prior-params signal-new-data!
                                   start-bg-refresh! stop-bg-refresh!
                                   statusline-stats]]
            [cch.log :as log]
            [cch.projections]
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

(deftest bg-loop-survives-throwable-from-refresh
  ;; Regression: a Throwable (Error, not Exception) escaping do-refresh! used
  ;; to kill the bg thread silently, freezing /forecast on a stale snapshot.
  (with-fresh-bg 100
    (fn [debounce-ms]
      (let [calls (atom 0)
            mode  (atom :throw)
            real  @#'cch.forecast/do-refresh!]
        (with-redefs [cch.forecast/do-refresh!
                      (fn []
                        (swap! calls inc)
                        (case @mode
                          :throw (throw (Error. "simulated fatal"))
                          :ok    (real)))]
          (start-bg-refresh! :debounce-ms debounce-ms)
          (Thread/sleep 300) ; initial seed throws
          (signal-new-data!)
          (Thread/sleep 300) ; second refresh also throws
          (let [calls-after-throws @calls]
            (is (>= calls-after-throws 2)
                "thread must keep handling signals after throwing")
            (reset! mode :ok)
            (signal-new-data!)
            (Thread/sleep 300)
            (is (> @calls calls-after-throws)
                "subsequent signal must trigger another refresh attempt")))))))

(deftest timer-backstop-refreshes-without-signals
  ;; Liveness must not depend on signal delivery: even with zero signals,
  ;; the bg loop must keep the cache fresh via its timer wakeup.
  (with-fresh-bg 50
    (fn [debounce-ms]
      (start-bg-refresh! :debounce-ms debounce-ms :max-stale-ms 150)
      (Thread/sleep 100) ; initial seed
      (let [updates (atom 0)
            cache   @#'cch.forecast/forecast-cache]
        (add-watch cache ::tick-test (fn [_ _ _ _] (swap! updates inc)))
        (try
          ;; Send no signals — wait long enough for ≥2 timer ticks.
          (Thread/sleep 500)
          (is (>= @updates 2)
              "timer backstop must drive refreshes when no signals arrive")
          (finally
            (remove-watch cache ::tick-test)))))))

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

;; ---------------------------------------------------------------------------
;; window-priors — both /usage and /forecast (statusline) MUST share this so
;; their projections agree. Regression: /usage used to call rate-bayes-
;; projection without window-info priors, falling back to its hardcoded 7d
;; defaults (μ=0.42) for the 5h window. Net effect: a 5h chart projection
;; that barely moved, even when /forecast's 5h projected ~22 pts above
;; current. (claude-code-hooks-z3w)
;; ---------------------------------------------------------------------------

(deftest window-priors-distinct-per-window
  (testing "5h seed prior is much larger than 7d (μ in %/hr units)"
    (with-redefs [cch.forecast/learned-prior (fn [_ _] nil)]
      (let [seven-day (#'cch.forecast/window-priors "claude-code" :seven-day)
            five-hour (#'cch.forecast/window-priors "claude-code" :five-hour)]
        (is (= 0.42 (:prior-mu seven-day)))
        (is (= 3.75 (:prior-mu five-hour))
            "5h prior must NOT silently default to the 7d rate — that was z3w")
        (is (< (:prior-mu seven-day) (:prior-mu five-hour)))))))

(deftest window-priors-applies-learned-when-available
  ;; The learned empirical-Bayes prior (per agent, per window) MUST reach the
  ;; projection as :prior-mu/:prior-sigma, replacing the cold-start seed.
  ;; Regression: window-priors used to `(merge base learned)`, leaving learned
  ;; under :mu/:sigma where downstream :prior-mu/:prior-sigma consumers dropped
  ;; it — the learned prior was dead code.
  (with-redefs [cch.forecast/learned-prior
                (fn [_ window-key]
                  (case window-key
                    :seven-day {:mu 0.99 :sigma 0.01}
                    :five-hour {:mu 5.5  :sigma 0.7}))]
    (let [seven-day (#'cch.forecast/window-priors "claude-code" :seven-day)
          five-hour (#'cch.forecast/window-priors "claude-code" :five-hour)]
      (is (= 0.99 (:prior-mu seven-day)) "learned prior reaches 7d as :prior-mu")
      (is (= 0.01 (:prior-sigma seven-day)))
      (is (= 5.5 (:prior-mu five-hour)) "learned prior reaches 5h as :prior-mu")
      (is (= 0.7 (:prior-sigma five-hour))))))

(deftest window-priors-falls-back-to-seed-without-history
  (testing "no learned prior → window-config cold-start seed is used"
    (with-redefs [cch.forecast/learned-prior (fn [_ _] nil)]
      (let [five-hour (#'cch.forecast/window-priors "claude-code" :five-hour)]
        (is (= 3.75 (:prior-mu five-hour)))
        (is (= 1.3 (:prior-sigma five-hour)))))))

(deftest build-current-window-projection-uses-window-specific-prior
  ;; Reproduce z3w. Build-current-window's projection bundle must carry the
  ;; same prior-mu/prior-sigma that compute-window-stats would pass — so the
  ;; /usage chart and the /forecast statusline agree.
  (let [captured (atom [])]
    (with-redefs [cch.forecast/learned-prior          (fn [_ _] nil)
                  cch.forecast/latest-resets-at       (fn [_ _] 1000000000)
                  cch.forecast/filtered-samples       (fn [_ _ _] [])
                  cch.forecast/rate-5h-samples        (fn [_ _] [])
                  cch.forecast/raw-sample-count       (fn [_ _ _] 0)
                  cch.forecast/historical-final-pcts  (fn [_ _] nil)
                  cch.projections/rate-bayes-projection
                  (fn [_observed window-info]
                    (swap! captured conj (select-keys window-info
                                                     [:prior-mu :prior-sigma]))
                    {:proj 0.0})]
      (#'cch.forecast/build-current-window "claude-code" :five-hour)
      (let [pi (last @captured)]
        (is (= 3.75 (:prior-mu pi))
            "/usage's 5h projection must use the 5h prior, not the 7d default")
        (is (= 1.3 (:prior-sigma pi)))))))
