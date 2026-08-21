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
;; bg-refresh: fixed-cadence timer gated on a snapshot watermark.
;;
;; The refresh no longer wakes per snapshot (that pinned a core at a busy box's
;; snapshot rate); it recomputes at most once per cadence tick, and only when
;; context_snapshots has grown. These tests drive the pure gate (maybe-refresh!)
;; directly where possible, and keep one lenient timing test for the loop.
;; ---------------------------------------------------------------------------

(def ^:private maybe-refresh!   #'cch.forecast/maybe-refresh!)
(def ^:private snapshot-watermark #'cch.forecast/snapshot-watermark)
(def ^:private do-refresh!      #'cch.forecast/do-refresh!)
(def ^:private safe-refresh!    #'cch.forecast/safe-refresh!)

(defn- insert-snapshot! []
  (db/query "INSERT INTO context_snapshots (session_id) VALUES ('bg-test')"))

(defn- with-fresh-bg
  "Run `f` against a throw-away SQLite DB, then stop the bg thread and
  reset global atoms so tests don't bleed into each other."
  [f]
  (let [tmp     (str (fs/create-temp-dir {:prefix "forecast-bg-test-"}))
        db-path (str tmp "/events.db")]
    (with-redefs [db/db-path (fn [] db-path)]
      (log/ensure-db! db-path)
      (reset! @#'cch.forecast/last-watermark nil)
      (try
        (f)
        (finally
          (stop-bg-refresh!)
          (reset! @#'cch.forecast/forecast-cache nil)
          (reset! @#'cch.forecast/last-watermark nil)
          (fs/delete-tree tmp))))))

(deftest bg-refresh-seeds-cache-on-startup
  (with-fresh-bg
    (fn []
      (start-bg-refresh! :interval-ms 10000) ; long — we only want the startup seed
      (Thread/sleep 300)
      (testing "statusline-stats returns a map immediately after startup"
        (is (map? (statusline-stats))
            "cache should be a map (windows may be nil-valued on empty DB)")))))

(deftest maybe-refresh-gates-on-watermark
  (with-fresh-bg
    (fn []
      (testing "first call refreshes (watermark moves from nil)"
        ;; empty DB: MAX(id) is nil, last-watermark is nil → no change → skip
        (is (nil? (snapshot-watermark)))
        (is (not (maybe-refresh!)) "no rows, no movement → no refresh"))
      (testing "a new snapshot moves the watermark → one refresh"
        (insert-snapshot!)
        (is (some? (snapshot-watermark)))
        (is (maybe-refresh!) "new row → refresh")
        (is (map? (statusline-stats))))
      (testing "no further rows → gate skips, however many ticks"
        (is (not (maybe-refresh!)))
        (is (not (maybe-refresh!))))
      (testing "another snapshot re-opens the gate"
        (insert-snapshot!)
        (is (maybe-refresh!))))))

(deftest safe-refresh-swallows-throwable
  ;; Regression: a Throwable (Error, not Exception) escaping do-refresh! used
  ;; to kill the bg thread, freezing /forecast on a stale snapshot. safe-refresh!
  ;; must swallow it so the loop survives.
  (with-fresh-bg
    (fn []
      (with-redefs [cch.forecast/do-refresh! (fn [] (throw (Error. "simulated fatal")))]
        (is (nil? (safe-refresh!)) "must not propagate the Error")
        ;; and the gate, which calls safe-refresh!, also must not throw
        (insert-snapshot!)
        (is (true? (maybe-refresh!)) "gate still reports it attempted a refresh")))))

(deftest timer-drives-refresh-on-new-data
  ;; One lenient timing test: with a short cadence, a snapshot inserted after
  ;; startup must be picked up by a subsequent tick.
  (with-fresh-bg
    (fn []
      (start-bg-refresh! :interval-ms 80)
      (Thread/sleep 150) ; startup seed + at least one tick on the empty DB
      (let [updates (atom 0)
            cache   @#'cch.forecast/forecast-cache]
        (add-watch cache ::tick (fn [_ _ _ _] (swap! updates inc)))
        (try
          (insert-snapshot!)
          (Thread/sleep 300) ; several 80ms ticks — one must catch the new row
          (is (pos? @updates) "a tick must recompute after new data arrives")
          (finally
            (remove-watch cache ::tick)))))))

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
