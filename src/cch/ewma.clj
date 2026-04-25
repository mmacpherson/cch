(ns cch.ewma
  "Exponentially-weighted moving averages of the seven_day rate-limit
  burn rate, computed from the rate_limits payload that Claude Code's
  statusLine POSTs into context_snapshots.

  Used by GET /ewma to feed the statusLine an at-a-glance pace signal:

      pace: 0.7× ↘     (slow EWMA as a multiple of linear weekly pace,
                        arrow shows whether the fast EWMA is above,
                        near, or below the slow one)

  Reset events (used_percentage drops when the 7d window rolls forward)
  are detected as negative deltas and start a fresh EWMA segment."
  (:require [babashka.process :as p]
            [cheshire.core :as json]
            [cch.log :as log]
            [clojure.string :as str])
  (:import (java.time Instant)))

(def ^:private target-pct-per-hr
  "Linear pace for the 7-day window: 100% ÷ (7 days × 24 hours)."
  (/ 100.0 (* 7 24)))

(def ^:private tau-fast-hr 1.0)
(def ^:private tau-slow-hr 6.0)
(def ^:private window-size 500) ; how many recent snapshots to consider

;; Sub-15-minute samples carry almost no rate information (used_percentage
;; is integer; even at 5× target, a whole-percent move takes ~20 minutes),
;; so two samples that close apart get folded into one transition. Larger
;; gaps with zero Δpct are kept — long quiet stretches genuinely are
;; evidence of a low burn rate.
(def ^:private min-gap-hr 0.25)

(defn- iso->epoch-seconds
  "Parse the DB's ISO8601-ish timestamps. Rows are written as naive
  strings (no zone), so append 'Z' to satisfy Instant/parse's strict
  contract — context_snapshots are recorded server-side in UTC."
  [^String iso]
  (let [iso' (if (or (str/ends-with? iso "Z")
                     (re-find #"[+-]\d\d:?\d\d$" iso))
               iso
               (str iso "Z"))]
    (-> iso' Instant/parse .getEpochSecond)))

(defn ewma-step
  "One EWMA update. Returns the new estimate.
   prev=nil seeds with new-rate (no smoothing on first sample)."
  [prev new-rate dt-hr tau-hr]
  (if (nil? prev)
    (double new-rate)
    (let [alpha (- 1.0 (Math/exp (- (/ (double dt-hr) (double tau-hr)))))]
      (+ (* alpha (double new-rate))
         (* (- 1.0 alpha) (double prev))))))

(defn- arrow-for [fast-x slow-x]
  (cond
    (> fast-x (* slow-x 1.15)) "↗"
    (< fast-x (* slow-x 0.85)) "↘"
    :else "→"))

(defn- recent-snapshots
  "Pull (timestamp, payload) for the most recent `n` rows that have a
  rate_limits.seven_day block. Returns oldest-first for fold."
  [n]
  (let [path (log/db-path)
        sql (format
              (str "SELECT json_object('ts', timestamp, 'payload', payload) "
                   "FROM (SELECT timestamp, payload FROM context_snapshots "
                   "WHERE payload LIKE '%%rate_limits%%' "
                   "AND session_id NOT LIKE 'test%%' "
                   "ORDER BY id DESC LIMIT %d) ORDER BY timestamp ASC;")
              n)
        result (p/sh ["sqlite3" path sql])]
    (when (zero? (:exit result))
      (->> (str/split-lines (str/trim (:out result)))
           (remove str/blank?)
           (keep (fn [line]
                   (try
                     (let [row (json/parse-string line true)
                           p   (json/parse-string (:payload row) true)
                           sd  (get-in p [:rate_limits :seven_day])]
                       (when (and sd (:used_percentage sd) (:resets_at sd))
                         {:ts          (iso->epoch-seconds (:ts row))
                          :pct         (double (:used_percentage sd))
                          :resets-at   (long (:resets_at sd))}))
                     (catch Exception _ nil))))))))

(defn fold-ewma
  "Fold a sequence of {:ts :pct :resets-at} into final EWMA state.
   Returns {:fast :slow :last :samples} or nil if <2 usable transitions.

   - Reset events (negative Δpct) start a fresh segment but keep the
     most recent estimates carried over (the burn rate doesn't reset
     just because the window did).
   - Samples closer than min-gap-hr to the previous accepted sample
     are folded forward (no information, just statusLine spam).
   - Larger gaps with zero Δpct ARE kept: a 4-hour quiet stretch is
     genuine rate-0 evidence and should drag the EWMAs down."
  [snapshots]
  (loop [xs snapshots, prev nil, fast nil, slow nil, n 0]
    (if-let [s (first xs)]
      (cond
        (nil? prev)
        (recur (rest xs) s fast slow n)

        (< (:pct s) (:pct prev))   ; window reset rolled forward
        (recur (rest xs) s fast slow n)

        :else
        (let [dt-hr (/ (- (:ts s) (:ts prev)) 3600.0)]
          (if (< dt-hr min-gap-hr)
            (recur (rest xs) prev fast slow n)   ; too close, fold forward
            (let [rate  (/ (- (:pct s) (:pct prev)) dt-hr)
                  fast' (ewma-step fast rate dt-hr tau-fast-hr)
                  slow' (ewma-step slow rate dt-hr tau-slow-hr)]
              (recur (rest xs) s fast' slow' (inc n))))))
      (when (and fast slow (>= n 2))
        {:fast fast :slow slow :last prev :samples n}))))

(defn project-end-of-window
  "Extrapolate the slow EWMA forward to the 7d reset moment.
   Returns the projected used_percentage at reset, clamped at 0.
   slow-rate is in %/hr; resets-at is unix epoch seconds."
  [now-epoch current-pct slow-rate resets-at]
  (let [hours-to-reset (max 0.0 (/ (- resets-at now-epoch) 3600.0))]
    (max 0.0 (+ current-pct (* slow-rate hours-to-reset)))))

(defn current-window
  "Data bundle for the /usage page: observed snapshots in the current
   7d rate-limit window, plus EWMA-derived projection and confidence
   band edges. Returns nil if there isn't enough data yet.

   {:observed     [{:ts :pct} ...]   ; snapshots within window, oldest first
    :resets-at    epoch-seconds      ; when the 7d window rolls
    :window-start epoch-seconds      ; resets-at - 7d
    :now          epoch-seconds
    :last-pct     latest used %
    :slow_x       slow EWMA / target pace
    :fast_x       fast EWMA / target pace
    :proj-pct     projected used % at reset (slow EWMA)
    :proj-lo      lower band (min of fast/slow projections)
    :proj-hi      upper band (max of fast/slow projections)
    :samples      count of usable transitions}"
  []
  (let [snaps (recent-snapshots window-size)]
    (when-let [latest (last snaps)]
      (let [resets-at    (:resets-at latest)
            window-start (- resets-at (* 7 86400))
            in-window    (filterv #(>= (:ts %) window-start) snaps)]
        (when-let [{:keys [fast slow last samples]} (fold-ewma in-window)]
          (let [now           (-> (Instant/now) .getEpochSecond)
                pct           (:pct last)
                proj-slow     (project-end-of-window now pct slow resets-at)
                proj-fast     (project-end-of-window now pct fast resets-at)]
            {:observed     (mapv #(select-keys % [:ts :pct]) in-window)
             :resets-at    resets-at
             :window-start window-start
             :now          now
             :last-pct     pct
             :slow_x       (/ slow target-pct-per-hr)
             :fast_x       (/ fast target-pct-per-hr)
             :proj-pct     proj-slow
             :proj-lo      (min proj-slow proj-fast)
             :proj-hi      (max proj-slow proj-fast)
             :samples      samples}))))))

(defn current-status
  "Compute the current EWMA status from cch's events DB. Returns a map
  shaped for JSON, or {} when there isn't enough data yet to be honest.

  {:slow_x        slow EWMA / target pace (e.g. 0.74)
   :fast_x        fast EWMA / target pace
   :arrow         '↗' '→' or '↘'
   :text          B-style field, e.g. 'pace: 0.7× ↘'
   :proj_pct      projected seven_day used % at reset
   :proj_text     C-style field, e.g. '7d→ 65%' or '7d→ 110% ⚠'
   :samples       count of usable transitions
   :seven_day_pct latest seven_day used %}"
  []
  (let [snaps (recent-snapshots window-size)]
    (if-let [{:keys [fast slow last samples]} (fold-ewma snaps)]
      (let [slow-x (/ slow target-pct-per-hr)
            fast-x (/ fast target-pct-per-hr)
            arr    (arrow-for fast-x slow-x)
            now    (-> (Instant/now) .getEpochSecond)
            proj   (project-end-of-window now (:pct last) slow (:resets-at last))
            warn   (if (> proj 100.0) " ⚠" "")]
        {:slow_x        slow-x
         :fast_x        fast-x
         :arrow         arr
         :text          (format "pace: %.1f× %s" slow-x arr)
         :proj_pct      proj
         :proj_text     (format "7d→ %.0f%%%s" proj warn)
         :samples       samples
         :seven_day_pct (:pct last)})
      {})))
