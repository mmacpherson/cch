(ns cch.ewma
  "Statusline data layer.

  Pulls 5h and 7d rate-limit observations from context_snapshots and
  hands them to cch.projections for Bayesian forecasts. The /ewma
  endpoint serves what statusline-command.sh consumes:

      5h: 60% · ~67% · 22m
      7d: 36% · ~94% · 4d 8h

  This namespace owns the SQL and the per-window prior; cch.projections
  owns the math."
  (:require [babashka.process :as p]
            [cheshire.core :as json]
            [cch.log :as log]
            [cch.projections :as proj]
            [clojure.string :as str])
  (:import (java.time Instant)))

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

(def ^:private window-json-key
  "Mapping from a window keyword to its key in the rate_limits payload."
  {:seven-day :seven_day
   :five-hour :five_hour})

(defn- extract-window
  "Pull {:ts :pct :resets-at} from a parsed JSON row for a given window."
  [parsed-row window-key]
  (let [p  (json/parse-string (:payload parsed-row) true)
        sd (get-in p [:rate_limits (window-json-key window-key)])]
    (when (and sd (:used_percentage sd) (:resets_at sd))
      {:ts        (iso->epoch-seconds (:ts parsed-row))
       :pct       (double (:used_percentage sd))
       :resets-at (long (:resets_at sd))})))

(defn- snapshots-since
  "Pull rate-limit samples newer than `since-iso` for the given window
   (`:seven-day` or `:five-hour`). Querying by timestamp keeps the full
   window's history regardless of write density."
  ([since-iso] (snapshots-since since-iso :seven-day))
  ([since-iso window-key]
   (let [path (log/db-path)
         sql  (format
                (str "SELECT json_object('ts', timestamp, 'payload', payload) "
                     "FROM context_snapshots "
                     "WHERE timestamp >= '%s' "
                     "AND payload LIKE '%%rate_limits%%' "
                     "AND session_id NOT LIKE 'test%%' "
                     "ORDER BY timestamp ASC;")
                since-iso)
         result (p/sh ["sqlite3" path sql])]
     (when (zero? (:exit result))
       (->> (str/split-lines (str/trim (:out result)))
            (remove str/blank?)
            (keep (fn [line]
                    (try
                      (extract-window (json/parse-string line true) window-key)
                      (catch Exception _ nil)))))))))

(defn- latest-snapshot
  "The single most-recent rate-limits snapshot for the given window."
  ([] (latest-snapshot :seven-day))
  ([window-key]
   (let [path (log/db-path)
         sql  (str "SELECT json_object('ts', timestamp, 'payload', payload) "
                   "FROM context_snapshots "
                   "WHERE payload LIKE '%rate_limits%' "
                   "AND session_id NOT LIKE 'test%' "
                   "ORDER BY id DESC LIMIT 1;")
         result (p/sh ["sqlite3" path sql])]
     (when (zero? (:exit result))
       (when-let [line (-> result :out str/trim not-empty)]
         (try
           (extract-window (json/parse-string line true) window-key)
           (catch Exception _ nil)))))))

(defn- epoch->iso [secs]
  (str (java.time.Instant/ofEpochSecond secs)))

(defn current-window
  "Data bundle for the /usage page: observed snapshots in the current
   7d rate-limit window plus the full set of forward projections.
   Returns nil if there isn't enough data yet."
  []
  (when-let [latest (latest-snapshot)]
    (let [resets-at    (:resets-at latest)
          window-start (- resets-at (* 7 86400))
          raw          (vec (snapshots-since (epoch->iso window-start)))
          ;; Thin bursty regions to one sample per 15 minutes. statusLine
          ;; can fire 100+ times in a minute during active use; OLS would
          ;; otherwise count them all as independent observations.
          in-window    (proj/thin-by-time raw 900)
          now          (-> (Instant/now) .getEpochSecond)
          last-pct     (:pct (last in-window))
          window-info  {:now now :resets-at resets-at
                        :window-start window-start :last-pct last-pct}
          obs-pairs    (mapv #(select-keys % [:ts :pct]) in-window)
          projs        (proj/all-projections obs-pairs window-info)]
      (when (and last-pct (seq projs))
        {:observed     obs-pairs
         :resets-at    resets-at
         :window-start window-start
         :now          now
         :last-pct     last-pct
         :samples      (count obs-pairs)
         :projections  projs}))))

(def ^:private window-config
  "Per-window span and Bayesian prior. 7d prior reflects the user's
   stated baseline (typically 80-95% end-of-week → mean rate ≈ 0.55,
   tight σ on the across-week variability). 5h prior is the linear
   target with wide σ — 5h windows are too variable for a tight
   informative prior."
  {:seven-day {:span-secs (* 7 86400) :prior-mu 0.55 :prior-sigma 0.045}
   :five-hour {:span-secs (* 5 3600)  :prior-mu 15.0 :prior-sigma 8.0}})

(defn- compute-bayes-stats
  "Run the Bayesian projection for one window. Returns
   {:current_pct :projected_pct :secs_left} or nil if no data yet."
  [window-key]
  (when-let [latest (latest-snapshot window-key)]
    (let [{:keys [span-secs prior-mu prior-sigma]} (window-config window-key)
          resets-at    (:resets-at latest)
          window-start (- resets-at span-secs)
          raw          (vec (snapshots-since (epoch->iso window-start) window-key))
          ;; Bucket size: 15min for 7d, 1min for 5h (5h has finer time
          ;; resolution to play with — don't over-thin).
          bucket       (if (= window-key :seven-day) 900 60)
          in-window    (proj/thin-by-time raw bucket)
          now          (-> (Instant/now) .getEpochSecond)
          last-pct     (:pct (last in-window))
          window-info  {:now now :resets-at resets-at
                        :window-start window-start :last-pct last-pct
                        :prior-mu prior-mu :prior-sigma prior-sigma}
          obs-pairs    (mapv #(select-keys % [:ts :pct]) in-window)
          proj-result  (when last-pct (proj/bayes-projection obs-pairs window-info))]
      (when last-pct
        {:current_pct   last-pct
         :projected_pct (or (:proj proj-result) last-pct)
         :secs_left     (max 0 (- resets-at now))}))))

(defn statusline-stats
  "Bundle for the statusLine: current pct, Bayesian projection at reset,
   and time-to-reset for both windows. Either may be nil if no rate-limit
   data is present yet for that window."
  []
  {:five_hour (compute-bayes-stats :five-hour)
   :seven_day (compute-bayes-stats :seven-day)})
