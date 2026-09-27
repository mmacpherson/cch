(ns cch.usage-ledger
  "Prospective forecast ledger.

  Retrospective backtests have been used to choose the model, so they no
  longer protect the final choice from selection bias. The ledger records
  forecasts as they are made, under frozen model ids, and scores them only
  after each window ends: evidence the model choices never saw.

  Once an hour per agent/window, each ledger model's forecast is stored as
  19 quantiles (levels 0.05..0.95) of demand at reset, plus P(cap). Scoring
  compares the quantiles, capped at 100, with the window's capped final meter
  reading: the observable outcome. Bands above 100 are not scoreable. The
  score is the 19-level quantile (pinball) score, a discrete approximation of
  CRPS, plus 50%/90% coverage and the P(cap) Brier score, under
  cch.usage-eval's estimand with calendar-week bootstrap intervals.

  Model ids are frozen: a change to a model's code or settings must use a new
  id, so every id's record describes one fixed forecaster."
  (:require [cch.db :as db]
            [cch.usage-eval :as ev]
            [cch.usage-model :as m]
            [cheshire.core :as json]
            [clojure.string :as str]
            [next.jdbc :as jdbc]))

(def models
  "Frozen ledger model ids."
  {"ml-harmonic-v1" "maximum-likelihood usage model, harmonic fleet profile (production since 85c953c)"
   "rate-bayes-v1" "rate-Bayes projection with duration-weighted mean (baseline, 010ad44)"
   "nb-weekly-v1" "count-process rung 6: negative binomial bursts with a per-window gamma pace, harmonic fleet profile, ML refit daily (challenger, 5b8be3c)"
   "onoff-v1" "count-process rung 7: rung 6 with on/off model blocks (days for 7d, hours for 5h), harmonic fleet profile, ML refit daily (challenger, e5d9454)"})

(def levels
  "Quantile levels stored for every forecast."
  (mapv #(/ % 20.0) (range 1 20)))

(defn gaussian-quantiles
  "Quantiles of a normal forecast given its median and a 90% band (5th-95th
  percentile, as the rate-Bayes projection reports it), floored at the
  current reading (usage never decreases within a window)."
  [median lo hi floor]
  (let [sd (max 1e-9 (/ (- hi lo) (* 2 1.6448536269514722)))
        ;; standard normal quantiles at the 19 fixed levels
        z (fn [p]
            (nth [-1.6448536269514722 -1.2815515655446004 -1.0364333894937898 -0.8416212335729143
                  -0.6744897501960817 -0.5244005127080407 -0.38532046640756773 -0.2533471031357997
                  -0.12566134685507402 0.0 0.12566134685507402 0.2533471031357997 0.38532046640756773
                  0.5244005127080407 0.6744897501960817 0.8416212335729143 1.0364333894937898
                  1.2815515655446004 1.6448536269514722]
                 (int (Math/round (dec (* p 20))))))]
    (mapv #(max floor (+ median (* sd (z %)))) levels)))

;; --- recording ---

(def ^:private last-recorded (atom {}))

(defn due?
  "True once per clock hour per [agent window-key] in this process (the table's
  unique key also prevents duplicates across restarts)."
  [k now]
  (let [hour (quot now 3600)]
    (when (not= hour (get @last-recorded k))
      (swap! last-recorded assoc k hour)
      true)))

(defn- datasource []
  (jdbc/get-datasource {:dbtype "sqlite" :dbname (db/db-path)}))

(defn record!
  "Store one forecast. `quantiles` are the 19 levels of demand at reset."
  [{:keys [model agent window-key resets-at now current-pct quantiles p-cap]}]
  (jdbc/execute! (datasource)
                 [(str "INSERT OR IGNORE INTO usage_forecast_ledger "
                       "(model, agent, window_key, resets_at, hour, recorded_at, current_pct, quantiles, p_cap) "
                       "VALUES (?,?,?,?,?,?,?,?,?)")
                  model agent ({:seven-day "seven_day" :five-hour "five_hour"} window-key)
                  resets-at (* 3600 (quot now 3600)) now current-pct
                  ;; percent, to 0.01
                  (json/generate-string (mapv #(/ (Math/round (* 100.0 (double %))) 100.0) quantiles))
                  p-cap]))

;; --- scoring ---

(defn quantile-score
  "Mean pinball loss over the levels, times 2: a discrete approximation of
  CRPS for the capped outcome y (quantiles are capped at 100 first)."
  [quantiles y]
  (* 2.0 (/ (reduce + (map (fn [tau q]
                             (let [q (min 100.0 q)]
                               (if (>= y q) (* tau (- y q)) (* (- 1.0 tau) (- q y)))))
                           levels quantiles))
            (count levels))))

(defn score-row
  "Score one ledger record against its window's capped final."
  [{:keys [quantiles p-cap]} final capped?]
  (let [q (fn [tau] (min 100.0 (nth quantiles (int (Math/round (dec (* tau 20)))))))
        in? (fn [lo hi] (if capped? (>= hi 99.0) (<= (- lo 1.0) final (+ hi 1.0))))]
    {:crps (quantile-score quantiles final)
     :cov50 (in? (q 0.25) (q 0.75))
     :cov90 (in? (q 0.05) (q 0.95))
     :brier (when p-cap (Math/pow (- p-cap (if capped? 1.0 0.0)) 2))}))

(defn- hourly-rows [agent wk]
  (mapv (fn [r] {:resets-at (long (:resets_at r)) :hour (long (:h r)) :pct (double (:pct r))})
        (db/query (format (str "SELECT resets_at, (observed_at/3600000)*3600 AS h, MAX(used_percentage) AS pct "
                               "FROM usage_observations WHERE agent='%s' AND window_key='%s' GROUP BY resets_at, h")
                          (str/replace agent "'" "''") wk))))

(defn scored-records
  "Ledger records whose windows have ended, scored:
  [{:model :agent :window-key :win :week :hours :crps :cov50 :cov90 :brier}]."
  [now]
  (let [recs (db/query "SELECT * FROM usage_forecast_ledger ORDER BY recorded_at")]
    (vec
      (for [[[agent wk] rs] (group-by (juxt :agent :window_key) recs)
            :let [window-key ({"seven_day" :seven-day "five_hour" :five-hour} wk)
                  spec (m/specs window-key)
                  wins (vec (m/windows (hourly-rows agent wk) spec))]
            r rs
            :let [[i w] (first (keep-indexed
                                 (fn [i w] (when (<= (Math/abs (- (:end w) (:resets_at r))) (:cluster-secs spec)) [i w]))
                                 wins))]
            :when (and w (or (:cap w) (< (:eff-end w) (- now 3600))))
            :let [final (m/cum-at w (inc (:eff-end w)))]]
        (merge {:model (:model r) :agent agent :window-key window-key
                :win i :week (quot (:start w) 604800)
                :hours (/ (- (:recorded_at r) (:start w)) 3600.0)
                :hour (:hour r)}
               (score-row {:quantiles (json/parse-string (:quantiles r)) :p-cap (:p_cap r)}
                          final (boolean (:cap w))))))))

(defn report
  "Print the ledger's scores per agent/window and model, and the paired
  difference of each model vs the baseline."
  [& {:keys [baseline] :or {baseline "rate-bayes-v1"}}]
  (let [now (quot (System/currentTimeMillis) 1000)
        rows (scored-records now)]
    (println (format "ledger: %d scored forecasts (%d recorded)"
                     (count rows) (or (:n (first (db/query "SELECT COUNT(*) AS n FROM usage_forecast_ledger"))) 0)))
    (doseq [[[agent wk] crs] (sort-by key (group-by (juxt :agent :window-key) rows))
            :let [by-model (group-by :model crs)
                  base-by-key (into {} (map (juxt (juxt :win :hour) identity) (get by-model baseline)))]]
      (println (format "\n%s %s  (%d windows)" agent (name wk) (count (distinct (map :win crs)))))
      (doseq [[model mrs] (sort-by key by-model)
              :let [lv (fn [f] (:est (ev/level mrs wk f)))
                    paired (filter #(base-by-key [(:win %) (:hour %)]) mrs)
                    d (when (and (not= model baseline) (seq paired))
                        (ev/paired paired (map #(base-by-key [(:win %) (:hour %)]) paired) wk :crps))]]
        (println (format "  %-16s score %6.2f  cov50 %3.0f%%  cov90 %3.0f%%%s"
                         model (lv :crps) (* 100 (lv #(if (:cov50 %) 1.0 0.0)))
                         (* 100 (lv #(if (:cov90 %) 1.0 0.0)))
                         (if d (format "  vs %s %+6.2f [%+.2f, %+.2f]" baseline (:delta d) (:lo d) (:hi d)) "")))))))
