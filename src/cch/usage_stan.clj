(ns cch.usage-stan
  "Bridge between cch.usage-model and the offline hierarchical Stan fit
  (resources/stan/usage_model.stan, run by bin/cch-usage-stan-fit).

  `fit-data` exports every agent/window cell's model blocks, built by the
  same code the in-JVM fit uses, so the Stan likelihood sees identical
  inputs. `read-draws` loads the job's posterior draws as per-cell
  {:theta :profile} fits that cch.usage-model/forecast can consume."
  (:require [cch.numeric]
            [cch.usage-model :as m]
            [cheshire.core :as json])
  (:import (java.time ZoneId)))

(def ^:private type-index {:seven-day 1 :five-hour 2})

(def ^:private profile-basis
  "Size of the Stan model's hour-of-week profile basis: daily harmonics
  (down to 4-hour detail), weekly harmonics (weekday drift), and daily
  harmonics that apply only on weekends."
  {:n_daily 6 :n_weekly 3 :n_weekend 2})

(defn- block-entries
  "Model blocks of an hour series with their hour-level entries:
  [{:y usage :entries [[bin live hourly-usage] ...]} ...]."
  [series zone spec]
  (reduce (fn [acc [h [live y]]]
            (let [entry [(inc (m/hour-of-week zone h)) live y]]
              (if (or (empty? acc) (m/block-start? zone spec h))
                (conj acc {:y y :entries [entry]})
                (update acc (dec (count acc))
                        #(-> % (update :y + y) (update :entries conj entry))))))
          [] series))

(defn fit-data
  "Stan data for all cells at time `now`. `rows-by-cell` maps
  [agent window-key] -> hourly aggregate rows observed before `now`. Cells
  with fewer than `min-windows` completed windows, or no usage in the fit
  span, are left out. Returns
  {:data <stan json map> :cells [[agent window-key] ...]}."
  [rows-by-cell now ^ZoneId zone & {:keys [min-windows] :or {min-windows 5}}]
  (let [series-of (fn [[_ window-key] rows]
                    (let [spec (m/specs window-key)
                          s (m/hour-series (m/windows rows spec) now)
                          lookback (:fit-lookback-secs spec)]
                      (if lookback (into (sorted-map) (filter #(>= (key %) (- now lookback)) s)) s)))
        cells (->> rows-by-cell
                   (filter (fn [[[_ wk] rows]]
                             (>= (count (filter #(< (:eff-end %) now) (m/windows rows (m/specs wk))))
                                 min-windows)))
                   (sort-by key))
        per-cell (->> cells
                      (map (fn [[cell rows]]
                             (let [s (series-of cell rows)]
                               {:cell cell :series s
                                :blocks (block-entries s zone (m/specs (second cell)))})))
                      ;; A cell with no usage in its fit span (e.g. a provider that
                      ;; stopped reporting a window) only adds cost.
                      (filter #(pos? (reduce + (map :y (:blocks %)))))
                      vec)
        agents (vec (distinct (map (comp first :cell) per-cell)))
        fleet (m/smooth-profile (m/pooled-rates (map #(m/bin-stats (:series %) zone) per-cell)))
        blocks (mapcat :blocks per-cell)
        entries (mapcat :entries blocks)
        starts (reductions + 1 (map #(count (:blocks %)) per-cell))
        ptr (vec (reductions + 1 (map #(count (:entries %)) blocks)))]
    {:cells (mapv :cell per-cell)
     :data (merge profile-basis
                  {:C (count per-cell)
                   :T 2
                   :ctype (mapv #(type-index (second (:cell %))) per-cell)
                   :A (count agents)
                   :agent (mapv #(inc (.indexOf ^java.util.List agents (first (:cell %)))) per-cell)
                   :x0 [(get-in m/specs [:seven-day :x0]) (get-in m/specs [:five-hour :x0])]
                   :NB (count blocks)
                   :y (mapv :y blocks)
                   :cell_start (vec (butlast starts))
                   :cell_len (mapv #(count (:blocks %)) per-cell)
                   :NW (count entries)
                   :block_ptr ptr
                   :hbin (mapv first entries)
                   :live (mapv second entries)
                   :yh (mapv #(nth % 2) entries)
                   :min_mass (mapv #(:min-mass (m/specs (second (:cell %)))) per-cell)
                   :g_init (mapv #(Math/log %) fleet)})}))

(defn write-fit-data
  "Write Stan data JSON; returns the cells in Stan index order."
  [path rows-by-cell now zone]
  (let [{:keys [data cells]} (fit-data rows-by-cell now zone)]
    (spit path (json/generate-string data))
    cells))

(defn read-draws
  "Posterior draws from bin/cch-usage-stan-fit output, as
  {cell [{:theta [...] :profile [...]} ...]} for the given cell order."
  [path cells now]
  (let [{:keys [draws]} (json/parse-string (slurp path) true)]
    (into {}
          (map-indexed
            (fn [i cell]
              [cell (mapv (fn [{:keys [theta prof]}]
                            {:theta (m/unpack (nth theta i))
                             :profile (vec (nth prof i))
                             :fitted-at now})
                          draws)])
            cells))))

(defn mixture-forecast
  "Posterior predictive from Stan draws: run the filter and simulation once
  per parameter draw (`per-draw` paths each) and pool the paths. Returns the
  same summary shape as cch.usage-model/predict."
  [fits rows spec zone now resets-at x & {:keys [per-draw] :or {per-draw 100}}]
  (let [pooled (double-array
                 (mapcat (fn [i fit]
                           (seq (:draws (m/forecast fit rows spec zone now resets-at x
                                                    :path? false :n per-draw :seed (inc i)))))
                         (range) fits))
        n (alength pooled)
        q (fn [p] (cch.numeric/quantile pooled p))]
    (java.util.Arrays/sort pooled)
    {:median (q 0.5) :lo (q 0.05) :q25 (q 0.25) :q75 (q 0.75) :hi (q 0.95)
     :p-cap (/ (double (count (filter #(>= % 100.0) pooled))) n)
     :draws pooled}))
