(ns cch.stats
  "Aggregate SQL queries for dashboard analytics."
  (:require [cch.db :as db]
            [honey.sql :as sql]))

(defn decision-counts
  "Count events grouped by decision, applying the same filters as query-events.
  Returns a map like {\"allow\" 7, \"ask\" 1, \"deny\" 2, nil 14}."
  [& {:keys [hook event session since cwd-prefix]}]
  (let [q (cond-> {:select   [[:%count.* :n]
                               :decision]
                    :from     [:events]
                    :group-by [:decision]}
            hook       (update :where (fnil conj [:and]) [:= :hook-name hook])
            event      (update :where (fnil conj [:and]) [:= :event-type event])
            session    (update :where (fnil conj [:and]) [:= :session-id session])
            since      (update :where (fnil conj [:and]) [:> :timestamp since])
            cwd-prefix (update :where (fnil conj [:and]) [:like :cwd (str cwd-prefix "%")]))]
    (->> (db/query (first (sql/format q {:inline true})))
         (reduce (fn [m {:keys [decision n]}]
                   (assoc m (or decision :observe) n))
                 {}))))

(defn event-count-today
  "Total events since midnight (local time)."
  []
  (-> (db/query "SELECT COUNT(*) AS n FROM events WHERE timestamp >= date('now', 'localtime')")
      first :n))

(defn deny-count-today
  "Deny events since midnight."
  []
  (-> (db/query "SELECT COUNT(*) AS n FROM events WHERE decision = 'deny' AND timestamp >= date('now', 'localtime')")
      first :n))

(defn avg-dispatch-latency
  "Average elapsed_ms for events with timing data, within a time range."
  [& {:keys [since] :or {since "date('now', 'localtime')"}}]
  (-> (db/query (str "SELECT AVG(elapsed_ms) AS avg_ms, COUNT(*) AS n FROM events WHERE elapsed_ms IS NOT NULL AND timestamp >= " since))
      first))

(defn hourly-activity-24h
  "Per-hour event counts for the last 24 hours. Returns [{:hour \"HH\" :n count} ...]."
  []
  (db/query "SELECT strftime('%H', timestamp, 'localtime') AS hour, COUNT(*) AS n FROM events WHERE timestamp >= datetime('now', '-24 hours') GROUP BY hour ORDER BY hour"))

(defn top-hooks
  "Top hooks by fire count within a time range. Returns [{:hook_name :fires :denies} ...]."
  [& {:keys [since limit] :or {since "datetime('now', '-7 days')" limit 8}}]
  (db/query (format "SELECT hook_name, COUNT(*) AS fires, SUM(CASE WHEN decision = 'deny' THEN 1 ELSE 0 END) AS denies FROM events WHERE timestamp >= %s GROUP BY hook_name ORDER BY fires DESC LIMIT %d"
                    since limit)))

(defn hook-fire-counts
  "Fire count per hook (all time). Returns {\"event-log\" 4821, ...}."
  []
  (->> (db/query "SELECT hook_name, COUNT(*) AS n FROM events GROUP BY hook_name")
       (reduce (fn [m {:keys [hook_name n]}] (assoc m hook_name n)) {})))
