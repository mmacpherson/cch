(ns cch.control.usage-forecast
  "Hosted projections over the broker's bounded normalized usage read model."
  (:require [cch.control.usage-read-model :as read-model]
            [cch.forecast :as forecast]
            [cch.projections :as projections]))

(def ^:private window-keywords
  {"five_hour" :five-hour
   "seven_day" :seven-day})

(defn- round-one [value]
  (Double/parseDouble (format "%.1f" (double value))))

(defn project-window
  [generated-at window input]
  (let [window-key (window-keywords window)
        {:keys [span-seconds]}
        (get read-model/window-settings window)
        now (quot generated-at 1000)
        raw-reset (:resets-at input)
        resets-at (if (<= raw-reset now)
                    (+ raw-reset span-seconds)
                    raw-reset)
        samples (if (= raw-reset resets-at) (:samples input) [])
        observed (mapv (fn [{:keys [observed-at used-percentage]}]
                         {:ts (quot observed-at 1000)
                          :pct (double used-percentage)})
                       samples)
        last-pct (:pct (last observed))
        finals (:historical-finals input)
        {:keys [prior-mu prior-sigma]}
        (forecast/prior-params window-key finals)]
    (when last-pct
      (let [window-info {:now now
                         :resets-at resets-at
                         :window-start (- resets-at span-seconds)
                         :last-pct last-pct
                         :prior-mu prior-mu
                         :prior-sigma prior-sigma
                         :historical-finals finals}
            projection (projections/rate-bayes-projection observed window-info)
            projected (or (:proj projection) last-pct)
            band (:band projection)
            rates (projections/rate-samples observed)
            recent-rate (when (>= (count rates) 2)
                          (let [recent (take-last 3 rates)]
                            (/ (reduce + 0.0 (map :rate recent))
                               (count recent))))
            page-data {:agent nil
                       :window-key window-key
                       :span-secs span-seconds
                       :observed observed
                       :rate-samples
                       (mapv (fn [{:keys [ts pct]}]
                               {:ts ts :pct pct :resets-at resets-at})
                             observed)
                       :rate-scale 1.0
                       :resets-at resets-at
                       :window-start (- resets-at span-seconds)
                       :now now
                       :last-pct last-pct
                       :samples (:sample-count input)
                       :projection projection
                       :rate-phr recent-rate}]
        (cond->
          {:current-pct (Math/round last-pct)
           :projected-pct (round-one projected)
           :resets-at resets-at
           :seconds-left (max 0 (- resets-at now))
           :sample-count (:sample-count input)
           :prior {:mu prior-mu :sigma prior-sigma}
           :page-data page-data}
          band
          (assoc :band {:lo (Math/round (double (:lo band)))
                        :hi (Math/round (double (:hi band)))}))))))

(defn from-read-model
  "Project every agent/window in the internal broker read model."
  [{:keys [generated-at agents]}]
  {:generated-at generated-at
   :agents
   (->> agents
        (map (fn [[agent windows]]
               [agent
                (->> windows
                     (keep (fn [[window input]]
                             (when-let [projected
                                        (project-window generated-at window input)]
                               [window projected])))
                     (into (sorted-map)))]))
        (into (sorted-map)))})
