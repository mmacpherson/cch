(ns cch.control.usage-forecast
  "Hosted projections over the broker's bounded normalized usage read model.

  Uses the gamma-process usage model (cch.usage-model) once an agent/window
  has enough completed windows in the read model's hourly history, and the
  rate-Bayes projection before that. Fits are lazy: the first projection
  that needs one fits it, and it is reused for 24 hours per agent/window.
  Hour-of-week profiles use the JVM's default zone, the same zone the Usage
  page labels its axes in; set TZ on the broker service to the operator's
  local zone."
  (:require [cch.control.usage-read-model :as read-model]
            [cch.forecast :as forecast]
            [cch.projections :as projections]
            [cch.usage-model :as model])
  (:import (java.time ZoneId)))

(def ^:private window-keywords
  {"five_hour" :five-hour
   "seven_day" :seven-day})

(defn- round-one [value]
  (Double/parseDouble (format "%.1f" (double value))))

(def ^:private min-model-windows
  "Completed windows needed before the model replaces the rate projection."
  5)

(def ^:private refit-secs 86400)

(def ^:private model-cache (atom {}))

(defn- model-projection
  "Gamma-process forecast from the read model's hourly history, shaped like
  the rate projection ({:proj :band}), or nil with too little history."
  [agent window-key hourly now resets-at last-pct]
  (let [spec (model/specs window-key)
        completed (count (filter #(< (:eff-end %) now) (model/windows hourly spec)))]
    (when (>= completed min-model-windows)
      (let [zone (ZoneId/systemDefault)
            k [agent window-key]
            cached (get @model-cache k)
            fitted (if (and cached (< (- now (:fitted-at cached)) refit-secs))
                     cached
                     (let [fit (model/fit-model hourly spec zone now :x0 (:x cached))]
                       (swap! model-cache assoc k fit)
                       fit))
            {:keys [median lo hi p-cap path]}
            (model/forecast fitted hourly spec zone now resets-at last-pct)]
        {:method :gamma-process
         :name "Gamma process"
         :proj median
         :band {:lo lo :hi hi}
         :p-cap p-cap
         :path path}))))

(defn project-window
  [generated-at agent window input]
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
            projection (or (when (seq (:hourly input))
                             (model-projection agent window-key (:hourly input)
                                               now resets-at last-pct))
                           (projections/rate-bayes-projection observed window-info))
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
                        :hi (Math/round (double (:hi band)))})
          (:p-cap projection)
          (assoc :p-cap (:p-cap projection)))))))

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
                                        (project-window generated-at agent window input)]
                               [window projected])))
                     (into (sorted-map)))]))
        (into (sorted-map)))})
