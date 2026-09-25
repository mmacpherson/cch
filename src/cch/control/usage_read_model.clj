(ns cch.control.usage-read-model
  "Bounded, privacy-safe forecast inputs derived from normalized observations.

  The result contains only agent/window kinds, reset times, percentage samples,
  aggregate counts, completed-window finals, and hourly percentage maxima. It is an internal web read
  model, not a runner API, and never carries source runner or machine identity."
  (:require [clojure.string :as str]))

(def window-settings
  {"five_hour" {:span-seconds (* 5 60 60) :bucket-seconds 60}
   "seven_day" {:span-seconds (* 7 24 60 60) :bucket-seconds 360}})

(defn- latest-reset [observations]
  (some->> observations
           (filter #(pos? (double (:used-percentage %))))
           (sort-by (juxt :observed-at #(or (:cursor %) 0)))
           last
           :resets-at))

(defn- monotone-samples [observations bucket-seconds]
  (let [bucket-ms (* 1000 bucket-seconds)]
    (->> observations
         (sort-by (juxt :observed-at #(or (:cursor %) 0)))
         (reduce (fn [{:keys [maximum samples]} observation]
                   (let [pct (double (:used-percentage observation))]
                     (if (or (nil? maximum) (>= pct maximum))
                       {:maximum (max (or maximum pct) pct)
                        :samples (conj samples observation)}
                       {:maximum maximum :samples samples})))
                 {:maximum nil :samples []})
         :samples
         (reduce (fn [{:keys [seen samples]} observation]
                   (let [bucket (quot (:observed-at observation) bucket-ms)]
                     (if (contains? seen bucket)
                       {:seen seen :samples samples}
                       {:seen (conj seen bucket)
                        :samples (conj samples
                                       {:observed-at (:observed-at observation)
                                        :used-percentage
                                        (double (:used-percentage observation))})})))
                 {:seen #{} :samples []})
         :samples)))

(defn- historical-finals [observations now-seconds]
  (->> observations
       (filter #(< (:resets-at %) now-seconds))
       (group-by :resets-at)
       (map (fn [[reset rows]]
              {:reset reset
               :final (reduce max (map #(double (:used-percentage %)) rows))}))
       (sort-by :reset >)
       (take 12)
       (keep (fn [{:keys [final]}] (when (>= final 10.0) final)))
       vec))

(defn- hourly-maxima
  "Hourly max percentage per reset across the retained history — the usage
  model's input. Hour-level aggregates only: no per-observation timing and
  no source identity."
  [observations]
  (->> observations
       (reduce (fn [m {:keys [observed-at resets-at used-percentage]}]
                 (update m [resets-at (* 3600 (quot observed-at 3600000))]
                         (fnil max 0.0) (double used-percentage)))
               {})
       (map (fn [[[reset hour] pct]] {:resets-at reset :hour hour :pct pct}))
       (sort-by (juxt :hour :resets-at))
       vec))

(defn pair-input
  "Build one bounded window input from already normalized observations."
  [observations now-ms window-key]
  (let [{:keys [span-seconds bucket-seconds]} (get window-settings window-key)
        reset (latest-reset observations)]
    (when (and span-seconds reset)
      (let [window-start-ms (* 1000 (- reset span-seconds))
            current (->> observations
                         (filter #(and (= reset (:resets-at %))
                                       (>= (:observed-at %) window-start-ms)))
                         vec)]
        {:resets-at reset
         :sample-count (count current)
         :samples (monotone-samples current bucket-seconds)
         :historical-finals (historical-finals observations
                                                (quot now-ms 1000))
         :hourly (hourly-maxima observations)}))))

(defn from-observations
  "Build the complete hosted read model from retained normalized rows."
  [observations now-ms]
  (let [safe (filter #(and (contains? window-settings (:window %))
                           (string? (:agent %))
                           (not (str/blank? (:agent %))))
                     observations)]
    {:generated-at now-ms
     :agents
     (->> safe
          (group-by :agent)
          (sort-by key)
          (map (fn [[agent agent-rows]]
                 [agent
                  (->> window-settings
                       keys
                       sort
                       (keep (fn [window-key]
                               (when-let [input
                                          (pair-input
                                            (filter #(= window-key (:window %))
                                                    agent-rows)
                                            now-ms window-key)]
                                 [window-key input])))
                       (into (sorted-map)))]))
          (into (sorted-map)))}))
