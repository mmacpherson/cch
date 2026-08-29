(ns cch.usage-observation
  "Privacy-safe, provider-neutral usage observations.

  This is the narrow durable/wire shape used for cross-node forecasts. It is
  deliberately derived from — rather than a copy of — provider status payloads:
  no session, account, machine, repository, model, or raw payload identity is
  retained."
  (:require [cheshire.core :as json]
            [clojure.string :as str])
  (:import [java.math BigDecimal]
           [java.nio.charset StandardCharsets]
           [java.security MessageDigest]
           [java.time Instant LocalDateTime ZoneOffset]))

(def schema-version 1)

(def ^:private windows
  ["five_hour" "seven_day"])

(def ^:private observation-keys
  #{:schema-version :event-id :observed-at :agent :window
    :used-percentage :resets-at})

(defn- finite-number?
  [value]
  (and (number? value)
       (Double/isFinite (double value))))

(defn- epoch-millis
  [value]
  (try
    (cond
      (and (finite-number? value)
           (pos? (double value))
           (== (double value) (Math/floor (double value))))
      (long value)

      (and (string? value) (not (str/blank? value)))
      ;; SQLite's strftime default is an ISO local timestamp in UTC without a
      ;; zone suffix. New captures use epoch millis, but the compatibility
      ;; importer must understand both that historical form and strict
      ;; ISO-8601 instants.
      (try
        (.toEpochMilli (Instant/parse value))
        (catch Exception _
          (-> (LocalDateTime/parse value)
              (.toInstant ZoneOffset/UTC)
              .toEpochMilli)))

      :else nil)
    (catch Exception _ nil)))

(defn- percentage
  [value]
  (when (and (finite-number? value)
             (<= 0.0 (double value) 100.0))
    (double value)))

(defn- epoch-seconds
  [value]
  (when (and (finite-number? value)
             (pos? (double value))
             (== (double value) (Math/floor (double value))))
    (long value)))

(defn- agent-name
  [value]
  (let [candidate (cond
                    (keyword? value) (name value)
                    (string? value) value
                    :else nil)]
    (when (and candidate
               (re-matches #"[a-z][a-z0-9._-]{0,63}" candidate))
      candidate)))

(defn- canonical-decimal
  [value]
  (-> (BigDecimal/valueOf (double value))
      .stripTrailingZeros
      .toPlainString))

(defn- sha256
  [value]
  (let [digest (.digest (MessageDigest/getInstance "SHA-256")
                        (.getBytes ^String value StandardCharsets/UTF_8))]
    (apply str (map #(format "%02x" (bit-and (int %) 0xff)) digest))))

(defn- event-id
  [{:keys [observed-at agent window used-percentage resets-at]}]
  (sha256
    (str schema-version "\n"
         observed-at "\n"
         agent "\n"
         window "\n"
         (canonical-decimal used-percentage) "\n"
         resets-at)))

(defn validate-observation!
  "Validate and canonicalize one normalized observation received at a trust
  boundary. Extra fields are rejected so raw provider or machine-local data
  cannot hitch a ride. The supplied event id must match the semantic content."
  [value]
  (when-not (and (map? value) (= observation-keys (set (keys value))))
    (throw (ex-info "Usage observation has an invalid shape"
                    {:type :invalid-usage-observation})))
  (let [candidate {:schema-version (when (= schema-version (:schema-version value))
                                     schema-version)
                   :observed-at (epoch-millis (:observed-at value))
                   :agent (agent-name (:agent value))
                   :window (when (some #{(:window value)} windows)
                             (:window value))
                   :used-percentage (percentage (:used-percentage value))
                   :resets-at (epoch-seconds (:resets-at value))}
        expected-id (when (every? some? (vals candidate))
                      (event-id candidate))]
    (when-not (and expected-id (= expected-id (:event-id value)))
      (throw (ex-info "Usage observation is invalid"
                      {:type :invalid-usage-observation})))
    (assoc candidate :event-id expected-id)))

(defn- payload-map
  [payload]
  (cond
    (map? payload) payload
    (string? payload) (try
                        (json/parse-string payload true)
                        (catch Exception _ nil))
    :else nil))

(defn from-snapshot
  "Derive zero to two validated usage observations from a status snapshot.

  Input keys are `:agent`, `:observed-at` (epoch millis or ISO-8601), and
  `:payload` (a map or JSON string with canonical `rate_limits`). Invalid
  payloads and individual windows are ignored. Returned maps contain only the
  public-safe semantic fields needed for forecasting."
  [{:keys [agent observed-at payload]}]
  (let [agent       (agent-name agent)
        observed-at (epoch-millis observed-at)
        payload     (payload-map payload)]
    (if (and agent observed-at payload)
      (->> windows
           (keep (fn [window]
                   (let [limit (or (get-in payload [:rate_limits (keyword window)])
                                   (get-in payload ["rate_limits" window]))
                         pct   (percentage (or (:used_percentage limit)
                                               (get limit "used_percentage")))
                         reset (epoch-seconds (or (:resets_at limit)
                                                  (get limit "resets_at")))]
                     (when (and pct reset)
                       (let [observation {:schema-version schema-version
                                          :observed-at observed-at
                                          :agent agent
                                          :window window
                                          :used-percentage pct
                                          :resets-at reset}]
                         (assoc observation :event-id (event-id observation)))))))
           vec)
      [])))
