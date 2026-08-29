(ns cch.usage-observation-test
  (:require [cch.usage-observation :as usage]
            [cheshire.core :as json]
            [clojure.test :refer [deftest is testing]]))

(def synthetic-payload
  {:session_id "synthetic-session"
   :account {:email "example@example.invalid"}
   :model {:id "synthetic-model"}
   :rate_limits
   {:five_hour {:used_percentage 6
                :resets_at 1779930704}
    :seven_day {:used_percentage 21.5
                :resets_at 1780174277}}})

(deftest derives-narrow-canonical-observations
  (let [observations (usage/from-snapshot
                       {:agent "codex"
                        :observed-at "2026-05-27T19:00:00Z"
                        :payload synthetic-payload})]
    (is (= ["five_hour" "seven_day"] (mapv :window observations)))
    (is (= [6.0 21.5] (mapv :used-percentage observations)))
    (is (= [1779930704 1780174277] (mapv :resets-at observations)))
    (is (every? #(= 1 (:schema-version %)) observations))
    (is (every? #(re-matches #"[0-9a-f]{64}" (:event-id %)) observations))
    (is (= #{:schema-version :event-id :observed-at :agent :window
             :used-percentage :resets-at}
           (set (keys (first observations))))
        "raw payload and identifying source fields never enter the normalized shape")
    (is (not-any? #(some #{"synthetic-session" "example@example.invalid"
                           "synthetic-model"}
                         (vals %))
                  observations))))

(deftest accepts-json-and-string-keyed-payloads
  (let [input {:agent "claude-code"
               :observed-at 1780000000123}
        from-json (usage/from-snapshot
                    (assoc input :payload (json/generate-string synthetic-payload)))
        from-map  (usage/from-snapshot (assoc input :payload synthetic-payload))]
    (is (= from-map from-json))))

(deftest event-ids-are-deterministic-and-semantic
  (let [base {:agent "agy"
              :observed-at 1780000000123
              :payload {:rate_limits
                        {:five_hour {:used_percentage 6
                                     :resets_at 1780001000}}}}
        first-id (:event-id (first (usage/from-snapshot base)))
        repeat-id (:event-id (first (usage/from-snapshot
                                     (assoc-in base
                                               [:payload :rate_limits :five_hour
                                                :used_percentage]
                                               6.0))))
        later-id (:event-id (first (usage/from-snapshot
                                    (update base :observed-at inc))))]
    (is (= first-id repeat-id) "equivalent numeric forms deduplicate")
    (is (not= first-id later-id) "a later observation remains a distinct event")))

(deftest invalid-input-is-dropped-per-window
  (testing "one bad window does not poison another valid window"
    (let [observations
          (usage/from-snapshot
            {:agent "claude-code"
             :observed-at 1780000000123
             :payload {:rate_limits
                       {:five_hour {:used_percentage 101
                                    :resets_at 1780001000}
                        :seven_day {:used_percentage 0
                                    :resets_at 1780100000}}}})]
      (is (= ["seven_day"] (mapv :window observations)))
      (is (= 0.0 (:used-percentage (first observations))))))
  (testing "malformed envelopes produce no observation"
    (is (= [] (usage/from-snapshot {:agent "Not a safe label"
                                    :observed-at 1780000000123
                                    :payload synthetic-payload})))
    (is (= [] (usage/from-snapshot {:agent "codex"
                                    :observed-at -1
                                    :payload synthetic-payload})))
    (is (= [] (usage/from-snapshot {:agent "codex"
                                    :observed-at 1780000000123
                                    :payload "not json"})))))
