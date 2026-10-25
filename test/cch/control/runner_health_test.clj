(ns cch.control.runner-health-test
  (:require [babashka.fs :as fs]
            [cch.control.runner-health :as health]
            [clojure.test :refer [deftest is]])
  (:import [java.time Instant]))

(def ^:private t0 (Instant/parse "2026-01-01T00:00:00Z"))

(defn- at [seconds] (.plusSeconds t0 seconds))

(defn- failure [message]
  {:type "broker-unavailable" :message message})

(deftest failures-accumulate-from-the-first-and-success-clears-them
  (let [h (-> (health/initial "runner-a" t0)
              (health/tick-started (at 1))
              (health/record-failure (failure "one") (at 1))
              (health/record-failure (failure "two") (at 2)))]
    (is (= 2 (:consecutive-failures h)))
    (is (= (at 1) (:failing-since h)) "the episode start is kept")
    (is (= "two" (get-in h [:last-error :message])))
    (let [ok (health/record-success h (at 3))]
      (is (= 0 (:consecutive-failures ok)))
      (is (= (at 3) (:last-success-at ok)))
      (is (not-any? ok [:failing-since :last-error :tick-started-at])))))

(deftest assess-flags-a-stalled-tick-and-a-sustained-outage
  (let [opts {:stall-ms 10000 :max-failure-ms 60000}
        base (health/initial "runner-a" t0)]
    (is (nil? (health/assess (health/tick-started base (at 0)) (at 5) opts)))
    (is (= :stalled
           (:reason (health/assess (health/tick-started base (at 0)) (at 11) opts))))
    (let [failing (health/record-failure base (failure "x") (at 0))]
      (is (nil? (health/assess failing (at 30) opts))
          "a short outage is tolerated")
      (is (= :sustained-failure
             (:reason (health/assess failing (at 61) opts)))))))

(deftest status-round-trips-and-classifies
  (let [path (str (fs/create-temp-dir {:prefix "cch-runner-status-"})
                  "/status.json")
        alive (constantly true)
        failing (-> (health/initial "runner-a" t0)
                    (health/record-success (at 1))
                    (health/record-failure (failure "x") (at 2)))]
    (health/write! path failing (at 3))
    (let [status (health/read-status path)]
      (is (= (at 2) (:failing-since status)))
      (is (= {:status :failing :since (at 2) :consecutive-failures 1
              :last-success-at (at 1) :error-type "broker-unavailable"
              :cause-class nil}
             (health/classify status (at 4) {:alive? alive})))
      (is (= :stopped (:status (health/classify status (at 120) {:alive? alive})))
          "an unrefreshed file means the watchdog is gone")
      (is (= :stopped (:status (health/classify status (at 4)
                                                {:alive? (constantly false)})))))
    (is (= :unknown (:status (health/classify (health/read-status (str path ".absent"))
                                              (at 4)))))
    (let [stuck (-> (health/initial "runner-a" t0)
                    (health/record-success (at 1))
                    (health/tick-started (at 2)))]
      (health/write! path stuck (at 200))
      (is (= :stalled (:status (health/classify (health/read-status path) (at 201)
                                                {:alive? alive :stall-ms 1000})))))))
