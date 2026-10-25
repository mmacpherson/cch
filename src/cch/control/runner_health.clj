(ns cch.control.runner-health
  "Liveness and broker-exchange health for the paired runner.

  The runner folds each tick outcome into a small health map and its watchdog
  persists that map to an owner-local status file. `cch control doctor` and
  the send path read it back, so a runner that is failing every tick, or is
  wedged inside one, is distinguishable from a healthy one. The file holds no
  tokens, routes, or message content."
  (:require [babashka.fs :as fs]
            [cheshire.core :as json])
  (:import [java.lang ProcessHandle]
           [java.time Duration Instant]))

(def ^:const path-property "cch.control.runner.status.path")

(def ^:const default-stall-ms
  "A tick in progress longer than this is wedged. Every blocking call inside a
  tick has its own, shorter bound, so this only fires on an unbounded wait."
  120000)

(def ^:const default-max-failure-ms
  "Continuous failure longer than this ends the process so its supervisor
  starts a fresh one; a restart discards any wedged client state."
  600000)

(def ^:const stale-ms
  "The watchdog rewrites the status file every few seconds while the runner
  process is alive, so an older file means the runner is not running."
  60000)

(defn status-path
  "Sits beside the control store. The system property keeps tests away from an
  operator's real runner status."
  []
  (or (System/getProperty path-property)
      (str (or (System/getenv "XDG_DATA_HOME")
               (str (System/getProperty "user.home") "/.local/share"))
           "/cch/control-runner-status.json")))

(defn- ms-between [^Instant from ^Instant to]
  (.toMillis (Duration/between from to)))

(defn error-summary
  "Typed, bounded description of a tick failure for logs and the status file."
  [^Throwable error]
  (let [data (ex-data error)]
    (cond-> {:type (or (some-> data :type name) "unexpected-error")
             :message (or (.getMessage error) (.getName (class error)))}
      (:status data) (assoc :status (:status data))
      (:cause-class data) (assoc :cause-class (:cause-class data)))))

(defn initial [runner-id ^Instant now]
  {:runner-id runner-id
   :pid (.pid (ProcessHandle/current))
   :started-at now
   :consecutive-failures 0})

(defn tick-started [health now]
  (assoc health :tick-started-at now))

(defn record-success [health now]
  (-> health
      (assoc :last-success-at now
             :last-tick-at now
             :consecutive-failures 0)
      (dissoc :tick-started-at :failing-since :last-error)))

(defn record-failure [health summary now]
  (-> health
      (assoc :last-tick-at now :last-error summary)
      (update :consecutive-failures inc)
      (update :failing-since #(or % now))
      (dissoc :tick-started-at)))

(defn assess
  "Return nil while the runner is healthy enough to keep running, otherwise a
  reason map. A broker outage is tolerated up to max-failure-ms."
  [health now {:keys [stall-ms max-failure-ms]
               :or {stall-ms default-stall-ms
                    max-failure-ms default-max-failure-ms}}]
  (let [{:keys [tick-started-at failing-since consecutive-failures]} health]
    (cond
      (and tick-started-at (> (ms-between tick-started-at now) stall-ms))
      {:reason :stalled :since tick-started-at}

      (and failing-since (> (ms-between failing-since now) max-failure-ms))
      {:reason :sustained-failure
       :since failing-since
       :consecutive-failures consecutive-failures})))

(defn- instant-keys [m]
  (into {}
        (map (fn [[k v]] [k (if (instance? Instant v) (str v) v)]))
        m))

(defn write!
  "Atomically replace the status file (tmp + rename) with health as of now."
  ([health now] (write! (status-path) health now))
  ([path health now]
   (let [directory (fs/parent path)]
     (fs/create-dirs directory)
     (let [temporary (str (fs/create-temp-file
                            {:dir directory :prefix ".cch-runner-status-"}))]
       (try
         (spit temporary (json/generate-string
                           (instant-keys (assoc health :written-at now))))
         (fs/move temporary path {:replace-existing true})
         (finally
           (fs/delete-if-exists temporary)))))))

(def ^:private instant-fields
  #{:started-at :tick-started-at :last-tick-at :last-success-at
    :failing-since :written-at})

(defn read-status
  "Return the last persisted health, or nil when absent or unreadable."
  ([] (read-status (status-path)))
  ([path]
   (try
     (when (fs/exists? path)
       (into {}
             (map (fn [[k v]]
                    [k (if (and (instant-fields k) (string? v))
                         (Instant/parse v)
                         v)]))
             (json/parse-string (slurp path) true)))
     (catch Exception _ nil))))

(defn- process-alive? [pid]
  (boolean (some-> (ProcessHandle/of (long pid)) (.orElse nil) .isAlive)))

(defn classify
  "Summarize persisted health for diagnostics. Status is one of :ok, :failing,
  :stalled, :stopped, or :unknown (no status has ever been written)."
  ([status now] (classify status now {}))
  ([status now {:keys [alive? stall-ms]
                :or {alive? process-alive?
                     stall-ms default-stall-ms}}]
   (let [{:keys [pid written-at tick-started-at last-success-at
                 consecutive-failures failing-since last-error]} status
         age-ms (fn [instant] (when instant (ms-between instant now)))]
     (cond
       (nil? status)
       {:status :unknown}

       (or (nil? written-at)
           (> (age-ms written-at) stale-ms)
           (and pid (not (alive? pid))))
       {:status :stopped :last-success-at last-success-at}

       (and tick-started-at (> (age-ms tick-started-at) stall-ms))
       {:status :stalled :since tick-started-at :last-success-at last-success-at}

       (pos? (or consecutive-failures 0))
       {:status :failing
        :since failing-since
        :consecutive-failures consecutive-failures
        :last-success-at last-success-at
        :error-type (:type last-error)
        :cause-class (:cause-class last-error)}

       (nil? last-success-at)
       {:status :starting}

       :else
       {:status :ok :last-success-at last-success-at}))))
