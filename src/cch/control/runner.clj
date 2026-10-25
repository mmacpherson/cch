(ns cch.control.runner
  "Outbound polling runner for cross-machine native routing.

  A runner refreshes sanitized local presence, pulls messages addressed to its
  routes, submits them through local native adapters, and acknowledges only
  terminal outcomes. It owns no listener and does not launch agent processes."
  (:require [cch.control.activity-sync :as activity-sync]
            [cch.control.remote :as remote]
            [cch.control.runner-health :as health]
            [cch.control.usage-sync :as usage-sync]
            [cch.db :as db]
            [clojure.string :as str])
  (:import [java.time Instant]))

(def ^:const default-poll-ms 500)
(def ^:const default-usage-poll-ms 5000)
(def ^:const max-usage-backoff-ms 60000)

(def permanent-delivery-errors
  #{:invalid-message :invalid-route :message-too-large :stale-session
    :unknown-session :unsupported-agent})

(def ^:const still-failing-report-ms
  "While failing continuously, repeat a summary line this often so a log that
  stops changing never looks like a healthy runner."
  60000)

(def ^:const default-watchdog-ms 5000)

(defn log!
  "Timestamped operator log line on stderr."
  [& parts]
  (binding [*out* *err*]
    (println (str (Instant/now) " cch control runner: " (apply str parts)))))

(defn- report-loop-error! [error]
  (let [{:keys [type status message]} (health/error-summary error)]
    (log! type (when status (str " (HTTP " status ")")) ": " message)))

(defn- thread-stack [^Thread thread]
  (str/join "\n" (map #(str "    at " %) (.getStackTrace thread))))

(defn tick!
  "Run one reconnect-safe exchange. Dependencies are explicit so the transport
  and native delivery boundary can be exercised without real agent sessions."
  [config {:keys [list-local-sessions deliver-local!]}]
  (let [presence (list-local-sessions)
        ;; Publish only live presence. list-local-sessions returns every
        ;; discovered session, including stale/dead ones; registering all of
        ;; them both misrepresents fleet presence and can push the payload past
        ;; the broker's request-body cap, which rejects the whole register (413)
        ;; so nothing lands and the runner silently vanishes from the fleet.
        live-sessions (filterv :available (:sessions presence))]
    (remote/register! config live-sessions)
    (reduce
      (fn [result {:keys [message-id source target body]}]
        (try
          (deliver-local! {:message-id message-id
                           :source source
                           :target target
                           :message body})
          (remote/ack! config message-id "delivered")
          (update result :delivered inc)
          (catch clojure.lang.ExceptionInfo error
            (let [type (:type (ex-data error))]
              (if (contains? permanent-delivery-errors type)
                (do
                  (remote/ack! config message-id "failed" type)
                  (update result :failed inc))
                ;; No ack: the broker's bounded lease/expiry policy retries it.
                (update result :retry-later inc))))
          (catch Exception _
            (update result :retry-later inc))))
      {:delivered 0 :failed 0 :retry-later 0}
      (remote/poll! config))))

(defn default-dependencies
  "The real local control adapters and observation syncs."
  [config]
  {:list-local-sessions
   (requiring-resolve 'cch.control.core/list-local-sessions)
   :deliver-local!
   (requiring-resolve 'cch.control.core/send-local-message!)
   :sync-usage!
   #(usage-sync/tick! config (db/db-path))
   :sync-activity!
   #(activity-sync/tick! config (db/db-path))})

(defn start!
  "Start a daemon polling thread plus a watchdog that persists runner health.
  The optional dependency map defaults to the real local control adapters but
  remains injectable for integration tests. When the watchdog finds a stalled
  tick or a sustained failure it logs why and calls :on-unhealthy! with the
  verdict, if given; the CLI uses that to exit for a supervisor restart."
  ([config]
   (start! config (default-dependencies config)))
  ([config {:keys [sync-usage! sync-activity! on-unhealthy!] :as dependencies}]
   (let [stopping? (atom false)
         poll-ms (or (:poll-ms config) default-poll-ms)
         now #(Instant/now)
         health (atom (health/initial (:runner-id config) (now)))
         last-error (atom nil)
         last-summary-at (atom nil)
         run-tick! (fn []
                     (swap! health health/tick-started (now))
                     (try
                       (tick! config dependencies)
                       (let [[prior _] (swap-vals! health health/record-success (now))]
                         (when (pos? (:consecutive-failures prior))
                           (log! "recovered after " (:consecutive-failures prior)
                                 " consecutive failures since " (:failing-since prior)))
                         (reset! last-error nil)
                         (reset! last-summary-at nil))
                       (catch Exception error
                         (let [summary (health/error-summary error)
                               signature [(:type summary) (:message summary)]
                               current (swap! health health/record-failure summary (now))]
                           (cond
                             (not= signature @last-error)
                             (do (report-loop-error! error)
                                 (reset! last-error signature)
                                 (reset! last-summary-at (now)))

                             (> (- (System/currentTimeMillis)
                                   (.toEpochMilli ^Instant @last-summary-at))
                                still-failing-report-ms)
                             (do (log! "still failing: " (:consecutive-failures current)
                                       " consecutive failures since "
                                       (:failing-since current)
                                       "; last " (:type summary) ": " (:message summary))
                                 (reset! last-summary-at (now))))))))
         thread (Thread.
                  ^Runnable
                  (bound-fn []
                    (try
                      (while (not @stopping?)
                        (run-tick!)
                        (Thread/sleep poll-ms))
                      (catch InterruptedException _ nil))))
         status-path (or (:status-path config) (health/status-path))
         watchdog-thread
         (Thread.
           ^Runnable
           (bound-fn []
             (try
               (loop [reported nil]
                 (when-not @stopping?
                   (let [snapshot @health
                         verdict (health/assess snapshot (now) config)]
                     (try
                       (health/write! status-path snapshot (now))
                       (catch Exception error
                         (when-not reported
                           (log! "could not write runner status: " (.getMessage error)))))
                     (when (and verdict (not= (:reason verdict) (:reason reported)))
                       (case (:reason verdict)
                         :stalled
                         (log! "polling tick stalled since " (:since verdict)
                               "; runner thread stack:\n" (thread-stack thread))
                         :sustained-failure
                         (log! "broker exchange has failed continuously since "
                               (:since verdict) " (" (:consecutive-failures verdict)
                               " attempts)"))
                       (when on-unhealthy!
                         (on-unhealthy! verdict)))
                     (Thread/sleep (long (or (:watchdog-ms config) default-watchdog-ms)))
                     (recur verdict))))
               (catch InterruptedException _ nil))))
         usage-thread
         (when (or sync-usage! sync-activity!)
           (Thread.
             ^Runnable
             (bound-fn []
               (try
                 (loop [delay-ms (or (:usage-poll-ms config)
                                     default-usage-poll-ms)
                        prior-error nil]
                   (when-not @stopping?
                     (let [result (try
                                    {:errors
                                     (->> [[:usage sync-usage!]
                                           [:activity sync-activity!]]
                                          (mapcat
                                            (fn [[operation sync!]]
                                              (if-not sync!
                                                []
                                                (try
                                                  (let [sync-result (sync!)]
                                                    (if (seq (:errors sync-result))
                                                      (:errors sync-result)
                                                      []))
                                                  (catch Exception error
                                                    [{:operation operation
                                                      :error error}])))))
                                          vec)}
                                    (catch Exception error
                                      {:errors [{:operation :tick
                                                 :error error}]}))
                           error (some-> result :errors first :error)
                           signature (when error
                                       [(some-> error ex-data :type)
                                        (.getMessage ^Exception error)])]
                       (when (and error (not= signature prior-error))
                         (report-loop-error! error))
                       (Thread/sleep delay-ms)
                       (recur (if error
                                (min max-usage-backoff-ms (* 2 delay-ms))
                                (or (:usage-poll-ms config)
                                    default-usage-poll-ms))
                              signature))))
                 (catch InterruptedException _ nil)))))]
     (.setName thread "cch-control-runner")
     (.setDaemon thread true)
     (.start thread)
     (.setName watchdog-thread "cch-control-runner-watchdog")
     (.setDaemon watchdog-thread true)
     (.start watchdog-thread)
     (when usage-thread
       (.setName usage-thread "cch-control-usage-sync")
       (.setDaemon usage-thread true)
       (.start usage-thread))
     {:thread thread
      :usage-thread usage-thread
      :watchdog-thread watchdog-thread
      :health health
      :stop (fn []
              (reset! stopping? true)
              (.interrupt thread)
              (.interrupt watchdog-thread)
              (when usage-thread
                (.interrupt usage-thread)))})))
