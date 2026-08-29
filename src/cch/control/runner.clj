(ns cch.control.runner
  "Outbound polling runner for cross-machine native routing.

  A runner refreshes sanitized local presence, pulls messages addressed to its
  routes, submits them through local native adapters, and acknowledges only
  terminal outcomes. It owns no listener and does not launch agent processes."
  (:require [cch.control.remote :as remote]
            [cch.control.usage-sync :as usage-sync]
            [cch.db :as db]))

(def ^:const default-poll-ms 500)
(def ^:const default-usage-poll-ms 5000)
(def ^:const max-usage-backoff-ms 60000)

(def permanent-delivery-errors
  #{:invalid-message :invalid-route :message-too-large :stale-session
    :unknown-session :unsupported-agent})

(defn- report-loop-error! [error]
  (binding [*out* *err*]
    (println (str "cch control runner: "
                  (or (some-> error ex-data :type name) "unexpected-error")
                  ": " (.getMessage error)))))

(defn tick!
  "Run one reconnect-safe exchange. Dependencies are explicit so the transport
  and native delivery boundary can be exercised without real agent sessions."
  [config {:keys [list-local-sessions deliver-local!]}]
  (let [presence (list-local-sessions)]
    (remote/register! config (:sessions presence))
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

(defn start!
  "Start a daemon polling thread. The optional dependency map defaults to the
  real local control adapters but remains injectable for integration tests."
  ([config]
   (start! config
           {:list-local-sessions
            (requiring-resolve 'cch.control.core/list-local-sessions)
            :deliver-local!
            (requiring-resolve 'cch.control.core/send-local-message!)
            :sync-usage!
            #(usage-sync/tick! config (db/db-path))}))
  ([config {:keys [sync-usage!] :as dependencies}]
   (let [stopping? (atom false)
         poll-ms (or (:poll-ms config) default-poll-ms)
         last-error (atom nil)
         thread (Thread.
                  ^Runnable
                  (fn []
                    (try
                      (while (not @stopping?)
                        (try
                          (tick! config dependencies)
                          (reset! last-error nil)
                          (catch Exception error
                            (let [signature [(some-> error ex-data :type)
                                             (.getMessage error)]]
                              (when (not= signature @last-error)
                                (report-loop-error! error)
                                (reset! last-error signature)))))
                        (Thread/sleep poll-ms))
                      (catch InterruptedException _ nil))))
         usage-thread
         (when sync-usage!
           (Thread.
             ^Runnable
             (fn []
               (try
                 (loop [delay-ms (or (:usage-poll-ms config)
                                     default-usage-poll-ms)
                        prior-error nil]
                   (when-not @stopping?
                     (let [result (try
                                    (sync-usage!)
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
     (when usage-thread
       (.setName usage-thread "cch-control-usage-sync")
       (.setDaemon usage-thread true)
       (.start usage-thread))
     {:thread thread
      :usage-thread usage-thread
      :stop (fn []
              (reset! stopping? true)
              (.interrupt thread)
              (when usage-thread
                (.interrupt usage-thread)))})))
