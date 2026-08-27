(ns cch.control.runner
  "Outbound polling runner for the disposable cross-machine POC.

  A runner refreshes sanitized local presence, pulls messages addressed to its
  routes, submits them through local native adapters, and acknowledges only
  terminal outcomes. It owns no listener and does not launch agent processes."
  (:require [cch.control.remote :as remote]))

(def ^:const default-poll-ms 500)

(def permanent-delivery-errors
  #{:invalid-message :invalid-route :message-too-large :stale-session
    :unknown-session :unsupported-agent})

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
  real local control adapters but remains injectable for POC tests."
  ([config]
   (start! config
           {:list-local-sessions
            (requiring-resolve 'cch.control.core/list-local-sessions)
            :deliver-local!
            (requiring-resolve 'cch.control.core/send-local-message!)}))
  ([config dependencies]
   (let [stopping? (atom false)
         poll-ms (or (:poll-ms config) default-poll-ms)
         thread (Thread.
                  ^Runnable
                  (fn []
                    (try
                      (while (not @stopping?)
                        (try
                          (tick! config dependencies)
                          (catch Exception _ nil))
                        (Thread/sleep poll-ms))
                      (catch InterruptedException _ nil))))]
     (.setName thread "cch-control-runner")
     (.setDaemon thread true)
     (.start thread)
     {:thread thread
      :stop (fn []
              (reset! stopping? true)
              (.interrupt thread))})))
