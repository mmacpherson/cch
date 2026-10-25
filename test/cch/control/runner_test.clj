(ns cch.control.runner-test
  (:require [cch.control.broker :as broker]
            [cch.control.broker-http :as broker-http]
            [cch.control.remote :as remote]
            [cch.control.runner :as runner]
            [clojure.test :refer [deftest is]])
  (:import [java.net ServerSocket]
           [java.util.concurrent CountDownLatch TimeUnit]))

(defn- free-port []
  (with-open [socket (ServerSocket. 0)]
    (.getLocalPort socket)))

(defn- route [agent suffix]
  {:id (str agent ":20000000-0000-0000-0000-00000000000" suffix)
   :agent agent :status "idle" :available true})

(deftest two-outbound-runners-deliver-request-and-reply
  (let [port (free-port)
        b (broker/new-broker {"runner-a" "synthetic-token-a"
                              "runner-b" "synthetic-token-b"})
        server (broker-http/start! b {:port port})
        config-a {:url (str "http://127.0.0.1:" port)
                  :runner-id "runner-a" :token "synthetic-token-a"}
        config-b {:url (str "http://127.0.0.1:" port)
                  :runner-id "runner-b" :token "synthetic-token-b"}
        codex-a (route "codex" "a")
        codex-b (route "codex" "b")
        claude-b (route "claude" "c")
        delivered-a (atom [])
        delivered-b (atom [])
        dependencies-a {:list-local-sessions #(hash-map :sessions [codex-a] :errors [])
                        :deliver-local! #(do (swap! delivered-a conj %)
                                             (assoc % :status "submitted"))}
        dependencies-b {:list-local-sessions #(hash-map :sessions [codex-b claude-b]
                                                        :errors [])
                        :deliver-local! #(do (swap! delivered-b conj %)
                                             (assoc % :status "submitted"))}]
    (try
      ;; Registration is automatic on each polling tick; agents do not pair.
      (runner/tick! config-a dependencies-a)
      (runner/tick! config-b dependencies-b)
      (remote/enqueue! config-a {:source (:id codex-a) :target (:id claude-b)
                                 :message "Synthetic cross-runner request"
                                 :message-id "runner-message-1"})
      (is (= {:delivered 1 :failed 0 :retry-later 0}
             (runner/tick! config-b dependencies-b)))
      (is (= [{:source (:id codex-a) :target (:id claude-b)
               :message "Synthetic cross-runner request"
               :message-id "runner-message-1"}]
             @delivered-b))
      (remote/enqueue! config-a {:source (:id codex-a) :target (:id codex-b)
                                 :message "Synthetic Codex review request"
                                 :message-id "runner-message-codex"})
      (is (= {:delivered 1 :failed 0 :retry-later 0}
             (runner/tick! config-b dependencies-b)))
      (is (= (:id codex-b) (:target (second @delivered-b))))
      (remote/enqueue! config-b {:source (:id claude-b) :target (:id codex-a)
                                 :message "Synthetic cross-runner reply"
                                 :message-id "runner-message-2"})
      (is (= {:delivered 1 :failed 0 :retry-later 0}
             (runner/tick! config-a dependencies-a)))
      (is (= "Synthetic cross-runner reply" (:message (first @delivered-a))))
      (is (empty? (remote/poll! config-a)))
      (finally
        ((:stop server) :timeout 100)))))

(deftest transient-local-failure-is-left-for-bounded-broker-retry
  (let [registered (atom nil)
        acked (atom [])]
    (with-redefs [remote/register! (fn [_ sessions] (reset! registered sessions))
                  remote/poll! (constantly [{:message-id "retry-message"
                                             :source "codex:synthetic-a"
                                             :target "codex:synthetic-b"
                                             :body "Synthetic"}])
                  remote/ack! (fn [& args] (swap! acked conj args))]
      (is (= {:delivered 0 :failed 0 :retry-later 1}
             (runner/tick! {:runner-id "runner-b"}
                           {:list-local-sessions #(hash-map :sessions [(route "codex" "b")])
                            :deliver-local! #(throw (ex-info "temporary"
                                                            {:type :codex-daemon-unavailable}))})))
      (is (= [(route "codex" "b")] @registered))
      (is (empty? @acked)))))

(deftest tick-registers-only-available-local-presence
  ;; list-local-sessions returns every discovered session, including stale/dead
  ;; ones. Publishing those both misrepresents fleet presence and can push the
  ;; register payload past the broker's request-body cap, which rejects the
  ;; whole register (413). The runner must advertise only live presence.
  (let [registered (atom nil)
        live-a (route "codex" "a")
        live-b (assoc (route "claude" "b") :available true)
        stale  (assoc (route "codex" "c") :available false :status "stale")]
    (with-redefs [remote/register! (fn [_ sessions] (reset! registered sessions))
                  remote/poll! (constantly [])]
      (runner/tick! {:runner-id "runner-a"}
                    {:list-local-sessions
                     #(hash-map :sessions [live-a stale live-b] :errors [])
                     :deliver-local! identity})
      (is (= [live-a live-b] @registered)
          "stale sessions are dropped before registering"))))

(deftest report-loop-error-surfaces-http-status
  ;; A broker rejection carries an HTTP status in ex-data; the operator-facing
  ;; log must show it so a typed 413/409 is diagnosable, not a bare message.
  (let [report! @(ns-resolve 'cch.control.runner 'report-loop-error!)
        sink (java.io.StringWriter.)]
    (binding [*err* sink]
      (report! (ex-info "Register presence snapshot exceeds the maximum body size"
                        {:type :register-too-large :status 413})))
    (is (re-find #"register-too-large \(HTTP 413\)" (str sink)))))

(deftest polling-loop-reports-distinct-errors-and-recovers
  (let [calls (atom 0)
        reported (atom [])
        completed (CountDownLatch. 1)
        report-var (ns-resolve 'cch.control.runner 'report-loop-error!)]
    (with-redefs-fn
      {#'runner/tick!
       (fn [_ _]
         (let [call (swap! calls inc)]
           (case call
             (1 2 4) (throw (ex-info "broker unavailable"
                                     {:type :broker-unavailable}))
             5 (do (.countDown completed) {:delivered 0})
             {:delivered 0})))
       report-var
       (fn [error]
         (swap! reported conj [(:type (ex-data error)) (.getMessage error)]))}
      (fn []
        (let [{:keys [thread stop]}
              (runner/start! {:poll-ms 1}
                             {:list-local-sessions (constantly {:sessions []})
                              :deliver-local! identity})]
          (try
            (is (.await completed 1 TimeUnit/SECONDS))
            (finally
              (stop)
              (.join thread 1000))))))
    ;; Consecutive repetitions are suppressed. A successful tick resets the
    ;; signature, so the same later outage remains visible to the operator.
    (is (= [[:broker-unavailable "broker unavailable"]
            [:broker-unavailable "broker unavailable"]]
           @reported))))

(deftest usage-sync-runs-independently-of-routing
  (let [routing-ran (CountDownLatch. 1)
        usage-entered (CountDownLatch. 1)
        release-usage (CountDownLatch. 1)]
    (with-redefs [runner/tick! (fn [_ _]
                                 (.countDown routing-ran)
                                 {:delivered 0})]
      (let [{:keys [thread usage-thread stop]}
            (runner/start!
              {:poll-ms 5 :usage-poll-ms 5}
              {:list-local-sessions (constantly {:sessions []})
               :deliver-local! identity
               :sync-usage! (fn []
                              (.countDown usage-entered)
                              (.await release-usage 1 TimeUnit/SECONDS)
                              {:errors []})})]
        (try
          (is (.await usage-entered 1 TimeUnit/SECONDS))
          (is (.await routing-ran 1 TimeUnit/SECONDS)
              "routing progresses while usage exchange is blocked")
          (finally
            (.countDown release-usage)
            (stop)
            (.join thread 1000)
            (.join usage-thread 1000)))))))

(defn- status-path []
  (str (babashka.fs/create-temp-dir {:prefix "cch-runner-test-"}) "/status.json"))

(deftest watchdog-reports-a-stalled-tick-with-its-stack-and-asks-for-restart
  ;; An unbounded wait inside a tick used to leave the runner silent and
  ;; unregistered while still looking alive to its supervisor.
  (let [release (CountDownLatch. 1)
        verdicts (atom [])
        unhealthy (CountDownLatch. 1)
        sink (java.io.StringWriter.)
        path (status-path)]
    (with-redefs [runner/tick! (fn [_ _] (.await release) {:delivered 0})]
      (binding [*err* sink]
        (let [{:keys [thread stop]}
              (runner/start! {:poll-ms 1 :stall-ms 50 :watchdog-ms 10
                              :status-path path}
                             {:list-local-sessions (constantly {:sessions []})
                              :deliver-local! identity
                              :on-unhealthy! #(do (swap! verdicts conj %)
                                                  (.countDown unhealthy))})]
          (try
            (is (.await unhealthy 2 TimeUnit/SECONDS))
            (Thread/sleep 50)
            (finally
              (.countDown release)
              (stop)
              (.join thread 1000))))))
    (is (= [:stalled] (map :reason @verdicts)) "reported once per episode")
    (is (re-find #"polling tick stalled since .*\n    at " (str sink)))
    (is (some? (get (cch.control.runner-health/read-status path) :tick-started-at))
        "the status file shows the tick in progress")))

(deftest sustained-failure-asks-for-restart-and-is-visible-in-status
  (let [verdicts (atom [])
        unhealthy (CountDownLatch. 1)
        path (status-path)]
    (with-redefs [runner/tick! (fn [_ _]
                                 (throw (ex-info "Broker request failed"
                                                 {:type :broker-unavailable})))]
      (binding [*err* (java.io.StringWriter.)]
        (let [{:keys [thread stop]}
              (runner/start! {:poll-ms 1 :max-failure-ms 50 :watchdog-ms 10
                              :status-path path}
                             {:list-local-sessions (constantly {:sessions []})
                              :deliver-local! identity
                              :on-unhealthy! #(do (swap! verdicts conj %)
                                                  (.countDown unhealthy))})]
          (try
            (is (.await unhealthy 2 TimeUnit/SECONDS))
            (finally
              (stop)
              (.join thread 1000))))))
    (is (= :sustained-failure (:reason (first @verdicts))))
    (let [status (cch.control.runner-health/read-status path)]
      (is (pos? (:consecutive-failures status)))
      (is (= "broker-unavailable" (get-in status [:last-error :type]))))))

(deftest runner-reregisters-after-the-broker-goes-away-and-returns
  ;; Reproduces a broker or overlay blip against the real HTTP client: the
  ;; broker stops, the runner fails, the broker returns on the same port, and
  ;; the runner's routes reappear without a restart. The log carries a
  ;; timestamped error and a recovery line.
  (let [port (free-port)
        make-broker #(broker/new-broker {"runner-a" "synthetic-token-a"})
        config {:url (str "http://127.0.0.1:" port)
                :runner-id "runner-a" :token "synthetic-token-a"
                :poll-ms 10 :watchdog-ms 10 :status-path (status-path)}
        codex-a (route "codex" "a")
        sink (java.io.StringWriter.)
        routes (fn [b] (set (map :id (broker/sessions b))))
        eventually (fn [pred]
                     (loop [n 0]
                       (cond (pred) true
                             (> n 300) false
                             :else (do (Thread/sleep 10) (recur (inc n))))))
        first-broker (make-broker)
        server (atom (broker-http/start! first-broker {:port port}))]
    (binding [*err* sink]
      (let [{:keys [thread stop health]}
            (runner/start! config
                           {:list-local-sessions #(hash-map :sessions [codex-a])
                            :deliver-local! identity})]
        (try
          (is (eventually #(contains? (routes first-broker) (:id codex-a))))
          ((:stop @server) :timeout 100)
          (is (eventually #(pos? (:consecutive-failures @health))))
          ;; A fresh in-memory broker stands in for a restarted one: it has
          ;; no lease until the runner registers again.
          (let [second-broker (make-broker)]
            (reset! server (broker-http/start! second-broker {:port port}))
            (is (eventually #(contains? (routes second-broker) (:id codex-a))))
            (is (eventually #(zero? (:consecutive-failures @health)))))
          (finally
            (stop)
            (.join thread 1000)
            ((:stop @server) :timeout 100)))))
    (is (re-find #"\d{4}-\d\d-\d\dT[\d:.]+Z cch control runner: broker-unavailable"
                 (str sink)))
    (is (re-find #"recovered after \d+ consecutive failures" (str sink)))))
