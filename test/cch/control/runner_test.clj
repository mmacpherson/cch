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
