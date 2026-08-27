(ns cch.control.runner-test
  (:require [cch.control.broker :as broker]
            [cch.control.broker-http :as broker-http]
            [cch.control.remote :as remote]
            [cch.control.runner :as runner]
            [clojure.test :refer [deftest is]])
  (:import [java.net ServerSocket]))

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
