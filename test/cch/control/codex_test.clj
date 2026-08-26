(ns cch.control.codex-test
  (:require [cch.control.codex :as codex]
            [cch.control.unix-websocket :as websocket]
            [cheshire.core :as json]
            [clojure.test :refer [deftest is]]))

(defrecord FakeTransport [responses requests]
  websocket/TextTransport
  (send-text! [_ text]
    (swap! requests conj (json/parse-string text true)))
  (read-text! [_]
    (let [response (first @responses)]
      (swap! responses subvec 1)
      (json/generate-string response)))
  (close! [_] nil))

(defn- fake-transport []
  (->FakeTransport
    (atom [{:id 1 :result {}}
           {:id 2 :result
            {:data [{:id "thread-1"
                     :cwd "/synthetic/project"
                     :status {:type "idle"}
                     :canAcceptDirectInput nil
                     :cliVersion "0.149.0"
                     :source "cli"}]}}
           {:id 3 :result {:queuedSubmission {}}}])
    (atom [])))

(deftest discovers-and-queues-through-app-server-json-rpc
  (binding [codex/*connect!* (fn [_] (fake-transport))]
    (let [sessions (codex/sessions)]
      (is (= ["codex:thread-1"] (mapv :id sessions)))
      (is (true? (:available (first sessions)))))
    (let [result (codex/send! {:route-id "codex:thread-1"
                               :message "Synthetic POC ping"
                               :message-id "message-1"
                               :source "claude:session-1"})]
      (is (= "codex-app-server" (:transport result)))
      (is (= "submitted" (:status result))))))

(deftest unavailable-daemon-has-actionable-error
  (binding [codex/*connect!* (fn [_] (throw (Exception. "native-daemon-missing")))]
    (let [message (try (codex/sessions) nil
                       (catch clojure.lang.ExceptionInfo e (.getMessage e)))]
      (is (re-find #"shared app-server daemon is unavailable" message))
      (is (re-find #"cch control install" message))
      (is (re-find #"app-server --listen unix://" message))
      (is (not (re-find #"standalone" message))))))
