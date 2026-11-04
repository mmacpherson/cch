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
           {:id 2 :result {:data ["thread-1"]}}
           {:id 3 :result
            {:thread {:id "thread-1"
                      :cwd "/synthetic/project"
                      :status {:type "idle"}
                      :canAcceptDirectInput nil
                      :cliVersion "0.149.0"
                      :source "cli"}}}
           {:id 4 :result {:queuedSubmission {}}}])
    (atom [])))

(defn- refresh-transport [servers]
  (->FakeTransport
    (atom [{:id 1 :result {}}
           {:id 2 :result {}}
           {:id 3 :result {:data servers}}])
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

(deftest refreshes-shared-mcp-runtime-and-verifies-cch-tools
  (let [transport (refresh-transport
                    [{:name "cch"
                      :tools {:list_sessions {}
                              :get_session {}
                              :send_message {}
                              :set_session_alias {}}}])]
    (binding [codex/*connect!* (constantly transport)]
      (is (= {:agent "codex"
              :server "cch"
              :status "refreshed"
              :tools '("get_session" "list_sessions" "send_message"
                       "set_session_alias")}
             (codex/refresh-mcp!))))
    (is (= [{:id 1
             :method "initialize"
             :params {:clientInfo {:name "cch-control-plane" :version "0.1.0"}
                      :capabilities {:experimentalApi true
                                     :optOutNotificationMethods []}}}
            {:method "initialized" :params {}}
            {:id 2 :method "config/mcpServer/reload" :params nil}
            {:id 3 :method "mcpServerStatus/list"
             :params {:detail "toolsAndAuthOnly"}}]
           @(:requests transport)))))

(deftest refresh-fails-closed-when-cch-does-not-return
  (doseq [[servers expected]
          [[[] #"cch server is unavailable"]
           [[{:name "cch" :tools {:list_sessions {}}}]
            #"expected tools are missing: get_session, send_message"]
           [[{:name "cch"
              :tools {:list_sessions {}
                      :get_session {}
                      :send_message {}
                      :set_session_alias {}
                      :unexpected_tool {}}}]
            #"unexpected tools are present: unexpected_tool"]]]
    (binding [codex/*connect!* (constantly (refresh-transport servers))]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo expected
                            (codex/refresh-mcp!))))))

(defrecord SilentTransport [closed]
  websocket/TextTransport
  (send-text! [_ _] nil)
  (read-text! [_]
    ;; A daemon that accepted the socket but never answers: block until the
    ;; transport is closed, as a real channel read would.
    (while (not (realized? closed)) (Thread/sleep 5))
    (throw (java.nio.channels.AsynchronousCloseException.)))
  (close! [_] (deliver closed true)))

(deftest a-silent-daemon-times-out-instead-of-blocking-the-caller
  (let [closed (promise)]
    (binding [codex/*connect!* (constantly (->SilentTransport closed))
              codex/*call-timeout-ms* 100]
      (let [error (try (codex/sessions) nil
                       (catch clojure.lang.ExceptionInfo error error))]
        (is (= :codex-daemon-timeout (:type (ex-data error))))
        (is (deref closed 1000 false)
            "the socket is closed so the abandoned worker unblocks")))))

(defn- thread-transport [status & responses]
  (->FakeTransport
    (atom (into [{:id 1 :result {}}
                 {:id 2 :result {:data ["thread-1"]}}
                 {:id 3 :result {:thread {:id "thread-1"
                                          :cwd "/synthetic/project"
                                          :status status
                                          :canAcceptDirectInput nil
                                          :source "cli"}}}]
                (map-indexed (fn [i response] (assoc response :id (+ 4 i))))
                responses))
    (atom [])))

(defn- send-through! [transport]
  (binding [codex/*connect!* (fn [_] transport)]
    (codex/send! {:route-id "codex:thread-1"
                  :message "Synthetic ping"
                  :message-id "message-1"
                  :source "claude:session-1"
                  :seq 3
                  :in-reply-to "message-0"})))

(defn- requests [transport]
  (filterv :method @(:requests transport)))

(deftest a-running-turn-is-steered-instead-of-queued
  (let [transport (thread-transport
                    {:type "active" :activeFlags []}
                    {:result {:data [{:id "turn-9" :status "inProgress" :items []}]}}
                    {:result {:turnId "turn-9"}})
        result (send-through! transport)
        [turns steer] (drop 4 (requests transport))]
    (is (= "steered" (:delivery result)))
    (is (= "thread/turns/list" (:method turns)))
    (is (= "notLoaded" (get-in turns [:params :itemsView]))
        "the lookup never loads transcript items")
    (is (= "turn/steer" (:method steer)))
    (is (= {:threadId "thread-1" :expectedTurnId "turn-9"
            :clientUserMessageId "message-1"
            :input [{:type "text"
                     :text (str "[cch message from claude:session-1; id message-1;"
                                " seq 3; re message-0]\nSynthetic ping")}]}
           (:params steer)))
    (is (= 6 (count (requests transport))) "no queue/add after a steer")))

(deftest a-turn-that-ends-before-the-steer-falls-back-to-the-queue
  (let [transport (thread-transport
                    {:type "active" :activeFlags []}
                    {:result {:data [{:id "turn-9" :status "inProgress" :items []}]}}
                    {:error {:message "expected turn is not active"}}
                    {:result {}})
        result (send-through! transport)]
    (is (= "queued" (:delivery result)))
    (is (= ["thread/turns/list" "turn/steer" "thread/queue/add"]
           (mapv :method (drop 4 (requests transport)))))))

(deftest an-idle-thread-is-queued-without-a-turn-lookup
  (let [transport (thread-transport {:type "idle"} {:result {}})
        result (send-through! transport)]
    (is (= "queued" (:delivery result)))
    (is (= ["thread/queue/add"] (mapv :method (drop 4 (requests transport)))))))

(deftest an-active-thread-without-a-running-turn-is-queued
  (let [transport (thread-transport
                    {:type "active" :activeFlags ["waitingOnUserInput"]}
                    {:result {:data [{:id "turn-8" :status "completed" :items []}]}}
                    {:result {}})
        result (send-through! transport)]
    (is (= "queued" (:delivery result)))
    (is (= ["thread/turns/list" "thread/queue/add"]
           (mapv :method (drop 4 (requests transport)))))))

(defn- served-tool [description & parameters]
  {:description description
   :inputSchema {:type "object"
                 :properties (into {} (map (fn [p] [(keyword p) {:type "string"}]))
                                   parameters)}})

(def ^:private current-tools
  {:list_sessions (served-tool "List")
   :get_session (served-tool "Get" "session_id")
   :send_message (served-tool "Send" "target" "message" "in_reply_to")
   :set_session_alias (served-tool "Alias" "alias")})

(def ^:private expected-signatures
  (into {} (map (fn [[k v]] [(name k) (codex/tool-signature v)])) current-tools))

(deftest refresh-accepts-a-server-running-this-build
  (binding [codex/*connect!* (constantly (refresh-transport
                                            [{:name "cch" :tools current-tools}]))]
    (is (= "refreshed" (:status (codex/refresh-mcp! expected-signatures))))))

(deftest refresh-reports-a-server-still-running-older-code
  (let [stale (assoc current-tools :send_message (served-tool "Send" "target" "message"))]
    (binding [codex/*connect!* (constantly (refresh-transport
                                              [{:name "cch" :tools stale}]))]
      (is (= {:type :codex-mcp-stale :server "cch" :stale-tools ["send_message"]}
             (try (codex/refresh-mcp! expected-signatures)
                  (catch clojure.lang.ExceptionInfo error (ex-data error))))))))
