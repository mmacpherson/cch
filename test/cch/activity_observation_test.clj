(ns cch.activity-observation-test
  (:require [cch.activity-observation :as activity]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as prop]
            [malli.core :as m]
            [malli.generator :as mg]))

(defspec generated-observations-survive-boundary-canonicalization 100
  (prop/for-all [observation (mg/generator activity/observation-schema)]
    (m/validate activity/observation-schema
                (activity/validate-observation! observation))))

(defspec generated-observations-reject-added-provider-content 100
  (prop/for-all [observation (mg/generator activity/observation-schema)]
    (= :invalid-activity-observation
       (try
         (activity/validate-observation!
           (assoc observation :raw-provider-payload "synthetic-private"))
         (catch clojure.lang.ExceptionInfo error
           (:type (ex-data error)))))))

(deftest local-events-become-coarse-allowlisted-activity
  (let [raw {:id 42
             :timestamp "2026-08-29T12:34:56.789Z"
             :agent "claude-code"
             :hook_name "event-log"
             :event_type "PreToolUse"
             :tool_name "mcp__private_server__exec_command"
             :decision "ask"
             :elapsed_ms 4.25
             :session_id "private-session"
             :file_path "/private/project/secret.clj"
             :cwd "/private/project"
             :reason "private explanation"
             :extra "{\"command\":\"private command\"}"}
        observation (activity/from-local-event raw)
        rendered (pr-str observation)]
    (is (= #{:event-id :schema-version :observed-at :agent :action
             :tool-category :outcome :duration-ms}
           (set (keys observation))))
    (is (= "tool.requested" (:action observation)))
    (is (= "execute" (:tool-category observation)))
    (is (= "approval-needed" (:outcome observation)))
    (doseq [private-value ["private_server" "private-session" "/private"
                           "secret.clj" "private explanation" "private command"]]
      (is (not (str/includes? rendered private-value))))))

(deftest schema-rejects-unknown-and-provider-content-fields
  (let [valid {:event-id (apply str (repeat 64 "a"))
               :schema-version 1
               :observed-at 2000000000000
               :agent "codex"
               :action "turn.started"
               :outcome "observed"}]
    (is (= valid (activity/validate-observation! valid)))
    (doseq [field [:path :cwd :command :prompt :reason :payload :runner-id
                   :session-id :account :transcript :credential]]
      (testing (name field)
        (is (= :invalid-activity-observation
               (try
                 (activity/validate-observation! (assoc valid field "private"))
                 (catch clojure.lang.ExceptionInfo error
                   (:type (ex-data error))))))))))

(deftest schema-enforces-cross-field-and-numeric-contracts
  (let [base {:event-id (apply str (repeat 64 "b"))
              :schema-version 1
              :observed-at 2000000000000
              :agent "codex"
              :action "turn.started"
              :outcome "observed"}
        invalid [(assoc base :tool-category "execute")
                 (assoc base :action "tool.requested")
                 (assoc base :duration-ms Double/NaN)
                 (assoc base :duration-ms 3600001)
                 (assoc base :schema-version 2)]]
    (doseq [observation invalid]
      (is (= :invalid-activity-observation
             (try
               (activity/validate-observation! observation)
               (catch clojure.lang.ExceptionInfo error
                 (:type (ex-data error)))))))
    (is (= (assoc base :action "tool.requested"
                       :tool-category "execute"
                       :duration-ms 12.0)
           (activity/validate-observation!
             (assoc base :action "tool.requested"
                         :tool-category "execute"
                         :duration-ms 12))))))

(deftest policy-hook-duplicates-and-unrecognized-events-are-not-exported
  (is (nil? (activity/from-local-event
              {:id 1 :timestamp "2026-08-29T12:00:00Z"
               :agent "claude-code" :hook_name "scope-lock"
               :event_type "PreToolUse" :tool_name "Edit"})))
  (is (nil? (activity/from-local-event
              {:id 2 :timestamp "2026-08-29T12:00:00Z"
               :agent "claude-code" :hook_name "event-log"
               :event_type "UnknownFutureProviderEvent"}))))
