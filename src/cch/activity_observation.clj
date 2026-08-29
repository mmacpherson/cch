(ns cch.activity-observation
  "Narrow, provider-agnostic activity records safe for the hosted event feed."
  (:require [cch.control.store :as store]
            [cch.schema :as schema]
            [clojure.string :as str])
  (:import [java.time Instant LocalDateTime ZoneOffset]
           [java.time.format DateTimeFormatter DateTimeParseException]))

(def schema-version 1)
(def agents #{"claude-code" "codex" "agy"})
(def actions
  #{"session.started" "session.stopped" "session.ended"
    "turn.started" "tool.requested" "tool.completed" "tool.failed"
    "tool.permission" "context.compacted" "attention.requested"})
(def tool-categories #{"read" "write" "execute" "search" "agent" "web" "other"})
(def outcomes #{"observed" "allowed" "approval-needed" "denied" "completed" "failed"})
(defn- finite-number? [value]
  (and (number? value) (Double/isFinite (double value))))

(def ^:private tool-actions
  (set (filter #(str/starts-with? % "tool.") actions)))

(def ^:private non-tool-actions
  (remove tool-actions actions))

(def ^:private common-observation-entries
  [[:event-id [:re #"[a-f0-9]{64}"]]
   [:schema-version [:= schema-version]]
   [:observed-at [:int {:min 1}]]
   [:agent (into [:enum] (sort agents))]
   [:outcome (into [:enum] (sort outcomes))]
   [:duration-ms {:optional true}
    [:or [:int {:min 0 :max 3600000}]
     [:double {:min 0.0 :max 3600000.0}]]]])

(def observation-schema
  "Closed, versioned wire contract for one fleet-safe activity observation."
  [:or
   (into [:map {:closed true}]
         (concat common-observation-entries
                 [[:action (into [:enum] (sort tool-actions))]
                  [:tool-category (into [:enum] (sort tool-categories))]]))
   (into [:map {:closed true}]
         (concat common-observation-entries
                 [[:action (into [:enum] (sort non-tool-actions))]]))])

(def ^:private observation-validator
  (schema/validator observation-schema))

(defn validate-observation!
  "Validate and return the canonical allowlisted shape. Unknown keys fail
  closed so a future caller cannot accidentally smuggle a raw provider field."
  [observation]
  (schema/validate! observation-validator observation
                    :invalid-activity-observation
                    "Activity observation is invalid")
  (cond-> observation
    (some? (:duration-ms observation))
    (update :duration-ms double)))

(defn- parse-millis [value]
  (cond
    (integer? value) value
    (string? value)
    (try
      (.toEpochMilli (Instant/parse value))
      (catch DateTimeParseException _
        (try
          (-> (LocalDateTime/parse value DateTimeFormatter/ISO_LOCAL_DATE_TIME)
              (.toInstant ZoneOffset/UTC)
              .toEpochMilli)
          (catch DateTimeParseException _ nil))))
    :else nil))

(defn- canonical-agent [value]
  (case (some-> value str/lower-case)
    ("claude" "claude-code") "claude-code"
    "codex" "codex"
    ("agy" "antigravity" "gemini") "agy"
    nil))

(defn- tool-category [tool]
  (let [tool (some-> tool str/lower-case)]
    (cond
      (nil? tool) "other"
      (re-find #"read|view|cat|notebookread" tool) "read"
      (re-find #"edit|write|patch|notebookedit" tool) "write"
      (re-find #"bash|shell|exec|command|terminal" tool) "execute"
      (re-find #"search|find|grep|glob|rg" tool) "search"
      (re-find #"agent|task|delegate" tool) "agent"
      (re-find #"web|http|fetch|browser" tool) "web"
      :else "other")))

(defn- action-for [event-type]
  (case event-type
    "SessionStart" "session.started"
    "Stop" "session.stopped"
    "SubagentStop" "session.stopped"
    "SessionEnd" "session.ended"
    "UserPromptSubmit" "turn.started"
    "PreToolUse" "tool.requested"
    "PostToolUse" "tool.completed"
    "PostToolUseFailure" "tool.failed"
    "PermissionRequest" "tool.permission"
    "PreCompact" "context.compacted"
    "Notification" "attention.requested"
    nil))

(defn- outcome-for [action decision]
  (case (some-> decision str/lower-case)
    "allow" "allowed"
    "ask" "approval-needed"
    "deny" "denied"
    (case action
      "tool.completed" "completed"
      "tool.failed" "failed"
      "observed")))

(defn from-local-event
  "Derive at most one safe observation from a local hook row. Only the
  universal event-log row is used, avoiding duplicates from policy hooks."
  [{:keys [id timestamp agent hook_name event_type tool_name decision elapsed_ms]}]
  (let [agent (canonical-agent agent)
        action (action-for event_type)
        observed-at (parse-millis timestamp)]
    (when (and (= "event-log" hook_name) agent action observed-at)
      (let [category (when (str/starts-with? action "tool.")
                       (tool-category tool_name))
            outcome (outcome-for action decision)
            event-id (store/content-digest
                       (pr-str [schema-version id observed-at agent action
                                category outcome]))]
        (validate-observation!
          (cond-> {:event-id event-id
                   :schema-version schema-version
                   :observed-at observed-at
                   :agent agent
                   :action action
                   :outcome outcome}
            category (assoc :tool-category category)
            (finite-number? elapsed_ms)
            (assoc :duration-ms (max 0.0 (min 3600000.0
                                              (double elapsed_ms))))))))))
