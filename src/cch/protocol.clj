(ns cch.protocol
  "Claude Code JSON protocol: stdin/stdout contract for hooks.

  Hooks read JSON on stdin and write JSON (or nothing) on stdout. Different
  Claude Code events use different response schemas — this namespace knows
  the mapping from event to shape.

  Response-shape groups:

  | Event(s)                                     | Shape                                            |
  |----------------------------------------------|--------------------------------------------------|
  | PreToolUse                                   | hookSpecificOutput.permissionDecision            |
  | PostToolUse, PostToolUseFailure, Stop,       |                                                  |
  |   SubagentStop, UserPromptSubmit,            | top-level {decision, reason}                     |
  |   ConfigChange, TaskCreated, TaskCompleted   |                                                  |
  | PermissionRequest                            | hookSpecificOutput.decision.{behavior, ...}      |
  | everything else (SessionStart, Notification, | no output (nil); events support observation only |
  |   PreCompact, CwdChanged, FileChanged, ...)  |                                                  |

  WorktreeCreate's path-on-stdout output is intentionally unsupported by the
  generic renderer — a WorktreeCreate hook would need a bespoke main that
  prints its path directly."
  (:require [cheshire.core :as json]))

(defn read-input
  "Read and parse JSON from stdin. Returns a keyword-keyed map."
  []
  (json/parse-string (slurp *in*) true))

(defn extract-file-path
  "Extract file_path from hook input, checking both key conventions."
  [input]
  (or (get-in input [:tool_input :file_path])
      (get-in input [:tool_params :file_path])))

;; --- Event → shape dispatch ---

(def ^:private top-level-decision-events
  "Events whose response uses top-level {decision, reason}."
  #{"PostToolUse" "PostToolUseFailure" "Stop" "SubagentStop"
    "UserPromptSubmit" "ConfigChange" "TaskCreated" "TaskCompleted"})

(defn- event-shape
  "Classify an event name into a response-shape group."
  [event-name]
  (cond
    (= event-name "PreToolUse")             ::pretooluse
    (= event-name "PermissionRequest")      ::permission-request
    (top-level-decision-events event-name)  ::top-level-decision
    :else                                   ::no-output))

(defmulti ^:private ->response-map
  "Build the response map (or nil) for a hook decision.
  Returns a JSON-serializable map, or nil to emit no output.
  Dispatches on the event's response-shape group."
  (fn [event-name _decision] (event-shape event-name)))

(defmethod ->response-map ::pretooluse
  [event-name {:keys [decision reason context updated-input]}]
  {:hookSpecificOutput
   (cond-> {:hookEventName            event-name
            :permissionDecision       (name decision)
            :permissionDecisionReason reason}
     context       (assoc :additionalContext context)
     updated-input (assoc :updatedInput updated-input))})

(defmethod ->response-map ::top-level-decision
  [event-name {:keys [decision reason context]}]
  ;; Claude Code's top-level decision schema uses "block" as the only
  ;; meaningful value across these events. Normalize :deny and :block to
  ;; "block"; keep the literal name for anything else so callers that
  ;; pass through non-standard values get an obvious mismatch rather
  ;; than silent coercion.
  (let [decision-str (if (#{:deny :block} decision) "block" (name decision))
        base         {:decision decision-str :reason reason}]
    (cond-> base
      context
      (assoc :hookSpecificOutput
             {:hookEventName     event-name
              :additionalContext context}))))

(defmethod ->response-map ::permission-request
  [event-name {:keys [decision reason updated-input updated-permissions]}]
  (let [behavior (if (= decision :allow) "allow" "deny")
        inner    (cond-> {:behavior behavior}
                   (and (= behavior "deny") reason) (assoc :message reason)
                   updated-input                    (assoc :updatedInput updated-input)
                   updated-permissions              (assoc :updatedPermissions
                                                           updated-permissions))]
    {:hookSpecificOutput
     {:hookEventName event-name
      :decision      inner}}))

(defmethod ->response-map ::no-output
  [_event-name _decision]
  nil)

;; --- Public API ---

(defn ->response
  "Build the JSON response string for a hook decision.
  Returns nil when:
    - decision is nil (pure allow / no-op)
    - the event type has no output shape (SessionStart, Notification, etc.)"
  [event-name decision]
  (when decision
    (when-let [m (->response-map event-name decision)]
      (json/generate-string m))))

(defn write-response!
  "Write a hook decision to stdout. No output when response is nil.
  event-name defaults to PreToolUse for backward compatibility."
  ([decision]
   (write-response! "PreToolUse" decision))
  ([event-name decision]
   (when-let [out (->response event-name decision)]
     (println out))))
