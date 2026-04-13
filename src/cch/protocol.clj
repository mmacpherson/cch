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
  | SessionStart (with :context)                 | hookSpecificOutput.additionalContext             |
  | UserPromptSubmit (context-only, no decision) | hookSpecificOutput.additionalContext             |
  | any event with :hook-specific-output         | pass-through escape hatch                        |
  | everything else                              | no output (nil); events support observation only |

  Escape hatch: a hook that returns {:hook-specific-output {...}} gets its
  map emitted verbatim under hookSpecificOutput (with hookEventName auto-
  populated). Takes precedence over :decision when both are present. Use
  for events whose shape isn't modeled here — Elicitation responses,
  future event types, experimental additionalContext on events like
  FileChanged.

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

(defn- dispatch
  "Route an event + decision-map to a response-shape key. Takes both
  args because some shapes are content-triggered (escape hatch, context-
  only additionalContext) rather than event-name-only."
  [event-name decision-map]
  (cond
    ;; Explicit escape hatch wins over everything — the hook author
    ;; asked for a literal shape, honor it even on events we model.
    (:hook-specific-output decision-map)
    ::escape-hatch

    ;; SessionStart's only documented output is additionalContext; :decision
    ;; alone still emits nothing (SessionStart has no block/allow semantics).
    (and (= event-name "SessionStart") (:context decision-map))
    ::additional-context

    ;; UserPromptSubmit can inject context without blocking. If there's a
    ;; :decision, the top-level-decision shape handles context alongside it;
    ;; this branch covers the context-only case.
    (and (= event-name "UserPromptSubmit")
         (nil? (:decision decision-map))
         (:context decision-map))
    ::additional-context

    (= event-name "PreToolUse")             ::pretooluse
    (= event-name "PermissionRequest")      ::permission-request
    (top-level-decision-events event-name)  ::top-level-decision
    :else                                   ::no-output))

(defmulti ^:private ->response-map
  "Build the response map (or nil) for a hook decision.
  Returns a JSON-serializable map, or nil to emit no output.
  Dispatches on the event + decision-map content (see `dispatch`)."
  dispatch)

(defmethod ->response-map ::escape-hatch
  [event-name {:keys [hook-specific-output]}]
  ;; Pass user-supplied map through verbatim; auto-populate hookEventName
  ;; only if the hook didn't set it (let explicit wins).
  {:hookSpecificOutput
   (merge {:hookEventName event-name} hook-specific-output)})

(defmethod ->response-map ::additional-context
  [event-name {:keys [context]}]
  {:hookSpecificOutput
   {:hookEventName     event-name
    :additionalContext context}})

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
