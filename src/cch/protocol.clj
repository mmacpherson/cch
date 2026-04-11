(ns cch.protocol
  "Claude Code JSON protocol: stdin/stdout contract for hooks.

  Hooks receive JSON on stdin describing the tool call, and return
  JSON on stdout with permission decisions. This namespace handles
  the serialization boundary."
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

(defn ->response
  "Build the JSON response envelope for a hook decision.

  event-name is the hook event (e.g. \"PreToolUse\", \"PostToolUse\").
  decision is a map with:
    :decision  — :allow, :deny, or :ask
    :reason    — human-readable explanation
    :context   — (optional) additional context for Claude
    :updated-input — (optional) replacement tool_input"
  [event-name {:keys [decision reason context updated-input]}]
  (let [output (cond-> {:hookEventName            event-name
                        :permissionDecision       (name decision)
                        :permissionDecisionReason reason}
                 context       (assoc :additionalContext context)
                 updated-input (assoc :updatedInput updated-input))]
    (json/generate-string {:hookSpecificOutput output})))

(defn write-response!
  "Write a hook decision to stdout. nil decision means allow (no output).
  event-name defaults to \"PreToolUse\" if not provided."
  ([decision]
   (write-response! "PreToolUse" decision))
  ([event-name decision]
   (when decision
     (println (->response event-name decision)))))
