(ns cch.control.codex-binding
  "Trusted Codex PreToolUse-to-MCP caller binding.

  Codex supplies the native session and tool-call ids to the hook. The hook
  records that observation locally and injects only the opaque tool-call id
  into the pending MCP arguments. The MCP process later consumes the binding;
  the model never supplies the trusted source route."
  (:require [cch.control.store :as store]
            [clojure.string :as str]))

(def ^:const tool-name "mcp__cch__send_message")
(def ^:const proof-key :source_proof)

(defn- destination
  "Normalize Codex's observed `route` alias without allowing an ambiguous
  envelope. Codex 0.149 may emit `route` for an MCP argument advertised as
  `target`; the trusted hook sees that rewritten input before the MCP server."
  [{:keys [target route]}]
  (when (and target route)
    (throw (ex-info "Codex source binding requires exactly one destination field"
                    {:type :invalid-codex-binding-hook
                     :fields [:target :route]})))
  (or target route))

(defn bind!
  "Record one Codex send_message invocation and return the supported
  PreToolUse updatedInput response. Throws on any unexpected hook shape so the
  eventual MCP call fails closed without a valid proof."
  [payload]
  (let [{:keys [hook_event_name session_id tool_name tool_use_id tool_input]}
        payload
        {:keys [message message_id]} tool_input
        target (destination tool_input)]
    (when-not (= "PreToolUse" hook_event_name)
      (throw (ex-info "Codex source binding requires PreToolUse"
                      {:type :invalid-codex-binding-hook})))
    (when-not (= tool-name tool_name)
      (throw (ex-info "Codex source binding received an unexpected tool"
                      {:type :invalid-codex-binding-hook :tool tool_name})))
    (doseq [[label value] [["session_id" session_id]
                           ["tool_use_id" tool_use_id]
                           ["target" target]
                           ["message" message]]]
      (when-not (and (string? value) (not (str/blank? value)))
        (throw (ex-info (str label " is required for Codex source binding")
                        {:type :invalid-codex-binding-hook :field label}))))
    (store/record-codex-binding! {:tool-use-id tool_use_id
                                  :session-id session_id
                                  :target target
                                  :message-id message_id
                                  :message message})
    {:hookSpecificOutput
     {:hookEventName "PreToolUse"
      :permissionDecision "allow"
      :updatedInput (-> tool_input
                        ;; Codex's code-mode bridge attaches non-protocol
                        ;; execution metadata to nested MCP calls.
                        (dissoc :route :runtime)
                        (assoc :target target proof-key tool_use_id))}}))

(defn claim-source!
  "Resolve and consume the hook-injected proof for an MCP envelope."
  [{:keys [source-proof target message-id message]}]
  (or (and (not (str/blank? source-proof))
           (store/claim-codex-binding! {:tool-use-id source-proof
                                        :target target
                                        :message-id message-id
                                        :message message}))
      (throw
        (ex-info
          (str "Codex caller identity is missing, expired, mismatched, or already used. "
               "Run `cch control install`, restart the Codex session, and retry.")
          {:type :unbound-codex-source}))))
