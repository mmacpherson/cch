(ns cch.control.codex-binding
  "Trusted Codex PreToolUse-to-MCP caller binding.

  Codex supplies the native session and tool-call ids to the hook. The hook
  records that observation locally and injects only the opaque tool-call id
  into the pending MCP arguments. The MCP process later consumes the binding;
  the model never supplies the trusted source route."
  (:require [cch.control.store :as store]
            [clojure.string :as str]))

(def ^:const send-tool-name "mcp__cch__send_message")
(def ^:const alias-tool-name "mcp__cch__set_session_alias")
(def ^:const tool-name send-tool-name)
(def ^:const tool-matcher
  "mcp__cch__(send_message|set_session_alias)")
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
  "Bind one identity-sensitive Codex MCP invocation to the native session
  observed by PreToolUse. The proof is one-time and payload-bound."
  [payload]
  (let [{:keys [hook_event_name session_id tool_name tool_use_id tool_input]}
        payload]
    (when-not (= "PreToolUse" hook_event_name)
      (throw (ex-info "Codex source binding requires PreToolUse"
                      {:type :invalid-codex-binding-hook})))
    (when-not (contains? #{send-tool-name alias-tool-name} tool_name)
      (throw (ex-info "Codex source binding received an unexpected tool"
                      {:type :invalid-codex-binding-hook :tool tool_name})))
    (doseq [[label value] [["session_id" session_id]
                           ["tool_use_id" tool_use_id]]]
      (when-not (and (string? value) (not (str/blank? value)))
        (throw (ex-info (str label " is required for Codex source binding")
                        {:type :invalid-codex-binding-hook :field label}))))
    (let [updated-input
          (case tool_name
            "mcp__cch__send_message"
            (let [{:keys [message message_id]} tool_input
                  target (destination tool_input)]
              (doseq [[label value] [["target" target] ["message" message]]]
                (when-not (and (string? value) (not (str/blank? value)))
                  (throw
                    (ex-info (str label " is required for Codex source binding")
                             {:type :invalid-codex-binding-hook :field label}))))
              (store/record-codex-binding! {:tool-use-id tool_use_id
                                            :session-id session_id
                                            :target target
                                            :message-id message_id
                                            :message message})
              (-> tool_input
                  (dissoc :route :runtime)
                  (assoc :target target proof-key tool_use_id)))

            "mcp__cch__set_session_alias"
            (let [alias (:alias tool_input)]
              (when-not (string? alias)
                (throw (ex-info "alias is required for Codex source binding"
                                {:type :invalid-codex-binding-hook
                                 :field "alias"})))
              (store/record-codex-alias-binding!
                {:tool-use-id tool_use_id
                 :session-id session_id
                 :alias alias})
              (-> tool_input
                  (dissoc :runtime)
                  (assoc proof-key tool_use_id))))]
      {:hookSpecificOutput
       {:hookEventName "PreToolUse"
        :permissionDecision "allow"
        :updatedInput updated-input}})))

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

(defn claim-alias-source!
  "Resolve and consume a hook-injected proof for a Codex self-alias call."
  [{:keys [source-proof alias]}]
  (or (and (not (str/blank? source-proof))
           (store/claim-codex-alias-binding!
             {:tool-use-id source-proof :alias alias}))
      (throw
        (ex-info
          (str "Codex caller identity is missing, expired, mismatched, or already used. "
               "Run `cch control install`, restart the Codex session, and retry.")
          {:type :unbound-codex-source}))))
