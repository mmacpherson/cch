(ns cch.control.codex-binding
  "Trusted PreToolUse-to-MCP caller binding for Codex and Claude.

  The provider supplies the native session and tool-call ids to the hook. The hook
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

(defn bind-for!
  "Bind one identity-sensitive cch MCP invocation to the native session a
  trusted PreToolUse hook observed. agent is the provider family (\"codex\" or
  \"claude\") and prefixes the bound route. Returns the tool input to forward,
  with the one-time, payload-bound proof injected."
  [agent payload]
  (let [{:keys [hook_event_name session_id tool_name tool_use_id tool_input]}
        payload]
    (when-not (= "PreToolUse" hook_event_name)
      (throw (ex-info "Source binding requires PreToolUse"
                      {:type :invalid-codex-binding-hook})))
    (when-not (contains? #{send-tool-name alias-tool-name} tool_name)
      (throw (ex-info "Source binding received an unexpected tool"
                      {:type :invalid-codex-binding-hook :tool tool_name})))
    (doseq [[label value] [["session_id" session_id]
                           ["tool_use_id" tool_use_id]]]
      (when-not (and (string? value) (not (str/blank? value)))
        (throw (ex-info (str label " is required for source binding")
                        {:type :invalid-codex-binding-hook :field label}))))
    (case tool_name
      "mcp__cch__send_message"
      (let [{:keys [message message_id]} tool_input
            target (destination tool_input)]
        (doseq [[label value] [["target" target] ["message" message]]]
          (when-not (and (string? value) (not (str/blank? value)))
            (throw
              (ex-info (str label " is required for source binding")
                       {:type :invalid-codex-binding-hook :field label}))))
        (store/record-codex-binding! {:agent agent
                                      :tool-use-id tool_use_id
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
          (throw (ex-info "alias is required for source binding"
                          {:type :invalid-codex-binding-hook
                           :field "alias"})))
        (store/record-codex-alias-binding!
          {:agent agent
           :tool-use-id tool_use_id
           :session-id session_id
           :alias alias})
        (-> tool_input
            (dissoc :runtime)
            (assoc proof-key tool_use_id))))))

(defn bind!
  "Bind one identity-sensitive Codex MCP invocation to the native session
  observed by PreToolUse. The proof is one-time and payload-bound."
  [payload]
  {:hookSpecificOutput
   {:hookEventName "PreToolUse"
    :permissionDecision "allow"
    :updatedInput (bind-for! "codex" payload)}})

(defn- unbound [agent]
  (ex-info
    (if (= "claude" agent)
      (str "Claude caller identity is missing, expired, mismatched, or already used. "
           "Make sure cch's hook dispatcher is running and the control-source hook "
           "is enabled, then retry.")
      (str "Codex caller identity is missing, expired, mismatched, or already used. "
           "Run `cch control install`, restart the Codex session, and retry."))
    {:type :unbound-codex-source :agent agent}))

(defn- own-family [agent source]
  (when (and source (str/starts-with? source (str agent ":")))
    source))

(defn claim-source!
  "Resolve and consume the hook-injected proof for an MCP envelope. The proof
  must resolve to a route of the caller's own provider family."
  [{:keys [source-proof target message-id message agent] :or {agent "codex"}}]
  (or (and (not (str/blank? source-proof))
           (own-family agent
                       (store/claim-codex-binding! {:tool-use-id source-proof
                                                    :target target
                                                    :message-id message-id
                                                    :message message})))
      (throw (unbound agent))))

(defn claim-alias-source!
  "Resolve and consume a hook-injected proof for a self-alias call."
  [{:keys [source-proof alias agent] :or {agent "codex"}}]
  (or (and (not (str/blank? source-proof))
           (own-family agent
                       (store/claim-codex-alias-binding!
                         {:tool-use-id source-proof :alias alias})))
      (throw (unbound agent))))
