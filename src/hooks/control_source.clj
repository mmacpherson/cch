(ns hooks.control-source
  "PreToolUse hook that binds a Claude session's cch MCP calls to that
  session.

  Over the shared HTTP transport the cch MCP server runs in the long-lived
  dispatcher, not in a child of the Claude session, so it cannot read the
  session's identity from its environment. Claude Code gives this hook the
  session and tool-call ids. The hook records the observation locally, then
  rewrites the pending input to carry only the opaque tool-call id as a
  one-time proof, which the MCP handler consumes to learn the sender. The model
  never supplies the source route.

  Codex runs its own binding hook, so this one acts only on Claude payloads."
  (:require [cch.control.codex-binding :as binding]
            [cch.core :refer [defhook]]))

(def ^:private bound-tools
  #{binding/send-tool-name binding/alias-tool-name})

(defn applies?
  "True for a Claude Code PreToolUse of an identity-sensitive cch tool."
  [{:keys [hook_event_name tool_name] :as input}]
  (and (= "PreToolUse" hook_event_name)
       (= "claude-code" (:cch/agent input "claude-code"))
       (contains? bound-tools tool_name)))

(defhook control-source
  "Binds Claude cch MCP calls to the calling session."
  {}
  [input]
  (when (applies? input)
    {:decision :allow
     :reason "cch bound the calling session to this message"
     :updated-input (binding/bind-for! "claude" input)}))
