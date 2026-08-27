(ns cch.control.mcp
  "PluMCP stdio facade for cch's native session directory and router."
  (:require [cch.control.codex-binding :as codex-binding]
            [cch.control.core :as control]
            [cheshire.core :as json]
            [plumcp.core.api.entity-gen :as eg]
            [plumcp.core.api.entity-support :as es]
            [plumcp.core.api.mcp-server :as ms]
            [plumcp.core.impl.var-support :as vs]
            [plumcp.core.server.server-support :as ss]))

(defn- tool-result [value]
  (-> value json/generate-string eg/make-text-content vector
      eg/make-call-tool-result))

(defn caller-agent []
  (System/getenv "CCH_MCP_CALLER"))

(def ^:private send-message-keys
  #{:target :message :message_id :source_proof})

(defn- validate-send-arguments!
  "Fail closed when a client attempts to smuggle a second protocol through
  the generic message tool. Provider credentials, permission replies, raw
  frames, and command input are not fields in this capability."
  [arguments caller]
  (let [unsupported (seq (remove send-message-keys (keys arguments)))]
    (when unsupported
      (throw (ex-info "send_message accepts only its documented text envelope"
                      {:type :unsupported-control-input
                       :fields (vec (sort (map name unsupported)))})))
    (when (and (not= "codex" caller) (contains? arguments :source_proof))
      (throw (ex-info "source_proof is reserved for the trusted Codex hook"
                      {:type :unsupported-control-input
                       :fields ["source_proof"]})))
    arguments))

(defn ^{:mcp-name "list_sessions" :mcp-type :tool}
  list-sessions
  "List active native Claude and Codex sessions. Credentials and transcript
  content are never returned. Adapter failures appear in the errors array."
  [_]
  (tool-result (control/list-sessions)))

(defn ^{:mcp-name "get_session" :mcp-type :tool}
  get-session
  "Get sanitized presence metadata for one cch route id."
  [{:keys [^{:doc "Route id such as claude:<uuid> or codex:<uuid>"
             :type "string"}
           session_id]}]
  (tool-result {:session (control/get-session session_id)}))

(defn ^{:mcp-name "send_message" :mcp-type :tool}
  send-message
  "Send plain text to a native agent inbox. This API cannot carry approval,
  permission, control, or arbitrary transport frames. Reusing message_id with
  the same request is idempotent."
  [{:keys [^{:doc "Destination route id" :type "string"}
           target
           ^{:doc "Plain-text message (maximum 32768 UTF-8 bytes)" :type "string"}
           message
           ^{:doc "Stable UUID-like id for retries" :type "string" :required? false}
           message_id
           ^{:doc "Reserved Codex hook proof; callers must not supply this field"
             :type "string" :required? false}
           source_proof]
    :as arguments}]
  (let [caller (caller-agent)
        _ (validate-send-arguments! arguments caller)
        source (case caller
                 "codex" (codex-binding/claim-source!
                           {:source-proof source_proof
                            :target target
                            :message-id message_id
                            :message message})
                 ;; Claude exposes its session identity to the MCP child.
                 ;; Unknown/manual callers retain the explicit operator path.
                 nil)]
    (tool-result
      (control/send-message! {:target target :message message
                              :message-id message_id :source source}))))

(def tools
  [(vs/make-tool-from-var #'list-sessions)
   (vs/make-tool-from-var #'get-session)
   (vs/make-tool-from-var #'send-message)])

(def server-options
  (ss/make-server-options
    {:primitives {:tools tools}
     :info (es/make-info "cch native control plane" "0.1.0"
                         "Local native Claude/Codex session routing")}))

(defn -main [& _]
  (ms/run-mcp-server (merge {:transport :stdio} server-options)))
