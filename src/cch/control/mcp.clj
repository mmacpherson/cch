(ns cch.control.mcp
  "PluMCP stdio facade for cch's native session directory and router."
  (:require [cch.control.codex-binding :as codex-binding]
            [cch.control.core :as control]
            [cheshire.core :as json]
            [clojure.string :as str]
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
  #{:target :route :message :message_id :message-id
    :source_proof :source-proof})

(defn- runtime-key?
  "Recognize only the internal metadata field attached by Codex's code-mode
  MCP bridge. Depending on the bridge path it may retain a namespace or arrive
  as a non-keyword named key. Its value is always discarded."
  [key]
  (and (or (keyword? key) (symbol? key) (string? key))
       (= "runtime" (name key))))

(defn- reject-duplicate-aliases!
  [tool-name arguments aliases label]
  (when (< 1 (count (filter #(contains? arguments %) aliases)))
    (throw (ex-info (str tool-name " accepts exactly one " label " field")
                    {:type :unsupported-control-input
                     :fields (mapv name aliases)}))))

(defn- validate-send-arguments!
  "Fail closed when a client attempts to smuggle a second protocol through
  the generic message tool. Provider credentials, permission replies, raw
  frames, and command input are not fields in this capability."
  [arguments caller]
  (let [unsupported (seq (remove #(or (contains? send-message-keys %)
                                       (runtime-key? %))
                                 (keys arguments)))
        route (:route arguments)]
    (when unsupported
      (throw (ex-info (str "send_message accepts only its documented text envelope; "
                           "unsupported fields: "
                           (str/join ", " (sort (map name unsupported))))
                      {:type :unsupported-control-input
                       :fields (vec (sort (map name unsupported)))})))
    (reject-duplicate-aliases! "send_message" arguments
                               [:target :route] "destination")
    (reject-duplicate-aliases! "send_message" arguments
                               [:message_id :message-id] "message id")
    (reject-duplicate-aliases! "send_message" arguments
                               [:source_proof :source-proof] "source proof")
    (when (and (not= "codex" caller)
               (some #(contains? arguments %) [:source_proof :source-proof]))
      (throw (ex-info "source_proof is reserved for the trusted Codex hook"
                      {:type :unsupported-control-input
                       :fields ["source_proof"]})))
    (cond-> (apply dissoc arguments
                   :route :message-id :source-proof
                   (filter runtime-key? (keys arguments)))
      route (assoc :target route)
      (contains? arguments :message-id)
      (assoc :message_id (:message-id arguments))
      (contains? arguments :source-proof)
      (assoc :source_proof (:source-proof arguments)))))

(defn- validate-alias-arguments! [arguments caller]
  (let [supported #{:alias :source_proof :source-proof}
        unsupported (seq (remove #(or (contains? supported %)
                                       (runtime-key? %))
                                 (keys arguments)))]
    (when unsupported
      (throw (ex-info
               (str "set_session_alias accepts only alias; unsupported fields: "
                    (str/join ", " (sort (map name unsupported))))
               {:type :unsupported-control-input
                :fields (vec (sort (map name unsupported)))})))
    (reject-duplicate-aliases! "set_session_alias" arguments
                               [:source_proof :source-proof] "source proof")
    (when (and (not= "codex" caller)
               (some #(contains? arguments %) [:source_proof :source-proof]))
      (throw (ex-info "source_proof is reserved for the trusted Codex hook"
                      {:type :unsupported-control-input
                       :fields ["source_proof"]})))
    (cond-> (apply dissoc arguments :source-proof
                   (filter runtime-key? (keys arguments)))
      (contains? arguments :source-proof)
      (assoc :source_proof (:source-proof arguments)))))

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

#_{:clj-kondo/ignore [:unused-binding]}
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
        arguments (validate-send-arguments! arguments caller)
        {:keys [target message message_id source_proof]} arguments
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

#_{:clj-kondo/ignore [:unused-binding]}
(defn ^{:mcp-name "set_session_alias" :mcp-type :tool}
  set-session-alias
  "Set or clear a short broker-visible alias for your current session. This is
  presentation metadata only; never use secrets, filesystem paths, repository
  names, client names, or other private context. Pass an empty string to clear."
  [{:keys [^{:doc "Short broker-visible alias; empty text clears it"
             :type "string"}
           alias
           ^{:doc "Reserved Codex hook proof; callers must not supply this field"
             :type "string" :required? false}
           source_proof]
    :as arguments}]
  (let [caller (caller-agent)
        arguments (validate-alias-arguments! arguments caller)
        {:keys [alias source_proof]} arguments
        route-id (case caller
                   "codex" (codex-binding/claim-alias-source!
                             {:source-proof source_proof :alias alias})
                   "claude" (control/inferred-source)
                   (throw (ex-info "Session aliases require a native agent caller"
                                   {:type :unbound-session-source})))]
    (tool-result (control/set-session-alias! {:route-id route-id
                                              :alias alias}))))

(def tools
  [(vs/make-tool-from-var #'list-sessions)
   (vs/make-tool-from-var #'get-session)
   (vs/make-tool-from-var #'send-message)
   (vs/make-tool-from-var #'set-session-alias)])

(def server-options
  (ss/make-server-options
    {:primitives {:tools tools}
     :info (es/make-info "cch native control plane" "0.2.0"
                         "Local native Claude/Codex session routing")}))

(defn -main [& _]
  (ms/run-mcp-server (merge {:transport :stdio} server-options)))
