(ns cch.control.mcp-http
  "Shared HTTP transport for the cch MCP tools.

  The stdio child (`cch control mcp`) loads a full copy of the control plane
  per session — one ~200MB JVM per agent thread. This mounts the same four
  tools once, on the long-running `cch serve` JVM, as an authenticated
  streamable-HTTP MCP endpoint. Both Codex (`--url` + bearer token) and Claude
  Code (`--transport http` + header) can point at it, so N sessions share one
  server instead of spawning N JVMs. See claude-code-hooks-wf5.

  Security: the endpoint rides the same httpkit listener as the dispatch API,
  which is reachable by any local process, so every call MUST carry a valid
  bearer token. The token both authenticates the caller and names its
  agent kind (codex/claude), which the plumcp tools read via
  `mcp/*caller-override*`. Session identity (Codex's source_proof) still rides
  in the tool arguments and is unaffected by the transport. Parsing/decision
  logic here is pure; only `load-tokens` and `handle` touch the world."
  (:require [cch.control.mcp :as mcp]
            [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [plumcp.core.deps.runtime :as rt]
            [plumcp.core.server.http-ring :as hring]))

(def default-port
  "Matches cch.server's default port; the dispatch hooks target the same host."
  8888)

(defn endpoint-url
  "The URL agents point their MCP client at. Loopback by default (the shared
  server binds all interfaces, but clients should reach it over 127.0.0.1);
  overridable via CCH_MCP_URL for non-default deployments."
  []
  (or (not-empty (System/getenv "CCH_MCP_URL"))
      (str "http://127.0.0.1:" default-port "/mcp")))

;; --- Token store (token -> caller-agent) ---

(defn tokens-path
  "Path to the local MCP token file. Overridable via CCH_MCP_TOKENS_PATH;
  otherwise under the cch data dir. The file is provisioned in a later phase;
  until it exists the endpoint is inert (503), so mounting the route exposes
  nothing on its own."
  []
  (or (not-empty (System/getenv "CCH_MCP_TOKENS_PATH"))
      (str (or (not-empty (System/getenv "XDG_DATA_HOME"))
               (str (System/getProperty "user.home") "/.local/share"))
           "/cch/mcp-tokens.json")))

(defn parse-tokens
  "Parse the token-file contents into a {token -> caller} map. Accepts a JSON
  object mapping token strings to caller kinds ({\"abc\":\"codex\"}). Blank or
  malformed content yields nil (endpoint stays inert). Pure."
  [contents]
  (when-not (str/blank? contents)
    (try
      (let [parsed (json/parse-string contents)]
        (when (map? parsed)
          (let [pairs (for [[token caller] parsed
                            :when (and (string? token) (not (str/blank? token))
                                       (string? caller) (not (str/blank? caller)))]
                        [token caller])]
            (not-empty (into {} pairs)))))
      (catch Exception _ nil))))

(defn load-tokens
  "Read and parse the token file. nil when absent/blank/malformed."
  []
  (let [f (io/file (tokens-path))]
    (when (.isFile f)
      (parse-tokens (slurp f)))))

;; --- Auth (pure) ---

(defn bearer-token
  "Extract the bearer token from a Ring request's Authorization header.
  Case-insensitive scheme; header lookup tolerant of casing. Pure."
  [req]
  (let [headers (:headers req)
        auth (or (get headers "authorization")
                 (get headers "Authorization"))]
    (when (string? auth)
      (let [[scheme value] (str/split (str/trim auth) #"\s+" 2)]
        (when (and value (= "bearer" (str/lower-case scheme)))
          (not-empty (str/trim value)))))))

(defn resolve-caller
  "Return the caller-agent for a request given a {token -> caller} map, or nil
  when the bearer token is missing or unknown. Pure."
  [tokens req]
  (when tokens
    (get tokens (bearer-token req))))

;; --- Ring handler (plumcp streamable-HTTP transport) ---

(def ^:private ring-handler
  ;; make-server-options already yields :runtime and :jsonrpc-handler; feed
  ;; them to plumcp's streamable-HTTP ring handler. wrap-request-body-reader
  ;; lets the handler read the POST body under a plain ring adapter (httpkit).
  (delay
    (-> (hring/make-ring-handler (:runtime mcp/server-options)
                                 (:jsonrpc-handler mcp/server-options)
                                 :uri-set #{"/mcp" "/mcp/"})
        (hring/wrap-request-body-reader))))

(defn handle
  "Handle an authenticated MCP request on /mcp. 503 until tokens are
  provisioned; 401 on a missing/unknown bearer token; otherwise binds the
  caller kind and delegates to the plumcp MCP handler. The 2-arity takes an
  explicit delegate for testing."
  ([req] (handle req @ring-handler))
  ([req delegate]
   (if-let [tokens (load-tokens)]
     (if-let [caller (resolve-caller tokens req)]
       ;; Carry the caller in plumcp's per-request runtime bag rather than a
       ;; thread-local binding: plumcp dispatches the tool on a session-worker
       ;; thread, so a binding here would not reach it. mcp/caller-binding-
       ;; methods-wrapper reads this key and binds *caller-override* there.
       (delegate (rt/upsert-runtime req {mcp/runtime-caller-key caller}))
       {:status 401
        :headers {"WWW-Authenticate" "Bearer" "Content-Type" "text/plain"}
        :body "unauthorized"})
     {:status 503
      :headers {"Content-Type" "text/plain"}
      :body "cch mcp endpoint not configured"})))
