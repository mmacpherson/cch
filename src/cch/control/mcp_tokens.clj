(ns cch.control.mcp-tokens
  "Provision and read the local bearer tokens that gate the shared /mcp
  endpoint. One token per caller kind (codex, claude): a caller's token both
  authenticates it and names its agent kind, which is how the shared HTTP
  transport recovers the identity the stdio child got from CCH_MCP_CALLER.

  Tokens are minted once and reused (stable, so client configs stay valid
  across restarts), stored user-only (0600) under the cch data dir. Codex
  sends its token as a bearer (`--bearer-token-env-var`); Claude Code sends
  its token as an Authorization header. See claude-code-hooks-wf5."
  (:require [babashka.fs :as fs]
            [cch.control.mcp-http :as mcp-http]
            [cheshire.core :as json])
  (:import [java.security SecureRandom]
           [java.util Base64]))

(def caller-kinds
  "Agent kinds that get a distinct token. Mirrors the CCH_MCP_CALLER values the
  stdio transport uses."
  ["codex" "claude"])

(defonce ^:private secure-random (SecureRandom.))

(defn mint-token
  "A URL-safe, unpadded 256-bit random bearer token."
  []
  (let [buf (byte-array 32)]
    (.nextBytes secure-random buf)
    (.encodeToString (.withoutPadding (Base64/getUrlEncoder)) buf)))

(defn caller->token
  "Invert a {token -> caller} map to {caller -> token}, keeping one token per
  caller. Pure — used by install to look up what to write into client config."
  [tokens]
  (reduce (fn [acc [token caller]] (assoc acc caller token)) {} tokens))

(defn token-for
  "The bearer token provisioned for a caller kind, or nil."
  [tokens caller]
  (get (caller->token tokens) caller))

(defn- write-tokens!
  "Atomically write {token -> caller} as JSON with owner-only permissions."
  [path tokens]
  (let [file (fs/file path)
        dir (fs/parent file)]
    (when dir (fs/create-dirs dir))
    (let [tmp (str path ".tmp")]
      (spit tmp (json/generate-string tokens))
      ;; Restrict before the rename so the token is never briefly world-readable
      ;; at its final path. Best-effort on non-POSIX filesystems.
      (try (fs/set-posix-file-permissions tmp "rw-------") (catch Exception _ nil))
      (fs/move tmp path {:replace-existing true}))))

(defn ensure-tokens!
  "Idempotently ensure the token file has a token for every caller kind.
  Existing tokens are preserved; only missing kinds are minted and merged in
  (so a partial or older file is topped up, never rewritten wholesale).
  Returns the resulting {token -> caller} map."
  ([] (ensure-tokens! (mcp-http/tokens-path)))
  ([path]
   (let [existing (or (mcp-http/parse-tokens
                        (when (fs/exists? path) (slurp path)))
                      {})
         present (set (vals existing))
         missing (remove present caller-kinds)
         tokens (into existing (map (fn [caller] [(mint-token) caller]) missing))]
     (when (seq missing)
       (write-tokens! path tokens))
     tokens)))
