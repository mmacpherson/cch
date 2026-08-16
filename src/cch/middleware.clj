(ns cch.middleware
  "Ring-style middleware for hook handlers.

  Each middleware wraps a handler: (fn [handler] (fn [input] result)).
  The chain is pre-composed at load time via comp for zero runtime cost."
  (:require [cch.events :as events]
            [cch.log :as log]
            [cheshire.core :as json]
            [clojure.walk :as walk]))

(def ^:const max-response-chars
  "Longest string kept inside a `tool_response` before truncation.

  Measured 2026-08-16: tool_response was 2459MB of a 5.7GB events.db,
  and the only reader anywhere is the /events detail pane. Roughly two
  thirds of it is content that already exists elsewhere — whole
  pre-edit files under Edit's :originalFile, file bodies under Read's
  :file/:content, and base64 image payloads — or is unreadable as text.

  4096 keeps the median response (881 chars) untouched and preserves
  Edit's :oldString, :newString and :structuredPatch, which average
  around 2KB and describe the change itself."
  4096)

(def ^:const verbatim-response-tools
  "Tools whose response is the only surviving record of what happened.

  Command output cannot be reconstructed from the filesystem the way a
  file body can, so it is kept whole regardless of length. Bash also
  happens to be the cheapest per row — 2.2KB average across 311k rows."
  #{"Bash"})

(defn truncate-long-strings
  "Truncate every string in `x` longer than `limit`, noting what was
  dropped. Walks nested maps and vectors, since tool responses vary in
  shape: Edit returns a flat map, Read nests under :file, image tools
  return a vector. Stating the rule rather than listing keys means new
  tool shapes are covered without an update here. Pure."
  [x limit]
  (walk/postwalk
    (fn [v]
      (if (and (string? v) (> (count v) limit))
        (str (subs v 0 limit)
             "…[cch truncated " (- (count v) limit) " chars]")
        v))
    x))

(defn prune-payload
  "Shrink a hook payload before it is persisted.

  Only `tool_response` is touched — inputs, decisions and metadata are
  small and are what queries actually read. Tools in
  `verbatim-response-tools` are exempt. Pure."
  [input]
  (if (or (contains? verbatim-response-tools (:tool_name input))
          (not (contains? input :tool_response)))
    input
    (update input :tool_response truncate-long-strings max-response-chars)))

(defn wrap-timing
  "Adds :cch/elapsed-ms to result metadata. When the handler returns nil
  (allow), wraps it in an empty map so the metadata survives — wrap-logging
  treats a result with no :decision key the same as nil."
  [handler]
  (fn [input]
    (let [start   (System/nanoTime)
          result  (handler input)
          elapsed (/ (- (System/nanoTime) start) 1e6)]
      (vary-meta (or result {}) assoc :cch/elapsed-ms elapsed))))

(defn wrap-error-handler
  "Catches exceptions and returns a deny decision with the error message."
  [handler]
  (fn [input]
    (try
      (handler input)
      (catch Exception e
        {:decision :deny
         :reason   (str "cch hook error: " (.getMessage e))}))))

(defn wrap-logging
  "Fire-and-forget event logging to SQLite, plus pub/sub publish for
  live-dashboard subscribers.

  Reads :cch/elapsed-ms from result metadata if present (set by wrap-timing).
  Degrades gracefully to nil elapsed time if timing middleware is absent.

  Captures the full input payload (minus cch's internal :cch/hook-name
  marker) as JSON in the `extra` column so every row carries
  event-specific fields that don't map to structured columns —
  trigger for PreCompact, reason for SessionEnd, prompt for
  UserPromptSubmit, etc.

  After logging, publishes the event to cch.events so SSE subscribers
  (dashboard live-stream clients) can push a card into their .event-list."
  [handler]
  (fn [input]
    (let [result    (handler input)
          extra     (json/generate-string
                      (prune-payload (dissoc input :cch/hook-name)))
          ;; SQLite-column shape — matches cch.log/query-events output
          ;; so the server's event-card renderer can consume either a
          ;; freshly-logged row or a historical one without divergence.
          pub-event {:id          nil
                     :timestamp   (str (java.time.Instant/now))
                     :agent       (or (:cch/agent input) "claude-code")
                     :session_id  (:session_id input)
                     :hook_name   (or (:cch/hook-name input)
                                      (:hook_event_name input))
                     :event_type  (or (:hook_event_name input) "PreToolUse")
                     :tool_name   (:tool_name input)
                     :file_path   (or (get-in input [:tool_input :file_path])
                                      (get-in input [:tool_params :file_path]))
                     :cwd         (:cwd input)
                     :decision    (some-> (:decision result) name)
                     :reason      (:reason result)
                     :elapsed_ms  (:cch/elapsed-ms (meta result))
                     :extra       extra}]
      (log/log-event!
        {:hook-name  (:hook_name pub-event)
         :event-type (:event_type pub-event)
         :tool-name  (:tool_name pub-event)
         :file-path  (:file_path pub-event)
         :cwd        (:cwd pub-event)
         :session-id (:session_id pub-event)
         :decision   (:decision result)
         :reason     (:reason pub-event)
         :elapsed-ms (:elapsed_ms pub-event)
         :extra      extra
         :agent      (:cch/agent input)})
      (events/publish! pub-event)
      (when (:decision result) result))))

(def default-middleware
  "Default middleware stack. Order matters — compose-middleware reverses then
  reduces, producing: logging(timing(error-handler(handler))).
  Execution flow on a call:
    logging → timing → error-handler → handler → error-handler → timing → logging
  - wrap-logging outermost: sees timing metadata on the return path, logs all
    invocations including exceptions caught by error-handler
  - wrap-timing: measures elapsed time, attaches :cch/elapsed-ms to result metadata
  - wrap-error-handler innermost: catches exceptions, returns {:decision :deny}"
  [wrap-logging
   wrap-timing
   wrap-error-handler])
