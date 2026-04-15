(ns cch.middleware
  "Ring-style middleware for hook handlers.

  Each middleware wraps a handler: (fn [handler] (fn [input] result)).
  The chain is pre-composed at load time via comp for zero runtime cost."
  (:require [cch.events :as events]
            [cch.log :as log]
            [cheshire.core :as json]))

(defn wrap-timing
  "Adds :cch/elapsed-ms to result metadata. Preserves nil (allow)."
  [handler]
  (fn [input]
    (let [start   (System/nanoTime)
          result  (handler input)
          elapsed (/ (- (System/nanoTime) start) 1e6)]
      (if result
        (vary-meta result assoc :cch/elapsed-ms elapsed)
        result))))

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
          extra     (json/generate-string (dissoc input :cch/hook-name))
          ;; SQLite-column shape — matches cch.log/query-events output
          ;; so the server's event-card renderer can consume either a
          ;; freshly-logged row or a historical one without divergence.
          pub-event {:id          nil
                     :timestamp   (str (java.time.Instant/now))
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
         :extra      extra})
      (events/publish! pub-event)
      result)))

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
