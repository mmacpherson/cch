(ns cch.middleware
  "Ring-style middleware for hook handlers.

  Each middleware wraps a handler: (fn [handler] (fn [input] result)).
  The chain is pre-composed at load time via comp for zero runtime cost."
  (:require [cch.log :as log]))

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
  "Fire-and-forget event logging to SQLite.
  Reads :cch/elapsed-ms from result metadata if present (set by wrap-timing).
  Degrades gracefully to nil elapsed time if timing middleware is absent."
  [handler]
  (fn [input]
    (let [result (handler input)]
      (log/log-event!
        {:hook-name  (or (:cch/hook-name input)
                         (:hook_event_name input))
         :event-type (or (:hook_event_name input) "PreToolUse")
         :tool-name  (:tool_name input)
         :file-path  (or (get-in input [:tool_input :file_path])
                         (get-in input [:tool_params :file_path]))
         :cwd        (:cwd input)
         :session-id (:session_id input)
         :decision   (:decision result)
         :reason     (:reason result)
         :elapsed-ms (:cch/elapsed-ms (meta result))})
      result)))

(def default-middleware
  "Default middleware stack. Order matters:
  - wrap-timing outermost: measures total elapsed time, attaches to result metadata
  - wrap-logging: reads :cch/elapsed-ms from metadata, fires sqlite3 insert.
    Runs outside error-handler so failed hooks are still logged.
  - wrap-error-handler innermost: catches exceptions, returns {:decision :deny}"
  [wrap-timing
   wrap-logging
   wrap-error-handler])
