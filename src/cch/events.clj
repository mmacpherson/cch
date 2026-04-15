(ns cch.events
  "In-process pub/sub for the live-dashboard SSE stream.

  The dispatcher's wrap-logging middleware calls `publish!` after every
  hook invocation. The `cch.server` `/events/stream` route subscribes
  each SSE client to receive those events, formats them as Datastar
  merge-fragments frames, and writes them to the open HTTP channel.

  No persistence, no backpressure — if a subscriber is slow or dead,
  exceptions are swallowed so the publishing hot path never blocks.
  Subscribers are expected to detach via the returned unsubscribe fn
  when their connection closes.")

(defonce ^:private subscribers (atom #{}))

(defn subscribe!
  "Register a 1-arg handler called with every published event.
  Returns an unsubscribe fn; call it to detach (typically on SSE
  channel :on-close)."
  [handler-fn]
  (swap! subscribers conj handler-fn)
  (fn unsubscribe [] (swap! subscribers disj handler-fn)))

(defn publish!
  "Call every subscriber with `event`. Exceptions are swallowed — one
  slow or broken subscriber must not block the publishing thread, which
  is the hook dispatcher hot path."
  [event]
  (doseq [h @subscribers]
    (try (h event)
         (catch Exception _ nil))))

(defn subscriber-count
  "How many SSE clients are currently attached. For diagnostics / health."
  []
  (count @subscribers))
