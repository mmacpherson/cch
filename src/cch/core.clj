(ns cch.core
  "Core framework: defhook macro and middleware composition.

  defhook generates three artifacts in one shot:
    1. handler-fn   — the raw check function (for in-process dispatch / REPL)
    2. composed     — handler-fn wrapped in the middleware chain (logging,
                      timing, error handling). Used by both command and HTTP modes.
    3. -main        — command-mode entry point (stdin → stdout). Reads JSON
                      from stdin, runs through `composed`, writes response."
  (:require [cch.protocol :as proto]
            [cch.middleware :as mw]))

(defn compose-middleware
  "Pre-compose a middleware stack around a handler. Returns a single fn."
  [handler middleware]
  (reduce (fn [h mw-fn] (mw-fn h)) handler (reverse middleware)))

(defmacro defhook
  "Define a Claude Code hook with automatic protocol handling.

  The body receives the parsed input map and should return:
    nil                              — allow (no output, fastest path)
    {:decision :deny|:ask :reason s} — deny or prompt user

  Options map:
    :middleware — custom middleware vec (default: cch.middleware/default-middleware)

  Generates three defs in the current namespace:
    handler-fn  — your raw body as a function (for REPL / in-process dispatch)
    composed    — handler-fn wrapped in the middleware chain (shared by
                  command and HTTP dispatch paths)
    -main       — command-mode entry point used by `bb -m <ns>`

  Hook wiring metadata (event type, matcher pattern) lives in
  cli/registry.clj — the single source of truth for installation.

  Example:
    (defhook my-hook
      \"Blocks edits to secret files.\"
      {}
      [input]
      (when (secret-file? (get-in input [:tool_input :file_path]))
        {:decision :deny :reason \"Cannot edit secret files\"}))"
  [hook-name _docstring opts bindings & body]
  (let [middleware# (:middleware opts `mw/default-middleware)]
    `(do
       ;; Raw handler — the user's body, no middleware, no protocol. Useful
       ;; for REPL work and direct invocation from the HTTP server.
       (defn ~'handler-fn ~bindings ~@body)

       ;; Composed handler — wraps handler-fn in the middleware chain.
       ;; Both -main (command mode) and cch.server (HTTP mode) go through
       ;; this, so logging / timing / error handling are identical.
       (def ~'composed (compose-middleware ~'handler-fn ~middleware#))

       ;; Command-mode entry point: read JSON from stdin, run through the
       ;; composed handler, write JSON to stdout.
       (defn ~'-main [& _args#]
         (let [raw-input#  (proto/read-input)
               event-name# (or (:hook_event_name raw-input#) "PreToolUse")
               input#      (assoc raw-input# :cch/hook-name ~(str hook-name))
               result#     (~'composed input#)]
           (proto/write-response! event-name# result#))))))
