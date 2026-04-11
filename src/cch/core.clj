(ns cch.core
  "Core framework: defhook macro and middleware composition.

  defhook generates a -main function that:
    1. Reads JSON from stdin (Claude Code hook protocol)
    2. Runs the handler through the middleware chain
    3. Writes the decision to stdout (nil = allow, no output)"
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
    `(defn ~'-main [& _args#]
       (let [handler# (compose-middleware (fn ~bindings ~@body) ~middleware#)
             input#   (assoc (proto/read-input)
                             :cch/hook-name ~(str hook-name))
             result#  (handler# input#)]
         (proto/write-response! result#)))))
