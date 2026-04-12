(ns hooks.event-log
  "Universal observer. Subscribes to every Claude Code event cch supports
  (24 minus WorktreeCreate and FileChanged) and does nothing except log
  the invocation via cch's existing middleware stack.

  Why the hook body returns nil: observation-only hooks must not emit
  decisions, since most non-PreToolUse events don't support them and the
  ones that do (PermissionRequest, Stop, etc.) would cause unintended
  behavior. The no-output response shape is handled by cch.protocol.

  Why it's trivial: wrap-logging already captures hook-name, event-type,
  and the full input payload (as JSON in the `extra` column). Nothing
  event-specific needs to happen in the hook body."
  (:require [cch.core :refer [defhook]]))

(defhook event-log
  "Log every Claude Code event to the cch SQLite DB."
  {}
  [_input]
  nil)
