(ns cli.registry
  "Registry of available hooks with their metadata.
  Only lists hooks that have implementations under src/hooks/.

  Planned (not yet implemented):
  - protect-files: hard-deny edits to .env, secrets, keys
  - command-audit: log all Bash commands (PostToolUse)
  - format-on-save: run formatters after writes (PostToolUse)
  - slow-confirm: prompt for destructive commands")

(def hooks
  "Built-in hooks and their configuration."
  {"scope-lock"    {:ns          "hooks.scope-lock"
                    :event       "PreToolUse"
                    :matcher     "Edit|Write"
                    :description "Enforces file edit scope per git worktree"}
   "command-audit" {:ns          "hooks.command-audit"
                    :event       "PostToolUse"
                    :matcher     "Bash"
                    :description "Logs Bash commands; flags configured regex patterns"}})

(defn get-hook
  "Look up a hook by name. Returns nil if not found."
  [name]
  (get hooks name))

(defn list-hooks
  "Return all hooks as a seq of [name metadata] pairs."
  []
  (sort-by first hooks))
