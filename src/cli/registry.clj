(ns cli.registry
  "Registry of available hooks with their metadata.")

(def hooks
  "Built-in hooks and their configuration."
  {"scope-lock"    {:ns         "hooks.scope-lock"
                    :event      "PreToolUse"
                    :matcher    "Edit|Write"
                    :description "Enforces file edit scope per git worktree"}
   "protect-files" {:ns         "hooks.protect-files"
                    :event      "PreToolUse"
                    :matcher    "Edit|Write"
                    :description "Hard-deny edits to sensitive files (.env, secrets, keys)"}
   "command-audit" {:ns         "hooks.command-audit"
                    :event      "PostToolUse"
                    :matcher    "Bash"
                    :description "Log all Bash commands for review (observability)"}
   "format-on-save" {:ns        "hooks.format-on-save"
                     :event      "PostToolUse"
                     :matcher    "Edit|Write"
                     :description "Run formatters after writes (ruff, prettier, etc.)"}
   "slow-confirm"  {:ns         "hooks.slow-confirm"
                    :event      "PreToolUse"
                    :matcher    "Bash"
                    :description "Prompt for destructive commands (rm -rf, git push --force)"}})

(defn get-hook
  "Look up a hook by name. Returns nil if not found."
  [name]
  (get hooks name))

(defn list-hooks
  "Return all hooks as a seq of [name metadata] pairs."
  []
  (sort-by first hooks))
