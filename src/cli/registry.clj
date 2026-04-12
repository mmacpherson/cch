(ns cli.registry
  "Registry of available hooks with their metadata.
  Only lists hooks that have implementations under src/hooks/.

  Planned (not yet implemented):
  - protect-files: hard-deny edits to .env, secrets, keys
  - command-audit: log all Bash commands (PostToolUse)
  - format-on-save: run formatters after writes (PostToolUse)
  - slow-confirm: prompt for destructive commands")

;; Registry schema:
;;   :ns           — Clojure namespace implementing the hook
;;   :event        — single event name (legacy single-event hooks)
;;   :matcher      — matcher string for :event (legacy single-event hooks)
;;   :events       — vector of {:event :matcher} maps for multi-event hooks.
;;                   When present, overrides :event/:matcher at install time.
;;   :description  — shown by `cch list`
(def hooks
  "Built-in hooks and their configuration."
  {"scope-lock"    {:ns          "hooks.scope-lock"
                    :event       "PreToolUse"
                    :matcher     "Edit|Write"
                    :description "Enforces file edit scope per git worktree"}
   "command-audit" {:ns          "hooks.command-audit"
                    :event       "PostToolUse"
                    :matcher     "Bash"
                    :description "Logs Bash commands; flags configured regex patterns"}
   "event-log"     {:ns          "hooks.event-log"
                    :events      [;; Tool events: matcher ".*" matches every tool.
                                  {:event "PreToolUse"          :matcher ".*"}
                                  {:event "PostToolUse"         :matcher ".*"}
                                  {:event "PostToolUseFailure"  :matcher ".*"}
                                  {:event "PermissionRequest"   :matcher ".*"}
                                  {:event "PermissionDenied"    :matcher ".*"}
                                  {:event "SubagentStart"       :matcher ".*"}
                                  {:event "SubagentStop"        :matcher ".*"}
                                  ;; Non-tool events: matcher omitted.
                                  {:event "SessionStart"        :matcher nil}
                                  {:event "SessionEnd"          :matcher nil}
                                  {:event "UserPromptSubmit"    :matcher nil}
                                  {:event "Stop"                :matcher nil}
                                  {:event "StopFailure"         :matcher nil}
                                  {:event "TaskCreated"         :matcher nil}
                                  {:event "TaskCompleted"       :matcher nil}
                                  {:event "TeammateIdle"        :matcher nil}
                                  {:event "Notification"        :matcher nil}
                                  {:event "InstructionsLoaded"  :matcher nil}
                                  {:event "ConfigChange"        :matcher nil}
                                  {:event "CwdChanged"          :matcher nil}
                                  {:event "PreCompact"          :matcher nil}
                                  {:event "PostCompact"         :matcher nil}
                                  {:event "WorktreeRemove"      :matcher nil}
                                  {:event "Elicitation"         :matcher nil}
                                  {:event "ElicitationResult"   :matcher nil}]
                    ;; WorktreeCreate is intentionally excluded: hooks on that
                    ;; event replace the default git-worktree behavior, and a
                    ;; nil-returning observer would fail worktree creation.
                    ;; FileChanged is intentionally excluded: the matcher
                    ;; doubles as the literal filename watch list so there's
                    ;; no wildcard — user opt-in only.
                    :description "Logs every Claude Code event to the cch SQLite DB"}})

(defn get-hook
  "Look up a hook by name. Returns nil if not found."
  [name]
  (get hooks name))

(defn list-hooks
  "Return all hooks as a seq of [name metadata] pairs."
  []
  (sort-by first hooks))
