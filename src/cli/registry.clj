(ns cli.registry
  "Registry of available hooks with their metadata.
  Only lists hooks that have implementations under src/hooks/.

  Planned (not yet implemented):
  - format-on-save: run formatters after writes (PostToolUse)
  - slow-confirm: prompt for destructive commands"
  (:require [clojure.string :as str]))

;; Registry schema:
;;   :type          — :code (default, Clojure fn via defhook)
;;                    :prompt (native Claude Code prompt hook)
;;                    :agent  (native Claude Code agent hook)
;;   :ns            — Clojure namespace (:code hooks only)
;;   :event         — single event name (single-event hooks)
;;   :matcher       — tool-name regex for :event (tool events only)
;;   :events        — [{:event :matcher} ...] for multi-event hooks.
;;                    When present, overrides :event/:matcher.
;;   :description   — shown on dashboard + CLI
;;
;; Optional (all types):
;;   :if            — Claude Code permission-rule string (tool events only)
;;   :timeout       — seconds; per-type defaults: 30 http/prompt, 60 agent
;;   :status-message — spinner label shown while the hook runs
;;
;; :type :prompt only:
;;   :prompt-template — prompt text with $ARGUMENTS placeholder
;;   :model          — model override (optional)
;;
;; :type :agent only:
;;   :agent-spec     — map describing the subagent
;;
;; Future (v1 placeholder):
;;   :config-schema  — malli schema for hook options (see 8gj)

(def hooks
  "Built-in hooks and their configuration."
  {"scope-lock"    {:type        :code
                    :ns          "hooks.scope-lock"
                    :event       "PreToolUse"
                    :matcher     "Edit|Write"
                    :description "Enforces file edit scope per git worktree"}
   "protect-files" {:type        :code
                    :ns          "hooks.protect-files"
                    :event       "PreToolUse"
                    :matcher     "Edit|Write"
                    :description "Hard-denies edits to secrets/keys/.env/.git/.ssh"}
   "command-audit" {:type        :code
                    :ns          "hooks.command-audit"
                    :event       "PostToolUse"
                    :matcher     "Bash"
                    :description "Logs Bash commands; flags configured regex patterns"}
   "command-guard" {:type        :code
                    :ns          "hooks.command-guard"
                    :event       "PreToolUse"
                    :matcher     "Bash"
                    :description "Hard-denies destructive commands (rm -rf, fork bombs, curl|sh)"}
   "push-gate"     {:type           :code
                    :ns             "hooks.push-gate"
                    :event          "PreToolUse"
                    :matcher        "Bash"
                    :timeout        600
                    :status-message "running push gates"
                    :description    "Runs configured lint/test gates before `git push`"}
   "context-governor" {:type        :code
                    :ns          "hooks.context-governor"
                    :events      [{:event "PreCompact"        :matcher nil}
                                  {:event "UserPromptSubmit"  :matcher nil}]
                    :description "Budget-aware compaction governor with repo-specific preservation"}
   "event-log"     {:type        :code
                    :ns          "hooks.event-log"
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
                    ;; WorktreeCreate intentionally excluded: hooks on that
                    ;; event replace the default worktree behavior.
                    ;; FileChanged intentionally excluded: its matcher is
                    ;; the watched-filename list, so there's no wildcard.
                    :description "Logs every Claude Code event to the cch SQLite DB"}})

(defn hook-type
  "Return the :type of a registry entry, defaulting to :code."
  [entry]
  (:type entry :code))

(defn get-hook
  "Look up a hook by name. Returns nil if not found."
  [name]
  (get hooks name))

(defn list-hooks
  "Return all hooks as a sorted seq of [name metadata] pairs."
  []
  (sort-by first hooks))

(defn hook-events
  "Return the seq of {:event :matcher} pairs this hook subscribes to.
  Normalizes single-event (:event/:matcher) and multi-event (:events) forms."
  [entry]
  (if-let [evs (:events entry)]
    evs
    (when (:event entry)
      [{:event (:event entry) :matcher (:matcher entry)}])))

(defn dispatcher-events
  "Return [{:event :matcher}] pairs describing the settings.json entries
  cch should install for its dispatcher. One per distinct event across
  all :code hooks. For tool events, matcher is '.*' (Claude Code must
  forward every tool call so the dispatcher can decide per-payload).
  Non-tool events get nil matcher."
  []
  (let [code-hook-events (for [[_ entry] (list-hooks)
                               :when (= :code (hook-type entry))
                               ev (hook-events entry)]
                           ev)
        by-event (group-by :event code-hook-events)]
    (->> by-event
         (map (fn [[event evs]]
                ;; If any subscriber uses a matcher, keep '.*' so all tools
                ;; reach the dispatcher. If all use nil (non-tool event),
                ;; keep nil so settings.json is tidy.
                {:event   event
                 :matcher (when (some :matcher evs) ".*")}))
         (sort-by :event))))

(defn matcher-matches?
  "True when a registered matcher regex (or nil) matches a tool name.
  nil matcher matches anything (used for non-tool events)."
  [matcher-str tool-name]
  (or (nil? matcher-str)
      (nil? tool-name)
      (boolean (re-find (re-pattern matcher-str) tool-name))))

;; --- Validation ---

(defn- entry-errors
  "Returns a seq of error strings for one registry entry, or empty when valid."
  [[name entry]]
  (let [t (hook-type entry)]
    (concat
      ;; Common required fields
      (when-not (:description entry)
        [(str "'" name "': missing :description")])
      ;; Per-type required fields
      (case t
        :code
        (concat
          (when-not (:ns entry)
            [(str "'" name "': :type :code requires :ns")])
          (when-not (or (:event entry) (:events entry))
            [(str "'" name "': :type :code requires :event or :events")])
          (when (or (:prompt-template entry) (:model entry) (:agent-spec entry))
            [(str "'" name "': :type :code must not set :prompt-template/:model/:agent-spec")]))

        :prompt
        (concat
          (when-not (:prompt-template entry)
            [(str "'" name "': :type :prompt requires :prompt-template")])
          (when-not (:event entry)
            [(str "'" name "': :type :prompt requires :event")])
          (when (or (:ns entry) (:agent-spec entry))
            [(str "'" name "': :type :prompt must not set :ns/:agent-spec")]))

        :agent
        (concat
          (when-not (:agent-spec entry)
            [(str "'" name "': :type :agent requires :agent-spec")])
          (when-not (:event entry)
            [(str "'" name "': :type :agent requires :event")])
          (when (or (:ns entry) (:prompt-template entry))
            [(str "'" name "': :type :agent must not set :ns/:prompt-template")]))

        ;; Unknown type
        [(str "'" name "': unknown :type " (pr-str t))]))))

(defn registry-errors
  "Returns a seq of validation error strings across every hook. Empty = valid."
  []
  (mapcat entry-errors hooks))

(defn validate-registry!
  "Throws ex-info listing every validation error if any are present.
  Intended to be called on server/CLI startup."
  []
  (let [errors (registry-errors)]
    (when (seq errors)
      (throw (ex-info (str "registry validation failed:\n  "
                           (str/join "\n  " errors))
                      {:type ::invalid-registry :errors errors})))))
