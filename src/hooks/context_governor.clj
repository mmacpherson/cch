(ns hooks.context-governor
  "Context budget governor with two responsibilities:

  1. PreCompact — injects repo-aware compaction preservation instructions
     from .claude/compact-instructions.md (or a generic default).

  2. UserPromptSubmit — checks the latest context snapshot (written by
     the statusLine command via /context-snapshot) and injects an
     advisory telling Claude to compact when usage exceeds a threshold."
  (:require [cch.core :refer [defhook]]
            [cch.config :as config]
            [cch.log :as log]
            [babashka.fs :as fs]
            [clojure.string :as str]))

(def ^:private default-compact-threshold-pct 80)

(def ^:private default-instructions
  "Preserve in order of priority:
1. The current task: what we are building or fixing, the specific issue being worked
2. File paths and functions currently being modified, with their purpose
3. Architectural decisions and constraints discovered in this session
4. Test results from the most recent test run (pass/fail, which tests)
5. Any workarounds or non-obvious patterns established

Drop aggressively:
- Exploratory dead ends and abandoned approaches
- Verbose tool output (file listings, git log, grep results)
- Intermediate debugging output
- Full file contents that were read but not modified
- Redundant re-readings of the same files")

(defn- find-compact-instructions
  "Walk up from cwd to worktree root looking for .claude/compact-instructions.md.
  Returns the file contents as a string, or nil."
  [cwd]
  (when-let [root (config/worktree-root cwd)]
    (let [path (str root "/.claude/compact-instructions.md")]
      (when (fs/exists? path)
        (str/trim (slurp path))))))

(defn- compact-threshold
  "Read the threshold from .cch-config.yaml, or use the default."
  [cwd]
  (let [root (config/worktree-root cwd)
        cfg-path (when root (config/find-config-up (or cwd root) ".cch-config.yaml" root))
        cfg (try (config/load-yaml cfg-path) (catch Exception _ nil))]
    (or (get-in cfg [:hooks :context-governor :compact-threshold-pct])
        default-compact-threshold-pct)))

(defn- handle-pre-compact
  [input]
  (let [cwd          (:cwd input)
        repo-instructions (find-compact-instructions cwd)
        instructions (or repo-instructions default-instructions)]
    {:hook-specific-output {:customInstructions instructions}}))

(defn- handle-user-prompt-submit
  [input]
  (let [session-id (:session_id input)
        snapshot   (when session-id (log/latest-context-snapshot session-id))]
    (when-let [pct (:used_pct snapshot)]
      (let [threshold (compact-threshold (:cwd input))]
        (when (> pct threshold)
          {:context (format "CONTEXT BUDGET WARNING: Context usage is at %.0f%% (threshold: %d%%). Please run /compact before continuing to avoid excessive token consumption." pct threshold)})))))

(defhook context-governor
  "Budget-aware compaction governor with repo-specific preservation."
  {}
  [input]
  (case (:hook_event_name input)
    "PreCompact"       (handle-pre-compact input)
    "UserPromptSubmit" (handle-user-prompt-submit input)
    nil))
