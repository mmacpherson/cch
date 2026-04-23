(ns hooks.context-governor
  "PreCompact hook that injects repo-aware compaction instructions.

  Reads .claude/compact-instructions.md from the project root (if present)
  and returns it as customInstructions so the compaction preserves what
  matters for that specific repo. Falls back to a sensible generic default."
  (:require [cch.core :refer [defhook]]
            [cch.config :as config]
            [babashka.fs :as fs]
            [clojure.string :as str]))

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

(defhook context-governor
  "Inject repo-aware compaction preservation instructions."
  {}
  [input]
  (let [cwd          (:cwd input)
        repo-instructions (find-compact-instructions cwd)
        instructions (or repo-instructions default-instructions)]
    {:hook-specific-output {:customInstructions instructions}}))
