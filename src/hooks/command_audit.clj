(ns hooks.command-audit
  "PostToolUse hook for Bash: log every command, optionally flag configured
  regex patterns as advisory context back to Claude.

  Config at hooks.command-audit.flag-patterns in .cch-config.yaml:

    hooks:
      command-audit:
        flag-patterns:
          - \"rm -rf /\"
          - \"curl .* \\\\| sh\"

  PostToolUse cannot prevent the command (it already ran); a pattern match
  surfaces as a block-decision whose reason becomes conversation context
  for Claude to notice. Every invocation is logged to the SQLite event DB
  regardless of whether anything flags.

  Fails noisy (not closed): if .cch-config.yaml is malformed or a
  flag-pattern is not a valid regex, the hook emits a block-decision
  naming the problem. The command has already run so there's no
  fail-closed option; surfacing the error in-context is the best we can
  do to prompt the user to fix their config."
  (:require [cch.core :refer [defhook]]
            [cch.config :as config]))

(defn- worktree-root
  "Git worktree root for cwd. Thin passthrough to the memoized
  cch.config/worktree-root so repeated calls share a single cache."
  [cwd]
  (config/worktree-root cwd))

(defn- compile-patterns
  "Compile pattern strings to {:raw string :re Pattern} pairs.
  Returns {:ok [...]} or {:error msg} if any pattern fails to compile."
  [patterns]
  (try
    {:ok (mapv (fn [p] {:raw p :re (re-pattern p)}) patterns)}
    (catch Exception e
      {:error (str "invalid regex in flag-patterns: " (.getMessage e))})))

(defn check-command
  "Pure logic: return nil (no feedback) or a {:decision :block :reason ...}
  map. No I/O.

  command       — Bash command string from tool_input (may be nil)
  flag-patterns — seq of regex strings (may be empty or nil)"
  [command flag-patterns]
  (when (and command (seq flag-patterns))
    (let [compiled (compile-patterns flag-patterns)]
      (if (:error compiled)
        {:decision :block
         :reason   (str "command-audit: " (:error compiled))}
        (when-let [hit (some (fn [{:keys [raw re]}]
                               (when (re-find re command) raw))
                             (:ok compiled))]
          {:decision :block
           :reason   (str "command-audit: matched flag-pattern "
                          (pr-str hit) " in: " command)})))))

(defhook command-audit
  "Advisory PostToolUse Bash logger with optional pattern flagging."
  {}
  [input]
  (let [cwd         (:cwd input)
        root        (worktree-root cwd)
        config-path (when root
                      (config/find-config-up (or cwd root) ".cch-config.yaml" root))
        cfg         (try
                      (config/load-yaml config-path)
                      (catch clojure.lang.ExceptionInfo _e
                        ::malformed))]
    (if (= cfg ::malformed)
      {:decision :block
       :reason   (str "command-audit: malformed config at " config-path)}
      (check-command (get-in input [:tool_input :command])
                     (get-in cfg [:hooks :command-audit :flag-patterns])))))
