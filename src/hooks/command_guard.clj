(ns hooks.command-guard
  "PreToolUse hook for Bash: hard-deny destructive commands before execution.

  Built-in patterns catch the obvious foot-guns (rm -rf /, fork bombs,
  curl|sh pipes, disk-wiping dd). Additional patterns can be added via
  .cch-config.yaml:

    hooks:
      command-guard:
        extra-patterns:
          - \"DROP TABLE\"
          - \"mkfs\""
  (:require [cch.core :refer [defhook]]
            [cch.config :as config]
            [clojure.string :as str]))

(def ^:private builtin-patterns
  [{:label "recursive force-delete of system paths"
    :re    #"rm\s+.*-\w*[rRf]\w*\s+(/\s|/$|/\*|~/|/home|/etc|/usr|/var|/boot|/sys|/proc)"
    :match #(re-find %2 %1)}
   {:label "fork bomb"
    :re    #":\(\)\s*\{|\./:&|/dev/null\s*&"
    :match #(re-find %2 %1)}
   {:label "pipe to shell"
    :re    #"curl\s.*\|\s*(ba)?sh|wget\s.*\|\s*(ba)?sh"
    :match #(re-find %2 %1)}
   {:label "disk wipe"
    :re    #"dd\s+if=/dev/(zero|urandom)\s+of=/dev/[a-z]"
    :match #(re-find %2 %1)}
   {:label "chmod 777 recursive"
    :re    #"chmod\s+(-\w*R\w*\s+)?777\s"
    :match #(re-find %2 %1)}])

(defn- load-extra-patterns
  "Load additional deny patterns from .cch-config.yaml."
  [cwd]
  (when-let [root (config/worktree-root cwd)]
    (let [cfg-path (config/find-config-up (or cwd root) ".cch-config.yaml" root)
          cfg      (try (config/load-yaml cfg-path) (catch Exception _ nil))]
      (get-in cfg [:hooks :command-guard :extra-patterns]))))

(defn check-dangerous
  "Pure logic: return nil (allow) or {:decision :deny :reason ...}.
  command is the Bash command string; extra-pattern-strs is a seq of
  regex strings from config."
  [command extra-pattern-strs]
  (when command
    (or
      (some (fn [{:keys [label re match]}]
              (when (match command re)
                {:decision :deny
                 :reason   (str "command-guard: blocked — " label)}))
            builtin-patterns)
      (when (seq extra-pattern-strs)
        (some (fn [pat-str]
                (try
                  (when (re-find (re-pattern pat-str) command)
                    {:decision :deny
                     :reason   (str "command-guard: blocked by extra pattern "
                                    (pr-str pat-str))})
                  (catch Exception _e nil)))
              extra-pattern-strs)))))

(defhook command-guard
  "Hard-deny destructive commands before execution."
  {}
  [input]
  (check-dangerous
    (get-in input [:tool_input :command])
    (load-extra-patterns (:cwd input))))
