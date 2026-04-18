(ns hooks.push-gate
  "PreToolUse hook for Bash: before allowing `git push`, run configured
  gate commands (e.g. lint + tests). If any gate fails, the push is denied
  and the tail of its output surfaces as feedback for Claude.

  Config at hooks.push-gate.gates in .cch-config.yaml (list of shell commands
  run in order from the worktree root):

      hooks:
        push-gate:
          gates:
            - just lint-all
            - just test

  No config, no gates configured, or not in a git repo → pass through.
  Malformed YAML denies, matching scope-lock's fail-closed posture: a
  silently-missing gate must not let unverified code ship.

  Detection is leading-command only. Chained `foo && git push` or shell
  trickery that tucks the push behind a pipe is not caught — workarounds
  belong in the conversation, not the hook."
  (:require [babashka.process :as p]
            [cch.core :refer [defhook]]
            [cch.config :as config]
            [clojure.string :as str]))

(def ^:private push-re
  "Matches `git push` as the leading command, allowing env-var prefixes
  (`FOO=bar git push ...`). Requires whitespace-or-end after `push` so
  we don't trip on hypothetical `git push-foo`."
  #"^(?:\w+=\S*\s+)*git\s+push(?:\s|$)")

(defn is-push?
  "Does this Bash command invoke `git push` as its leading command?"
  [command]
  (boolean (and command (re-find push-re (str/trim command)))))

(defn- tail-lines
  "Keep the last n lines of a string so denial reasons stay readable."
  [n s]
  (->> (str/split-lines (or s ""))
       (take-last n)
       (str/join "\n")))

(defn- run-gate
  "Run a single gate command in `dir`. Returns {:ok bool :tail string}."
  [dir cmd]
  (let [res (p/shell {:dir      dir
                      :out      :string
                      :err      :string
                      :continue true}
                     "sh" "-c" cmd)
        combined (str (:out res) (:err res))]
    {:ok   (zero? (:exit res))
     :tail (tail-lines 40 combined)}))

(defn check-push
  "Run gates in order; return nil (allow) or a deny decision on first failure.

  dir   — working directory gates should run from (worktree root)
  gates — seq of shell command strings; if empty/nil, returns nil

  Intentionally short-circuits: later gates shouldn't burn the wall clock
  once an earlier one has already decided the push is not ready."
  [dir gates]
  (when (seq gates)
    (loop [[cmd & more] gates]
      (when cmd
        (let [{:keys [ok tail]} (run-gate dir cmd)]
          (if ok
            (recur more)
            {:decision :deny
             :reason   (str "push-gate: `" cmd "` failed — fix before pushing.\n\n"
                            "--- tail of output ---\n" tail)}))))))

(defhook push-gate
  "Run configured lint/test gates before allowing `git push`."
  {}
  [input]
  (let [command (get-in input [:tool_input :command])]
    (when (is-push? command)
      (let [cwd         (:cwd input)
            root        (config/worktree-root cwd)
            config-path (when root
                          (config/find-config-up (or cwd root) ".cch-config.yaml" root))
            cfg         (try
                          (config/load-yaml config-path)
                          (catch clojure.lang.ExceptionInfo _e
                            ::malformed))]
        (cond
          (= cfg ::malformed)
          {:decision :deny
           :reason   (str "push-gate: malformed config at " config-path
                          " — refusing to push (fail closed)")}

          (and root (seq (get-in cfg [:hooks :push-gate :gates])))
          (check-push root (get-in cfg [:hooks :push-gate :gates])))))))
