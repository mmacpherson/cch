# Create a New Hook

Create a new cch hook based on the user's description.

## Steps

1. **Understand the intent** — what tool(s) should this hook match? What decision should it make (deny, ask, allow)?

2. **Create the hook file** at `src/hooks/<name>.clj`:
   - Write a pure check function first (no I/O, easy to test)
   - Wrap it with `(defhook ...)` from `cch.core`
   - Use `cch.protocol/extract-file-path` for file-based hooks
   - If the hook needs configuration, read `.cch-config.yaml` via `cch.config/load-yaml` and extract your section with `(get-in cfg [:hooks :<hook-name> ...])`. Wrap the load in try/catch on `ExceptionInfo` and fail closed (return a deny) if it's malformed — do not proceed as if no config existed.

3. **Register the hook** in `src/cli/registry.clj`:
   - Add an entry to the `hooks` map with `:ns`, `:event`, `:matcher`, `:description`
   - For a multi-event hook (subscribes to several Claude Code events), use `:events [{:event "..." :matcher "..."} ...]` instead of `:event`/`:matcher`. The handler then dispatches inside via `(case (:hook_event_name input) ...)`. See `hooks/context-governor` for an example covering `PreCompact` + `UserPromptSubmit`.
   - `:type` defaults to `:code` (Clojure fn via `defhook`). The registry also supports `:prompt` and `:agent` for native Claude Code hook types — those use `:prompt-template`/`:model` or `:agent-*` fields instead of `:ns`. Most hooks should stay `:code`. Optional fields: `:if` (permission-rule string), `:timeout`, `:status-message`. Read the schema comment at the top of `registry.clj` before reaching for the rarer fields.

4. **Write tests** in `test/hooks/<name>_test.clj`:
   - Unit tests for the pure check function (no I/O, explicit args)
   - At least one integration test running `bb -cp src:resources -m hooks.<name>` as a subprocess

5. **Run tests**: `bb test`

6. **Test manually**:
   ```bash
   echo '{"cwd":"/repo","tool_name":"Edit","tool_input":{"file_path":"/path"}}' \
     | bb -cp src:resources -m hooks.<name>
   ```

## Hook Pattern Reference

**Single-event (most common):**

```clojure
(ns hooks.my-hook
  (:require [cch.core :refer [defhook]]
            [cch.protocol :as proto]))

(defn check-something [file-path tool-name]
  ;; Return nil to allow, or {:decision :deny/:ask :reason "..."}
  ...)

(defhook my-hook
  "One-line description."
  {:event "PreToolUse" :matcher "Edit|Write"}
  [input]
  (check-something
    (proto/extract-file-path input)
    (:tool_name input)))
```

**Multi-event:**

```clojure
(defn- handle-pre-compact     [input] ...)
(defn- handle-user-prompt-submit [input] ...)

(defhook my-hook
  "Subscribes to multiple events; dispatches by hook_event_name."
  {} ; events live in registry.clj as :events [...]
  [input]
  (case (:hook_event_name input)
    "PreCompact"       (handle-pre-compact input)
    "UserPromptSubmit" (handle-user-prompt-submit input)
    nil))
```

Return values are per-event (`:decision` for tool events; `:context` for `UserPromptSubmit`; `:hook-specific-output` for `PreCompact`; etc.). Returning `nil` is always "no opinion."

## Arguments

$ARGUMENTS — Description of what the hook should do (e.g., "block edits to migration files", "require confirmation for docker commands")
