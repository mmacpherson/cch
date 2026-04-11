# Create a New Hook

Create a new cch hook based on the user's description.

## Steps

1. **Understand the intent** — what tool(s) should this hook match? What decision should it make (deny, ask, allow)?

2. **Create the hook file** at `src/hooks/<name>.clj`:
   - Write a pure check function first (no I/O, easy to test)
   - Wrap it with `(defhook ...)` from `cch.core`
   - Use `cch.protocol/extract-file-path` for file-based hooks
   - Use `cch.config/load-config` or `cch.config/find-config-up` if the hook needs configuration

3. **Register the hook** in `src/cli/registry.clj`:
   - Add an entry to the `hooks` map with `:ns`, `:event`, `:matcher`, `:description`

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

## Arguments

$ARGUMENTS — Description of what the hook should do (e.g., "block edits to migration files", "require confirmation for docker commands")
