# Architecture

## Three Execution Contexts

cch operates in three distinct contexts with different constraints:

### 1. Hook Execution (hot path)

**Budget: <50ms total including Babashka startup.**

When Claude Code invokes a tool, it spawns a bb subprocess for each matching hook. The subprocess reads JSON from stdin, runs the hook logic through a middleware chain, and writes JSON to stdout.

```
Claude Code                    bb subprocess
    │                              │
    ├─── JSON on stdin ──────────► │ proto/read-input
    │                              │ middleware chain (timing → error → logging → handler)
    │ ◄── JSON on stdout ──────── │ proto/write-response!
    │                              │ fire-and-forget sqlite3 spawn
    │                              ▼ exit 0
```

**Performance breakdown (~12ms total):**
- bb startup: ~8ms
- Namespace loading: ~2ms
- JSON parse + hook logic: ~0.5ms
- Fire-and-forget log: ~1.3ms (non-blocking)

### 2. CLI (`cch`)

Human-invoked commands for managing hooks. Reads/writes JSON settings files and queries SQLite. No latency constraints.

### 3. Dev Server (`cch dev`) — planned

Long-running process: httpkit web dashboard + nREPL + file watcher. For debugging and interactive hook development.

## Module Map

```
src/
├── cch/                    Framework core (loaded by every hook)
│   ├── protocol.clj        JSON stdin/stdout contract with Claude Code
│   ├── core.clj            defhook macro, middleware composition
│   ├── middleware.clj       wrap-timing, wrap-error-handler, wrap-logging
│   ├── config.clj           3-tier config loading (global < project < hook)
│   └── log.clj              SQLite event logging (fire-and-forget)
│
├── hooks/                   Hook implementations
│   └── scope_lock.clj       File edit scope enforcement
│
└── cli/                     CLI tool
    ├── cch.clj              Entry point, subcommand dispatch
    ├── init.clj             cch init
    ├── install.clj          cch install / cch uninstall
    ├── list_cmd.clj         cch list
    ├── log_cmd.clj          cch log
    ├── registry.clj         Hook metadata catalog
    └── settings.clj         Atomic settings.json manipulation
```

## Hook Authoring Model

### defhook Macro

`defhook` eliminates the boilerplate of reading stdin, parsing JSON, handling errors, logging events, and formatting output. You write only the decision logic.

```clojure
(defhook scope-lock
  "Enforces file edit scope per worktree."
  {}
  [input]
  (let [file-path (proto/extract-file-path input)
        cwd       (:cwd input)
        root      (worktree-root cwd)
        cfg       (config/load-yaml (config/find-config-up cwd ".cch-config.yaml" root))]
    (check-scope file-path root (get-in cfg [:hooks :scope-lock :allowed-paths]))))
```

This expands to a `-main` function that:
   - Reads JSON from stdin via `proto/read-input`
   - Injects `:cch/hook-name` into the input map
   - Runs the body (as an anonymous fn) through the middleware chain
   - Writes the result to stdout via `proto/write-response!`

Hook wiring metadata (event type, matcher pattern) lives in `cli/registry.clj` — the single source of truth for `cch install`.

### Decision Protocol

Hook functions return `nil` (allow) or a decision map:

```clojure
nil                                    ;; allow — fastest path, no stdout
{:decision :allow :reason "..."}       ;; explicit allow
{:decision :ask   :reason "..."}       ;; prompt user
{:decision :deny  :reason "..."}       ;; block tool call
```

This maps to Claude Code's JSON response envelope — which varies by event. `cch.protocol/->response` is a `defmulti` dispatching on event name across four shape groups:

| Event(s) | Shape |
|----------|-------|
| `PreToolUse` | nested `hookSpecificOutput.permissionDecision` |
| `PostToolUse`, `PostToolUseFailure`, `Stop`, `SubagentStop`, `UserPromptSubmit`, `ConfigChange`, `TaskCreated`, `TaskCompleted` | top-level `{decision, reason}` |
| `PermissionRequest` | nested `hookSpecificOutput.decision.behavior` |
| everything else (`SessionStart`, `Notification`, `FileChanged`, ...) | no output — observation-only |

Hook authors don't need to know which shape applies — they always return `{:decision :deny/:ask/:allow :reason ...}` (or `nil`). The renderer normalizes `:deny` to `"block"` for top-level-decision events so the same Clojure idiom works across every supported event type.

`scope-lock` emits PreToolUse shape:

```json
{
  "hookSpecificOutput": {
    "hookEventName": "PreToolUse",
    "permissionDecision": "ask",
    "permissionDecisionReason": "scope-lock: edit outside worktree"
  }
}
```

`command-audit` emits the PostToolUse shape — top-level:

```json
{"decision": "block", "reason": "command-audit: matched flag-pattern \"rm -rf /\" in: rm -rf /"}
```

## Middleware

Ring-style middleware: `(fn [handler] (fn [input] result))`.

The chain is pre-composed at load time via `reduce`/`comp`, so at runtime it's a single function call through nested closures. Measured overhead: ~0.05ms.

### Default Stack

```
wrap-logging                    ← outermost: sees everything, including exceptions
  └── wrap-timing               ← measures elapsed, attaches to result metadata
        └── wrap-error-handler  ← catches exceptions, returns {:decision :deny}
              └── (your hook)
```

| Middleware | Purpose |
|-----------|---------|
| `wrap-logging` | Fire-and-forget SQLite event insert. Outermost so all invocations are logged, including exceptions. Reads `:cch/elapsed-ms` from timing. |
| `wrap-timing` | Measures elapsed ms, attaches to result metadata |
| `wrap-error-handler` | Catches exceptions, returns `{:decision :deny}` with error message |

### Custom Middleware

Override the stack in `defhook`:

```clojure
(defhook my-hook
  "..."
  {:middleware [my-custom-middleware mw/wrap-error-handler]}
  [input]
  ...)
```

## Event Logging

### SQLite via CLI

Events are logged to `~/.local/share/cch/events.db` (XDG_DATA_HOME). The key design choice: we spawn the `sqlite3` CLI as a fire-and-forget process rather than using the SQLite pod.

**Why sqlite3 CLI over pod-babashka-go-sqlite3:**
- No pod installation required
- No additional startup latency
- sqlite3 is universally available
- ~1.3ms non-blocking spawn

**Tradeoff:** SQL construction via string formatting instead of parameterized queries. Values are escaped via single-quote doubling.

### Schema

```sql
CREATE TABLE events (
  id, timestamp, session_id, hook_name, event_type,
  tool_name, file_path, cwd, decision, reason, elapsed_ms, extra
);
```

Indexed on: session_id, timestamp, hook_name, decision.

The `extra TEXT` column holds the full hook input as JSON (minus cch's internal `:cch/hook-name` marker). `wrap-logging` populates it for every invocation, so event-specific fields that don't map to structured columns — `trigger` for PreCompact, `reason` for SessionEnd, `prompt` for UserPromptSubmit, `last_assistant_message` for Stop — are always recoverable via `json_extract(extra, '$.trigger')` etc.

## HTTP dispatcher (`cch serve`)

`src/cch/server.clj` runs a long-lived Babashka HTTP server (bundled `org.httpkit.server`) that collapses per-event latency from ~50ms (bb startup) to a few milliseconds (in-process dispatch).

**Dispatch flow:**

```
Claude Code event fires
  ↓ settings.json says {"type":"http", "url":"http://127.0.0.1:8888/hooks/<name>"}
  ↓ POST to the server
  ↓ route lookup → registered hook's `composed` handler
  ↓ same middleware chain as command mode (logging, timing, error-handling)
  ↓ check function returns nil / decision map
  ↓ proto/->response serializes JSON
  ↓ response body back to Claude Code
```

The key trick is in `defhook` (see `src/cch/core.clj`): each hook emits three defs — `handler-fn` (raw body), `composed` (middleware-wrapped), and `-main` (command-mode stdin/stdout entry point). Both command and HTTP paths go through `composed`, so logging/metrics/error semantics are byte-identical across modes.

Server binds to `127.0.0.1` only. No auth, no CORS — localhost is the defense.

### Service management

`cch install-service` writes an OS-native unit/plist that keeps the dispatcher running across reboots and crashes:

- **Linux** — systemd user unit at `~/.config/systemd/user/cch.service`. Uses the `%h` specifier for the user's home directory so the unit is portable across accounts. `Restart=on-failure` with `RestartSec=3`.
- **macOS** — launchd LaunchAgent at `~/Library/LaunchAgents/com.cch.server.plist`. Uses `RunAtLoad` + `KeepAlive` for auto-start + crash restart. Home directory is baked in at install time (launchd has no `%h` equivalent).

Templates live under `resources/service/` as real `.template` files — reviewable separately from the Clojure code. `src/cli/service_cmd.clj` detects the OS via `os.name`, renders the template (substituting `{{HOME}}` for macOS), writes to the canonical path, and prints the activation command the user runs explicitly. No auto-activation keeps the step reviewable.

`cch install <hook> --http` probes the server at install time via a TCP connect to `127.0.0.1:8888` — if it fails, a prominent warning tells the user to run `cch install-service` before expecting HTTP-mode hooks to work. This prevents the silent-ECONNREFUSED failure mode where hooks are installed into a broken state.

### Web dashboard

The same HTTP server renders a read-only dashboard at `/`. Server-rendered HTML via `hiccup2.core`, styled with Pico.css + Google Fonts (Roboto / Roboto Condensed), **zero client JS**. Filters are plain `<form method="get">` submits; auto-refresh uses `<meta http-equiv="refresh">`; click-to-expand is native `<details>`/`<summary>`. The entire dashboard lives in `cch.server/dashboard-html` — no separate templates, no build step.

### The universal observer

`hooks.event-log` is a built-in observer that subscribes to every Claude Code event cch supports (24 of the ~26 documented events — WorktreeCreate and FileChanged are excluded by design). The hook body returns `nil`; all the work lives in the existing middleware. Registering it requires a new `:events` (plural) field on registry entries: a vec of `{:event, :matcher}` maps that `cch install` expands into N `settings.json` entries, all sharing the `# cch:event-log` tag so `cch uninstall` removes them in one shot.

This is also the dogfood that proves the protocol renderer split: 24 events across four output-shape groups, all handled by `cch.protocol/->response`'s `defmulti` with zero per-event branching in the hook itself.

## Settings.json Management

### How Hooks Get Wired

`cch install scope-lock` adds this to `.claude/settings.local.json`:

```json
{
  "hooks": {
    "PreToolUse": [{
      "matcher": "Edit|Write",
      "hooks": [{
        "type": "command",
        "command": "bb -cp \"$CLAUDE_PROJECT_DIR/.claude/hooks/src:~/.local/share/cch/repo/src:~/.local/share/cch/repo/resources\" -m hooks.scope-lock # cch:scope-lock"
      }]
    }]
  }
}
```

The trailing `# cch:scope-lock` comment is a tag that lets `cch uninstall` find and remove exactly this entry without disturbing other hooks.

### Classpath Strategy

The hook command includes three classpath entries, in priority order:

1. **`$CLAUDE_PROJECT_DIR/.claude/hooks/src`** — project-local hooks (first, so they can override built-ins)
2. **`~/.local/share/cch/repo/src`** — framework core + built-in hooks (global)
3. **`~/.local/share/cch/repo/resources`** — schema.sql for SQLite DB creation

A project can override any built-in hook by providing the same namespace locally. Because the project path comes first, Clojure resolves the project's version.

### Global vs Project

- **`cch install <hook>`** — writes to `.claude/settings.local.json` (project-local, default)
- **`cch install <hook> --global`** — writes to `~/.claude/settings.json` (all projects)

Claude Code merges both at session start.

### Atomic Writes

Settings files are written atomically: write to `.tmp`, then rename. This prevents Claude Code from reading a partially-written file.

## Configuration

### Two-Tier Merge

```
~/.config/cch/config.yaml         Global (per-user) defaults
  ↓ deep-merge
<project>/.cch-config.yaml        Project settings (committed, pre-commit-style)
```

Both files share the same schema. Project values win over global. Maps are deep-merged; scalars use later-wins.

```yaml
# .cch-config.yaml
log:
  enabled: true
hooks:
  scope-lock:
    allowed-paths:
      - src/
```

Each hook reads its own section via `(get-in cfg [:hooks :<hook-name>])`. A missing `hooks:` section or a missing per-hook subsection behaves as "no config" for that hook.

### Config Discovery

`.cch-config.yaml` is found by walking up the directory tree from the current working directory, bounded by the worktree root. The boundary prevents stray config files above the project root from silently applying. Malformed YAML throws — hooks translate that to a fail-closed deny rather than proceeding as if no config existed.

## Hook Types

Claude Code supports four hook types. cch currently implements `command` but the architecture can accommodate all four.

| Type | How it works | cch approach |
|------|-------------|--------------|
| `command` | Shell command, JSON stdin/stdout | **Implemented.** defhook macro + middleware chain |
| `http` | POST event JSON to a URL | Dashboard server (Phase 4) can serve as the endpoint |
| `prompt` | Claude evaluates a prompt template | CLI generates settings.json entry (no Babashka needed) |
| `agent` | Subagent with Read/Grep/Glob access | CLI generates settings.json entry (no Babashka needed) |

For `prompt` and `agent` types, the framework's role is purely configuration management — generating the correct settings.json entries via `cch install`. The execution is handled by Claude Code itself.

For `http` type, the planned dashboard server (`cch dev`) could serve as both a debugging UI and a hook endpoint, receiving events via POST and applying decision logic.

### Extensibility: Pure Functions Across Hook Types

The pure-function-plus-defhook-wrapper pattern is designed for this. A function like `check-scope` takes plain data arguments and returns a decision map — it knows nothing about stdin, HTTP, or subagents. `defhook` generates the `command` type wiring (stdin → parse → call → respond), but the same `check-scope` function can be called from an HTTP handler:

```clojure
;; In the dashboard server (Phase 4)
(defn handle-hook-request [req]
  (let [input (parse-body req)]
    (respond (scope-lock/check-scope
               (get-in input [:tool_input :file_path])
               (:cwd input)
               (scope-lock/worktree-root)))))
```

This is why hooks should keep decision logic in standalone functions rather than inline in `defhook` — the function is reusable across all hook types.

## Design Principles

1. **nil is allow.** The fastest path produces no output. Only non-nil decisions generate JSON.

2. **Pure logic, then wire.** Keep decision logic in pure functions. `defhook` handles the I/O boundary.

3. **Fire and forget.** Logging must never delay the hook response. The sqlite3 process is spawned without waiting.

4. **Explicit over magic.** Hook entries in settings.json are tagged with `# cch:name` for traceability. Config files are plain EDN at known paths.

5. **Test like Claude Code would.** Integration tests spawn `bb -m hooks.name` as a subprocess with JSON on stdin — the exact invocation path Claude Code uses.
