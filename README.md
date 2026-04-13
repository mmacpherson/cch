# cch — Claude Code Hooks

A Babashka-based framework for authoring, installing, and debugging [Claude Code](https://claude.ai/code) hooks. Replaces ad-hoc shell scripts with composable, testable, observable Clojure functions.

**Why Babashka?** Hooks fire on every tool call. Babashka starts in ~9ms (vs 18-21s for Node.js-based alternatives), has JSON/EDN/filesystem built-in, and Clojure's data-first idiom is a natural fit for JSON-in/JSON-out hooks.

## Quick Start

```bash
# Clone and set up
git clone <repo-url> ~/projects/claude-code-hooks
cd ~/projects/claude-code-hooks
bb -cp src:resources -m cli.cch init

# See available hooks
bb -cp src:resources -m cli.cch list

# Install a hook (project-local by default)
bb -cp src:resources -m cli.cch install scope-lock

# Check event history
bb -cp src:resources -m cli.cch log
```

### Prerequisites

- [Babashka](https://babashka.org/) v1.12+ (`brew install babashka` or `curl -sLO https://raw.githubusercontent.com/babashka/babashka/master/install`)
- SQLite3 CLI (usually pre-installed on Linux/macOS)

## How It Works

Claude Code hooks are shell commands that run before or after tool calls. They receive JSON on stdin describing the tool call and return JSON on stdout with a permission decision.

**Without cch:** You write shell scripts or raw Babashka one-offs, wire them manually into `settings.json`, and debug by tailing `/tmp/` log files.

**With cch:** You write a pure Clojure function, wrap it with `defhook`, and the framework handles JSON protocol, error handling, timing, and centralized logging automatically.

```clojure
(ns hooks.my-hook
  (:require [cch.core :refer [defhook]]))

(defhook my-hook
  "Block edits to secret files."
  {}
  [input]
  (when (re-find #"\.env$" (get-in input [:tool_input :file_path] ""))
    {:decision :deny :reason "Cannot edit .env files"}))
```

That's it. The `defhook` macro generates a `-main` entry point with middleware for timing, error handling, and SQLite event logging.

## CLI Commands

| Command | Description |
|---------|-------------|
| `cch init` | Set up global config, SQLite database, and project config |
| `cch install <hook>` | Wire a hook into `settings.local.json` (or `--global`, or `--http`) |
| `cch uninstall <hook>` | Remove a hook from settings |
| `cch list` | Show available hooks with installation status |
| `cch log` | Query event history from SQLite |
| `cch serve` | Run the HTTP dispatcher + web dashboard |
| `cch install-service` | Install OS-native auto-start (systemd/launchd) for `cch serve` |
| `cch uninstall-service` | Remove the auto-start unit/plist |

### Log Queries

```bash
cch log                          # Last 20 events
cch log --hook=scope-lock        # Filter by hook
cch log --decision=deny          # Show all denials
cch log --session=abc123         # Filter by session
cch log --limit=50               # More results
```

## Built-in Hooks

| Hook | Event | Matcher | Description |
|------|-------|---------|-------------|
| `scope-lock` | PreToolUse | Edit\|Write | Enforce file edit scope per git worktree |
| `command-audit` | PostToolUse | Bash | Log every Bash command; flag configured regex patterns as advisory context |
| `event-log` | *(24 events)* | *(all)* | Universal observer — logs every Claude Code event to SQLite |

Planned (not yet implemented): `protect-files`, `format-on-save`, `slow-confirm`.

### The universal observer

`cch install event-log` subscribes to every Claude Code event cch supports — session boundaries, prompt submissions, tool calls (pre + post), task lifecycle, config changes, compactions, MCP elicitations, and more. Each invocation writes a row to `~/.local/share/cch/events.db` with the event type, timestamp, and the full input payload (as JSON in the `extra` column). Query with `cch log --event=<type>` or directly via `sqlite3`.

**Latency caveat.** Each Claude Code event triggers a fresh Babashka process (~50ms). Subscribing to all 24 events is imperceptible on a casual workflow but noticeable on heavy tool loops (every Bash / Edit fires two events). Two options:

- Install a subset: `cch install event-log --exclude=PreToolUse,PostToolUse,PostToolUseFailure`
- Wait for the HTTP dispatcher (tracked in `claude-code-hooks-bq2`), which eliminates per-event startup cost.

**Payload capture.** The `extra` column stores the full event input as JSON — including `tool_input` for Bash/Edit/Write (commands, file paths, contents), `prompt` text from UserPromptSubmit, and `last_assistant_message` from Stop. It's a local file on your machine; no data leaves the host. Be aware of this before using cch in a regulated environment.

## `cch serve` — HTTP dispatcher + dashboard

```bash
cch serve                        # default: 127.0.0.1:8888
cch serve --port 9000 --host 127.0.0.1
```

Runs a long-lived Babashka HTTP server that dispatches hooks in-process and serves a simple dashboard.

**Why:** command-mode hooks spawn a fresh Babashka per event (~50ms of bb startup). With the observer installed across 24 events, that adds up. Installing a hook in HTTP mode collapses dispatch latency to a few milliseconds because the JVM is already running.

```bash
cch serve &
cch install event-log --global --http   # now fires against http://127.0.0.1:8888/hooks/event-log
```

**Dashboard** at `http://127.0.0.1:8888/` — server-rendered (no client JS), styled with Pico.css + Roboto. Filter by repo, hook, event type, session, decision, or time range. Click a row to expand the full event payload. `?open=all` expands every row at once; refresh manually with the link in the meta area.

**Server routes:**

| Method | Path | Purpose |
|--------|------|---------|
| `POST` | `/hooks/<name>` | Dispatches to the named hook's composed handler. Returns the same JSON shape command-mode produces. |
| `GET`  | `/` | Dashboard HTML (events table + filters). |
| `GET`  | `/health` | JSON liveness check and registered-hooks list. |

Server binds to `127.0.0.1` only — never exposed beyond localhost.

### Running `cch serve` as a service

For HTTP-installed hooks to work reliably across reboots and crashes, `cch serve` needs to be a managed service. `cch install-service` writes the canonical unit/plist for your OS:

```bash
cch install-service

# Linux — activate:
systemctl --user enable --now cch

# macOS — activate:
launchctl bootstrap gui/$UID ~/Library/LaunchAgents/com.cch.server.plist
```

Without this, HTTP-installed hooks fail with ECONNREFUSED any time the server isn't running. `cch install <hook> --http` probes the server at install time and prints a warning if it's not reachable, so you don't silently install into a broken state.

Windows service support isn't shipped yet — if you need it, say so in an issue and we'll prioritize.

## Writing Custom Hooks

### The Pattern

Every hook follows a two-part structure:

1. **Pure logic function** — takes data, returns a decision. No I/O. Independently testable.
2. **`defhook` wrapper** — handles JSON protocol, middleware, and entry point generation.

```clojure
(ns hooks.my-custom-hook
  (:require [cch.core :refer [defhook]]
            [cch.protocol :as proto]))

;; 1. Pure logic — easy to test
(defn check-something [file-path tool-name]
  (when (and (= tool-name "Write")
             (re-find #"migrations/" (or file-path "")))
    {:decision :ask
     :reason   "Writing to migrations/ — are you sure?"}))

;; 2. Wire it up with defhook
(defhook my-custom-hook
  "Prompt before writing migration files."
  {}
  [input]
  (check-something
    (proto/extract-file-path input)
    (:tool_name input)))
```

### Testing

```clojure
(ns hooks.my-custom-hook-test
  (:require [clojure.test :refer [deftest is]]
            [hooks.my-custom-hook :as hook]))

(deftest test-allows-normal-writes
  (is (nil? (hook/check-something "/repo/src/app.py" "Write"))))

(deftest test-asks-for-migrations
  (is (= :ask (:decision (hook/check-something "/repo/migrations/001.sql" "Write")))))
```

Run tests: `bb test`

### Hook Decisions

Your hook function returns one of:

| Return | Effect |
|--------|--------|
| `nil` | Allow (fastest — no output, exit 0) |
| `{:decision :allow :reason "..."}` | Explicit allow with reason |
| `{:decision :ask :reason "..."}` | Prompt user for permission |
| `{:decision :deny :reason "..."}` | Block the tool call |

### Claude Code Input (stdin JSON)

The input map your hook receives includes:

```clojure
{:session_id      "abc123"
 :cwd             "/path/to/project"
 :tool_name       "Edit"           ; Edit, Write, Bash, Read, etc.
 :tool_input      {:file_path "/path/to/file.py"
                   :old_string "..."
                   :new_string "..."}
 :permission_mode "default"
 :hook_event_name "PreToolUse"
 :cch/hook-name   "my-custom-hook" ; injected by defhook
 }
```

## Configuration

Two tiers, merged in order (project wins over global):

| Tier | File | Purpose |
|------|------|---------|
| Global | `~/.config/cch/config.yaml` | User preferences across all projects |
| Project | `.cch-config.yaml` | Shared project settings (commit this) |

Hook-specific settings live under a `hooks:` section keyed by hook name, pre-commit-style.

### Example: scope-lock narrowing

```yaml
# .cch-config.yaml (at repo root, committed)
hooks:
  scope-lock:
    allowed-paths:
      - src/
      - .claude/
```

## Architecture

See [ARCHITECTURE.md](ARCHITECTURE.md) for the full design — execution contexts, middleware chain, SQLite logging, and the settings.json management approach.

## Development

```bash
# Run tests
bb test

# Test a hook manually
echo '{"cwd":"/repo","tool_input":{"file_path":"/etc/passwd"}}' \
  | bb -cp src:resources -m hooks.scope-lock

# Query the event log
sqlite3 ~/.local/share/cch/events.db "SELECT * FROM events ORDER BY id DESC LIMIT 10;"
```

## Roadmap

- [ ] **Dashboard** — httpkit web UI on localhost:7777 for real-time event viewing
- [ ] **Dev server** — `cch dev` with nREPL + file watching + dashboard
- [ ] **bbin distribution** — `bbin install` for global CLI installation
- [ ] **More hooks** — protect-files, command-audit, format-on-save, slow-confirm
