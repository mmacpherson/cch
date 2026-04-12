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
| `cch install <hook>` | Wire a hook into `settings.local.json` (or `--global`) |
| `cch uninstall <hook>` | Remove a hook from settings |
| `cch list` | Show available hooks with installation status |
| `cch log` | Query event history from SQLite |

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

Planned (not yet implemented): `protect-files`, `format-on-save`, `slow-confirm`.

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
