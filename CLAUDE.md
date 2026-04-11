# CLAUDE.md

## Project Overview

`cch` is a Babashka-based framework for Claude Code hooks — the lifecycle events (PreToolUse, PostToolUse, SessionStart, etc.) that Claude Code fires as shell commands.

## Build & Test

```bash
bb test                    # Run all tests (29 tests, 70 assertions)
bb -cp src:resources -m cli.cch <command>   # Run CLI commands
```

## Architecture

See [ARCHITECTURE.md](ARCHITECTURE.md) for the full design.

**Key concepts:**
- **Three execution contexts:** hook (hot, <50ms), CLI (human), dev server (long-running)
- **`defhook` macro** generates `-main` with middleware (timing, error handling, logging)
- **Pure logic functions** for testability — `check-scope`, not `-main`, is what you test
- **SQLite event log** at `~/.local/share/cch/events.db` via fire-and-forget sqlite3 CLI
- **Settings.json management** with atomic writes and `# cch:name` tagging

## Source Layout

| Path | Purpose |
|------|---------|
| `src/cch/` | Framework core — protocol, middleware, config, logging |
| `src/hooks/` | Built-in hook implementations |
| `src/cli/` | CLI commands — init, install, list, log |
| `test/` | Mirrors src/ — unit + integration tests |
| `resources/schema.sql` | SQLite event table schema |

## Writing Hooks

1. Create `src/hooks/my_hook.clj` with a pure check function + `defhook` wrapper
2. Add metadata to `src/cli/registry.clj`
3. Write tests in `test/hooks/my_hook_test.clj`
4. Run `bb test` to verify

**Hook return values:** `nil` = allow, `{:decision :ask/:deny :reason "..."}` = prompt/block.

## Code Standards

- Pure functions for all decision logic (no I/O in check functions)
- Clojure idioms: data-first, prefer `cond`/`when` over `if` chains
- Tests mirror source structure under `test/`
- Integration tests run hooks as subprocesses (like Claude Code does)

## Important Constraints

- Hook execution budget: <50ms total including bb startup
- No pod dependencies in the hot path (adds startup latency)
- SQLite logging must be fire-and-forget (non-blocking)
- Settings.json writes must be atomic (tmp + rename)
