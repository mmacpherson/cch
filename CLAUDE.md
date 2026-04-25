# CLAUDE.md

## Project Overview

`cch` is a Babashka-based framework for Claude Code hooks — the lifecycle events (PreToolUse, PostToolUse, SessionStart, etc.) that Claude Code fires as shell commands.

## Public Repo — No Personal Info

This is a **public open-source repo**. Keep all committed content free of personal information about the maintainer or anyone else. The only personal identifier that should appear is the standard git commit author line (name + GitHub noreply email), which is already public on every commit.

This applies to **everything that ships in the repo**: source files, comments, docstrings, tests, fixtures, commit messages, README/CLAUDE.md/ARCHITECTURE.md, and beads issue bodies/notes/design (`.beads/issues.jsonl` is committed).

Do not commit:
- Real email addresses, phone numbers, physical addresses
- Absolute paths that include a username (`/home/<user>/...`, `/Users/<user>/...`) — use `~`, `$HOME`, `(System/getProperty "user.home")`, or a tmp dir instead
- Names of family, friends, colleagues, or non-public collaborators
- Personal anecdotes, health/financial/relationship context
- API keys, tokens, SSH/TLS material, credentials of any kind
- Hostnames, internal URLs, or other infra identifiers from private systems

When writing new code or beads issues, prefer portable references (`<repo-root>`, `~/.config/...`) over hardcoded local paths. When fixing bugs, the fix and the rationale go in the diff and commit message — not personal context about how it was discovered.

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


<!-- BEGIN BEADS INTEGRATION v:1 profile:minimal hash:ca08a54f -->
## Beads Issue Tracker

This project uses **bd (beads)** for issue tracking. Run `bd prime` to see full workflow context and commands.

### Quick Reference

```bash
bd ready              # Find available work
bd show <id>          # View issue details
bd update <id> --claim  # Claim work
bd close <id>         # Complete work
```

### Rules

- Use `bd` for ALL task tracking — do NOT use TodoWrite, TaskCreate, or markdown TODO lists
- Run `bd prime` for detailed command reference and session close protocol
- Use `bd remember` for persistent knowledge — do NOT use MEMORY.md files

## Session Completion

**When ending a work session**, you MUST complete ALL steps below. Work is NOT complete until `git push` succeeds.

**MANDATORY WORKFLOW:**

1. **File issues for remaining work** - Create issues for anything that needs follow-up
2. **Run quality gates** (if code changed) - Tests, linters, builds
3. **Update issue status** - Close finished work, update in-progress items
4. **PUSH TO REMOTE** - This is MANDATORY:
   ```bash
   git pull --rebase
   bd dolt push
   git push
   git status  # MUST show "up to date with origin"
   ```
5. **Clean up** - Clear stashes, prune remote branches
6. **Verify** - All changes committed AND pushed
7. **Hand off** - Provide context for next session

**CRITICAL RULES:**
- Work is NOT complete until `git push` succeeds
- NEVER stop before pushing - that leaves work stranded locally
- NEVER say "ready to push when you are" - YOU must push
- If push fails, resolve and retry until it succeeds
<!-- END BEADS INTEGRATION -->
