# CLAUDE.md

## Project Overview

`cch` (Common Craft Hall) is a Clojure-based, local-first control plane for
collaborating coding agents. It coordinates native Claude Code and Codex
sessions across paired runners and incorporates supported AGY observations,
while leaving execution, credentials, transcripts, approvals, and terminal
control with provider runtimes. Claude Code hooks remain one local policy and
observation capability.

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
just test                  # Run all tests (clj -X:test)
clj -M:cli <command>       # Run CLI commands
clj -M:server              # Run the dispatcher (HTTP + nREPL)
```

## Architecture

See [ARCHITECTURE.md](ARCHITECTURE.md) for the full design.

**Key concepts:**
- **Native authority:** providers own execution, credentials, transcripts, approvals, and rich control
- **Runner-local authority:** SQLite holds detailed events, hook policy, and usage observations
- **Fleet coordination:** a paired runner and Postgres broker exchange sanitized presence, ordinary text, and normalized read models
- **Narrow federation:** purpose-built activity and usage observations replace raw table copying
- **`defhook` macro:** local hook decisions use shared timing, error handling, and logging middleware
- **Atomic ownership:** cch reconciles only its tagged provider configuration entries

## Source Layout

| Path | Purpose |
|------|---------|
| `src/cch/` | Local server, storage, normalized observations, and control plane |
| `src/cch/control/` | Broker, paired runner, native adapters, MCP, and fleet web application |
| `src/cch/agents/` | Provider observation adapters |
| `src/hooks/` | Built-in hook implementations |
| `src/cli/` | CLI commands — init, install, list, log |
| `test/` | Mirrors src/ — unit + integration tests |
| `resources/schema.sql` | SQLite event table schema |

## Writing Hooks

1. Create `src/hooks/my_hook.clj` with a pure check function + `defhook` wrapper
2. Add metadata to `src/cli/registry.clj`
3. Write tests in `test/hooks/my_hook_test.clj`
4. Run `just test` to verify

**Hook return values:** `nil` = allow, `{:decision :ask/:deny :reason "..."}` = prompt/block.

## Code Standards

- Pure functions for all decision logic (no I/O in check functions)
- Clojure idioms: data-first, prefer `cond`/`when` over `if` chains
- Tests mirror source structure under `test/`
- Integration tests exercise provider-shaped hook and native-control boundaries

## Important Constraints

- Hook execution budget: <50ms per dispatch — hooks run in-process inside
  the long-running JVM server (HTTP-type entries in settings.json POST to
  localhost), so JVM startup is paid once at boot, not per hook
- Raw provider event payloads remain runner-local; fleet schemas must be normalized and privacy-bounded
- Cross-agent input is ordinary text only; never broaden it to credentials, approvals, commands, or PTY input
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
