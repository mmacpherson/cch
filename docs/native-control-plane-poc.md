# Native control-plane proof of concept

This repository is testing a broader product identity: a multi-agent local
execution control plane. The first gate is deliberately small. It proves native
Claude-to-Codex, Codex-to-Claude, and Codex-to-Codex text routing on one machine
before a hosted broker, web UI, or data-federation redesign is built.

The POC does not wrap terminals or launch agents. Claude sessions publish their
documented per-session inbox to a separate local SQLite `control.db` from an
asynchronous `SessionStart` hook, so registration does not delay agent startup.
Codex sessions are discovered and addressed through the shared app-server
daemon. A PluMCP stdio server exposes only three operations:
`list_sessions`, `get_session`, and `send_message`.

## One-time setup

Install `cch` so it is on `PATH`, then run:

```bash
cch control install
```

The command installs one global Claude registration hook, registers the same
local MCP server with Claude and Codex, installs a narrowly matched Codex
`PreToolUse` caller-binding hook, and, on Linux, installs and starts a systemd
user service for the package-managed Codex binary:

```text
codex app-server --listen unix://
```

This local service does not use Codex's standalone shell-script installer or
updater, and it does not enable Codex Remote Control. Starting an agent after
that has no cch-specific pairing step and no additional provider login: each
native CLI continues to use its own existing account credentials. The service
captures the install-time `PATH` so its Codex threads can start the configured
cch MCP process. The MCP registrations also receive the resolved `CODEX_HOME`,
so they find the same owner-only app-server socket even when Codex uses a
non-default config directory. On platforms without systemd, start the same
local app-server command with an OS-native supervisor before running the smoke
test.

For this POC, start Codex sessions as clients of the shared app-server:

```bash
codex --remote unix://
```

There is no pairing or provider login per start. A bare `codex` currently owns
its own in-process runtime and is not discoverable through the shared daemon.
Whether a separate command or shell abbreviation is acceptable is part of the
POC go/no-go decision; cch does not install a `codex` PATH shim.

Codex asks once whether to trust newly installed hooks when a session first
sees the configuration change. That is configuration trust, not per-agent cch
pairing. Each `send_message` still displays Codex's ordinary MCP approval unless
the operator separately chooses a broader native Codex permission.

Claude Code must be version 2.1.224 or newer for native cross-session messaging.
Claude or Codex Remote Control may be enabled independently after the POC; cch
does not proxy either provider's cloud connection or approval channel.

## Manual smoke test

Start two `codex --remote unix://` sessions and one ordinary Claude session. Ask
any of them to call `list_sessions`, then send a synthetic message to another
route. The destination receives text prefixed with its claimed source route and
stable message id. Repeat the call with the same `message_id`; it returns
`duplicate` without delivering again.

The CLI offers the same narrow surface for diagnosis:

```bash
cch control sessions
cch control get codex:00000000-0000-0000-0000-000000000000
cch control send \
  --to claude:00000000-0000-0000-0000-000000000000 \
  --source operator \
  --message-id 00000000-0000-0000-0000-000000000001 \
  --message "Synthetic POC ping"
```

## Live validation result

The local matrix was exercised with two simultaneous Codex clients and one
ordinary Claude session. Real agents delivered and answered synthetic messages
in all three directions: Codex to Codex, Codex to Claude, and Claude to Codex.
The same-id retry returned `duplicate`, restarting the supervised app-server
and reconnecting both Codex clients worked, and delivery to a stopped Claude
session failed as `stale` instead of silently queueing.

The exercise found and fixed two integration boundaries. Provider CLIs do not
necessarily pass the service's `CODEX_HOME` to child MCP processes, so `control
install` reconciles both cch-owned MCP registrations with the resolved value.
Codex also does not expose its native thread id directly to the MCP child. The
Codex `PreToolUse` hook does expose both the native session id and tool-use id,
so cch records a short-lived one-time binding and overwrites a reserved
`source_proof` argument before Codex shows its normal MCP permission prompt.
The MCP server atomically consumes that proof and rejects a missing,
mismatched, expired, or replayed proof rather than attributing it to
`operator`.

Two simultaneous Codex clients then sent messages in both directions. Each
destination displayed the other client's native route as its source without
either model supplying a trusted source claim. The remaining local question is
product UX: Codex must currently be launched with `--remote unix://`.

## Security and privacy boundary

- The MCP API accepts plain text, not raw native frames. It cannot send approval
  responses, permission decisions, slash commands, or terminal keystrokes.
- Provider-native approval prompts remain in the originating CLI. In the live
  matrix, approving cch tool use authorized only the plain-text tool call; no
  approval response was exposed to or forwarded by cch.
- Codex caller bindings contain a native route, destination, optional message
  id, body digest, and timestamps. They expire, are consumed once, and never
  store the message body or provider credentials.
- Attribution covers calls through the registered cch MCP path; it is not an
  OS security boundary against a same-user process deliberately invoking
  operator CLI commands. Native tool approvals remain the authorization layer.
- Claude inbox tokens remain in the owner-readable local `control.db`. That
  operational database is separate from federated hook-event history; tokens
  are omitted from listing APIs and must never be sent to a future broker.
- Message bodies and transcript previews are not persisted by cch. Delivery
  deduplication stores only a SHA-256 digest and routing metadata.
- No private hostnames, private user identities, private repository names,
  OAuth client details, session transcripts, or real route IDs belong in this
  public repository.

## Gate before hosted work

The local POC has passed its technical gate: all three direction pairs work
with trustworthy source attribution, idempotent retries, clear stale-session
failures, restart recovery, and no permission relay. The non-default Codex
launch UX remains a product decision, not a routing uncertainty. The next gate
repeats the test across two machines. Postgres, Google OIDC, the hosted
switchboard, and federation migration remain downstream of those results.
