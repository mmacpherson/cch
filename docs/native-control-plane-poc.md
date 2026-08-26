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
codex remote-control start
```

The first command installs one global Claude registration hook and registers the
same local MCP server with Claude and Codex. Starting an agent after that has no
cch-specific pairing step and no additional provider login: each native CLI
continues to use its own existing account credentials. Codex currently requires
the standalone distribution managed by its installer for the shared daemon; the
control plane reports an actionable error when that prerequisite is absent.

Claude Code must be version 2.1.224 or newer for native cross-session messaging.
Global Remote Control may be enabled independently; cch does not proxy its cloud
connection or approval channel.

## Manual smoke test

Start two ordinary Codex sessions and one ordinary Claude session. Ask any of
them to call `list_sessions`, then send a synthetic message to another route.
The destination receives text prefixed with its claimed source route and stable
message id. Repeat the call with the same `message_id`; it returns `duplicate`
without delivering again.

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

## Security and privacy boundary

- The MCP API accepts plain text, not raw native frames. It cannot send approval
  responses, permission decisions, slash commands, or terminal keystrokes.
- Claude inbox tokens remain in the owner-readable local `control.db`. That
  operational database is separate from federated hook-event history; tokens
  are omitted from listing APIs and must never be sent to a future broker.
- Message bodies and transcript previews are not persisted by cch. Delivery
  deduplication stores only a SHA-256 digest and routing metadata.
- No hostnames, user identities, private repository names, OAuth client details,
  session transcripts, or real route IDs belong in this public repository.

## Gate before hosted work

The local POC is successful only after all three direction pairs work with
source attribution, idempotent retries, clear stale-session failures, and no
permission relay. The next gate repeats that test across two machines. Postgres,
Google OIDC, the hosted switchboard, and federation migration remain downstream
of those results.
