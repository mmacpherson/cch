# Native control-plane proof of concept

This repository is testing a broader product identity: a multi-agent local
execution control plane. The first gate deliberately proved native
Claude-to-Codex, Codex-to-Claude, and Codex-to-Codex text routing on one machine.
The second gate added two paired runners and a disposable broker before a
hosted service, web UI, or data-federation redesign. Both physical-machine
gates passed; the implementation is now being hardened into the production
foundation described below.

The POC does not wrap terminals or launch provider agent sessions. Claude
sessions publish their documented per-session inbox to a separate local SQLite
`control.db` from an asynchronous `SessionStart` hook, so registration does not
delay agent startup. Codex sessions are discovered and addressed through the
shared app-server daemon. A PluMCP stdio server exposes only four operations:
`list_sessions`, `get_session`, `send_message`, and `set_session_alias`.

## One-time setup

Install `cch` so it is on `PATH`, then run:

```bash
cch control install
```

The command installs one global Claude registration hook, registers the same
local MCP server with Claude and Codex, installs a narrowly matched Codex
`PreToolUse` caller-binding hook, and installs and starts a systemd user service
or macOS LaunchAgent for the package-managed Codex binary:

```text
codex app-server --listen unix://
```

This local service does not use Codex's standalone shell-script installer or
updater. It does not change Codex Remote Control configuration. Starting an
agent after that has no cch-specific pairing step and no additional provider
login: each native CLI continues to use its own existing account credentials.
Hooks and MCP registrations use the absolute installed `cch` executable and
receive the resolved `CODEX_HOME`, so service-launched agents do not depend on
an interactive shell's `PATH` and find the same owner-only app-server socket.
On platforms without systemd or launchd, start the same local app-server
command with an OS-native supervisor before running the smoke test.

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
pairing. The installer allowlists exactly `list_sessions`, `get_session`,
`send_message`, and `set_session_alias` on Codex's `cch` MCP server and grants
exactly the corresponding four Claude permission rules. It does not use a
wildcard or change either provider's global approval policy, auto mode, or
permissions for other tools.

### Refreshing active sessions after a cch upgrade

Re-run `cch control install` after upgrading cch so future sessions receive the
current absolute executable path, environment, hooks, and tool allowlist. The
installer deliberately does not restart active agents or their existing MCP
subprocesses. It also records a SHA-256 revision of the deployed cch code in
the cch-owned MCP environment. The digest contains no source, paths, machine
identity, or credentials; it gives provider refresh machinery a deterministic
configuration change when the executable path itself stays constant.

An active Claude session keeps the cch subprocess and tool catalogue it started
with. Refresh only that session by entering this native Claude Code command in
it:

```text
/mcp reconnect cch
```

The conversation remains active and unrelated Claude sessions are untouched.
Run the command once in each already-running Claude session that needs the new
cch version. `cch control refresh-mcp claude` prints this instruction when it is
not convenient to remember it.

Codex sessions connected to cch's shared app-server use its typed MCP reload
request instead:

```bash
cch control refresh-mcp codex
```

The command calls `config/mcpServer/reload`, then checks
`mcpServerStatus/list` for exactly cch's expected tools. Codex defines reload at
app-server scope, so other MCP servers managed by that daemon may reconnect
briefly. The shared daemon and all attached Codex agent processes remain
running. A failure to restore cch is reported rather than presented as a
successful refresh.

These are explicit operator actions rather than part of install or upgrade.
New Claude sessions and new `codex --remote unix://` sessions start the current
cch MCP command automatically and need no refresh step.

Claude Code must be version 2.1.224 or newer for native cross-session messaging.
Claude or Codex Remote Control may be enabled independently after the POC; cch
does not proxy either provider's cloud connection or approval channel.

## Disposable cross-runner broker

The cross-machine POC is a separate, transient transport. A broker holds an
in-memory directory of short route leases and in-memory message envelopes. Each
machine runs one outbound polling runner. Agents do not connect to the broker,
open listening ports, or pair themselves; their already-installed cch MCP
process sends through the machine's runner identity.

Create one opaque id and random pairing token per machine. On the broker, pass
the mapping as process environment and bind to loopback behind an existing
Tailscale HTTPS proxy:

```bash
export CCH_CONTROL_RUNNER_TOKENS='{"runner-a":"replace-with-random-token-a","runner-b":"replace-with-random-token-b"}'
cch control broker --host 127.0.0.1 --port 8787
```

Pair each machine once with its own values. The HTTPS URL is the private overlay
URL that forwards to the broker's loopback listener:

```bash
export CCH_CONTROL_BROKER_URL='https://broker-name.example-tailnet.ts.net'
export CCH_CONTROL_RUNNER_ID='runner-a'
export CCH_CONTROL_RUNNER_TOKEN='replace-with-random-token-a'

cch control pair
```

`control pair` atomically stores the three values in an owner-only
`control-runner.json` outside the repository and installs the outbound runner
as a systemd user service or macOS LaunchAgent. The token is not copied into
provider MCP definitions or service files. Hooks, MCP processes, and the runner
all use the same local configuration. Starting dozens of later agent sessions
requires no cch authentication or pairing chore. Re-running setup never
restarts an active runner or shared Codex app-server implicitly.

The runner refreshes only `id`, agent family, availability, and coarse native
status. It omits names, cwd values, PIDs, sockets, provider credentials, and
transcript data. The broker accepts only ordinary text messages, retries
unacknowledged delivery three times within a short expiry window, and drops the
body after a terminal acknowledgement or expiry. Pairing tokens are never
written to this repository or printed by the broker.

Unsetting the three `CCH_CONTROL_*` runner values returns cch to local-only
routing only when there is no saved pairing file. Remove or replace that local
file deliberately to unpair a machine. Stopping the runner or broker does not
stop, wrap, or alter any native Claude or Codex session.

## Production Postgres broker

When `CCH_CONTROL_DATABASE_URL` is set, the same broker command replaces the
disposable route directory with Postgres-backed leases and message metadata.
If it is unset, the broker intentionally stays in memory-only development
mode. A shared Postgres cluster is appropriate, but cch should own an isolated
database or schema and a role that cannot access other applications' tables.
For example, run the privileged setup once with values chosen outside this
repository:

```sql
CREATE ROLE cch_control_app LOGIN PASSWORD 'replace-outside-source-control';
GRANT CONNECT ON DATABASE application_data TO cch_control_app;
CREATE SCHEMA cch_control AUTHORIZATION cch_control_app;
```

Then configure the broker process through private deployment environment, not
checked-in files:

```bash
export CCH_CONTROL_DATABASE_URL='jdbc:postgresql://db.internal/application_data'
export CCH_CONTROL_DATABASE_USER='cch_control_app'
export CCH_CONTROL_DATABASE_PASSWORD='replace-outside-source-control'
export CCH_CONTROL_DATABASE_SCHEMA='cch_control'       # default
export CCH_CONTROL_DATABASE_POOL_SIZE='4'              # clamped to 1..8
```

Startup takes a transaction-scoped Postgres advisory lock and applies numbered
migrations inside the pre-created schema. The application role needs to own
only that schema; it does not need cluster administration or access to other
schemas. The broker pool defaults to four connections and enables PostgreSQL
TCP keepalive.

Postgres is authoritative for runner leases, sanitized route ownership,
message ids, content digests, delivery status, attempt counts, and expiry.
There is deliberately no message-body, token, credential, transcript, cwd,
socket, or provider-native metadata column. Message text remains in the active
broker process for a maximum of 30 seconds. After a broker restart, a pending
metadata row moves to `awaiting-replay`; an identical source retry rehydrates
the transient body, while a different body or route with that id is rejected.
Destination-side durable deduplication still prevents a replay from becoming a
second native submission. Terminal metadata is retained for 24 hours by
default and then removed, bounding the idempotency ledger.

## Cloudflare Access-protected operator switchboard

The broker can also serve a small, server-rendered switchboard at `/`. It shows
only the sanitized active route directory: agent family, opaque route id,
opaque runner id, bounded provider-advertised session name, native status, and
a coarse ready/working/needs-you state. It has no transcript, prompt history,
cwd, process id, socket, token, or message body view. A native action is shown
only when the local adapter advertises a narrowly validated, exact session URL;
generic provider home links are omitted.
For a Claude session with Remote Control active, the adapter incrementally
reads Claude's structured `bridge_status` transcript event, caches its dedicated
`claude.ai/code/session_...` URL in the machine-local operational store, and
leases only that URL with presence. Codex 0.149 exposes daemon-level Remote
Control status but no per-thread URL in its typed app-server protocol, so Codex
sessions intentionally have no native action yet. cch never reads or federates
transcript content beyond that provider-authored bridge event.

Every route also receives a stable mnemonic derived only from its opaque route
id, such as `quiet-otter-a31f`. When Claude or Codex advertises the same bounded
native session name shown by its own UI, cch carries that name as presentation
metadata. An operator or the owning agent can set a short explicit alias, which
takes precedence over the native name. The switchboard gives that meaningful
name the heading by itself. It shows the mnemonic as the heading only for an
unnamed route or beside duplicate names that require disambiguation; otherwise
the mnemonic, route id, runner, native state, and rename control remain under a
collapsed details disclosure. Routing always continues to use the route id.
Native names and aliases are bounded and escaped. They are broker-visible by
design; cwd, prompts, transcript content, credentials, and other local metadata
are not used to derive a name. Runner-authenticated alias updates must target a
route currently leased by that runner; Codex self-renames additionally use the
same one-time native-session hook binding as agent-originated messages.

The runner API and human UI use separate listeners. Machines reach only the
runner listener through tailnet-private Tailscale Serve. A stable human-facing
hostname reaches only the UI listener through an outbound Cloudflare Tunnel,
with Google identity enforced by Cloudflare Access before the request reaches
cch. Funnel and public origin ports are not required.

Configure the UI listener through its private deployment environment. The
example values below are deliberately synthetic; Access identity, operator
addresses, audience tags, and secrets must remain outside this public
repository:

```bash
export CCH_CONTROL_WEB_ORIGIN='https://control.example.invalid'
export CCH_CONTROL_CLOUDFLARE_ISSUER='https://example.cloudflareaccess.com'
export CCH_CONTROL_CLOUDFLARE_AUDIENCE='replace-with-access-audience-tag'
export CCH_CONTROL_WEB_ALLOWED_EMAILS='operator@example.invalid'
export CCH_CONTROL_WEB_SESSION_SECRET='replace-with-at-least-32-random-characters'
export CCH_CONTROL_WEB_HOST='0.0.0.0' # container listener; defaults to loopback
export CCH_CONTROL_WEB_PORT='8788'    # optional; this is the default
```

All five required values are fail-closed: a partial configuration prevents
broker startup, while leaving all of them unset disables only the human UI.
The runner JSON API remains physically absent from the web listener and is
authenticated exclusively by runner bearer tokens on its tailnet listener.

Cloudflare Access injects a signed application assertion after its Google and
email-policy checks. cch independently validates the assertion signature
against the pinned Access issuer's keys, plus issuer, audience, time claims,
subject, and the exact normalized-email allowlist. Assertion expiry is the
browser session boundary. Routing and logout forms additionally require an
HMAC-signed token bound to the stable verified Access identity and application.
Form provenance requires either the exact configured `Origin`, or the browser's
`Sec-Fetch-Site: same-origin` signal together with the exact configured host.
The Access assertion itself is never rendered or persisted.

Manual messages have the fixed source `operator`, receive a fresh id, and use
the same transient body and bounded delivery policy as agent-originated text.
The page can show delivery metadata for the message it just created, but never
stores or renders its body. Slash-command input is rejected; approvals,
permission decisions, terminal frames, and provider credentials remain outside
the contract.

The checked-in container definition packages the same JVM artifact used by the
host CLI without an installation script:

```bash
clj -T:build uber
podman build -t cch-control:local -f Containerfile .
```

Run the broker container on the private network that can resolve Postgres and
publish both listeners to host loopback only. Put Tailscale Serve in front of
the runner listener and the outbound Cloudflare Tunnel in front of the UI
listener. Serve provides tailnet-only machine transport; Cloudflare Access
provides the human Google identity boundary; Funnel remains disabled.

Run the sanitized capability check at any time:

```bash
cch control doctor
```

It checks Claude and Codex discovery, generates the installed Codex CLI's own
app-server schema to verify `thread/list` and `thread/queue/add`, and reports
pairing and runner supervision. It emits counts and versions, not route ids,
paths, broker identities, tokens, transcripts, or message bodies.

## Manual smoke test

Start two `codex --remote unix://` sessions and one ordinary Claude session. Ask
any of them to call `list_sessions`, then send a synthetic message to another
route. The destination receives text prefixed with its claimed source route and
stable message id. Repeat the call with the same `message_id`; it returns
`duplicate` without delivering again.

Ask one agent to call `set_session_alias` with a synthetic name. Only that
agent's route changes; list output and the switchboard show the alias alongside
its stable mnemonic. An empty alias clears it.

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

The disposable broker and outbound runner are exercised by an actual HTTP
integration harness with two independently authenticated logical machines.
Synthetic Codex-to-Claude delivery and its Claude-to-Codex reply cross the
broker, acknowledgements remove message bodies, duplicate ids are suppressed,
unacknowledged messages stop after three attempts, leases and envelopes expire,
and registration recovers after lease loss. This harness deliberately uses
loopback and synthetic routes. The same request/reply matrix then passed across
two physical machines over tailnet-private HTTPS, including native
Codex-to-Codex and Codex-to-Claude delivery, reconnect, acknowledgement, and
duplicate suppression. Codex versions with different TUI repaint behavior
preserved native delivery and thread persistence semantics.

## Security and privacy boundary

- The MCP API accepts plain text, not raw native frames. It cannot send approval
  responses, permission decisions, slash commands, or terminal keystrokes.
- Unknown envelope fields are rejected rather than ignored, including fields
  shaped as credentials, approval replies, permissions, commands, or raw
  frames. Provider credentials are not part of the message contract. As with
  any text channel, operators and agents must still avoid pasting secrets into
  an ordinary message body.
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
  operational database is separate from local hook-event history; tokens are
  omitted from listing APIs and are never sent to the broker.
- Message bodies and transcript previews are not persisted by cch. Delivery
  deduplication stores only a SHA-256 digest and routing metadata.
- In the disposable broker, cross-runner message bodies exist transiently in
  memory only. It stores neither provider credentials nor native machine
  metadata. A persistent pairing credential identifies a runner, not each agent
  session, and remains in one owner-only local file.
- No private hostnames, private user identities, private repository names,
  OAuth client details, session transcripts, or real route IDs belong in this
  public repository.

## Hosted-work decision

The architecture is a GO. Local and physical cross-machine tests passed with
trustworthy source attribution, idempotent retries, clear stale-session
failures, restart recovery, and no permission relay. Native local execution
continues during runner or broker failure. The non-default Codex launch UX
remains a product concern, not a routing uncertainty. Postgres broker storage,
Google OIDC protects the human webapp. Provider credentials, approvals,
transcripts, and terminal bytes remain outside the hosted boundary.

## Usage federation migration

Forecast federation now uses the paired control plane rather than copying raw
provider context payloads between machines:

- Each local capture derives a versioned observation containing only event id,
  observation time, agent kind, window kind, used percentage, and reset time.
  Session, account, repository, machine, model, path, and raw payload identity
  are excluded and rejected at the broker boundary.
- Local SQLite remains the durable write buffer. Independent publish and pull
  cursors make retries idempotent, and remotely materialized rows are marked
  non-publishable so they cannot echo around the fleet.
- The hosted Postgres broker retains 91 days. Local history import and publish
  use a 90-day horizon, leaving a transport-day buffer while preserving the
  twelve completed seven-day windows used by the learned forecast prior.
- Normalized forecasts are authoritative. Setting
  `CCH_FORECAST_USAGE_SOURCE=legacy` temporarily restores local compatibility
  reads during rollback.
- Raw `context_snapshots` and hook events remain local for native
  context-governor and dashboard behavior. The generic table shipper and
  collector are retired; legacy provenance columns remain only so existing
  SQLite databases can be read without a destructive rewrite.

Cutover required both forecast windows to agree on presence, current percentage
within 1 point, projected percentage within 2 points, reset time within 2
seconds, learned prior mean and sigma within 0.05, and at least 90% sample
coverage. Live source-local and cross-node cold-start comparisons met those
tolerances before raw context-snapshot shipping was disabled.
