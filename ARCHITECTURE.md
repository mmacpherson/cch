# Architecture

## Product boundary

Common Craft Hall (`cch`) is a multi-agent local execution control plane. It
coordinates native agent runtimes without replacing them.

The most important boundary is **native authority**:

- Claude Code, Codex, and AGY own execution and provider authentication.
- Provider-native interfaces own full transcripts, approvals, terminal input,
  and rich session history.
- cch owns local policy, observation, sanitized presence, ordinary-text routing,
  and normalized fleet read models.

This makes the broker useful but non-critical. A broker outage prevents new
cross-runner coordination; it does not stop an agent or move its execution away
from its runner.

## Execution contexts

### 1. Native provider adapters

Adapters translate provider-specific capabilities into a deliberately small cch
contract:

- Claude Code lifecycle hooks register sessions and dispatch local policy.
- Claude Code and Codex expose cch routing tools through MCP.
- Codex sessions are discovered and addressed through its shared app-server.
- AGY lifecycle and status-line feeds contribute supported observations.

Adapter code is isolated under `src/cch/control/` and `src/cch/agents/`. The rest
of the system deals in normalized sessions, messages, activity, and usage data.

### 2. Runner-local server

`cch serve` is a long-running JVM Clojure process on each runner. It provides:

- in-process hook dispatch on the provider hot path;
- SQLite-backed event, configuration, and usage history;
- local overview, events, hooks, and usage pages;
- native session discovery and delivery endpoints for the paired runner;
- health and diagnostic endpoints.

Claude Code HTTP hooks POST to the loopback dispatcher. The server process is
started once, so JVM startup is not part of each tool call.

### 3. Paired runner service

The runner service maintains an outbound relationship with the broker. It
registers sanitized session presence, polls for leased messages, delivers them
through the local native adapter, and acknowledges the result.

Pairing is a one-time machine operation. Provider sessions register
automatically after installation and do not each need broker credentials.

### 4. Broker and fleet application

The central broker stores the minimum shared coordination state:

- runner and session presence;
- display names and operator aliases;
- message identifiers, leases, attempts, and terminal delivery status;
- normalized activity observations;
- normalized usage-window observations.

Postgres is the durable production store. An in-memory broker exists for tests
and disposable demonstrations. The authenticated fleet application reads the
same broker model and presents overview, agents, events, and usage pages.

The broker does not accept provider credentials and does not become a PTY or
transcript relay.

### 5. CLI

The `cch` CLI installs provider integrations, manages services, queries local
state, diagnoses wiring, pairs runners, and exposes lower-level control-plane
operations. It is an administrative surface, not an additional runtime.

## Topology

```text
┌──────────────── runner A ────────────────┐
│ Claude ─┐                               │
│ Codex ──┼─ native adapters ─ cch server │
│ AGY ────┘                     │         │
│                         local SQLite    │
│                               │         │
│                       paired runner ────┼────┐
└─────────────────────────────────────────┘    │ outbound HTTPS
                                               ▼
┌──────────────── runner B ────────────────┐  broker/Postgres
│ native agents ─ adapters ─ cch server   │    │
│                              │           │    └─ authenticated fleet UI
│                      paired runner ──────┼────┘
└──────────────────────────────────────────┘
```

## Session and message model

### Registration

A native adapter publishes a session with:

- an opaque route identifier;
- provider family;
- availability and coarse native state;
- a broker-safe mnemonic or explicit alias;
- runner identity;
- an exact native session URL when the provider supplies one.

Paths, transcripts, prompts, and provider credentials are not session-directory
fields. Human-readable names are presentation data; routing continues to use the
opaque route identifier.

### Sending ordinary text

The message protocol is intentionally narrower than terminal control:

1. An authenticated local caller supplies a source route, destination route,
   stable message id, and ordinary text.
2. The broker validates that the source belongs to the caller's runner and
   creates an idempotent delivery record.
3. The destination runner leases the message.
4. Its native adapter injects text using the provider-supported session API.
5. The runner acknowledges delivery or a terminal failure.
6. The broker retains delivery metadata, not a durable message body.

Commands, approval decisions, keystrokes, terminal input, and credentials are
rejected at this boundary. Rich interaction belongs in the native interface.

## Data ownership and federation

Federation is read-model replication, not database federation.

### Runner-local source data

Each runner's SQLite database remains authoritative for detailed events and
usage samples produced there. Raw lifecycle payloads can contain sensitive
commands, prompts, paths, or tool inputs and stay local.

### Fleet observations

The runner derives explicit broker-safe records:

- **activity observations** describe agent family, runner, action category,
  outcome, and timing without copying raw payloads;
- **usage observations** describe provider rate-limit windows and projection
  inputs without copying unrelated event history.

These schemas are versioned product contracts. The fleet UI consumes them from
the broker rather than querying or synchronizing runner SQLite tables.

This replaces the earlier design that copied append-only event tables between
machines. The normalized approach has fewer moving parts, clearer privacy
boundaries, and no cross-machine SQLite ownership problem.

## Boundary schemas

External contracts should be expressed as Malli data schemas. The intended
scope is provider payloads, MCP arguments, broker HTTP requests and responses,
runner registration, message envelopes, authentication claims, and normalized
activity and usage observations.

Schemas are compiled once at startup. Federation schemas are closed or followed
by explicit field selection so adding a runner-local field cannot implicitly
publish it. Coercion is conservative and explicit, and validation errors name
the invalid field without echoing potentially sensitive values.

Inside a validated boundary, cch continues to use ordinary immutable maps and
pure transforms. Schema checks do not belong on every internal function call or
database read. The same boundary schemas can support generated contract tests
and protocol documentation.

## Hook dispatch

Claude Code sends lifecycle events to loopback URLs of the form:

```text
POST http://127.0.0.1:8888/dispatch/<event>
```

The server uses registry metadata to select enabled hooks whose event and
matcher apply, calls their pre-composed middleware-wrapped handlers, reconciles
their decisions, and renders the provider-specific response envelope.

Hook functions return a normalized decision:

```clojure
nil
{:decision :allow :reason "..."}
{:decision :ask   :reason "..."}
{:decision :deny  :reason "..."}
```

`nil` is the fast allow path. Provider response shapes vary by event, so
`cch.protocol/->response` owns rendering and hook logic stays data-oriented.

The default middleware stack supplies timing, fail-closed error handling, and
local SQLite logging. Hooks run in the long-lived server process and should keep
their policy checks small and deterministic.

## Local storage

SQLite is appropriate for runner-local state because each runner has one local
authority, writes are modest, and queries benefit from an embedded transactional
database. Current schemas include detailed events, hook configuration, context
and usage snapshots, and projection support.

Postgres is appropriate at the central broker because multiple runners and the
fleet UI share durable coordination state. The two databases have different
roles; the broker is not a remote replacement for runner SQLite.

## Configuration and installation

Portable state lives below the existing `cch` namespace:

- `~/.config/cch/` — user configuration and owner-only pairing material;
- `~/.local/share/cch/` — installed code/runtime and SQLite data;
- `.cch-config.yaml` — optional project policy;
- `cch.service` / `com.cch.*` — OS-native service identifiers.

`cch install --all` detects supported provider CLIs and reconciles only
cch-owned configuration entries. Existing unrelated hooks and MCP servers are
preserved. Settings writes are atomic.

The pairing file contains a runner-scoped broker credential and must remain
owner-only. Deployment endpoints, tokens, allowed human identities, and machine
names are private deployment configuration and must not be committed.

## Web surfaces

The runner-local application exposes:

- **Overview** — local status and recent activity;
- **Events** — detailed local execution history;
- **Hooks** — runner-specific policy configuration;
- **Usage** — locally observed rate-limit windows and forecasts.

The authenticated fleet application exposes:

- **Overview** — fleet presence and recent normalized activity;
- **Agents** — session identification, aliases, native links, and text routing;
- **Events** — normalized cross-runner activity with runner attribution;
- **Usage** — global provider usage windows and projections.

Hooks do not appear in the fleet application because hook policy belongs to a
runner. Usage has near-identical semantics on both surfaces because its
normalized read model is fleet-safe.

## Failure behavior

- Local hooks and agents continue when the broker is unavailable.
- Runner polling and registration retries are bounded and idempotent.
- Stable message ids prevent a retry from creating a second logical delivery.
- Message leases allow recovery after a runner interruption.
- A destination that cannot accept native input reports a terminal result
  instead of falling back to PTY injection.
- Full provider state remains available through native interfaces even when cch
  has only coarse presence.

## Source map

```text
src/
├── cch/
│   ├── agents/             Provider observation adapters
│   ├── control/            Sessions, broker, runner, MCP, native adapters, web auth
│   ├── server.clj          Runner-local HTTP dispatcher and application
│   ├── db.clj              SQLite connection and schema lifecycle
│   ├── events.clj          Local event queries
│   ├── usage*.clj          Usage observations and read models
│   ├── activity*.clj       Normalized activity observations
│   └── protocol.clj        Provider hook response rendering
├── hooks/                  Built-in local policy and observation hooks
└── cli/                    Installation, service, diagnostics, and control commands
```

## Design principles

1. **Native first.** Use supported provider APIs and links; do not scrape a TUI
   when a structured interface exists.
2. **Local execution.** Credentials, code execution, transcripts, and detailed
   history stay with the runner and provider.
3. **Narrow federation.** Share purpose-built observations, not raw databases.
4. **Least authority.** Agent routing accepts ordinary text and nothing more
   powerful.
5. **Graceful degradation.** Coordination failure must not become execution
   failure.
6. **Opaque routing, readable presentation.** Names help humans; stable ids keep
   delivery unambiguous.
7. **Idempotent boundaries.** Registration, delivery, acknowledgement, and
   installation tolerate retries.
8. **Schemas at edges, maps within.** Validate and minimize external data once;
   keep the internal model simple and composable.
