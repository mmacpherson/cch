CREATE TABLE IF NOT EXISTS events (
  id          INTEGER PRIMARY KEY AUTOINCREMENT,
  timestamp   TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%f', 'now')),
  agent       TEXT NOT NULL DEFAULT 'claude-code',
  session_id  TEXT,
  hook_name   TEXT NOT NULL,
  event_type  TEXT NOT NULL,
  tool_name   TEXT,
  file_path   TEXT,
  cwd         TEXT,
  decision    TEXT,
  reason      TEXT,
  elapsed_ms  REAL,
  extra       TEXT,
  -- Legacy federation provenance retained so existing databases remain
  -- readable. New rows leave both fields NULL; hook-event tables stay local.
  node        TEXT,
  origin_id   INTEGER
);

CREATE INDEX IF NOT EXISTS idx_events_session   ON events(session_id);
CREATE INDEX IF NOT EXISTS idx_events_timestamp ON events(timestamp);
CREATE INDEX IF NOT EXISTS idx_events_hook      ON events(hook_name);
CREATE INDEX IF NOT EXISTS idx_events_decision  ON events(decision);
CREATE INDEX IF NOT EXISTS idx_events_cwd       ON events(cwd);
CREATE INDEX IF NOT EXISTS idx_events_event     ON events(event_type);
CREATE INDEX IF NOT EXISTS idx_events_agent     ON events(agent);
CREATE INDEX IF NOT EXISTS idx_events_node      ON events(node);
CREATE UNIQUE INDEX IF NOT EXISTS idx_events_node_origin ON events(node, origin_id);

-- Per-hook configuration: which hooks are enabled at which scope, plus
-- a free-form JSON options blob. scope is either the literal 'global'
-- or 'repo:<abs-path>'. Source of truth for the HTTP dispatcher's
-- routing decisions — see src/cch/server.clj.
CREATE TABLE IF NOT EXISTS hook_config (
  hook_name  TEXT NOT NULL,
  scope      TEXT NOT NULL,
  enabled    INTEGER NOT NULL DEFAULT 1,
  options    TEXT,
  updated_at TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%f', 'now')),
  PRIMARY KEY (hook_name, scope)
);

CREATE INDEX IF NOT EXISTS idx_hook_config_scope ON hook_config(scope);

-- Per-turn context window snapshots fed by the statusLine command.
-- The context-governor hook queries the latest row per session to
-- decide whether to inject a "please compact" advisory.
CREATE TABLE IF NOT EXISTS context_snapshots (
  id             INTEGER PRIMARY KEY AUTOINCREMENT,
  timestamp      TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%f', 'now')),
  agent          TEXT NOT NULL DEFAULT 'claude-code',
  session_id     TEXT NOT NULL,
  used_pct       REAL,
  current_tokens INTEGER,
  window_size    INTEGER,
  model_id       TEXT,
  payload        TEXT,
  -- Legacy federation provenance; new local rows leave both fields NULL.
  node           TEXT,
  origin_id      INTEGER
);

CREATE INDEX IF NOT EXISTS idx_ctx_session   ON context_snapshots(session_id);
CREATE INDEX IF NOT EXISTS idx_ctx_timestamp ON context_snapshots(timestamp);
CREATE INDEX IF NOT EXISTS idx_ctx_agent     ON context_snapshots(agent);
CREATE INDEX IF NOT EXISTS idx_ctx_node      ON context_snapshots(node);
CREATE UNIQUE INDEX IF NOT EXISTS idx_ctx_node_origin ON context_snapshots(node, origin_id);

-- Narrow, provider-neutral observations used by the forecast federation.
-- These intentionally exclude session, account, machine, repository, model,
-- and provider payload identity. event_id is a deterministic content hash, so
-- retrying a local capture or remote delivery is idempotent. publishable=0 is
-- reserved for remotely materialized rows and prevents federation echo.
CREATE TABLE IF NOT EXISTS usage_observations (
  id              INTEGER PRIMARY KEY AUTOINCREMENT,
  event_id        TEXT NOT NULL UNIQUE,
  schema_version  INTEGER NOT NULL,
  observed_at     INTEGER NOT NULL,
  agent           TEXT NOT NULL,
  window_key      TEXT NOT NULL CHECK (window_key IN ('five_hour', 'seven_day')),
  used_percentage REAL NOT NULL CHECK (used_percentage >= 0 AND used_percentage <= 100),
  resets_at       INTEGER NOT NULL,
  publishable     INTEGER NOT NULL DEFAULT 1 CHECK (publishable IN (0, 1)),
  received_at     TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%f', 'now'))
);

CREATE INDEX IF NOT EXISTS idx_usage_observations_forecast
  ON usage_observations(agent, window_key, resets_at, observed_at);
CREATE INDEX IF NOT EXISTS idx_usage_observations_publish
  ON usage_observations(publishable, id);

-- Independent crash-safe cursors for the paired runner's normalized usage
-- exchange. These are machine-local transport state, never federated data.
CREATE TABLE IF NOT EXISTS usage_sync_state (
  direction  TEXT PRIMARY KEY CHECK (direction IN ('publish', 'pull')),
  cursor     INTEGER NOT NULL DEFAULT 0 CHECK (cursor >= 0),
  updated_at TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%f', 'now'))
);

-- Incremental source cursor for bounded import from the legacy local snapshot
-- table during the normalized-forecast migration. Only source-local rows are
-- imported; the table can be removed after the compatibility path is retired.
CREATE TABLE IF NOT EXISTS usage_backfill_state (
  singleton_id    INTEGER PRIMARY KEY CHECK (singleton_id = 1),
  last_context_id INTEGER NOT NULL DEFAULT 0 CHECK (last_context_id >= 0),
  updated_at      TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%f', 'now'))
);

-- Covering expression indexes for the forecast's completed-window "finals"
-- queries (cch.forecast/historical-finals-sql). Those GROUP BY resets_at and
-- MAX(used_percentage) per window across ALL history — an unbounded scan that,
-- without these, re-parses every payload blob (seconds per call on a large DB;
-- once pinned a core in the bg-refresh loop). Indexing the extracted JSON paths
-- makes the query index-only (~50ms), never touching the payload column.
-- The column order matches the query's filters: agent (=), resets_at (range +
-- GROUP BY), then used_percentage and session_id to make the index cover it.
CREATE INDEX IF NOT EXISTS idx_ctx_7d_finals ON context_snapshots(
  agent,
  json_extract(payload, '$.rate_limits.seven_day.resets_at'),
  json_extract(payload, '$.rate_limits.seven_day.used_percentage'),
  session_id);
CREATE INDEX IF NOT EXISTS idx_ctx_5h_finals ON context_snapshots(
  agent,
  json_extract(payload, '$.rate_limits.five_hour.resets_at'),
  json_extract(payload, '$.rate_limits.five_hour.used_percentage'),
  session_id);

-- Covering index for the 7d statusline sample query (cch.forecast/filtered-
-- samples): a windowed CTE over the last 7 days of snapshots. Ordered
-- agent (=), timestamp (range + ORDER BY), then the extracted pct/resets_at and
-- session_id so the CTE's source scan is index-only. Takes that query from
-- ~350ms to ~40ms. (The 5h sample query spans only 5h of rows and is already
-- cheap, so it doesn't need one.)
CREATE INDEX IF NOT EXISTS idx_ctx_7d_samples ON context_snapshots(
  agent,
  timestamp,
  json_extract(payload, '$.rate_limits.seven_day.used_percentage'),
  json_extract(payload, '$.rate_limits.seven_day.resets_at'),
  session_id);

-- Legacy shipper watermarks retained for non-destructive upgrades. Current
-- releases neither read nor advance this table.
CREATE TABLE IF NOT EXISTS federation_offsets (
  table_name      TEXT PRIMARY KEY,
  last_shipped_id INTEGER NOT NULL DEFAULT 0,
  updated_at      TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%f', 'now'))
);
