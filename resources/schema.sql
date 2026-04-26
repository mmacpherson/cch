CREATE TABLE IF NOT EXISTS events (
  id          INTEGER PRIMARY KEY AUTOINCREMENT,
  timestamp   TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%f', 'now')),
  session_id  TEXT,
  hook_name   TEXT NOT NULL,
  event_type  TEXT NOT NULL,
  tool_name   TEXT,
  file_path   TEXT,
  cwd         TEXT,
  decision    TEXT,
  reason      TEXT,
  elapsed_ms  REAL,
  extra       TEXT
);

CREATE INDEX IF NOT EXISTS idx_events_session   ON events(session_id);
CREATE INDEX IF NOT EXISTS idx_events_timestamp ON events(timestamp);
CREATE INDEX IF NOT EXISTS idx_events_hook      ON events(hook_name);
CREATE INDEX IF NOT EXISTS idx_events_decision  ON events(decision);
CREATE INDEX IF NOT EXISTS idx_events_cwd       ON events(cwd);
CREATE INDEX IF NOT EXISTS idx_events_event     ON events(event_type);

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
  session_id     TEXT NOT NULL,
  used_pct       REAL,
  current_tokens INTEGER,
  window_size    INTEGER,
  model_id       TEXT,
  payload        TEXT
);

CREATE INDEX IF NOT EXISTS idx_ctx_session   ON context_snapshots(session_id);
CREATE INDEX IF NOT EXISTS idx_ctx_timestamp ON context_snapshots(timestamp);
