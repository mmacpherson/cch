-- Actual commands that triggered command-guard denials.
-- Extracts the command string from the extra JSON payload.
SELECT json_group_array(
  json_object(
    'command', command,
    'reason',  reason,
    'cwd',     cwd,
    'count',   cnt
  )
) AS result
FROM (
  SELECT
    json_extract(extra, '$.tool_input.command') AS command,
    reason,
    cwd,
    COUNT(*) AS cnt
  FROM events
  WHERE timestamp >= datetime('now', :window)
    AND hook_name = 'command-guard'
    AND decision = 'deny'
  GROUP BY command
  ORDER BY cnt DESC
  LIMIT 20
);
