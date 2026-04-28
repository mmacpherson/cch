-- Top block/deny reasons — what is actually being stopped and why.
SELECT json_group_array(
  json_object(
    'hook',    hook_name,
    'tool',    tool_name,
    'reason',  reason,
    'count',   cnt
  )
) AS result
FROM (
  SELECT hook_name, tool_name, reason, COUNT(*) AS cnt
  FROM events
  WHERE timestamp >= datetime('now', :window)
    AND decision IN ('deny', 'block', 'blocked')
  GROUP BY hook_name, reason
  ORDER BY cnt DESC
  LIMIT 30
);
