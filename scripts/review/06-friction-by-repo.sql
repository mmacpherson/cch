-- Where is friction concentrated? Blocks/asks broken down by repo.
SELECT json_group_array(
  json_object(
    'cwd',      cwd,
    'hook',     hook_name,
    'decision', decision,
    'count',    cnt
  )
) AS result
FROM (
  SELECT cwd, hook_name, decision, COUNT(*) AS cnt
  FROM events
  WHERE timestamp >= datetime('now', :window)
    AND decision IN ('deny', 'block', 'blocked', 'ask')
  GROUP BY cwd, hook_name, decision
  ORDER BY cnt DESC
  LIMIT 20
);
