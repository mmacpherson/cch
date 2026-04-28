-- Decision summary by hook over the review window.
-- Arg: :window e.g. '-7 days'
SELECT json_group_array(
  json_object(
    'hook',     hook_name,
    'decision', decision,
    'count',    cnt
  )
) AS result
FROM (
  SELECT hook_name, decision, COUNT(*) AS cnt
  FROM events
  WHERE timestamp >= datetime('now', :window)
    AND decision IS NOT NULL
  GROUP BY hook_name, decision
  ORDER BY hook_name, cnt DESC
);
