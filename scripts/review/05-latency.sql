-- Hook latency: avg and max elapsed_ms, flagging anything above 20ms average.
SELECT json_group_array(
  json_object(
    'hook',    hook_name,
    'avg_ms',  avg_ms,
    'max_ms',  max_ms,
    'n',       n,
    'slow',    CASE WHEN avg_ms > 20 THEN 1 ELSE 0 END
  )
) AS result
FROM (
  SELECT
    hook_name,
    ROUND(AVG(elapsed_ms), 1) AS avg_ms,
    ROUND(MAX(elapsed_ms), 1) AS max_ms,
    COUNT(*) AS n
  FROM events
  WHERE timestamp >= datetime('now', :window)
    AND elapsed_ms IS NOT NULL
  GROUP BY hook_name
  ORDER BY avg_ms DESC
);
