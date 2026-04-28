-- scope-lock ask prompts: which external paths are generating friction.
-- Strips the worktree prefix from the reason to surface the target path.
SELECT json_group_array(
  json_object(
    'target_path', target_path,
    'count',       cnt
  )
) AS result
FROM (
  SELECT
    REPLACE(
      reason,
      'scope-lock: edit outside worktree (' ||
        SUBSTR(reason, INSTR(reason,'(')+1, INSTR(reason,')')-INSTR(reason,'(')-1) ||
      '): ',
      ''
    ) AS target_path,
    COUNT(*) AS cnt
  FROM events
  WHERE timestamp >= datetime('now', :window)
    AND hook_name = 'scope-lock'
    AND decision = 'ask'
    AND reason LIKE '%edit outside worktree%'
  GROUP BY target_path
  ORDER BY cnt DESC
  LIMIT 25
);
