# Run Tests

Run the cch test suite and report results.

```bash
cd ~/projects/claude-code-hooks && bb test
```

If tests fail, read the failing test file and the corresponding source to diagnose. Fix issues and re-run until green.

Also run a manual smoke test of the hook that was most recently changed:

```bash
echo '{"cwd":"/tmp","tool_name":"Edit","tool_input":{"file_path":"/tmp/test.py"}}' \
  | bb -cp src:resources -m hooks.scope-lock
```
