Review workspace changes, create clean commits, sync beads, verify lint/tests, and push.

## 1. Understand the changes

- `git status` — untracked, unstaged, staged
- `git diff` and `git diff --staged` — actual changes
- `git log --oneline -5` — match the repo's commit-message style

## 2. Decide what belongs in this commit

For each changed/untracked file, decide: **commit, stash, or delete?**

- **Clearly related to current work** → stage it
- **Unrelated edits** → ask the user. Suggest stashing or a separate commit.
- **Temp files, experiments, scratch** → ask before deleting
- **Enormous files, binaries** → flag to user
- **Sensitive files** (`.env`, credentials, secrets, anything with API keys) → never commit, warn the user
- **`.beads/` files** → **always commit**. The beads issue tracker stores state under `.beads/` (Dolt embedded data, JSONL backups, hooks, metadata). Include beads changes alongside your feature commit, or as a separate `chore(beads):` commit if substantial. **Never `git checkout -- .beads/`** — that's data loss.
- **`/home/<user>` paths** in source/tests/issues → flag and fix per the public-repo policy in `CLAUDE.md`

Use discretion. If something feels off, stop and ask rather than blindly committing.

## 3. Stage and commit

Group related changes into focused commits. If the changes span multiple logical units (e.g. a refactor + a new feature + a config fix), make separate commits — don't lump everything together.

For each commit:
- `git add <specific-paths>` — never `git add .` or `git add -A`
- Write a clear message following the repo's style (~50-72 char subject, imperative mood). Use the body to explain *why* when non-obvious.
- End with the correct co-author for your model:
  - Opus 4.7: `Co-Authored-By: Claude Opus 4.7 <noreply@anthropic.com>`
  - Sonnet 4.6: `Co-Authored-By: Claude Sonnet 4.6 <noreply@anthropic.com>`
- Use a HEREDOC (`git commit -m "$(cat <<'EOF' ... EOF)"`) to preserve formatting

## 4. Handle pre-commit hook failures

If hooks fail: fix the issues, re-stage, create a **NEW** commit. Never `--amend` after hook failure — the failed commit didn't happen, so amending would modify the *previous* commit and silently destroy that one's content.

## 5. Verify clean workspace

```bash
git status
```

The workspace must be clean — nothing untracked, nothing unstaged. Address anything remaining: stash unrelated WIP, delete known scratch, ask the user about anything ambiguous. Do **not** proceed until the tree is clean.

## 6. Run lint

```bash
just lint-all
```

All checks must pass. If any fail, fix and create a new commit (not `--amend`).

## 7. Run tests

```bash
just test
```

All tests must pass. If a test is genuinely flaky and unrelated to the change, re-run once to confirm it's a flake; if still failing, investigate and fix before pushing.

## 8. Sync beads

```bash
bd dolt push
```

Pushes local beads issue changes to the Dolt remote so other sessions and worktrees see them. This is independent of `git push` and must happen before it (the post-push verification step assumes beads is already in sync).

## 9. Push to remote

```bash
git pull --rebase
git push
git status     # MUST report "up to date with origin/<branch>"
```

If the rebase produces conflicts, resolve them (don't `git rebase --abort` and reset — that loses the work). If `git push` is rejected, investigate before retrying. Never `git push --force` to a shared branch unless the user explicitly approves.

If no remote is configured yet, ask the user whether to create one.

---

**Goal:** End with a clean workspace, all lint passing, all tests passing, beads synced to Dolt, code pushed, and `git status` reporting the branch is up to date with origin.
