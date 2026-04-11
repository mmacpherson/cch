Review workspace changes, create clean commits, verify lint/tests, and push.

## 1. Understand the changes

- `git status` to see untracked, unstaged, and staged files
- `git diff` and `git diff --staged` to understand the actual changes
- `git log --oneline -5` for recent commit style

## 2. Decide what belongs in this commit

For each changed/untracked file, decide: **commit, stash, or delete?**

- **Clearly related to current work** → stage it
- **Unrelated edits** → ask the user. Suggest stashing or a separate commit.
- **Temp files, experiments, scratch** → ask before deleting
- **Enormous files, binaries** → flag to user
- **Sensitive files (.env, credentials)** → never commit, warn the user

Use discretion. If something feels off, stop and ask rather than blindly committing.

## 3. Stage and commit

Group related changes into focused commits. If the changes span multiple logical units (e.g. a refactor + a new feature + a config fix), make separate commits — don't lump everything into one.

For each commit:
- `git add <specific-paths>` (never `git add .` or `git add -A`)
- Write a clear commit message following the repo's style (~50-72 char subject, imperative mood)
- Add body if the "why" needs explanation
- End with the correct co-author for your model:
  - Opus: `Co-Authored-By: Claude Opus 4.6 <noreply@anthropic.com>`
  - Sonnet: `Co-Authored-By: Claude Sonnet 4.5 <noreply@anthropic.com>`
- Use a HEREDOC for the commit message to preserve formatting

## 4. Handle pre-commit hook failures

If hooks fail: fix the issues, re-stage, create a NEW commit (never `--amend` after hook failure — the failed commit didn't happen, so amending would modify the previous commit).

## 5. Verify clean workspace

Run `git status`. The workspace MUST be clean — nothing untracked, nothing unstaged.

If anything remains, address it explicitly:
- Stash unrelated WIP
- Delete known temp/scratch files
- Ask the user about anything ambiguous

Do NOT proceed until `git status` shows a clean tree.

## 6. Run lint

```bash
just lint
```

All checks must pass. If any fail, fix and re-commit (new commit, not amend).

## 7. Run tests

```bash
just test
```

All tests must pass. If any fail, investigate and fix.

## 8. Push

```bash
git push
```

If no remote is configured yet, ask the user whether to create one.

---

**Goal:** End with a clean workspace, all lint passing, all tests passing, and code pushed.
