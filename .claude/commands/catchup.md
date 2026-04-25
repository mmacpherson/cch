Read all files modified in the current branch vs `main`, then summarize what changed.

## 1. Find changed files

```bash
git diff --name-only main...HEAD
```

If the current branch *is* `main`, fall back to recent uncommitted/recent state:

```bash
git diff --name-only HEAD~10..HEAD
```

Filter out:
- Binary files (use `git diff --numstat main...HEAD` to spot `-` markers indicating binary)
- Deleted files (they're gone — read the parent commit's version if needed via `git show`)
- Generated/cached output (e.g. `.beads/embeddeddolt/`, `dist/`, `build/`)

## 2. Read in parallel

Read each remaining file with the `Read` tool. Multiple Read calls in a single response will be batched automatically — fan them out rather than reading sequentially.

## 3. Summarize

Provide a concise summary organized by *type of change*, not by file:

- **New features** — what was added and why
- **Bug fixes** — what broke and how it's fixed
- **Refactors** — what was reorganized; what's now simpler
- **Tests** — coverage added/dropped
- **Docs / config** — anything that affects how the project is built, run, or onboarded

Cap the summary at ~200 words unless the user asks for more depth. Reference specific file paths and line numbers (`src/cch/projections.clj:147`) so the user can navigate directly.

## 4. End with a punchlist

A short bulleted list of "what's left before this branch ships" — anything the diff suggests is unfinished (TODO comments, half-implemented features, missing tests, unpushed beads work, dirty .beads/ state).

---

**Goal:** Hand the user a 30-second briefing on what they (or another contributor) was working on, organized by intent rather than file order, with specific pointers for follow-up.
