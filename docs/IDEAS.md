# cch ideas & future directions

A living doc of candidate hooks, architectural themes, and design sketches. Not a roadmap — that lives in [beads](../.beads/). Think of this as the ideation journal that *feeds* the roadmap.

Structure: hooks grouped by the Green-Belt curriculum modules they'd support (since several of those exercises map neatly to cch primitives), then a north-star theme on "direction-changes as training data," then a rough shipping-order take.

---

## Context: the curriculum as a reference point

There's a Green-Belt curriculum at `~/projects/example-worktrees/claude-green-belt/projects/claude-code-study/green-belt-curriculum.md` — a four-module syllabus for advancing from Blue (structured workflows, delegation) to Green (*the system catches mistakes before you do*).

**Provenance matters:** the curriculum was Claude-generated at the operator's request, drawing on his `/insights` friction-event data (56/mo wrong-approach, 48/mo buggy-code, etc.). It's a **hypothesis from the session logs**, not an independent human prescription. That has two consequences:

1. The curriculum's specific exercises are data-grounded but not binding — Mike may disagree with the diagnosis or the recommended order.
2. **The curriculum itself is an artifact of a loop that cch is well-positioned to accelerate.** `/insights` reads session data → Claude proposes a curriculum → user practices → metrics shift → next `/insights` produces an updated curriculum. cch's event-log is the raw material for that loop. The highest-leverage cch feature might be the one that makes the loop cheaper, not the ones that ship individual curriculum exercises.

With that reframing: the hook ideas below are *candidates* to weigh against one's own priors. The curriculum is a useful cross-check, not a checklist. cch shipping an exercise doesn't replace the curriculum's pedagogical goal — the user still needs to learn when to reach for each primitive — but it does mean the primitive exists instead of having to be hand-built.

Every idea below is tagged:
- **(IS)** *is* a curriculum exercise that cch could deliver turnkey, or
- **(SUPPORTS)** infrastructure a curriculum exercise builds on top of, or
- **(ORTHOGONAL)** outside the curriculum's scope but still useful.

Baseline metrics (from `/insights`, April 2026):
- 56/mo wrong-approach interventions
- 48/mo buggy-code friction
- 10/mo excessive-changes friction
- Frequent context-recovery across sessions

Goal: prefer hooks whose shipment measurably moves those numbers down.

---

## Module 1 — Hooks as Guardrails

Curriculum target: reject mistakes before they appear in review.

### `scope-lock` · (IS Exercise 1a) · **already shipped ✓**
Exactly this module's first exercise. Already in the registry.

### `drift-detection` · (IS Exercise 1b) · **new hook**
PreToolUse · matcher `Bash` · `if: "Bash(uv run lyla services deploy *)"` (or similar deploy-shaped patterns). Before the deploy runs, diff the repo template against the deployed file on the target host; return an `ask` decision with "stale since 2026-04-05 (3 days, commit 1744db09)" if they diverge. Exactly the stale-`acquire-pdf.sh` scenario that silently broke classification.

Implementation sketch:
```clojure
(defn check-drift [cmd deploy-rules]
  (when-let [rule (match-rule cmd deploy-rules)]
    (let [[repo-hash deployed-hash age] (compare-template rule)]
      (when (not= repo-hash deployed-hash)
        {:decision :ask
         :reason (format "deploy target is %s days stale (%s vs %s)"
                         age repo-hash deployed-hash)}))))
```
`.cch-config.yaml` declares the deploy-shape patterns and how to fetch the deployed artifact per service. Easily generalizes beyond the scanning pipeline — any `scp`/`rsync`/`kubectl apply` workflow fits.

### `test-gate` · (IS Exercise 1c) · **new hook**
PostToolUse · `Edit|Write` · async · runs `ruff check` (or project's chosen linter via `.cch-config.yaml`) on just the files touched in this session and feeds results as context to the next turn. Difference from the project's existing git pre-commit: this runs *during* the session, so Claude self-corrects in flow instead of the user seeing a commit-time failure and re-prompting.

Directly addresses the 48 buggy-code friction events.

### `protect-files` · (SUPPORTS Module 1) · **new hook**
PreToolUse · `Edit|Write` · hard-deny for `.env*`, `**/secrets/**`, `~/.ssh/*`, SOPS-encrypted files. Not a curriculum exercise, but it's the "dumb-easy" guardrail every project benefits from. Ship first in any guardrail batch.

### `slow-confirm` · (ORTHOGONAL) · **new hook**
PreToolUse · `Bash` · `ask` for destructive patterns. Useful but lateral to the curriculum's focus.

### `branch-gate` · (ORTHOGONAL) · **new hook**
PreToolUse · `Edit|Write` · deny edits on protected branches. Also lateral.

---

## Module 2 — Skills That Think Before They Act

Curriculum target: encode *judgment*, not procedure.

These are **skills**, not hooks — cch hooks don't replace them but provide the substrate.

### `session-primer` · (SUPPORTS Exercise 2c) · **new hook**
SessionStart · inject `additionalContext`: current git status, last N commits on this branch, open beads ready queue, recent `cch log` highlights scoped to this cwd, the project's CLAUDE.md architecture section (first 2KB). Architecture-first `/implement` gets the architecture section for free. Directly supports reducing the 56/mo wrong-approach count — Claude sees the architecture before it acts.

### `prompt-expander` · (SUPPORTS Module 2 broadly) · **new hook**
UserPromptSubmit · config-defined `@alias` expansion (file reads, shell commands, dated-file "most recent" resolution). Makes prompts self-documenting: *"review @last-session against @plan and draft @next-week"* becomes shareable and automatable. Skills consume fewer tokens because prompts carry context.

### `mcp-allowlist` · (SUPPORTS Module 2) · **new hook**
PreToolUse · MCP tool matcher · restrict which MCP tools a project allows. cch's contribution to architecture-first: Claude can't accidentally use a tool that's wrong for this project.

---

## Module 3 — Contracts, Not Tasks

Curriculum target: define *what should be true*, not *what to do*.

This is where cch's value proposition sharpens most. The curriculum prescribes a contract YAML format and `/health-check` skill; cch can ship the primitive.

### `contract-verify` · (IS Exercise 3a/3b) · **new CLI + hook pair**
- **CLI:** `cch verify` reads `.cch-contracts/*.yml` in the current project, runs each invariant (shell check or hook call), reports pass/fail with diagnostics.
- **Hook (SessionStart or scheduled):** runs the same verification automatically. On failure, injects a context message or sends a notification via an existing system (`notify-send`, `ntfy`).
- **Storage:** verification results land in `cch`'s SQLite log (new `contract_runs` table) — so `cch log --contract=scanning-pipeline` gives you a history.

The single biggest cch feature that would shift a user to Green: a contract language that composes primitives (shell checks, hook results, contract nesting), with persistent history and a native CLI.

Design thought: the contract spec from the curriculum is already well-formed YAML. cch could ship an interpreter nearly verbatim.

### `scheduled-contracts` · (IS Exercise 3b) · **new feature, blocked by 6er**
Once `cch install-service` (ticket `6er`) lands, `cch schedule '@daily' verify` is a natural follow-up. Run as a cch-server-hosted cron, results into SQLite, notifications on failure. The curriculum's Exercise 3b becomes one-line config.

### `self-healing-contracts` · (ORTHOGONAL — Exercise 3c is explicitly aspirational)
The curriculum says don't implement yet. Respect that. File as a distant follow-up once `contract-verify` has baked.

---

## Module 4 — Persistent Computation

Curriculum target: work survives session boundaries.

### `handoff-write` · (IS Exercise 4b) · **new hook**
SessionEnd · writes `.cch-handoffs/YYYY-MM-DDTHH-MM.md` (or `.claude-handoff.yml` per the curriculum's exact filename). Structure per the curriculum:
```yaml
session_date: 2026-04-08
branch: sample-infrastructure
what_we_did: [...]
whats_left: [...]
decisions_made: [...]
gotchas: [...]
```
Content sourced from: `cch log --session=X` (what tools fired, what files were touched), final `git status --porcelain`, tail of the transcript for prose-y "what was done" extraction.

### `handoff-read` · (IS Exercise 4b, other half) · **new hook**
SessionStart · read the latest `.cch-handoffs/*.md` (or `.claude-handoff.yml`), include as `additionalContext`. The pair closes the loop.

The curriculum already observes: *"Claude writes context for its future self in a format that's both machine-parseable and human-reviewable."* That's exactly these two hooks.

### `checkpoint-log` · (SUPPORTS Exercise 4a) · **utility, maybe not a hook**
cch's event-log schema already has `extra` for arbitrary JSON. Add a convention: `hook_name = "checkpoint"`, `extra = {batch_id, status, payload}`. A tiny bb library (`cch.checkpoint`) for hooks and user scripts to read/write these rows. Not a hook itself — it's the Module 4 primitive that hooks and skills both draw on. Makes the "state lives in SQLite, not conversation context" pattern one-liners.

---

## Observability that completes the picture

### `insights` (reuses existing event-log data) · **new CLI**
The curriculum closes with a metrics-to-track table. The user already has a `/insights` skill today. cch could ship `cch insights` that produces the same rollups from `events.db` — wrong-approach counts from `reason LIKE '%wrong%'` patterns, buggy-code counts from test-gate failures, excessive-changes from scope-lock ask decisions.

This makes curriculum progress measurable *automatically* — run `cch insights --monthly` and see the trend line, no manual analysis step.

### `cost-tracker` · (ORTHOGONAL) · still worth shipping
Token cost rollups per session. Less curriculum-aligned but universally useful.

---

## The meta-observation

cch's three existing hooks (scope-lock, command-audit, event-log) already cover the curriculum's Module 1 exercise set and Module 4's event-log primitive. The next hooks to ship could be the remaining Module 1 items (`drift-detection`, `test-gate`) and Module 4's handoff pair — because those are the exercises *a user would otherwise have to hand-build*. Shipping them as cch built-ins gives green-belt behaviors to every cch user without doing the pedagogical exercises.

The curriculum's Module 3 (`contract-verify`) is the **biggest leap** — a new primitive that doesn't exist in cch yet, and the one most likely to shift the friction-event metrics.

**But the biggest meta-leverage move is `cch insights`.** If the loop is *session data → friction analysis → curriculum → practice → better metrics*, then making that loop's input side (structured friction-event capture) and analysis side (automated rollups) both cheaper compounds across everything else. Every hook we ship produces more structured events, which produces better `/insights`, which produces better curricula, which produces better practice. Shipping just `drift-detection` is linear value. Shipping `cch insights` is multiplicative.

That shifts the recommendation: alongside any specific hook, prioritize landing `cch insights` so future curricula get written against data that's *more* structured than today's raw session logs.

---

## North-star theme: direction-changes as training data

> "I'd like to make it so that ideally it never happens that someone's PR gets alternative-PR'd or approve-with-major-changes'd."

This is the same object `/insights` is already capturing from session transcripts. Two streams, one model:

| Stream | "Hit the brakes" looks like | Pair captured |
|---|---|---|
| In-session (Claude as author) | Mid-session intervention: *"no, do it this other way"* → revised tool call or re-written file | original tool call → corrected tool call |
| PR review (human collaborator as author) | `request-changes`, `approve-with-major-changes`, or a user-authored **alternative PR** that closes the original | original PR diff → merged final diff |

Both are records of moments the reviewer disagreed with direction. Both produce the supervision pair that matters: *(what was proposed, what it should have been)*. The *author* varies by stream but is uniform in information value — an AI-authored PR that got alternative-PR'd and a Claude-session patch that got mid-course-corrected teach the same thing.

**The generalization:** cch's highest-leverage data product isn't "session logs" or "PR history" — it's a **direction-changes store** that unifies both streams, and everything on top (retrieval, flagging, codification) queries the same table.

Proposed schema sketch:

```sql
CREATE TABLE direction_changes (
  id           INTEGER PRIMARY KEY,
  timestamp    TEXT NOT NULL,
  source       TEXT NOT NULL,   -- 'session' | 'pr'
  author       TEXT,            -- 'claude-code', human collaborator name, etc.
  reviewer     TEXT,            -- who hit the brakes
  context      TEXT,            -- cwd, repo, PR URL, file-path
  original     TEXT,            -- JSON: original tool call OR original PR diff summary
  corrected    TEXT,            -- JSON: what ended up instead
  note         TEXT,            -- natural-language rationale if any
  -- link back to the raw events for drill-down
  session_id   TEXT,
  event_id     INTEGER
);
```

Every hook and feature below reads this one table.

### Four layered features, most-valuable first

### Capture layer · **two ingesters, one table**
Feed the `direction_changes` table from both streams:

**Session-stream ingester** — analyze cch's event-log (already capturing PreToolUse input + subsequent tool calls) for patterns that indicate "I hit the brakes": user re-typing a similar request, un-doing Claude's writes, interrupting a tool call, etc. Much of this is what `/insights` already does against the transcript; cch can do it against its structured event log, which is cleaner signal. Emits `source='session'` rows.

**PR-stream ingester** — `cch gh sync` subcommand (or a scheduled-contract-style cron once `6er` lands) pulls GitHub PR review events for tracked repos: `request-changes` bodies, approve-with-changes merged SHA diffs, alternative-PR-vs-original pairs (detect when a user-authored PR closes a collaborator's PR without merging). Emits `source='pr'` rows.

Both ingesters land in the same table. No judgment at capture time — just recording.

### Utilization layer · **retrieval hook**
SessionStart (or PreToolUse Edit/Write) · before Claude works on a file, query `direction_changes` for corrections that touched the same file, module, or pattern recently — across both sources. Inject as context:

> *"This area has been corrected 3 times in the last month. Recent example: on src/foo.clj, an original PR passed `db-map` into `process-row`; the corrected version used `with-db`. Reviewer note: 'prefer withXXX helpers, don't smear the db into callees.'"*

Claude sees the pattern and applies it. Doesn't matter whether the original author was Claude or a teammate — the lesson is the same.

### Codification layer · **pattern-mining CLI**
`cch corrections suggest-claudemd` · periodically analyze the `direction_changes` table for recurring patterns (same corrected pattern across many files, or similar diff shape repeated). Draft CLAUDE.md additions from observed patterns. User reviews, commits. Knowledge gets baked into the repo's rules, applying to every future session for everyone using cch in that repo.

### Preventative layer · **proactive flagging hook**
PreToolUse Edit/Write · if the file being edited has a history of corrections AND the proposed change looks similar to the original-pattern (not the corrected-pattern), return `ask` with context. *Before* the bad code gets written, not after.

This is the north-star outcome. A collaborator never has to receive a request-changes; Claude never has to produce an intervention-worthy tool call; alternative PRs never have to exist.

### Why this is beautiful against the curriculum

- Module 1 (Hooks as guardrails): proactive flagging is a guardrail whose rule is *your own historical judgment*.
- Module 2 (Skills that think before they act): architecture-first `/implement` that also consulted `direction_changes` is materially better than one that only reads CLAUDE.md — it's learning from your actual correction patterns, not just stated rules.
- Module 3 (Contracts not tasks): mined patterns effectively *are* contracts — "this repo's style is such that we prefer X over Y."
- Module 4 (Persistent computation): corrections are state that explicitly survives session boundaries *and* spans authors (human + AI), which handoff-notes alone don't cover.

### Why this IS the `cch insights` loop

This isn't a sibling of `cch insights` — it's the same engine looking at a richer-structured view of the same data. `/insights` today does pattern recognition on raw session transcripts; this plan gives the underlying store structure so the same pattern recognition gets cheaper, more accurate, and applies to PR data too. Every piece of investment here makes `/insights` better for free.

### Realistic scope

This is a bigger feature than a single hook — probably a 2-3 week project once prioritized. Phase it:

1. **Phase 1:** just the capture layer — `cch gh sync` plus the `direction_changes` table. Prove we can get the data.
2. **Phase 2:** correction-retrieval at SessionStart. Plain text injection.
3. **Phase 3:** pattern-mining CLI.
4. **Phase 4:** proactive flagging — the north-star capability.

Filing the capture layer as a bead would be a reasonable first move; the rest can follow once we see the shape of the data.

---

## Rough shipping-order take

Given the "the curriculum is an artifact of a loop cch can accelerate" reframing:

1. **First:** `protect-files` — the cheapest security win, unrelated to the curriculum loop.
2. **Feedback loop enablers (top priority):** `cch insights` + whatever minimal schema additions it needs on event-log (e.g., tagging friction-event patterns so they're queryable rather than regex-grepped). Makes every subsequent hook's value visible in data.
3. **Curriculum Module 1 gaps:** `drift-detection` (1b), `test-gate` (1c). Each addresses specific friction-event classes `/insights` has been logging.
4. **Module 4 handoff pair:** `handoff-write` + `handoff-read`. Addresses context-recovery friction the curriculum flags.
5. **Module 2 substrate:** `session-primer` + `prompt-expander`. Cheaper prompts, architecture-first context.
6. **Module 3 leap:** `contract-verify` as a first-class cch feature (CLI + hook + SQLite schema). Bigger scope, likely warrants its own plan pass.
7. **Autonomous mode:** `scheduled-contracts` once `6er` (service-install) lands.
8. **Deferred:** `self-healing-contracts` — the curriculum itself says don't rush this one.

---

## Candidates for bead tickets

Ready to work:

1. `drift-detection` hook (Exercise 1b) — P2, ready.
2. `test-gate` hook (Exercise 1c) — P2, ready. First async hook; forces a design pass on async ergonomics.
3. `protect-files` hook — P2, ready. Smallest surface.
4. `session-primer` hook — P2, ready.
5. `prompt-expander` hook — P2, requires a short design doc on templating syntax before coding.
6. `handoff-write` + `handoff-read` pair — P2, best done together.
7. `contract-verify` CLI + hook — P1 if you agree Module 3 is the biggest leverage. Bigger scope than a single hook; probably warrants its own plan.
8. `cch insights` — P3, follows `contract-verify`.
9. `direction_changes` capture layer — P2, experimental; best as a 2-week project once prioritized.
