# Review CCH Hook Activity

Analyse CCH hook friction from the past week (or the period in `$ARGUMENTS`, e.g. `14d`) and produce a findings report with proposed resolutions. **Do not make code changes** — findings are for a follow-up session.

## Step 1 — Establish the review window

Default to 7 days unless `$ARGUMENTS` specifies otherwise (e.g. `14d` → `-14 days`). Map to a SQLite interval string for the runner.

## Step 2 — Run the queries

The queries live in `scripts/review/`. Run the whole suite at once:

```bash
bash scripts/review/run-review.sh "-7 days"
```

This emits a single JSON object with one key per query:
- `decision-summary` — counts by hook × decision
- `block-deny-patterns` — top deny/block reasons
- `scope-lock-asks` — external paths generating ask prompts
- `command-guard-commands` — actual commands that were blocked (from event payload)
- `latency` — avg/max elapsed_ms per hook; `slow: 1` if avg > 20ms
- `friction-by-repo` — deny/ask counts broken down by cwd

Individual queries can be run directly for spot-checks:
```bash
bash scripts/review/run-review.sh "-7 days" | jq '.["scope-lock-asks"]'
sqlite3 ~/.local/share/cch/events.db < scripts/review/03-scope-lock-asks.sql  # after manual :window substitution
```

## Step 3 — Analyse the results

Work through each section:

**False positives** (`block-deny-patterns` + `command-guard-commands`)
Look at the actual command text. Does the deny reason match what the command actually does? A mismatch means the regex is over-broad. Note the pattern, the count, and what a tighter rule would look like.

**Scope-lock ask fatigue** (`scope-lock-asks`)
Paths appearing >5× are candidates for `global-allowed-paths` in `~/.config/cch/config.yaml` (personal mono repo — not this public repo). Group by path prefix; siblings under `~/.claude/` can be covered by a single entry.

**Missing guard patterns** (`command-guard-commands`, cross-reference with `decision-summary`)
Are there commands that got through which look risky in hindsight? Describe the pattern and a proposed regex.

**Latency** (`latency`)
Any hook with `slow: 1` (avg > 20ms). The budget is <50ms for the full chain. Note which hook and suggest where to look (usually a SQLite query or a filesystem walk in the hot path).

**Concentration** (`friction-by-repo`)
If >80% of friction comes from one repo, a per-repo `.cch-config.yaml` may be a better fix than a global change.

## Step 4 — Produce the findings report

Only include sections where there is an actual finding.

---

### CCH Hook Review — [date range]

#### False positives
| Hook | Pattern | Count | Root cause | Proposed fix |
|------|---------|-------|------------|--------------|

#### Scope-lock ask fatigue
- **`<path>`** (N asks) — add to `global-allowed-paths` in personal config

#### New guard patterns worth adding
- **Pattern observed**: …  
- **Proposed regex**: …

#### Latency concerns
- **`<hook>`**: avg Nms — investigate …

#### Low-signal / working well
…

---

## Notes for follow-up

- **Hook logic / regex changes**: open a session in `~/projects/claude-code-hooks`
- **Personal config** (`global-allowed-paths`, per-repo `.cch-config.yaml`): edit in `~/projects/private-monorepo` — these must **not** be committed to this public repo
