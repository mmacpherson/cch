#!/usr/bin/env bash
# AGY statusLine adapter: forward the full payload to cch without delaying the
# TUI, then render a compact local status line from the same JSON.

input=$(cat)

if command -v curl >/dev/null 2>&1; then
  (
    printf '%s' "$input" |
      curl -sS -o /dev/null \
        --connect-timeout 0.1 --max-time 0.5 \
        -X POST http://127.0.0.1:8888/context-snapshot \
        -H 'Content-Type: application/json' \
        -H 'X-CCH-Agent: agy' \
        --data-binary @-
  ) >/dev/null 2>&1 &
fi

if ! command -v jq >/dev/null 2>&1; then
  printf 'AGY\n'
  exit 0
fi

model=$(printf '%s' "$input" |
  jq -r '.model.display_name // .model.id // "AGY"')
ctx_pct=$(printf '%s' "$input" |
  jq -r '.context_window.used_percentage // empty')
quota_pct=$(printf '%s' "$input" |
  jq -r '[.quota[]?.remaining_fraction | select(type == "number")]
         | if length > 0 then ((1 - min) * 100) else empty end')
branch=$(printf '%s' "$input" | jq -r '.vcs.branch // empty')
dirty=$(printf '%s' "$input" | jq -r '.vcs.dirty // false')

line="$model"
if [ -n "$ctx_pct" ]; then
  line=$(printf '%s │ Ctx: %.0f%%' "$line" "$ctx_pct")
fi
if [ -n "$quota_pct" ]; then
  line=$(printf '%s │ Quota: %.0f%%' "$line" "$quota_pct")
fi
if [ -n "$branch" ]; then
  if [ "$dirty" = "true" ]; then
    branch="${branch}*"
  fi
  line="${line} │ ${branch}"
fi

printf '%s\n' "$line"
