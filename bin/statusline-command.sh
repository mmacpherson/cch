#!/usr/bin/env bash
# Claude Code statusLine command
# Outputs: Model │ Ctx: N% │ 5h: NOW% · ~PROJ% · LEFT │ 7d: NOW% · ~PROJ% · LEFT │ branch[*] │ style
# Each window shows: current observed pct · estimated end-of-window pct · time-to-reset.
# The ~PROJ% is rendered italic with a tilde to mark it as an estimate; coloring
# (default→yellow→orange→red) reflects the projected danger level.

input=$(cat)

# --- Feed full status payload to cch server (fire-and-forget) ---
echo "$input" | curl -s -o /dev/null --max-time 2 \
  -X POST http://127.0.0.1:8888/context-snapshot \
  -H 'Content-Type: application/json' -d @- &

# --- Model ---
model=$(echo "$input" | jq -r '.model.display_name // empty')

# --- Context window ---
ctx_pct=$(echo "$input" | jq -r '.context_window.used_percentage // empty')
ctx_part=""
if [ -n "$ctx_pct" ]; then
    ctx_part=$(printf "Ctx: %.0f%%" "$ctx_pct")
fi

# --- Helpers ---

fmt_duration_short() {
    local secs="$1"
    if [ "$secs" -le 0 ]; then echo "now"; return; fi
    local days=$(( secs / 86400 ))
    local hours=$(( (secs % 86400) / 3600 ))
    local mins=$(( (secs % 3600) / 60 ))
    if   [ "$days"  -ge 1 ] && [ "$hours" -ge 1 ]; then echo "${days}d${hours}h"
    elif [ "$days"  -ge 1 ];                       then echo "${days}d"
    elif [ "$hours" -ge 1 ];                       then echo "${hours}h${mins}m"
    else                                                echo "${mins}m"
    fi
}

# ANSI color/style codes — italic for "this is an estimate"
reset=$'\033[0m'
italic=$'\033[3m'
bold=$'\033[1m'
blink=$'\033[5m'
yellow=$'\033[33m'
orange=$'\033[38;5;208m'
red=$'\033[1;31m'

# Pick a color for a projected pct value: default below 80, then yellow, orange, red.
color_for_proj() {
    local pct="$1"
    if   awk -v x="$pct" 'BEGIN{exit !(x>120)}'; then printf '%s' "$red"
    elif awk -v x="$pct" 'BEGIN{exit !(x>100)}'; then printf '%s' "$orange"
    elif awk -v x="$pct" 'BEGIN{exit !(x>=80)}'; then printf '%s' "$yellow"
    fi
}

# --- /ewma: Bayesian projection for both 5h and 7d windows ---
# Tight timeout — if the cch dev server is down, we silently fall back to
# whatever we can pull from the payload itself.
ewma_json=$(curl -sf --max-time 1.0 http://127.0.0.1:8888/forecast 2>/dev/null)

# Build the per-window string. Falls back to payload-only if the projection
# isn't available (e.g. fresh DB, or server momentarily unreachable).
build_window_part() {
    local label="$1" payload_pct="$2" payload_resets="$3" json_path="$4"
    local now_pct proj_pct secs_left time_left col

    # Always use the live payload for the observed pct — the forecast server
    # has a debounce/cache delay and will be one snapshot behind.
    now_pct="$payload_pct"

    # Use the forecast server only for the projection and time-to-reset.
    if [ -n "$ewma_json" ]; then
        proj_pct=$(echo "$ewma_json" | jq -r "${json_path}.projected_pct // empty" 2>/dev/null)
        secs_left=$(echo "$ewma_json" | jq -r "${json_path}.secs_left // empty"    2>/dev/null)
    fi
    if [ -z "$secs_left" ] && [ -n "$payload_resets" ]; then
        secs_left=$(( payload_resets - $(date +%s) ))
    fi

    # Nothing to print if we don't even have an observed pct.
    [ -z "$now_pct" ] && return

    time_left=""
    [ -n "$secs_left" ] && time_left=$(fmt_duration_short "$secs_left")

    local proj_field=""
    if [ -n "$proj_pct" ]; then
        col=$(color_for_proj "$proj_pct")
        proj_field=$(printf "%s%s~%.0f%%%s" "$col" "$italic" "$proj_pct" "$reset")
    fi

    # Compose: "5h: NN% · ~PP% · TTL" — drop missing parts gracefully.
    local out
    out=$(printf "%s: %.0f%%" "$label" "$now_pct")
    [ -n "$proj_field" ] && out="${out} · ${proj_field}"
    [ -n "$time_left"  ] && out="${out} · ${time_left}"
    printf '%s' "$out"
}

five_payload_pct=$(echo "$input"  | jq -r '.rate_limits.five_hour.used_percentage // empty')
five_payload_res=$(echo "$input"  | jq -r '.rate_limits.five_hour.resets_at // empty')
seven_payload_pct=$(echo "$input" | jq -r '.rate_limits.seven_day.used_percentage // empty')
seven_payload_res=$(echo "$input" | jq -r '.rate_limits.seven_day.resets_at // empty')

five_part=$(build_window_part  "5h" "$five_payload_pct"  "$five_payload_res"  ".five_hour")
seven_part=$(build_window_part "7d" "$seven_payload_pct" "$seven_payload_res" ".seven_day")

# --- Git branch + dirty flag ---
cwd=$(echo "$input" | jq -r '.cwd // empty')
git_part=""
if [ -n "$cwd" ]; then
    branch=$(git -C "$cwd" branch --show-current 2>/dev/null)
    if [ -n "$branch" ]; then
        dirty=$(git -C "$cwd" status --porcelain 2>/dev/null)
        if [ -n "$dirty" ]; then git_part="${branch}*"; else git_part="$branch"; fi
    fi
fi

# --- Output style ---
style_name=$(echo "$input" | jq -r '.output_style.name // empty')
style_part=""
if [ -n "$style_name" ] && [ "$style_name" != "default" ]; then
    style_part="$style_name"
fi

# --- 7d burn rate: ⚡N%/h between ctx and 5h ---
# Absolute %/hr — not relative to remaining quota, so no shrinking denominator.
# Thresholds from observed distribution: normal=5-6, heavy=10-12, gap at 8-10.
burn_part=""
if [ -n "$ewma_json" ]; then
    rate_phr=$(echo "$ewma_json" | jq -r '.seven_day.local_rate_phr // empty' 2>/dev/null)
    if [ -n "$rate_phr" ]; then
        if   awk -v x="$rate_phr" 'BEGIN{exit !(x>=50.0)}'; then
            col="${bold}${blink}${red}"
        elif awk -v x="$rate_phr" 'BEGIN{exit !(x>=20.0)}'; then
            col="${red}"
        elif awk -v x="$rate_phr" 'BEGIN{exit !(x>=12.0)}'; then
            col="${orange}"
        elif awk -v x="$rate_phr" 'BEGIN{exit !(x>=8.0)}'; then
            col="${yellow}"
        else
            col=""
        fi
        burn_part=$(printf "%s%.1f%%/h%s" "$col" "$rate_phr" "$reset")
    fi
fi

# --- Assemble line ---
sep=" │ "
line=""
append() {
    local field="$1"
    [ -z "$field" ] && return
    if [ -z "$line" ]; then line="$field"; else line="${line}${sep}${field}"; fi
}

append "$model"
append "$ctx_part"
append "$burn_part"
append "$five_part"
append "$seven_part"
append "$git_part"
append "$style_part"

echo "$line"
