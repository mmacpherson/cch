#!/usr/bin/env bash
# One-cell usage meter for a tmux status line, plus a detail view for a popup.
#
#   cch-usage-glyph.sh [agent]           -> tmux-formatted glyph (default: codex)
#   cch-usage-glyph.sh --detail [agent]  -> plain-text forecast for display-popup
#
# The glyph is a vertical bar (▁▂▃▄▅▆▇█, 12.5% per step) for the median
# projected 7d usage at reset, colored yellow from 80% and red above 100%
# (projected demand past the cap). It is wrapped in a tmux user range so a
# MouseDown1StatusRight binding can open the detail popup:
#
#   set -g status-right '#(cch-usage-glyph.sh codex) ...'
#   bind -T root MouseDown1StatusRight if -F '#{==:#{mouse_status_range},cch-codex}' \
#     "display-popup -w 64 -h 14 -E 'cch-usage-glyph.sh --detail codex; read -rsn1'"
#
# tmux runs #() on every status-interval, so the forecast is cached on disk
# for 60s; a down server renders nothing rather than stalling the bar.
set -euo pipefail

detail=false
if [[ "${1:-}" == "--detail" ]]; then detail=true; shift; fi
agent="${1:-codex}"
url="${CCH_URL:-http://127.0.0.1:8888}/forecast?agent=${agent}"
cache="${XDG_RUNTIME_DIR:-/tmp}/cch-usage-${agent}.json"

fetch() {
    if [[ -f "$cache" ]] && (( $(date +%s) - $(stat -c %Y "$cache") < 60 )) && ! $detail; then
        cat "$cache"
        return
    fi
    local body
    body=$(curl -sf --max-time 1 "$url") || return 1
    printf '%s' "$body" > "$cache.tmp" && mv "$cache.tmp" "$cache"
    printf '%s' "$body"
}

json=$(fetch) || exit 0

if ! $detail; then
    median=$(jq -r '.seven_day.projected_pct // empty' <<<"$json")
    [[ -z "$median" ]] && exit 0
    bars=(▁ ▂ ▃ ▄ ▅ ▆ ▇ █)
    idx=$(awk -v m="$median" 'BEGIN{i=int((m+12.49)/12.5)-1; if(i<0)i=0; if(i>7)i=7; print i}')
    color=$(awk -v m="$median" 'BEGIN{ if (m>100) print "colour9"; else if (m>=80) print "colour3"; else print "colour250" }')
    printf '#[range=user|cch-%s]#[fg=%s]%s#[default]#[norange]' "$agent" "$color" "${bars[$idx]}"
    exit 0
fi

fmt_left() {
    local s=$1
    if (( s <= 0 )); then echo "now"; return; fi
    local d=$((s / 86400)) h=$(((s % 86400) / 3600)) m=$(((s % 3600) / 60))
    if (( d > 0 )); then echo "${d}d ${h}h"; elif (( h > 0 )); then echo "${h}h ${m}m"; else echo "${m}m"; fi
}

echo "${agent} usage forecast"
echo
for w in seven_day five_hour; do
    jq -e ".${w}" >/dev/null <<<"$json" || continue
    label=$([[ $w == seven_day ]] && echo "7d" || echo "5h")
    read -r cur med lo hi pcap left < <(jq -r ".${w} | [.current_pct, .projected_pct, (.band.lo // \"-\"), (.band.hi // \"-\"), (.p_cap // \"-\"), .secs_left] | @tsv" <<<"$json")
    printf '  %s  now %s%%   median at reset %s%%   90%% range %s-%s%%\n' "$label" "$cur" "$(printf '%.0f' "$med")" "$lo" "$hi"
    if [[ "$pcap" != "-" ]]; then
        printf '      chance of hitting the cap: %.0f%%   resets in %s\n' "$(awk -v p="$pcap" 'BEGIN{print p*100}')" "$(fmt_left "$left")"
    else
        printf '      resets in %s\n' "$(fmt_left "$left")"
    fi
done
echo
echo "  (press any key)"
