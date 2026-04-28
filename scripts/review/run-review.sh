#!/usr/bin/env bash
# Run all cch review queries and emit a single JSON object.
# Usage: run-review.sh [WINDOW]   (default: -7 days)
#   WINDOW is a SQLite interval string, e.g. "-14 days"
#
# Output: JSON object with one key per query file, value is the query result.
# Pipe to jq for pretty-print, or pass raw to a Claude session.

set -euo pipefail

DB="${CCH_DB:-$HOME/.local/share/cch/events.db}"
WINDOW="${1:--7 days}"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

if [ ! -f "$DB" ]; then
  echo '{"error": "events DB not found: '"$DB"'"}' >&2
  exit 1
fi

run_query() {
  local file="$1"
  # Substitute :window literal before passing to sqlite3 (CLI has no named params).
  sed "s/:window/'${WINDOW}'/g" "$file" \
    | sqlite3 "$DB" 2>/dev/null \
    | head -1
}

# Collect results into a JSON object keyed by query name (filename sans number/extension).
result="{"
first=1
for sql_file in "$SCRIPT_DIR"/[0-9]*.sql; do
  key=$(basename "$sql_file" .sql | sed 's/^[0-9]*-//')
  value=$(run_query "$sql_file")
  [ -z "$value" ] && value="null"
  [ "$first" -eq 0 ] && result="${result},"
  result="${result}\"${key}\":${value}"
  first=0
done
result="${result}}"

echo "$result"
