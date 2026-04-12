# cch — Claude Code Hooks framework

# Show available commands
help:
    @just --list

# Run clj-kondo linter on all source and test files
lint:
    clj-kondo --lint src test

# Run test suite
test:
    bb test

# Run pre-commit hooks on all files
lint-all:
    pre-commit run --all-files

# Install pre-commit hooks
install-hooks:
    pre-commit install

# Update pre-commit hook versions
update-hooks:
    pre-commit autoupdate

# Check development environment is ready
doctor:
    #!/usr/bin/env bash
    set -euo pipefail
    ok=true
    check() {
        if command -v "$1" &>/dev/null; then
            printf "  ✓ %-14s %s\n" "$1" "$($1 --version 2>&1 | head -1)"
        else
            printf "  ✗ %-14s not found\n" "$1"
            ok=false
        fi
    }
    echo "Dependencies:"
    check bb
    check clj-kondo
    check sqlite3
    check pre-commit
    check git
    echo ""
    echo "Database:"
    db="$HOME/.local/share/cch/events.db"
    if [ -f "$db" ]; then
        count=$(sqlite3 "$db" "SELECT COUNT(*) FROM events;" 2>/dev/null || echo "?")
        printf "  ✓ %-14s %s events\n" "events.db" "$count"
    else
        printf "  · %-14s not initialized (run: bb -cp src:resources -m cli.cch init)\n" "events.db"
    fi
    echo ""
    echo "Config:"
    cfg="$HOME/.config/cch/config.yaml"
    if [ -f "$cfg" ]; then
        printf "  ✓ %-14s %s\n" "global config" "$cfg"
    else
        printf "  · %-14s not found (run: bb -cp src:resources -m cli.cch init)\n" "global config"
    fi
    echo ""
    if $ok; then
        echo "All good."
    else
        echo "Missing dependencies — install them before proceeding."
        exit 1
    fi

# Remove generated files
clean:
    rm -rf .cpcache .nrepl-port
