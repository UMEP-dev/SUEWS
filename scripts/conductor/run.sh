#!/bin/bash
# Conductor run script for SUEWS workspace.
# Starts the site preview (browser-sync, :3000) and live documentation
# (sphinx-autobuild, :8000) in the background, then serves the workspace
# Dashboard (:4000) in the foreground so Conductor surfaces that URL.
# Self-heals if .venv, supy, or docs deps are missing.

set -e
# Enable job control so background jobs land in their own process groups;
# the cleanup trap below uses this to kill whole trees.
set -m

if [ ! -d .venv ]; then
    echo "[run] No .venv found - creating one with uv..."
    uv venv
fi

echo "[run] Activating virtual environment..."
source .venv/bin/activate

if ! python -c "import supy" >/dev/null 2>&1; then
    echo "[run] supy not importable - running 'make dev' (first build takes a few minutes)..."
    make dev
fi

if ! command -v sphinx-autobuild >/dev/null 2>&1; then
    echo "[run] sphinx-autobuild missing - running 'make docs-setup'..."
    make docs-setup
fi

# Pick a free port at or above $1 so parallel Conductor workspaces do not
# collide on the same fixed ports.
find_free_port() {
    local port=$1
    while lsof -iTCP:"$port" -sTCP:LISTEN -P >/dev/null 2>&1; do
        port=$((port + 1))
    done
    echo "$port"
}

PORT_SITE=$(find_free_port 3000)
PORT_DOCS=$(find_free_port 8000)
PORT_DASH=$(find_free_port 4000)
echo "[run] Ports: site=$PORT_SITE docs=$PORT_DOCS dashboard=$PORT_DASH"

# Track background PIDs so the trap can kill each whole process group.
bg_pids=()
cleanup() {
    for pid in "${bg_pids[@]}"; do
        kill -TERM -- -"$pid" 2>/dev/null || kill -TERM "$pid" 2>/dev/null || true
    done
}
trap cleanup EXIT INT TERM

# Start the dashboard FIRST so its port is the first one Conductor sees and
# wires into the "Open" button, rather than the site preview or live docs.
echo "[run] Starting workspace dashboard (port $PORT_DASH)..."
python scripts/conductor/dashboard.py --port "$PORT_DASH" \
    --site-url "http://localhost:$PORT_SITE" \
    --docs-url "http://localhost:$PORT_DOCS" &
bg_pids+=($!)

# Brief pause so the dashboard binds before the other dev servers open their
# ports — Conductor latches onto the earliest port it detects.
sleep 1

echo "[run] Starting browser-sync for site preview (port $PORT_SITE)..."
npx -y browser-sync start \
    --server site \
    --port "$PORT_SITE" \
    --files 'site/**/*' \
    --no-open \
    --no-ui &
bg_pids+=($!)

echo "[run] Starting Sphinx live documentation (port $PORT_DOCS)..."
(cd docs && make livehtml LIVEHTML_OPEN_BROWSER="" LIVEHTML_PORT="$PORT_DOCS") &
bg_pids+=($!)

# Wait for any child to exit (or for a trapped signal).
# Running `wait` instead of foregrounding python lets the INT/TERM trap
# fire immediately when Conductor stops the run.
wait
