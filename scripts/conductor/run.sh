#!/bin/bash
# Conductor run script for SUEWS workspace
# Starts browser-sync for site preview and Sphinx live documentation.
# Self-heals if .venv or supy build is missing so clicking Run just works.

set -e

if [ ! -d .venv ]; then
    echo "[run] No .venv found — creating one with uv..."
    uv venv
fi

echo "[run] Activating virtual environment..."
source .venv/bin/activate

if ! python -c "import supy" >/dev/null 2>&1; then
    echo "[run] supy not importable — running 'make dev' (first build takes a few minutes)..."
    make dev
fi

if ! command -v sphinx-autobuild >/dev/null 2>&1; then
    echo "[run] sphinx-autobuild missing — running 'make docs-setup'..."
    make docs-setup
fi

# Make sure background jobs (browser-sync) die with this script
trap 'kill $(jobs -p) 2>/dev/null || true' EXIT

echo "[run] Starting browser-sync for site preview..."
npx -y browser-sync start --server site --port 3000 --files 'site/**/*' &

echo "[run] Starting Sphinx live documentation..."
cd docs && make livehtml
