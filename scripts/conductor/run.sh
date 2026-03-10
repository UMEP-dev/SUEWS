#!/bin/bash
# Conductor run script for SUEWS workspace
# Starts browser-sync for site preview and Sphinx live documentation

set -e

echo "[run] Activating virtual environment..."
source .venv/bin/activate

echo "[run] Starting browser-sync for site preview..."
npx -y browser-sync start --server site --port 3000 --files 'site/**/*' &

echo "[run] Starting Sphinx live documentation..."
cd docs && make livehtml
