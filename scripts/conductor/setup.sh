#!/bin/bash
# Conductor setup script for SUEWS workspace
# Resets to remote branch, cleans up extra remotes, and sets up Python environment

set -e

echo "[setup] Fetching from origin..."
git fetch origin --prune

# Get current branch name (empty if detached HEAD)
branch=$(git branch --show-current)

# Reset to remote tracking branch, fallback to origin/master if not available
if [ -n "$branch" ] && git rev-parse --verify "origin/$branch" >/dev/null 2>&1; then
    echo "[setup] Resetting to origin/$branch"
    git reset --hard "origin/$branch"
else
    echo "[setup] Branch '$branch' not found on origin, falling back to origin/master"
    git reset --hard origin/master
fi

# Remove any remotes other than origin
echo "[setup] Cleaning up extra remotes..."
for remote in $(git remote | grep -v '^origin$'); do
    echo "[setup] Removing remote: $remote"
    git remote remove "$remote"
done

# Set up Python environment
echo "[setup] Setting up Python virtual environment..."
uv venv --clear

echo "[setup] Activating virtual environment..."
source .venv/bin/activate

echo "[setup] Done."
