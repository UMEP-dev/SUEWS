#!/bin/bash
# Conductor archive script for SUEWS workspace
# Resets workspace to clean state matching remote

set -e

# Change to workspace root
cd "$CONDUCTOR_ROOT_PATH"

echo "[archive] Fetching from origin..."
git fetch origin --prune

# Get current branch name (empty if detached HEAD)
branch=$(git branch --show-current)

# Reset to remote tracking branch, fallback to origin/master if not available
if [ -n "$branch" ] && git rev-parse --verify "origin/$branch" >/dev/null 2>&1; then
    echo "[archive] Resetting to origin/$branch"
    git reset --hard "origin/$branch"
else
    echo "[archive] Branch '$branch' not found on origin, falling back to origin/master"
    git reset --hard origin/master
fi

echo "[archive] Done."
