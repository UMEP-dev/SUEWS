#!/bin/bash
# Conductor setup script for SUEWS workspace
# Resets to remote branch, cleans up extra remotes, and sets up Python environment

set -e

echo "[setup] Fetching from origin..."
git fetch origin --prune

# Get current branch name (empty if detached HEAD)
branch=$(git branch --show-current)

# Reset to remote branch:
#   1. origin/$branch if it exists (branch already pushed — sync with remote)
#   2. @{upstream} if set (new branch tracking e.g. origin/rtd)
#   3. Skip reset (fresh worktree, already at the right commit)
if [ -n "$branch" ] && git rev-parse --verify "origin/$branch" >/dev/null 2>&1; then
    echo "[setup] Resetting to origin/$branch"
    git reset --hard "origin/$branch"
else
    upstream=$(git rev-parse --abbrev-ref --symbolic-full-name @{upstream} 2>/dev/null || true)
    if [ -n "$upstream" ]; then
        echo "[setup] Branch '$branch' not on origin, resetting to upstream: $upstream"
        git reset --hard "$upstream"
    else
        echo "[setup] Branch '$branch' has no remote or upstream — skipping reset"
    fi
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
