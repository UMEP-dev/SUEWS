#!/bin/bash
# Shared utilities for Conductor scripts

# Fetch from origin with automatic repair on failure.
# Usage: fetch_origin "[tag]"
fetch_origin() {
    local tag="${1:-[git]}"
    if git fetch origin --prune; then
        return 0
    fi

    echo "$tag Fetch failed; repairing remote refs and retrying..."
    git pack-refs --all
    git fetch origin --prune
}
