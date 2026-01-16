# Useful Git Commands

```bash
# Get recent commits with dates
git log --format="%ad||%h||%s||%an" --date=format:"%d %b %Y" -30

# Find merged PRs
git log --merges --format="%h %s"

# Get diff stats between dates
git log --since="2025-01-01" --until="2025-01-31" --oneline

# Show files changed in recent commits
git log --name-only -10
```
