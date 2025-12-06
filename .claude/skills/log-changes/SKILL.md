---
name: log-changes
description: Analyse recent code changes and update documentation/CHANGELOG as needed. Use when preparing releases, documenting recent work, or ensuring CHANGELOG is up to date. Categorises changes and follows SUEWS CHANGELOG conventions.
---

# Log Changes

Analyse code changes and fill the gap in CHANGELOG.md from the last documented date to today.

## Workflow

### 1. Identify the Gap

- Read CHANGELOG.md to find the last documented date
- Find all commits from the day after that date until today
- Use actual commit dates, not today's date for historical changes

### 2. Group Commits by Date

- Use `git log --format="%ad||%h||%s||%an" --date=format:"%d %b %Y"` to get commits with dates
- Check if commits are part of merged PRs: `git log --merges --format="%h %s"`
- Group commits that happened on the same day
- Only include dates where significant changes occurred

### 3. Categorise Each Significant Change

- **[feature]**: New functionality added
- **[bugfix]**: Bug fixes (note if GitHub issue should be created)
- **[change]**: User-facing changes
- **[maintenance]**: Codebase maintenance (including Claude Code/dev tooling AND CLAUDE.md updates)
- **[doc]**: Documentation updates (user-facing documentation in docs/, NOT CLAUDE.md)

### 4. Update CHANGELOG.md with Links

Each change item must include a reference that GitHub will auto-link:
- For PR-related changes: `#123` - GitHub auto-links to PR
- For direct commits: Use 7-character SHA like `abc1234` - GitHub auto-links to commit

Format examples:
- `[feature]: Add new validation system (#123)`
- `[bugfix]: Fix memory leak in parser (abc1234)`
- `[maintenance]: Update CLAUDE.md with new guidelines (#456, def5678)`

Guidelines:
- Use the actual commit date, not today's date
- Only use today's date if you're documenting changes made today
- Maintain chronological order (newest dates first)
- Group related changes from the same day together
- Follow British English spelling

### 5. Check Documentation Needs

- If data model changed: Run `python docs/generate_datamodel_rst.py`
- If config schema changed: Run `python docs/gen_schema.py`
- Update relevant docs/ files for API changes
- Update examples/tutorials if needed

### 6. Show a Summary

- Date range covered (from last documented to today)
- Number of commits analysed
- Dates added to CHANGELOG with change counts
- Documentation updates performed

## Remember

- Skip minor changes (formatting, typos, small refactors)
- Use actual commit dates to maintain accurate history
- Group commits from the same day into coherent entries
- Only add dates where meaningful changes occurred
- Preserve the existing CHANGELOG format exactly

## Scripts

The `scripts/changelog_helper.py` script can restructure a CHANGELOG file:
- Parses CHANGELOG.md to structured JSON
- Cleans duplicates and merges entries
- Sorts properly by date
- Formats back to clean markdown with TOC and statistics

**Warning**: Only run this script when explicitly requested - it can cause merge conflicts if run frequently.

## Useful Git Commands

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
