# CHANGELOG Management Scripts

This directory contains scripts for maintaining the CHANGELOG.md file.

## Scripts

### `changelog_helper.py`
Enhanced script for restructuring and maintaining CHANGELOG.md with modern features:
- Parses CHANGELOG to structured JSON format
- Removes duplicate entries
- Sorts entries in reverse chronological order
- Groups entries by category within each date
- **NEW**: Generates Table of Contents by year
- **NEW**: Creates annual statistics table showing category breakdown
- **NEW**: Converts PR/issue references to GitHub links
- **NEW**: Uses proper markdown heading levels (years as ##, dates as ###)
- Creates backup before making changes

**Usage:**
```bash
cd /path/to/SUEWS
python3 .claude/scripts/changelog_helper.py
```

The script will:
1. Parse the current CHANGELOG.md
2. Clean any duplicate entries
3. Sort all entries by date (newest first)
4. Generate a Table of Contents with links to each year
5. Create an annual statistics table showing counts by category
6. Convert all #xxx and PR #xxx references to clickable GitHub links
7. Organize entries by year with proper heading hierarchy
8. Group entries by category within each date
9. Create a backup file (CHANGELOG.md.backup)
10. Save the enhanced CHANGELOG.md
11. Generate a debug JSON file (changelog_debug.json) for inspection

**Enhanced Features:**
- **Table of Contents**: Quick navigation to any year
- **Annual Statistics**: At-a-glance view of project activity by year and category
- **GitHub Integration**: All PR and issue references become clickable links
- **Better Structure**: Clear year and date sections for easier reading

## CHANGELOG Categories
- `[feature]`: New user-facing functionality
- `[bugfix]`: Bug fixes (create GitHub issue)
- `[change]`: User-facing behaviour changes
- `[maintenance]`: Codebase maintenance (including Claude Code/dev tooling)
- `[doc]`: Documentation updates

## Best Practices
1. Run `changelog_helper.py` after adding new entries to ensure proper sorting
2. For analyzing git commits and generating new entries, use the `/log-changes` slash command
3. The script automatically creates backups before making changes
4. Clean up temporary files (*.backup, *.json) after verifying changes