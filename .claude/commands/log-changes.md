---
description: Analyse recent code changes and update documentation/CHANGELOG as needed
allowed-tools: Bash(git log:*), Bash(git diff:*), Bash(git status:*), Read, Write, Edit, Glob, Grep
---

## Context
- Today's date: !`date +"%d %b %Y"`
- Last documented date in CHANGELOG: !`grep -E "^- [0-9]+ [A-Za-z]+ [0-9]+:" CHANGELOG.md | head -1 | sed 's/^- //' | sed 's/:$//'`
- Recent commits with dates: !`git log --format="%ad %h %s" --date=format:"%d %b %Y" -30`

## Your Task

Analyse code changes and fill the gap in CHANGELOG.md from the last documented date to today:

1. **Identify the gap**:
   - Read CHANGELOG.md to find the last documented date
   - Find all commits from the day after that date until today
   - Use actual commit dates, not today's date for historical changes

2. **Group commits by date**:
   - Use `git log --format="%ad||%h||%s||%an" --date=format:"%d %b %Y"` to get commits with dates
   - Check if commits are part of merged PRs: `git log --merges --format="%h %s"`
   - Group commits that happened on the same day
   - Only include dates where significant changes occurred

3. **Categorise each significant change**:
   - [feature]: New functionality added
   - [bugfix]: Bug fixes (note if GitHub issue should be created)
   - [change]: User-facing changes
   - [maintenance]: Codebase maintenance (including Claude Code/dev tooling AND CLAUDE.md updates)
   - [doc]: Documentation updates (user-facing documentation in docs/, NOT CLAUDE.md)

4. **Update CHANGELOG.md with links (GitHub auto-linking)**:
   - Add entries for each date with significant changes
   - **IMPORTANT**: Each change item must include a reference that GitHub will auto-link:
     - For PR-related changes: `#123` - GitHub auto-links to PR
     - For direct commits: Use 7-character SHA like `abc1234` - GitHub auto-links to commit
   - Format examples:
     - `[feature]: Add new validation system (#123)`
     - `[bugfix]: Fix memory leak in parser (abc1234)`
     - `[maintenance]: Update CLAUDE.md with new guidelines (#456, def5678)`
   - Use the actual commit date, not today's date
   - Only use today's date if you're documenting changes made today
   - Maintain chronological order (newest dates first)
   - Group related changes from the same day together
   - Follow British English spelling

5. **Check documentation needs**:
   - If data model changed: Run `python docs/generate_datamodel_rst.py`
   - If config schema changed: Run `python docs/gen_schema.py`
   - Update relevant docs/ files for API changes
   - Update examples/tutorials if needed

6. **Show a summary**:
   - Date range covered (from last documented to today)
   - Number of commits analysed
   - Dates added to CHANGELOG with change counts
   - Documentation updates performed

Remember to:
- Skip minor changes (formatting, typos, small refactors)
- Use actual commit dates to maintain accurate history
- Group commits from the same day into coherent entries
- Only add dates where meaningful changes occurred
- Preserve the existing CHANGELOG format exactly