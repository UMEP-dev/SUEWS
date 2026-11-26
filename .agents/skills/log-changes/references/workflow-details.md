# Log Changes Workflow Details

## Step 1: Identify the Gap

- Read CHANGELOG.md to find the last documented date
- Find all commits from the day after that date until today
- Use actual commit dates, not today's date for historical changes

## Step 2: Group Commits by Date

- Use `git log --format="%ad||%h||%s||%an" --date=format:"%d %b %Y"`
- Check if commits are part of merged PRs: `git log --merges --format="%h %s"`
- Group commits that happened on the same day
- Only include dates where significant changes occurred

## Step 3: Categorise Changes and Governance Status Tags

Categories:
- **[feature]** - New functionality added
- **[bugfix]** - Bug fixes (note if GitHub issue should be created)
- **[change]** - User-facing or breaking changes
- **[maintenance]** - Codebase maintenance (including agent/dev tooling and `AGENTS.md` / `.claude/` updates)
- **[doc]** - Documentation updates (user-facing documentation in `docs/`, NOT `AGENTS.md`)

Governance status tags (for `[feature]` and `[change]`, or `[maintenance][internal]` when explicitly internal; never add a colon after tags):
- **`[experimental]`** - Default for new features and changes under active development; excluded from public release notes
- **`[stable]`** - Governance-approved for public release announcements
- **`[internal]`** - Internal tooling/infrastructure; never announced publicly

## Step 4: Update CHANGELOG.md with Links

Format examples (no colons after `[category]` or `[status]` tags):
- `- [feature][experimental] Add new validation system (#123)`
- `- [change][stable] Deprecate legacy Parquet writer (#456)`
- `- [bugfix] Fix memory leak in parser (abc1234)`
- `- [maintenance][internal] Update AGENTS.md with new guidelines (#789, def5678)`

Guidelines:
- Do NOT modify the `## Annual Statistics` table
- Use the actual commit date, not today's date
- Maintain chronological order (newest dates first)
- Group related changes from the same day
- Follow British English spelling and plain ASCII

## Step 5: Check Documentation Needs

- If data model changed: Run `python docs/generate_datamodel_rst.py`
- If config schema changed: Run `suews schema export -o docs/source/inputs/tables/schema.json`
- Update relevant `docs/` files for API changes

## Step 6: Show Summary

- Date range covered
- Number of commits analysed
- Dates added to CHANGELOG with change counts

## Scripts

The `scripts/changelog_helper.py` script can restructure CHANGELOG:
- Parses to structured JSON
- Cleans duplicates and merges entries
- Sorts by date
- Formats back to markdown

**Warning**: Only run when explicitly requested - can cause merge conflicts.
