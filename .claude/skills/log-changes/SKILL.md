---
name: log-changes
description: Update CHANGELOG.md with recent commits. Follows SUEWS conventions.
---

# Log Changes

Fill CHANGELOG.md gap from last documented date to today.

## Workflow

1. **Identify gap**: Last CHANGELOG date → today
2. **Get commits**: `git log --format="%ad||%h||%s" --date=format:"%d %b %Y"`
3. **Categorise**: `[feature]`, `[bugfix]`, `[change]`, `[maintenance]`, `[doc]`
4. **Update**: Add entries with refs (`#123` or `abc1234`)
5. **Check docs**: Run generators if data model changed
6. **Summary**: Date range, commit count, changes added

## Categories

- `[feature]` - New functionality
- `[bugfix]` - Bug fixes
- `[change]` - User-facing changes
- `[maintenance]` - Dev tooling, CLAUDE.md
- `[doc]` - User docs (not CLAUDE.md)

## Status Tags (Governance)

For `[feature]` and `[change]` entries, add a status tag:

- `[experimental]` - Default for new features, **excluded from public release notes**
- `[stable]` - Governance-approved, **included in public release notes**
- `[internal]` - Internal tooling, **excluded from public release notes**

**Default behaviour**: New `[feature]` and `[change]` entries get `[experimental]` tag unless explicitly marked otherwise.

**Release impact**: When `prep-release` builds version history RST and GitHub Release notes, only `[stable]` and untagged features/changes are included. `[experimental]` items remain in the CHANGELOG only. `[bugfix]`, `[maintenance]`, and `[doc]` entries are always included regardless of status tags.

## Key Rules

- Use **actual commit dates**, not today's date
- Include refs: `(#123)` for PRs, `(abc1234)` for commits
- Skip minor changes (formatting, typos)
- British English
- **New features default to `[experimental]`** — only governance-approved features get `[stable]`

Details: `references/workflow-details.md`

## References

- `references/workflow-details.md` - Full workflow
- `references/git-commands.md` - Useful commands
- `.claude/rules/changelog/format.md` - Format conventions
