---
name: log-changes-skill
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

| Category | Use for |
|----------|---------|
| `[feature]` | New functionality |
| `[bugfix]` | Bug fixes |
| `[change]` | User-facing changes |
| `[maintenance]` | Dev tooling, CLAUDE.md |
| `[doc]` | User docs (not CLAUDE.md) |

## Key Rules

- Use **actual commit dates**, not today's date
- Include refs: `(#123)` for PRs, `(abc1234)` for commits
- Skip minor changes (formatting, typos)
- British English

Details: `references/workflow-details.md`

## References

- `references/workflow-details.md` - Full workflow
- `references/git-commands.md` - Useful commands
- `.claude/rules/changelog/format.md` - Format conventions
