---
name: lint-code
description: Check code style against SUEWS conventions. Complements ruff/fprettify.
---

# Lint Code

Check code against project conventions in `.claude/rules/`.

## Workflow

1. Run standard tools: `ruff check`, `fprettify --diff`
2. Apply SUEWS-specific checks from `.claude/rules/`
3. Report issues by file and priority

## File Types

| Type | Rules |
|------|-------|
| Fortran | `.claude/rules/fortran/conventions.md` |
| Python | `.claude/rules/python/conventions.md` |
| RST | `.claude/rules/docs/conventions.md` |
| CHANGELOG | `.claude/rules/changelog/format.md` |

## Output Format

```
[code-style] Analysis

=== Fortran ===
  file.f95:L12: issue description

=== Python ===
  file.py:L23: issue description

Summary: N files, M issues
```

## References

- `.claude/rules/` - Full conventions (auto-loaded)
- `references/quick-checks.md` - Common issues
