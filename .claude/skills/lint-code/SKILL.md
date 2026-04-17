---
name: lint-code
description: Check code style against SUEWS conventions. Complements ruff/fprettify.
---

# Lint Code

Check code against project conventions in `.claude/rules/`.

## Workflow

1. Run standard tools: `ruff check`, `fprettify --diff`
2. For changed Python files in `src/supy/`, run `ruff check --select D` to surface
   numpy-style docstring violations (the global `ruff check` already enforces `D`,
   but this scoped call keeps docstring findings separated in the report).
   - File scope: `git diff --name-only origin/master...HEAD -- 'src/supy/*.py' 'src/supy/**/*.py'`
   - Legacy debt is parked in `[lint.per-file-ignores]` in `.ruff.toml`; any finding
     reported here is new debt introduced on this branch.
3. Apply SUEWS-specific checks from `.claude/rules/`
4. Report issues by file and priority

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

=== Docstrings ===
  file.py:L23: Dxxx description (numpy-style rule)

Summary: N files, M issues
```

If a changed file still shows docstring findings after a good-faith fix, check whether
the file is in `[lint.per-file-ignores]` in `.ruff.toml` — if so, either (a) finish
cleaning it and remove its entry from that table, or (b) leave it alone (existing debt).

## References

- `.claude/rules/` - Full conventions (auto-loaded)
- `references/quick-checks.md` - Common issues
