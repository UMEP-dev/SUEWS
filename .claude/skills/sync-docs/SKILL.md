---
name: sync-docs
description: Check doc-code content consistency. For RST/Markdown formatting, use lint-code.
---

# Sync Docs

Check documentation **content** matches codebase (values, signatures, parameters).

## Workflow

1. **Analyse** recent code changes (functions, signatures, config, deprecated features)
2. **Identify** documentation locations (docstrings, docs/, README, config docs)
3. **Detect** sync issues (missing docs, outdated params, wrong examples)
4. **Classify** by priority: Critical > High > Medium > Low

## Areas to Check

| Area | Code Location | Doc Location | Priority |
|------|---------------|--------------|----------|
| Scientific | `src/suews/src/suews_phys_*.f95` | `docs/source/parameterisations-and-sub-models.rst` | Critical |
| API | `src/supy/*.py` | `docs/source/api/` | Critical |
| Config | `src/supy/data_model/` | `docs/source/inputs/yaml/` | High |
| Output vars | `src/supy/data_model/output/` | `docs/source/output_files/` | High |
| Build | `Makefile` | `README.md`, installation docs | Medium |

Details: `references/areas-to-check.md`

## Output Format

```
[sync-docs] Documentation-Code Consistency Report

=== [Area] ===
  file.rst:
    L45: Issue description

Summary: N issues (X critical, Y high, Z medium)
```

## Priority Levels

1. **Critical**: Scientific mismatches, API changes, missing output variables
2. **High**: Config defaults, missing physics methods
3. **Medium**: Missing feature docs, unit notation
4. **Low**: Description wording, formatting

## References

- `references/areas-to-check.md` - Detailed check areas with examples
- `references/data-model-checks.md` - Pydantic fields and output variables
- `references/verification-commands.md` - Bash/Python verification scripts
