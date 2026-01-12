---
name: audit-pr-skill
description: Review SUEWS PRs. Drafts comments for approval before posting.
---

# Audit PR

Comprehensive PR review with GitHub integration.

## Quick Start

```bash
gh pr view {pr} --json number,title,files,commits
gh pr diff {pr} --name-only
```

## Workflow

| Step | Action | Skill/Reference |
|------|--------|-----------------|
| 1 | **Context** | Gather files, classify changes |
| 2 | **Code Style** | lint-code-skill |
| 3 | **Scientific** | Physics validation (if applicable) |
| 4 | **Testing** | Coverage, FIRST principles |
| 5 | **Docs** | CHANGELOG, PR description |
| 6 | **Build** | CI status, meson.build |
| 7 | **Draft** | Comments for approval |
| 8 | **Approval** | Wait for human confirmation |
| 9 | **Post** | Only after approval |

Details: `references/workflow-steps.md`

## Change Classification

| Type | Pattern | Review |
|------|---------|--------|
| Physics | `suews_phys_*.f95` | Scientific + code |
| Utility | `suews_util_*.f95` | Code only |
| Control | `suews_ctrl_*.f95` | Code only |
| Python | `src/supy/*.py` | Code only |

## Code Style Quick Reference

**Fortran**: File naming, module naming, IMPLICIT NONE, precision, units
**Python**: Variable prefixes, logging, pathlib, type hints, docstrings

Details: `references/style-checks.md`

## Draft Output Format

```
=== DRAFT LINE COMMENTS ===
1. file.f95:42
   > Issue description

=== DRAFT SUMMARY ===
**Status**: Needs attention / Ready

| Category | Status |
|----------|--------|
| Code Style | PASS/FAIL |
| Scientific | PASS/FAIL/N/A |
| Testing | PASS/FAIL |
| Docs | PASS/FAIL |

### Key Findings
- [issues list]

### Suggested Reviewers
@reviewer (module:name)
```

## Approval Options

`approve` | `approve with edits` | `skip line comments` | `cancel`

## References

- `references/review-checklist.md` - Detailed checklist
- `references/workflow-steps.md` - Full workflow
- `references/style-checks.md` - Style rules
- `dev-ref/CODING_GUIDELINES.md`
- `dev-ref/REVIEW_PROCESS.md`
