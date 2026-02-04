---
name: audit-pr
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

- **Step 1 - Context**: Gather files, classify changes
- **Step 2 - Code Style**: lint-code
- **Step 3 - Scientific**: Physics validation (if applicable)
- **Step 4 - Testing**: Coverage, FIRST principles
- **Step 5 - Docs**: CHANGELOG, PR description
- **Step 6 - Governance**: Check feature status tags (see below)
- **Step 7 - Build**: CI status, meson.build
- **Step 8 - Draft**: Comments for approval
- **Step 9 - Approval**: Wait for human confirmation
- **Step 10 - Post**: Only after approval

## Governance Check (Step 6)

For PRs that add new features or breaking changes:

- Verify CHANGELOG entries have appropriate status tags
- New `[feature]` entries should have `[experimental]` tag by default
- Flag if `[stable]` tag is used without governance approval
- Check PR description mentions if feature is experimental

**Questions to ask**:
- Is this feature ready for public announcement?
- Does it have associated publication/evaluation?
- Has the governance panel reviewed it?

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
