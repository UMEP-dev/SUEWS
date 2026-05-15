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

- **Context**: Gather files, classify changes
- **Code Style**: lint-code
- **Scientific**: Physics validation (if applicable)
- **Testing**: Coverage, FIRST principles
- **Docs**: CHANGELOG, PR description
- **Governance**: Check feature status tags (see below)
- **Schema version audit**: If the PR touches `src/supy/data_model/`, apply the triggers in `.claude/rules/python/schema-versioning.md`. Field rename, removal, type change, or required-field addition should be accompanied by a bump to `CURRENT_SCHEMA_VERSION`, a new `SCHEMA_VERSIONS` entry, a matching handler in `src/supy/util/converter/yaml_upgrade.py`, and a `sample_config.yml` resync. Flag any of these that are missing. CI gate `.github/workflows/schema-version-audit.yml` runs the automated check; if the PR carries the `schema-audit-ok` bypass label, verify from the diff yourself that the reason is genuinely cosmetic before approving.
- **Build**: CI status, meson.build
- **Draft**: Comments for approval
- **Approval**: Wait for human confirmation
- **Post**: Only after approval

## Governance Check

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
