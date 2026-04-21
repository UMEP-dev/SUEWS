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
- **Step 3 - Scientific**: Physics validation + description rigour (if applicable)
- **Step 4 - Testing**: Coverage, FIRST principles
- **Step 5 - Docs**: CHANGELOG, PR description
- **Step 6 - Governance**: Check feature status tags (see below)
- **Step 7 - Schema version audit**: If the PR touches `src/supy/data_model/`, apply the triggers in `.claude/rules/python/schema-versioning.md`. Field rename, removal, type change, or required-field addition should be accompanied by a bump to `CURRENT_SCHEMA_VERSION`, a new `SCHEMA_VERSIONS` entry, a matching handler in `src/supy/util/converter/yaml_upgrade.py`, and a `sample_config.yml` resync. Flag any of these that are missing. CI gate `.github/workflows/schema-version-audit.yml` runs the automated check; if the PR carries the `schema-audit-ok` bypass label, verify from the diff yourself that the reason is genuinely cosmetic before approving.
- **Step 8 - Build**: CI status, meson.build
- **Step 9 - Draft**: Comments for approval
- **Step 10 - Approval**: Wait for human confirmation
- **Step 11 - Post**: Only after approval

## Scientific PR Description Rigour (Step 3)

For PRs that introduce or materially change a scientific component
(physics, numerics, calibration, benchmark), the PR description + thread
must expose the science clearly so reviewers can judge it in-place. This
is a description-quality check, not a separate artefact requirement —
evidence lives in the PR, not in a sidecar dashboard.

**A PR is "scientific" if it touches any of**:

- `src/suews/src/suews_phys_*.f95`
- `src/suews/src/suews_ctrl_daily_state.f95`
- `src/supy/data_model/` (new physics-facing fields or enums)
- New YAML parameters that affect model output
- A CHANGELOG entry tagged `[feature]` or `[change]` that is not pure
  tooling / build / docs

**Required in the PR description (or posted as comments in the thread)**:

- [ ] **Methodology** — what was changed and how it was developed/validated
      (governing equation, numerical approach, calibration procedure)
- [ ] **Scientific decisions** — key choices made and rationale
      (why this parameterisation, why this threshold, what alternatives
      were considered)
- [ ] **Results** — quantitative evidence that the change works, posted
      in the PR (numbers, plots, before/after comparisons, test outputs).
      A bare "tests pass" is not sufficient for scientific changes.

**If missing or thin**: flag as a blocking review comment and ask the
author to expand the PR body or post a follow-up comment with the
missing pieces. Do not approve a scientific PR whose description reads
as purely mechanical ("refactored X", "added Y") without scientific
narrative. If scope is ambiguous, ask the author to classify rather
than guess.

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
| Description rigour | PASS/FAIL/N/A |
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
