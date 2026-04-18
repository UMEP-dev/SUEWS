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
- **Step 3 - Scientific**: Physics validation + dev-note demo (if applicable)
- **Step 4 - Testing**: Coverage, FIRST principles
- **Step 5 - Docs**: CHANGELOG, PR description
- **Step 6 - Governance**: Check feature status tags (see below)
- **Step 7 - Build**: CI status, meson.build
- **Step 8 - Draft**: Comments for approval
- **Step 9 - Approval**: Wait for human confirmation
- **Step 10 - Post**: Only after approval

## Scientific PR Dev-Note Demo (Step 3)

For PRs that introduce or materially change a scientific component (physics,
numerics, calibration, benchmark), require an archived dev-note demonstrating
the change works.

A PR is "scientific" if it touches any of:

- `src/suews/src/suews_phys_*.f95`
- `src/suews/src/suews_ctrl_daily_state.f95`
- `src/supy/data_model/` (new physics-facing fields or enums)
- New YAML parameters that affect model output
- Any change whose CHANGELOG entry is `[feature]` or `[change]` and is
  not pure tooling / build / docs

**Required artefacts on a scientific PR**:

- [ ] Summary page at `docs/source/development/<slug>.rst`
- [ ] Dashboard at `docs/source/_extra/dev-notes/<slug>/dashboard.html`,
      self-contained by default (figures committed as siblings)
- [ ] `development/index.rst` toctree updated to include the new page
- [ ] Only if the dev-note ships with genuinely heavy assets (figures
      >5 MB each, videos, NetCDF, large PDFs): a GitHub Release tagged
      `dev-notes-<slug>` with those assets uploaded and the committed
      HTML referencing them via release URLs

**If the demo is missing**: flag as a blocking review comment and propose
`scripts/suews/build_dev_note.py` as the path to generate the artefacts.
Do not approve the PR for merge until the dev-note lands, unless the PR
author and a maintainer explicitly agree the change is not scientific.

**If the demo is present but incomplete**: use line comments to request
the specific missing pieces (e.g. "sensitivity sweep figure missing",
"released asset https://github.com/.../fig.png returns 404 — release not
uploaded?"). A release URL is only expected when the PR opted into
offloading large figures; small figures sitting next to the HTML is the
default and is correct.

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
| Dev-note demo | PASS/FAIL/N/A |
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
