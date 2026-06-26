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

## Size Gate (run first)

Before the detailed review, assess the review surface. If the PR is too large to
review well in one pass, or bundles unrelated concerns, lead with a split
recommendation instead of spending the review on an unreviewable diff:

- Check diff scope: `gh pr diff {pr} --name-only` plus additions/deletions per
  file. Watch for a large meaningful diff, many touched subsystems, or a mix of
  refactor + behaviour change + formatting.
- Apply `.claude/rules/work-sizing.md`. If the PR fails the right-sized-PR test,
  recommend splitting it (a stacked series of small PRs, or carving out
  mechanical/generated changes first) before review. PR size relates to
  function/file/module granularity and the language conventions.
- State the proposed split concretely: which files/concerns go in which PR, and
  the suggested order. This is a recommendation; do not rewrite the branch.
- A best-effort review may still follow if the author prefers, but the size
  recommendation comes first.
- To carry out the split, hand off to `split-pr`; audit-pr only recommends.

## Workflow

- **Context**: Gather files, classify changes
- **Size gate (first)**: see "Size Gate" above -- recommend a split before reviewing an oversized or bundled PR
- **Code Style**: lint-code
- **Scientific**: Physics validation (if applicable)
- **Testing**: Coverage, FIRST principles
- **Docs**: CHANGELOG, PR description
- **Governance**: Check feature status tags (see below)
- **Schema version audit**: If the PR touches `src/supy/data_model/`, apply the triggers in `.claude/rules/python/schema-versioning.md`. Field rename, removal, type change, or required-field addition should be accompanied by a bump to `CURRENT_SCHEMA_VERSION`, a new `SCHEMA_VERSIONS` entry, a matching handler in `src/supy/util/converter/yaml_upgrade.py`, and a `sample_config.yml` resync. Flag any of these that are missing. CI gate `.github/workflows/schema-version-audit.yml` runs the automated check; if the PR carries the `0-ci:schema-audit-ok` bypass label, verify from the diff yourself that the reason is genuinely cosmetic before approving.
- **Physics-change evidence audit**: If the diff touches physics source (`suews_phys_*.f95`, Rust physics backend), moves a reference fixture (`test/fixtures/data_test/sample_output.csv.gz`, `.../stebbs_test/sample_output_stebbs.csv`, `test/fixtures/benchmark1/*.pkl`), or changes a physics-affecting data-model default, apply `.claude/rules/physics-change-evidence.md`. Confirm the `0-physics:change` label is present (apply or flag if missing), the PR body has a `## Scientific evidence` section (quantities + mechanism, before/after comparison, sign/magnitude reasoning -- a bare "outputs changed" is blocking), the owner sign-off is present for any owned subsystem (STEBBS -> `@yiqing1021`), any moved fixture is refreshed in this PR (or a linked PR), and the full `-m physics` tier (including `slow`) has run green.
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
Verdict: clean | needs-attention
Size gate: pass | split-recommended

| Category | Status |
|----------|--------|
| Code Style | PASS/FAIL |
| Scientific | PASS/FAIL/N/A |
| Testing | PASS/FAIL |
| Docs | PASS/FAIL |

### Key Findings
- [severity] finding (severity = blocking | major | minor | false-positive)

The `Verdict`, `Size gate`, and per-finding `[severity]` tags are the fields a
consuming skill (e.g. `fix-issue` step 5) branches on; keep them explicit.

### Suggested Reviewers
@reviewer (module:name)
```

## Approval Options

`approve` | `approve with edits` | `skip line comments` | `cancel`

## Privacy scrub gate

Before drafting any line comment or summary, scan the model-authored finding text
(problem descriptions, summary, reviewer notes) for internal tracker IDs
(`PER-\d+` and similar), absolute local/home paths (`/Users/...`,
worktree/internal-tooling paths), and private host names (internal compute
servers and the like). Redact these in distilled text. If a private token appears inside a
verbatim quote pulled from the diff, CI log, or stack trace that must be preserved
to make the finding actionable, replace it with neutral phrasing or escalate
rather than post as-is. This operationalises the "Privacy is a hard invariant on
every outward write" rule in `.claude/rules/autonomous-workflow.md`.

## Autonomous Mode

audit-pr is a read-only review stage in the issue -> PR -> merge pipeline. Under
an autonomous orchestrator it NEVER posts to GitHub; its terminal output is the
drafted review plus the machine-readable handoff block (`Verdict`, `Size gate`,
per-finding `[severity]`).

- Auto-applicable (read-only / self-scoped): gather context, run the Size Gate,
  perform the review, and emit the verdict block. This review-and-emit tier is the
  only one an opt-in standing approval can cover.
- Human-gated, always escalate (never auto-applied, in any mode): posting any line
  comment or review onto the PR. The "Wait for human confirmation" step is NOT
  satisfied by a batch standing approval; there is no standing approval for
  posting. The autonomous terminal is escalate-with-draft, the same shape as
  `triage-issue`'s needs-discussion and `queue-pr`'s merge escalation.

## References

- `references/review-checklist.md` - Detailed checklist
- `references/workflow-steps.md` - Full workflow
- `references/style-checks.md` - Style rules
- `dev-ref/CODING_GUIDELINES.md`
- `dev-ref/REVIEW_PROCESS.md`
