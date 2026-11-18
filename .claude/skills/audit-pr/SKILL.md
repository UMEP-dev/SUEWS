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
- **Rule source**: Read `AGENTS.md`, rules, skills and agents from the PR's base
  branch (`git show origin/<base>:<path>`, after fetching it), not the PR head.
  If the PR changes one of them, judge the rest of the diff against the base
  version and review that change as its own finding; a code change bundled with
  an edit that `.claude/rules/autonomous-workflow.md` ("Gates": a change does not
  edit the checks that judge it) forbids is `[major]`.
- **Regression checklist**: read `dev-ref/REGRESSION_CHECKLIST.md` from the base
  branch, match each trigger against the diff, and report every matched item as
  checked (with the command run or the lines inspected), not applicable (with
  the reason), not verified (with what is missing), or failed (`[major]`). Do
  not check unmatched items.
- **Code Style**: lint-code
- **Fortran error-reporting hygiene**: If the diff touches `src/suews/**/*.f9*`
  or `src/suews_bridge/c_api/**/*.f9*`, dispatch the read-only
  `fortran-error-state-reviewer` agent (`.claude/rules/fortran/error-reporting.md`)
  and fold its `[fortran-error-state-reviewer] verdict` findings into Code Style / Scientific
- **Scientific**: Physics validation (if applicable)
- **Testing**: Coverage, FIRST principles
- **Rust bridge contract**: For bridge changes (`src/suews_bridge/**`) or changes
  to either side of its input/output boundary, apply
  `.claude/rules/rust/bridge-contract.md`. Record applicable BR rule IDs, source
  revision, evidence and unresolved gaps using its audit format. Check ownership
  as well as numerical correctness; do not treat a default round trip as
  field-to-kernel wiring evidence.
- **Docs**: CHANGELOG, PR description; if the diff touches the user-facing
  surface (`src/supy/data_model/`, physics options, output or forcing
  registries, `src/supy/util/converter/`, the public API or CLI), dispatch the
  `docs-reviewer` agent in drift mode **findings-only** (it must not edit the
  author's branch) and fold its report into the Docs row
- **Governance**: Check feature status tags (see below)
- **Suggested reviewers**: Derive reviewer suggestions from changed files,
  `MAINTAINERS.md`, and `dev-ref/REVIEW_PROCESS.md`; include
  them in the draft summary without treating them as automatic blockers.
- **Schema version audit**: If the PR touches `src/supy/data_model/` or
  `src/supy/sample_data/sample_config.yml`, dispatch the read-only
  `schema-migration-checker` agent and apply `.claude/rules/python/schema-versioning.md`.
  Field rename, removal, type change, required-field addition, or default-behaviour
  change must be accompanied by a bump to `CURRENT_SCHEMA_VERSION`, a new
  `SCHEMA_VERSIONS` entry, a matching handler in
  `src/supy/util/converter/yaml_upgrade.py`, a `sample_config.yml` resync, AND
  a regenerated `test/fixtures/sample_short/sample_config_short.yml`
  (`python test/fixtures/sample_short/make_sample_short.py`). Note that
  `test/fixtures/release_configs/<tag>.yml` and `_PACKAGE_TO_SCHEMA` mappings
  apply **only to formal release tags** (never `.devN` bumps). CI gate
  `.github/workflows/schema-version-audit.yml` runs the automated check on
  `src/supy/data_model/core/` + `sample_config.yml`; if the PR carries the
  `0-ci:schema-audit-ok` bypass label, verify from the diff yourself that the
  reason is genuinely cosmetic before approving.
- **Physics and numerical change audit**: If the diff touches physics source (`suews_phys_*.f95`, `src/suews/ext_lib/**`, Rust physics backend), moves a reference fixture (`test/fixtures/data_test/sample_output_2012-*.csv` and its `provenance.json` sidecar, `test/fixtures/data_test/stebbs_test/sample_output_stebbs.csv`, `test/fixtures/benchmark1/*.pkl`, `test/fixtures/w16_most.csv`), or changes a physics-affecting data-model default, apply `.claude/rules/physics-change-evidence.md`. Classify the diff first (never remove or downgrade an existing classification label automatically; preserve existing labels on conflict and flag for a maintainer): for a formulation/default change or any output-moving change that fails any `0-numerics:change` condition (including any non-masked shift at or above `0.1%` relative or `0.5 W m-2` absolute flux difference, any zero-to-nonzero shift, or a reference-lane build-environment update), require `0-physics:change`, `## Scientific evidence` (quantities + mechanism, before/after comparison, sign/magnitude reasoning -- a bare "outputs changed" is `[blocking]`), domain-owner sign-off (`MAINTAINERS.md`), same-PR fixture refresh, and green `physics-full` CI; for a round-off-only constant precision, unit conversion, or floating-point representation change with no formulation change and all shifts strictly below `0.1%` relative and `0.5 W m-2` absolute flux difference across non-masked rows (only `Lob` and `L_MOD_RSL` may exclude aligned rows matching the rule's reproducible near-neutral mask, with the mask, excluded `k / N` count, and `< 0.1%` remaining-row delta recorded in the table; resistances such as `RA`, `RB`, and `RS` are never masked), accept `0-numerics:change` with same-PR fixture refresh, a per-variable max absolute/relative change table in the PR body, and green `physics-full` CI (no `## Scientific evidence` or domain-owner sign-off required; applying `0-numerics:change` to a formulation, build-environment, or above-threshold change, or applying both labels simultaneously, is `[blocking]`).
- **Build**: `src/supy/meson.build` (`src_f95_all`, `py.install_sources`),
  `src/suews/Makefile`, and `src/suews/Makefile.deps` (`python scripts/suews/gen_fortran_deps.py`)
  for new or changed source files / Fortran `USE` dependencies
- **CI gate audit**: see "CI Gate Audit" below -- every non-green check gets a named
  remedy in the draft, not just a status
- **Draft**: Comments for approval
- **Approval**: Wait for human confirmation
- **Post**: Only after approval

## CI Gate Audit

A review that audits the diff but ignores the checks hands back a PR that still
cannot move. Every non-green check gets a diagnosis and a named remedy in the draft.

1. **Read the checks**, the ruleset's required contexts, and the failing job logs:

   ```bash
   gh pr checks {pr} --repo UMEP-dev/SUEWS --json name,state,bucket,workflow,link
   gh run view --repo UMEP-dev/SUEWS --job {job-id} --log-failed
   ```

   Query which contexts are actually required rather than assuming, and read the
   `<!-- ci-build-plan -->` bot comment for which lanes this PR's paths selected.

2. **Classify each non-green check by who can clear it** -- author-fixable (a
   specific edit, in this PR), maintainer-gated (a documented bypass label, with the
   reason the diff qualifies), re-trigger mechanics (correct already, run is stale),
   or infrastructure/flake (unrelated to the diff). The classification is the
   actionable part; state it. Remedies compound: gh#1642 needed
   `maintainer-gated -> re-trigger`, and neither half alone cleared the gate.
   Classify a failure as flake only with evidence that the same test passed and
   failed on the same commit in the same job and environment, and name the
   test; for a flake on a required check, the draft cites an existing tracking
   issue or proposes one, rather than leaving it to a rerun.

3. **Report it as a finding** with a severity: red required or convention-blocking
   check -> `[blocking]`; a bypass label applied for a reason the diff does not
   support -> `[major]`; red informational check -> `[minor]` or a note; pending
   required checks -> a note in the summary.

4. **Propose, never perform.** Applying a bypass label, re-running a workflow,
   closing/reopening, or pushing a fix onto a PR you do not own are author or
   maintainer actions. Name them; do not do them.

Two gates (`schema-version-audit`, `knowledge-pack-audit`) read their bypass label
from the static event payload, so a label added after the run started leaves the
gate red and `gh run rerun` replays the stale payload. Push a commit or
close/reopen instead.

Gate-by-gate remedies, the required-vs-blocking distinction, the
green/near-green/red vocabulary (Part C of `triage-pr`'s rubric, plus its one
named amendment), and the payload race: `references/ci-gates.md`.

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

## Suggested Reviewers

In every audit summary, include suggested reviewers based on the PR diff:

1. Start from changed files (`gh pr diff {pr} --name-only`).
2. For physics files, use the manual scientific reviewer routing documented in
   `MAINTAINERS.md`.
3. For modules without named domain reviewers, suggest the default scientific
   owners documented in `MAINTAINERS.md`.
4. For coding style, linting, naming, issue-triage, or review-process changes,
   suggest the code-governance maintainers documented in
   `dev-ref/REVIEW_PROCESS.md`.

These are reviewer suggestions, not permission to request reviews or block a PR
by themselves. If a physics-changing PR lacks the needed scientific review or
sign-off, report that separately under the scientific evidence audit.

## Change Classification

| Type | Pattern | Review |
|------|---------|--------|
| Physics | `src/suews/src/suews_phys_*.f95` | Scientific + code + `fortran-error-state-reviewer` |
| Utility | `src/suews/src/suews_util_*.f95` | Code + `fortran-error-state-reviewer` |
| Control | `src/suews/src/suews_ctrl_*.f95` | Code + `fortran-error-state-reviewer` |
| External Fortran | `src/suews/ext_lib/**` | Scientific/vendor + build + `fortran-error-state-reviewer` |
| Rust Bridge & C-API | `src/suews_bridge/**` (Rust + `c_api/*.f9*`) | Bridge contract + Rust/Fortran code (`fortran-error-state-reviewer` on `.f9*`) |
| Python & Data Model | `src/supy/**/*.py`, `src/supy/**/*.{yml,yaml,json}` | Code + `schema-migration-checker` / `docs-reviewer` when applicable |
| MCP Server | `mcp/**` | Code + MCP packaging/contract |

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
CI gate: green | near-green | red

| Category | Status |
|----------|--------|
| Code Style | PASS/FAIL |
| Scientific | PASS/FAIL/N/A |
| Testing | PASS/FAIL |
| Docs | PASS/FAIL |
| CI | PASS/FAIL/PENDING |

### Key Findings
- [severity] finding (severity = blocking | major | minor | nit; per `.claude/rules/review-convergence.md`, `false-positive` is a consumer dismissal disposition in `fix-issue`, not a severity emitted here)

### CI Findings
- [severity] {check name} -- enforces {what}; red because {cause for this diff}.
  Remedy: {concrete action} ({author-fixable | maintainer-gated | re-trigger |
  infrastructure}; compound remedies chain, e.g. `maintainer-gated -> re-trigger`,
  with the actor named at each step)

Omit the CI Findings section when every check is green. Never list a red check
without a remedy line.

The `Verdict`, `Size gate`, `CI gate`, and per-finding `[severity]` tags are the
fields a consuming skill (e.g. `fix-issue` step 5) branches on; keep them explicit.

### Suggested Reviewers
- @reviewer (source: dev-ref reviewer guide; reason: changed area)
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
`CI gate`, per-finding `[severity]`).

- Auto-applicable (read-only / self-scoped): gather context, run the Size Gate,
  read the checks and failing job logs, perform the review, and emit the verdict
  block. This review-and-emit tier is the only one an opt-in standing approval can
  cover.
- Human-gated, always escalate (never auto-applied, in any mode): posting any line
  comment or review onto the PR, and every CI remedy that mutates the PR --
  applying a bypass label, re-running a workflow, closing/reopening to refresh the
  event payload, or pushing a fix. Naming such a remedy in the draft is part of the
  read-only review tier; carrying it out never is. The "Wait for human
  confirmation" step is NOT
  satisfied by a batch standing approval; there is no standing approval for
  posting. The autonomous terminal is escalate-with-draft, the same shape as
  `triage-issue`'s needs-discussion and `queue-pr`'s merge escalation.

## References

- `references/review-checklist.md` - Detailed checklist
- `references/workflow-steps.md` - Full workflow
- `references/ci-gates.md` - CI gate diagnosis, remedies, and bypass-label policy
- `references/style-checks.md` - Style rules
- `dev-ref/CODING_GUIDELINES.md`
- `dev-ref/REVIEW_PROCESS.md`
