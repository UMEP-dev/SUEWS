# SUEWS Review and Governance Process

> **Canonical Rules Notice**
>
> Active standing rules and review skills supersede this overview whenever an example drifts. Always read:
> - Rules: [`.claude/rules/physics-change-evidence.md`](../.claude/rules/physics-change-evidence.md), [`.claude/rules/labels.md`](../.claude/rules/labels.md), [`.claude/rules/autonomous-workflow.md`](../.claude/rules/autonomous-workflow.md), [`.claude/rules/work-sizing.md`](../.claude/rules/work-sizing.md), and [`.claude/rules/review-convergence.md`](../.claude/rules/review-convergence.md)
> - Skills: `/triage-issue` ([`.claude/skills/triage-issue/SKILL.md`](../.claude/skills/triage-issue/SKILL.md)), `/triage-pr` ([`.claude/skills/triage-pr/SKILL.md`](../.claude/skills/triage-pr/SKILL.md)), `/audit-pr` ([`.claude/skills/audit-pr/SKILL.md`](../.claude/skills/audit-pr/SKILL.md)), and `/queue-pr` ([`.claude/skills/queue-pr/SKILL.md`](../.claude/skills/queue-pr/SKILL.md))

## Overview

This document outlines how SUEWS connects issue triage, pull-request review, scientific evidence gates, and code-governance routing. SUEWS does not maintain a GitHub `CODEOWNERS` file; who holds each role is recorded in [`MAINTAINERS.md`](../MAINTAINERS.md), reviewer routing is documented here and in [`.claude/rules/physics-change-evidence.md`](../.claude/rules/physics-change-evidence.md), and enforced during review via `/audit-pr` and CI checks.

## Review and Governance Panels

### Scientific Review and Domain-Owner Gates

Domain-owner sign-off on `0-physics:change` PRs is governed by [`.claude/rules/physics-change-evidence.md`](../.claude/rules/physics-change-evidence.md).
Scientific reviewers are listed in
[`MAINTAINERS.md`](../MAINTAINERS.md). Use that file to identify
reviewers for physics modules and request them manually when science review is
needed. Scientific module reviewers are intentionally not wired into GitHub
automatic review requests.

### Code Governance Panel

The table below is an advisory maintainer-routing reference. It does not grant
automatic ownership, create required reviews, or mean every PR touching these
paths needs a review request. Who the maintainers are is recorded in
[`MAINTAINERS.md`](../MAINTAINERS.md).

| Area | Path examples | Suggested maintainer |
|------|---------------|-----------------------|
| Review process and issue triage | `dev-ref/REVIEW_PROCESS.md`, `dev-ref/ISSUE_TRIAGE.md` | both maintainers |
| Roles, owners and reviewers | `MAINTAINERS.md` | both maintainers |
| Coding style, linting, and naming conventions | `dev-ref/CODING_GUIDELINES.md`, `dev-ref/FORTRAN_NAMING_CONVENTIONS.md`, `scripts/lint/`, `.ruff.toml`, `pyproject.toml` | both maintainers |
| Agent rules, skills, and audit protocol | `AGENTS.md`, `.claude/rules/`, `.claude/skills/`, `dev-ref/CODEBASE_AUDIT_PROTOCOL.md` | both maintainers |
| GitHub workflow administration | `.github/workflows/`, `.github/actions/`, `.github/scripts/` | software maintainer |

## Review Workflow

### 1. Issue Triage (`/triage-issue`)

Open issues carry the four-tier label hierarchy defined in [`.claude/rules/labels.md`](../.claude/rules/labels.md) and [`ISSUE_TRIAGE.md`](ISSUE_TRIAGE.md):

- **Type (`1-*`, pick one)**: `1-bug`, `1-feature`, `1-maintenance`, `1-question`
- **Area (`2-*`, one or more)**: `2-module:*`, `2-infra:*`, `2-doc:*`, `2-meta:*`
- **Priority (`3-*`, pick one)**: `3-P0`, `3-P1`, `3-P2`
- **Status (`4-*`, pick one)**: `4-ready`, `4-in-progress`, `4-needs-deps`, `4-needs-discussion`, `4-needs-science`, `4-needs-obs`, `4-deferred`

Assignees are set only when someone is expected to drive the issue. For module
issues that need scientific input, use the relevant `2-module:*` label and
`4-needs-science` (or `4-needs-obs` when blocked on observational
evaluation), then route the discussion to the panel owner listed in
[`MAINTAINERS.md`](../MAINTAINERS.md).

### 2. PR Triage and Labelling (`/triage-pr`)

Every PR carries:
1. Exactly one `1-*` type label (`1-bug`, `1-feature`, `1-maintenance`, or `1-question`).
2. One or more `2-*` area labels covering the touched subsystems.
3. Any required `0-*` automation/classification labels:
   - **`0-physics:change` or `0-numerics:change` (mutually exclusive; mandatory when physics or reference outputs move)**: Classified per [`.claude/rules/physics-change-evidence.md`](../.claude/rules/physics-change-evidence.md). Any PR that alters process formulation, changes a physics-affecting default, or moves outputs outside the round-off numerical class carries `0-physics:change`; a constant-precision, unit-conversion, or floating-point-representation change strictly within `< 0.5%` scale-relative (`max_i |new_i - ref_i| / max_i |ref_i|` across non-`NaN` rows per variable in each fixture; `Lob` / `L_MOD_RSL` judged by `zL` / `zeta`; any change to an identically-zero reference variable fails) and `< 0.5 W m-2` absolute flux difference carries `0-numerics:change`. CI's `Physics-change label` job fails PRs that modify reference fixtures (`sample_output_2012-*.csv` + `provenance.json`, `sample_output_stebbs.csv`, `benchmark1/*.pkl`, `w16_most.csv`) without **exactly one** of these labels.
   - **`0-pr:*` disposition labels** (`0-pr:advance`, `0-pr:continue`, `0-pr:deferred`, `0-pr:close-proposed`, `0-pr:escalate`) when triaged by `/triage-pr`.
   - Priority (`3-*`) and status (`4-*`) labels stay on the linked issue, not on the PR.

### 3. Physics and Numerical Change Gates (`0-physics:change` / `0-numerics:change`)

Per [`.claude/rules/physics-change-evidence.md`](../.claude/rules/physics-change-evidence.md):
- **Every `0-physics:change` PR** MUST include:
  1. **A `## Scientific evidence` section in the PR body** documenting which physical quantities change and the physical mechanism chain, a before/after comparison (table or figure), and expected sign and magnitude reasoning.
  2. **Domain-owner sign-off** where the rule requires it: the person in the "Sign-off required from" column of [`MAINTAINERS.md`](../MAINTAINERS.md), or the software maintainer for a module with no named owner, plus manual requests to the advisory reviewers listed there (see "Scientific Review Request" below).
  3. **Reference-fixture refresh in the same PR**, generated from the CI `reference-regenerated-cp312-manylinux-x86_64` artefact (`scripts/suews/gen_sample_output.py`, updating both the shards and `test/fixtures/data_test/provenance.json`).
  4. **Green `physics-full` CI tier** before merge (`determine-matrix.sh` automatically selects `test_tier=physics-full` on ready PRs and merge-queue runs carrying `0-physics:change` or `0-numerics:change`).
- **Every `0-numerics:change` PR** (no process-formulation change; constant precision, unit conversions, or floating-point representation strictly within the threshold in [`.claude/rules/physics-change-evidence.md`](../.claude/rules/physics-change-evidence.md)) MUST include same-PR fixture refresh, a per-variable numerical change table in the PR body, and green `physics-full` CI, without requiring `## Scientific evidence` or domain-owner sign-off.

### 4. Scientific Review Request

Request scientific review when a PR:

- changes model physics, equations, parameterisations, or scientific constants
- changes module-specific Fortran types, data-model defaults, or benchmark
  behaviour with physical meaning
- refreshes reference fixtures or benchmark outputs because the model behaviour
  changed
- updates module documentation in a way that needs domain judgement

For draft PRs, request early advice only when the reviewer is being asked for
design input. Otherwise wait until the PR is ready enough for a focused review.
Scientific reviewers are requested by hand; they are not wired into GitHub
automatic review requests, and their review is advisory.

For PRs requiring scientific validation:

1. Apply the relevant `2-module:*` label.
2. Request review manually from the domain expert(s) identified by
   [`MAINTAINERS.md`](../MAINTAINERS.md).
3. Provide context about what specifically needs review.
4. Track review status in PR comments and GitHub's review system.

Example comment:
```
<@reviewer> <@reviewer> - This PR modifies 2-module:stebbs temperature calculations.
Please review for scientific validity, particularly the changes to surface temperature iteration in suews_phys_stebbs.f95.
```

### 5. Code and Scientific Review (`/audit-pr`)

Reviewers (human maintainers, domain experts, and `/audit-pr`) check:
1. **Size Gate (`work-sizing.md`)**: One coherent, revertible concern; oversized or bundled diffs are routed to `/split-pr`.
2. **Physical Validity**: Equations match literature, units are consistent, conservation laws hold, and `## Scientific evidence` is complete when `0-physics:change` applies.
3. **Implementation & Thread Safety**: No implicitly `SAVE`d Fortran locals, proper `set_supy_error` / `supy_error_flag()` error propagation, schema version bump + upgrader when `src/supy/data_model/` changes structurally, and BR-01..BR-10 compliance across `src/suews_bridge/`.
4. **CI Gate**: Required checks (`pr-gate`, lints, exact reference gate, docs checks) are green.

### 6. Merge Criteria (`/queue-pr`)

PRs enter the GitHub merge queue (always human-gated; never pushed directly to `master`) when:
- Required CI gates (`pr-gate`, including `physics-full` when `0-physics:change` or `0-numerics:change` is present) are green.
- Code review is approved and all review threads have converged (`.claude/rules/review-convergence.md`).
- Required domain-owner sign-off is recorded for any `0-physics:change` PR.
- Documentation and `CHANGELOG.md` (where applicable) are updated.

## Guidelines for AI-Assisted Changes

For PRs authored or reviewed with coding agents:
1. Agents follow `.claude/rules/autonomous-workflow.md`: merging/enqueuing, closing issues, force-pushing unowned branches, and posting onto third-party PRs are **always human-gated**.
2. `0-physics:change` PRs always require human domain-owner / maintainer sign-off, and neither `0-physics:change` nor `0-numerics:change` has a cosmetic bypass label.
3. Verify physical mechanisms and numerical evidence directly rather than trusting plausible prose.

## Physics Module Labels (`2-module:*`)

- `2-module:ohm` - Objective Hysteresis Model
- `2-module:anohm` - Analytical OHM
- `2-module:ehc` - Explicit Heat Conduction
- `2-module:estm` - Element Surface Temperature Method (disabled; see `ESTM_REACTIVATION.md`)
- `2-module:stebbs` - STEBBS building energy model
- `2-module:lumps` - LUMPS energy balance
- `2-module:narp` - Net All-wave Radiation Parameterisation
- `2-module:spartacus` - SPARTACUS radiation
- `2-module:solweig` - Solar and longwave radiation
- `2-module:beers` - Building radiation
- `2-module:evap` - Evaporation processes
- `2-module:waterdist` - Water distribution
- `2-module:bluews` - Building water balance
- `2-module:snow` - Snow processes
- `2-module:atmmoiststab` - Atmospheric stability
- `2-module:resist` - Resistance calculations
- `2-module:rslprof` - Roughness sublayer profiles
- `2-module:anthro` - Anthropogenic heat
- `2-module:biogenco2` - Biogenic CO2
- `2-module:dailystate` - Daily state updates and LAI phenology

## Escalation

For cross-cutting or contested design and physics questions:
1. Apply `4-needs-discussion` (or `4-needs-science`) on the tracking issue so it enters the weekly dev meeting agenda.
2. Record the agreed rationale on the issue/PR and remove `4-needs-discussion` once resolved.

## Contributing as a Reviewer

Interested in joining the review panel? Contact a maintainer (see [`MAINTAINERS.md`](../MAINTAINERS.md)) with:
- Your domain expertise
- SUEWS experience
- GitHub handle
- Availability for reviews
