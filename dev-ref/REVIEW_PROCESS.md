# SUEWS Review and Governance Process

> **Canonical Rules Notice**
>
> Active standing rules and review skills supersede this overview whenever an example drifts. Always read:
> - Rules: [`.claude/rules/physics-change-evidence.md`](../.claude/rules/physics-change-evidence.md), [`.claude/rules/labels.md`](../.claude/rules/labels.md), [`.claude/rules/autonomous-workflow.md`](../.claude/rules/autonomous-workflow.md), [`.claude/rules/work-sizing.md`](../.claude/rules/work-sizing.md), and [`.claude/rules/review-convergence.md`](../.claude/rules/review-convergence.md)
> - Skills: `/triage-issue` ([`.claude/skills/triage-issue/SKILL.md`](../.claude/skills/triage-issue/SKILL.md)), `/triage-pr` ([`.claude/skills/triage-pr/SKILL.md`](../.claude/skills/triage-pr/SKILL.md)), `/audit-pr` ([`.claude/skills/audit-pr/SKILL.md`](../.claude/skills/audit-pr/SKILL.md)), and `/queue-pr` ([`.claude/skills/queue-pr/SKILL.md`](../.claude/skills/queue-pr/SKILL.md))

## Overview

This document outlines how SUEWS connects issue triage, pull-request review, scientific evidence gates, and code-governance routing. SUEWS does not maintain a GitHub `CODEOWNERS` file; reviewer routing is documented here, in [`SCIENTIFIC_REVIEWERS.md`](SCIENTIFIC_REVIEWERS.md), and in [`.claude/rules/physics-change-evidence.md`](../.claude/rules/physics-change-evidence.md), and enforced during review via `/audit-pr` and CI checks.

## Review and Governance Panels

### Scientific Review and Domain-Owner Gates

Scientific reviewer routing is maintained in [`SCIENTIFIC_REVIEWERS.md`](SCIENTIFIC_REVIEWERS.md) alongside [`.claude/rules/physics-change-evidence.md`](../.claude/rules/physics-change-evidence.md):

- **Blocking domain-owner sign-off (`0-physics:change`)**: When a physics-changing PR alters the physics or behaviour of a subsystem with a designated owner, that owner's approval is **blocking before merge**:
  - **STEBBS** (`suews_phys_stebbs.f95`, STEBBS data model, STEBBS reference fixtures) -> `@yiqing1021` (blocking sign-off required; cross-cutting non-functional maintenance with zero numerical impact may be recorded by the maintainer per `.claude/rules/physics-change-evidence.md`).
  - **Subsystems without a named subsystem owner** -> maintainer sign-off (`@sunt05`).
- **Advisory domain reviewers**: Other module reviewers listed in [`SCIENTIFIC_REVIEWERS.md`](SCIENTIFIC_REVIEWERS.md) are requested manually for domain expertise and are advisory unless designated as a blocking subsystem owner in `.claude/rules/physics-change-evidence.md`.

### Code Governance Panel

The table below is an advisory maintainer-routing reference for governance, style, and infrastructure changes:

| Area | Path examples | Suggested maintainers |
|------|---------------|-----------------------|
| Review process and issue triage | `dev-ref/REVIEW_PROCESS.md`, `dev-ref/ISSUE_TRIAGE.md` | `@sunt05`, `@suegrimmond` |
| Scientific reviewer guide | `dev-ref/SCIENTIFIC_REVIEWERS.md` | `@sunt05`, `@suegrimmond` |
| Coding style, linting, and naming conventions | `dev-ref/CODING_GUIDELINES.md`, `dev-ref/FORTRAN_NAMING_CONVENTIONS.md`, `scripts/lint/`, `.ruff.toml`, `pyproject.toml` | `@sunt05`, `@suegrimmond` |
| Agent rules, skills, and audit protocol | `AGENTS.md`, `.claude/rules/`, `.claude/skills/`, `dev-ref/CODEBASE_AUDIT_PROTOCOL.md` | `@sunt05`, `@suegrimmond` |
| GitHub workflow administration | `.github/workflows/`, `.github/actions/`, `.github/scripts/` | `@sunt05` |

## Review Workflow

### 1. Issue Triage (`/triage-issue`)

Open issues carry the four-tier label hierarchy defined in [`.claude/rules/labels.md`](../.claude/rules/labels.md) and [`ISSUE_TRIAGE.md`](ISSUE_TRIAGE.md):

- **Type (`1-*`, pick one)**: `1-bug`, `1-feature`, `1-maintenance`, `1-question`
- **Area (`2-*`, one or more)**: `2-module:*`, `2-infra:*`, `2-doc:*`, `2-meta:*`
- **Priority (`3-*`, pick one)**: `3-P0`, `3-P1`, `3-P2`
- **Status (`4-*`, pick one)**: `4-ready`, `4-in-progress`, `4-needs-deps`, `4-needs-discussion`, `4-needs-science`, `4-needs-obs`, `4-deferred`

For module issues needing scientific judgement, apply `2-module:*` and `4-needs-science` (or `4-needs-obs` when blocked on observational evaluation) and consult [`SCIENTIFIC_REVIEWERS.md`](SCIENTIFIC_REVIEWERS.md).

### 2. PR Triage and Labelling (`/triage-pr`)

Every PR carries:
1. Exactly one `1-*` type label (`1-bug`, `1-feature`, `1-maintenance`, or `1-question`).
2. One or more `2-*` area labels covering the touched subsystems.
3. Any required `0-*` automation/classification labels:
   - **`0-physics:change` (mandatory when physics or reference outputs move)**: Required whenever a PR touches physics source with numerical effect, alters a physics-affecting default/coefficient, or refreshes a reference fixture (`sample_output_2012-*.csv` + `provenance.json`, `sample_output_stebbs.csv`, `benchmark1/*.pkl`, `w16_most.csv`). CI's `Physics-change label` job fails PRs that modify reference fixtures without `0-physics:change`.
   - **`0-pr:*` disposition labels** (`0-pr:advance`, `0-pr:continue`, `0-pr:deferred`, `0-pr:close-proposed`, `0-pr:escalate`) when triaged by `/triage-pr`.
   - Priority (`3-*`) and status (`4-*`) labels stay on the linked issue, not on the PR.

### 3. Physics-Change Evidence Gate (`0-physics:change`)

Per [`.claude/rules/physics-change-evidence.md`](../.claude/rules/physics-change-evidence.md), every `0-physics:change` PR MUST include:
1. **A `## Scientific evidence` section in the PR body** documenting:
   - Which physical quantities change and the physical mechanism chain.
   - A before/after comparison (table or figure) of the affected outputs.
   - Expected sign and magnitude reasoning explaining why the new values are physically right.
2. **Blocking domain-owner sign-off** where applicable (STEBBS -> `@yiqing1021`; otherwise maintainer `@sunt05`), plus manual requests to relevant advisory reviewers in [`SCIENTIFIC_REVIEWERS.md`](SCIENTIFIC_REVIEWERS.md).
3. **Reference-fixture refresh in the same PR**, generated from the CI `reference-regenerated-cp312-manylinux-x86_64` artefact (`scripts/suews/gen_sample_output.py`, updating both the shards and `test/fixtures/data_test/provenance.json`).
4. **Green `physics-full` CI tier** before merge (`determine-matrix.sh` automatically selects `test_tier=physics-full` on ready PRs and merge-queue runs carrying `0-physics:change`).

Example review request comment:
```text
@yiqing1021 @denisehertwig - This 0-physics:change PR modifies 2-module:stebbs
indoor-temperature calculations in suews_phys_stebbs.f95. Please review the
## Scientific evidence section and refreshed STEBBS fixture (@yiqing1021 blocking
owner sign-off requested per physics-change-evidence.md).
```

### 4. Code and Scientific Review (`/audit-pr`)

Reviewers (human maintainers, domain experts, and `/audit-pr`) check:
1. **Size Gate (`work-sizing.md`)**: One coherent, revertible concern; oversized or bundled diffs are routed to `/split-pr`.
2. **Physical Validity**: Equations match literature, units are consistent, conservation laws hold, and `## Scientific evidence` is complete when `0-physics:change` applies.
3. **Implementation & Thread Safety**: No implicitly `SAVE`d Fortran locals, proper `set_supy_error` / `supy_error_flag()` error propagation, schema version bump + upgrader when `src/supy/data_model/` changes structurally, and BR-01..BR-10 compliance across `src/suews_bridge/`.
4. **CI Gate**: Required checks (`pr-gate`, lints, exact reference gate, docs checks) are green.

### 5. Merge Criteria (`/queue-pr`)

PRs enter the GitHub merge queue (always human-gated; never pushed directly to `master`) when:
- Required CI gates (`pr-gate`, including `physics-full` when `0-physics:change` is present) are green.
- Code review is approved and all review threads have converged (`.claude/rules/review-convergence.md`).
- Required domain-owner sign-off is recorded for any `0-physics:change` PR.
- Documentation and `CHANGELOG.md` (where applicable) are updated.

## Guidelines for AI-Assisted Changes

For PRs authored or reviewed with coding agents:
1. Agents follow `.claude/rules/autonomous-workflow.md`: merging/enqueuing, closing issues, force-pushing unowned branches, and posting onto third-party PRs are **always human-gated**.
2. `0-physics:change` PRs always require human domain-owner / maintainer sign-off; there is no cosmetic bypass label for the physics evidence gate.
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

Interested in joining the scientific review panel? Contact `@sunt05` or `@suegrimmond` with your domain expertise, SUEWS experience, GitHub handle, and availability.
