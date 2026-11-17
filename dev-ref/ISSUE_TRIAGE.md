# SUEWS Issue Triage Guide

> **Canonical Rules Notice**
>
> Active standing rules and automated triage skills supersede this guide whenever an example drifts. Always read:
> - Rules: [`.claude/rules/labels.md`](../.claude/rules/labels.md), [`.claude/rules/autonomous-workflow.md`](../.claude/rules/autonomous-workflow.md), and [`.claude/rules/work-sizing.md`](../.claude/rules/work-sizing.md)
> - Skills: `/triage-issue` ([`.claude/skills/triage-issue/SKILL.md`](../.claude/skills/triage-issue/SKILL.md) and [`.claude/skills/triage-issue/references/label-hierarchy.md`](../.claude/skills/triage-issue/references/label-hierarchy.md)) and `/triage-pr` ([`.claude/skills/triage-pr/SKILL.md`](../.claude/skills/triage-pr/SKILL.md))

This guide describes how issues in `UMEP-dev/SUEWS` are triaged, scoped, and labelled under the repository's tiered label hierarchy.

## Quick Reference

### Four-Tier Label System (`1-*`..`4-*`) + `0-*` Automation Metadata
Every open issue carries labels from the four tiers below, using **only** existing labels in the tiered hierarchy (`gh label list` -- never invent or create untiered labels):

- **`1-*` Type (exactly one)**: `1-bug`, `1-feature`, `1-maintenance` (never `1-enhancement`), `1-question`
- **`2-*` Area (one or more)**: `2-module:*`, `2-infra:*`, `2-doc:*`, `2-meta:*`
- **`3-*` Priority (exactly one on an open issue)**: `3-P0`, `3-P1`, `3-P2`
- **`4-*` Status (exactly one on an open issue)**: `4-ready`, `4-in-progress`, `4-needs-deps`, `4-needs-discussion`, `4-needs-science`, `4-needs-obs`, `4-deferred`

**`0-*` Automation and CI metadata labels** do not count towards the four tiers and are applied only where a rule or skill calls for them:
- Issue automation / provenance: `0-auto:audited` (applied by `/triage-issue` after auditing an issue), `0-source:discourse` (imported from SUEWS Discourse)
- PR classification / disposition / CI gates: `0-physics:change`, `0-auto:pr-audited`, `0-pr:advance`, `0-pr:continue`, `0-pr:deferred`, `0-pr:close-proposed`, `0-pr:escalate`, `0-ci:schema-audit-ok`, `0-ci:knowledge-pack-audit-ok`, `0-ci:encoding-audit-ok`, `0-ci:clippy-ok`

**Pull Request Labels vs Issue Labels**:
- A PR carries **one `1-*` type** and **one or more `2-*` areas**, plus any `0-*` label required by its rules (such as `0-physics:change`).
- Priority (`3-*`) and status (`4-*`) labels belong on the **issue**, not on the PR.

## Triage Decision Tree

### Step 1: Issue Type (Pick ONE `1-*` Label)
```text
Is it reporting incorrect behaviour, a crash, wrong docs, or a regression?
  `-> 1-bug

Is it requesting a new capability, rule, or expanded behaviour?
  `-> 1-feature

Is it cleanup, refactoring, dependency updates, or tooling/docs maintenance without behaviour change?
  `-> 1-maintenance

Is it a support question, uncertainty, or open design question?
  `-> 1-question
```

### Step 2: Area (Pick ONE OR MORE `2-*` Labels)
```text
What part(s) of the system does it touch? (Apply multiple labels for cross-cutting issues)
|-- Project governance or release? -> 2-meta:governance, 2-meta:release
|-- Physics module?                -> 2-module:ohm, 2-module:snow, 2-module:stebbs, ...
|-- Infrastructure / Python / CI?  -> 2-infra:data-model, 2-infra:ci, 2-infra:build, ...
`-- Documentation?                 -> 2-doc:user, 2-doc:api, 2-doc:dev
```

### Step 3: Priority (Pick ONE `3-*` Label)
```text
|-- Critical breakage, segfault, data corruption, or release blocker? -> 3-P0
|-- High-priority correctness bug or important validator/API issue?   -> 3-P1
`-- Standard feature, cleanup, docs update, or backlog item?          -> 3-P2
```

### Step 4: Status (Pick ONE `4-*` Label)
```text
What is the current execution state? (Never leave two 4-* labels on one issue)
|-- Clear, scoped, and planned for near-term work  -> 4-ready
|-- Actively worked on or has an open PR           -> 4-in-progress
|-- Waiting on another issue, PR, or prerequisite  -> 4-needs-deps
|-- Agenda item needing maintainer decision        -> 4-needs-discussion
|-- Requires scientific input or model judgement   -> 4-needs-science
|-- Requires observational data / evaluation       -> 4-needs-obs
`-- Valid issue, not planned for near-term work    -> 4-deferred
```

### Step 5: Right-Sizing Check (`work-sizing.md`)
Before marking an issue `4-ready`, verify that it represents a single, reviewable concern with testable acceptance criteria (see `.claude/rules/work-sizing.md`). If it bundles multiple deliverables or spans independent subsystems, `/triage-issue` marks readiness `needs-split` and proposes an umbrella + native GitHub sub-issues decomposition (creating issues or closing duplicates is always human-gated).

## Complete Tiered Label Reference

### `1-*` Type Labels (Exactly One)
- `1-bug` - Observed defect, crash, regression, or documentation inaccuracy
- `1-feature` - New functionality, parameterisation, or user-facing capability
- `1-maintenance` - Refactoring, cleanup, dependency updates, CI/tooling maintenance
- `1-question` - User question, support query, or unframed design question

### `2-*` Area Labels (One or More)

#### Meta / Project
- `2-meta:governance` - Project governance and process
- `2-meta:release` - Release planning and management

#### Physics Modules
- `2-module:anohm` - Analytical OHM
- `2-module:anthro` - Anthropogenic heat
- `2-module:atmmoiststab` - Atmospheric stability
- `2-module:beers` - Building radiation
- `2-module:biogenco2` - Biogenic CO2
- `2-module:bluews` - Building water balance
- `2-module:dailystate` - Daily state updates and LAI phenology
- `2-module:ehc` - Explicit Heat Conduction
- `2-module:estm` - Element Surface Temperature Method (currently disabled; see `ESTM_REACTIVATION.md`)
- `2-module:evap` - Evaporation processes
- `2-module:lumps` - LUMPS energy balance
- `2-module:narp` - Net All-wave Radiation Parameterisation
- `2-module:ohm` - Objective Hysteresis Model
- `2-module:resist` - Resistance calculations
- `2-module:rslprof` - Roughness sublayer profiles
- `2-module:snow` - Snow processes
- `2-module:solweig` - Solar and longwave radiation
- `2-module:spartacus` - SPARTACUS radiation
- `2-module:stebbs` - STEBBS building energy model
- `2-module:waterdist` - Water distribution

#### Infrastructure
- `2-infra:data-model` - Pydantic data models, schema versioning, and validation
- `2-infra:ci` - CI/CD pipelines and GitHub Actions
- `2-infra:build` - Build system (`meson.build`, `Makefile`, Cargo/Fortran compilation)
- `2-infra:packaging` - Wheel distribution and dependencies
- `2-infra:test` - Testing infrastructure and pytest markers
- `2-infra:fortran-python` - Rust/C-ABI Fortran-Python bridge (`src/suews_bridge/`)
- `2-infra:logging` - Logging, kernel warnings, and diagnostic output
- `2-infra:type-safety` - Type safety and compiler checks
- `2-infra:code-refactor` - Code refactoring and cleanup
- `2-infra:input` - Input data loading and conversion
- `2-infra:output` - Output formatting and export
- `2-infra:utility` - Utility helpers

#### Documentation
- `2-doc:user` - User guides and tutorials
- `2-doc:api` - API and schema documentation
- `2-doc:dev` - Developer documentation (`dev-ref/`, `.claude/`)

### `3-*` Priority Labels (Exactly One on Open Issues)
- `3-P0` - Critical (crashes, silent wrong physics, release blockers)
- `3-P1` - High (major functionality broken or high-impact user/validator defect)
- `3-P2` - Medium / standard backlog priority

### `4-*` Status Labels (Exactly One on Open Issues)
- `4-ready` - Scoped and ready for near-term implementation
- `4-in-progress` - Active work in flight or open PR linked
- `4-needs-deps` - Blocked on another issue, PR, or upstream dependency
- `4-needs-discussion` - Queued for the weekly dev catch-up meeting; remove once decided
- `4-needs-science` - Needs scientific evidence, parameterisation choice, or domain sign-off
- `4-needs-obs` - Needs observational dataset evaluation before proceeding
- `4-deferred` - Valid issue parked outside near-term priorities

*Note*: Untiered labels (such as `1-enhancement`, `design-sensitive`, `mechanical`, or `hard-gate`) are retired and must not be recreated.

## Triage Examples

### Example 1: Physics Bug Report
**Issue**: "SUEWS crashes when snow module is enabled on multi-grid runs"
- `1-bug`, `2-module:snow`, `3-P0`, `4-ready`

### Example 2: Cross-Cutting Maintenance Refactor
**Issue**: "Consolidate Fortran snow derived types and update bridge docs"
- `1-maintenance`, `2-module:snow`, `2-infra:fortran-python`, `2-doc:dev`, `3-P2`, `4-ready`

### Example 3: Weekly Meeting Decision Item
**Issue**: "Decide deprecation timeline for legacy table converter flags"
- `1-maintenance`, `2-infra:input`, `2-meta:governance`, `3-P2`, `4-needs-discussion`

### Example 4: Observational Evaluation Needed
**Issue**: "Calibrate new storage-heat coefficients against urban flux-tower observations"
- `1-feature`, `2-module:ohm`, `3-P2`, `4-needs-obs`

## Useful GitHub Search Filters

```text
# High-priority items ready to implement
is:issue is:open label:3-P0,3-P1 label:4-ready

# Weekly dev meeting agenda
is:issue is:open label:4-needs-discussion

# Issues awaiting scientific or observational input
is:issue is:open label:4-needs-science,4-needs-obs

# Unaudited open issues (for /triage-issue batch sweeps)
is:issue is:open -label:0-auto:audited
```

## Quick Triage Checklist

- [ ] Check live labels (`gh label list`) -- never create new labels without maintainer approval
- [ ] Set exactly one `1-*` type (`1-bug`, `1-feature`, `1-maintenance`, `1-question`)
- [ ] Set one or more `2-*` area labels (`2-module:*`, `2-infra:*`, `2-doc:*`, `2-meta:*`)
- [ ] Set exactly one `3-*` priority (`3-P0`, `3-P1`, `3-P2`)
- [ ] Set exactly one `4-*` status (`4-ready`, `4-in-progress`, `4-needs-deps`, `4-needs-discussion`, `4-needs-science`, `4-needs-obs`, `4-deferred`)
- [ ] Verify right-sizing (`work-sizing.md`) before marking `4-ready`
- [ ] Close completed issues directly rather than inventing a "done" label
