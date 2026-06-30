# SUEWS Review and Governance Process

## Overview

This document outlines how SUEWS connects issue triage, PR review, scientific
review, and code-governance ownership. The goal is to preserve the existing
review panel while keeping scientific review requests intentional and manual.
GitHub CODEOWNERS is reserved for governance, workflow, linting, and process
ownership.

## Review and Governance Panels

The SUEWS review panel consists of domain experts responsible for validating
module-specific scientific changes. The code-governance panel owns review
process, coding style, issue-triage conventions, and automatic ownership for
governance files.

### Scientific Review Panel

Scientific reviewer routing is maintained in
[`SCIENTIFIC_REVIEWERS.md`](SCIENTIFIC_REVIEWERS.md). Use that guide to identify
reviewers for physics modules and request them manually when science review is
needed. Scientific module reviewers are intentionally not listed in
`.github/CODEOWNERS`, so GitHub does not request them automatically when a PR is
marked ready.

### Code Governance Panel

| Area | Owners |
|------|--------|
| Review process and issue triage | @sunt05, @suegrimmond |
| Coding style, linting, and naming conventions | @sunt05, @suegrimmond |
| GitHub workflow administration | @sunt05 |

## Review Workflow

### 1. Issue Triage

When an issue is opened, maintainers apply the MECE label system described in
[`ISSUE_TRIAGE.md`](ISSUE_TRIAGE.md):

- Type labels: `1-bug`, `1-feature`, `1-question`
- Area labels: `2-module:*`, `2-infra:*`, `2-doc:*`, `2-meta:*`
- Priority labels: `3-P0`, `3-P1`, `3-P2`
- Status labels: `4-ready`, `4-in-progress`, `4-needs-science`, `4-needs-deps`

Assignees are set only when someone is expected to drive the issue. For module
issues that need scientific input, use the relevant `2-module:*` label and
`4-needs-science`, then route the discussion to the panel owner listed in
[`SCIENTIFIC_REVIEWERS.md`](SCIENTIFIC_REVIEWERS.md).

### 2. PR Triage

When a PR is opened, maintainers perform initial triage:

1. Assess whether changes affect model physics, scientific calculations, coding
   style, process, or infrastructure.
2. Determine which labels and ownership areas apply.
3. Use `.github/CODEOWNERS` for governance ownership and
   [`SCIENTIFIC_REVIEWERS.md`](SCIENTIFIC_REVIEWERS.md) for manual scientific
   reviewer suggestions.
4. Document physics impact and scientific rationale in the PR description or PR
   comments, rather than creating one-off labels for every review state.

### 3. Scientific Review Request

For PRs requiring scientific validation:

1. Apply the relevant `2-module:*` label.
2. Request review manually from the domain expert(s) identified by
   [`SCIENTIFIC_REVIEWERS.md`](SCIENTIFIC_REVIEWERS.md).
3. Provide context about what specifically needs review.
4. Track review status in PR comments and GitHub's review system.

Example comment:
```
@yiqing1021 @denisehertwig - This PR modifies 2-module:stebbs temperature calculations.
Please review for scientific validity, particularly the changes to surface temperature iteration in suews_phys_stebbs.f95.
```

### 4. Review Process

Domain experts should:
1. **Verify Physical Validity**
   - Check equations against literature
   - Ensure units are consistent
   - Validate boundary conditions

2. **Assess Implementation**
   - Confirm numerical methods are appropriate
   - Check for stability issues
   - Verify conservation principles

3. **Test Impact**
   - Review test results
   - Consider edge cases
   - Evaluate against benchmarks

4. **Document Findings**
   - Comment on scientific rationale
   - Note any concerns or limitations
   - Suggest improvements if needed

### 5. Approval

When satisfied with scientific validity:
1. Domain expert approves the PR
2. Comment with approval rationale
3. PR can proceed to merge

### 6. Merge Criteria

PRs can be merged when:
- All CI tests pass
- Code review is approved
- Scientific review is approved (if physics changes)
- Documentation is updated
- For physics changes: Scientific rationale is documented in PR
- For benchmark impacts: Changes are assessed and documented

## Guidelines for AI-Assisted Changes

Special attention for PRs using AI tools (e.g., Claude Code):
1. Always require human scientific review
2. Verify physical reasoning, not just code correctness
3. Check for subtle errors in equations or logic
4. Ensure consistency with SUEWS physics

## Label Reference

### Module Labels (Physics - suews_phys_*)
Module labels identify which physics components are affected:

- `2-module:ohm` - Objective Hysteresis Model
- `2-module:anohm` - Analytical OHM
- `2-module:ehc` - Explicit Heat Conduction
- `2-module:estm` - Element Surface Temperature Method
- `2-module:stebbs` - Surface Temperature Energy Balance
- `2-module:lumps` - LUMPS energy balance
- `2-module:narp` - Net All-wave Radiation Parameterization
- `2-module:spartacus` - SPARTACUS radiation
- `2-module:solweig` - Solar and longwave radiation
- `2-module:beers` - Building radiation
- `2-module:evap` - Evaporation processes
- `2-module:waterdist` - Water distribution
- `2-module:bluews` - Building water balance
- `2-module:snow` - Snow processes
- `2-module:atmmoiststab` - Atmospheric stability
- `2-module:resist` - Resistance calculations
- `2-module:rslprof` - RSL profiles
- `2-module:anthro` - Anthropogenic heat
- `2-module:biogenco2` - Biogenic CO2
- `2-module:dailystate` - Daily state updates

### Simplified Label System
Following the simplified labeling approach, PRs use:
- Module labels (as above) to identify affected components
- Scientific reviewer suggestions are routed through
  [`SCIENTIFIC_REVIEWERS.md`](SCIENTIFIC_REVIEWERS.md)
- CODEOWNERS remains limited to governance, workflow, linting, and process
  ownership
- Review status is tracked in PR comments and GitHub's review system
- Physics impacts are documented in PR descriptions rather than labels

## Escalation

For complex or controversial changes:
1. Add comment requesting discussion
2. Schedule review panel discussion at steering dev meeting
3. Document decision rationale
4. Update relevant documentation

## Contributing as a Reviewer

Interested in joining the review panel? Contact @sunt05 or @suegrimmond with:
- Your domain expertise
- SUEWS experience
- GitHub handle
- Availability for reviews
