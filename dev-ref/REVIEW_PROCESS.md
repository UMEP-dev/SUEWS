# SUEWS Scientific Review Process

## Overview

This document outlines the scientific review process for SUEWS pull requests to ensure changes maintain physical validity and scientific correctness.

## Review Panel

The SUEWS review panel consists of domain experts responsible for validating changes based on specific modules:

### Current Panel Members

| Module | Label | Reviewers |
|--------|-------|-----------|
| STEBBS | [`module:stebbs`](https://github.com/UMEP-dev/SUEWS/labels/module%3Astebbs) | @yiqing1021, @denisehertwig |
| RSL Profiles | [`module:rslprof`](https://github.com/UMEP-dev/SUEWS/labels/module%3Arslprof) | @vitorlavor, @suegrimmond |
| SPARTACUS | [`module:spartacus`](https://github.com/UMEP-dev/SUEWS/labels/module%3Aspartacus) | @suegrimmond, @yiqing1021 |
| Biogenic CO2 | [`module:biogenco2`](https://github.com/UMEP-dev/SUEWS/labels/module%3Abiogenco2) | @havum, @ljarvi |
| Snow | [`module:snow`](https://github.com/UMEP-dev/SUEWS/labels/module%3Asnow) | @havum, @ljarvi |
| EHC | [`module:ehc`](https://github.com/UMEP-dev/SUEWS/labels/module%3Aehc) | @sunt05 |
| AnOHM | [`module:anohm`](https://github.com/UMEP-dev/SUEWS/labels/module%3Aanohm) | @sunt05 |
| Overall | General PRs | @sunt05, @MatthewPaskin, @dayantur |

### Modules Seeking Reviewers

- Atmospheric stability ([`module:atmmoiststab`](https://github.com/UMEP-dev/SUEWS/labels/module%3Aatmmoiststab))
- Evaporation ([`module:evap`](https://github.com/UMEP-dev/SUEWS/labels/module%3Aevap))
- Water distribution ([`module:waterdist`](https://github.com/UMEP-dev/SUEWS/labels/module%3Awaterdist))
- Anthropogenic heat ([`module:anthro`](https://github.com/UMEP-dev/SUEWS/labels/module%3Aanthro))
- OHM ([`module:ohm`](https://github.com/UMEP-dev/SUEWS/labels/module%3Aohm))
- ESTM ([`module:estm`](https://github.com/UMEP-dev/SUEWS/labels/module%3Aestm))
- LUMPS ([`module:lumps`](https://github.com/UMEP-dev/SUEWS/labels/module%3Alumps))
- NARP ([`module:narp`](https://github.com/UMEP-dev/SUEWS/labels/module%3Anarp))
- SOLWEIG ([`module:solweig`](https://github.com/UMEP-dev/SUEWS/labels/module%3Asolweig))
- BEERS ([`module:beers`](https://github.com/UMEP-dev/SUEWS/labels/module%3Abeers))
- Resistance ([`module:resist`](https://github.com/UMEP-dev/SUEWS/labels/module%3Aresist))
- BLUEWS ([`module:bluews`](https://github.com/UMEP-dev/SUEWS/labels/module%3Abluews))
- Daily state ([`module:dailystate`](https://github.com/UMEP-dev/SUEWS/labels/module%3Adailystate))

## Review Workflow

### 1. Initial Triage

When a PR is opened, maintainers perform initial triage:
- Assess if changes affect model physics or scientific calculations
- Determine which modules are impacted
- Apply appropriate labels:
  - Module labels (e.g., `module:stebbs`, `module:rslprof`, `module:biogenco2`)
  - Note: Physics impact is documented in PR description instead of labels

### 2. Scientific Review Request

For PRs requiring scientific validation:
1. Tag relevant domain expert(s) based on module labels
2. Provide context about what specifically needs review
3. Track review status in PR comments

Example comment:
```
@yiqing1021 @denisehertwig - This PR modifies module:stebbs temperature calculations. 
Please review for scientific validity, particularly the changes to surface temperature iteration in suews_phys_stebbs.f95.
```

### 3. Review Process

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

### 4. Approval

When satisfied with scientific validity:
1. Domain expert approves the PR
2. Comment with approval rationale
3. PR can proceed to merge

### 5. Merge Criteria

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
Module labels remain unchanged and are used to identify which physics components are affected:
- `module:ohm` - Objective Hysteresis Model
- `module:anohm` - Analytical OHM
- `module:ehc` - Explicit Heat Conduction
- `module:estm` - Element Surface Temperature Method
- `module:stebbs` - Surface Temperature Energy Balance
- `module:lumps` - LUMPS energy balance
- `module:narp` - Net All-wave Radiation Parameterization
- `module:spartacus` - SPARTACUS radiation
- `module:solweig` - Solar and longwave radiation
- `module:beers` - Building radiation
- `module:evap` - Evaporation processes
- `module:waterdist` - Water distribution
- `module:bluews` - Building water balance
- `module:snow` - Snow processes
- `module:atmmoiststab` - Atmospheric stability
- `module:resist` - Resistance calculations
- `module:rslprof` - RSL profiles
- `module:anthro` - Anthropogenic heat
- `module:biogenco2` - Biogenic CO2
- `module:dailystate` - Daily state updates

### Simplified Label System
Following the simplified labeling approach, PRs use:
- Module labels (as above) to identify affected components
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