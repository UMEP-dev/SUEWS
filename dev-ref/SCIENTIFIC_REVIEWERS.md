# Scientific Reviewer Routing

> **Canonical Rules Notice**
>
> Domain-owner sign-off gates and physics evidence requirements are governed by:
> - Rules: [`.claude/rules/physics-change-evidence.md`](../.claude/rules/physics-change-evidence.md), [`.claude/rules/labels.md`](../.claude/rules/labels.md), and [`.claude/rules/autonomous-workflow.md`](../.claude/rules/autonomous-workflow.md)
> - Skills: `/triage-pr` ([`.claude/skills/triage-pr/SKILL.md`](../.claude/skills/triage-pr/SKILL.md)) and `/audit-pr` ([`.claude/skills/audit-pr/SKILL.md`](../.claude/skills/audit-pr/SKILL.md))

This guide is the maintained directory for scientific reviewer routing in SUEWS. SUEWS does not use a GitHub `CODEOWNERS` file for automatic review requests; PR authors and maintainers request domain reviewers manually using the routing below, while `/audit-pr` enforces blocking subsystem-owner gates defined in [`.claude/rules/physics-change-evidence.md`](../.claude/rules/physics-change-evidence.md).

## Blocking Domain-Owner Gates vs Advisory Reviewers

Distinguish between **blocking subsystem owners** on `0-physics:change` PRs and **advisory manual reviewers**:

1. **Blocking Subsystem-Owner Sign-Off (`0-physics:change`)**:
   - **STEBBS** (`src/suews/src/suews_phys_stebbs.f95`, `src/suews/src/suews_type_stebbs.f95`, STEBBS data model, and `test/fixtures/data_test/stebbs_test/**`): **`@yiqing1021` sign-off is blocking** on any `0-physics:change` PR that modifies STEBBS physics or behaviour (a purely mechanical, non-numerical tree-wide refactor may be ruled exempt by the maintainer in the PR's `Domain owner sign-off` line; see `.claude/rules/physics-change-evidence.md`).
   - **Subsystems without a named subsystem owner**: Blocking sign-off on `0-physics:change` PRs falls to the maintainer (`@sunt05`).
2. **Advisory Scientific Reviewers**:
   - All other domain reviewers in the tables below provide **advisory scientific review** unless designated as a blocking subsystem owner in `.claude/rules/physics-change-evidence.md`. Request them manually whenever module-specific physics, equations, or documentation benefit from specialist scrutiny.

## When to Apply `0-physics:change` and Request Scientific Review

Apply `0-physics:change` (never an untiered `physics-change` label) and include a `## Scientific evidence` section in the PR body when a PR:
- Changes physics source (`src/suews/src/suews_phys_*.f95` or `src/suews_bridge/` behind `physics`) with numerical effect.
- Moves or refreshes any vendored reference fixture (`sample_output_2012-*.csv` + `provenance.json`, `sample_output_stebbs.csv`, `benchmark1/*.pkl`, `w16_most.csv`).
- Alters a physics-affecting default or coefficient in `src/supy/data_model/` so an unchanged user configuration produces different outputs.

Also request advisory scientific review (without `0-physics:change` if no code/output moves) when updating scientific documentation or parameter guidance that requires domain judgement. For draft PRs, request early review only when seeking upfront scientific design input; otherwise wait until the PR is ready for review.

## How to Route a Review

1. Apply the relevant `2-module:*` label (and `0-physics:change` if model physics or reference outputs move).
2. Include the `## Scientific evidence` block in the PR body when `0-physics:change` applies (quantities + mechanism, before/after comparison, sign/magnitude justification, refreshed fixtures, and `Domain owner sign-off`).
3. Identify the required subsystem owner (blocking) and smallest relevant advisory reviewer set from the tables below, and request them manually on GitHub.
4. Track sign-off through GitHub PR reviews; `/audit-pr` blocks approval until required `0-physics:change` owner sign-off and `physics-full` CI checks are present.

## Named Module Reviewers

| Module | Label | File paths | Blocking owner (`0-physics:change`) | Advisory manual reviewers |
|--------|-------|------------|-------------------------------------|---------------------------|
| STEBBS | [`2-module:stebbs`](https://github.com/UMEP-dev/SUEWS/labels/2-module%3Astebbs) | `/src/suews/src/suews_phys_stebbs.f95`, `/src/suews/src/suews_type_stebbs.f95`, `test/fixtures/data_test/stebbs_test/**` | `@yiqing1021` | `@denisehertwig` |
| RSL Profiles | [`2-module:rslprof`](https://github.com/UMEP-dev/SUEWS/labels/2-module%3Arslprof) | `/src/suews/src/suews_phys_rslprof.f95` | `@sunt05` (maintainer default) | `@vitorlavor`, `@suegrimmond` |
| SPARTACUS | [`2-module:spartacus`](https://github.com/UMEP-dev/SUEWS/labels/2-module%3Aspartacus) | `/src/suews/src/suews_phys_spartacus.f95`, `/src/suews/src/suews_type_spartacus.f95`, `/src/suews/ext_lib/spartacus-surface/` | `@sunt05` (maintainer default) | `@suegrimmond`, `@yiqing1021`, `@vitorlavor` |
| Biogenic CO2 | [`2-module:biogenco2`](https://github.com/UMEP-dev/SUEWS/labels/2-module%3Abiogenco2) | `/src/suews/src/suews_phys_biogenco2.f95` | `@sunt05` (maintainer default) | `@havum`, `@ljarvi` |
| Snow | [`2-module:snow`](https://github.com/UMEP-dev/SUEWS/labels/2-module%3Asnow) | `/src/suews/src/suews_phys_snow.f95`, `/src/suews/src/suews_type_snow.f95` | `@sunt05` (maintainer default) | `@havum`, `@ljarvi` |
| EHC | [`2-module:ehc`](https://github.com/UMEP-dev/SUEWS/labels/2-module%3Aehc) | `/src/suews/src/suews_phys_ehc.f95`, `/src/suews/src/suews_type_ehc.f95` | `@sunt05` | `@sunt05` |
| AnOHM | [`2-module:anohm`](https://github.com/UMEP-dev/SUEWS/labels/2-module%3Aanohm) | `/src/suews/src/suews_phys_anohm.f95` | `@sunt05` | `@sunt05` |

## Default Scientific Review Coverage

Modules without a separate domain panel route to `@sunt05` (blocking maintainer sign-off on `0-physics:change`) and `@suegrimmond` (advisory scientific review):

| Module | Label | File path | Manual reviewers |
|--------|-------|-----------|------------------|
| Atmospheric stability | [`2-module:atmmoiststab`](https://github.com/UMEP-dev/SUEWS/labels/2-module%3Aatmmoiststab) | `/src/suews/src/suews_phys_atmmoiststab.f95` | `@sunt05`, `@suegrimmond` |
| Evaporation | [`2-module:evap`](https://github.com/UMEP-dev/SUEWS/labels/2-module%3Aevap) | `/src/suews/src/suews_phys_evap.f95` | `@sunt05`, `@suegrimmond` |
| Water distribution | [`2-module:waterdist`](https://github.com/UMEP-dev/SUEWS/labels/2-module%3Awaterdist) | `/src/suews/src/suews_phys_waterdist.f95` | `@sunt05`, `@suegrimmond` |
| Anthropogenic heat | [`2-module:anthro`](https://github.com/UMEP-dev/SUEWS/labels/2-module%3Aanthro) | `/src/suews/src/suews_phys_anthro.f95` | `@sunt05`, `@suegrimmond` |
| OHM | [`2-module:ohm`](https://github.com/UMEP-dev/SUEWS/labels/2-module%3Aohm) | `/src/suews/src/suews_phys_ohm.f95` | `@sunt05`, `@suegrimmond` |
| ESTM (disabled) | [`2-module:estm`](https://github.com/UMEP-dev/SUEWS/labels/2-module%3Aestm) | out of the build since gh#1802; source in git history, see [`ESTM_REACTIVATION.md`](ESTM_REACTIVATION.md) | `@sunt05`, `@suegrimmond` |
| LUMPS | [`2-module:lumps`](https://github.com/UMEP-dev/SUEWS/labels/2-module%3Alumps) | `/src/suews/src/suews_phys_lumps.f95` | `@sunt05`, `@suegrimmond` |
| NARP | [`2-module:narp`](https://github.com/UMEP-dev/SUEWS/labels/2-module%3Anarp) | `/src/suews/src/suews_phys_narp.f95` | `@sunt05`, `@suegrimmond` |
| SOLWEIG | [`2-module:solweig`](https://github.com/UMEP-dev/SUEWS/labels/2-module%3Asolweig) | `/src/suews/src/suews_phys_solweig.f95` | `@sunt05`, `@suegrimmond` |
| BEERS | [`2-module:beers`](https://github.com/UMEP-dev/SUEWS/labels/2-module%3Abeers) | `/src/suews/src/suews_phys_beers.f95` | `@sunt05`, `@suegrimmond` |
| Resistance | [`2-module:resist`](https://github.com/UMEP-dev/SUEWS/labels/2-module%3Aresist) | `/src/suews/src/suews_phys_resist.f95` | `@sunt05`, `@suegrimmond` |
| BLUEWS | [`2-module:bluews`](https://github.com/UMEP-dev/SUEWS/labels/2-module%3Abluews) | `/src/suews/src/suews_phys_bluews.f95` | `@sunt05`, `@suegrimmond` |
| Daily state | [`2-module:dailystate`](https://github.com/UMEP-dev/SUEWS/labels/2-module%3Adailystate) | `/src/suews/src/suews_phys_dailystate.f95` | `@sunt05`, `@suegrimmond` |
| LAI phenology (GDD/SDD) | [`2-module:dailystate`](https://github.com/UMEP-dev/SUEWS/labels/2-module%3Adailystate) | `/src/suews/src/suews_phys_lai.f95` | `@sunt05`, `@suegrimmond` |

For any new `src/suews/src/suews_phys_*.f95` module not listed here, use `@sunt05` and `@suegrimmond` until this guide is updated.

## General Scientific Review

For cross-module scientific PRs or model-wide questions that do not map cleanly to one module, start with `@sunt05` and `@MatthewPaskin`.

## Example Request

```text
@yiqing1021 @denisehertwig - This 0-physics:change PR modifies 2-module:stebbs
temperature calculations in suews_phys_stebbs.f95. Please review the scientific
validity of the surface temperature iteration and the before/after comparison in
## Scientific evidence (@yiqing1021 blocking owner sign-off requested).
```
