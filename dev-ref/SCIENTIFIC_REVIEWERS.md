# Scientific Reviewer Routing

This guide is the maintained source for scientific reviewer suggestions in
SUEWS. Scientific reviewer routing is intentionally kept out of
`.github/CODEOWNERS` so GitHub does not automatically request domain reviewers
when a PR is marked ready. PR authors and maintainers should request these
reviews manually when a science or module-specific review is needed.

## When to Request Scientific Review

Request scientific review when a PR:

- changes model physics, equations, parameterisations, or scientific constants
- changes module-specific Fortran types, data-model defaults, or benchmark
  behaviour with physical meaning
- refreshes reference fixtures or benchmark outputs because the model behaviour
  changed
- updates module documentation in a way that needs domain judgement

For draft PRs, request early advice only when the reviewer is being asked for
design input. Otherwise wait until the PR is ready enough for a focused review.

## How to Route a Review

1. Apply the relevant `2-module:*` label.
2. Use the changed files and the tables below to identify the smallest relevant
   reviewer group.
3. Request those reviewers manually in GitHub and include the review scope in
   the PR description or a short comment.
4. Track scientific sign-off through GitHub reviews or PR comments.

Do not rely on `.github/CODEOWNERS` for scientific reviewer requests. CODEOWNERS
is reserved for governance, workflow, linting, and process ownership.

## Named Module Reviewers

| Module | Label | File paths | Manual reviewers |
|--------|-------|------------|------------------|
| STEBBS | [`2-module:stebbs`](https://github.com/UMEP-dev/SUEWS/labels/2-module%3Astebbs) | `/src/suews/src/suews_phys_stebbs.f95` | @yiqing1021, @denisehertwig |
| RSL Profiles | [`2-module:rslprof`](https://github.com/UMEP-dev/SUEWS/labels/2-module%3Arslprof) | `/src/suews/src/suews_phys_rslprof.f95` | @vitorlavor, @suegrimmond |
| SPARTACUS | [`2-module:spartacus`](https://github.com/UMEP-dev/SUEWS/labels/2-module%3Aspartacus) | `/src/suews/src/suews_phys_spartacus.f95`, `/src/suews/src/suews_type_spartacus.f95`, `/src/suews/ext_lib/spartacus-surface/` | @suegrimmond, @yiqing1021, @vitorlavor |
| Biogenic CO2 | [`2-module:biogenco2`](https://github.com/UMEP-dev/SUEWS/labels/2-module%3Abiogenco2) | `/src/suews/src/suews_phys_biogenco2.f95` | @havum, @ljarvi |
| Snow | [`2-module:snow`](https://github.com/UMEP-dev/SUEWS/labels/2-module%3Asnow) | `/src/suews/src/suews_phys_snow.f95` | @havum, @ljarvi |
| EHC | [`2-module:ehc`](https://github.com/UMEP-dev/SUEWS/labels/2-module%3Aehc) | `/src/suews/src/suews_phys_ehc.f95` | @sunt05 |
| AnOHM | [`2-module:anohm`](https://github.com/UMEP-dev/SUEWS/labels/2-module%3Aanohm) | `/src/suews/src/suews_phys_anohm.f95` | @sunt05 |

## Default Scientific Review Coverage

Modules without a named domain panel are routed manually to @sunt05 and
@suegrimmond until the panel appoints module-specific reviewers.

| Module | Label | File path | Manual reviewers |
|--------|-------|-----------|------------------|
| Atmospheric stability | [`2-module:atmmoiststab`](https://github.com/UMEP-dev/SUEWS/labels/2-module%3Aatmmoiststab) | `/src/suews/src/suews_phys_atmmoiststab.f95` | @sunt05, @suegrimmond |
| Evaporation | [`2-module:evap`](https://github.com/UMEP-dev/SUEWS/labels/2-module%3Aevap) | `/src/suews/src/suews_phys_evap.f95` | @sunt05, @suegrimmond |
| Water distribution | [`2-module:waterdist`](https://github.com/UMEP-dev/SUEWS/labels/2-module%3Awaterdist) | `/src/suews/src/suews_phys_waterdist.f95` | @sunt05, @suegrimmond |
| Anthropogenic heat | [`2-module:anthro`](https://github.com/UMEP-dev/SUEWS/labels/2-module%3Aanthro) | `/src/suews/src/suews_phys_anthro.f95` | @sunt05, @suegrimmond |
| OHM | [`2-module:ohm`](https://github.com/UMEP-dev/SUEWS/labels/2-module%3Aohm) | `/src/suews/src/suews_phys_ohm.f95` | @sunt05, @suegrimmond |
| ESTM | [`2-module:estm`](https://github.com/UMEP-dev/SUEWS/labels/2-module%3Aestm) | `/src/suews/src/suews_phys_estm.f95` | @sunt05, @suegrimmond |
| LUMPS | [`2-module:lumps`](https://github.com/UMEP-dev/SUEWS/labels/2-module%3Alumps) | `/src/suews/src/suews_phys_lumps.f95` | @sunt05, @suegrimmond |
| NARP | [`2-module:narp`](https://github.com/UMEP-dev/SUEWS/labels/2-module%3Anarp) | `/src/suews/src/suews_phys_narp.f95` | @sunt05, @suegrimmond |
| SOLWEIG | [`2-module:solweig`](https://github.com/UMEP-dev/SUEWS/labels/2-module%3Asolweig) | `/src/suews/src/suews_phys_solweig.f95` | @sunt05, @suegrimmond |
| BEERS | [`2-module:beers`](https://github.com/UMEP-dev/SUEWS/labels/2-module%3Abeers) | `/src/suews/src/suews_phys_beers.f95` | @sunt05, @suegrimmond |
| Resistance | [`2-module:resist`](https://github.com/UMEP-dev/SUEWS/labels/2-module%3Aresist) | `/src/suews/src/suews_phys_resist.f95` | @sunt05, @suegrimmond |
| BLUEWS | [`2-module:bluews`](https://github.com/UMEP-dev/SUEWS/labels/2-module%3Abluews) | `/src/suews/src/suews_phys_bluews.f95` | @sunt05, @suegrimmond |
| Daily state | [`2-module:dailystate`](https://github.com/UMEP-dev/SUEWS/labels/2-module%3Adailystate) | `/src/suews/src/suews_phys_dailystate.f95` | @sunt05, @suegrimmond |

For any new `src/suews/src/suews_phys_*.f95` module not listed here, use the
default reviewers until this guide is updated.

## General Scientific Review

For cross-module scientific PRs or model-wide questions that do not map cleanly
to one module, start with @sunt05 and @MatthewPaskin.

## Example Request

```text
@yiqing1021 @denisehertwig - This PR modifies 2-module:stebbs temperature
calculations in suews_phys_stebbs.f95. Please review the scientific validity of
the surface temperature iteration and any benchmark impacts noted in the PR
description.
```
