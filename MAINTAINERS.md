# Maintainers

The one place where roles in this repository are tied to people.
Every other file names the role and points here; it does not name the person.
When someone takes over a role, this is the only file that changes.

## Maintainers

Two maintainers hold the highest authority, with different remits.

- **Science maintainer**: Sue Grimmond (`@suegrimmond`).
  Final decision on scientific questions: model physics, parameterisations, and what is scientifically acceptable.
- **Software maintainer**: Ting Sun (`@sunt05`).
  Final decision on software, architecture, process and agent instructions, and the day-to-day maintenance of the repository.

Day-to-day sign-off and merging are the software maintainer's, including for the science of modules that have no named owner.
The science maintainer has the final say when a scientific question is in doubt or disputed.
"A maintainer" in any other file means the software maintainer unless the question is one of those.

## Module owners and reviewers

One list for who answers for the science of each module.
A change to a module's own physics or behaviour needs sign-off before it is merged (`.claude/rules/physics-change-evidence.md`).
The people under "Also ask" are requested by hand when a change would benefit from their scientific view; their review is advisory and never blocks a merge.
When and how to request a review is in `dev-ref/REVIEW_PROCESS.md`.

| Module | Label | File paths | Sign-off required from | Also ask |
|--------|-------|------------|------------------------|----------|
| STEBBS | [`2-module:stebbs`](https://github.com/UMEP-dev/SUEWS/labels/2-module%3Astebbs) | `/src/suews/src/suews_phys_stebbs.f95` | `@yiqing1021` | `@denisehertwig` |
| RSL Profiles | [`2-module:rslprof`](https://github.com/UMEP-dev/SUEWS/labels/2-module%3Arslprof) | `/src/suews/src/suews_phys_rslprof.f95` | the software maintainer | `@vitorlavor`, `@suegrimmond` |
| SPARTACUS | [`2-module:spartacus`](https://github.com/UMEP-dev/SUEWS/labels/2-module%3Aspartacus) | `/src/suews/src/suews_phys_spartacus.f95`, `/src/suews/src/suews_type_spartacus.f95`, `/src/suews/ext_lib/spartacus-surface/` | the software maintainer | `@suegrimmond`, `@yiqing1021`, `@vitorlavor` |
| Biogenic CO2 | [`2-module:biogenco2`](https://github.com/UMEP-dev/SUEWS/labels/2-module%3Abiogenco2) | `/src/suews/src/suews_phys_biogenco2.f95` | the software maintainer | `@havum`, `@ljarvi` |
| Snow | [`2-module:snow`](https://github.com/UMEP-dev/SUEWS/labels/2-module%3Asnow) | `/src/suews/src/suews_phys_snow.f95` | the software maintainer | `@havum`, `@ljarvi` |
| EHC | [`2-module:ehc`](https://github.com/UMEP-dev/SUEWS/labels/2-module%3Aehc) | `/src/suews/src/suews_phys_ehc.f95` | the software maintainer | - |
| AnOHM | [`2-module:anohm`](https://github.com/UMEP-dev/SUEWS/labels/2-module%3Aanohm) | `/src/suews/src/suews_phys_anohm.f95` | the software maintainer | - |

### Modules with no named owner or reviewer

Sign-off falls to the software maintainer; either maintainer may be asked for a scientific view.
Any new `src/suews/src/suews_phys_*.f95` module not listed in this file is treated the same way.

| Module | Label | File path |
|--------|-------|-----------|
| Atmospheric stability | [`2-module:atmmoiststab`](https://github.com/UMEP-dev/SUEWS/labels/2-module%3Aatmmoiststab) | `/src/suews/src/suews_phys_atmmoiststab.f95` |
| Evaporation | [`2-module:evap`](https://github.com/UMEP-dev/SUEWS/labels/2-module%3Aevap) | `/src/suews/src/suews_phys_evap.f95` |
| Water distribution | [`2-module:waterdist`](https://github.com/UMEP-dev/SUEWS/labels/2-module%3Awaterdist) | `/src/suews/src/suews_phys_waterdist.f95` |
| Anthropogenic heat | [`2-module:anthro`](https://github.com/UMEP-dev/SUEWS/labels/2-module%3Aanthro) | `/src/suews/src/suews_phys_anthro.f95` |
| OHM | [`2-module:ohm`](https://github.com/UMEP-dev/SUEWS/labels/2-module%3Aohm) | `/src/suews/src/suews_phys_ohm.f95` |
| ESTM (disabled) | [`2-module:estm`](https://github.com/UMEP-dev/SUEWS/labels/2-module%3Aestm) | out of the build since gh#1802; source in git history, see [`ESTM_REACTIVATION.md`](ESTM_REACTIVATION.md) |
| LUMPS | [`2-module:lumps`](https://github.com/UMEP-dev/SUEWS/labels/2-module%3Alumps) | `/src/suews/src/suews_phys_lumps.f95` |
| NARP | [`2-module:narp`](https://github.com/UMEP-dev/SUEWS/labels/2-module%3Anarp) | `/src/suews/src/suews_phys_narp.f95` |
| SOLWEIG | [`2-module:solweig`](https://github.com/UMEP-dev/SUEWS/labels/2-module%3Asolweig) | `/src/suews/src/suews_phys_solweig.f95` |
| BEERS | [`2-module:beers`](https://github.com/UMEP-dev/SUEWS/labels/2-module%3Abeers) | `/src/suews/src/suews_phys_beers.f95` |
| Resistance | [`2-module:resist`](https://github.com/UMEP-dev/SUEWS/labels/2-module%3Aresist) | `/src/suews/src/suews_phys_resist.f95` |
| BLUEWS | [`2-module:bluews`](https://github.com/UMEP-dev/SUEWS/labels/2-module%3Abluews) | `/src/suews/src/suews_phys_bluews.f95` |
| Daily state | [`2-module:dailystate`](https://github.com/UMEP-dev/SUEWS/labels/2-module%3Adailystate) | `/src/suews/src/suews_phys_dailystate.f95` |
| LAI phenology (GDD/SDD) | [`2-module:dailystate`](https://github.com/UMEP-dev/SUEWS/labels/2-module%3Adailystate) | `/src/suews/src/suews_phys_lai.f95` |

### Cross-module questions

For a scientific change that spans modules, or a model-wide question, start with the software maintainer and `@MatthewPaskin`.
