---
paths:
  - src/suews/src/suews_phys_*.f95
  - src/suews/ext_lib/**/*
  - test/fixtures/data_test/sample_output_2012-*.csv
  - test/fixtures/data_test/provenance.json
  - test/fixtures/data_test/stebbs_test/**
  - test/fixtures/benchmark1/**
  - test/fixtures/w16_most.csv
---

# Evidence and Fixture Rules for Physics and Numerical Changes

Rules for any pull request that changes model physics or moves a vendored reference output.
Output-moving pull requests fall into two classes: **physics changes** (`0-physics:change`), which alter how a process is formulated or shift outputs at or beyond round-off, and **numerical changes** (`0-numerics:change`), which leave every process formulation intact and move reference outputs only at round-off level within a bounded threshold. [decision: software maintainer, 2026-11-18]
Both classes refresh moved reference fixtures in the same PR and run the full `-m physics` CI tier (including `slow`) before merge; `0-physics:change` additionally requires a `## Scientific evidence` section and domain-owner sign-off. [decision: software maintainer, 2026-11-18]

Motivated originally by the gap exposed around gh#1570 / gh#1575: the kdown direct/diffuse
partition (#1570) was a legitimate physics improvement that shifted STEBBS
building-energy outputs (indoor temperature +~0.5 K, cooling-load peak 28 W vs
19 W), but two things went wrong:

- The stale STEBBS regression fixture only failed in the **nightly full-physics
  build**, not in the PR or merge-queue checks. At the time, the merge queue's
  reduced `standard` matrix excluded every `slow` test, so a known-output-
  changing PR merged without the change being caught.
- There was **no recorded scientific justification** for the new numbers. The
  physical reasoning (longwave cascade -> indoor temperature -> threshold-driven
  cooling) had to be reconstructed after the fact in #1575 to update the
  reference and get owner sign-off.

Separating `0-numerics:change` from `0-physics:change` keeps full scientific review and domain-owner sign-off focused on genuine formulation changes while still requiring round-off constant/precision clean-ups (such as unifying physical constants in `suews_ctrl_const.f95`) to refresh fixtures in the same PR, tabulate the numerical delta, and pass the full physics tier before merge. [decision: software maintainer, 2026-11-18]

---

## Classification: physics change vs numerical change

Vendored reference fixtures governed by this rule:

- `test/fixtures/data_test/sample_output_2012-*.csv` and `test/fixtures/data_test/provenance.json` (main SuPy reference run, twelve monthly shards),
- `test/fixtures/data_test/stebbs_test/sample_output_stebbs.csv` (STEBBS reference run),
- `test/fixtures/benchmark1/*.pkl` (benchmark reference outputs),
- `test/fixtures/w16_most.csv` (W16 atmospheric-stability reference run). [fact: git grep -n "w16_most" .github/workflows/build-publish_to_pypi.yml test/physics/test_stability_guards.py]

### Physics change (`0-physics:change`)

A PR is a **physics change** -- and MUST carry `0-physics:change` -- when it does any of: [decision: software maintainer, 2026-11-18]

- **Changes how a process is formulated** in `src/suews/src/suews_phys_*.f95`, embedded external library source under `src/suews/ext_lib/**`, or the Rust physics backend under `src/suews_bridge/` behind the `physics` feature -- equations, the form of a parameterisation, how a process is partitioned, scheme dispatch, or calibrated coefficients. [decision: software maintainer, 2026-11-18]
- **Changes a physics-affecting default or coefficient** in `src/supy/data_model/` such that an unchanged user configuration produces different numbers (for example a changed default albedo, emissivity, or scheme selection). [decision: software maintainer, 2026-11-18]
- **Moves any vendored reference output at or beyond the numerical threshold** below, regardless of how small the source edit looks. [decision: software maintainer, 2026-11-18]

### Numerical change (`0-numerics:change`)

A PR is a **numerical change** -- and MUST carry `0-numerics:change` (unless elevated to `0-physics:change`) -- when ALL of the following hold: [decision: software maintainer, 2026-11-18]

- **No process formulation changes.** Equations, parameterisation forms, process partitioning, and calibrated coefficients are untouched.
- **Only numerical representation changes:** constant precision, unit conversions, or floating-point representation (for example unifying the Stefan-Boltzmann constant, double-precision `pi`, or `C2K = 273.15D0`), or a reference-lane build-environment update (pinned container or compiler bump) with unchanged physics source.
- **Reference outputs move only at round-off level**, strictly within the numerical threshold below across every vendored reference fixture.

### Numerical threshold and excluded diagnostics

A change qualifies as `0-numerics:change` only when, over all vendored reference outputs, every non-excluded variable satisfies **both**: [decision: software maintainer, 2026-11-18]

- **maximum relative change < 0.1%** (`1e-3`), AND
- **maximum absolute flux difference < 0.5 W m-2** (for energy and radiation fluxes in `W m-2`).

Diagnostics that legitimately diverge because their denominator approaches zero are excluded from the threshold test: [decision: software maintainer, 2026-11-18]

- **Obukhov length** (`Lob` in the `SUEWS` output group, `L_MOD_RSL` in `RSL`), which diverges (`-> +-infinity`) as sensible heat flux approaches zero near neutral stratification.
- **Surface resistance** (`RS` in `SUEWS` and `debug`, `RSS_*` in `debug`), which approaches its closed-conductance cap (`10000 s m-1`) when stomatal or surface conductance approaches zero at night or on dry/frozen surfaces.
- **Aerodynamic and boundary-layer resistances** (`RA` in `SUEWS` and `debug`, `RAsnow` and `RB` in `debug`) as wind and friction velocity approach zero (`UStar -> 0`).

Excluded diagnostics that move must still be refreshed in the fixture and listed in the PR's change table (marked as excluded from the threshold). [procedure: Principle 1]
If any non-excluded variable reaches or exceeds `0.1%` relative change or `0.5 W m-2` absolute flux difference, the PR is a **physics change** (`0-physics:change`) and requires the full scientific-evidence and sign-off gate. [decision: software maintainer, 2026-11-18]

### Neither class (no label required)

A PR is neither `0-physics:change` nor `0-numerics:change` when it:

- Only refactors physics code with a proven bit-for-bit identical output (state this explicitly; `test_sample_output_exact.py` and the `slow` physics regressions passing unchanged are the proof).
- Touches utility or control Fortran (`suews_util_*`, `suews_ctrl_*`) with no numerical effect on any output.
- Adds an `Optional` data-model field with a default that does not alter any existing run (see `python/schema-versioning.md` for the parallel schema test).
- Edits docs, comments, CI, or tests that do not regenerate a reference fixture.

---

## Requirements by class

### What a `0-physics:change` PR MUST carry

#### 1. A "Scientific evidence" section in the PR body

The PR description must include a section (heading `## Scientific evidence`)
covering:

- **Which physical quantities change** and through which mechanism (the physical
  chain, e.g. "kdown partition -> longwave cascade -> indoor air temperature ->
  threshold-driven cooling load").
- **A before/after comparison** of the affected outputs -- a figure or a small
  table of the moved values (old vs new), not just "outputs changed".
- **The expected sign and magnitude**, with the physical reasoning for why the
  new direction and size are correct (not merely different).

A copy-paste skeleton:

```markdown
## Scientific evidence

**Quantities changed:** <variable(s), units>
**Mechanism:** <physical chain from the code change to the output shift>

| Output | Before | After | Expected? |
|--------|--------|-------|-----------|
| <var>  | <old>  | <new> | <sign/magnitude reasoning> |

**Reference fixtures refreshed in this PR:** <paths, or "none expected">
**Domain owner sign-off:** <@handle> (<subsystem>)
```

#### 2. Domain-owner sign-off

When a `0-physics:change` PR touches an owned subsystem, the owner must approve before merge.
Current owned subsystems:

- **STEBBS** (`suews_phys_stebbs.f95`, STEBBS data model, STEBBS fixtures) ->
  the STEBBS owner.

Who holds each role is recorded in `MAINTAINERS.md` and nowhere else.

Owner sign-off is for changes to a subsystem's own physics or behaviour. A
cross-cutting maintenance fix that touches an owned file only as one of many
(for example marking never-assigned locals `PARAMETER` across the Fortran tree,
with no numerical or functional change to that subsystem) does not need the
owner's approval; the maintainer records that ruling in the PR's
`Domain owner sign-off` line instead (see #1803).

For subsystems without a named owner, sign-off falls to the software maintainer.
Who to ask for a scientific view is listed in `MAINTAINERS.md`; how to request
one is in `dev-ref/REVIEW_PROCESS.md`. The `audit-pr` gate records the required sign-off
and blocks approval until it is present.

#### 3. Reference-fixture refresh in the same PR

Any reference fixture the change moves must be refreshed in the **same PR** (or a
PR explicitly linked from it). A change and its fixture update must not
drift across separate, unlinked PRs -- that is the exact failure #1575 had to
repair after the fact.

A refresh of the main SuPy reference must also carry the regenerated
`test/fixtures/data_test/provenance.json` sidecar, which records the SuPy build,
git commit, compiler and platform that produced the numbers, and the SHA-256 of
each shard. Run `scripts/suews/gen_sample_output.py`, which writes both; do not
hand-edit the shards. The shards are written at seven significant figures
(`float_format="%.7g"`), the precision justified against the tightest test
tolerance in `test/fixtures/data_test/sample_output_io.py`, so a refresh
produced any other way will not match the committed reference's shape.
`test_reference_provenance_matches_shards` fails when the sidecar and the shards
disagree, so a refresh without it does not merge.

The main reference is generated in the CI build that checks it exactly (see
"Exact reference gate" below). Take a refresh from that build's
`reference-regenerated-cp312-manylinux-x86_64` artefact, or run the generator
inside the same `manylinux2014` image; a refresh made on any other machine
fails the exact check in CI.

#### 4. Full `-m physics` CI tier before merge

The `slow` physics regression tests must run as a required check before merge so output shifts surface in the PR and merge queue rather than in the nightly (gh#1576; see "CI gate" and "What `physics-full` adds over `standard`" below).

### What a `0-numerics:change` PR MUST carry

A `0-numerics:change` PR requires **no** `## Scientific evidence` section and **no** domain-owner sign-off. [decision: software maintainer, 2026-11-18]
Instead, it MUST carry: [decision: software maintainer, 2026-11-18]

1. **The `0-numerics:change` label** (and not `0-physics:change`).
2. **Reference-fixture refresh in the same PR** -- identical provenance and CI reference-lane artefact rules as item 3 above (`sample_output_2012-*.csv` + `provenance.json`, `sample_output_stebbs.csv`, `benchmark1/*.pkl`, `w16_most.csv` as applicable).
3. **A short numerical change table in the PR body** stating what constant, unit conversion, or floating-point representation changed, confirming no process formulation changed, and reporting per moved variable the maximum absolute and maximum relative difference against the previous reference (noting any excluded divergent diagnostic):

   ```markdown
   ## Numerical change

   **Change:** <constant precision / unit conversion / representation, no formulation change>
   **Reference fixtures refreshed in this PR:** <paths>

   | Variable | Max abs diff | Max rel diff (%) | Within threshold? |
   |----------|--------------|------------------|-------------------|
   | <var>    | <val>        | <pct>            | yes (<0.1%, <0.5 W m-2) / excluded (<reason>) |
   ```

4. **Full `-m physics` CI tier (`physics-full`) green before merge** -- CI runs the exact same `physics-full` tier as on `0-physics:change` PRs to confirm across the suite that the change is numerical only.

---

## The `0-physics:change` and `0-numerics:change` labels

Two mutually exclusive classification labels in the `0-` automation namespace: [decision: software maintainer, 2026-11-18]

- `0-physics:change`: formulation change or reference shift at/above the numerical threshold; activates all four requirements (`## Scientific evidence`, domain-owner sign-off, fixture refresh, `physics-full` CI tier).
- `0-numerics:change`: constant precision, unit conversion, or floating-point representation only, with all non-excluded reference shifts below `0.1%` relative and `0.5 W m-2` absolute flux difference; activates fixture refresh, numerical change table, and `physics-full` CI tier without `## Scientific evidence` or domain-owner sign-off.
- If a PR combines a formulation change with a constant clean-up, or if any non-excluded variable meets or exceeds the threshold, label the PR `0-physics:change` (or split the PR per `work-sizing.md`). [procedure: Principle 3]
- Applied at PR triage (`triage-pr`) or by `audit-pr` when the diff matches the triggers above.
- A maintainer creates each label once; the autonomous tier applies labels that already exist and never creates labels (`autonomous-workflow.md`).
- Removing or downgrading a label (`0-physics:change` -> `0-numerics:change` or none) is a maintainer action and must state the verified reason (for example "refactor proven bit-identical" or "constant precision only; max flux delta 0.08 W m-2, max rel 0.09%"). [procedure: Principle 2]

There is **no cosmetic bypass label** for reference-moving changes (unlike `0-ci:schema-audit-ok`): both labels require refreshing moved fixtures in the same PR and passing `physics-full`.
A physics diff that genuinely moves no output needs neither label once `test_sample_output_exact.py` and the `slow` physics regressions pass unchanged.

---

## PR review gate (automated via audit-pr skill)

When reviewing a PR that touches physics source, physics-affecting defaults, or vendored reference fixtures: [procedure: Principle 1, 2]

- Classify the diff against the triggers above (`0-physics:change`, `0-numerics:change`, or bit-identical refactor).
- For a **physics change** (`0-physics:change`):
  - Confirm `0-physics:change` is present (propose/flag if missing; flag as `[blocking]` if `0-numerics:change` was applied to a formulation change or an above-threshold shift).
  - Confirm `## Scientific evidence` covers quantities + mechanism, before/after comparison, and expected sign/magnitude reasoning (a bare "outputs changed, fixture updated" is `[blocking]`).
  - Confirm domain-owner sign-off is present for any owned subsystem (`MAINTAINERS.md`) -> `[blocking]` until resolved.
  - Confirm all moved reference fixtures (and `provenance.json`) are refreshed in this PR (or an explicitly linked PR) and `physics-full` is green.
- For a **numerical change** (`0-numerics:change`):
  - Verify from the diff that no process formulation, equation, parameterisation form, partitioning, or calibrated coefficient changed.
  - Confirm the PR body includes the per-variable change table (max absolute and max relative difference) and that every non-excluded variable is strictly below `0.1%` relative and `0.5 W m-2` absolute flux difference (`Lob`, `L_MOD_RSL`, `RS`, `RSS_*`, `RA`, `RAsnow`, `RB` may be excluded where their denominator approaches zero, if noted).
  - Confirm all moved reference fixtures (and `provenance.json`) are refreshed in this PR and `physics-full` is green.
  - Do NOT require `## Scientific evidence` or domain-owner sign-off.

## CI gate (gh#1576)

`.github/scripts/determine-matrix.sh` selects `test_tier=standard` for both
`pull_request` (ready) and `merge_group`. Standard retains `core` + `slow`
regressions while excluding non-core `slow` tests. Only `schedule` (nightly)
and tag/full-dispatch use `test_tier=all`.

When a PR carries `0-physics:change` or `0-numerics:change`, `determine_matrix` sets `PHYSICS_CHANGE=true` and `determine-matrix.sh` selects `test_tier=physics-full` in both ready-PR and `merge_group` contexts (and widens `merge_group` to `PR_PLATFORMS`), so the `slow` physics tests run as a required check before merge. [fact: git grep -n "0-numerics:change" .github/workflows/build-publish_to_pypi.yml && git grep -n "TIER=physics-full" .github/scripts/determine-matrix.sh]
The physics axis then runs `-m physics` (`.github/actions/build-suews/action.yml`); the api axis is unchanged.

## Exact reference gate

The tolerance test (`test_sample_output_validation_full_year`) is sized for
cross-platform noise, so it cannot see a change like #1848 (273.16 -> 273.15 K),
which moved QS by up to 0.08 W m-2 and was merged without a refresh or the
label. `test/core/test_sample_output_exact.py` closes that gap:

- On the build that generated the reference (system, machine, C library,
  Fortran compiler and build profile as recorded in `provenance.json`), the
  full-year sample run is written at `%.7g` and must equal the committed shards
  byte for byte. Any moved value fails, listing the columns, their largest
  change and how to refresh.
- CI's cp312 manylinux x86_64 release lane sets `SUEWS_REFERENCE_LANE=1`. There
  the check always runs, and a reference generated elsewhere is a failure, so a
  refresh made on another machine cannot switch the gate off. Elsewhere the
  check skips with the reason when the build does not match.
- A change to that lane's build environment (a cibuildwheel bump that moves
  the pinned `manylinux2014` image, or a different compiler in it) therefore
  fails the check until the reference is refreshed from the lane's
  `reference-regenerated` artefact, even when no physics changed. Such a refresh carries `0-numerics:change` (with the per-variable change table noting the build-environment bump) when all shifts sit within the numerical threshold, or `0-physics:change` if any shift reaches or exceeds it. [decision: software maintainer, 2026-11-18]
- On a mismatch the lane uploads the regenerated shards and sidecar as the
  `reference-regenerated-cp312-manylinux-x86_64` artefact: the refresh for an
  intended change.
- It is `core` and `medium`, so it runs on draft PRs as well as ready PRs and
  the merge queue.
- The `Physics-change label` job (`check_physics_label`) fails a PR (through `pr-gate`) that changes a stored reference (the sample-output shards or `provenance.json`, the STEBBS and benchmark fixtures, `w16_most.csv`) without `0-physics:change` or `0-numerics:change`. [fact: git grep -n "0-numerics:change" .github/workflows/build-publish_to_pypi.yml]
  It warns, without failing, when `suews_phys_*.f95` changes without either label: a bit-identical refactor legitimately carries none, and one that moves the output already fails the exact check.
  Labels are read from the API, so labelling the PR and re-running `check_physics_label` clears the label job (while `determine_matrix` on `pull_request` reads the event payload, so push a commit or close/reopen to trigger `physics-full` if the label was added after the PR opened).

Together: an output-moving change cannot merge without a refresh, and a refresh cannot merge without `0-physics:change` or `0-numerics:change`, both of which run `physics-full` while `audit-pr` and human review enforce the class-specific body and sign-off requirements.

## What `physics-full` adds over `standard`

Today: nothing. The physics axis of `standard` runs
`-m "physics and (core or not slow)"` and `physics-full` runs `-m physics`, so
the only tests the label can add are those matching
`physics and slow and not core`, and that expression collects zero nodes: on
9 September 2026 (master at `ce29700498`) both tiers collected the same 178
physics nodes, because the two `slow` physics regressions
(`test/core/test_sample_output.py::TestSampleOutput::test_sample_output_validation_full_year`
and `::TestSTEBBSOutput::test_stebbs_building_energy_outputs`) are also
`core` and already run in `standard`. The label's present value is the
evidence and sign-off contract above, not extra test coverage.

A collection check keeps this paragraph honest.
`test/core/test_physics_tier_delta.py::test_physics_full_tier_adds_exactly_what_the_rule_records`
(a `core` test, so `standard` and every fuller tier and `make test` run it;
logic in `scripts/lint/check_physics_tier_delta.py`) collects
`physics and slow and not core` with pytest itself and compares the node ids
with the list recorded below, failing on a difference in either direction: an
unrecorded test (it would run only in `physics-full` and in the nightly, and
nobody said so) or a recorded pattern that matches nothing (the rule would
claim coverage the tier no longer adds). When you add a `slow` physics test
that is not `core`, either mark it `core` too so `standard` keeps running it,
or record its node id below (one per line; `*` and `?` wildcards, so
`path::test_name[*]` covers a parametrised test) and rewrite the paragraph
above. When a recorded test gains `core` or goes away, remove it. The check
cannot run in the "Check pytest marker axis" job, which has no built supy to
collect with.

<!-- physics-full-only nodes: begin -->
```text
(none)
```
<!-- physics-full-only nodes: end -->

## Related

- `.claude/rules/python/schema-versioning.md` -- the parallel gate for data-model shape changes (its "do not bump" list mirrors the "Neither class" list here).
- `.claude/rules/ci/conventions.md` -- CI workflow conventions for the matrix and required-check wiring.
- `.claude/rules/autonomous-workflow.md` -- the `0-` automation label namespace and gating model that `0-physics:change` and `0-numerics:change` join.
- gh#1570, gh#1575 -- the motivating case.
