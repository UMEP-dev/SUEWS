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
- **Moves any model output or refreshes any vendored reference fixture without satisfying all conditions of `0-numerics:change` below** -- including any shift at or above the numerical threshold, any zero-to-nonzero shift, or any output-moving cause outside the three numerical operations (for example a reference-lane build-environment update). [decision: software maintainer, 2026-11-18]

### Numerical change (`0-numerics:change`)

A PR is a **numerical change** -- and MUST carry `0-numerics:change` (unless elevated to `0-physics:change`) -- when ALL of the following hold: [decision: software maintainer, 2026-11-18]

- **No process formulation changes.** Equations, parameterisation forms, process partitioning, scheme dispatch, calibrated coefficients, and physics-affecting defaults are untouched.
- **Only numerical representation changes:** restricted to **constant precision, unit conversions, or floating-point representation** (for example unifying the Stefan-Boltzmann constant, double-precision `pi`, or `C2K = 273.15D0`).
- **Reference outputs move only at round-off level**, strictly within the numerical threshold below across every vendored reference fixture.

### Numerical threshold and excluded diagnostics

Compute differences pointwise against the previous reference (`ref` vs `new` at each sample; positions where both `ref` and `new` are `NaN` are ignored, and any `NaN`/finite mismatch fails the threshold): [decision: software maintainer, 2026-11-18]

- **Pointwise absolute difference:** `|new - ref|`.
- **Pointwise relative change:** `|new - ref| / |ref|` when `ref != 0`, `0` (`0%`) when `ref == 0` and `new == 0`, and unbounded (fails `< 0.1%`, with no sample dropping or denominator floor) when `ref == 0` and `new != 0`.

Taking the maximum over all compared samples in every vendored reference fixture, every non-excluded variable must satisfy **both**: [decision: software maintainer, 2026-11-18]

- **maximum relative change < 0.1%** (`1e-3`), AND
- **maximum absolute flux difference < 0.5 W m-2** (for energy and radiation fluxes in `W m-2`).

Only the two Obukhov-length diagnostics below may exclude near-neutral samples from the `< 0.1%` relative check: [decision: software maintainer, 2026-11-18]

- **Obukhov length and its direct RSL counterpart** (`Lob` in the `SUEWS` output group and `L_MOD_RSL` in `RSL`): physically `|L| -> infinity` as sensible heat flux `QH -> 0` near neutral stratification, whereas stored values remain finite because `stability_scales` clamps `|(z - d) / L| <= ZL_NEUTRAL` (`1e-12`) via `length = zzd / SIGN(ZL_NEUTRAL, zl)` (`src/suews/src/suews_phys_atmmoiststab.f95`) and `RSL` clamps `|Lc / L|` (`src/suews/src/suews_phys_rslprof.f95`), so sub-threshold round-off near `QH = 0` can flip sign or swing `1 / zL` between large finite magnitudes without any physical change. [fact: git grep -n "ZL_NEUTRAL\|lc_over_L = SIGN" src/suews/src/suews_phys_atmmoiststab.f95 src/suews/src/suews_phys_rslprof.f95]

Exclude only aligned sample rows inside the model's neutral band (`neut_limit = 1e-4` in `src/suews/src/suews_phys_atmmoiststab.f95`) in both runs -- `max(|zL_ref|, |zL_new|) <= 1e-4` wherever aligned `zL` is stored (small `QH` alone never exempts a row, and any row with `|zL| > 1e-4` in `ref` or `new` is never exempted), or for a fixture lacking `zL` a documented equivalent stability parameter `zeta = (z - d) / L` satisfying `max(|zeta_ref|, |zeta_new|) <= 1e-4` (`0` rows excluded if unavailable) -- and apply the ordinary `< 0.1%` maximum relative change threshold to all remaining rows of `Lob` and `L_MOD_RSL`. [procedure: Principle 1]
Resistances (`RA`, `RAsnow`, `RB`, `RS`, `RSS_*`) are guarded by finite wind/friction-velocity floors or conductance caps in SUEWS (`src/suews/src/suews_phys_resist.f95`) and can legitimately move under physics changes, so they remain included across all rows without masking. [fact: git grep -n "AVU1_use = MAX\|USTAR_GUARD" src/suews/src/suews_phys_resist.f95]
Moved `Lob` / `L_MOD_RSL` values must still be refreshed across all rows in the fixture and recorded in the PR's change table with the exact sample mask, the excluded/total sample count (`k / N`), and the maximum absolute and relative difference across the remaining rows. [procedure: Principle 1]
Any output-moving change that fails any `0-numerics:change` condition -- including a non-excluded variable reaching or exceeding `0.1%` relative change or `0.5 W m-2` absolute flux difference, a zero-to-nonzero shift, or an output shift from another cause -- is a **physics change** (`0-physics:change`) and requires the full scientific-evidence and sign-off gate. [decision: software maintainer, 2026-11-18]

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
3. **A short numerical change table in the PR body** stating what constant, unit conversion, or floating-point representation changed, confirming no process formulation changed, and reporting per moved variable the maximum absolute and maximum relative difference against the previous reference (for `Lob` / `L_MOD_RSL`, recording the reproducible near-neutral sample mask, excluded/total row count `k / N`, and maximum differences on the remaining rows):

   ```markdown
   ## Numerical change

   **Change:** <constant precision / unit conversion / representation, no formulation change>
   **Reference fixtures refreshed in this PR:** <paths>

   | Variable | Max abs diff | Max rel diff (%) | Excluded rows (mask) | Within threshold? |
   |----------|--------------|------------------|----------------------|-------------------|
   | <var>    | <val>        | <pct>            | 0 / <N> (none) or <k> / <N> (<mask>) | yes (<0.1%, <0.5 W m-2 on remaining rows) |
   ```

4. **Full `-m physics` CI tier (`physics-full`) green before merge** -- CI runs the exact same `physics-full` tier as on `0-physics:change` PRs to confirm across the suite that the change is numerical only.

---

## The `0-physics:change` and `0-numerics:change` labels

Two mutually exclusive classification labels in the `0-` automation namespace: [decision: software maintainer, 2026-11-18]

- `0-physics:change`: formulation change, physics-affecting default change, or any output-moving / fixture-refreshing change that fails any `0-numerics:change` condition (including shifts at/above the numerical threshold or build-environment updates); activates all four requirements (`## Scientific evidence`, domain-owner sign-off, fixture refresh, `physics-full` CI tier).
- `0-numerics:change`: constant precision, unit conversion, or floating-point representation only, with all non-excluded reference shifts below `0.1%` relative and `0.5 W m-2` absolute flux difference; activates fixture refresh, numerical change table, and `physics-full` CI tier without `## Scientific evidence` or domain-owner sign-off.
- If a PR combines a formulation change with a constant clean-up, or if any non-excluded variable meets or exceeds the threshold, label the PR `0-physics:change` (or split the PR per `work-sizing.md`). [procedure: Principle 3]
- Applied at PR triage (`triage-pr`) only when neither classification label is already present on the PR, or proposed/flagged by `audit-pr` when the diff matches the triggers above; on any conflict (including when an existing label disagrees with the diff or both labels are present), automation preserves the existing label(s) untouched and flags the proposed change for a maintainer to decide and record. [procedure: Principle 2]
- A maintainer creates each label once; the autonomous tier applies labels that already exist and never creates labels (`autonomous-workflow.md`).
- Removing, replacing, or downgrading an existing classification label (`0-physics:change` -> `0-numerics:change` or none) is always a maintainer action and must state the verified reason (for example "refactor proven bit-identical" or "constant precision only; max flux delta 0.08 W m-2, max rel 0.09%"). [procedure: Principle 2]

There is **no cosmetic bypass label** for reference-moving changes (unlike `0-ci:schema-audit-ok`): both labels require refreshing moved fixtures in the same PR and passing `physics-full`.
Only a verified formulation-preserving refactor with bit-for-bit identical output (see "Neither class" above; `test_sample_output_exact.py` and the `slow` physics regressions passing unchanged) carries neither label; a formulation change in an unexercised scheme still requires `0-physics:change`. [decision: software maintainer, 2026-11-18]

---

## PR review gate (automated via audit-pr skill)

When reviewing a PR that touches physics source, physics-affecting defaults, or vendored reference fixtures: [procedure: Principle 1, 2]

- Classify the diff against the triggers above (`0-physics:change`, `0-numerics:change`, or verified formulation-preserving bit-identical refactor).
- For a **physics change** (`0-physics:change`):
  - Confirm `0-physics:change` is present alone (propose/flag if missing; flag as `[blocking]` if `0-numerics:change` was applied to a formulation, build-environment, or above-threshold change, or if both labels are present).
  - Confirm `## Scientific evidence` covers quantities + mechanism, before/after comparison, and expected sign/magnitude reasoning (a bare "outputs changed, fixture updated" is `[blocking]`).
  - Confirm domain-owner sign-off is present for any owned subsystem (`MAINTAINERS.md`) -> `[blocking]` until resolved.
  - Confirm all moved reference fixtures (and `provenance.json`) are refreshed in this PR (or an explicitly linked PR) and `physics-full` is green.
- For a **numerical change** (`0-numerics:change`):
  - Verify from the diff that no process formulation, equation, parameterisation form, partitioning, scheme dispatch, calibrated coefficient, or physics-affecting default changed, and that the change is restricted to constant precision, unit conversions, or floating-point representation.
  - Confirm the PR body includes the per-variable change table (max absolute and max relative difference using the pointwise zero-reference rule above) and that every variable is strictly below `0.1%` relative and `0.5 W m-2` absolute flux difference across all non-masked rows (only `Lob` and `L_MOD_RSL` may apply the reproducible near-neutral mask above, with the mask and excluded `k / N` count recorded and `< 0.1%` verified on remaining rows; resistances such as `RA`, `RB`, and `RS` are never masked).
  - Confirm all moved reference fixtures (and `provenance.json`) are refreshed in this PR and `physics-full` is green.
  - Do NOT require `## Scientific evidence` or domain-owner sign-off.

## CI gate (gh#1576)

`.github/scripts/determine-matrix.sh` selects `test_tier=standard` for both
`pull_request` (ready) and `merge_group`. Standard retains `core` + `slow`
regressions while excluding non-core `slow` tests. Only `schedule` (nightly)
and tag/full-dispatch use `test_tier=all`.

When a PR carries `0-physics:change` or `0-numerics:change`, `determine_matrix` sets `physics_change=true` (requiring the `build_wheels` job and its green check in `pr-gate` even if no build-filtered code paths changed) and `determine-matrix.sh` selects `test_tier=physics-full` in both ready-PR and `merge_group` contexts (widening `merge_group` to `PR_PLATFORMS`), so the `slow` physics tests run as a required check before merge. [fact: git grep -n "physics_change:" .github/workflows/build-publish_to_pypi.yml && git grep -n "TIER=physics-full" .github/scripts/determine-matrix.sh]
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
  `reference-regenerated` artefact, even when no physics changed. Such a refresh
  still carries `0-physics:change`, with a `## Scientific evidence` section saying the stored
  values moved only through the build environment.
- On a mismatch the lane uploads the regenerated shards and sidecar as the
  `reference-regenerated-cp312-manylinux-x86_64` artefact: the refresh for an
  intended change.
- It is `core` and `medium`, so it runs on draft PRs as well as ready PRs and
  the merge queue.
- The `Physics-change label` job (`check_physics_label`) fails a `pull_request` or single-PR `merge_group` run (through `pr-gate`, which also requires `build_wheels == success` whenever either label is present) when stored references (`sample_output_2012-*.csv`, `provenance.json`, `stebbs_test/**`, `benchmark1/**`, `w16_most.csv`) change without **exactly one** of `0-physics:change` or `0-numerics:change`, when a PR carries both labels simultaneously, or when a labelled ready PR or `merge_group` run did not execute `test_tier=physics-full`. [fact: git grep -n "0-numerics:change" .github/workflows/build-publish_to_pypi.yml && git grep -n "PHYSICS_CHANGE" .github/scripts/pr-gate.sh]
  It warns, without failing, when `suews_phys_*.f95` changes without either label: a verified formulation-preserving refactor with bit-identical output legitimately carries none, and one that moves reference output already fails the exact check.
  Both `determine_matrix` and `check_physics_label` query live PR labels via the GitHub API; because `build-publish_to_pypi.yml` keeps `master`'s event triggers (no automatic rerun on label events), adding or changing a classification label after `determine_matrix` has started requires clicking "Re-run all jobs" (or pushing a commit / closing and reopening the PR) so `determine_matrix` and `build_wheels` execute `physics-full`. [procedure: Principle 1]
  On `merge_group`, `gh-readonly-queue/<base>/pr-<n>-<sha>` identifies a single-PR diff when `<sha>` equals `merge_group.base_sha` (all single-entry `ALLGREEN` runs); multi-PR or unparsed queue refs keep `master`'s safe default of running `physics-full` and rely on the required `pull_request` `PR build validation` gate for per-PR label attribution (pre-merge squash queue commits have no `/commits/{sha}/pulls` mapping). [fact: git grep -n "ref_base_sha" .github/workflows/build-publish_to_pypi.yml]

Together: an output-moving change cannot merge without a refresh, and a refresh cannot merge without carrying **exactly one** of `0-physics:change` or `0-numerics:change` on a green `physics-full` run while `audit-pr` and human review enforce the class-specific body and sign-off requirements.

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
