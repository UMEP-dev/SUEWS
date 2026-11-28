---
paths:
  - src/suews/src/suews_phys_*.f95
  - src/suews/ext_lib/**/*
  - test/fixtures/data_test/sample_output_2012-*.csv
  - test/fixtures/data_test/provenance.json
  - test/fixtures/data_test/stebbs_test/sample_output_stebbs.csv
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
- `test/fixtures/w16_most.csv` (W16 atmospheric-stability reference run). [fact: git grep -n "w16_most" .github/workflows/build-publish_to_pypi.yml test/physics/test_stability_guards.py]

### Physics change (`0-physics:change`)

A PR is a **physics change** -- and MUST carry `0-physics:change` -- when it does any of: [decision: software maintainer, 2026-11-18]

- **Changes how a process is formulated** in `src/suews/src/suews_phys_*.f95`, embedded external library source under `src/suews/ext_lib/**`, or the Rust physics backend under `src/suews_bridge/` behind the `physics` feature -- equations, the form of a parameterisation, how a process is partitioned, scheme dispatch, or calibrated coefficients. [decision: software maintainer, 2026-11-18]
- **Changes a physics-affecting default or coefficient** in `src/supy/data_model/` such that an unchanged user configuration produces different numbers (for example a changed default albedo, emissivity, or scheme selection). [decision: software maintainer, 2026-11-18]
- **Moves any model output or refreshes any vendored reference fixture without satisfying all conditions of `0-numerics:change` below** -- including any shift at or above the numerical threshold, any change to an identically-zero reference variable, or any output-moving cause outside the three numerical operations (for example a reference-lane build-environment update). [decision: software maintainer, 2026-11-18]

### Numerical change (`0-numerics:change`)

A PR is a **numerical change** -- and MUST carry `0-numerics:change` (unless elevated to `0-physics:change`) -- when ALL of the following hold: [decision: software maintainer, 2026-11-18]

- **No process formulation changes.** Equations, parameterisation forms, process partitioning, scheme dispatch, calibrated coefficients, and physics-affecting defaults are untouched.
- **Only numerical representation changes:** restricted to **constant precision, unit conversions, or floating-point representation** (for example unifying the Stefan-Boltzmann constant, double-precision `pi`, or `C2K = 273.15D0`).
- **Reference outputs move only at round-off level**, strictly within the numerical threshold below across every vendored reference fixture.

### Numerical threshold and Obukhov-length diagnostics

Measure each variable against its own scale in each vendored reference fixture (`sample_output_2012-*.csv` combined across its twelve monthly shards, `sample_output_stebbs.csv`, and `w16_most.csv`), not as a pointwise ratio `|new_i - ref_i| / |ref_i|`: pointwise ratios blow up whenever a variable crosses or approaches zero (for example night-time `QN`, surface temperature differences, or cooling loads), turning round-off-sized absolute differences into large apparent percentages. [decision: software maintainer, 2026-11-18]
Across all aligned samples `i` of a variable in a fixture, positions where both `ref_i` and `new_i` are `NaN` are ignored and any `NaN`/finite mismatch fails the threshold; over all remaining non-`NaN` samples `i` of that variable in that fixture: [decision: software maintainer, 2026-11-18]

- **Maximum absolute difference:** `max_i |new_i - ref_i|` (`0` when no non-`NaN` samples remain).
- **Scale-relative change:** `max_i |new_i - ref_i| / max_i |ref_i|` when `max_i |ref_i| > 0`, `0` (`0%`) when `max_i |ref_i| == 0` and `max_i |new_i - ref_i| == 0` (or when no non-`NaN` samples remain), and unbounded (fails `< 0.5%`: a variable whose reference is identically zero fails if it changes at all) when `max_i |ref_i| == 0` and `max_i |new_i - ref_i| > 0`.

In every vendored reference fixture, every variable must satisfy **both**: [decision: software maintainer, 2026-11-18]

- **scale-relative change < 0.5%** (`5e-3`), AND
- **maximum absolute flux difference < 0.5 W m-2** (for energy and radiation fluxes in `W m-2`).

The `< 0.5%` (`5e-3`) scale-relative bound is calibrated on gh#1970, where several small-magnitude STEBBS variables reached `0.11-0.29%` scale-relative change with absolute differences of at most `0.05 W m-2` (still round-off level). [decision: software maintainer, 2026-11-18]

Only the two Obukhov-length diagnostics below are judged through their stability parameter instead of `L` itself: [decision: software maintainer, 2026-11-19]

- **Why `L` is the wrong quantity:** physically `|L| -> infinity` as sensible heat flux `QH -> 0`, while stored values are clamped (`ZL_NEUTRAL = 1e-12` in `stability_scales`, `src/suews/src/suews_phys_atmmoiststab.f95`, and `|Lc / L| <= 2` in `RSL_cal_prms`, `src/suews/src/suews_phys_rslprof.f95`), so round-off near `QH = 0` swings `1 / zL` across large finite magnitudes without physical change; the stability parameter itself stays finite and bounded (`zL` in `[-2.0, 0.5]`), so every non-`NaN` row is evaluated directly. [fact: git grep -n "ZL_NEUTRAL\|ZL_STABLE_MAX\|ZL_UNSTABLE_MIN\|lc_over_L = SIGN" src/suews/src/suews_phys_atmmoiststab.f95 src/suews/src/suews_phys_rslprof.f95]
- **`Lob` (`SUEWS` output group):** judged by `zL` (`(z - d) / L`, stored in `SUEWS`) using the same scale-relative criterion `max_i |dzL| / max_i |zL_ref| < 0.5%` (`5e-3`, where `dzL = zL_new - zL_ref`) across all non-`NaN` rows; any `NaN`/finite mismatch on `Lob` or `zL` fails. [decision: software maintainer, 2026-11-19]
- **`L_MOD_RSL` (`RSL` output group):** `dataoutLineRSL` (`src/suews/src/suews_phys_rslprof.f95`) does not store a stability-parameter column directly -- it outputs `z_1..z_30`, `U_1..U_30`, `T_1..T_30`, `q_1..q_30`, `L_MOD_RSL`, `zH_RSL`, `Lc`, `beta`, `zd_RSL`, `z0_RSL`, `elm`, `Scc`, `f` (`fx` in Fortran), `UStar_RSL`, `UStar_heat`, `TStar_RSL`, `FAI`, `PAI`, and `flag_RSL`. Judge `L_MOD_RSL` by its forcing-height stability parameter `zeta = (z_30 - zd_RSL) / L_MOD_RSL` (matching `(zMeas - zd_RSL) / L_MOD_RSL` in `RSLProfile`, where `z_30` is `zarray(nz)` and `zd_RSL` is the RSL displacement height), or by aligned `zL` from `SUEWS` where `z_30` and `zd_RSL` are absent, using the same `< 0.5%` (`5e-3`) scale-relative criterion (`max_i |dzeta| / max_i |zeta_ref| < 0.5%`); any `NaN`/finite mismatch (or non-finite `zeta`) fails. [fact: git grep -n "dataoutLineRSL =\|(zMeas - zd_RSL)/L_MOD_RSL" src/suews/src/suews_phys_rslprof.f95]
- **Fail closed when no stability parameter is available:** if a fixture stores `Lob` or `L_MOD_RSL` without either `zL` or the columns (`z_30`, `zd_RSL`) needed to form `zeta`, judge `Lob` / `L_MOD_RSL` directly under `< 0.5%` scale-relative change with no exemption. [decision: software maintainer, 2026-11-19]

Resistances (`RA`, `RAsnow`, `RB`, `RS`, `RSS_*`) are guarded by finite wind/friction-velocity floors or conductance caps in SUEWS (`src/suews/src/suews_phys_resist.f95`) and can legitimately move under physics changes, so they remain included across all rows without any exemption. [fact: git grep -n "AVU1_use = MAX\|USTAR_GUARD" src/suews/src/suews_phys_resist.f95]
Moved `Lob` and `L_MOD_RSL` values must still be refreshed across all rows in the fixture and recorded in the PR's numerical change table for information (their own maximum absolute difference and scale-relative change), but they do not decide the class; the `zL` / `zeta` row decides. [procedure: Principle 1]
Any output-moving change that fails any `0-numerics:change` condition -- including a variable (or `zL` / `zeta` for `Lob` / `L_MOD_RSL`) reaching or exceeding `0.5%` scale-relative change, an energy/radiation flux reaching or exceeding `0.5 W m-2` absolute difference, any change to an identically-zero reference variable, any `NaN`/finite mismatch, or an output shift from another cause -- is a **physics change** (`0-physics:change`) and requires the full scientific-evidence and sign-off gate. [decision: software maintainer, 2026-11-18]

### Neither class (no label required)

A PR is neither `0-physics:change` nor `0-numerics:change` when it:

- Only refactors physics code with a proven bit-for-bit identical output (state this explicitly; `test_sample_output_exact.py` and the `slow` physics regressions passing unchanged are the proof).
- Touches utility or control Fortran (`suews_util_*`, `suews_ctrl_*`) with no numerical effect on any output.
- Adds an `Optional` data-model field with a default that does not alter any existing run (see `python/schema-versioning.md` for the parallel schema test).
- Edits docs, comments, CI, or tests that do not regenerate a reference fixture.

---

## Requirements by class

### What a `0-physics:change` PR MUST carry

#### 1. A "Scientific evidence" section in the PR body (and supporting comments)

`## Scientific evidence` sits inside the standard pull request structure defined by `.claude/rules/pr-description.md`. [decision: software maintainer, 2026-11-19]
Present scientific evidence at a depth proportional to the change's physical reach: [decision: software maintainer, 2026-11-19]

- **Level 1 -- narrow or single-module fix** (one routine, local initialisation, or a bounded regime): put the complete evidence directly in `## Scientific evidence` inside the PR body. [decision: software maintainer, 2026-11-19]
- **Level 2 -- formulation, parameterisation, or multi-site / cross-module change**: keep `## Scientific evidence` in the PR body as an executive scientific brief readable in two to three minutes, and place large per-site tables, derivations, and diagnostic figures in `<details>` blocks or structured, linked PR comments. [decision: software maintainer, 2026-11-19]

In both levels, `## Scientific evidence` must cover: [decision: software maintainer, 2026-11-19]

- **Quantities changed and physical mechanism:** the affected variables (with units), the physical causal chain (for example "kdown partition -> longwave cascade -> indoor air temperature -> threshold-driven cooling load"), and the literature reference or equation when introducing or changing a formulation.
- **Before/after comparison with provenance:** a compact table (and figures for Level 2 where they clarify the claim) comparing old and new values between a stated **baseline commit SHA** and **PR head commit SHA**, with physical reasoning for why the new sign and magnitude are expected -- honestly reporting regressions and runtime/iteration costs alongside improvements. [procedure: Principle 1]
- **Conservation, physical bounds, and known limits:** confirm energy and water budget closure (`Rn = QH + QE + QS + QF`, water balance) and stability/convergence behaviour where affected, and state regimes the change does not solve or leaves to follow-up issues. [procedure: Principle 1]
- **Reference-fixture impact summary:** which vendored fixtures moved, summary magnitude (for example share of moved rows and maximum flux/temperature shifts), and how they were regenerated, reminding reviewers that regenerated CSV diffs need not be read line by line.
- **Domain-owner sign-off and review ask:** the required role from `MAINTAINERS.md` and the specific scientific question the reviewer should verify. [procedure: Principle 2]
- **Evidence index (Level 2 only):** a short list of links to supporting PR comments (figures, per-site tables, method notes), stating what each comment covers and which commit SHA it reflects. [procedure: Principle 1]

##### Diagnostic figures standard

When a physics PR includes diagnostic figures: [decision: software maintainer, 2026-11-19]

- **Do not commit evaluation figures into the PR's git branch.**
  Host diagnostic PNG/PDF figures on the public repository's `gh-artifacts` orphan branch (`pr/<PR>/...`, embedded via `raw.githubusercontent.com/UMEP-dev/SUEWS/gh-artifacts/...`) or as GitHub web attachments, so evaluation plots do not bloat git history or clutter the code diff. [decision: software maintainer, 2026-11-19]
- **Respect observational data licensing.**
  Never plot restricted or non-redistributable observational time series publicly; plot model outputs, public forcing, or derived summary metrics (for example RMSE change or bias difference against observations) and note that restriction explicitly above the figures. [decision: software maintainer, 2026-11-19]
- **Provide a self-contained caption on every figure.**
  Give every embedded image meaningful alt text and a two-to-four-sentence caption naming the site(s) or dataset, time period, relevant model switches/configuration, baseline and PR commit SHAs, and the physical takeaway the plot demonstrates. [procedure: Principle 1]
- **Include only plots that answer the physical claim.**
  Prefer: (a) regime or functional response curves (for example resistance against wind speed before/after), (b) representative or worst-case episode time series showing key states, fluxes, and forcing together, (c) multi-site or multi-configuration distributions (such as per-site RMSE change or iteration-limit counts) showing both gains and regressions, or (d) conservation/residual diagnostics. [decision: software maintainer, 2026-11-19]

##### Revision provenance and superseding

Every evidence table and figure comment must state the baseline commit SHA and PR head commit SHA (plus build environment where relevant). [procedure: Principle 1]
If a subsequent commit alters the physics outputs, do not leave earlier figures or tables looking current: update the headline numbers in `## Scientific evidence` to match the new head, add a short `**Changed on <date>:**` note explaining what shifted and why, and mark superseded evidence comments clearly or link the refreshed comment from the evidence index. [procedure: Principle 1]

##### Copy-paste skeleton

```markdown
## Scientific evidence

**Baseline vs head:** `<base-sha>` vs `<head-sha>` (<build/environment if relevant>)
**Quantities changed:** <variable(s), units>
**Mechanism:** <physical chain from the code change to the output shift; literature ref/equation if formulation changed>

| Output / metric | Before | After | Expected? |
|-----------------|--------|-------|-----------|
| <var (unit)>    | <old>  | <new> | <sign/magnitude reasoning; include any regressions/costs> |

**Conservation, costs, and known limits:** <energy/water closure, stability/runtime impact, regimes not addressed>
**Reference fixtures refreshed in this PR:** <paths, summary shifts, regeneration source, or "none expected">
**Domain owner sign-off:** <role per MAINTAINERS.md> (<subsystem>) -- <specific scientific check requested>

<!-- Level 2 only -- omit for narrow Level 1 fixes -->
**Supporting comments (at `<head-sha>`):**
- [Evidence figures](<comment-url>): <what each figure shows>
- [Per-site / multi-config tables](<comment-url>): <scope and summary>
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
2. **Reference-fixture refresh in the same PR** -- identical provenance and CI reference-lane artefact rules as item 3 above (`sample_output_2012-*.csv` + `provenance.json`, `sample_output_stebbs.csv`, `w16_most.csv` as applicable).
3. **A short numerical change table in the PR body** stating what constant, unit conversion, or floating-point representation changed, confirming no process formulation changed, and reporting per moved variable the maximum absolute difference and scale-relative change (`max_i |new_i - ref_i| / max_i |ref_i|`) against the previous reference (for `Lob` / `L_MOD_RSL`, recording their own maximum absolute difference and scale-relative change for information while the `zL` / `zeta` row decides the class):

   ```markdown
   ## Numerical change

   **Change:** <constant precision / unit conversion / representation, no formulation change>
   **Reference fixtures refreshed in this PR:** <paths>

   | Variable   | Max abs diff | Scale-rel change (%) | Within threshold? |
   |------------|--------------|----------------------|-------------------|
   | <var>      | <val>        | <pct>                | yes (<0.5%, <0.5 W m-2 for fluxes) |
   | Lob (info) | <val>        | <pct>                | judged by zL row |
   | zL         | <val>        | <pct>                | yes (<0.5%) |
   ```

4. **Full `-m physics` CI tier (`physics-full`) green before merge** -- CI runs the exact same `physics-full` tier as on `0-physics:change` PRs to confirm across the suite that the change is numerical only.

---

## The `0-physics:change` and `0-numerics:change` labels

Two mutually exclusive classification labels in the `0-` automation namespace: [decision: software maintainer, 2026-11-18]

- `0-physics:change`: formulation change, physics-affecting default change, or any output-moving / fixture-refreshing change that fails any `0-numerics:change` condition (including shifts at/above the numerical threshold or build-environment updates); activates all four requirements (`## Scientific evidence`, domain-owner sign-off, fixture refresh, `physics-full` CI tier).
- `0-numerics:change`: constant precision, unit conversion, or floating-point representation only, with all reference shifts below `0.5%` scale-relative change (with `Lob` / `L_MOD_RSL` judged by `zL` / `zeta`) and `0.5 W m-2` absolute flux difference; activates fixture refresh, numerical change table, and `physics-full` CI tier without `## Scientific evidence` or domain-owner sign-off.
- If a PR combines a formulation change with a constant clean-up, or if any variable meets or exceeds the threshold, label the PR `0-physics:change` (or split the PR per `work-sizing.md`). [procedure: Principle 3]
- Applied at PR triage (`triage-pr`) only when neither classification label is already present on the PR, or proposed/flagged by `audit-pr` when the diff matches the triggers above; on any conflict (including when an existing label disagrees with the diff or both labels are present), automation preserves the existing label(s) untouched and flags the proposed change for a maintainer to decide and record. [procedure: Principle 2]
- A maintainer creates each label once; the autonomous tier applies labels that already exist and never creates labels (`autonomous-workflow.md`).
- Removing, replacing, or downgrading an existing classification label (`0-physics:change` -> `0-numerics:change` or none) is always a maintainer action and must state the verified reason (for example "refactor proven bit-identical" or "constant precision only; max flux delta 0.08 W m-2, max scale-rel 0.09%"). [procedure: Principle 2]

There is **no cosmetic bypass label** for reference-moving changes (unlike `0-ci:schema-audit-ok`): both labels require refreshing moved fixtures in the same PR and passing `physics-full`.
Only a verified formulation-preserving refactor with bit-for-bit identical output (see "Neither class" above; `test_sample_output_exact.py` and the `slow` physics regressions passing unchanged) carries neither label; a formulation change in an unexercised scheme still requires `0-physics:change`. [decision: software maintainer, 2026-11-18]

---

## PR review gate (automated via audit-pr skill)

When reviewing a PR that touches physics source, physics-affecting defaults, or vendored reference fixtures: [procedure: Principle 1, 2]

- Classify the diff against the triggers above (`0-physics:change`, `0-numerics:change`, or verified formulation-preserving bit-identical refactor).
- For a **physics change** (`0-physics:change`):
  - Confirm `0-physics:change` is present alone (propose/flag if missing; flag as `[blocking]` if `0-numerics:change` was applied to a formulation, build-environment, or above-threshold change, or if both labels are present).
  - Confirm `## Scientific evidence` covers quantities + mechanism, before/after comparison with baseline and PR head SHAs, expected sign/magnitude reasoning (including any regressions, conservation/costs, and known limits), and fixture refresh summary (a bare "outputs changed, fixture updated" is `[blocking]`). [decision: software maintainer, 2026-11-19]
  - When diagnostic figures or linked evidence comments are attached, confirm figures are not committed to the code branch, carry self-contained captions with commit provenance, respect observational data-licensing guards, and have no silently stale superseded numbers (`[major]` if missing provenance/captions or stale; `[blocking]` if restricted observational time series are exposed). [decision: software maintainer, 2026-11-19]
  - Confirm domain-owner sign-off is present for any owned subsystem (`MAINTAINERS.md`) -> `[blocking]` until resolved.
  - Confirm all moved reference fixtures (and `provenance.json`) are refreshed in this PR (or an explicitly linked PR) and `physics-full` is green.
- For a **numerical change** (`0-numerics:change`):
  - Verify from the diff that no process formulation, equation, parameterisation form, partitioning, scheme dispatch, calibrated coefficient, or physics-affecting default changed, and that the change is restricted to constant precision, unit conversions, or floating-point representation.
  - Confirm the PR body includes the per-variable change table (maximum absolute difference and scale-relative change `max_i |new_i - ref_i| / max_i |ref_i|`, failing if an identically-zero reference variable changes at all or on any `NaN`/finite mismatch) and that every variable is strictly below `0.5%` scale-relative change and `0.5 W m-2` absolute flux difference across all non-`NaN` rows (`Lob` is judged by `zL` and `L_MOD_RSL` by `zeta = (z_30 - zd_RSL) / L_MOD_RSL`, or aligned `zL` where absent, using the same `< 0.5%` scale-relative criterion, failing closed to `L` directly if neither is available, with `Lob` / `L_MOD_RSL` still refreshed and reported for information; resistances such as `RA`, `RAsnow`, `RB`, `RS`, and `RSS_*` carry no exemption).
  - Confirm all moved reference fixtures (and `provenance.json`) are refreshed in this PR and `physics-full` is green.
  - Do NOT require `## Scientific evidence` or domain-owner sign-off.

## CI gate (gh#1576)

General event, lane, and merge-queue selection under `GATE_AXIS_SCHEMA=2`
(including which triggers select `physics-core`, `physics-full`, or `all`, and
the Windows merge-queue `smoke` physics split) is defined once in
`.claude/rules/ci/conventions.md` ("Multi-Axis CI Topology", "Test Tier
Assignment by Event", and "Merge Queue Coverage Policy").

For `0-physics:change` and `0-numerics:change`, `determine_matrix` sets
`physics_change=true` (forcing `build_wheels` and requiring its green check in
`pr-gate` even if no build-filtered code paths changed) and
`.github/scripts/determine-matrix.sh` forces `test_tier=physics-full` on both
`pull_request` and `merge_group` (keeping `MERGE_QUEUE_PLATFORMS` on
`merge_group`), so the full Linux `-m physics` suite (including `slow`) gates
the change before merge; non-physics axes (`tooling`, `unit`, `native`,
`os_compat`) stay on their ready `(core or not slow) and not qgis` expressions. [fact: git grep -n "physics_change:" .github/workflows/build-publish_to_pypi.yml && git grep -n "TIER=physics-full" .github/scripts/determine-matrix.sh && git grep -n '"physics-full": f"{axis}' scripts/suews/ci_tiers.py]

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
- The `Physics-change label` job (`check_physics_label`) fails a `pull_request` or single-PR `merge_group` run (through `pr-gate`, which also requires `build_wheels == success` whenever either label is present) when stored reference outputs (`sample_output_2012-*.csv`, `provenance.json`, `stebbs_test/sample_output_stebbs.csv`, `w16_most.csv`) change without **exactly one** of `0-physics:change` or `0-numerics:change`, when a PR carries both labels simultaneously, or when a labelled ready PR or `merge_group` run did not execute `test_tier=physics-full`. [fact: git grep -n "0-numerics:change" .github/workflows/build-publish_to_pypi.yml && git grep -n "PHYSICS_CHANGE" .github/scripts/pr-gate.sh]
  It warns, without failing, when `suews_phys_*.f95` changes without either label: a verified formulation-preserving refactor with bit-identical output legitimately carries none, and one that moves reference output already fails the exact check.
  Both `determine_matrix` and `check_physics_label` query live PR labels via the GitHub API; because `build-publish_to_pypi.yml` keeps `master`'s event triggers (no automatic rerun on label events), adding or changing a classification label after `determine_matrix` has started requires clicking "Re-run all jobs" (or pushing a commit / closing and reopening the PR) so `determine_matrix` and `build_wheels` execute `physics-full`. [procedure: Principle 1]
  On `merge_group`, `gh-readonly-queue/<base>/pr-<n>-<sha>` identifies a single-PR diff when `<sha>` equals `merge_group.base_sha` (all single-entry `ALLGREEN` runs); multi-PR or unparsed queue refs keep `master`'s safe default of running `physics-full` and rely on the required `pull_request` `PR build validation` gate for per-PR label attribution (pre-merge squash queue commits have no `/commits/{sha}/pulls` mapping). [fact: git grep -n "ref_base_sha" .github/workflows/build-publish_to_pypi.yml]

Together: an output-moving change cannot merge without a refresh, and a refresh cannot merge without carrying **exactly one** of `0-physics:change` or `0-numerics:change` on a green `physics-full` run while `audit-pr` and human review enforce the class-specific body and sign-off requirements.

## What `physics-full` adds over `standard`

Today: the continuation matrix. Comparing those two tier definitions directly,
the physics axis of `standard` runs `-m "physics and (core or not slow)"` and
`physics-full` runs `-m physics`, so the delta between `standard` and
`physics-full` is `physics and slow and not core`. [fact: git grep -n 'STANDARD_EXPR = "physics and (core or not slow)"' scripts/lint/check_physics_tier_delta.py && git grep -n 'DELTA_EXPR = "physics and slow and not core"' scripts/lint/check_physics_tier_delta.py]
(On a Python-only pull request starting at `physics-core`, adding a
classification label additionally brings in the non-smoke `physics` tests
between `physics-core` and `standard`; see `.claude/rules/ci/conventions.md`,
"Multi-Axis CI Topology" and "Test Tier Assignment by Event", for full event
selection.) The `physics and slow and not core` set is the module
`test/physics/test_continuation_matrix.py`: the parametrised
`test_continuation_matrix` (chunked, checkpoint-resumed and streamed
runs against an uninterrupted run, crossed with the default, leap-day,
snow, SPARTACUS, STEBBS, dual-timescale EHC, DyOHM, DyOHM-building,
irrigation and CO2 modes) and its
negative control `test_checkpoint_perturbation_is_detected`. It is `slow` and
not `core` on purpose: #2100 asks for this restart verification to stay out of
`standard` and `physics-core`, so it runs whenever CI selects full `-m physics`
and not on Python-only PRs that remain at `physics-core` (see
`.claude/rules/ci/conventions.md`, "Multi-Axis CI Topology"). [decision: software maintainer, 2026-11-22; aligned with GATE_AXIS_SCHEMA=2 decision, 2026-11-26] The two `slow`
physics regressions
(`test/core/test_sample_output.py::TestSampleOutput::test_sample_output_validation_full_year`
and `::TestSTEBBSOutput::test_stebbs_building_energy_outputs`) are also `core`
and run in `standard`. Before this module (9 September 2026, master at
`ce29700498`) the two tiers collected the same 178 physics nodes.

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
test/physics/test_continuation_matrix.py::test_continuation_matrix[*]
test/physics/test_continuation_matrix.py::test_checkpoint_perturbation_is_detected
```
<!-- physics-full-only nodes: end -->

## Related

- `.claude/rules/pr-description.md` -- unified PR description structure, writing principles, and `## How to review` / `## Verification` rules that `## Scientific evidence` and `## Numerical change` slot into.
- `.claude/rules/python/schema-versioning.md` -- the parallel gate for data-model shape changes (its "do not bump" list mirrors the "Neither class" list here).
- `.claude/rules/ci/conventions.md` -- CI workflow conventions for the matrix and required-check wiring.
- `.claude/rules/autonomous-workflow.md` -- the `0-` automation label namespace and gating model that `0-physics:change` and `0-numerics:change` join.
- gh#1570, gh#1575 -- the motivating case; gh#1956 -- Level 2 presentation example.
