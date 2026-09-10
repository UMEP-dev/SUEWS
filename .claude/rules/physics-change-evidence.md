---
paths:
  - src/suews/src/suews_phys_*.f95
  - test/fixtures/data_test/sample_output_2012-*.csv
  - test/fixtures/data_test/provenance.json
  - test/fixtures/data_test/stebbs_test/**
  - test/fixtures/benchmark1/**
---

# Recorded Scientific Evidence for Physics-Changing PRs

Rules for any pull request that changes model physics or moves a reference
output. Such a PR must carry recorded scientific evidence that the new numbers
are correct, obtain domain-owner sign-off for the subsystem it touches, and run
the full `-m physics` test tier (including `slow`) before merge -- not after, in
the nightly.

Motivated by the gap exposed around gh#1570 / gh#1575: the kdown direct/diffuse
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

We already refresh reference outputs when physics changes. This rule adds the
three things that were missing: *evidence that the new output is scientifically
correct*, a *domain-owner sign-off*, and a guarantee that *the relevant physics
tests actually run before merge*.

---

## What counts as a physics-changing PR

A PR is physics-changing -- and MUST be labelled `0-physics:change` (see below)
-- when it does any of:

- **Touches physics source.** Any `src/suews/src/suews_phys_*.f95`, or the Rust
  physics backend under `src/suews_bridge/` behind the `physics` feature.
- **Moves a reference output.** Any change that alters a vendored reference
  fixture:
  - `test/fixtures/data_test/sample_output_2012-*.csv` (main SuPy reference
    run, twelve monthly shards),
  - `test/fixtures/data_test/stebbs_test/sample_output_stebbs.csv` (STEBBS
    reference run),
  - `test/fixtures/benchmark1/*.pkl` (benchmark reference outputs).
- **Changes a physics-affecting default or coefficient** in the data model
  (`src/supy/data_model/`) such that an unchanged user config produces different
  numbers (e.g. a changed default albedo, emissivity, or scheme selection).

It is NOT physics-changing when it:

- Only refactors physics code with a proven bit-for-bit identical output (state
  this explicitly; the `slow` physics tier passing unchanged is the proof).
- Touches utility/control Fortran (`suews_util_*`, `suews_ctrl_*`) with no
  numerical effect.
- Adds an `Optional` data-model field with a default that does not alter any
  existing run (see `python/schema-versioning.md` for the parallel schema test).
- Edits docs, comments, CI, or tests that do not regenerate a reference fixture.

When unsure, check whether the `slow` physics tier still reproduces every
vendored fixture after the change. If any fixture needs a refresh, the PR is
physics-changing.

---

## What a physics-changing PR MUST carry

### 1. A "Scientific evidence" section in the PR body

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

### 2. Domain-owner sign-off

When the change touches an owned subsystem, the owner must approve before merge.
Current owned subsystems:

- **STEBBS** (`suews_phys_stebbs.f95`, STEBBS data model, STEBBS fixtures) ->
  `@yiqing1021`.

For subsystems without a named owner, sign-off falls to the maintainer
(`@sunt05`). Scientific reviewer routing is documented in
`dev-ref/SCIENTIFIC_REVIEWERS.md` so domain reviewers are requested manually
when science review is needed. The `audit-pr` gate records the required sign-off
and blocks approval until it is present.

### 3. The reference-fixture refresh travels with the change

Any reference fixture the change moves must be refreshed in the **same PR** (or a
PR explicitly linked from it). A physics change and its fixture update must not
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

### 4. The full `-m physics` tier runs before merge

The `slow` physics regression tests must run as a required check on a
`0-physics:change` PR, so output shifts surface in the PR/merge-queue rather than
in the nightly. The CI wiring landed with gh#1576 (see "CI gate" below); read
"What `physics-full` adds over `standard`" for what the tier currently delivers.

---

## The `0-physics:change` label

A repo label in the `0-` automation namespace marking that a PR changes model
physics or moves a reference output. It is the switch that activates this rule's
three requirements (evidence section, owner sign-off, full physics CI tier).

- Applied at PR triage (`triage-pr`) or by `audit-pr` when the diff matches the
  "What counts as a physics-changing PR" triggers above.
- A maintainer creates the label once; the autonomous tier applies it if it
  exists and never creates labels (consistent with the `0-` namespace policy in
  `autonomous-workflow.md`).
- Removing the label is a maintainer action and should be accompanied by a stated
  reason (e.g. "refactor proven bit-identical; no output moved").

There is deliberately **no cosmetic bypass label** for this gate (unlike
`0-ci:schema-audit-ok`): the gate is satisfied by *providing the evidence*, not
by waiving it. A physics diff that genuinely moves no output satisfies the gate
by saying so in the evidence section and by the `slow` physics tier reproducing
every fixture unchanged.

---

## PR review gate (automated via audit-pr skill)

When reviewing a PR whose diff matches the physics-change triggers:

- Confirm the PR carries the `0-physics:change` label; if missing, apply it (or
  flag for a maintainer) before continuing.
- Confirm the PR body has a `## Scientific evidence` section with the three
  required parts (quantities + mechanism, before/after, sign/magnitude
  reasoning). A bare "outputs changed, fixture updated" is not sufficient -> flag
  as blocking.
- If the change touches an owned subsystem, confirm the owner's sign-off is
  present (or request it) -> blocking until resolved.
- Confirm any moved reference fixture is refreshed in this PR (or a linked PR).
- Confirm the full `-m physics` tier (including `slow`) has run green: the
  `physics-full` wheel-build checks on a labelled PR.

## CI gate (gh#1576)

`.github/scripts/determine-matrix.sh` selects `test_tier=standard` for both
`pull_request` (ready) and `merge_group`. Standard retains `core` + `slow`
regressions while excluding non-core `slow` tests. Only `schedule` (nightly)
and tag/full-dispatch use `test_tier=all`.

When a PR carries `0-physics:change`, the same script selects
`test_tier=physics-full` instead, in both contexts, so the `slow` physics tests
run as a required check before merge. The physics axis then runs `-m physics`
(`.github/actions/build-suews/action.yml`); the api axis is unchanged.

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

- `.claude/rules/python/schema-versioning.md` -- the parallel gate for
  data-model shape changes (its "do not bump" list mirrors the "NOT
  physics-changing" list here).
- `.claude/rules/ci/conventions.md` -- CI workflow conventions for the matrix
  and required-check wiring.
- `.claude/rules/autonomous-workflow.md` -- the `0-` automation label namespace
  and gating model that `0-physics:change` joins.
- gh#1570, gh#1575 -- the motivating case.
