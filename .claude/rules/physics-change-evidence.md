---
paths:
  - src/suews/src/suews_phys_*.f95
  - test/fixtures/data_test/sample_output.csv.gz
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
  build**, not in the PR or merge-queue checks. The merge queue runs a reduced
  matrix (`test_tier=standard`) whose pytest expression excludes `slow`, so a
  known-output-changing PR merged without the change being caught.
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
  - `test/fixtures/data_test/sample_output.csv.gz` (main SuPy reference run),
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
(`@sunt05`). The enforcement mechanism (a CODEOWNERS mapping with branch
protection vs. the manual `audit-pr` gate below) is being decided in gh#1576; in
the interim, `audit-pr` records the required sign-off and blocks approval until
it is present.

### 3. The reference-fixture refresh travels with the change

Any reference fixture the change moves must be refreshed in the **same PR** (or a
PR explicitly linked from it). A physics change and its fixture update must not
drift across separate, unlinked PRs -- that is the exact failure #1575 had to
repair after the fact.

### 4. The full `-m physics` tier runs before merge

The `slow` physics regression tests must run as a required check on a
`0-physics:change` PR, so output shifts surface in the PR/merge-queue rather than
in the nightly. The CI wiring for this is tracked as the second PR of gh#1576
(see "CI gate" below); until it lands, `audit-pr` requires a manual full-tier run
(`pytest -m physics`) and a link to its result.

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
- Confirm the full `-m physics` tier (including `slow`) has run green -- via the
  required check once the CI wiring lands, or via a linked manual run in the
  interim.

## CI gate (the second PR of gh#1576)

`.github/scripts/determine-matrix.sh` currently selects `test_tier=standard` for
both `pull_request` (ready) and `merge_group`, and `standard` excludes `slow`.
Only `schedule` (nightly) and tag/full-dispatch use `test_tier=all`, which is why
the stale STEBBS fixture only failed overnight.

The CI wiring bumps the tier to include the `slow` physics tests when a PR
carries `0-physics:change`, making the full physics regression a required check
before merge. This is implemented in the follow-up CI PR; this rule documents the
contract the wiring must satisfy.

## Related

- `.claude/rules/python/schema-versioning.md` -- the parallel gate for
  data-model shape changes (its "do not bump" list mirrors the "NOT
  physics-changing" list here).
- `.claude/rules/ci/conventions.md` -- CI workflow conventions for the matrix
  and required-check wiring.
- `.claude/rules/autonomous-workflow.md` -- the `0-` automation label namespace
  and gating model that `0-physics:change` joins.
- gh#1570, gh#1575 -- the motivating case.
