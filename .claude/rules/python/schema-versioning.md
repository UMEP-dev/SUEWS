# YAML Schema Versioning

Rules for bumping `CURRENT_SCHEMA_VERSION` in
`src/supy/data_model/schema/version.py` and for keeping the
`yaml_upgrade` migration path aligned with reality.

Motivated by the gap closed in gh#1304: `CURRENT_SCHEMA_VERSION` stayed
at `2025.12` through multiple breaking changes (#879 STEBBS clean-up,
#1240 `DeepSoilTemperature` rename, #1242 DHW volume-bound removal,
#1261 setpoint split). The lineage scaffolding existed; it just was not
bumped at each release. This rule exists so the gap does not reopen.

---

## When to bump `CURRENT_SCHEMA_VERSION`

Bump whenever a PR touches `src/supy/data_model/` in a way that makes a
previously-valid user YAML no longer round-trip. Triggers:

- **Rename** a public field (e.g. `DeepSoilTemperature` ->
  `AnnualMeanAirTemperature`).
- **Remove** a public field (e.g. `MinimumVolumeOfDHWinUse` dropped in
  #1242).
- **Change the type / shape** of a public field (scalar -> profile dict
  in #1261; `Optional` -> required-with-no-default).
- **Add a required field** without a sensible default (old YAMLs that
  omit it will now fail validation).
- **Restructure a nested section** (split one sub-object into several,
  or merge siblings).
- **Tighten an enum / literal** so a value that parsed before is now
  rejected.

Do NOT bump for:

- Adding an `Optional` field with a default value.
- Adding an output variable under `src/supy/data_model/output/` (those
  are derived artefacts, not user input).
- Validator rule tightening that rejects scientifically wrong but
  structurally valid YAMLs (the shape is unchanged; only accepted
  ranges narrow).
- Internal refactors (renaming private helpers, reorganising imports)
  that leave the YAML surface identical.

When unsure, check whether the vendored release fixtures under
`test/fixtures/release_configs/` still round-trip through the current
validator after the change. If any of them needs a new handler, you
need to bump.

---

## Dev-label convention during a release cycle

Between formal releases we use PEP 440 `.devN` suffixes rather than
inflating the CalVer number once per PR. This keeps the "target
release" label stable throughout development while still satisfying
the CI lint (which only checks that `CURRENT_SCHEMA_VERSION` moves
*at all* when `src/supy/data_model/` is touched).

**Naming rule:**

- First structural PR after a formal release bumps to
  `<next-target>.dev1` (e.g. immediately after shipping `2026.4`, the
  first breaking PR moves to `"2026.5.dev1"` — never directly to
  `"2026.5"`).
- Each subsequent structural PR increments the dev counter:
  `.dev1` -> `.dev2` -> `.dev3` ...
- Only the **release PR itself** drops the `.devN` suffix, collapsing
  the dev chain into the final label (`"2026.5.devN"` -> `"2026.5"`).

**Per-PR bookkeeping inside a dev cycle:**

- `SCHEMA_VERSIONS` gains a new entry keyed on the new dev label,
  describing only the delta this PR introduces (not a cumulative
  summary).
- `_HANDLERS` in `yaml_upgrade.py` registers a handler keyed on the
  previous dev label (or the last released label, for `.dev1`).
  Example: PR 3 in a cycle adds
  `("2026.5.dev2", "2026.5.dev3"): _migrate_2026_5_dev2_to_current`.
- `sample_config.yml`'s `schema_version` follows `CURRENT_SCHEMA_VERSION`
  at each step.
- `docs/source/contributing/schema/schema_versioning.rst` and
  `docs/source/inputs/transition_guide.rst` each get a dedicated
  per-dev-label entry (the schema-audit CI check requires doc churn
  in the same PR whenever the version literal moves).

**Release-PR collapse step** (codified in the `prep-release` skill):

- Set `CURRENT_SCHEMA_VERSION` to the plain CalVer label
  (`"2026.5.devN"` -> `"2026.5"`).
- Remove every `.devN` entry from `SCHEMA_VERSIONS`; add one
  consolidated `"2026.5"` entry summarising the union of deltas from
  all dev labels in the cycle.
- Remove every `(2026.5.devN, 2026.5.devM)` and
  `(<prev-release>, 2026.5.devN)` entry from `_HANDLERS`; add a single
  `(<prev-release>, "2026.5")` handler whose body applies the union
  of all dev deltas (ordered to match how they applied in sequence,
  so logged rename/drop lines stay audit-friendly).
- Update the vendored release fixture under
  `test/fixtures/release_configs/<release-tag>.yml` to the collapsed
  shape.
- Add the release-tag mapping to
  `yaml_upgrade.py::_PACKAGE_TO_SCHEMA` (e.g.
  `"2026.5.0": "2026.5"`).

**Rationale:**

- The version number stays semantically tied to the release it's
  targeting; we don't chew through five CalVer labels for five PRs
  that all land in the same release.
- The CI's `check_schema_version_bump.py` gate is satisfied by the
  `.devN` increment (literal string moves).
- Released packages never expose dev labels: the collapse happens
  atomically in the release PR.
- Users who pull development branches see `2026.5.dev3` (or similar)
  in their local samples — a clear signal that the schema is still
  in flux.

**Caveat about `get_schema_compatibility_message`:** the float-parse
path in `version.py::get_schema_compatibility_message` raises
`ValueError` on `"5.dev3"` and falls through to a generic message
via `except (ValueError, IndexError):`. This degrades the "older vs
newer" judgement but does not crash. Fix is optional; if addressed,
use `packaging.version.Version` for the comparison.

---

## How to bump

1. Edit `src/supy/data_model/schema/version.py`:
   - Set `CURRENT_SCHEMA_VERSION` per the dev-label convention above
     — `"<target>.dev1"` for the first structural PR of a new cycle,
     `.devN+1` for subsequent PRs, plain CalVer (e.g. `"2026.5"`)
     only inside the release PR itself. Use the shortest form
     (`"2026.5"`, not `"2026.05"`).
   - Add one entry to `SCHEMA_VERSIONS` describing precisely what
     changed, with issue / PR references.
   - (There is no separate compatibility table to update.
     `is_schema_compatible` derives the answer from the migration
     handler registry — step 3 is what makes older schemas compatible.)
2. Update `src/supy/sample_data/sample_config.yml` so its
   `schema_version` field matches the new `CURRENT_SCHEMA_VERSION`.
3. Add a migration handler in
   `src/supy/util/converter/yaml_upgrade.py`:
   - Key the handler by `(old_schema, new_schema)` in `_HANDLERS`.
   - Express the delta through rename / drop tables plus any bespoke
     reshaping (see `_migrate_2026_1_to_current` for the pattern).
   - Every drop must carry a human-readable `reason` — Pydantic's
     default `extra="ignore"` will otherwise silently swallow the
     user's value.
   - Adding this handler is what makes the previous schema
     compatible with the new one: `SchemaMigrator` pulls `_HANDLERS`
     into its registry at construction time, and
     `is_schema_compatible` consults that registry directly.
4. Vendor a regression fixture under
   `test/fixtures/release_configs/<release-tag>.yml` capturing the
   outgoing shape. `test_release_compat.py` picks these up
   automatically through `_PACKAGE_TO_SCHEMA`.
5. If the bump coincides with a formal release, add the release tag ->
   schema version mapping in
   `src/supy/util/converter/yaml_upgrade.py::_PACKAGE_TO_SCHEMA`.
6. **Sync the user-facing documentation.** A schema bump is a
   user-visible change, so the docs must move with the code. In the
   same PR, touch every file below that the change actually affects:
   - `docs/source/contributing/schema/schema_versioning.rst` — keep
     the "Version History" list and the illustrative schema tags in
     sync with `SCHEMA_VERSIONS`; the CalVer / compatibility-registry
     narrative on that page is the authoritative user explanation.
   - `docs/source/inputs/transition_guide.rst` — add (or extend) the
     "YAML schema migrations" section with a per-release entry
     describing what users see change (renames, removals, profile
     splits) and the exact `suews-convert` / `suews-schema migrate`
     invocation that upgrades their YAML.
   - `docs/source/version-history/v<release-tag>.rst` — when the bump
     ships in a formal release, list the migration chain under
     "Breaking Changes" and link to the transition guide anchor.
   - `docs/source/contributing/schema/schema-developer.rst` and
     `schema_cli.rst` — only if the bump changes the developer
     workflow (e.g. new flag, new CLI behaviour). Cosmetic bumps
     don't need this.

   Cosmetic version-file edits (pure docstring/comment changes in
   `version.py`) that do not change the literal `CURRENT_SCHEMA_VERSION`
   value are not bumps and don't trigger this step.

---

## Pre-release gate (automated via prep-release skill)

Before cutting a release, run:

```bash
git log --oneline <last-release-tag>..HEAD -- src/supy/data_model/
```

If any commit in that range changed structure (per the triggers
above), confirm that:

- `CURRENT_SCHEMA_VERSION` has been bumped at least once since the last
  release tag,
- `SCHEMA_VERSIONS` has a corresponding entry for the new version,
- `sample_config.yml` carries the current schema version,
- `_HANDLERS` in `yaml_upgrade.py` has a handler from the previous
  schema to the current one — this is the single source of truth for
  compatibility; `is_schema_compatible` reads it directly,
- A vendored fixture for the previous release exists under
  `test/fixtures/release_configs/`.

If any of these are missing, stop the release and add them first. The
`prep-release` skill codifies this check.

---

## PR review gate (automated via audit-pr skill)

When reviewing a PR that touches `src/supy/data_model/`:

- If the diff includes a field rename, removal, type change, required
  addition, or structural reshape, `src/supy/data_model/schema/version.py`
  must also be touched in the same PR. Flag otherwise.
- The PR should add or update a handler in
  `src/supy/util/converter/yaml_upgrade.py` that covers the new delta.
- `TestNoSilentFieldDrops` in `test/data_model/test_yaml_upgrade.py`
  should still pass against every vendored fixture.
- If `CURRENT_SCHEMA_VERSION` moved, confirm the PR also touches
  `docs/source/contributing/schema/schema_versioning.rst` and
  `docs/source/inputs/transition_guide.rst` (see step 6 above).
  A schema bump without matching doc updates is a review blocker
  unless a maintainer applies the `schema-audit-ok` label.

## CI gate and bypass label

The `.github/workflows/schema-version-audit.yml` workflow runs
`scripts/lint/check_schema_version_bump.py` on every PR that touches
`src/supy/data_model/**` or `src/supy/sample_data/sample_config.yml`.
If those paths changed but
`src/supy/data_model/schema/version.py` did not, the job fails with
remediation guidance pointing at this rule.

The same script also enforces the docs sync from step 6: when
`CURRENT_SCHEMA_VERSION` does move, at least one of
`docs/source/contributing/schema/schema_versioning.rst` and
`docs/source/inputs/transition_guide.rst` must be touched in the same
PR. This closes the loop opened by gh#1304 (where the version number
was the missing piece) — now the user-facing explanation of what
changed has to move with it, or CI blocks the merge.

Bypass (for genuinely cosmetic diffs — docstrings, comments,
formatting, non-structural value tweaks): a maintainer adds the
`schema-audit-ok` label to the pull request. The workflow reads labels
before running and short-circuits when that label is present. The
label is deliberately specific to this gate so it cannot be
absent-mindedly reused for unrelated bypasses.

Corollary: a contributor should not add the label themselves. If CI
fails and you believe your diff is cosmetic, explain in the PR thread
and ask a maintainer to apply the label.
