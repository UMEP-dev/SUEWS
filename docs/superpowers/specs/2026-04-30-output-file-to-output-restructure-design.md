# Design: `output_file` → `output` YAML restructure (gh#1372 follow-up)

- **Date**: 2026-04-30
- **Branch**: `sunt05/gh1372-named-column-forcing`
- **Issue**: gh#1372 (companion restructure to the `forcing_file` → `forcing.file` change shipped in `2026.5.dev7`)
- **Schema bump**: `2026.5.dev7` → `2026.5.dev8`

---

## 1. Motivation

`forcing_file` was promoted to a `forcing:` sub-object in `2026.5.dev7` so future
forcing-related fields (sub-hourly disaggregation, resampling policy) have a
stable home. The current `output_file` field is asymmetric:

- It is a `Union[str, OutputConfig]` with the string form already deprecated and
  silently ignored.
- The sibling `forcing` block is now a clean sub-object, while `output_file`
  reads as a flat field even though it actually nests `format / freq / groups /
  path`.

This design lifts `output_file` into a sibling sub-object `output:` so the
`model.control` surface is uniform and future output-related fields (e.g.
naming patterns, compression options, append modes) have a stable home.

The user-facing yardstick: a YAML reader should see

```yaml
model:
  control:
    forcing:
      file: forcing_2020.txt
    output:
      format: parquet
      freq: 3600
      dir: ./outputs
```

— two parallel `<channel>:` blocks under `control`, both structured the same
way.

---

## 2. Schema delta

### 2.1 Top-level rename

- Rename `ModelControl.output_file` → `ModelControl.output`.
- Drop `Union[str, OutputConfig]` — keep only the structured form.
- Rename the supporting class `OutputConfig` → `OutputControl` (mirrors
  `ForcingControl`; keeps the "Control" naming family for sub-objects under
  `ModelControl`).

### 2.2 Sub-field rename

Inside the new `OutputControl`:

- Keep `format`, `freq`, `groups` unchanged.
- Rename `path` → `dir`. Rationale: `path` is ambiguous between "directory"
  and "file path"; `dir` makes it explicit, and aligns with the asymmetry
  versus `forcing.file` (input has one file, output has a directory of
  auto-generated files).

### 2.3 Class shape after restructure

```python
class OutputControl(BaseModel):
    """Configuration for model output files."""

    model_config = ConfigDict(title="Output Control")

    format: OutputFormat = Field(default=OutputFormat.TXT, description=...)
    freq: Optional[int] = Field(default=None, description=...)
    groups: Optional[List[str]] = Field(default=None, description=...)
    dir: Optional[str] = Field(default=None, description=...)

    @field_validator("groups")
    def validate_groups(cls, v): ...   # unchanged
```

`ModelControl` field becomes:

```python
output: OutputControl = Field(
    default_factory=OutputControl,
    description="Output configuration (format, frequency, groups, output directory).",
)
```

### 2.4 In-memory legacy coercion

`ModelControl` gains a `_coerce_legacy_output_file` model_validator
(`mode="before"`), parallel to the existing `_coerce_legacy_forcing_file`. It
handles three input shapes:

- `output_file` is a dict → move under `output`, rename inner `path` → `dir`.
- `output_file` is a string → drop with a one-shot `DeprecationWarning`
  (preserves the existing "string form is ignored" semantics).
- Both `output_file` and `output` present → `output` wins; `output_file` is
  dropped with a warning that it's a duplicate of an already-migrated field.

The validator is the in-memory mirror of the YAML migration handler in §3.2.
It exists so consumers that build `SUEWSConfig` programmatically (tests,
notebooks, third-party callers) do not have to re-shape dicts before passing
them in.

---

## 3. Schema versioning

### 3.1 Version bump

- `src/supy/data_model/schema/version.py`:
  - `CURRENT_SCHEMA_VERSION` → `"2026.5.dev8"`.
  - Add `SCHEMA_VERSIONS["2026.5.dev8"]` describing the delta:
    > "Restructure `model.control.output_file` into the `output:` sub-object,
    > rename `path` → `dir`, and drop the legacy string form. Mirrors the
    > forcing.file restructure shipped in 2026.5.dev7."

### 3.2 Migration handler

`src/supy/util/converter/yaml_upgrade.py`:

- Register `(2026.5.dev7, 2026.5.dev8): _migrate_2026_5_dev7_to_current` in
  `_HANDLERS`.
- Implement `_apply_output_subobject_restructure(model_dict)` helper
  that, for each `model.control` block:
  - If `output_file` is a dict, pop it, rename inner `path` → `dir`, and
    install under `output`.
  - If `output_file` is a string, drop it with a logged
    `reason="legacy output_file string ignored since 2025.10.15; use the
    output sub-object"`.
  - If both `output_file` and `output` exist, drop `output_file` with reason.
- The dev1..dev6 chain inherits the new handler automatically through
  `_migrate_2026_5_to_current` (same composition pattern as the forcing
  restructure).
- `_PACKAGE_TO_SCHEMA` gains `"2026.5.dev7": "2026.5.dev7"` self-mapping so
  `_needs_upgrade` picks up the dev7-shape vendored fixture.

### 3.3 Vendored fixture

`test/fixtures/release_configs/2026.5.dev7.yml`: snapshot of the *current*
`sample_config.yml` (which is dev7-shape) before this PR mutates it. Used by
`test_release_compat.py` to exercise the new dev7 → dev8 migration.

---

## 4. Blast radius

Approximate scope (mirrors the forcing.file PR):

### 4.1 Typed-config callers (~10 files)

Files that read `model.control.output_file.<x>` and need updating to
`model.control.output.<x>`, with `path` → `dir`:

- `src/supy/suews_sim.py`
- `src/supy/_supy_module.py`
- `src/supy/cmd/SUEWS.py`
- `src/supy/cmd/validate_config.py`
- `src/supy/cmd/json_output.py`
- `src/supy/cmd/table_converter.py`
- `src/supy/data_model/core/config.py`
- `src/supy/data_model/doc_utils.py`
- `src/supy/data_model/schema/publisher.py` (if it references the field)
- `src/supy/util/converter/yaml.py`
- `src/supy/suews_output.py`

(Final list confirmed during planning by grepping each file for `output_file`
in a typed-access context. CLI parameter occurrences named `output_file` —
e.g. `--output-file` flags — are out of scope.)

### 4.2 Raw-YAML readers (need both-key compatibility)

These read raw dicts/YAML before the typed model coerces, so they must accept
both legacy `output_file` and new `output` keys:

- `src/suews_bridge/src/yaml_config.rs`
- `src/supy/data_model/validation/pipeline/phase_a.py`
- `src/supy/data_model/validation/core/yaml_helpers.py`
- `src/supy/data_model/schema/migration.py`

### 4.3 Sample / fixtures (rewritten to new shape)

- `src/supy/sample_data/sample_config.yml`
- `test/fixtures/sparse_site.yml`
- `test/fixtures/benchmark1/{benchmark1,benchmark1b,benchmark1_short}.yml`
- `test/fixtures/precheck_testcase/precheck_testcase{1,2,3}.yml`
- `test/fixtures/data_test/stebbs_test/sample_config.yml`
- `test/core/data/issue_1097/yaml_setup.yml`
- `docs/source/inputs/yaml/examples/output_config_{simple,txt,parquet,all_groups}.yml`
- `docs/source/inputs/yaml/examples/forcing_{single,multiple}_files.yml`

### 4.4 Documentation

- `docs/source/outputs/text_format.rst`
- `docs/source/outputs/parquet_format.rst`
- `docs/source/sub-tutorials/suews-simulation-tutorial.rst`
- `docs/source/contributing/schema/schema_versioning.rst` — add dev8 entry
- `docs/source/inputs/transition_guide.rst` — add dev8 migration section
- `CHANGELOG.md` — breaking entry pointing at `suews-convert`

### 4.5 Out of scope (false hits in grep)

These match `output_file` literally but are not the YAML field:

- `src/supy/cmd/to_yaml.py`, `cmd/json_output.py`, `cmd/table_converter.py`,
  `cmd/validate_config.py` — Python kwargs / CLI flags named `output_file`
  (file path arguments to converter CLIs), not the YAML field. Verified
  case-by-case; if any of these *also* touch the YAML field they're listed
  in §4.1.
- `docs/source/api/converter.rst`, `docs/source/api/python-cli-equivalents/
  conversion.rst` — `output_file=` argument in code examples.
- `docs/source/inputs/tables/met_input.rst:152,170` — local variable in a
  user code example.
- `docs/source/parameterisations-and-sub-models.rst:117`,
  `docs/source/inputs/tables/RunControl/File_related_options.rst:111` —
  prose mentions of "output files".
- `src/suews/ext_lib/spartacus-surface/...` — vendored upstream.

The auto-generated files
`docs/source/inputs/tables/schema.json` and
`docs/source/inputs/yaml/config-reference/modelcontrol.rst` regenerate
correctly once §2 lands; do not hand-edit.

---

## 5. Testing

### 5.1 New tests

- `test/data_model/test_yaml_upgrade.py::test_dev7_to_dev8_output_restructure`
  — covers four input shapes:
  - dict form with `path` (full migration: rename top-level + inner field)
  - dict form already using `dir` (idempotent on inner rename)
  - string form (drop with reason)
  - already-migrated form (no-op; both keys present → `output` wins)
- `test/io_tests/test_output_config.py::test_legacy_output_file_dict_coercion`
  — same matrix exercised through the in-memory `_coerce_legacy_output_file`
  model_validator.

### 5.2 Updated tests

Sweep the following to switch `output_file` → `output` and `path` → `dir`:

- `test/io_tests/test_output_config.py` (existing assertions on field shape)
- `test/io_tests/test_yaml_annotation.py`
- `test/data_model/test_yaml_processing.py`
- `test/data_model/test_validation.py`
- `test/core/test_load.py`
- `test/core/test_sample_output.py`
- `test/core/test_cli_validation.py`
- `test/core/test_cli_conversion.py`
- `test/cmd/test_df_state_conversion.py`
- `test/umep/test_processor.py`
- `test/umep/test_postprocessor.py`

### 5.3 Release compat

`test/fixtures/release_configs/2026.5.dev7.yml` (vendored in §3.3) is picked up
automatically by `test_release_compat.py` through `_PACKAGE_TO_SCHEMA`, so the
dev7 → dev8 chain is covered without an additional test.

---

## 6. Risk and rollback

- **Risk: typed-config caller missed**. A stale `model.control.output_file`
  read crashes at runtime with `AttributeError`. Mitigated by the typed-caller
  sweep in §4.1 and `make test-smoke` in CI.
- **Risk: raw-YAML reader doesn't accept new key**. Old user configs would
  still load (legacy compat retained), but new sample_config / fixtures fail
  to load. Mitigated by §4.2 dual-key acceptance.
- **Risk: schema version not bumped**. Caught by the schema-version-audit CI
  workflow (`scripts/lint/check_schema_version_bump.py`).
- **Rollback**: revert the merge commit. Schema version handlers and
  CHANGELOG are additive; nothing else needs special undo.

---

## 7. Out of scope (deferred)

- Adding a literal `output.file:` field for user-supplied output filenames.
  Decided against in brainstorming: output is multi-file by nature
  (site/group/year), so a single `file` is semantically prepared for parquet
  only. If demand surfaces post-merge, add as a future field under
  `OutputControl` without re-bumping the schema (additive Optional field).
- Cross-layer rename of Fortran TYPE / Rust struct identifiers — tracked in
  #1324/#1325/#1326. This PR only touches the Python/YAML surface.
- Renaming `output` to a more semantic alternative (e.g. `output_control`).
  Rejected: parallels `forcing` exactly, which is the design goal.

---

## 8. Open questions

None at design freeze.
