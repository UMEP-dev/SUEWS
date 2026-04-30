# `output_file` → `output` YAML restructure — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Lift `model.control.output_file` (`Union[str, OutputConfig]`) into a sibling sub-object `output:` (`OutputControl`) that mirrors the `forcing:` block shipped in schema `2026.5.dev7`. Drop the deprecated string form and rename the inner `path` → `dir`.

**Architecture:** Single PR, schema bump `2026.5.dev7` → `2026.5.dev8`. Mirrors the gh#1372 forcing.file restructure pattern: data-model class + in-memory legacy coercion validator → schema version + entry → YAML migration handler → vendored fixture → typed-config caller sweep → raw-YAML reader dual-key acceptance → fixtures + sample_config + docs sweep.

**Tech Stack:** Python (Pydantic v2 data models), Rust (`suews_bridge` raw-YAML reader), Sphinx RST docs, pytest.

**Spec:** `docs/superpowers/specs/2026-04-30-output-file-to-output-restructure-design.md`

---

## Task 1: Add `OutputControl` class + `ModelControl.output` field with legacy coercion

**Files:**
- Modify: `src/supy/data_model/core/model.py:946-1042`
- Test: `test/io_tests/test_output_config.py` (new test cases)

- [ ] **Step 1: Write failing tests for the new typed-model surface**

Append to `test/io_tests/test_output_config.py`:

```python
def test_modelcontrol_accepts_new_output_subobject():
    """The new `output:` sub-object replaces `output_file:`."""
    from supy.data_model.core.model import ModelControl

    mc = ModelControl(
        output={"format": "parquet", "freq": 3600, "dir": "./out"}
    )
    assert mc.output.format.value == "parquet"
    assert mc.output.freq == 3600
    assert mc.output.dir == "./out"
    assert not hasattr(mc, "output_file")


def test_modelcontrol_legacy_output_file_dict_coerced():
    """Old `output_file:` dict form is coerced to `output:` in-memory."""
    import warnings

    from supy.data_model.core.model import ModelControl

    with warnings.catch_warnings(record=True) as caught:
        warnings.simplefilter("always")
        mc = ModelControl(
            output_file={
                "format": "txt",
                "freq": 1800,
                "groups": ["SUEWS", "DailyState"],
                "path": "./legacy_out",
            }
        )
    assert mc.output.format.value == "txt"
    assert mc.output.freq == 1800
    assert mc.output.groups == ["SUEWS", "DailyState"]
    assert mc.output.dir == "./legacy_out"
    assert any(
        "output_file" in str(w.message).lower() for w in caught
    ), "expected DeprecationWarning mentioning output_file"


def test_modelcontrol_legacy_output_file_string_dropped():
    """Old `output_file: 'output.txt'` string form is dropped with a warning."""
    import warnings

    from supy.data_model.core.model import ModelControl

    with warnings.catch_warnings(record=True) as caught:
        warnings.simplefilter("always")
        mc = ModelControl(output_file="output.txt")
    # Default OutputControl is installed
    assert mc.output.format.value == "txt"
    assert mc.output.dir is None
    assert any(
        "output_file" in str(w.message).lower()
        and "ignored" in str(w.message).lower()
        for w in caught
    ), "expected DeprecationWarning that the string form is ignored"


def test_modelcontrol_both_keys_output_wins():
    """If both keys are present, `output:` wins; `output_file:` is dropped."""
    import warnings

    from supy.data_model.core.model import ModelControl

    with warnings.catch_warnings(record=True):
        warnings.simplefilter("always")
        mc = ModelControl(
            output={"format": "parquet", "freq": 3600, "dir": "./new"},
            output_file={"format": "txt", "path": "./old"},
        )
    assert mc.output.format.value == "parquet"
    assert mc.output.dir == "./new"


def test_outputcontrol_groups_validator_unchanged():
    """The groups validator still rejects unknown groups."""
    import pytest

    from supy.data_model.core.model import OutputControl

    with pytest.raises(ValueError, match="Invalid output groups"):
        OutputControl(format="txt", groups=["SUEWS", "Bogus"])
```

- [ ] **Step 2: Run tests — they should fail**

Run: `uv run pytest test/io_tests/test_output_config.py -v -k "output_subobject or legacy_output_file or both_keys or groups_validator_unchanged" 2>&1 | tail -40`

Expected: All five tests fail (`OutputControl` does not exist, `ModelControl.output` does not exist, `output_file` field still typed as `Union[str, OutputConfig]`).

- [ ] **Step 3: Replace `OutputConfig` → `OutputControl` and field rename in `model.py`**

In `src/supy/data_model/core/model.py:946-978`, rename the class `OutputConfig` to `OutputControl` (only the definition; keep imports/aliases as in step 4) and rename the `path` field to `dir`:

```python
class OutputControl(BaseModel):
    """Configuration for model output files."""

    model_config = ConfigDict(title="Output Control")

    format: OutputFormat = Field(
        default=OutputFormat.TXT,
        description="Output file format. Options: 'txt' for traditional text files (one per year/grid/group), 'parquet' for single Parquet file containing all data",
    )
    freq: Optional[int] = Field(
        default=None,
        description="Output frequency in seconds. Must be a multiple of the model timestep (tstep). If not specified, defaults to 3600 (hourly)",
    )
    groups: Optional[List[str]] = Field(
        default=None,
        description="List of output groups to save (only applies to txt format). Available groups: 'SUEWS', 'DailyState', 'snow', 'ESTM', 'RSL', 'BL', 'debug'. If not specified, defaults to ['SUEWS', 'DailyState']",
    )
    dir: Optional[str] = Field(
        default=None,
        description="Output directory where result files will be saved. If not specified, defaults to the current working directory.",
    )

    @field_validator("groups")
    def validate_groups(cls, v):
        if v is not None:
            valid_groups = {"SUEWS", "DailyState", "snow", "ESTM", "RSL", "BL", "debug"}
            dev_groups = {"SPARTACUS", "EHC", "STEBBS"}
            invalid = set(v) - valid_groups - dev_groups
            if invalid:
                raise ValueError(
                    f"Invalid output groups: {invalid}. Valid groups are: {valid_groups}"
                )
        return v
```

- [ ] **Step 4: Add the `output:` field on `ModelControl` and the legacy validator**

In `src/supy/data_model/core/model.py`, replace the `output_file` field on `ModelControl` (around line 1039–1042) with:

```python
    output: OutputControl = Field(
        default_factory=OutputControl,
        description=(
            "Output configuration: file format, frequency, groups (txt only), "
            "and output directory. For detailed information about output "
            "variables and file structure, see :ref:`output_files`."
        ),
    )
```

Add a `_coerce_legacy_output_file` model_validator next to the existing `_coerce_legacy_forcing_file` (around line 1008–1025). Place it directly after `_coerce_legacy_forcing_file`:

```python
    @model_validator(mode="before")
    @classmethod
    def _coerce_legacy_output_file(cls, values):
        """Accept legacy ``output_file`` input by lifting it under ``output``.

        Three input shapes are handled:

        * dict form ``output_file: {...}`` is moved to ``output: {...}`` and
          its inner ``path`` field is renamed to ``dir``;
        * string form ``output_file: 'name.txt'`` is dropped (the string form
          has been silently ignored since 2025.10.15);
        * if both ``output_file`` and ``output`` are present, ``output`` wins
          and the legacy key is dropped.

        A one-shot ``DeprecationWarning`` fires whenever ``output_file`` is
        encountered so users see their migration target.
        """
        import warnings

        if not isinstance(values, dict) or "output_file" not in values:
            return values

        values = values.copy()
        legacy = values.pop("output_file")

        if "output" in values:
            warnings.warn(
                "Both `output_file` and `output` were supplied; the legacy "
                "`output_file` value is ignored.",
                DeprecationWarning,
                stacklevel=2,
            )
            return values

        if isinstance(legacy, dict):
            migrated = {k: v for k, v in legacy.items() if k != "path"}
            if "path" in legacy and "dir" not in migrated:
                migrated["dir"] = legacy["path"]
            values["output"] = migrated
            warnings.warn(
                "`model.control.output_file` is deprecated; lifted under "
                "`model.control.output` (path -> dir).",
                DeprecationWarning,
                stacklevel=2,
            )
            return values

        # Legacy string form: silently dropped at runtime since 2025.10.15;
        # we now warn explicitly.
        warnings.warn(
            "`model.control.output_file` string form is ignored; use the "
            "`output:` sub-object instead.",
            DeprecationWarning,
            stacklevel=2,
        )
        return values
```

- [ ] **Step 5: Search for stale `OutputConfig` / `output_file` symbol references inside the data model itself**

Run:

```bash
grep -rn "OutputConfig\b\|output_file\b" /Users/tingsun/conductor/workspaces/suews/porto-v2/src/supy/data_model/
```

Expected hits to fix in this task: `core/config.py`, `doc_utils.py`, `validation/core/yaml_helpers.py`, `validation/pipeline/PHASE_A_DETAILED.md`, `validation/pipeline/ORCHESTRATOR.md`, `schema/migration.py`. For now, only update the **typed access paths** in `core/config.py` and `doc_utils.py` (these are imported at module load and would crash on `from .model import OutputConfig`):

In `src/supy/data_model/core/config.py`:

```bash
grep -n "OutputConfig\|output_file" src/supy/data_model/core/config.py
```

Replace each `OutputConfig` symbol import with `OutputControl`. Replace each `model.control.output_file` typed access with `model.control.output` and any `.path` with `.dir`. (Raw-YAML key handling is deferred to Task 6.)

In `src/supy/data_model/doc_utils.py`: same replacements.

In `src/supy/data_model/schema/migration.py`: this file routes by raw key so leave the key handling for Task 6 — but any imports of `OutputConfig` need renaming here too.

In `src/supy/data_model/validation/core/yaml_helpers.py`: raw-key access (`output_file`); leave for Task 6.

- [ ] **Step 6: Run the new tests — they should pass**

Run: `uv run pytest test/io_tests/test_output_config.py -v -k "output_subobject or legacy_output_file or both_keys or groups_validator_unchanged" 2>&1 | tail -40`

Expected: All five tests pass.

- [ ] **Step 7: Run the full output-config and validation test files to catch breakage**

Run: `uv run pytest test/io_tests/test_output_config.py test/io_tests/test_yaml_annotation.py -v 2>&1 | tail -60`

Expected: Existing tests that asserted on `output_file` / `OutputConfig` / `path` field will FAIL. **Note these failures — they are addressed in Task 9.** Pre-existing tests not touching the renamed surface should still pass.

- [ ] **Step 8: Commit**

```bash
git add src/supy/data_model/core/model.py src/supy/data_model/core/config.py src/supy/data_model/doc_utils.py src/supy/data_model/schema/migration.py test/io_tests/test_output_config.py
git commit -m "feat(data-model): add OutputControl sub-object (gh#1372)

Replaces ModelControl.output_file (Union[str, OutputConfig]) with a
structured OutputControl sub-object exposed under ModelControl.output.
Drops the legacy string form (silently ignored since 2025.10.15) and
renames the inner path -> dir. Mirrors the ForcingControl restructure
shipped in schema 2026.5.dev7.

A new _coerce_legacy_output_file model_validator accepts legacy
'output_file:' dict / string input shapes for in-memory construction
and emits DeprecationWarnings."
```

---

## Task 2: Bump `CURRENT_SCHEMA_VERSION` to `2026.5.dev8` and add `SCHEMA_VERSIONS` entry

**Files:**
- Modify: `src/supy/data_model/schema/version.py:25` (CURRENT_SCHEMA_VERSION)
- Modify: `src/supy/data_model/schema/version.py:213` (append to SCHEMA_VERSIONS)

- [ ] **Step 1: Bump the literal**

In `src/supy/data_model/schema/version.py:25`:

```python
CURRENT_SCHEMA_VERSION = "2026.5.dev8"
```

- [ ] **Step 2: Add the SCHEMA_VERSIONS entry**

Append immediately after the existing `"2026.5.dev7"` entry (closing bracket of the dict):

```python
    "2026.5.dev8": (
        "gh#1372 follow-up: structural restructure of output configuration. "
        "model.control.output_file (Union[str, OutputConfig]) is moved to "
        "model.control.output under a new OutputControl sub-object — "
        "mirrors the ForcingControl restructure shipped in 2026.5.dev7 so "
        "the model.control surface is uniform. The deprecated string form "
        "(silently ignored since 2025.10.15) is dropped; the inner `path` "
        "field is renamed to `dir` (clarifies it as a directory, parallels "
        "the asymmetry with forcing.file). The (2026.5.dev7 -> 2026.5.dev8) "
        "migration is registered in "
        "src/supy/util/converter/yaml_upgrade.py::_HANDLERS via "
        "_apply_output_subobject_restructure."
    ),
```

- [ ] **Step 3: Verify schema-version-audit lint passes**

Run:

```bash
uv run python /Users/tingsun/conductor/workspaces/suews/porto-v2/scripts/lint/check_schema_version_bump.py 2>&1 | tail -20
```

Expected: passes (or, if it complains about missing docs touch, that is addressed in Task 8).

- [ ] **Step 4: Commit**

```bash
git add src/supy/data_model/schema/version.py
git commit -m "feat(schema): bump to 2026.5.dev8 for output.dir restructure (gh#1372)

Records the YAML restructure (output_file -> output, path -> dir,
drop legacy string form) in SCHEMA_VERSIONS so the migration
registry can offer compatibility from 2026.5.dev7."
```

---

## Task 3: Add YAML migration handler `(2026.5.dev7, 2026.5.dev8)` and tests

**Files:**
- Modify: `src/supy/util/converter/yaml_upgrade.py` (add helper + handler + chain hookups)
- Test: `test/data_model/test_yaml_upgrade.py` (add new test class)

- [ ] **Step 1: Write failing tests for the new handler**

Append to `test/data_model/test_yaml_upgrade.py` (file already exists):

```python
class TestOutputSubobjectRestructure:
    """gh#1372 follow-up: dev7 -> dev8 lifts output_file -> output."""

    def _migrate(self, cfg: dict) -> dict:
        from supy.util.converter.yaml_upgrade import (
            _migrate_2026_5_dev7_to_current,
        )
        return _migrate_2026_5_dev7_to_current(cfg)

    def test_dict_form_lifts_and_renames_path(self):
        cfg = {
            "schema_version": "2026.5.dev7",
            "model": {
                "control": {
                    "output_file": {
                        "format": "txt",
                        "freq": 3600,
                        "groups": ["SUEWS"],
                        "path": "./legacy_out",
                    },
                },
            },
        }
        out = self._migrate(cfg)
        assert "output_file" not in out["model"]["control"]
        result = out["model"]["control"]["output"]
        assert result["format"] == "txt"
        assert result["freq"] == 3600
        assert result["groups"] == ["SUEWS"]
        assert result["dir"] == "./legacy_out"
        assert "path" not in result

    def test_dict_form_already_uses_dir(self):
        cfg = {
            "schema_version": "2026.5.dev7",
            "model": {
                "control": {
                    "output_file": {
                        "format": "parquet",
                        "freq": 1800,
                        "dir": "./already_dir",
                    },
                },
            },
        }
        out = self._migrate(cfg)
        assert out["model"]["control"]["output"]["dir"] == "./already_dir"
        assert "path" not in out["model"]["control"]["output"]

    def test_string_form_dropped(self):
        cfg = {
            "schema_version": "2026.5.dev7",
            "model": {"control": {"output_file": "output.txt"}},
        }
        out = self._migrate(cfg)
        assert "output_file" not in out["model"]["control"]
        # No `output` synthesised — the migrator drops the string form;
        # the data-model default_factory re-installs OutputControl() at load.
        assert "output" not in out["model"]["control"]

    def test_already_migrated_is_idempotent(self):
        cfg = {
            "schema_version": "2026.5.dev8",
            "model": {
                "control": {
                    "output": {"format": "parquet", "dir": "./out"},
                },
            },
        }
        out = self._migrate(cfg)
        assert out["model"]["control"]["output"] == {
            "format": "parquet",
            "dir": "./out",
        }

    def test_both_keys_output_wins(self):
        cfg = {
            "schema_version": "2026.5.dev7",
            "model": {
                "control": {
                    "output": {"format": "parquet", "dir": "./new"},
                    "output_file": {"format": "txt", "path": "./old"},
                },
            },
        }
        out = self._migrate(cfg)
        assert "output_file" not in out["model"]["control"]
        assert out["model"]["control"]["output"]["dir"] == "./new"
        assert out["model"]["control"]["output"]["format"] == "parquet"
```

- [ ] **Step 2: Run the new tests — they should fail**

Run: `uv run pytest test/data_model/test_yaml_upgrade.py::TestOutputSubobjectRestructure -v 2>&1 | tail -30`

Expected: All five tests fail (`_migrate_2026_5_dev7_to_current` does not exist).

- [ ] **Step 3: Add the helper and handler**

In `src/supy/util/converter/yaml_upgrade.py`, immediately after `_apply_forcing_subobject_restructure` (around line 666):

```python
def _apply_output_subobject_restructure(cfg: dict) -> dict:
    """Move ``model.control.output_file`` under ``model.control.output``.

    gh#1372 follow-up: introduces an OutputControl sub-object so the
    ``model.control`` surface is uniform with the new ``forcing:`` block.
    The dict form is preserved verbatim under ``output``, with the
    inner ``path`` field renamed to ``dir`` (clarifies it as a directory).
    The legacy string form (silently ignored since 2025.10.15) is dropped
    with a logged reason. If both ``output_file`` and ``output`` are
    present, ``output`` wins and the legacy key is dropped.
    """
    model = cfg.get("model")
    if not isinstance(model, dict):
        return cfg
    control = model.get("control")
    if not isinstance(control, dict):
        return cfg
    if "output_file" not in control:
        return cfg

    legacy = control.pop("output_file")

    if "output" in control:
        _log(
            "[yaml-upgrade]   dropped 'output_file' (already migrated; "
            "'output' key wins)"
        )
        return cfg

    if isinstance(legacy, dict):
        migrated = {k: v for k, v in legacy.items() if k != "path"}
        if "path" in legacy and "dir" not in migrated:
            migrated["dir"] = legacy["path"]
            _log("[yaml-upgrade]   renamed 'output_file.path' -> 'output.dir'")
        control["output"] = migrated
        _log(
            "[yaml-upgrade]   migrated 'output_file' (dict) -> 'output' "
            "sub-object (gh#1372)"
        )
        return cfg

    # Legacy string form (e.g. output_file: "output.txt"): drop with reason.
    _drop_obsolete_field(
        {"output_file": legacy},
        "output_file",
        "string form silently ignored since 2025.10.15; use the 'output:' "
        "sub-object",
    )
    _log(
        "[yaml-upgrade]   dropped 'output_file' string value "
        f"({legacy!r}); use the 'output:' sub-object"
    )
    return cfg
```

(The `_drop_obsolete_field` call on a fresh dict is a no-op for state — it's there only to emit the standardised log line through the same helper.)

Now add the new top-of-cycle handler `_migrate_2026_5_dev7_to_current` after `_migrate_2026_5_dev6_to_current` (around line 676):

```python
def _migrate_2026_5_dev7_to_current(cfg: dict) -> dict:
    """Upgrade 2026.5.dev7-shaped YAMLs to the current schema.

    Applies the gh#1372 follow-up output-config restructure: output_file
    is moved under a new output sub-object and the inner ``path`` is
    renamed to ``dir``.
    """
    cfg = _strip_internal_only_fields(cfg)
    _apply_output_subobject_restructure(cfg)
    return cfg
```

- [ ] **Step 4: Chain the new helper through every older handler**

Every handler that returns to `CURRENT_SCHEMA_VERSION` must now also apply the output restructure. Update each `_migrate_2026_5_devN_to_current` and `_migrate_2026_5_to_current` (lines 668–756) by appending one line each:

```python
    _apply_output_subobject_restructure(cfg)
    return cfg
```

The exact list of handlers to update (each gets the call inserted after the existing `_apply_forcing_subobject_restructure(cfg)` line):

- `_migrate_2026_5_dev6_to_current` (line ~668)
- `_migrate_2026_5_dev5_to_current` (line ~679)
- `_migrate_2026_5_dev4_to_current` (line ~686)
- `_migrate_2026_5_dev3_to_current` (line ~693)
- `_migrate_2026_5_dev2_to_current` (line ~706)
- `_migrate_2026_5_dev1_to_current` (line ~759)
- `_migrate_2026_5_to_current` (line ~721)

(The `_migrate_2026_4_to_current`, `_migrate_2026_1_to_current`, `_migrate_2025_12_to_current` chains compose `_migrate_2026_5_to_current`, so they pick the new step up automatically.)

- [ ] **Step 5: Register the new handler in `_HANDLERS`**

In `src/supy/util/converter/yaml_upgrade.py:817`, add the new entry directly above the `("2026.5.dev6", ...)` line:

```python
    ("2026.5.dev7", CURRENT_SCHEMA_VERSION): _migrate_2026_5_dev7_to_current,
```

Also extend the comment block above the registrations (line ~827) — add the trailing line `# + gh#1372 follow-up output_file -> output sub-object restructure (path -> dir).` to the bullet list.

- [ ] **Step 6: Update the `_PACKAGE_TO_SCHEMA` self-mapping for the dev7 fixture**

In `src/supy/util/converter/yaml_upgrade.py:73`, add `"2026.5.dev7": "2026.5.dev7"` after the existing `"2026.5.dev6"` self-mapping:

```python
    "2026.5.dev6": "2026.5.dev6",
    "2026.5.dev7": "2026.5.dev7",
```

- [ ] **Step 7: Run the new tests — they should pass**

Run: `uv run pytest test/data_model/test_yaml_upgrade.py::TestOutputSubobjectRestructure -v 2>&1 | tail -30`

Expected: All five tests pass.

- [ ] **Step 8: Commit**

```bash
git add src/supy/util/converter/yaml_upgrade.py test/data_model/test_yaml_upgrade.py
git commit -m "feat(yaml_upgrade): add output_file -> output handler (gh#1372)

Adds _apply_output_subobject_restructure helper and a
2026.5.dev7 -> 2026.5.dev8 migration. Earlier dev labels chain through
their existing _migrate_2026_5_devN_to_current handlers and inherit the
new restructure automatically."
```

---

## Task 4: Vendor `2026.5.dev7.yml` fixture for `test_release_compat`

**Files:**
- Create: `test/fixtures/release_configs/2026.5.dev7.yml`
- Run-only: `test/data_model/test_release_compat.py` (no edits — it auto-discovers)

- [ ] **Step 1: Snapshot the current sample_config (which is dev7-shape)**

```bash
cp /Users/tingsun/conductor/workspaces/suews/porto-v2/src/supy/sample_data/sample_config.yml /Users/tingsun/conductor/workspaces/suews/porto-v2/test/fixtures/release_configs/2026.5.dev7.yml
```

Confirm: file uses `forcing.file` and `output_file` (the dev7 shape):

```bash
grep -E "^  control:|forcing:|file:|output_file:" /Users/tingsun/conductor/workspaces/suews/porto-v2/test/fixtures/release_configs/2026.5.dev7.yml | head -20
```

Expected: shows `forcing:`/`file:` (not `forcing_file:`) and `output_file:` (not `output:`).

- [ ] **Step 2: Run the release-compat test against the new fixture**

Run: `uv run pytest test/data_model/test_release_compat.py -v 2>&1 | tail -40`

Expected: the new dev7 fixture is picked up and migrates cleanly through the dev7 → dev8 handler. **If the test fails because the fixture asserts on `output_file`**, that's normal — see Task 7 for fixture sweep.

- [ ] **Step 3: Commit**

```bash
git add test/fixtures/release_configs/2026.5.dev7.yml
git commit -m "feat(yaml-upgrade): vendor 2026.5.dev7 fixture (gh#1372)

Adds the dev7-shape sample_config snapshot under
test/fixtures/release_configs/2026.5.dev7.yml so test_release_compat.py
exercises the (2026.5.dev7 -> 2026.5.dev8) handler from Task 3."
```

---

## Task 5: Sweep typed-config callers (`output_file` → `output`, `path` → `dir`)

**Files (in order of importance):**
- Modify: `src/supy/suews_sim.py`
- Modify: `src/supy/_supy_module.py`
- Modify: `src/supy/cmd/SUEWS.py`
- Modify: `src/supy/data_model/core/config.py`
- Modify: `src/supy/data_model/doc_utils.py`
- Modify: `src/supy/data_model/schema/migration.py`
- Modify: `src/supy/util/converter/yaml.py`
- Modify: `src/supy/suews_output.py`
- Modify: `src/supy/cmd/validate_config.py` (if YAML-field reference, not CLI flag)
- Modify: `src/supy/cmd/json_output.py` (if YAML-field reference, not CLI flag)
- Modify: `src/supy/cmd/table_converter.py` (if YAML-field reference, not CLI flag)

The strict criterion: rewrite **only** call-sites that read `model.control.output_file.<x>` (typed config access) or that import `OutputConfig` as the data-model class. Do **not** touch CLI parameters / Python kwargs literally named `output_file` (those are file paths, unrelated to the YAML field).

- [ ] **Step 1: For each file, find typed-access sites**

For each file in the list above, run:

```bash
grep -nE "output_file|OutputConfig\b" <file>
```

Inspect each match. Replace **only** the typed-config matches:

- `output_file` → `output`
- `OutputConfig` → `OutputControl`
- `<expr>.output_file.path` → `<expr>.output.dir`
- `<expr>.output_file.<other>` → `<expr>.output.<other>`
- raw-YAML key reads (e.g. `cfg["model"]["control"]["output_file"]` accessed *typed* through Pydantic) → `["output"]` (raw-YAML readers in Task 6).

Skip `cmd/`-level `output_file` parameters that name an output-file CLI argument (e.g. `def main(..., output_file: Path)`). Verify by reading 5 lines around each match.

- [ ] **Step 2: Verify no orphan symbol left**

Run:

```bash
grep -rn "OutputConfig\b" src/supy/
grep -rn "\.output_file\b" src/supy/
```

Expected: no hits in source. (`output_file` may legitimately remain as a CLI argument name in `cmd/`; reconfirm each remaining hit is a CLI param.)

- [ ] **Step 3: Run the smoke test**

Run: `make test-smoke 2>&1 | tail -40`

Expected: passes (or fails only on assertions in fixture YAMLs that still carry `output_file`; those are addressed in Task 7).

- [ ] **Step 4: Commit**

```bash
git add src/supy/
git commit -m "fix: sweep stale output_file callers for output sub-object (gh#1372)

Updates every typed-config caller (SUEWSSimulation, _supy_module,
config validator, yaml table-converter, schema publisher, doc_utils)
to access model.control.output.X and dir instead of model.control.
output_file.X and path. Raw-YAML readers still accept both keys (Task 6)."
```

---

## Task 6: Sweep raw-YAML readers (accept both `output_file` and `output` keys)

**Files:**
- Modify: `src/suews_bridge/src/yaml_config.rs`
- Modify: `src/supy/data_model/validation/pipeline/phase_a.py`
- Modify: `src/supy/data_model/validation/core/yaml_helpers.py`
- Modify: `src/supy/data_model/schema/migration.py`

These four files inspect the **raw YAML dict** before the typed model lifts it. They must accept **both** legacy `output_file` and new `output` keys so user configs in either schema work pre-migration. (In contrast to Task 5, where typed access is rewritten outright.)

- [ ] **Step 1: Audit each file's raw-key access**

For each file, run:

```bash
grep -nE "output_file|output['\"]" <file>
```

Identify each spot that reads `cfg["model"]["control"]["output_file"]` (or analogous Rust `serde_yaml::Value` field access).

- [ ] **Step 2: Add dual-key acceptance**

Pattern in Python:

```python
control = cfg.get("model", {}).get("control", {})
output_block = control.get("output", control.get("output_file"))
```

Pattern in Rust (`yaml_config.rs`): mirror the pattern used for `forcing` / `forcing_file`, e.g.

```rust
let output_block = control
    .get("output")
    .or_else(|| control.get("output_file"));
```

- [ ] **Step 3: Run smoke + Rust build**

```bash
make test-smoke 2>&1 | tail -40
cargo build --manifest-path src/suews_bridge/Cargo.toml 2>&1 | tail -20
```

Expected: passes.

- [ ] **Step 4: Commit**

```bash
git add src/suews_bridge/src/yaml_config.rs src/supy/data_model/validation/ src/supy/data_model/schema/migration.py
git commit -m "fix: raw-YAML readers accept both output_file and output (gh#1372)

Mirrors the dual-key acceptance pattern used for forcing.file in
phase_a, validation/core/yaml_helpers, schema/migration, and the Rust
yaml_config reader so pre-migration user configs still load."
```

---

## Task 7: Update sample_config + test fixtures (rewrite to new shape)

**Files:**
- Modify: `src/supy/sample_data/sample_config.yml`
- Modify: `test/fixtures/sparse_site.yml`
- Modify: `test/fixtures/benchmark1/benchmark1.yml`
- Modify: `test/fixtures/benchmark1/benchmark1b.yml`
- Modify: `test/fixtures/benchmark1/benchmark1_short.yml`
- Modify: `test/fixtures/precheck_testcase/precheck_testcase1.yml`
- Modify: `test/fixtures/precheck_testcase/precheck_testcase2.yml`
- Modify: `test/fixtures/precheck_testcase/precheck_testcase3.yml`
- Modify: `test/fixtures/data_test/stebbs_test/sample_config.yml`
- Modify: `test/core/data/issue_1097/yaml_setup.yml`

- [ ] **Step 1: Bump `schema_version` and rewrite each fixture**

For every fixture above, find the `output_file:` block:

```yaml
    output_file:
      format: <fmt>
      freq: <n>
      ...maybe groups...
      path: <dir>
```

Rewrite to:

```yaml
    output:
      format: <fmt>
      freq: <n>
      ...maybe groups...
      dir: <dir>
```

Also update the top-of-file `schema_version: 2026.5.dev7` → `schema_version: 2026.5.dev8` for any fixture that pins it (use `grep -n schema_version <file>` to find).

`sample_config.yml` is the canonical reference — match its formatting.

- [ ] **Step 2: Run smoke test**

Run: `make test-smoke 2>&1 | tail -40`

Expected: passes.

- [ ] **Step 3: Commit**

```bash
git add src/supy/sample_data/sample_config.yml test/fixtures/ test/core/data/issue_1097/yaml_setup.yml
git commit -m "fix: rewrite sample_config + fixtures to new output shape (gh#1372)

Bumps schema_version to 2026.5.dev8 and rewrites every output_file:
block to the new output: sub-object with dir instead of path."
```

---

## Task 8: Update YAML examples in docs + RST docs + CHANGELOG + transition guide

**Files:**
- Modify: `docs/source/inputs/yaml/examples/output_config_simple.yml`
- Modify: `docs/source/inputs/yaml/examples/output_config_txt.yml`
- Modify: `docs/source/inputs/yaml/examples/output_config_parquet.yml`
- Modify: `docs/source/inputs/yaml/examples/output_config_all_groups.yml`
- Modify: `docs/source/inputs/yaml/examples/forcing_single_file.yml`
- Modify: `docs/source/inputs/yaml/examples/forcing_multiple_files.yml`
- Modify: `docs/source/outputs/text_format.rst`
- Modify: `docs/source/outputs/parquet_format.rst`
- Modify: `docs/source/sub-tutorials/suews-simulation-tutorial.rst`
- Modify: `docs/source/contributing/schema/schema_versioning.rst`
- Modify: `docs/source/inputs/transition_guide.rst`
- Modify: `CHANGELOG.md`

- [ ] **Step 1: Rewrite the 6 YAML example files**

Each file currently has an `output_file:` (or `output_file: "..."` legacy string) block. Rewrite analogously to Task 7. The four `output_config_*` examples are the canonical user-facing examples — preserve their educational structure (comments explaining each field).

For files using the legacy string `output_file: "name.txt"`, simply delete that line. The examples are showing forcing configurations (not output) so the line is incidental noise that no longer round-trips.

- [ ] **Step 2: Rewrite RST embedded YAML snippets**

Find every embedded YAML in `outputs/text_format.rst`, `outputs/parquet_format.rst`, `sub-tutorials/suews-simulation-tutorial.rst`:

```bash
grep -n "output_file" docs/source/outputs/text_format.rst docs/source/outputs/parquet_format.rst docs/source/sub-tutorials/suews-simulation-tutorial.rst
```

Replace each `output_file:` block with `output:` and `path:` with `dir:`. Also update prose mentions of `output_file.groups` → `output.groups` and similar.

- [ ] **Step 3: Add dev8 entry to schema_versioning.rst**

Append a new row in the version-history list of `docs/source/contributing/schema/schema_versioning.rst`. Mirror the existing `2026.5.dev7` row (the forcing entry):

```rst
- ``2026.5.dev8`` — gh#1372 follow-up: ``model.control.output_file`` is
  lifted into a sibling ``output:`` sub-object and the inner ``path``
  field is renamed to ``dir``. The deprecated string form
  (``output_file: "out.txt"``, silently ignored since 2025.10.15) is
  dropped. Users with older configs can migrate via
  ``suews-convert --to 2026.5.dev8 in.yml out.yml``.
```

- [ ] **Step 4: Add dev8 section to transition_guide.rst**

Under the existing "YAML schema migrations" section in `docs/source/inputs/transition_guide.rst`, add a sub-section after the dev7 one:

```rst
2026.5.dev8 (output configuration restructure)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The ``model.control.output_file:`` block becomes the sibling
``model.control.output:`` block, mirroring the ``forcing:`` restructure
shipped in dev7. The inner ``path:`` field is renamed to ``dir:``.

Old:

.. code-block:: yaml

   model:
     control:
       output_file:
         format: parquet
         freq: 3600
         path: ./out

New:

.. code-block:: yaml

   model:
     control:
       output:
         format: parquet
         freq: 3600
         dir: ./out

The legacy string form ``output_file: "name.txt"`` was already
silently ignored from 2025.10.15 and is now dropped outright by the
migrator. Run::

   suews-convert --to 2026.5.dev8 in.yml out.yml

to rewrite an older YAML.
```

- [ ] **Step 5: Update `CHANGELOG.md`**

Add a new entry to the unreleased section. Mirror the dev7 forcing entry in tone:

```markdown
### Breaking

- **YAML schema** — `model.control.output_file` is restructured into a
  sibling `output:` sub-object (parallel to the new `forcing:` block).
  The inner `path` field is renamed to `dir`. The deprecated string
  form (silently ignored since 2025.10.15) is dropped. Run
  `suews-convert --to 2026.5.dev8 in.yml out.yml` to upgrade existing
  YAMLs (gh#1372).
```

- [ ] **Step 6: Build the docs**

Run:

```bash
make docs 2>&1 | tail -30
```

Expected: builds without errors. (Warnings from unrelated areas are tolerable.)

- [ ] **Step 7: Commit**

```bash
git add docs/source/inputs/yaml/examples/ docs/source/outputs/ docs/source/sub-tutorials/ docs/source/contributing/schema/schema_versioning.rst docs/source/inputs/transition_guide.rst CHANGELOG.md
git commit -m "docs: output_file -> output sub-object restructure (gh#1372)

Documents schema 2026.5.dev8: output_file becomes the output sibling
sub-object, path -> dir, legacy string form dropped. CHANGELOG flags
the breaking YAML restructure with the suews-convert escape hatch."
```

---

## Task 9: Sweep test assertion updates

**Files:**
- Modify: `test/io_tests/test_output_config.py`
- Modify: `test/io_tests/test_yaml_annotation.py`
- Modify: `test/data_model/test_yaml_processing.py`
- Modify: `test/data_model/test_validation.py`
- Modify: `test/core/test_load.py`
- Modify: `test/core/test_sample_output.py`
- Modify: `test/core/test_cli_validation.py`
- Modify: `test/core/test_cli_conversion.py`
- Modify: `test/cmd/test_df_state_conversion.py`
- Modify: `test/umep/test_processor.py`
- Modify: `test/umep/test_postprocessor.py`

- [ ] **Step 1: For each file, update assertions**

For every file in the list, rewrite:

- `output_file` → `output` (when referring to the typed-config field)
- `OutputConfig` → `OutputControl` (when referring to the class)
- `.path` → `.dir` (when accessing the field, not the unrelated `os.path` module!)

Be discriminating about `.path`: only rewrite where the surrounding context is `OutputConfig.path` / `output_file.path`. Run `grep -B2 -A2 "\.path" <file>` first.

For tests that pass YAML dicts inline:

```python
{"output_file": {"format": "txt", "path": "./out"}}
```

→

```python
{"output": {"format": "txt", "dir": "./out"}}
```

- [ ] **Step 2: Run the full test suite**

```bash
uv run pytest test/io_tests/ test/data_model/ test/core/ test/cmd/ test/umep/ -x 2>&1 | tail -40
```

Expected: all pass. If a test fails on a YAML-string assertion (e.g. asserting that a stringified config contains `"output_file"`), update the expected string to `"output"`.

- [ ] **Step 3: Run smoke + targeted upgrade test**

```bash
make test-smoke 2>&1 | tail -20
uv run pytest test/data_model/test_yaml_upgrade.py test/data_model/test_release_compat.py -v 2>&1 | tail -40
```

Expected: passes.

- [ ] **Step 4: Commit**

```bash
git add test/
git commit -m "test: sweep assertions for output sub-object rename (gh#1372)

Updates every test asserting on output_file / OutputConfig / .path to
use output / OutputControl / .dir. Fixture YAMLs already migrated in
Task 7; this task covers Python-side assertions."
```

---

## Task 10: Final verification

- [ ] **Step 1: Schema-version-audit lint**

```bash
uv run python /Users/tingsun/conductor/workspaces/suews/porto-v2/scripts/lint/check_schema_version_bump.py 2>&1 | tail -20
```

Expected: passes (data-model touched + schema version bumped + docs touched).

- [ ] **Step 2: Smoke test**

```bash
make test-smoke 2>&1 | tail -20
```

Expected: passes.

- [ ] **Step 3: Full test suite (longer)**

```bash
uv run pytest test/ -x --ignore=test/integration 2>&1 | tail -30
```

Expected: passes. Address any remaining failures inline (likely missed `.path` access in unfamiliar files; treat each as a Task 5/9 follow-up).

- [ ] **Step 4: Final orphan-symbol grep**

```bash
grep -rn "OutputConfig\b\|\.output_file\b" src/ test/ docs/ 2>&1 | grep -v "\.pyc:" | head -30
```

Expected: no source-code hits. The only remaining matches should be (a) CHANGELOG / transition_guide narrative referring to the legacy name, (b) `cmd/` CLI parameter `output_file` (a file-path argument).

- [ ] **Step 5: Verify docs build**

```bash
make docs 2>&1 | tail -10
```

Expected: passes.

---

## Self-Review

**Spec coverage:** ✓ §2.1 lifted to T1; §2.2 (path → dir) covered in T1+T7+T9; §2.3 class shape implemented in T1; §2.4 in-memory coercion in T1; §3.1 version bump in T2; §3.2 migration handler in T3; §3.3 vendored fixture in T4; §4.1 typed-caller sweep in T5; §4.2 raw-YAML reader sweep in T6; §4.3 fixtures in T7; §4.4 docs in T8; §5 testing covered across T1, T3, T9 with verification in T10.

**Placeholder scan:** ✓ No "TBD/TODO/handle edge cases" placeholders. Each file modification step provides exact code or exact grep + replace pattern. The audit step in Task 5 ("inspect each match. Replace only the typed-config matches") is a deliberate decision criterion, not a placeholder — the criterion is concrete.

**Type consistency:** ✓ `OutputControl`, `output`, `dir`, `_coerce_legacy_output_file`, `_apply_output_subobject_restructure`, `_migrate_2026_5_dev7_to_current`, schema label `2026.5.dev8` — all spelled identically across tasks.
