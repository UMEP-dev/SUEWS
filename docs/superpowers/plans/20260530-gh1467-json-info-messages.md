# gh1467 — Informational messages in the validate JSON sidecar — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make the `suews validate` JSON sidecar (`report_<name>.json`) carry non-error informational messages, not just validation errors, by writing the consolidated multi-phase `ValidationReport` to it.

**Architecture:** A single helper in `src/supy/cmd/validate_config.py`, called from the existing `_emit_pipeline_result` funnel, serialises `ValidationReport(phases).to_dict()` (all phases, all severities) to `Path(report_path).with_suffix(".json")`. This supersedes the Phase-C-only `PhaseReport` sidecar that `run_phase_c` leaves at that path. The payload is identical to the stdout `--format json` envelope's `data.validation_report`. Per-phase sidecars written by direct `run_phase_*` library calls are untouched.

**Tech stack:** Python 3.9+, Click CLI, pytest (`pytest.mark.api`), the existing `report_schema.py` dataclasses (`Issue`, `PhaseReport`, `ValidationReport`, `JSON_REPORT_WRITER`).

**Spec:** `docs/superpowers/specs/20260530-gh1467-json-info-messages-design.md`

**Pre-verified empirical facts (do not re-derive):**
- Running `suews validate --forcing off <cfg>` writes the final report at `<cfg_dir>/report_<stem>.txt` and the sidecar at `<cfg_dir>/report_<stem>.json` (from `setup_output_paths`: `final_report = dirname / f"report_{name_without_ext}.txt"`, used as `pydantic_report_file` in ABC and as the `report_path` passed to `_emit_pipeline_result`).
- `sample_config.yml` with `sites[0].properties.h_std` removed → run **succeeds (exit 0)**; Phase A emits exactly one non-error issue `A.MISSING_PARAM` (severity `WARNING`, `yaml_path` contains `h_std`); the text report shows it under `## INFO` (`-- sites[0].properties.h_std added to the updated YAML and set to null`); the **current** sidecar is `{phase:"C", status:"PASSED", issues:[]}` — the message is absent. This is the canonical gh#1467 reproduction.
- The bundled `sample_config.yml` is **not** issue-free: Phase B emits many `SUGGESTION`/`WARNING`/`PASS` issues (some CRU-derived, hence environment-dependent). **Tests must anchor on the deterministic `A.MISSING_PARAM`/`h_std` signal, never on "zero issues" or on CRU-derived suggestions.**
- `sample_config.yml` with `h_std` removed **and** `sites[0].properties.bogus_extra_param: 42` added → exit 1; aggregated `ValidationReport` has `overall_status == "FAILED"`, contains a `C.PYDANTIC.*` error and the `A.MISSING_PARAM` warning, phases `["A","B","C"]`.
- `json`, `os`, `sys`, and `Path` are already imported at the top of `src/supy/cmd/validate_config.py`.
- The worktree venv is built. Run all commands after `source .venv/bin/activate` from the worktree root `/Users/tingsun/conductor/suews/.claude/worktrees/gh1467-json-info-messages`.

---

## File Structure

- **Modify** `src/supy/cmd/validate_config.py`
  - Add `_write_consolidated_sidecar(phases, report_path)` (new top-level function, placed immediately above `_emit_pipeline_result`).
  - Call it inside `_emit_pipeline_result`, right after `ok = not has_errors`, so it runs in both `table` and `json` output modes.
- **Modify** `src/supy/data_model/validation/pipeline/report_schema.py`
  - Update the module docstring to state the consolidated CLI sidecar is a `ValidationReport`, while per-phase sidecars remain `PhaseReport`.
- **Modify** `test/cmd/test_validate_config.py`
  - Add three CLI tests and one `_emit_pipeline_result` sidecar-write test (this file already uses `CliRunner`, `tmp_path`, `yaml`, `json`, `copy`, `pytest.mark.api`, and already exercises `_emit_pipeline_result`).
- **Modify** `test/data_model/test_pipeline_structured_output.py`
  - Add one `_write_consolidated_sidecar` unit test (valid write + swallowed failure) and update the module docstring's contract sentence.
- **Modify** `docs/source/inputs/yaml/validation.rst`
  - Document the `report_<name>.json` sidecar in the "Output Files" list and note it now carries non-error informational messages.
- **Modify** `CHANGELOG.md`
  - Add a `[feature]` entry under a new `### 30 May 2026` subsection.

No `CURRENT_SCHEMA_VERSION` bump: this does not touch `src/supy/data_model/` config models (per `.claude/rules/python/schema-versioning.md`).

---

### Task 1: Failing CLI test — sidecar must carry the non-error informational message

**Files:**
- Test: `test/cmd/test_validate_config.py` (append new test function)

- [ ] **Step 1: Write the failing test**

Append to `test/cmd/test_validate_config.py`:

```python
def test_validate_sidecar_includes_non_error_info_messages(tmp_path: Path) -> None:
    """gh#1467: the consolidated CLI JSON sidecar carries non-error issues.

    Dropping the optional ``sites[0].properties.h_std`` makes the run
    succeed (exit 0) while Phase A records a non-error ``A.MISSING_PARAM``
    warning. That informational message must appear in the JSON sidecar,
    not only in the text report. Anchored on the deterministic ``h_std``
    signal (the sample config's Phase B issues are environment-dependent).
    """
    from supy.cmd.validate_config import cli as validate_cli

    with _sample_yaml_path() as sample:
        data = yaml.safe_load(sample.read_text(encoding="utf-8"))
    data["sites"][0]["properties"].pop("h_std")

    config_path = tmp_path / "myconfig.yml"
    _write_yaml(config_path, data)

    runner = CliRunner()
    result = runner.invoke(validate_cli, ["--forcing", "off", str(config_path)])
    assert result.exit_code == 0, result.output

    sidecar = tmp_path / "report_myconfig.json"
    assert sidecar.exists(), "sidecar JSON not written"
    payload = json.loads(sidecar.read_text(encoding="utf-8"))

    # Consolidated sidecar is a multi-phase ValidationReport (gh#1467),
    # not a single Phase-C PhaseReport.
    assert "overall_status" in payload, payload.keys()
    assert isinstance(payload.get("phases"), list)
    assert {p["phase"] for p in payload["phases"]} >= {"A", "B", "C"}

    all_issues = [i for p in payload["phases"] for i in p["issues"]]
    assert any(
        i["code"] == "A.MISSING_PARAM"
        and "h_std" in (i.get("yaml_path") or "")
        and i["severity"] != "ERROR"
        for i in all_issues
    ), f"h_std informational issue missing from sidecar: {all_issues}"
```

Note: `_sample_yaml_path` and `_write_yaml` are existing helpers in this test module; reuse them rather than re-importing the sample path.

- [ ] **Step 2: Run the test to verify it fails**

Run: `source .venv/bin/activate && python -m pytest test/cmd/test_validate_config.py::test_validate_sidecar_includes_non_error_info_messages -v`
Expected: FAIL — the current sidecar is `{"phase":"C","status":"PASSED","issues":[]}`, so `assert "overall_status" in payload` raises `AssertionError` (and the `h_std` assertion would also fail).

- [ ] **Step 3: Commit the failing test**

```bash
git add test/cmd/test_validate_config.py
git commit -m "test(validate): assert JSON sidecar carries non-error info (gh#1467)

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

### Task 2: Implement `_write_consolidated_sidecar` and wire it into `_emit_pipeline_result`

**Files:**
- Modify: `src/supy/cmd/validate_config.py` (add helper above `_emit_pipeline_result` at line ~950; add one call inside `_emit_pipeline_result` after `ok = not has_errors`)

- [ ] **Step 1: Add the helper function**

Insert this function immediately **above** `def _emit_pipeline_result(` in `src/supy/cmd/validate_config.py`:

```python
def _write_consolidated_sidecar(phases: list, report_path) -> None:
    """Write the consolidated multi-phase ``ValidationReport`` JSON sidecar.

    gh#1467: the CLI sidecar (``<report>.json``) carries the full
    multi-phase ``ValidationReport`` (every phase, every severity) so
    non-error informational messages reach machine consumers, not just
    validation errors. The payload is identical to the stdout
    ``--format json`` envelope's ``data.validation_report``.

    This supersedes the Phase-C-only ``PhaseReport`` sidecar that
    ``run_phase_c`` (and the move/copy helpers) leave at this path during
    the pipeline run. A serialisation or I/O failure is swallowed (with a
    ``SUEWS_DEBUG`` note) so a sidecar problem never breaks validation,
    mirroring ``_sync_report_json_paths``.

    Parameters
    ----------
    phases : list
        The ``PhaseReport`` objects for the phases that ran, in order.
    report_path : str or pathlib.Path
        Final text report path; the sidecar is written beside it with a
        ``.json`` suffix.
    """
    if not report_path:
        return

    from ..data_model.validation.pipeline.report_schema import ValidationReport

    json_path = Path(report_path).with_suffix(".json")
    try:
        payload = ValidationReport(phases=list(phases)).to_dict()
        json_path.write_text(
            json.dumps(payload, indent=2, ensure_ascii=False) + "\n",
            encoding="utf-8",
        )
    except (OSError, TypeError, ValueError) as exc:
        if os.environ.get("SUEWS_DEBUG", "").lower() in ("1", "true", "yes"):
            print(
                f"[DEBUG] consolidated sidecar write failed for {json_path}: {exc}",
                file=sys.stderr,
            )
```

- [ ] **Step 2: Call the helper inside `_emit_pipeline_result`**

In `_emit_pipeline_result`, find:

```python
    has_errors = any(p.has_errors for p in phases)
    ok = not has_errors

    if out_format == "json":
```

Replace with:

```python
    has_errors = any(p.has_errors for p in phases)
    ok = not has_errors

    # gh#1467: persist the consolidated multi-phase ValidationReport as the
    # JSON sidecar next to the final text report, in BOTH table and json
    # output modes (the sidecar is a disk artefact, independent of stdout).
    _write_consolidated_sidecar(phases, report_path)

    if out_format == "json":
```

- [ ] **Step 3: Run the Task 1 test to verify it passes**

Run: `source .venv/bin/activate && python -m pytest test/cmd/test_validate_config.py::test_validate_sidecar_includes_non_error_info_messages -v`
Expected: PASS.

- [ ] **Step 4: Commit**

```bash
git add src/supy/cmd/validate_config.py
git commit -m "feat(validate): consolidated JSON sidecar carries all phases/severities (gh#1467)

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

### Task 3: Add the clean-config shape test and the error+info coexistence test

**Files:**
- Modify: `test/cmd/test_validate_config.py` (append two tests)

- [ ] **Step 1: Write both tests**

Append to `test/cmd/test_validate_config.py`:

```python
def test_validate_sidecar_is_validation_report_for_clean_config(tmp_path: Path) -> None:
    """The consolidated CLI sidecar is a multi-phase ValidationReport even
    for the bundled sample config (which already carries Phase B notes)."""
    from supy.cmd.validate_config import cli as validate_cli

    with _sample_yaml_path() as sample:
        config_path = tmp_path / "clean.yml"
        config_path.write_text(sample.read_text(encoding="utf-8"), encoding="utf-8")

    runner = CliRunner()
    result = runner.invoke(validate_cli, ["--forcing", "off", str(config_path)])
    assert result.exit_code == 0, result.output

    payload = json.loads((tmp_path / "report_clean.json").read_text(encoding="utf-8"))
    assert payload["overall_status"] in {"PASSED", "WARNING"}
    assert {p["phase"] for p in payload["phases"]} >= {"A", "B", "C"}
    # Each phase entry preserves the PhaseReport shape.
    for phase_entry in payload["phases"]:
        assert {"phase", "status", "issues"} <= set(phase_entry)


def test_validate_sidecar_keeps_info_alongside_errors(tmp_path: Path) -> None:
    """A failing config keeps Phase A informational issues beside Phase C
    errors in the same consolidated sidecar."""
    from supy.cmd.validate_config import cli as validate_cli

    with _sample_yaml_path() as sample:
        data = yaml.safe_load(sample.read_text(encoding="utf-8"))
    data["sites"][0]["properties"].pop("h_std")               # -> A.MISSING_PARAM (info)
    data["sites"][0]["properties"]["bogus_extra_param"] = 42  # -> C.PYDANTIC error

    config_path = tmp_path / "bad.yml"
    _write_yaml(config_path, data)

    runner = CliRunner()
    result = runner.invoke(validate_cli, ["--forcing", "off", str(config_path)])
    assert result.exit_code != 0, result.output

    payload = json.loads((tmp_path / "report_bad.json").read_text(encoding="utf-8"))
    assert payload["overall_status"] == "FAILED"
    all_issues = [i for p in payload["phases"] for i in p["issues"]]
    assert any(i["code"].startswith("C.PYDANTIC") for i in all_issues), all_issues
    assert any(i["code"] == "A.MISSING_PARAM" for i in all_issues), all_issues
```

- [ ] **Step 2: Run both tests**

Run: `source .venv/bin/activate && python -m pytest "test/cmd/test_validate_config.py::test_validate_sidecar_is_validation_report_for_clean_config" "test/cmd/test_validate_config.py::test_validate_sidecar_keeps_info_alongside_errors" -v`
Expected: PASS (both).

- [ ] **Step 3: Commit**

```bash
git add test/cmd/test_validate_config.py
git commit -m "test(validate): cover clean-config shape and error+info coexistence (gh#1467)

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

### Task 4: Unit tests for the sidecar write (success + swallowed failure)

**Files:**
- Modify: `test/cmd/test_validate_config.py` (append one `_emit_pipeline_result` sidecar test)
- Modify: `test/data_model/test_pipeline_structured_output.py` (append one swallowed-failure test)

- [ ] **Step 1: Write the `_emit_pipeline_result` sidecar-write test**

Append to `test/cmd/test_validate_config.py`:

```python
def test_emit_pipeline_result_writes_consolidated_sidecar(
    tmp_path: Path,
    capsys: pytest.CaptureFixture[str],
) -> None:
    """gh#1467: `_emit_pipeline_result` writes the consolidated
    ValidationReport sidecar beside the report, in both output modes."""
    from supy.cmd.validate_config import _emit_pipeline_result
    from supy.data_model.validation.pipeline.report_schema import (
        Issue,
        PhaseReport,
        SEVERITY_INFO,
        SEVERITY_ERROR,
    )

    phases = [
        PhaseReport(
            phase="A",
            issues=[Issue(phase="A", severity=SEVERITY_INFO, code="A.INFO.NOTE",
                          message="informational note", yaml_path="model.physics")],
        ),
        PhaseReport(
            phase="C",
            issues=[Issue(phase="C", severity=SEVERITY_ERROR, code="C.PYDANTIC.X",
                          message="bad value", yaml_path="sites.1.properties.lat")],
        ),
    ]
    report_path = tmp_path / "report_x.txt"

    exit_code = _emit_pipeline_result(
        phases=phases,
        report_path=report_path,
        yaml_path=tmp_path / "updated_x.yml",
        out_format="table",
        command="suews validate",
        started_at="2026-05-30T00:00:00Z",
    )
    assert exit_code == 1  # has an ERROR

    sidecar = tmp_path / "report_x.json"
    assert sidecar.exists()
    payload = json.loads(sidecar.read_text(encoding="utf-8"))
    assert payload["overall_status"] == "FAILED"
    assert {p["phase"] for p in payload["phases"]} == {"A", "C"}
    info_codes = [i["code"] for p in payload["phases"] for i in p["issues"]]
    assert "A.INFO.NOTE" in info_codes  # non-error info survived
    assert "C.PYDANTIC.X" in info_codes
    capsys.readouterr()  # drain captured table output
```

- [ ] **Step 2: Write the swallowed-failure unit test**

Append to `test/data_model/test_pipeline_structured_output.py`:

```python
def test_write_consolidated_sidecar_swallows_io_failure(tmp_path):
    """gh#1467: a sidecar write to an unwritable path must not raise."""
    from supy.cmd.validate_config import _write_consolidated_sidecar
    from supy.data_model.validation.pipeline.report_schema import PhaseReport

    # Parent directory does not exist -> write raises OSError, swallowed.
    bad_report = tmp_path / "missing_dir" / "report_x.txt"
    _write_consolidated_sidecar([PhaseReport(phase="C", issues=[])], str(bad_report))
    assert not (tmp_path / "missing_dir" / "report_x.json").exists()


def test_write_consolidated_sidecar_writes_validation_report(tmp_path):
    """gh#1467: the helper serialises a ValidationReport for a phases list."""
    import json as _json
    from supy.cmd.validate_config import _write_consolidated_sidecar
    from supy.data_model.validation.pipeline.report_schema import (
        Issue,
        PhaseReport,
        SEVERITY_INFO,
    )

    phases = [
        PhaseReport(phase="A", issues=[
            Issue(phase="A", severity=SEVERITY_INFO, code="A.X", message="hi"),
        ]),
        PhaseReport(phase="C", issues=[]),
    ]
    report = tmp_path / "report_y.txt"
    _write_consolidated_sidecar(phases, str(report))

    payload = _json.loads((tmp_path / "report_y.json").read_text(encoding="utf-8"))
    assert payload["overall_status"] == "PASSED"  # INFO is neither error nor warning
    assert {p["phase"] for p in payload["phases"]} == {"A", "C"}
    assert payload["phases"][0]["issues"][0]["code"] == "A.X"
```

- [ ] **Step 3: Run the unit tests**

Run: `source .venv/bin/activate && python -m pytest "test/cmd/test_validate_config.py::test_emit_pipeline_result_writes_consolidated_sidecar" "test/data_model/test_pipeline_structured_output.py::test_write_consolidated_sidecar_swallows_io_failure" "test/data_model/test_pipeline_structured_output.py::test_write_consolidated_sidecar_writes_validation_report" -v`
Expected: PASS (all three).

- [ ] **Step 4: Commit**

```bash
git add test/cmd/test_validate_config.py test/data_model/test_pipeline_structured_output.py
git commit -m "test(validate): unit-cover consolidated sidecar write + failure swallow (gh#1467)

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

### Task 5: Update the sidecar-contract docstrings

**Files:**
- Modify: `src/supy/data_model/validation/pipeline/report_schema.py` (module docstring, lines ~14-18)
- Modify: `test/data_model/test_pipeline_structured_output.py` (module docstring, lines ~1-7)

- [ ] **Step 1: Update `report_schema.py` module docstring**

Find this paragraph at the top of `src/supy/data_model/validation/pipeline/report_schema.py`:

```python
The text reports written by each phase are unchanged; this module
adds a JSON sidecar (``<report>.json``) next to every ``<report>.txt``
so downstream tooling (MCP, agents, CI) can consume the validator
output without parsing the human-readable form.
```

Replace with:

```python
The text reports written by each phase are unchanged; this module
adds a JSON sidecar (``<report>.json``) next to every ``<report>.txt``
so downstream tooling (MCP, agents, CI) can consume the validator
output without parsing the human-readable form.

Two sidecar shapes exist (gh#1467):

- A direct ``run_phase_A/B/C`` library call writes a single
  ``PhaseReport`` for that phase.
- The full ``suews validate`` CLI run writes a consolidated
  ``ValidationReport`` (``{overall_status, phases:[...]}``) at the final
  report path, aggregating every phase's issues across all severities so
  non-error informational messages are exposed, not only errors. This is
  identical to the stdout ``--format json`` envelope's
  ``data.validation_report``.
```

- [ ] **Step 2: Update `test_pipeline_structured_output.py` module docstring**

Find at the top of `test/data_model/test_pipeline_structured_output.py`:

```python
Each phase (A, B, C) writes a ``<report>.json`` sidecar next to the
existing ``<report>.txt``. The sidecar conforms to ``PhaseReport.to_dict()``
from ``report_schema.py`` and is the canonical machine-readable
representation of the phase's findings.
```

Replace with:

```python
Each phase (A, B, C) writes a ``<report>.json`` sidecar next to the
existing ``<report>.txt``. A direct ``run_phase_*`` call writes a
``PhaseReport``; the full ``suews validate`` CLI run writes a
consolidated ``ValidationReport`` (gh#1467, covered in
``test/cmd/test_validate_config.py``). Both are the canonical
machine-readable representation of the validator's findings.
```

- [ ] **Step 3: Run the structured-output and report-schema tests to confirm no regression**

Run: `source .venv/bin/activate && python -m pytest test/data_model/test_pipeline_structured_output.py test/data_model/test_report_schema.py -v`
Expected: PASS (docstring edits do not change behaviour; the new helper tests added in Task 4 also pass).

- [ ] **Step 4: Commit**

```bash
git add src/supy/data_model/validation/pipeline/report_schema.py test/data_model/test_pipeline_structured_output.py
git commit -m "docs(validate): clarify per-phase vs consolidated sidecar contract (gh#1467)

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

### Task 6: User documentation and CHANGELOG

**Files:**
- Modify: `docs/source/inputs/yaml/validation.rst` (Output Files list, lines ~78-82)
- Modify: `CHANGELOG.md` (new dated subsection under `## 2026`)

- [ ] **Step 1: Document the sidecar in `validation.rst`**

Find in `docs/source/inputs/yaml/validation.rst`:

```rst
When you run ``suews-validate config.yml``, it creates:

- ``updated_config.yml`` - the updated configuration from the last successful
  validation phase
- ``report_config.txt`` - the consolidated validation report
```

Replace with:

```rst
When you run ``suews-validate config.yml``, it creates:

- ``updated_config.yml`` - the updated configuration from the last successful
  validation phase
- ``report_config.txt`` - the consolidated validation report
- ``report_config.json`` - the machine-readable sidecar. It is a consolidated
  ``ValidationReport`` (``{overall_status, phases:[...]}``) carrying every
  phase's findings across all severities (``ERROR``, ``WARNING``, ``INFO``,
  ``SUGGESTION``, ``APPLIED_FIX``, ``PASS``), so non-error informational
  messages are available to tooling, not just validation errors. This matches
  the ``data.validation_report`` field of the ``--format json`` output.
```

- [ ] **Step 2: Add the CHANGELOG entry**

In `CHANGELOG.md`, find the line:

```markdown
### 29 May 2026
```

Insert **immediately above** it:

```markdown
### 30 May 2026

- [feature] Expand `suews validate` JSON report to include non-error informational messages (#1467)
  - The JSON sidecar (`report_<name>.json`) written next to the text report is now the consolidated multi-phase `ValidationReport` (`{overall_status, phases:[...]}`), carrying every phase's issues across all severities (`ERROR`, `WARNING`, `INFO`, `SUGGESTION`, `APPLIED_FIX`, `PASS`), not just validation errors. This matches the `data.validation_report` field already exposed by `--format json`. Per-phase sidecars written by direct `run_phase_*` library calls are unchanged (still `PhaseReport`).

```

- [ ] **Step 3: Verify the docs build does not error on the edited file (lint-level check)**

Run: `source .venv/bin/activate && python -c "import docutils.parsers.rst" 2>/dev/null && echo "docutils present"`
Then confirm no stray non-ASCII was introduced: `grep -nP "[^\x00-\x7F]" docs/source/inputs/yaml/validation.rst || echo "ASCII-clean"`
Expected: `ASCII-clean` (the edit uses ASCII only).

- [ ] **Step 4: Commit**

```bash
git add docs/source/inputs/yaml/validation.rst CHANGELOG.md
git commit -m "doc(validate): document JSON sidecar informational messages (gh#1467)

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

### Task 7: Regression sweep, lint, and final verification

**Files:** none (verification only)

- [ ] **Step 1: Run the full validate/sidecar regression set**

Run:
```bash
source .venv/bin/activate && python -m pytest \
  test/cmd/test_validate_config.py \
  test/data_model/test_pipeline_structured_output.py \
  "test/data_model/test_validation.py::test_validate_cli_success_removes_temp_report_json_sidecars" \
  "test/data_model/test_validation.py::test_validate_cli_failure_removes_temp_report_json_sidecars" \
  -v
```
Expected: ALL PASS. In particular the two existing existence tests still pass (the sidecar file still exists at `report_myconfig.json`; no `temp_report{A,B}_*.json` leak).

- [ ] **Step 2: Lint the changed Python**

Run: `source .venv/bin/activate && ruff check src/supy/cmd/validate_config.py test/cmd/test_validate_config.py test/data_model/test_pipeline_structured_output.py`
Expected: no new findings (the diff uses `Optional`-free signatures and existing imports; no PEP 604 unions introduced).

- [ ] **Step 3: Smoke the real CLI end-to-end (sanity, outside pytest)**

Run:
```bash
source .venv/bin/activate
python - <<'PY'
import json, tempfile, copy
from pathlib import Path
import yaml, supy as sp
from click.testing import CliRunner
from supy.cmd.validate_config import cli
sample = Path(sp.__file__).parent / "sample_data" / "sample_config.yml"
data = yaml.safe_load(sample.read_text()); data["sites"][0]["properties"].pop("h_std")
r = CliRunner()
with r.isolated_filesystem() as tmp:
    cfg = Path(tmp)/"myconfig.yml"; cfg.write_text(yaml.safe_dump(data))
    res = r.invoke(cli, ["--forcing","off",str(cfg)])
    p = json.loads((Path(tmp)/"report_myconfig.json").read_text())
    print("exit", res.exit_code, "| overall", p["overall_status"],
          "| phases", [ph["phase"] for ph in p["phases"]],
          "| h_std?", any("h_std" in (i.get("yaml_path") or "")
                          for ph in p["phases"] for i in ph["issues"]))
PY
```
Expected: `exit 0 | overall WARNING | phases ['A', 'B', 'C'] | h_std? True`.

- [ ] **Step 4: Final state check**

Run: `git -C /Users/tingsun/conductor/suews/.claude/worktrees/gh1467-json-info-messages log --oneline -7` and `git status`
Expected: the six task commits present (plus the spec commit), working tree clean.

---

## Self-Review

**Spec coverage:**
- "Single-point fix in `_emit_pipeline_result`" → Task 2.
- "Consolidated sidecar = `ValidationReport`, identical to envelope" → Task 2 helper + Task 1/3/4 assertions.
- "Both `table` and `json` modes write the sidecar" → Task 2 call site (before the `out_format` branch) + Task 4 `table`-mode test.
- "Scope: only `_execute_pipeline` path; `validate` subcommand and `--dry-run` untouched" → only `_emit_pipeline_result` is modified; no change to `validate`/`--dry-run` code.
- "No `CURRENT_SCHEMA_VERSION` bump" → no `src/supy/data_model/` change; stated in File Structure.
- "Accepted nuance: structured severity vs text grouping" → reflected in tests anchoring on `A.MISSING_PARAM` = `WARNING`.
- "Defensive error handling, swallow + debug note" → Task 2 helper + Task 4 swallow test.
- Test matrix (info present / clean shape / error+info / unit + swallow / regression) → Tasks 1, 3, 4, 7.
- Contract docstrings → Task 5. CHANGELOG + docs → Task 6.

**Placeholder scan:** none — every step has concrete code/commands and verified expected output.

**Type/name consistency:** `_write_consolidated_sidecar(phases, report_path)` is defined in Task 2 and called identically in Task 2's call site and in Tasks 4's unit tests. `ValidationReport`, `PhaseReport`, `Issue`, `SEVERITY_INFO`, `SEVERITY_ERROR` are imported from `report_schema.py` where used. Sidecar filenames (`report_myconfig.json`, `report_clean.json`, `report_bad.json`, `report_x.json`, `report_y.json`) consistently match each test's config stem.
