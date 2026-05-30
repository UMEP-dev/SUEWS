# gh1467 — Expand the validate JSON sidecar to include non-error informational messages

## Problem

Running `suews validate <config>.yml` writes two artefacts next to each other:

- `report_<name>.txt` — the human-readable consolidated validation report, which has sections such as `## ACTION NEEDED`, `## SUGGESTED UPDATES`, `## APPLIED UPDATES`, and `## INFO`.
- `report_<name>.json` — a machine-readable sidecar.

Today the sidecar only carries validation **errors**. Any non-error informational content (missing optional parameters interpreted as defaults, applied renames, scientific suggestions, warnings) survives only in the text report. Downstream consumers that read the JSON (CI, agents, tooling) therefore cannot see information the human report shows. Issue gh#1467 asks for the JSON to include these non-error informational messages too.

## Root cause (verified by reading the pipeline)

- Every phase already builds fully-typed structured `Issue` objects across the whole severity vocabulary defined in `report_schema.py`: `ERROR`, `WARNING`, `INFO`, `SUGGESTION`, `APPLIED_FIX`, `PASS`.
  - Phase A: `_issues_from_phase_a_state` emits `WARNING` (missing optional / extra params, nlayer limits), `APPLIED_FIX` (renames), `ERROR` (dimension / forcing / critical-missing).
  - Phase B: `run_science_check` emits issues whose severity derives from each rule's status (`INFO`/`SUGGESTION`/`APPLIED_FIX`/`WARNING`/`PASS`).
  - Phase C: `collect_phase_c_issues` emits **only** `ERROR` (structured Pydantic failures).
- The stdout `--format json` envelope already serialises the **complete** multi-phase report: `_emit_pipeline_result` builds `ValidationReport(phases=list(phases))` and emits `validation_report.to_dict()`, which includes every issue of every severity.
- The on-disk **sidecar file** is the only gap. In the consolidated CLI run it is written by `run_phase_c` from `collect_phase_c_issues()` alone, so it is errors-only by construction. The rich Phase A/B issues are discarded from the file even though they exist in memory and in the envelope.

## Decisions (confirmed with the maintainer)

1. **Approach: aggregate structured `Issue`s.** Carry the structured issues (all severities) from every phase into the sidecar, rather than re-parsing the text report's prose. The whole point of the JSON is to be more processable than the text, so the structured form (severity + stable `code` + `yaml_path`) is preferred over verbatim prose.
2. **Shape: the consolidated CLI sidecar becomes a `ValidationReport`.** Top-level shape `{overall_status, phases: [{phase, status, issues, ...}]}`, identical to what the stdout `--format json` envelope already carries under `data.validation_report`. This unifies the two JSON surfaces, preserves per-phase provenance, and reuses the existing schema.

## Design

### Single-point change: `_emit_pipeline_result` owns the consolidated sidecar

`_emit_pipeline_result` (in `src/supy/cmd/validate_config.py`) is the single funnel every pipeline branch returns through. It already receives:

- `phases` — the in-memory `PhaseReport` list for the phases that ran (A/B/C). Their `issues` lists are plain Python objects and survive the on-disk file cleanup that happens earlier in `_execute_pipeline`.
- `report_path` — the final consolidated text report path.

It will additionally write `ValidationReport(phases=list(phases)).to_dict()` to `Path(report_path).with_suffix(".json")`, in **both** `table` and `json` output modes (the sidecar file is a disk artefact independent of the stdout format). This single write supersedes the Phase-C-only sidecar that `run_phase_c` and the `move_report_with_json` / `copy_report_with_json` helpers leave at that path.

A small helper, e.g. `_write_consolidated_sidecar(phases, report_path)`, encapsulates the write so both the success and failure branches share one implementation and it is unit-testable in isolation.

### Resulting contract (clear mental model)

- **CLI sidecar** (`suews validate …`, i.e. the `_execute_pipeline` path) = `ValidationReport`:
  ```json
  {
    "overall_status": "WARNING",
    "phases": [
      {"phase": "A", "status": "WARNING", "issues": [
        {"phase": "A", "severity": "WARNING", "code": "A.MISSING_PARAM",
         "message": "Missing parameter: ...", "yaml_path": "...", ...}
      ], "yaml_in": "...", "yaml_out": "...", "text_report_path": "...", "json_report_path": "...", "extra": {}},
      {"phase": "B", "status": "PASSED", "issues": [...]},
      {"phase": "C", "status": "PASSED", "issues": []}
    ]
  }
  ```
- **Library sidecar** (direct `run_phase_A/B/C()` calls) = `PhaseReport`, unchanged.

### Scope boundaries

- Only the default pipeline path (`_execute_pipeline`) writes report files, so only it is touched. The `validate` subcommand (batch schema check) and `--dry-run` emit stdout envelopes / Rich tables only — no `report_*.json` files — and remain untouched.
- No `CURRENT_SCHEMA_VERSION` bump: per `.claude/rules/python/schema-versioning.md`, that governs the YAML *config* schema under `src/supy/data_model/`, not this report artefact. The change does not touch the config data model.
- The per-phase `run_phase_*` functions keep writing their `PhaseReport` sidecars (needed for direct library calls and harmless during the pipeline, where the final write supersedes them). No flag is added to suppress the intermediate write; the modest double-write at the final path is negligible.

### Accepted nuance

The JSON's informational content is the structured-issue view, which is terser than, and classified slightly differently from, the text report's prose grouping. For example a missing optional parameter is `severity: WARNING` with `code: A.MISSING_PARAM` in the structured form, while the text report lists it under `## INFO` as a "interpret as default value" sentence. This divergence already exists in the per-phase sidecars today, so the consolidated sidecar is consistent with the established structured representation. The text report is unchanged.

### Error handling

The sidecar write is wrapped defensively: a serialisation or `OSError` failure is swallowed (with a `SUEWS_DEBUG` note to stderr) so a sidecar problem never breaks validation. This matches the existing `_sync_report_json_paths` pattern, which deliberately swallows sidecar I/O errors.

## Testing

New/updated tests under `test/data_model/` (module marker `pytest.mark.api`):

1. **Informational content present (CLI, ABC):** a config that triggers a missing optional parameter and a renamed parameter → invoke `suews validate --forcing off <config>` via the Click runner → assert the sidecar:
   - parses as a `ValidationReport` (`overall_status` present, `phases` is a list);
   - includes phases A, B, C;
   - carries at least one non-error issue (severity in `{INFO, WARNING, APPLIED_FIX, SUGGESTION, PASS}`).
2. **Clean sample config:** sidecar is a `ValidationReport`, `phases` present, no `ERROR` issues.
3. **Failing config:** `overall_status == "FAILED"`, Phase C has ≥1 `C.PYDANTIC.*` error, **and** Phase A informational issues are still present (proves errors and info coexist).
4. **Regression:** existing existence tests (`test_validation.py::test_validate_cli_success_removes_temp_report_json_sidecars` and `..._failure_...`) continue to pass (the file still exists; no `temp_report{A,B}_*.json` leak).
5. **Unit:** `_write_consolidated_sidecar` writes valid `ValidationReport` JSON for a hand-built phases list, and swallows a write failure (e.g. unwritable path) without raising.
6. **Contract docstrings:** update `report_schema.py` module docstring and `test_pipeline_structured_output.py` docstring to state: per-phase sidecars are `PhaseReport`; the consolidated CLI sidecar is a `ValidationReport`.

## Documentation / changelog

- Add a CHANGELOG.md entry (user-visible behaviour change).
- Check `docs/source` for any page documenting the validate report sidecar format; if present, update it to describe the `ValidationReport` shape and that non-error informational messages now appear. If absent, no doc change is required.

## Out of scope

- Re-parsing or restructuring the human-readable text report (unchanged).
- The `--dry-run` and `validate` subcommand stdout envelopes (already complete).
- Any change to the YAML config schema or `CURRENT_SCHEMA_VERSION`.
