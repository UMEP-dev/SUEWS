"""Unified in-memory and staged SUEWS configuration validation engine.

Provides :class:`ValidationEngine` and the public top-level
:func:`validate_config` entry point. By default validation runs completely in
memory with zero disk writes, executing the same Phase A (structural), Phase B
(scientific), and Phase C (Pydantic consistency) checks as ``suews validate``.
"""

from __future__ import annotations

from collections.abc import Mapping
from copy import deepcopy
from pathlib import Path
from typing import TYPE_CHECKING, Any

import yaml

from ..core._yaml_io import safe_load_yaml
from ..core.field_renames import read_physics_key
from ..core.physics_families import flatten_physics_in_config
from .core.yaml_helpers import get_stebbsmethod_value
from .pipeline import phase_a, phase_b, phase_c
from .pipeline.orchestrator import VALID_PIPELINES, run_pipeline
from .pipeline.phase_b_rules import RulesRegistry, ValidationContext, ValidationResult
from .pipeline.report_schema import (
    SEVERITY_ERROR,
    Issue,
    PhaseReport,
    ValidationReport,
)

if TYPE_CHECKING:
    from ..core.config import SUEWSConfig

VALID_MODES = frozenset({"public", "dev"})
VALID_FORCING_OPTIONS = frozenset({"on", "off"})
VALID_SCIENCE_FIXES = frozenset({"suggest", "apply", "off"})


def check_public_mode_restrictions(raw_data: Any) -> list[str]:
    """Return experimental-feature restriction messages violated in public mode.

    Accepts both canonical and legacy physics spellings. Malformed mappings
    return ``[]`` so downstream structural/schema validation reports their
    exact locations instead of masking them as policy violations.
    """
    if not isinstance(raw_data, Mapping):
        return []
    model = raw_data.get("model")
    if not isinstance(model, Mapping):
        return []
    physics = model.get("physics")
    if not isinstance(physics, Mapping):
        return []

    restrictions: list[str] = []
    try:
        stebbsmethod = get_stebbsmethod_value(physics)
    except ValueError as exc:
        restrictions.append(f"Invalid STEBBS physics configuration: {exc}")
        stebbsmethod = None
    if stebbsmethod is not None and stebbsmethod != 0:
        restrictions.append("STEBBS is enabled (stebbs.enabled is true)")

    snowuse = read_physics_key(physics, "snow_use")
    if snowuse is not None and snowuse != 0:
        restrictions.append("Snow calculations are enabled (snow_use != 0)")

    return restrictions


def _normalise_pipeline_arg(pipeline: str) -> str:
    normalised = str(pipeline).upper()
    if normalised not in VALID_PIPELINES:
        raise ValueError(
            f"Unknown validation pipeline {pipeline!r}; expected one of "
            f"{sorted(VALID_PIPELINES)}"
        )
    return normalised


def _normalise_mode_arg(mode: str) -> str:
    normalised = str(mode).lower()
    if normalised not in VALID_MODES:
        raise ValueError(
            f"Unknown validation mode {mode!r}; expected one of {sorted(VALID_MODES)}"
        )
    return normalised


def _normalise_forcing_arg(forcing: str) -> str:
    normalised = str(forcing).lower()
    if normalised not in VALID_FORCING_OPTIONS:
        raise ValueError(
            f"Unknown forcing option {forcing!r}; expected one of "
            f"{sorted(VALID_FORCING_OPTIONS)}"
        )
    return normalised


def _normalise_science_fixes_arg(science_fixes: str) -> str:
    normalised = str(science_fixes).lower()
    if normalised not in VALID_SCIENCE_FIXES:
        raise ValueError(
            f"Unknown science_fixes option {science_fixes!r}; expected one of "
            f"{sorted(VALID_SCIENCE_FIXES)}"
        )
    return normalised


def _dump_suews_config(config: SUEWSConfig) -> dict[str, Any]:
    """Serialise an in-memory ``SUEWSConfig`` to a YAML-equivalent dict.

    Mirrors ``SUEWSConfig.to_yaml``: ``round_trip=True`` excludes computed
    fields (such as ``stebbs.deep_soil_temperature``), ``exclude_none=True``
    avoids inapplicable null slots, private bookkeeping keys are stripped, and
    explicit runtime-null ``forcing.file`` is preserved.
    """
    raw_data = config.model_dump(exclude_none=True, mode="json", round_trip=True)
    node: Any = config.model
    for key in ("control", "forcing"):
        node = node.get(key) if isinstance(node, dict) else getattr(node, key, None)
    file_entry = (
        node.get("file", "") if isinstance(node, dict) else getattr(node, "file", "")
    )
    if node is not None and file_entry is None:
        forcing_dict = (
            raw_data.setdefault("model", {})
            .setdefault("control", {})
            .setdefault("forcing", {})
        )
        forcing_dict["file"] = None
    for key in ("_yaml_path", "_auto_generate_annotated", "_yaml_raw"):
        raw_data.pop(key, None)
    if raw_data.get("strict_initial_state_bounds") is True:
        raw_data.pop("strict_initial_state_bounds", None)
    return raw_data


def _load_source_in_memory(
    source: str | Path | Mapping[str, Any] | SUEWSConfig,
    *,
    first_phase: str,
) -> tuple[Any, Path | None, PhaseReport | None]:
    """Resolve *source* into ``(raw_data, source_path, error_report)`` without writes."""
    from ..core.config import SUEWSConfig  # noqa: PLC0415

    if isinstance(source, SUEWSConfig):
        return _dump_suews_config(source), None, None

    if isinstance(source, Mapping):
        return deepcopy(dict(source)), None, None

    if isinstance(source, (str, Path)):
        path = Path(source)
        try:
            text = path.read_text(encoding="utf-8")
        except FileNotFoundError as exc:
            return (
                None,
                path,
                PhaseReport(
                    phase=first_phase,
                    issues=[
                        Issue(
                            phase=first_phase,
                            severity=SEVERITY_ERROR,
                            code=f"{first_phase}.PIPELINE.FILE_NOT_FOUND",
                            message=f"File not found: {exc}",
                            category="PIPELINE",
                        )
                    ],
                    yaml_in=str(path),
                ),
            )
        except OSError as exc:
            return (
                None,
                path,
                PhaseReport(
                    phase=first_phase,
                    issues=[
                        Issue(
                            phase=first_phase,
                            severity=SEVERITY_ERROR,
                            code=f"{first_phase}.PIPELINE.READ_ERROR",
                            message=str(exc),
                            category="PIPELINE",
                        )
                    ],
                    yaml_in=str(path),
                ),
            )
        try:
            loaded = safe_load_yaml(text)
        except yaml.YAMLError as exc:
            return (
                None,
                path,
                PhaseReport(
                    phase=first_phase,
                    issues=[
                        Issue(
                            phase=first_phase,
                            severity=SEVERITY_ERROR,
                            code=f"{first_phase}.PIPELINE.YAML_PARSE_ERROR",
                            message=f"Invalid YAML: {exc}",
                            category="PIPELINE",
                        )
                    ],
                    yaml_in=str(path),
                ),
            )
        return loaded, path, None

    raise TypeError(
        "source must be a YAML path (str or Path), a mapping, or a SUEWSConfig; "
        f"got {type(source).__name__}"
    )


def _run_phase_a_in_memory(
    raw_data: Any,
    *,
    source_path: Path | None,
    forcing: str,
) -> tuple[Any, PhaseReport]:
    """Run Phase A structural checks in memory without writing any files."""
    from ..core.config import (  # noqa: PLC0415
        _soil_contract_conflict_message,
        _soil_contract_conflicts,
        _uses_legacy_observed_soil,
    )

    working = deepcopy(raw_data)
    forcing_snapshot = (
        phase_a._snapshot_forcing_user_data(working)
        if forcing == "on" and source_path is not None
        else phase_a._MISSING
    )
    nlayer = phase_a.detect_nlayer_from_yaml_data(working)
    legacy_soil = _uses_legacy_observed_soil(working)
    soil_conflicts = _soil_contract_conflicts(working)
    rename_skip = phase_a.LEGACY_SOIL_SELECTORS if legacy_soil else ()

    renamed_replacements = phase_a.handle_renamed_parameters_in_parsed_yaml(
        working, skip=rename_skip
    )
    normalised_data, compat_replacements = phase_a._normalise_phase_a_compatibility(
        working
    )
    working = normalised_data
    seen_renames = set(renamed_replacements)
    for repl in compat_replacements:
        if repl not in seen_renames:
            renamed_replacements.append(repl)
            seen_renames.add(repl)

    flatten_physics_in_config(working)

    dimension_errors = []
    if nlayer is not None:
        working, dimension_errors = phase_a.validate_nlayer_dimensions(
            working, nlayer, per_site=True
        )
    nlayer_limit_errors = phase_a.validate_nlayer_limit(working)

    forcing_errors: list[str] = []
    forcing_warnings: list[str] = []
    if forcing == "on" and source_path is not None:
        physics_config = (
            working.get("model", {}).get("physics")
            if isinstance(working, dict) and isinstance(working.get("model"), dict)
            else None
        )
        forcing_errors, _ = phase_a.validate_forcing_data(
            str(source_path),
            physics=physics_config,
            _preloaded_user_data=forcing_snapshot,
        )
        if phase_a.forcing_file_is_runtime(working):
            forcing_warnings.append(phase_a.RUNTIME_FORCING_WARNING)

    working_no_ref = phase_a._strip_ref_blocks(working)
    missing_params = phase_a.find_missing_parameters_from_schema(
        working_no_ref, _normalised=True
    )
    extra_params = phase_a.find_extra_parameters_from_schema(
        working_no_ref, _normalised=True
    )

    migration_errors: list[str] = []
    if legacy_soil:
        from ...util._forcing import SOIL_CONVERT_HINT  # noqa: PLC0415

        missing_params = [
            entry
            for entry in missing_params
            if not phase_a.is_legacy_soil_path(entry[0])
        ]
        extra_params = [
            path for path in extra_params if not phase_a.is_legacy_soil_path(path)
        ]
        if soil_conflicts:
            migration_errors.append(_soil_contract_conflict_message(soil_conflicts))
        else:
            migration_errors.append(
                f"{phase_a.LEGACY_OBSERVED_SOIL_MESSAGE} {SOIL_CONVERT_HINT}"
            )
    forcing_errors = phase_a._drop_repeated_convert_hint(
        migration_errors, forcing_errors
    )

    issues = phase_a._issues_from_phase_a_state(
        missing_params=missing_params,
        renamed_replacements=renamed_replacements,
        extra_params=extra_params,
        dimension_errors=dimension_errors,
        forcing_errors=forcing_errors,
        nlayer_limit_errors=nlayer_limit_errors,
        forcing_warnings=forcing_warnings,
        migration_errors=migration_errors,
    )
    return (
        working,
        PhaseReport(
            phase="A",
            issues=issues,
            yaml_in=str(source_path) if source_path is not None else None,
        ),
    )


def _run_phase_b_in_memory(
    working_data: Any,
    *,
    source_path: Path | None,
    science_fixes: str,
    upstream_has_errors: bool,
) -> tuple[Any, PhaseReport]:
    """Run Phase B scientific checks and optional in-memory adjustments."""
    data = deepcopy(working_data) if isinstance(working_data, dict) else {}
    flatten_physics_in_config(data)
    effective_fixes = (
        "off" if upstream_has_errors and science_fixes == "apply" else science_fixes
    )

    try:
        model_year, start_date, _end_date = phase_b.extract_simulation_parameters(data)
        validation_context = ValidationContext(
            yaml_data=data,
            start_date=start_date,
            model_year=model_year,
        )
        validation_results = RulesRegistry(context=validation_context).run_validation()
    except (ValueError, KeyError, FileNotFoundError) as exc:
        error_message = str(exc)
        parts = error_message.split("; ") if ";" in error_message else [error_message]
        validation_results = [
            ValidationResult(
                parameter="initialization",
                site_index=None,
                status="FAIL",
                message=part,
                severity="ERROR",
                category="INITIALIZATION",
            )
            for part in parts
        ]
        issues = [r.to_issue() for r in validation_results]
        return (
            working_data,
            PhaseReport(
                phase="B",
                issues=issues,
                yaml_in=str(source_path) if source_path is not None else None,
            ),
        )

    critical_errors = [
        r for r in validation_results if r.status == "FAIL" and r.severity == "ERROR"
    ]
    adjustments = []
    suggestions = []
    if not critical_errors:
        if effective_fixes == "apply":
            data, adjustments = phase_b.run_scientific_adjustment_pipeline(
                data,
                start_date,
                model_year,
                errors=validation_results,
            )
        elif effective_fixes == "suggest":
            suggestions = phase_b.collect_science_suggestions(
                data,
                start_date,
                model_year,
                errors=validation_results,
            )

    issues = [r.to_issue() for r in validation_results]
    issues.extend(phase_b._adjustment_to_issue(a) for a in adjustments)
    if effective_fixes == "suggest":
        issues.extend(s.to_issue() for s in suggestions)
    return (
        data,
        PhaseReport(
            phase="B",
            issues=issues,
            yaml_in=str(source_path) if source_path is not None else None,
        ),
    )


def _run_phase_c_in_memory(
    working_data: Any,
    *,
    source_path: Path | None,
) -> PhaseReport:
    """Run single-pass Phase C Pydantic validation in memory."""
    result = phase_c.validate_phase_c(
        str(source_path) if source_path is not None else None,
        raw_data=deepcopy(working_data) if isinstance(working_data, dict) else working_data,
    )
    return PhaseReport(
        phase="C",
        issues=result.issues,
        yaml_in=str(source_path) if source_path is not None else None,
    )


class ValidationEngine:
    """Single entry point for SUEWS YAML configuration validation.

    Runs phases A (structural completeness), B (scientific consistency), and
    C (Pydantic model validation) in memory by default with zero disk writes.
    Pass ``write_outputs=True`` with a file path to run the transactional
    staged writer that publishes ``updated_<name>.yml``, ``report_<stem>.txt``,
    and ``report_<stem>.json``.
    """

    def validate(
        self,
        source: str | Path | Mapping[str, Any] | SUEWSConfig,
        *,
        pipeline: str = "ABC",
        mode: str = "public",
        forcing: str = "on",
        science_fixes: str = "suggest",
        continue_on_error: bool = False,
        write_outputs: bool = False,
    ) -> ValidationReport:
        """Validate *source* and return a structured :class:`ValidationReport`."""
        pipeline_norm = _normalise_pipeline_arg(pipeline)
        mode_norm = _normalise_mode_arg(mode)
        forcing_norm = _normalise_forcing_arg(forcing)
        fixes_norm = _normalise_science_fixes_arg(science_fixes)

        if write_outputs:
            if not isinstance(source, (str, Path)):
                raise ValueError(
                    "write_outputs=True requires a YAML file path (str or Path)."
                )
            path_str = str(Path(source).resolve())
            raw_for_policy, _, load_err = _load_source_in_memory(
                path_str, first_phase=pipeline_norm[0]
            )
            if load_err is not None:
                return ValidationReport(phases=[load_err])
            if mode_norm == "public":
                restrictions = check_public_mode_restrictions(raw_for_policy)
                if restrictions:
                    return ValidationReport(
                        phases=[
                            PhaseReport(
                                phase=pipeline_norm[0],
                                issues=[
                                    Issue(
                                        phase=pipeline_norm[0],
                                        severity=SEVERITY_ERROR,
                                        code="MODE.PUBLIC.RESTRICTED_FEATURE",
                                        message=msg,
                                        yaml_path="model.physics",
                                        category="POLICY",
                                    )
                                    for msg in restrictions
                                ],
                                yaml_in=path_str,
                            )
                        ]
                    )
            staged = run_pipeline(
                path_str,
                pipeline=pipeline_norm,
                mode=mode_norm,
                forcing=forcing_norm,
                science_fixes=fixes_norm,
                continue_on_error=continue_on_error,
            )
            return ValidationReport(phases=staged.phases)

        first_phase = pipeline_norm[0]
        raw_data, source_path, load_error = _load_source_in_memory(
            source, first_phase=first_phase
        )
        if load_error is not None:
            return ValidationReport(phases=[load_error])

        policy_issues: list[Issue] = []
        if mode_norm == "public":
            restrictions = check_public_mode_restrictions(raw_data)
            if restrictions:
                policy_issues = [
                    Issue(
                        phase=first_phase,
                        severity=SEVERITY_ERROR,
                        code="MODE.PUBLIC.RESTRICTED_FEATURE",
                        message=msg,
                        yaml_path="model.physics",
                        category="POLICY",
                    )
                    for msg in restrictions
                ]
                if not continue_on_error:
                    return ValidationReport(
                        phases=[
                            PhaseReport(
                                phase=first_phase,
                                issues=policy_issues,
                                yaml_in=str(source_path) if source_path else None,
                            )
                        ]
                    )

        phases: list[PhaseReport] = []
        working_data = raw_data
        has_errors = bool(policy_issues)

        if "A" in pipeline_norm:
            working_data, phase_a_report = _run_phase_a_in_memory(
                raw_data,
                source_path=source_path,
                forcing=forcing_norm,
            )
            if policy_issues and first_phase == "A":
                phase_a_report.issues = [*policy_issues, *phase_a_report.issues]
                policy_issues = []
            phases.append(phase_a_report)
            has_errors = has_errors or phase_a_report.has_errors

        if "B" in pipeline_norm and (not has_errors or continue_on_error):
            working_data, phase_b_report = _run_phase_b_in_memory(
                working_data,
                source_path=source_path,
                science_fixes=fixes_norm,
                upstream_has_errors=has_errors,
            )
            if policy_issues and first_phase == "B":
                phase_b_report.issues = [*policy_issues, *phase_b_report.issues]
                policy_issues = []
            phases.append(phase_b_report)
            has_errors = has_errors or phase_b_report.has_errors

        if "C" in pipeline_norm and (not has_errors or continue_on_error):
            phase_c_report = _run_phase_c_in_memory(
                working_data,
                source_path=source_path,
            )
            if policy_issues and first_phase == "C":
                phase_c_report.issues = [*policy_issues, *phase_c_report.issues]
                policy_issues = []
            phases.append(phase_c_report)

        return ValidationReport(phases=phases)


def validate_config(
    source: str | Path | Mapping[str, Any] | SUEWSConfig,
    *,
    pipeline: str = "ABC",
    mode: str = "public",
    forcing: str = "on",
    science_fixes: str = "suggest",
    continue_on_error: bool = False,
    write_outputs: bool = False,
) -> ValidationReport:
    """Validate a SUEWS YAML configuration file, dict, or ``SUEWSConfig`` object.

    Runs completely in memory with zero file writes by default. Set
    ``write_outputs=True`` (with a file path) to publish ``updated_<name>.yml``,
    ``report_<stem>.txt``, and ``report_<stem>.json`` next to the config.

    Parameters
    ----------
    source : str, pathlib.Path, Mapping, or SUEWSConfig
        Path to a SUEWS YAML configuration, an in-memory config mapping, or an
        already-loaded :class:`~supy.data_model.SUEWSConfig`.
    pipeline : str, default "ABC"
        Phase combination to run: one of ``"A"``, ``"B"``, ``"C"``, ``"AB"``,
        ``"AC"``, ``"BC"``, or ``"ABC"``.
    mode : str, default "public"
        Validation mode: ``"public"`` (rejects experimental features such as
        STEBBS and snow) or ``"dev"``.
    forcing : str, default "on"
        Forcing validation switch (``"on"`` or ``"off"``). Only applies when
        *source* is a file path and phase A runs.
    science_fixes : str, default "suggest"
        Phase B scientific transformation policy: ``"suggest"``, ``"apply"``,
        or ``"off"``.
    continue_on_error : bool, default False
        When ``True``, continue running downstream read-only phases after an
        earlier phase reports blocking errors.
    write_outputs : bool, default False
        When ``True``, run the staged disk publisher (requires a file path).

    Returns
    -------
    ValidationReport
        Structured multi-phase validation report with ``is_valid``, ``errors``,
        ``warnings``, ``suggestions``, ``applied_fixes``, ``to_dict()``, and
        ``to_text()``.
    """
    return ValidationEngine().validate(
        source,
        pipeline=pipeline,
        mode=mode,
        forcing=forcing,
        science_fixes=science_fixes,
        continue_on_error=continue_on_error,
        write_outputs=write_outputs,
    )
