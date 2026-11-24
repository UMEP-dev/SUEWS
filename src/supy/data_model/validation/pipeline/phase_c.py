"""Phase C validation and report generation for configuration consistency."""

from __future__ import annotations

from collections.abc import Mapping
from copy import deepcopy
from dataclasses import dataclass, field
import logging
import os
import re
from typing import Any

from pydantic import BaseModel
import yaml

from ...core._yaml_io import safe_load_yaml
from ...core.field_renames import (
    STEBBS_PHYSICS_LEAF_RENAMES,
    fold_stebbs_physics,
    normalise_yaml_renames,
)
from ...core.physics_families import flatten_physics_in_config
from .document import (
    format_pydantic_loc,
    is_ref_only,
    site_gridid_map,
    unwrap_entry,
)
from .report_schema import SEVERITY_ERROR, SEVERITY_INFO, Issue
from .report_writer import REPORT_WRITER

# Use unified report title for all validation phases
REPORT_TITLE = "SUEWS Validation Report"

_INTERNAL_DEFAULT_SKIP = frozenset({
    "ref",
    "description",
    "version",
    "metadata",
    "comments",
})

_CRITICAL_PHYSICS_PARAMS = (
    "net_radiation",
    "emissions",
    "storage_heat",
    "ohm_inc_qf",
    "roughness_length_momentum",
    "roughness_length_heat",
    "stability",
    "soil_moisture",
    "water_use",
    "roughness_sublayer",
    "frontal_area_index",
    "roughness_sublayer_level",
    "surface_conductance",
    "snow_use",
    "capacitance",
    "setpoint",
    "same_albedo_wall",
    "same_albedo_roof",
    "same_emissivity_wall",
    "same_emissivity_roof",
)
_STEBBS_LEAF_NAMES = frozenset(STEBBS_PHYSICS_LEAF_RENAMES.values())


@dataclass
class PhaseCResult:
    """Result of a single-pass Phase C Pydantic validation."""

    config: Any = None
    issues: list[Issue] = field(default_factory=list)
    raw_data: dict[str, Any] | None = None
    had_signature: bool = True
    exception: Exception | None = None

    @property
    def has_errors(self) -> bool:
        """Whether Phase C produced any blocking ERROR issue."""
        return any(issue.severity == SEVERITY_ERROR for issue in self.issues)


def _collect_critical_physics_nulls(raw_data: Mapping[str, Any]) -> list[Issue]:
    """Detect critical physics parameters explicitly authored as null.

    Normalises legacy physics spellings and folds flat STEBBS keys onto a
    shallow copy so explicit nulls under legacy or nested spellings are
    surfaced as structured ``C.CRITICAL_PARAM.*`` errors instead of falling
    through to ``C.PIPELINE.UNKNOWN``.
    """
    model_raw = raw_data.get("model")
    if not isinstance(model_raw, Mapping):
        return []
    physics_raw = model_raw.get("physics")
    if not isinstance(physics_raw, Mapping):
        return []

    normalised_cfg = normalise_yaml_renames({"model": {"physics": deepcopy(dict(physics_raw))}})
    flatten_physics_in_config(normalised_cfg)
    physics = normalised_cfg.get("model", {}).get("physics", {})
    if not isinstance(physics, dict):
        return []
    folded = fold_stebbs_physics(deepcopy(physics), "model.physics")
    stebbs_block = folded.get("stebbs")
    if not isinstance(stebbs_block, Mapping):
        stebbs_block = {}

    issues: list[Issue] = []
    for param in _CRITICAL_PHYSICS_PARAMS:
        if param in _STEBBS_LEAF_NAMES:
            container: Mapping[str, Any] = stebbs_block
            field_path = f"model.physics.stebbs.{param}"
        else:
            container = physics
            field_path = f"model.physics.{param}"

        if param not in container:
            continue
        entry = container[param]
        is_null = (
            entry is None
            or (isinstance(entry, Mapping) and "value" in entry and entry["value"] is None)
            or is_ref_only(entry)
        )
        if is_null:
            issues.append(
                Issue(
                    phase="C",
                    severity=SEVERITY_ERROR,
                    code=f"C.CRITICAL_PARAM.{param.upper()}",
                    message=(
                        f"{param} at level {field_path}: Physics parameter is null "
                        "and will cause runtime crash"
                    ),
                    yaml_path=field_path,
                    category="CONFIG_CONSISTENCY",
                    suggested_value=(
                        "Set to appropriate non-null value - see documentation at "
                        "https://docs.suews.io/stable"
                    ),
                )
            )
    return issues


def _unwrap_model_scalar(value: Any) -> Any:
    """Return a display-friendly scalar from a RefValue, Enum, or primitive."""
    if hasattr(value, "value") and not isinstance(value, (dict, list, str, bytes)):
        inner = value.value
        if hasattr(inner, "value") and not isinstance(inner, (dict, list, str, bytes)):
            return inner.value
        return inner
    return value


def _collect_model_defaults(
    config: BaseModel,
    raw_data: Mapping[str, Any],
    *,
    had_signature: bool,
    gridid_map: Mapping[int, Any],
) -> list[Issue]:
    """Emit structured ``INFO`` issues for Pydantic-applied defaults via ``model_fields_set``.

    Walks only user-authored containers (never expanding omitted optional
    sub-models) and skips internal/private fields so compact configs are not
    flooded with inactive defaults while omitted user-level defaults such as
    ``schema_version`` are reported accurately without loading
    ``sample_config.yml``.
    """
    issues: list[Issue] = []

    if not had_signature and getattr(config, "schema_version", None) is not None:
        default_val = config.schema_version
        issues.append(
            Issue(
                phase="C",
                severity=SEVERITY_INFO,
                code="C.DEFAULT_APPLIED.SCHEMA_VERSION",
                message=(
                    "schema_version found missing in user YAML at level schema_version. "
                    f"The validation system will interpret that as default value: "
                    f"{default_val} - check doc for info on this parameter: "
                    "https://docs.suews.io/stable"
                ),
                yaml_path="schema_version",
                category="DEFAULTS",
                suggested_value=default_val,
            )
        )

    def _walk(model_obj: BaseModel, raw_map: Mapping[str, Any], prefix: str) -> None:
        fields_set = getattr(model_obj, "model_fields_set", set())
        for field_name in type(model_obj).model_fields:
            if field_name.startswith("_") or field_name in _INTERNAL_DEFAULT_SKIP:
                continue
            if field_name == "schema_version" and not prefix:
                continue

            val = getattr(model_obj, field_name, None)
            current_path = f"{prefix}.{field_name}" if prefix else field_name

            if field_name not in raw_map:
                if field_name in fields_set or val is None:
                    continue
                if isinstance(val, BaseModel) and not hasattr(val, "value"):
                    # Do not dump large omitted nested sub-models as scalar defaults.
                    continue
                if isinstance(val, (list, dict)):
                    continue
                display_val = _unwrap_model_scalar(val)
                if display_val is None:
                    continue
                issues.append(
                    Issue(
                        phase="C",
                        severity=SEVERITY_INFO,
                        code=f"C.DEFAULT_APPLIED.{field_name.upper()}",
                        message=(
                            f"{field_name} found missing in user YAML at level "
                            f"{current_path}. The validation system will interpret "
                            f"that as default value: {display_val} - check doc for "
                            "info on this parameter: https://docs.suews.io/stable"
                        ),
                        yaml_path=current_path,
                        category="DEFAULTS",
                        suggested_value=display_val,
                    )
                )
                continue

            raw_child = raw_map[field_name]
            if isinstance(val, BaseModel) and not hasattr(val, "value"):
                if isinstance(raw_child, Mapping):
                    _walk(val, raw_child, current_path)
            elif isinstance(val, list) and isinstance(raw_child, list):
                for idx, (raw_item, model_item) in enumerate(zip(raw_child, val)):
                    if isinstance(model_item, BaseModel) and isinstance(raw_item, Mapping):
                        if current_path == "sites":
                            item_path = f"sites.{gridid_map.get(idx, idx)}"
                        else:
                            item_path = f"{current_path}[{idx}]"
                        _walk(model_item, raw_item, item_path)
            elif val is not None:
                raw_inner = unwrap_entry(raw_child)
                if raw_inner is None:
                    display_val = _unwrap_model_scalar(val)
                    if display_val is not None and not isinstance(
                        display_val, (dict, list, BaseModel)
                    ):
                        issues.append(
                            Issue(
                                phase="C",
                                severity=SEVERITY_INFO,
                                code=f"C.DEFAULT_APPLIED.{field_name.upper()}",
                                message=(
                                    f"{field_name} found null in user YAML at level "
                                    f"{current_path}. The validation system will "
                                    f"interpret that as default value: {display_val} "
                                    "- check doc for info on this parameter: "
                                    "https://docs.suews.io/stable"
                                ),
                                yaml_path=current_path,
                                category="DEFAULTS",
                                suggested_value=display_val,
                            )
                        )

    _walk(config, raw_data, "")
    return issues


def validate_phase_c(
    input_yaml_file: str | None = None,
    *,
    raw_data: dict[str, Any] | None = None,
) -> PhaseCResult:
    """Run a single-pass Phase C Pydantic validation and collect structured issues.

    Calls ``SUEWSConfig._prepare_raw_input`` and ``SUEWSConfig._validate_raw_mapping``
    once so site-completeness checks (``_yaml_raw``) and structured
    ``ValidationError.errors()`` are both preserved without running Pydantic twice
    or loading ``sample_config.yml``.
    """
    issues: list[Issue] = []

    try:
        from pydantic import ValidationError
        from supy.data_model import SUEWSConfig
        from supy.data_model.core.config import _legacy_yaml_key_suggestion
    except ImportError as exc:
        issues.append(
            Issue(
                phase="C",
                severity=SEVERITY_ERROR,
                code="C.PIPELINE.IMPORT_ERROR",
                message=f"Cannot import SUEWSConfig: {exc}",
                category="PIPELINE",
            )
        )
        return PhaseCResult(issues=issues, exception=exc)

    if raw_data is None:
        try:
            with open(input_yaml_file, encoding="utf-8") as fh:
                loaded = safe_load_yaml(fh)
        except (OSError, yaml.YAMLError) as exc:
            issues.append(
                Issue(
                    phase="C",
                    severity=SEVERITY_ERROR,
                    code="C.PIPELINE.YAML_PARSE_ERROR",
                    message=str(exc),
                    category="PIPELINE",
                )
            )
            return PhaseCResult(issues=issues, exception=exc)
    else:
        loaded = raw_data

    if not isinstance(loaded, dict):
        issues.append(
            Issue(
                phase="C",
                severity=SEVERITY_ERROR,
                code="C.PIPELINE.YAML_NOT_MAPPING",
                message="YAML root is not a mapping",
                category="PIPELINE",
            )
        )
        return PhaseCResult(
            issues=issues,
            exception=ValueError("YAML root is not a mapping"),
        )

    gridid_map = site_gridid_map(loaded)
    supy_logger = logging.getLogger("SuPy")
    original_level = supy_logger.level
    supy_logger.setLevel(logging.CRITICAL)

    prepared: dict[str, Any] | None = None
    had_signature = "schema_version" in loaded
    try:
        prepared, had_signature = SUEWSConfig._prepare_raw_input(
            loaded, yaml_path=input_yaml_file
        )
        config = SUEWSConfig._validate_raw_mapping(prepared)
    except ValidationError as exc:
        for err in exc.errors():
            loc_tuple = err.get("loc", ())
            gridid_path = format_pydantic_loc(loc_tuple, gridid_map)
            err_type = err.get("type", "value_error")
            code_slug = str(err_type).upper().replace(".", "_")
            message = err.get("msg", str(err))
            if err_type == "extra_forbidden" and loc_tuple:
                suggestion = _legacy_yaml_key_suggestion(loc_tuple[-1])
                if suggestion is not None:
                    message += f"; legacy field was renamed to '{suggestion}'"
            issues.append(
                Issue(
                    phase="C",
                    severity=SEVERITY_ERROR,
                    code=f"C.PYDANTIC.{code_slug}",
                    message=message,
                    yaml_path=gridid_path,
                    category="CONFIG_CONSISTENCY",
                )
            )
        return PhaseCResult(
            config=None,
            issues=issues,
            raw_data=loaded,
            had_signature=had_signature,
            exception=exc,
        )
    except (TypeError, AttributeError) as exc:
        drift_exc = (
            SUEWSConfig._build_drift_error(exc, prepared, had_signature=had_signature)
            if prepared is not None
            else exc
        )
        issues.append(
            Issue(
                phase="C",
                severity=SEVERITY_ERROR,
                code="C.PIPELINE.EXCEPTION",
                message=str(drift_exc),
                category="PIPELINE",
            )
        )
        return PhaseCResult(
            config=None,
            issues=issues,
            raw_data=loaded,
            had_signature=had_signature,
            exception=drift_exc,
        )
    except Exception as exc:
        issues.append(
            Issue(
                phase="C",
                severity=SEVERITY_ERROR,
                code="C.PIPELINE.EXCEPTION",
                message=str(exc),
                category="PIPELINE",
            )
        )
        return PhaseCResult(
            config=None,
            issues=issues,
            raw_data=loaded,
            had_signature=had_signature,
            exception=exc,
        )
    finally:
        supy_logger.setLevel(original_level)

    critical_null_issues = _collect_critical_physics_nulls(loaded)
    issues.extend(critical_null_issues)

    if not critical_null_issues:
        if hasattr(config, "_validation_summary"):
            for msg in config._validation_summary.get("info_messages", []):
                issues.append(
                    Issue(
                        phase="C",
                        severity=SEVERITY_INFO,
                        code="C.VALIDATION_SUMMARY",
                        message=str(msg),
                        category="CONFIG_CONSISTENCY",
                    )
                )

        # Normalise a shallow copy for default comparison so legacy field
        # spellings authored by the user are not flagged as omitted defaults.
        normalised_for_defaults = normalise_yaml_renames(deepcopy(loaded))
        flatten_physics_in_config(normalised_for_defaults)
        issues.extend(
            _collect_model_defaults(
                config,
                normalised_for_defaults,
                had_signature=had_signature,
                gridid_map=gridid_map,
            )
        )

    return PhaseCResult(
        config=config,
        issues=issues,
        raw_data=loaded,
        had_signature=had_signature,
        exception=None,
    )


def collect_phase_c_issues(input_yaml_file: str) -> list[Issue]:
    """Run a structured Pydantic validation pass and return blocking ``Issue``\\ s."""
    result = validate_phase_c(input_yaml_file)
    return [issue for issue in result.issues if issue.severity == SEVERITY_ERROR]


def convert_pydantic_location_to_gridid_path(
    loc_tuple, input_yaml_file, *, gridid_map: Mapping[int, Any] | None = None
):
    """Convert Pydantic error location tuple to user-friendly GRIDID path.

    Args:
        loc_tuple: Tuple of location elements from Pydantic error (e.g., ('sites', 0, 'properties', 'lat'))
        input_yaml_file: Path to original YAML file to extract GRIDID values
        gridid_map: Optional precomputed ``{site_index: gridiv}`` map to avoid re-reading disk.

    Returns:
        str: User-friendly path with GRIDID (e.g., "sites.123.properties.lat")
    """
    if not loc_tuple:
        return ""

    if gridid_map is None and "sites" in loc_tuple and input_yaml_file:
        try:
            with open(input_yaml_file, encoding="utf-8") as f:
                yaml_data = safe_load_yaml(f)
            gridid_map = site_gridid_map(yaml_data)
        except Exception:
            gridid_map = {}

    return format_pydantic_loc(loc_tuple, gridid_map)


def _parse_previous_phase_report(report_content: str):
    """Parse previous phase report content to extract relevant information."""
    phase_a_renames = []
    phase_a_optional_missing = []
    phase_a_not_in_standard = []
    phase_b_science_warnings = []

    report_type = "unknown"
    if "SUEWS Scientific Validation Report" in report_content:
        report_type = "phase_b"
    elif "SUEWS Configuration Analysis Report" in report_content:
        report_type = "phase_a"

    lines = report_content.split("\n")
    current_section = None

    for line in lines:
        line = line.strip()
        if "Updated (" in line and "renamed parameter" in line:
            current_section = "renames"
        elif "Updated (" in line and "optional missing parameter" in line:
            current_section = "optional"
        elif "parameter(s) not in standard" in line:
            current_section = "not_standard"
        elif report_type == "phase_b" and "scientific warning" in line:
            current_section = "warnings"
        elif line.startswith("--"):
            if current_section == "renames":
                phase_a_renames.append(line[2:].strip())
            elif current_section == "optional":
                phase_a_optional_missing.append(line[2:].strip())
            elif current_section == "not_standard":
                phase_a_not_in_standard.append(line[2:].strip())
            elif current_section == "warnings":
                phase_b_science_warnings.append(line[2:].strip())

    return (
        phase_a_renames,
        phase_a_optional_missing,
        phase_a_not_in_standard,
        phase_b_science_warnings,
    )


def _parse_consolidated_messages(messages: list):
    """Parse consolidated messages to extract different categories."""
    phase_a_renames = []
    phase_a_optional_missing = []
    phase_a_not_in_standard = []
    phase_b_science_warnings = []
    phase_b_updates = []

    current_category = None
    for line in messages:
        line = line.strip()
        if "Updated (" in line and "renamed parameter" in line:
            current_category = "renames"
        elif "Updated (" in line and "optional missing parameter" in line:
            current_category = "optional"
        elif "Updated (" in line and "parameter(s):" in line:
            # This handles "Updated (X) parameter(s):" which are Phase B updates
            current_category = "phase_b_updates"
        elif "parameter(s) not in standard" in line:
            current_category = "not_standard"
        elif "scientific warning" in line or "Revise (" in line:
            current_category = "warnings"
        elif line.startswith("--"):
            detail = line[2:].strip()
            if current_category == "renames":
                phase_a_renames.append(detail)
            elif current_category == "optional":
                phase_a_optional_missing.append(detail)
            elif current_category == "not_standard":
                phase_a_not_in_standard.append(detail)
            elif current_category == "warnings":
                phase_b_science_warnings.append(detail)
            elif current_category == "phase_b_updates":
                phase_b_updates.append(detail)

    return (
        phase_a_renames,
        phase_a_optional_missing,
        phase_a_not_in_standard,
        phase_b_science_warnings,
        phase_b_updates,
    )


def generate_phase_c_report(
    validation_error: Exception,
    input_yaml_file: str,
    output_report_file: str,
    mode: str = "public",
    phase_a_report_file: str = None,
    phases_run: list = None,
    no_action_messages: list = None,
) -> None:
    """Generate Phase C validation report with previous phase consolidation."""
    report_lines = []

    phase_str = "".join(phases_run) if phases_run else "C"
    title = REPORT_TITLE

    report_lines.append(f"# {title}")
    report_lines.append("# " + "=" * 50)
    report_lines.append(
        f"# Mode: {'Public' if mode.lower() == 'public' else mode.title()}"
    )
    report_lines.append("# " + "=" * 50)
    report_lines.append("")

    phase_a_renames = []
    phase_a_optional_missing = []
    phase_a_not_in_standard = []
    phase_b_science_warnings = []
    phase_b_updates = []

    # Use passed no_action_messages if available, otherwise try to read from file
    if no_action_messages:
        # Parse the consolidated messages directly
        (
            phase_a_renames,
            phase_a_optional_missing,
            phase_a_not_in_standard,
            phase_b_science_warnings,
            phase_b_updates,
        ) = _parse_consolidated_messages(no_action_messages)
    elif phase_a_report_file and os.path.exists(phase_a_report_file):
        try:
            report_content = REPORT_WRITER.read(phase_a_report_file)
            (
                phase_a_renames,
                phase_a_optional_missing,
                phase_a_not_in_standard,
                phase_b_science_warnings,
            ) = _parse_previous_phase_report(report_content)
        except Exception:
            pass

    action_needed_items = []
    pydantic_errors = None

    if hasattr(validation_error, "errors"):
        errors_attr = validation_error.errors
        pydantic_errors = errors_attr() if callable(errors_attr) else errors_attr

    if pydantic_errors:
        for error in pydantic_errors:
            error_type = error.get("type", "unknown")
            # Convert Pydantic location to GRIDID-friendly path
            error_loc = error.get("loc", [])
            field_path = convert_pydantic_location_to_gridid_path(
                error_loc, input_yaml_file
            )
            error_msg = error.get("msg", "Unknown error")

            if not field_path:
                field_match = re.search(
                    r"Required field '(\w+)' has no value", error_msg
                )
                if field_match:
                    field_name = field_match.group(1)
                    if field_name in ["lat", "lon", "alt"]:
                        # Try to get GRIDID for first site as fallback
                        try:
                            with open(input_yaml_file, "r", encoding="utf-8") as f:
                                yaml_data = yaml.safe_load(f)
                            if (
                                "sites" in yaml_data
                                and isinstance(yaml_data["sites"], list)
                                and len(yaml_data["sites"]) > 0
                                and isinstance(yaml_data["sites"][0], dict)
                                and "gridiv" in yaml_data["sites"][0]
                            ):
                                gridiv = yaml_data["sites"][0]["gridiv"]
                                # Handle RefValue objects
                                if isinstance(gridiv, dict) and "value" in gridiv:
                                    gridid = gridiv["value"]
                                else:
                                    gridid = gridiv
                                field_path = f"sites.{gridid}.properties.{field_name}"
                            else:
                                field_path = f"sites.0.properties.{field_name}"
                        except Exception:
                            field_path = f"sites.0.properties.{field_name}"
                    else:
                        field_path = f"model.{field_name}"
                else:
                    field_path = "root_validation"
                    field_name = "configuration"
            else:
                field_name = (
                    field_path.split(".")[-1] if "." in field_path else field_path
                )

            full_error_parts = [error_msg]

            if error_type != "unknown":
                type_parts = [f"type={error_type}"]
                if "input" in error:
                    input_value = str(error.get("input", ""))
                    if len(input_value) > 100:
                        input_value = input_value[:97] + "..."
                    type_parts.append(f"input_value={input_value}")
                if "input_type" in error:
                    type_parts.append(f"input_type={error.get('input_type')}")
                full_error_parts.append(f"[{', '.join(type_parts)}]")

            if "url" in error:
                full_error_parts.append(
                    f"For further information visit {error.get('url')}"
                )

            complete_error_msg = " ".join(full_error_parts)

            # Check if this is our combined critical validation error
            if "Critical validation failed: " in error_msg:
                # Split the combined error message into individual issues
                # Extract the part after "Critical validation failed: "
                split_parts = error_msg.split("Critical validation failed: ", 1)
                combined_message = split_parts[1] if len(split_parts) > 1 else error_msg
                individual_issues = [
                    issue.strip() for issue in combined_message.split(";")
                ]

                for issue in individual_issues:
                    if issue:  # Skip empty issues
                        # Determine the field name and path based on the issue content
                        if "is set to null and will cause runtime crash" in issue:
                            param_name = issue.split(" is set to null")[0]
                            issue_field_name = param_name
                            issue_path = f"model.physics.{param_name}"
                        elif " -> " in issue or " \u2192 " in issue:
                            # StorageHeat parameter format: "site: storageheatmethod=6 -> properties.lambda_c must be set"
                            # Handle both ASCII (->) and Unicode (→) for backward compatibility
                            separator = " -> " if " -> " in issue else " \u2192 "
                            parts = issue.split(separator, 1)
                            if len(parts) == 2:
                                site_part = parts[0].strip()
                                param_part = parts[1].strip()
                                # Extract site name (before the colon)
                                site_name = (
                                    site_part.split(":")[0].strip()
                                    if ":" in site_part
                                    else "Unknown"
                                )
                                issue_field_name = param_part.split(" must be")[
                                    0
                                ].strip()
                                issue_path = (
                                    f"sites[{site_name}].properties.{issue_field_name}"
                                )
                            else:
                                issue_field_name = "StorageHeat parameter"
                                issue_path = "configuration"
                        elif "must be set" in issue and ":" in issue:
                            # RSL parameter format: "site: for rslmethod=2 and bldgs.sfr=0.38, bldgs.faibldg must be set"
                            parts = issue.split(":", 1)
                            if len(parts) == 2:
                                site_name = parts[0].strip()
                                param_desc = parts[1].strip()
                                if "faibldg" in param_desc:
                                    issue_field_name = "bldgs.faibldg"
                                    issue_path = f"sites[{site_name}].properties.land_cover.bldgs.faibldg"
                                else:
                                    issue_field_name = "RSL parameter"
                                    issue_path = f"sites[{site_name}]"
                            else:
                                issue_field_name = "RSL parameter"
                                issue_path = "configuration"
                        else:
                            # Generic issue
                            issue_field_name = "validation issue"
                            issue_path = "configuration"

                        action_needed_items.append({
                            "field": issue_field_name,
                            "path": issue_path,
                            "error": issue,
                        })
            else:
                # Original single error handling
                action_needed_items.append({
                    "field": field_name,
                    "path": field_path,
                    "error": complete_error_msg,
                })
    else:
        error_str = str(validation_error)

        # Check if this is our combined critical validation error
        if error_str.startswith("Critical validation failed: "):
            # Split the combined error message into individual issues
            combined_message = error_str.replace("Critical validation failed: ", "")
            individual_issues = [issue.strip() for issue in combined_message.split(";")]

            for issue in individual_issues:
                if issue:  # Skip empty issues
                    # Determine the field name and path based on the issue content
                    if "is set to null and will cause runtime crash" in issue:
                        param_name = issue.split(" is set to null")[0]
                        field_name = param_name
                        path = f"model.physics.{param_name}"
                    elif " -> " in issue or " \u2192 " in issue:
                        # StorageHeat parameter format: "site: storageheatmethod=6 -> properties.lambda_c must be set"
                        # Handle both ASCII (->) and Unicode (→) for backward compatibility
                        separator = " -> " if " -> " in issue else " \u2192 "
                        parts = issue.split(separator, 1)
                        if len(parts) == 2:
                            site_part = parts[0].strip()
                            param_part = parts[1].strip()
                            # Extract site name (before the colon)
                            site_name = (
                                site_part.split(":")[0].strip()
                                if ":" in site_part
                                else "Unknown"
                            )
                            field_name = param_part.split(" must be")[0].strip()
                            path = f"sites[{site_name}].properties.{field_name}"
                        else:
                            field_name = "StorageHeat parameter"
                            path = "configuration"
                    elif "must be set" in issue and ":" in issue:
                        # RSL parameter format: "site: for rslmethod=2 and bldgs.sfr=0.38, bldgs.faibldg must be set"
                        parts = issue.split(":", 1)
                        if len(parts) == 2:
                            site_name = parts[0].strip()
                            param_desc = parts[1].strip()
                            if "faibldg" in param_desc:
                                field_name = "bldgs.faibldg"
                                path = f"sites[{site_name}].properties.land_cover.bldgs.faibldg"
                            else:
                                field_name = "RSL parameter"
                                path = f"sites[{site_name}]"
                        else:
                            field_name = "RSL parameter"
                            path = "configuration"
                    else:
                        # Generic issue
                        field_name = "validation issue"
                        path = "configuration"

                    action_needed_items.append({
                        "field": field_name,
                        "path": path,
                        "error": issue,
                    })
        else:
            # Original single error handling
            action_needed_items.append({
                "field": "general",
                "path": "configuration",
                "error": error_str,
            })

    if action_needed_items:
        report_lines.append("## ACTION NEEDED")
        report_lines.append(
            f"- Found ({len(action_needed_items)}) critical configuration consistency error(s):"
        )

        for item in action_needed_items:
            report_lines.append(f"-- {item['field']}: {item['error']}")
            if item["path"] != "configuration":
                report_lines.append(f"   Location: {item['path']}")

        report_lines.append("")

    previous_phase_items = []

    phase_sections = [
        (phase_a_renames, "renamed parameter(s) to current standards"),
        (phase_a_optional_missing, "optional missing parameter(s) with null values"),
        (phase_a_not_in_standard, "parameter(s) not in standard"),
        (phase_b_updates, "parameter(s)"),
        (phase_b_science_warnings, "scientific warning(s) for information"),
    ]

    for items, description in phase_sections:
        if items:
            action = (
                "Updated"
                if "renamed" in description
                or "optional" in description
                or description == "parameter(s)"
                else "Revise"
                if "warning" in description
                else "Found"
            )
            previous_phase_items.append(f"- {action} ({len(items)}) {description}:")
            previous_phase_items.extend(f"-- {item}" for item in items)

    if previous_phase_items:
        report_lines.append("## NO ACTION NEEDED")
        report_lines.extend(previous_phase_items)
        report_lines.append("")

    if not action_needed_items and not previous_phase_items:
        # Map phase strings to descriptive messages
        if phase_str == "A":
            phase_message = "YAML structure check passed"
        elif phase_str == "B":
            phase_message = "Physics checks passed"
        elif phase_str == "C":
            phase_message = "Validation passed"
        elif phase_str == "AB":
            phase_message = "YAML structure check and Physics checks passed"
        elif phase_str == "BC":
            phase_message = "Physics checks and Validation passed"
        elif phase_str == "ABC" or phase_str == "AC":
            phase_message = "Validation passed"
        else:
            phase_message = f"Phase {phase_str} passed"  # fallback

        report_lines.append(phase_message)

    report_lines.extend(["", "# " + "=" * 50])

    REPORT_WRITER.write(output_report_file, "\n".join(report_lines))


def generate_fallback_report(
    validation_error: Exception,
    input_yaml_file: str,
    output_report_file: str,
    mode: str = "public",
    phase_a_report_file: str = None,
    phases_run: list = None,
    no_action_messages: list = None,
) -> None:
    """Generate simple fallback report when structured report generation fails."""
    phase_a_renames = []
    phase_a_optional_missing = []
    phase_a_not_in_standard = []
    phase_b_science_warnings = []
    phase_b_updates = []

    # Use passed no_action_messages if available, otherwise try to read from file
    if no_action_messages:
        # Parse the consolidated messages directly
        (
            phase_a_renames,
            phase_a_optional_missing,
            phase_a_not_in_standard,
            phase_b_science_warnings,
            phase_b_updates,
        ) = _parse_consolidated_messages(no_action_messages)
    elif phase_a_report_file and os.path.exists(phase_a_report_file):
        try:
            report_content = REPORT_WRITER.read(phase_a_report_file)
            (
                phase_a_renames,
                phase_a_optional_missing,
                phase_a_not_in_standard,
                phase_b_science_warnings,
            ) = _parse_previous_phase_report(report_content)
        except Exception:
            pass

    previous_phase_items = []

    phase_sections = [
        (phase_a_renames, "renamed parameter(s) to current standards"),
        (phase_a_optional_missing, "optional missing parameter(s) with null values"),
        (phase_a_not_in_standard, "parameter(s) not in standard"),
        (phase_b_updates, "parameter(s)"),
        (phase_b_science_warnings, "scientific warning(s) for information"),
    ]

    for items, description in phase_sections:
        if items:
            action = (
                "Updated"
                if "renamed" in description
                or "optional" in description
                or description == "parameter(s)"
                else "Revise"
                if "warning" in description
                else "Found"
            )
            previous_phase_items.append(f"- {action} ({len(items)}) {description}:")
            previous_phase_items.extend(f"-- {item}" for item in items)

    previous_phase_consolidation = (
        f"\n\n## NO ACTION NEEDED\n{chr(10).join(previous_phase_items)}"
        if previous_phase_items
        else ""
    )

    phase_str = "".join(phases_run) if phases_run else "C"
    title = REPORT_TITLE
    mode_title = "Public" if mode.lower() == "public" else mode.title()

    error_report = f"""# {title}
# ============================================
# Mode: {mode_title}
# ============================================

## ACTION NEEDED
- Found (1) critical configuration consistency error(s):
-- validation_error: {str(validation_error)}
   Suggested fix: Review and fix validation errors above
   Location: {input_yaml_file}{previous_phase_consolidation}

# ==================================================
"""

    REPORT_WRITER.write(output_report_file, error_report)
