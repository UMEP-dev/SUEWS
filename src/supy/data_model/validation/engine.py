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
import types
from typing import TYPE_CHECKING, Annotated, Any, ForwardRef, Union, get_args, get_origin

from pydantic import BaseModel
import yaml

from ..core._yaml_io import safe_load_yaml
from ..core.field_renames import normalise_yaml_renames, read_physics_key
from ..core.physics_families import flatten_physics_in_config
from ..core.profile import (
    normalise_hourly_profile_keys_in_raw_config,
    normalise_ten_minute_profile_keys_in_raw_config,
)
from .core.yaml_helpers import get_stebbsmethod_value
from .pipeline import phase_a, phase_b, phase_c
from .pipeline.document import thaw_copy
from .pipeline.orchestrator import VALID_PIPELINES, run_pipeline
from .pipeline.phase_b_rules import RulesRegistry, ValidationContext, ValidationResult
from .pipeline.report_schema import (
    SEVERITY_ERROR,
    SEVERITY_WARNING,
    Issue,
    PhaseReport,
    ValidationReport,
)
from .rules.readonly_model import ReadOnlyModelProxy, _unwrap_model_for_validation, freeze_model

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

    try:
        snowuse = read_physics_key(physics, "snow_use")
    except (ValueError, TypeError):
        # Defer invalid snow_use/snow values to downstream structural/Pydantic
        # validation instead of crashing public-mode policy checks.
        snowuse = None
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


def _strip_unauthored_critical_physics(
    config: SUEWSConfig, raw_data: dict[str, Any]
) -> None:
    """Drop required physics defaults that were never authored on *config*.

    ``model_dump()`` materialises Pydantic enum defaults for omitted switches,
    which would otherwise disguise an omitted choice as an explicit setting
    when validating an in-memory ``SUEWSConfig``. Inspecting live
    ``model_fields_set`` preserves omission provenance while still honouring
    subsequent in-memory assignments on *config*.
    """
    model_obj = getattr(config, "model", None)
    raw_model = raw_data.get("model")
    if not isinstance(model_obj, BaseModel) or not isinstance(raw_model, dict):
        return
    raw_physics = raw_model.get("physics")
    if not isinstance(raw_physics, dict):
        return

    physics_obj = getattr(model_obj, "physics", None)
    if isinstance(physics_obj, BaseModel):
        physics_fields_set = physics_obj.model_fields_set
        for param in phase_c._CRITICAL_PHYSICS_PARAMS:
            if param in phase_c._STEBBS_LEAF_NAMES:
                continue
            if param not in physics_fields_set:
                raw_physics.pop(param, None)
    elif not isinstance(physics_obj, Mapping):
        for param in phase_c._CRITICAL_PHYSICS_PARAMS:
            if param in phase_c._STEBBS_LEAF_NAMES:
                continue
            raw_physics.pop(param, None)

    raw_stebbs = raw_physics.get("stebbs")
    if not isinstance(raw_stebbs, dict):
        return
    if isinstance(physics_obj, BaseModel):
        stebbs_obj = getattr(physics_obj, "stebbs", None)
    elif isinstance(physics_obj, Mapping):
        stebbs_obj = physics_obj.get("stebbs")
    else:
        stebbs_obj = None
    if isinstance(stebbs_obj, BaseModel):
        stebbs_fields_set = stebbs_obj.model_fields_set
        for param in phase_c._STEBBS_LEAF_NAMES:
            if param not in stebbs_fields_set:
                raw_stebbs.pop(param, None)
    elif not isinstance(stebbs_obj, Mapping):
        for param in phase_c._STEBBS_LEAF_NAMES:
            raw_stebbs.pop(param, None)


def _annotation_allows_none(annotation: Any) -> bool:
    """Return True when *annotation* permits ``None`` (or is unconstrained)."""
    if (
        annotation is None
        or annotation is Any
        or annotation is type(None)
        or isinstance(annotation, (str, ForwardRef))
    ):
        return True
    origin = get_origin(annotation)
    if origin is Annotated:
        args = get_args(annotation)
        return bool(args) and _annotation_allows_none(args[0])
    if origin in (Union, types.UnionType):
        return any(_annotation_allows_none(arg) for arg in get_args(annotation))
    return False


def _expected_model_or_list_kind(
    annotation: Any, *, ref_value_cls: type[Any]
) -> tuple[str | None, Any]:
    """Classify a field annotation as expecting a nested ``BaseModel``, ``list``, or other."""
    while get_origin(annotation) is Annotated:
        args = get_args(annotation)
        if not args:
            return None, None
        annotation = args[0]

    origin = get_origin(annotation)
    if origin in (Union, types.UnionType):
        arms = [arg for arg in get_args(annotation) if arg is not type(None)]
        if not arms:
            return None, None
        if all(
            isinstance(arm, type)
            and issubclass(arm, BaseModel)
            and not issubclass(arm, ref_value_cls)
            for arm in arms
        ):
            return "model", tuple(arms)
        if len(arms) == 1:
            return _expected_model_or_list_kind(arms[0], ref_value_cls=ref_value_cls)
        return None, None

    if (
        isinstance(annotation, type)
        and issubclass(annotation, BaseModel)
        and not issubclass(annotation, ref_value_cls)
    ):
        return "model", (annotation,)

    if annotation is list or origin is list:
        args = get_args(annotation)
        return "list", args[0] if args else Any

    return None, None


def _validate_live_model_shapes(
    model_obj: BaseModel, *, ref_value_cls: type[Any], path: str = ""
) -> list[str]:
    """Recursively reject explicit nulls on non-optional fields and malformed nested shapes.

    Live ``BaseModel`` instances mutated via ``model_copy(update=...)`` or
    ``object.__setattr__`` can carry ``None`` on non-optional non-RefValue
    fields (such as ``sites=None``) or non-model values in nested model slots
    (such as ``initial_states.evetr = 7``). Checking these before running
    pre-dump validators or ``model_dump(exclude_none=True)`` ensures explicit
    nulls are never erased into defaults and malformed shapes never escape as
    ``AttributeError`` / ``TypeError``.
    """
    if isinstance(model_obj, ref_value_cls):
        return []

    errors: list[str] = []
    for field_name, field_info in type(model_obj).model_fields.items():
        if field_name.startswith("_"):
            continue
        val = getattr(model_obj, field_name, None)
        field_path = f"{path}.{field_name}" if path else field_name
        if val is None:
            if (
                not _annotation_allows_none(field_info.annotation)
                and field_info.default is not None
            ):
                errors.append(f"{field_path}: value cannot be null")
            continue

        kind, target = _expected_model_or_list_kind(
            field_info.annotation, ref_value_cls=ref_value_cls
        )
        if kind == "model":
            if isinstance(val, BaseModel):
                errors.extend(
                    _validate_live_model_shapes(
                        val, ref_value_cls=ref_value_cls, path=field_path
                    )
                )
            elif not isinstance(val, Mapping):
                errors.append(
                    f"{field_path}: expected mapping or model instance, "
                    f"got {type(val).__name__}"
                )
        elif kind == "list":
            if not isinstance(val, list):
                errors.append(
                    f"{field_path}: expected list, got {type(val).__name__}"
                )
            else:
                item_kind, _ = _expected_model_or_list_kind(
                    target, ref_value_cls=ref_value_cls
                )
                for idx, item in enumerate(val):
                    item_path = f"{field_path}[{idx}]"
                    if item is None:
                        if not _annotation_allows_none(target):
                            errors.append(f"{item_path}: value cannot be null")
                    elif item_kind == "model":
                        if isinstance(item, BaseModel):
                            errors.extend(
                                _validate_live_model_shapes(
                                    item,
                                    ref_value_cls=ref_value_cls,
                                    path=item_path,
                                )
                            )
                        elif not isinstance(item, Mapping):
                            errors.append(
                                f"{item_path}: expected mapping or model instance, "
                                f"got {type(item).__name__}"
                            )
                    elif isinstance(item, BaseModel):
                        errors.extend(
                            _validate_live_model_shapes(
                                item, ref_value_cls=ref_value_cls, path=item_path
                            )
                        )
        elif isinstance(val, BaseModel):
            errors.extend(
                _validate_live_model_shapes(
                    val, ref_value_cls=ref_value_cls, path=field_path
                )
            )
        elif isinstance(val, Mapping):
            for key, item in val.items():
                if isinstance(item, BaseModel):
                    errors.extend(
                        _validate_live_model_shapes(
                            item,
                            ref_value_cls=ref_value_cls,
                            path=f"{field_path}.{key}",
                        )
                    )
    return errors


def _restore_explicit_and_non_optional_nulls(
    model_obj: BaseModel, dumped: dict[str, Any], *, ref_value_cls: type[Any]
) -> None:
    """Restore explicit ``None`` values (and non-optional nulls) stripped by ``exclude_none=True``.

    Preserves optional fields explicitly set to ``None`` (via ``model_fields_set``)
    so downstream scientific checks see the same nulls as an equivalent mapping
    or YAML document, while leaving unauthored optional defaults omitted.
    """
    if isinstance(model_obj, ref_value_cls):
        return
    fields_set = getattr(model_obj, "model_fields_set", frozenset())
    for field_name, field_info in type(model_obj).model_fields.items():
        if field_name.startswith("_"):
            continue
        val = getattr(model_obj, field_name, None)
        if val is None:
            if field_name not in dumped and (
                field_name in fields_set
                or (
                    not _annotation_allows_none(field_info.annotation)
                    and field_info.default is not None
                )
            ):
                dumped[field_name] = None
            continue
        target = dumped.get(field_name)
        if (
            isinstance(val, BaseModel)
            and not isinstance(val, ref_value_cls)
            and isinstance(target, dict)
        ):
            _restore_explicit_and_non_optional_nulls(
                val, target, ref_value_cls=ref_value_cls
            )
        elif isinstance(val, list) and isinstance(target, list):
            for item_obj, item_target in zip(val, target):
                if (
                    isinstance(item_obj, BaseModel)
                    and not isinstance(item_obj, ref_value_cls)
                    and isinstance(item_target, dict)
                ):
                    _restore_explicit_and_non_optional_nulls(
                        item_obj, item_target, ref_value_cls=ref_value_cls
                    )
        elif isinstance(val, Mapping) and isinstance(target, dict):
            for key, item_obj in val.items():
                item_target = target.get(key)
                if (
                    isinstance(item_obj, BaseModel)
                    and not isinstance(item_obj, ref_value_cls)
                    and isinstance(item_target, dict)
                ):
                    _restore_explicit_and_non_optional_nulls(
                        item_obj, item_target, ref_value_cls=ref_value_cls
                    )


def _validate_dict_backed_profile_keys(config_copy: SUEWSConfig) -> None:
    """Validate hourly and 10-minute profile keys on a Python-mode dump before JSON coercion.

    Catches mixed integer/string key collisions (such as ``1`` alongside ``"1"``)
    even on unchecked ``SUEWSConfig`` objects whose ``sites`` entries remain raw
    mappings (for example from ``SUEWSConfig.from_dict(..., use_conditional_validation=False)``
    or ``model_copy(update=...)``).
    """
    from ..core.profile import (  # noqa: PLC0415
        _HOURLY_PROFILE_PATHS,
        _TEN_MINUTE_PROFILE_RAW_PATHS,
        normalise_hourly_profile_mapping,
        normalise_ten_minute_profile_mapping,
    )

    raw_py = config_copy.model_dump(mode="python", round_trip=True)
    sites = raw_py.get("sites") if isinstance(raw_py, Mapping) else None
    if not isinstance(sites, list):
        return

    errors: list[str] = []
    for site_idx, site in enumerate(sites):
        if not isinstance(site, Mapping):
            continue
        props = site.get("properties")
        if not isinstance(props, Mapping):
            continue
        for paths, normalise_mapping in (
            (_HOURLY_PROFILE_PATHS, normalise_hourly_profile_mapping),
            (_TEN_MINUTE_PROFILE_RAW_PATHS, normalise_ten_minute_profile_mapping),
        ):
            for rel_path in paths:
                node: Any = props
                for part in rel_path:
                    if not isinstance(node, Mapping) or part not in node:
                        node = None
                        break
                    node = node[part]
                    if (
                        isinstance(node, Mapping)
                        and "value" in node
                        and isinstance(node["value"], Mapping)
                    ):
                        node = node["value"]
                if not isinstance(node, Mapping):
                    continue
                for day_type in ("working_day", "holiday"):
                    day_map = node.get(day_type)
                    if (
                        isinstance(day_map, Mapping)
                        and "value" in day_map
                        and isinstance(day_map["value"], Mapping)
                    ):
                        day_map = day_map["value"]
                    if not isinstance(day_map, Mapping):
                        continue
                    try:
                        normalise_mapping(dict(day_map))
                    except ValueError as exc:
                        path_str = ".".join(rel_path)
                        errors.append(
                            f"sites[{site_idx}].properties.{path_str}.{day_type}: {exc}"
                        )
    if errors:
        raise ValueError("\n".join(errors))


def _validate_suews_config_before_dump(
    config: SUEWSConfig, *, first_phase: str
) -> tuple[SUEWSConfig, PhaseReport | None]:
    """Validate live model shapes, profile keys, and senescence states on a deep copy.

    ``model_dump(exclude_none=True, mode="json")`` coerces integer keys to
    strings (silently overwriting mixed int/string key collisions such as ``1``
    alongside ``"1"``) and drops explicit ``None`` values (such as
    ``sites=None`` or ``sdd_id=None`` from ``model_copy(update=...)``).
    Validating nested shapes first, then live invariants on a deep copy,
    mirrors ``SUEWSConfig.to_yaml()`` without mutating the caller's live
    ``SUEWSConfig`` instance or letting malformed nested values escape as raw
    exceptions.
    """
    from ..core.site import Site  # noqa: PLC0415
    from ..core.type import RefValue  # noqa: PLC0415

    config_copy = deepcopy(config)

    shape_errors = _validate_live_model_shapes(config_copy, ref_value_cls=RefValue)
    if shape_errors:
        return (
            config_copy,
            PhaseReport(
                phase=first_phase,
                issues=[
                    Issue(
                        phase=first_phase,
                        severity=SEVERITY_ERROR,
                        code=f"{first_phase}.CONFIG_CONSISTENCY.INVALID_MODEL_STATE",
                        message=msg,
                        category="CONFIG_CONSISTENCY",
                    )
                    for msg in shape_errors
                ],
                yaml_in=None,
            ),
        )

    validators: list[tuple[str, Any]] = []
    if isinstance(config_copy.sites, list) and all(
        isinstance(site, Site) for site in config_copy.sites
    ):
        validators.extend(
            [
                ("PROFILE_KEY_ERROR", config_copy.validate_hourly_profile_hours),
                ("PROFILE_KEY_ERROR", config_copy.validate_ten_minute_profile_keys),
            ]
        )
    validators.append(
        (
            "PROFILE_KEY_ERROR",
            lambda: _validate_dict_backed_profile_keys(config_copy),
        )
    )
    validators.append(("SENESCENCE_STATE_ERROR", config_copy._validate_senescence_states))
    validators.append(("NON_NULL_VALUE_ERROR", config_copy.validate_non_null_ref_values))

    issues: list[Issue] = []
    has_profile_error = False
    for code_suffix, validator in validators:
        if code_suffix == "PROFILE_KEY_ERROR" and has_profile_error:
            continue
        try:
            validator()
        except (ValueError, TypeError, AttributeError) as exc:
            if code_suffix == "PROFILE_KEY_ERROR":
                has_profile_error = True
            lines = [line.strip() for line in str(exc).splitlines() if line.strip()]
            if not lines:
                lines = [f"{type(exc).__name__}: invalid configuration state"]
            for stripped in lines:
                issues.append(
                    Issue(
                        phase=first_phase,
                        severity=SEVERITY_ERROR,
                        code=f"{first_phase}.CONFIG_CONSISTENCY.{code_suffix}",
                        message=stripped,
                        category="CONFIG_CONSISTENCY",
                    )
                )

    if issues:
        return (
            config_copy,
            PhaseReport(
                phase=first_phase,
                issues=issues,
                yaml_in=None,
            ),
        )
    return config_copy, None


def _dump_suews_config(
    config: SUEWSConfig, *, dumped_copy: SUEWSConfig | None = None
) -> dict[str, Any]:
    """Serialise an in-memory ``SUEWSConfig`` to a YAML-equivalent dict.

    Mirrors ``SUEWSConfig.to_yaml``: ``round_trip=True`` excludes computed
    fields (such as ``stebbs.deep_soil_temperature``), ``exclude_none=True``
    avoids inapplicable null slots while restoring explicit nulls on
    non-optional fields, private bookkeeping keys are stripped, explicit
    runtime-null ``forcing.file`` is preserved, and unauthored required
    physics defaults are removed using live ``model_fields_set``.
    """
    from ..core.type import RefValue  # noqa: PLC0415

    source_for_dump = config if dumped_copy is None else dumped_copy
    raw_data = source_for_dump.model_dump(
        exclude_none=True, mode="json", round_trip=True
    )
    _restore_explicit_and_non_optional_nulls(
        source_for_dump, raw_data, ref_value_cls=RefValue
    )
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
    _strip_unauthored_critical_physics(config, raw_data)
    for key in ("_yaml_path", "_auto_generate_annotated", "_yaml_raw"):
        raw_data.pop(key, None)
    if raw_data.get("strict_initial_state_bounds") is True:
        raw_data.pop("strict_initial_state_bounds", None)
    return raw_data


def _load_source_in_memory(
    source: str | Path | Mapping[str, Any] | SUEWSConfig | ReadOnlyModelProxy,
    *,
    first_phase: str,
) -> tuple[Any, Path | None, PhaseReport | None]:
    """Resolve *source* into ``(raw_data, source_path, error_report)`` without writes."""
    from ..core.config import SUEWSConfig  # noqa: PLC0415

    if isinstance(source, ReadOnlyModelProxy):
        try:
            source = _unwrap_model_for_validation(source)
        except TypeError as exc:
            return (
                None,
                None,
                PhaseReport(
                    phase=first_phase,
                    issues=[
                        Issue(
                            phase=first_phase,
                            severity=SEVERITY_ERROR,
                            code=f"{first_phase}.CONFIG_CONSISTENCY.INVALID_MODEL_STATE",
                            message=f"Could not serialise read-only config proxy safely: {exc}",
                            category="CONFIG_CONSISTENCY",
                        )
                    ],
                    yaml_in=None,
                ),
            )

    if isinstance(source, SUEWSConfig):
        validated_copy, pre_dump_err = _validate_suews_config_before_dump(
            source, first_phase=first_phase
        )
        if pre_dump_err is not None:
            return None, None, pre_dump_err
        try:
            dumped = _dump_suews_config(source, dumped_copy=validated_copy)
        except (ValueError, TypeError, AttributeError) as exc:
            return (
                None,
                None,
                PhaseReport(
                    phase=first_phase,
                    issues=[
                        Issue(
                            phase=first_phase,
                            severity=SEVERITY_ERROR,
                            code=f"{first_phase}.CONFIG_CONSISTENCY.INVALID_MODEL_STATE",
                            message=f"Could not serialise SUEWSConfig: {exc}",
                            category="CONFIG_CONSISTENCY",
                        )
                    ],
                    yaml_in=None,
                ),
            )
        return dumped, None, None

    if isinstance(source, Mapping):
        return thaw_copy(source), None, None

    if isinstance(source, (str, Path)):
        path = Path(source).resolve()
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
        except (OSError, UnicodeDecodeError) as exc:
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
        except (yaml.YAMLError, ValueError) as exc:
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

    if not isinstance(raw_data, Mapping):
        return (
            raw_data,
            PhaseReport(
                phase="A",
                issues=[
                    Issue(
                        phase="A",
                        severity=SEVERITY_ERROR,
                        code="A.PIPELINE.YAML_NOT_MAPPING",
                        message=(
                            "YAML root must be a mapping (got "
                            f"{type(raw_data).__name__})"
                        ),
                        category="STRUCTURE",
                    )
                ],
                yaml_in=str(source_path) if source_path is not None else None,
            ),
        )

    working = thaw_copy(raw_data)
    forcing_snapshot = (
        phase_a._snapshot_forcing_user_data(working)
        if forcing == "on" and source_path is not None
        else phase_a._MISSING
    )
    nlayer = phase_a.detect_nlayer_from_yaml_data(working)
    legacy_soil = _uses_legacy_observed_soil(working)
    soil_conflicts = _soil_contract_conflicts(working)
    rename_skip = phase_a.LEGACY_SOIL_SELECTORS if legacy_soil else ()

    try:
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
    except ValueError as exc:
        return (
            working,
            PhaseReport(
                phase="A",
                issues=[
                    Issue(
                        phase="A",
                        severity=SEVERITY_ERROR,
                        code="A.PIPELINE.NORMALISATION_ERROR",
                        message=str(exc),
                        yaml_path="model.physics",
                        category="STRUCTURE",
                    )
                ],
                yaml_in=str(source_path) if source_path is not None else None,
            ),
        )

    nlayer_limit_errors = phase_a.validate_nlayer_limit(working)
    dimension_errors = []
    if nlayer is not None:
        try:
            working, dimension_errors = phase_a.validate_nlayer_dimensions(
                working, nlayer, per_site=True
            )
        except (AttributeError, TypeError, ValueError) as exc:
            return (
                working,
                PhaseReport(
                    phase="A",
                    issues=[
                        Issue(
                            phase="A",
                            severity=SEVERITY_ERROR,
                            code="A.PIPELINE.DIMENSION_VALIDATION_ERROR",
                            message=f"Invalid vertical layer structure: {exc}",
                            yaml_path="sites",
                            category="STRUCTURE",
                        )
                    ],
                    yaml_in=str(source_path) if source_path is not None else None,
                ),
            )

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


def _prepare_typed_model_proxy(
    typed_model: BaseModel | ReadOnlyModelProxy | None,
) -> ReadOnlyModelProxy | None:
    """Return a read-only proxy for Phase B rules without failing on private metadata."""
    if isinstance(typed_model, ReadOnlyModelProxy):
        return typed_model
    if not isinstance(typed_model, BaseModel):
        return None
    try:
        return freeze_model(typed_model)
    except TypeError:
        try:
            clean_model = type(typed_model).model_validate(
                typed_model.model_dump(mode="python")
            )
            return freeze_model(clean_model)
        except Exception:  # noqa: BLE001
            return None


def _run_phase_b_in_memory(
    working_data: Any,
    *,
    source_path: Path | None,
    science_fixes: str,
    upstream_has_errors: bool,
    base_dir: Path | None = None,
    typed_model: BaseModel | ReadOnlyModelProxy | None = None,
) -> tuple[Any, PhaseReport]:
    """Run Phase B scientific checks and optional in-memory adjustments."""
    if not isinstance(working_data, Mapping):
        return (
            working_data,
            PhaseReport(
                phase="B",
                issues=[
                    Issue(
                        phase="B",
                        severity=SEVERITY_ERROR,
                        code="B.PIPELINE.YAML_NOT_MAPPING",
                        message=(
                            "YAML root must be a mapping (got "
                            f"{type(working_data).__name__})"
                        ),
                        category="STRUCTURE",
                    )
                ],
                yaml_in=str(source_path) if source_path is not None else None,
            ),
        )

    data = thaw_copy(working_data)
    effective_fixes = (
        "off" if upstream_has_errors and science_fixes == "apply" else science_fixes
    )

    try:
        data = normalise_yaml_renames(data)
        data, _ = normalise_hourly_profile_keys_in_raw_config(data)
        data, _ = normalise_ten_minute_profile_keys_in_raw_config(data)
        flatten_physics_in_config(data)
        resolved_base_dir = (
            base_dir
            if base_dir is not None
            else (source_path.parent if source_path is not None else None)
        )
        model_year, start_date, _end_date, bound_notes = (
            phase_b.resolve_simulation_parameters(
                data,
                resolved_base_dir,
            )
        )
        frozen_typed_model = _prepare_typed_model_proxy(typed_model)
        validation_context = ValidationContext(
            yaml_data=data,
            start_date=start_date,
            model_year=model_year,
            effective_fixes=effective_fixes,
            typed_model=frozen_typed_model,
        )
        validation_results = bound_notes + RulesRegistry(
            context=validation_context
        ).run_validation()
    except (ValueError, KeyError, FileNotFoundError, TypeError) as exc:
        error_message = str(exc)
        parts = error_message.split("; ") if ";" in error_message else [error_message]
        validation_results = [
            ValidationResult(
                parameter="model.control",
                site_index=None,
                status="ERROR",
                message=f"Phase B initialization failed: {part.strip()}",
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

    critical_errors = [r for r in validation_results if r.status == "ERROR"]
    adjustments = []
    suggestions = []
    adjustment_errors: list[ValidationResult] = []
    if not critical_errors:
        if effective_fixes == "apply":
            data, adjustments = phase_b.run_scientific_adjustment_pipeline(
                data,
                start_date,
                model_year,
                errors=adjustment_errors,
            )
        elif effective_fixes == "suggest":
            suggestions = phase_b.collect_science_suggestions(
                data,
                start_date,
                model_year,
                errors=adjustment_errors,
            )
    if adjustment_errors:
        validation_results = [*validation_results, *adjustment_errors]
        adjustments = []
        data = thaw_copy(working_data) if isinstance(working_data, Mapping) else {}

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
    check_missing_critical_physics: bool = False,
) -> PhaseReport:
    """Run single-pass Phase C Pydantic validation in memory."""
    result = phase_c.validate_phase_c(
        str(source_path) if source_path is not None else None,
        raw_data=deepcopy(working_data) if isinstance(working_data, dict) else working_data,
        check_missing_critical_physics=check_missing_critical_physics,
    )
    return PhaseReport(
        phase="C",
        issues=result.issues,
        yaml_in=str(source_path) if source_path is not None else None,
    )


def _build_policy_issues(
    raw_data: Any, *, mode_norm: str, first_phase: str
) -> list[Issue]:
    if mode_norm != "public":
        return []
    return [
        Issue(
            phase=first_phase,
            severity=SEVERITY_ERROR,
            code="MODE.PUBLIC.RESTRICTED_FEATURE",
            message=msg,
            yaml_path="model.physics",
            category="POLICY",
        )
        for msg in check_public_mode_restrictions(raw_data)
    ]


def _prepend_phase_issues(
    phases: list[PhaseReport],
    issues: list[Issue],
    phase_name: str,
    yaml_in: str | None,
) -> None:
    if phases:
        phases[0].issues = [*issues, *phases[0].issues]
    else:
        phases.append(PhaseReport(phase=phase_name, issues=issues, yaml_in=yaml_in))


class ValidationEngine:
    """Single entry point for SUEWS YAML configuration validation.

    Runs phases A (structural completeness), B (scientific consistency), and
    C (Pydantic model validation) in memory by default with zero disk writes.
    Pass ``write_outputs=True`` with a file path to run the transactional
    staged writer that publishes ``updated_<name>.yml``, ``report_<stem>.txt``,
    and ``report_<stem>.json``.
    """

    def _validate_staged(
        self,
        source: str | Path | Mapping[str, Any] | SUEWSConfig | ReadOnlyModelProxy,
        *,
        pipeline_norm: str,
        mode_norm: str,
        forcing_norm: str,
        fixes_norm: str,
        continue_on_error: bool,
    ) -> ValidationReport:
        if not isinstance(source, (str, Path)):
            raise ValueError(
                "write_outputs=True requires a YAML file path (str or Path)."
            )
        path_str = str(Path(source).resolve())
        raw_for_policy, _, load_err = _load_source_in_memory(
            path_str, first_phase=pipeline_norm[0]
        )
        if load_err is not None:
            return ValidationReport(
                phases=[load_err],
                _render_mode=mode_norm,
                _render_science_fixes=fixes_norm,
            )
        policy_issues = _build_policy_issues(
            raw_for_policy,
            mode_norm=mode_norm,
            first_phase=pipeline_norm[0],
        )
        if policy_issues and not continue_on_error:
            return ValidationReport(
                phases=[
                    PhaseReport(
                        phase=pipeline_norm[0],
                        issues=policy_issues,
                        yaml_in=path_str,
                    )
                ],
                _render_mode=mode_norm,
                _render_science_fixes=fixes_norm,
            )
        staged = run_pipeline(
            path_str,
            pipeline=pipeline_norm,
            mode=mode_norm,
            forcing=forcing_norm,
            science_fixes=(
                "off" if policy_issues and fixes_norm == "apply" else fixes_norm
            ),
            continue_on_error=continue_on_error,
            prepend_issues=policy_issues or None,
        )
        staged_phases = list(staged.phases)
        if staged.cleanup_warning:
            evidence_note = (
                f" Evidence preserved at: {staged.evidence_dir}"
                if staged.evidence_dir
                else ""
            )
            cleanup_issue = Issue(
                phase=pipeline_norm[-1],
                severity=SEVERITY_WARNING,
                code="PIPELINE.CLEANUP_WARNING",
                message=f"{staged.cleanup_warning}{evidence_note}",
                category="PIPELINE",
            )
            if staged_phases:
                staged_phases[-1].issues.append(cleanup_issue)
            else:
                staged_phases.append(
                    PhaseReport(
                        phase=pipeline_norm[-1],
                        issues=[cleanup_issue],
                        yaml_in=path_str,
                    )
                )
        return ValidationReport(
            phases=staged_phases,
            _render_mode=mode_norm,
            _render_science_fixes=fixes_norm,
        )

    def validate(
        self,
        source: str | Path | Mapping[str, Any] | SUEWSConfig | ReadOnlyModelProxy,
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
            return self._validate_staged(
                source,
                pipeline_norm=pipeline_norm,
                mode_norm=mode_norm,
                forcing_norm=forcing_norm,
                fixes_norm=fixes_norm,
                continue_on_error=continue_on_error,
            )

        first_phase = pipeline_norm[0]
        raw_data, source_path, load_error = _load_source_in_memory(
            source, first_phase=first_phase
        )
        if load_error is not None:
            return ValidationReport(
                phases=[load_error],
                _render_mode=mode_norm,
                _render_science_fixes=fixes_norm,
            )
        unwrapped_source: Any = (
            _unwrap_model_for_validation(source)
            if isinstance(source, ReadOnlyModelProxy)
            else source
        )

        policy_issues = _build_policy_issues(
            raw_data, mode_norm=mode_norm, first_phase=first_phase
        )
        if policy_issues and not continue_on_error:
            return ValidationReport(
                phases=[
                    PhaseReport(
                        phase=first_phase,
                        issues=policy_issues,
                        yaml_in=str(source_path) if source_path else None,
                    )
                ],
                _render_mode=mode_norm,
                _render_science_fixes=fixes_norm,
            )

        phases: list[PhaseReport] = []
        working_data = raw_data
        has_errors = bool(policy_issues)

        if "A" in pipeline_norm:
            phase_a_out, phase_a_report = _run_phase_a_in_memory(
                raw_data,
                source_path=source_path,
                forcing=forcing_norm,
            )
            phases.append(phase_a_report)
            if not phase_a_report.has_errors:
                working_data = phase_a_out
            has_errors = has_errors or phase_a_report.has_errors

        bound_base_dir: Path | None = (
            source_path.parent if source_path is not None else None
        )
        if bound_base_dir is None:
            yaml_path_attr = getattr(unwrapped_source, "_yaml_path", None)
            if isinstance(yaml_path_attr, (str, Path)) and str(yaml_path_attr).strip():
                bound_base_dir = Path(yaml_path_attr).resolve().parent

        if "B" in pipeline_norm and (not has_errors or continue_on_error):
            phase_b_typed_source: BaseModel | ReadOnlyModelProxy | None = (
                source
                if isinstance(source, ReadOnlyModelProxy)
                else (
                    unwrapped_source
                    if isinstance(unwrapped_source, BaseModel)
                    else None
                )
            )
            phase_b_out, phase_b_report = _run_phase_b_in_memory(
                working_data,
                source_path=source_path,
                science_fixes=fixes_norm,
                upstream_has_errors=has_errors,
                base_dir=bound_base_dir,
                typed_model=phase_b_typed_source,
            )
            phases.append(phase_b_report)
            if not phase_b_report.has_errors:
                working_data = phase_b_out
            has_errors = has_errors or phase_b_report.has_errors

        if "C" in pipeline_norm and (not has_errors or continue_on_error):
            phase_c_report = _run_phase_c_in_memory(
                working_data,
                source_path=source_path,
                check_missing_critical_physics="A" not in pipeline_norm,
            )
            phases.append(phase_c_report)

        if policy_issues:
            _prepend_phase_issues(
                phases,
                policy_issues,
                first_phase,
                str(source_path) if source_path else None,
            )

        return ValidationReport(
            phases=phases,
            _render_mode=mode_norm,
            _render_science_fixes=fixes_norm,
        )


def validate_config(
    source: str | Path | Mapping[str, Any] | SUEWSConfig | ReadOnlyModelProxy,
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
    source : str, pathlib.Path, Mapping, SUEWSConfig, or ReadOnlyModelProxy
        Path to a SUEWS YAML configuration, an in-memory config mapping, an
        already-loaded :class:`~supy.data_model.SUEWSConfig`, or a
        :class:`~supy.data_model.validation.rules.readonly_model.ReadOnlyModelProxy`.
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
