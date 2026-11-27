"""Shared Tier-1 hard invariants for SUEWS configuration validation.

Every check in this module is pure and schema/model-agnostic: it operates on
plain unwrapped Python values so both Pydantic data-model validators
(``SUEWSConfig``, ``IrrigationParams``, ``StebbsProperties``) and Phase B
scientific rules share one authoritative decision kernel.

Design constraints
------------------
- No raw ``["value"]`` or ``.get("value")`` unwrapping here; callers unwrap
  values through ``pipeline.document`` or Pydantic attribute access first.
- No top-level imports from ``supy.data_model.core`` or ``pipeline`` so core
  models can import this module without circular import hazards.
"""

from __future__ import annotations

import calendar
from collections.abc import Callable, Mapping, Sequence
from dataclasses import dataclass
import math
from typing import Any

import numpy as np

from ....constants import C2K
from ..required_fields import (
    BUILDING_PRESENT_SFR_TOL,
    SPARTACUS_BUILDING_STORAGE_METHODS,
    SPARTACUS_OBSERVED_STORAGE_METHOD,
    stebbs_shading_requirements,
)

SPARTACUS_METHODS: frozenset[int] = frozenset({1001, 1002, 1003})
SAME_SURFACE_TOL: float = 1e-9
_IRRIGATION_WEEKDAYS: tuple[str, ...] = (
    "monday",
    "tuesday",
    "wednesday",
    "thursday",
    "friday",
    "saturday",
    "sunday",
)
_WATER_USE_STRING_CODES: dict[str, int] = {
    "modelled": 0,
    "modeled": 0,
    "observed": 1,
}
LAND_COVER_SUM_PASS_TOL: float = 1e-6
LAND_COVER_SUM_AUTO_FIX_TOL: float = 1e-4

_SHADING_MISSING_SUGGESTIONS: dict[str, str] = {
    "reduction_factor_shading": "Provide a value between 0 and 1",
    "temperature_threshold_shading": "Provide temperature_threshold_shading in degC",
    "radiation_threshold_shading": "Provide radiation_threshold_shading in W m-2",
}

_SHADING_THRESHOLD_SPECS: tuple[tuple[str, float, bool, str], ...] = (
    ("temperature_threshold_shading", -C2K, False, "degC"),
    ("radiation_threshold_shading", 0.0, True, "W m-2"),
)


@dataclass(frozen=True)
class InvariantViolation:
    """One hard-invariant violation produced by the shared validation kernel."""

    code: str
    category: str
    parameter: str
    message: str
    suggested_value: str | None = None
    site_index: int | None = None
    site_gridid: Any = None


def _default_coerce_doy(value: Any, name: str) -> int:
    # Lazy import avoids circular bootstrap with supy.data_model.core.human_activity.
    from ...core.human_activity import coerce_integer_doy_scalar  # noqa: PLC0415

    return coerce_integer_doy_scalar(value, name)


def _is_bool_like(value: Any) -> bool:
    """Return True for Python ``bool`` and NumPy ``bool_`` scalars."""
    return isinstance(value, (bool, np.bool_))


def _is_finite_real_number(value: Any) -> bool:
    if not isinstance(value, (int, float, np.number)) or _is_bool_like(value):
        return False
    try:
        return math.isfinite(float(value))
    except (TypeError, ValueError, OverflowError):
        return False


def check_spartacus_storage_heat(
    net_radiation_code: int | None,
    storage_heat_code: int | None,
    *,
    bldgs_present: bool,
    bldgs_sfr: float | None = None,
    stebbs_method: int | None = None,
    site_name: str | None = None,
    check_stebbs_for_method_7: bool = False,
) -> list[InvariantViolation]:
    """Validate storage-heat compatibility with SPARTACUS radiation (gh#1834).

    Parameters
    ----------
    net_radiation_code : int or None
        Resolved integer code for ``model.physics.net_radiation``.
    storage_heat_code : int or None
        Resolved integer code for ``model.physics.storage_heat``.
    bldgs_present : bool
        Whether buildings are present (``land_cover.bldgs.sfr > BUILDING_PRESENT_SFR_TOL``).
    bldgs_sfr : float, optional
        Concrete site building surface fraction when checking one site.
    stebbs_method : int, optional
        Composed STEBBS method code (0/1/2), checked when
        ``check_stebbs_for_method_7`` is True.
    site_name : str, optional
        Site label used in ``SUEWSConfig`` per-site error messages. When
        ``None``, emits the model-wide Phase B phrasing.
    check_stebbs_for_method_7 : bool, default False
        When True (``SUEWSConfig`` path), also reject ``storage_heat == 7``
        with buildings present when STEBBS is not enabled. Phase B reports that
        separately in ``validate_storageheatmethod_dependencies``.
    """
    if net_radiation_code not in SPARTACUS_METHODS or storage_heat_code is None:
        return []

    violations: list[InvariantViolation] = []

    if storage_heat_code == SPARTACUS_OBSERVED_STORAGE_METHOD:
        if site_name is not None:
            msg = (
                f"{site_name}: observed storage heat fluxes (storage_heat 0) "
                "are not supported with SPARTACUS; use storage_heat 5 (EHC) "
                "or 7 (STEBBS)"
            )
        else:
            msg = (
                "Observed storage heat fluxes (StorageHeatMethod=0) are "
                "not supported with SPARTACUS."
            )
        violations.append(
            InvariantViolation(
                code="SPARTACUS.OBSERVED_STORAGE_HEAT",
                category="MODEL_OPTIONS",
                parameter="netradiationmethod-storageheatmethod",
                message=msg,
                suggested_value=(
                    "Set model.physics.storage_heat=7 (STEBBS, with "
                    "model.physics.stebbs.enabled=true) or 5 (EHC)."
                ),
            )
        )
        return violations

    effective_bldgs_present = (
        bldgs_sfr > BUILDING_PRESENT_SFR_TOL if bldgs_sfr is not None else bldgs_present
    )
    if not effective_bldgs_present:
        return violations

    if storage_heat_code not in SPARTACUS_BUILDING_STORAGE_METHODS:
        if site_name is not None:
            sfr_label = (
                f"land_cover.bldgs.sfr={bldgs_sfr}"
                if bldgs_sfr is not None
                else "land_cover.bldgs.sfr > 0"
            )
            msg = (
                f"{site_name}: SPARTACUS with buildings "
                f"({sfr_label}) requires a facet-resolved "
                "storage heat method: storage_heat 7 (STEBBS, with "
                "model.physics.stebbs.enabled=true) or 5 (EHC); got "
                f"storage_heat {storage_heat_code}"
            )
        else:
            msg = (
                f"StorageHeatMethod={storage_heat_code} is not supported "
                "with SPARTACUS when buildings are present "
                "(land_cover.bldgs.sfr > 0 at a site): SPARTACUS needs a "
                "storage heat method that resolves roof and wall facets."
            )
        violations.append(
            InvariantViolation(
                code="SPARTACUS.NON_FACET_STORAGE_HEAT",
                category="MODEL_OPTIONS",
                parameter="netradiationmethod-storageheatmethod-bldgs",
                message=msg,
                suggested_value=(
                    "Set model.physics.storage_heat=7 (STEBBS, with "
                    "model.physics.stebbs.enabled=true) or 5 (EHC)."
                ),
            )
        )
    elif (
        check_stebbs_for_method_7
        and storage_heat_code == 7
        and stebbs_method not in {1, 2}
    ):
        prefix = f"{site_name}: " if site_name is not None else ""
        violations.append(
            InvariantViolation(
                code="SPARTACUS.STEBBS_STORAGE_DISABLED",
                category="MODEL_OPTIONS",
                parameter="storageheatmethod-stebbsmethod",
                message=(
                    f"{prefix}SPARTACUS with buildings and storage_heat 7 "
                    "requires STEBBS to be enabled "
                    "(model.physics.stebbs.enabled=true)"
                ),
                suggested_value=(
                    "Set model.physics.stebbs.enabled=true with parameters "
                    "default or provided."
                ),
            )
        )

    return violations


def _coerce_finite_float(raw: Any) -> float | None:
    if _is_bool_like(raw):
        return None
    try:
        val = float(raw)
    except (TypeError, ValueError, OverflowError):
        return None
    return val if math.isfinite(val) else None


def check_same_surface_property(
    layer_values: Sequence[Any] | None,
    archetype_value: Any,
    *,
    site_name: str,
    layers_attr: str,
    layer_field: str,
    archetype_attr: str,
    param_name: str,
    property_name: str,
    archetype_present: bool = True,
    tol: float = SAME_SURFACE_TOL,
) -> list[InvariantViolation]:
    """Validate uniformity of a facet surface property across layers and archetype.

    Checks that:
    - ``vertical_layers.<layers_attr>`` is non-empty;
    - every layer provides a numeric ``<layer_field>`` and all layers agree
      within ``tol``;
    - ``properties.building_archetype.<archetype_attr>`` is present, numeric,
      and agrees with the common layer value within ``tol``.
    """
    violations: list[InvariantViolation] = []

    if not layer_values:
        violations.append(
            InvariantViolation(
                code=f"SAME_SURFACE.{param_name.upper()}.NO_LAYERS",
                category="MODEL_OPTIONS",
                parameter=f"vertical_layers.{layers_attr}",
                message=(
                    f"{site_name}: {param_name}=1, so vertical_layers.{layers_attr} "
                    "must be defined and non-empty"
                ),
            )
        )
        return violations

    vals: list[tuple[int, float]] = []
    for idx, raw in enumerate(layer_values):
        if raw is None:
            violations.append(
                InvariantViolation(
                    code=f"SAME_SURFACE.{param_name.upper()}.MISSING_LAYER_VALUE",
                    category="MODEL_OPTIONS",
                    parameter=f"vertical_layers.{layers_attr}[{idx}].{layer_field}",
                    message=(
                        f"{site_name}: {param_name}=1, so {layers_attr}[{idx}].{layer_field} "
                        "must be set"
                    ),
                )
            )
            continue
        parsed_val = _coerce_finite_float(raw)
        if parsed_val is None:
            violations.append(
                InvariantViolation(
                    code=f"SAME_SURFACE.{param_name.upper()}.NON_NUMERIC_LAYER",
                    category="MODEL_OPTIONS",
                    parameter=f"vertical_layers.{layers_attr}[{idx}].{layer_field}",
                    message=(
                        f"{site_name}: {param_name}=1 but {layers_attr}[{idx}].{layer_field} "
                        f"({raw!r}) is not a finite number"
                    ),
                )
            )
            continue
        vals.append((idx, parsed_val))

    if not vals:
        violations.append(
            InvariantViolation(
                code=f"SAME_SURFACE.{param_name.upper()}.NO_VALID_VALUES",
                category="MODEL_OPTIONS",
                parameter=f"vertical_layers.{layers_attr}",
                message=(
                    f"{site_name}: {param_name}=1 but no valid {layers_attr} "
                    f"{property_name} values found"
                ),
            )
        )
        return violations

    first_val = vals[0][1]
    mismatching = [(i, v) for i, v in vals if abs(v - first_val) > tol]
    if mismatching:
        all_str = ", ".join(f"{layers_attr}[{i}]={v}" for i, v in vals)
        violations.append(
            InvariantViolation(
                code=f"SAME_SURFACE.{param_name.upper()}.LAYER_MISMATCH",
                category="MODEL_OPTIONS",
                parameter=f"vertical_layers.{layers_attr}",
                message=(
                    f"{site_name}: {param_name}=1, so all {layers_attr} {property_name} "
                    f"values must be identical; found values: {all_str}"
                ),
            )
        )

    if not archetype_present:
        violations.append(
            InvariantViolation(
                code=f"SAME_SURFACE.{param_name.upper()}.MISSING_ARCHETYPE",
                category="MODEL_OPTIONS",
                parameter="building_archetype",
                message=(
                    f"{site_name}: {param_name}=1, so properties.building_archetype "
                    "must be defined"
                ),
            )
        )
        return violations

    if archetype_value is None:
        violations.append(
            InvariantViolation(
                code=f"SAME_SURFACE.{param_name.upper()}.MISSING_ARCHETYPE_FIELD",
                category="MODEL_OPTIONS",
                parameter=f"building_archetype.{archetype_attr}",
                message=(
                    f"{site_name}: {param_name}=1, so properties.building_archetype."
                    f"{archetype_attr} must be set"
                ),
            )
        )
        return violations

    arch_val = _coerce_finite_float(archetype_value)
    if arch_val is None:
        violations.append(
            InvariantViolation(
                code=f"SAME_SURFACE.{param_name.upper()}.NON_NUMERIC_ARCHETYPE",
                category="MODEL_OPTIONS",
                parameter=f"building_archetype.{archetype_attr}",
                message=(
                    f"{site_name}: {param_name}=1 but properties.building_archetype."
                    f"{archetype_attr} ({archetype_value!r}) is not a finite number"
                ),
            )
        )
        return violations

    if abs(first_val - arch_val) > tol:
        all_str = ", ".join(f"{layers_attr}[{i}]={v}" for i, v in vals)
        violations.append(
            InvariantViolation(
                code=f"SAME_SURFACE.{param_name.upper()}.ARCHETYPE_MISMATCH",
                category="MODEL_OPTIONS",
                parameter=f"building_archetype.{archetype_attr}",
                message=(
                    f"{site_name}: {param_name}=1, so common {layers_attr} {property_name} "
                    f"(found {all_str}) must equal properties.building_archetype."
                    f"{archetype_attr} ({arch_val})"
                ),
            )
        )

    return violations


def check_stebbs_internal_shading(
    shading_mode: Any,
    values_by_field: Mapping[str, Any],
    *,
    present_fields: set[str] | frozenset[str] | None = None,
    require_explicit_presence: bool = False,
    site_index: int | None = None,
    site_gridid: Any = None,
) -> list[InvariantViolation]:
    """Validate STEBBS internal window-shading mode, required inputs, and bounds.

    Shared by ``StebbsProperties._validate_internal_shading_parameters`` (with
    ``require_explicit_presence=True`` and ``present_fields=self.model_fields_set``)
    and Phase B's ``check_internal_shading`` rule.
    """
    violations: list[InvariantViolation] = []

    is_valid_mode = shading_mode is None or (
        isinstance(shading_mode, (int, float))
        and not isinstance(shading_mode, bool)
        and _is_finite_real_number(shading_mode)
        and shading_mode in (0, 1, 2)
    )
    if not is_valid_mode:
        violations.append(
            InvariantViolation(
                code="STEBBS.INTERNAL_SHADING.INVALID_MODE",
                category="MODEL_OPTIONS",
                parameter="stebbs.internal_shading",
                site_index=site_index,
                site_gridid=site_gridid,
                message=(
                    "internal_shading must be 0 (disabled), 1 (always active), "
                    f"or 2 (controlled), got '{shading_mode}'."
                ),
                suggested_value="Set internal_shading to 0, 1, or 2",
            )
        )
        return violations

    required_fields = stebbs_shading_requirements(shading_mode)
    if not required_fields:
        return violations

    for field_name, base_message in required_fields.items():
        val = values_by_field.get(field_name)
        is_missing = val is None or (
            require_explicit_presence
            and (present_fields is None or field_name not in present_fields)
        )
        if not is_missing:
            continue
        violations.append(
            InvariantViolation(
                code=f"STEBBS.{field_name.upper()}.MISSING",
                category="MODEL_OPTIONS",
                parameter=f"stebbs.{field_name}",
                site_index=site_index,
                site_gridid=site_gridid,
                message=base_message if require_explicit_presence else f"{base_message}.",
                suggested_value=_SHADING_MISSING_SUGGESTIONS[field_name],
            )
        )

    reduction_factor = values_by_field.get("reduction_factor_shading")
    if reduction_factor is not None and (
        not _is_finite_real_number(reduction_factor)
        or not 0.0 <= reduction_factor <= 1.0
    ):
        violations.append(
            InvariantViolation(
                code="STEBBS.REDUCTION_FACTOR_SHADING.OUT_OF_RANGE",
                category="MODEL_OPTIONS",
                parameter="stebbs.reduction_factor_shading",
                site_index=site_index,
                site_gridid=site_gridid,
                message=(
                    "reduction_factor_shading must be a finite value between "
                    f"0 and 1, got '{reduction_factor}'."
                ),
                suggested_value="Set reduction_factor_shading between 0 and 1",
            )
        )

    for field_name, lower_bound, inclusive, unit in _SHADING_THRESHOLD_SPECS:
        if field_name not in required_fields:
            continue
        threshold = values_by_field.get(field_name)
        if threshold is None:
            continue
        valid_number = _is_finite_real_number(threshold)
        above_lower = (
            (threshold >= lower_bound if inclusive else threshold > lower_bound)
            if valid_number
            else False
        )
        if not above_lower:
            comparison = "at least" if inclusive else "greater than"
            violations.append(
                InvariantViolation(
                    code=f"STEBBS.{field_name.upper()}.OUT_OF_RANGE",
                    category="MODEL_OPTIONS",
                    parameter=f"stebbs.{field_name}",
                    site_index=site_index,
                    site_gridid=site_gridid,
                    message=(
                        f"{field_name} must be finite and {comparison} "
                        f"{lower_bound} {unit}, got '{threshold}'."
                    ),
                    suggested_value=f"Provide a physically valid threshold in {unit}",
                )
            )

    return violations


def _unwrap_scalar(value: Any) -> Any:
    """Unwrap RefValue objects, RefValue mappings, Enum members, and NumPy scalars."""
    from ..pipeline.document import unwrap_entry  # noqa: PLC0415

    unwrapped = unwrap_entry(value)
    if hasattr(unwrapped, "value") and not isinstance(
        unwrapped, (Mapping, list, tuple, str, bytes)
    ):
        unwrapped = unwrapped.value
    if hasattr(unwrapped, "value") and not isinstance(
        unwrapped, (Mapping, list, tuple, str, bytes)
    ):
        unwrapped = unwrapped.value
    if getattr(type(unwrapped), "__module__", "").split(".")[0] == "numpy" and callable(
        getattr(unwrapped, "item", None)
    ):
        try:
            unwrapped = unwrapped.item()
        except (TypeError, ValueError):
            pass
    return unwrapped


def resolve_water_use_code(water_use: Any) -> int | None:
    """Resolve ``model.physics.water_use`` to its integer code, or ``None``.

    Returns ``0`` for ``WaterUseMethod.MODELLED`` / ``"modelled"`` / ``0``,
    ``1`` for ``WaterUseMethod.OBSERVED`` / ``"observed"`` / ``1``, and
    ``None`` when absent or unresolvable (so invalid selectors fail on their
    own physics validator instead of being coerced).
    """
    raw = _unwrap_scalar(water_use)
    if raw is None or isinstance(raw, bool):
        return None
    if isinstance(raw, str):
        cleaned = raw.strip().lower()
        if cleaned in _WATER_USE_STRING_CODES:
            return _WATER_USE_STRING_CODES[cleaned]
        try:
            return int(cleaned)
        except ValueError:
            return None
    if isinstance(raw, int):
        return raw
    if isinstance(raw, float):
        if math.isfinite(raw) and raw.is_integer():
            return int(raw)
        return None
    return None


def _irrigation_has_active_daywat(irrigation: Any) -> bool:
    """Return True iff at least one weekday flag in ``irrigation.daywat`` equals ``1.0``."""
    if irrigation is None:
        return False
    if isinstance(irrigation, Mapping):
        daywat = _unwrap_scalar(irrigation.get("daywat"))
    else:
        daywat = _unwrap_scalar(getattr(irrigation, "daywat", None))
    if daywat is None:
        return False

    for day in _IRRIGATION_WEEKDAYS:
        if isinstance(daywat, Mapping):
            raw_flag = daywat.get(day)
        else:
            raw_flag = getattr(daywat, day, None)
        flag = _unwrap_scalar(raw_flag)
        if isinstance(flag, bool):
            if flag:
                return True
            continue
        if _is_finite_real_number(flag) and float(flag) == 1.0:
            return True
        if isinstance(flag, str):
            try:
                parsed = float(flag.strip())
            except ValueError:
                continue
            if math.isfinite(parsed) and parsed == 1.0:
                return True
    return False


def is_modelled_irrigation_active(water_use: Any, irrigation: Any) -> bool:
    """Return True iff modelled irrigation can execute for *irrigation*.

    Resolves *water_use* first (invalid/unresolvable selectors return ``False``
    so their own physics validator reports them cleanly) and returns ``True``
    iff ``int(water_use) == 0`` (``WaterUseMethod.MODELLED``) AND at least one
    weekday flag in ``irrigation.daywat`` equals ``1.0``, matching Rust
    ``yaml_config.rs::apply_irrigation_overrides`` and Fortran
    ``suews_phys_dailystate.f95::update_WaterUse``.
    """
    code = resolve_water_use_code(water_use)
    if code != 0:
        return False
    return _irrigation_has_active_daywat(irrigation)


def check_irrigation_doy_pair(
    ie_start: Any,
    ie_end: Any,
    *,
    model_year: int | None = None,
    site_name: str | None = None,
    coerce_doy: Callable[[Any, str], int] | None = None,
) -> tuple[list[InvariantViolation], int | None, int | None]:
    """Validate paired irrigation day-of-year inputs (``ie_start``, ``ie_end``).

    Returns ``(violations, ie_start_int, ie_end_int)``. When ``model_year`` is
    provided, enforces leap-year bounds (``1..366`` vs ``1..365``); when
    ``model_year`` is ``None`` (model construction without a year), enforces
    ``1..366`` and paired presence.
    """
    coerce = coerce_doy or _default_coerce_doy
    is_leap = calendar.isleap(model_year) if model_year is not None else True
    max_doy = 366 if is_leap else 365
    site_suffix = f" in site {site_name}" if site_name is not None else ""

    violations: list[InvariantViolation] = []
    parsed: dict[str, int | None] = {}
    has_type_error = False

    for param_name, param_value in (("ie_start", ie_start), ("ie_end", ie_end)):
        if param_value is None:
            parsed[param_name] = None
            continue
        try:
            parsed[param_name] = coerce(param_value, param_name)
        except ValueError as exc:
            has_type_error = True
            violations.append(
                InvariantViolation(
                    code=f"IRRIGATION.{param_name.upper()}.INVALID_TYPE",
                    category="IRRIGATION",
                    parameter=f"{param_name}{site_suffix}",
                    message=str(exc),
                    suggested_value=(
                        f"Replace decimal or non-integer values with an integer "
                        f"day of year (1-{max_doy}), or null/None to disable"
                    ),
                )
            )

    if has_type_error:
        return violations, None, None

    ie_start_int = parsed["ie_start"]
    ie_end_int = parsed["ie_end"]

    has_range_error = False
    for param_name, param_value in (
        ("ie_start", ie_start_int),
        ("ie_end", ie_end_int),
    ):
        if param_value is not None and not (1 <= param_value <= max_doy):
            has_range_error = True
            if model_year is not None:
                year_detail = (
                    f" for {'leap' if is_leap else 'non-leap'} year {model_year}"
                )
            else:
                year_detail = ""
            violations.append(
                InvariantViolation(
                    code=f"IRRIGATION.{param_name.upper()}.OUT_OF_RANGE",
                    category="IRRIGATION",
                    parameter=f"{param_name}{site_suffix}",
                    message=(
                        f"{param_name}={param_value} is invalid. "
                        f"Must be an integer day of year between 1 and {max_doy}"
                        f"{year_detail}, or null/None to disable irrigation"
                    ),
                    suggested_value=f"Valid range: 1-{max_doy}, or null/None to disable",
                )
            )

    if has_range_error:
        return violations, ie_start_int, ie_end_int

    if (ie_start_int is None) != (ie_end_int is None):
        prefix = f"{site_name}: " if site_name is not None and model_year is None else ""
        violations.append(
            InvariantViolation(
                code="IRRIGATION.WINDOW_UNPAIRED",
                category="IRRIGATION",
                parameter=f"irrigation{site_suffix}",
                message=(
                    f"{prefix}Both ie_start and ie_end must be specified together "
                    "(both set to valid integer DOY), "
                    "or both left as null/None (irrigation disabled). "
                    f"Currently: ie_start={ie_start}, ie_end={ie_end}"
                ),
                suggested_value=(
                    "Set both to valid integer DOY (1-365/366) or both to null/None"
                ),
            )
        )

    return violations, ie_start_int, ie_end_int


def check_dls_doy_pair(
    startdls: Any,
    enddls: Any,
    *,
    model_year: int | None = None,
    site_name: str | None = None,
    site_index: int | None = None,
    site_gridid: Any = None,
    coerce_doy: Callable[[Any, str], int] | None = None,
) -> tuple[list[InvariantViolation], bool, int | None, int | None]:
    """Validate DLS day-of-year scalar types and bounds, and report half-pairing.

    Returns ``(violations, half_configured, startdls_int, enddls_int)``.
    ``SUEWSConfig.validate_dls_parameters`` treats ``half_configured`` as a
    hard error, and Phase B's geographic rule also emits a blocking ``ERROR``
    under ``off``/``suggest`` or when DST cannot be computed for the site,
    emitting ``INFO`` only under ``--science-fixes apply`` when seasonal
    adjustment can fill the missing bound.
    """
    coerce = coerce_doy or _default_coerce_doy
    is_leap = calendar.isleap(model_year) if model_year is not None else True
    max_doy = 366 if is_leap else 365

    violations: list[InvariantViolation] = []
    parsed: dict[str, int | None] = {}

    for dls_name, dls_val in (("startdls", startdls), ("enddls", enddls)):
        if dls_val is None:
            parsed[dls_name] = None
            continue
        try:
            dls_int = coerce(dls_val, dls_name)
        except ValueError as exc:
            parsed[dls_name] = None
            violations.append(
                InvariantViolation(
                    code=f"GEOGRAPHY.DLS.{dls_name.upper()}.INVALID_TYPE",
                    category="GEOGRAPHY",
                    parameter=f"anthropogenic_emissions.{dls_name}",
                    site_index=site_index,
                    site_gridid=site_gridid,
                    message=str(exc),
                    suggested_value=(
                        "Replace decimal or non-integer values with an "
                        f"integer day of year between 1 and {max_doy}"
                    ),
                )
            )
            continue

        parsed[dls_name] = dls_int
        if not (1 <= dls_int <= max_doy):
            if site_name is not None and model_year is not None:
                msg = (
                    f"{site_name}: {dls_name} DOY {int(dls_int)} invalid for "
                    f"{'leap' if is_leap else 'non-leap'} year {model_year}. "
                    f"Must be in range [1, {max_doy}]"
                )
            else:
                msg = (
                    f"{dls_name}={dls_int} is invalid. Must be an "
                    f"integer day of year between 1 and {max_doy}."
                )
            violations.append(
                InvariantViolation(
                    code=f"GEOGRAPHY.DLS.{dls_name.upper()}.OUT_OF_RANGE",
                    category="GEOGRAPHY",
                    parameter=f"anthropogenic_emissions.{dls_name}",
                    site_index=site_index,
                    site_gridid=site_gridid,
                    message=msg,
                    suggested_value=(
                        f"Set an integer day of year between 1 and {max_doy}, "
                        "or omit both startdls and enddls to disable "
                        "daylight saving"
                    ),
                )
            )

    half_configured = (startdls is None) != (enddls is None)
    return violations, half_configured, parsed["startdls"], parsed["enddls"]


@dataclass(frozen=True)
class LandCoverSumResult:
    """Outcome of checking land-cover surface fractions against tolerance bands."""

    band: str
    severity: str
    total: float
    difference: float
    max_surface: str | None = None
    correction: float | None = None
    violation: InvariantViolation | None = None


def coerce_land_cover_fraction(raw: Any) -> float | None:
    """Coerce a raw or unwrapped land-cover surface fraction to a finite float.

    Shared by Phase B rules, scientific patch planners, and the land-cover sum
    kernel so numeric strings (such as ``"0.6"``) are admitted identically
    everywhere while booleans, ``None``, and non-finite or non-numeric values
    return ``None``.
    """
    return _coerce_finite_float(raw)


def check_land_cover_fraction_sum(
    fractions: Mapping[str, Any],
) -> LandCoverSumResult:
    """Validate land-cover fraction sum using the shared three-band policy.

    Bands evaluated on finite per-surface fractions in ``[0.0, 1.0]``:

    - ``abs(sum - 1.0) <= LAND_COVER_SUM_PASS_TOL`` (``1e-6``): ``pass``.
    - ``1e-6 < abs(sum - 1.0) <= LAND_COVER_SUM_AUTO_FIX_TOL`` (``1e-4``):
      ``auto_fix`` (``WARNING``, non-blocking; auto-normalisable under
      ``--science-fixes apply``).
    - ``abs(sum - 1.0) > LAND_COVER_SUM_AUTO_FIX_TOL`` or zero/invalid input:
      ``error`` (``ERROR``, blocking).
    """
    if not isinstance(fractions, Mapping) or not fractions:
        return LandCoverSumResult(
            band="error",
            severity="ERROR",
            total=0.0,
            difference=1.0,
            violation=InvariantViolation(
                code="LAND_COVER.SURFACE_FRACTIONS.ALL_ZERO",
                category="LAND_COVER",
                parameter="land_cover.surface_fractions",
                message="All surface fractions are zero or missing",
                suggested_value=(
                    "Set surface fractions (paved.sfr, bldgs.sfr, evetr.sfr, "
                    "dectr.sfr, grass.sfr, bsoil.sfr, water.sfr) that sum to 1.0"
                ),
            ),
        )

    clean: dict[str, float] = {}
    for surf_name, raw_val in fractions.items():
        val = coerce_land_cover_fraction(raw_val)
        if val is None:
            return LandCoverSumResult(
                band="error",
                severity="ERROR",
                total=0.0,
                difference=1.0,
                violation=InvariantViolation(
                    code="LAND_COVER.SFR.NON_FINITE",
                    category="LAND_COVER",
                    parameter=f"{surf_name}.sfr",
                    message=(
                        f"Surface fraction '{surf_name}.sfr' ({raw_val!r}) "
                        "is not a finite number"
                    ),
                    suggested_value=(
                        "Set surface fraction to a finite number between 0.0 and 1.0"
                    ),
                ),
            )
        if not 0.0 <= val <= 1.0:
            return LandCoverSumResult(
                band="error",
                severity="ERROR",
                total=0.0,
                difference=1.0,
                violation=InvariantViolation(
                    code="LAND_COVER.SFR.OUT_OF_RANGE",
                    category="LAND_COVER",
                    parameter=f"{surf_name}.sfr",
                    message=(
                        f"Surface fraction '{surf_name}.sfr' ({val:g}) "
                        "must be between 0.0 and 1.0"
                    ),
                    suggested_value="Set surface fraction between 0.0 and 1.0",
                ),
            )
        clean[surf_name] = val

    canonical_order = {
        name: idx
        for idx, name in enumerate(
            ("paved", "bldgs", "evetr", "dectr", "grass", "bsoil", "water")
        )
    }
    ordered_clean = sorted(
        clean.items(),
        key=lambda item: (canonical_order.get(item[0], len(canonical_order)), item[0]),
    )
    total = sum(val for _, val in ordered_clean)
    difference = abs(total - 1.0)
    max_surface = max(ordered_clean, key=lambda item: item[1])[0]

    if total == 0.0:
        return LandCoverSumResult(
            band="error",
            severity="ERROR",
            total=0.0,
            difference=1.0,
            max_surface=max_surface,
            violation=InvariantViolation(
                code="LAND_COVER.SURFACE_FRACTIONS.ALL_ZERO",
                category="LAND_COVER",
                parameter="land_cover.surface_fractions",
                message="All surface fractions are zero or missing",
                suggested_value=(
                    "Set surface fractions (paved.sfr, bldgs.sfr, evetr.sfr, "
                    "dectr.sfr, grass.sfr, bsoil.sfr, water.sfr) that sum to 1.0"
                ),
            ),
        )

    if difference <= LAND_COVER_SUM_PASS_TOL:
        return LandCoverSumResult(
            band="pass",
            severity="PASS",
            total=total,
            difference=difference,
            max_surface=max_surface,
        )

    surface_list = ", ".join(f"{surf}={val:.4f}" for surf, val in ordered_clean)
    if difference <= LAND_COVER_SUM_AUTO_FIX_TOL:
        return LandCoverSumResult(
            band="auto_fix",
            severity="WARNING",
            total=total,
            difference=difference,
            max_surface=max_surface,
            correction=1.0 - total,
            violation=InvariantViolation(
                code="LAND_COVER.SURFACE_FRACTIONS.SMALL_DRIFT",
                category="LAND_COVER",
                parameter=f"{max_surface}.sfr",
                message=(
                    f"Surface fractions sum to {total:.6f} (difference "
                    f"{difference:.2e} within auto-normalisation tolerance "
                    f"{LAND_COVER_SUM_AUTO_FIX_TOL:.1e}: {surface_list}). "
                    f"Re-run with --science-fixes apply to auto-normalise "
                    f"{max_surface}.sfr."
                ),
                suggested_value=(
                    f"Normalise {max_surface}.sfr (or adjust surface fractions) "
                    "so they sum to 1.0"
                ),
            ),
        )

    return LandCoverSumResult(
        band="error",
        severity="ERROR",
        total=total,
        difference=difference,
        max_surface=max_surface,
        violation=InvariantViolation(
            code="LAND_COVER.SURFACE_FRACTIONS.SUM_ERROR",
            category="LAND_COVER",
            parameter=f"{max_surface}.sfr",
            message=(
                f"Surface fractions sum to {total:.4f}, should equal 1.0 "
                f"(auto-normalisation range: 1.0 +/- "
                f"{LAND_COVER_SUM_AUTO_FIX_TOL:.1e}, current: {surface_list})"
            ),
            suggested_value=(
                f"Adjust {max_surface}.sfr or other surface fractions so they "
                f"sum to 1.0 (within {LAND_COVER_SUM_AUTO_FIX_TOL:.1e})"
            ),
        ),
    )
