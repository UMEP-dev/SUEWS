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

from ..required_fields import (
    BUILDING_PRESENT_SFR_TOL,
    SPARTACUS_BUILDING_STORAGE_METHODS,
    SPARTACUS_OBSERVED_STORAGE_METHOD,
    stebbs_shading_requirements,
)

SPARTACUS_METHODS: frozenset[int] = frozenset({1001, 1002, 1003})
SAME_SURFACE_TOL: float = 1e-9

_SHADING_MISSING_SUGGESTIONS: dict[str, str] = {
    "reduction_factor_shading": "Provide a value between 0 and 1",
    "temperature_threshold_shading": "Provide temperature_threshold_shading in degC",
    "radiation_threshold_shading": "Provide radiation_threshold_shading in W m-2",
}

_SHADING_THRESHOLD_SPECS: tuple[tuple[str, float, bool, str], ...] = (
    ("temperature_threshold_shading", -273.15, False, "degC"),
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


def _is_finite_real_number(value: Any) -> bool:
    return (
        isinstance(value, (int, float))
        and not isinstance(value, bool)
        and math.isfinite(value)
    )


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

    vals: list[float] = []
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
        try:
            vals.append(float(raw))
        except (TypeError, ValueError):
            violations.append(
                InvariantViolation(
                    code=f"SAME_SURFACE.{param_name.upper()}.NON_NUMERIC_LAYER",
                    category="MODEL_OPTIONS",
                    parameter=f"vertical_layers.{layers_attr}[{idx}].{layer_field}",
                    message=(
                        f"{site_name}: {param_name}=1 but {layers_attr}[{idx}].{layer_field} "
                        f"({raw!r}) is not numeric"
                    ),
                )
            )

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

    first_val = vals[0]
    mismatching = [(i, v) for i, v in enumerate(vals) if abs(v - first_val) > tol]
    if mismatching:
        all_str = ", ".join(f"{layers_attr}[{i}]={v}" for i, v in enumerate(vals))
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

    try:
        arch_val = float(archetype_value)
    except (TypeError, ValueError):
        violations.append(
            InvariantViolation(
                code=f"SAME_SURFACE.{param_name.upper()}.NON_NUMERIC_ARCHETYPE",
                category="MODEL_OPTIONS",
                parameter=f"building_archetype.{archetype_attr}",
                message=(
                    f"{site_name}: {param_name}=1 but properties.building_archetype."
                    f"{archetype_attr} ({archetype_value!r}) is not numeric"
                ),
            )
        )
        return violations

    if abs(first_val - arch_val) > tol:
        all_str = ", ".join(f"{layers_attr}[{i}]={v}" for i, v in enumerate(vals))
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

    if isinstance(shading_mode, bool) or shading_mode not in {0, 1, 2, None}:
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
        if require_explicit_presence:
            is_missing = present_fields is None or field_name not in present_fields
        else:
            is_missing = values_by_field.get(field_name) is None
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
    hard error, whereas Phase B's geographic rule emits a warning because
    ``--science-fixes apply`` can still compute DLS from site coordinates.
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
