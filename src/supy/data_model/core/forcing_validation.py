"""Cross-checks between physics options and loaded forcing columns (gh#1372).

The helper is intentionally a plain function rather than a Pydantic
``model_validator`` because Pydantic validators run at config-construction
time, before the forcing file has been loaded. Call this helper from the
simulation orchestration layer once both the resolved physics options and
the forcing column set are available.
"""

from __future__ import annotations

from enum import Enum
from typing import Any

from ..forcing import FORCING_REGISTRY

_PHYSICS_REQUIRED_FORCING = FORCING_REGISTRY.current_requirements

LAI_VEG_COLUMNS = FORCING_REGISTRY.per_landcover_columns["lai"]
# Per-land-cover external water-use forcing columns, named with the same
# `wuh_<suffix>` convention users write in their forcing files (see
# supy._load.WUH_LANDCOVER_SUFFIXES). Each falls back to the bulk `wuh`
# column when its per-surface column is absent, mirroring LAI_VEG_COLUMNS.
WUH_SURF_COLUMNS = FORCING_REGISTRY.per_landcover_columns["wuh"]


def _resolve(value: Any) -> Any:
    """Unwrap RefValue-style ``{value: ...}`` mappings and ``.value`` attributes."""
    if isinstance(value, dict) and "value" in value:
        return _resolve(value["value"])
    if isinstance(value, (list, tuple, set, frozenset)):
        return type(value)(_resolve(item) for item in value)
    if isinstance(value, Enum):
        return _resolve(value.value)
    if hasattr(value, "value"):
        inner = value.value
        if inner is value:
            return value
        return _resolve(inner)
    return value


def _matches_option_value(actual_value: Any, option_value: int) -> bool:
    """Return True when a scalar or per-grid iterable selects ``option_value``."""
    if isinstance(actual_value, (list, tuple, set, frozenset)):
        return option_value in actual_value
    return actual_value == option_value


def _forcing_columns(forcing: Any) -> set[str]:
    if hasattr(forcing, "columns"):
        return {str(col).lower() for col in forcing.columns}
    return {str(col).lower() for col in forcing}


def _columns_by_lower(forcing: Any) -> dict[str, Any]:
    if hasattr(forcing, "columns"):
        return {str(col).lower(): col for col in forcing.columns}
    return {str(col).lower(): col for col in forcing}


def _series_has_valid_data(series: Any) -> bool:
    """Return True only when every row is non-missing/non-sentinel."""
    import pandas as pd

    from ...util._missing import SUEWS_MISSING_THRESHOLD

    numeric = pd.to_numeric(series, errors="coerce")
    valid_mask = (numeric.notna()) & (numeric > SUEWS_MISSING_THRESHOLD)
    return bool(valid_mask.to_numpy().all())


def _column_has_valid_data(forcing: Any, col: str) -> bool:
    """Return True if ``col`` exists and has no missing/sentinel rows."""
    if not hasattr(forcing, "columns"):
        return True

    columns_by_lower = _columns_by_lower(forcing)
    source_col = columns_by_lower.get(col)
    if source_col is None:
        return False

    return _series_has_valid_data(forcing[source_col])


def _lai_validity_issue(forcing: Any) -> str | None:
    """Return the validation issue reason for laimethod=0, or None."""
    columns_by_lower = _columns_by_lower(forcing)
    bulk_lai_col = columns_by_lower.get("lai")
    source_cols: list[Any] = []
    for lai_col in LAI_VEG_COLUMNS:
        source_col = columns_by_lower.get(lai_col, bulk_lai_col)
        if source_col is None:
            return "missing"
        source_cols.append(source_col)

    if not hasattr(forcing, "columns"):
        return None

    for source_col in dict.fromkeys(source_cols):
        if not _series_has_valid_data(forcing[source_col]):
            return "all_missing"
    return None

def _wuh_validity_issue(forcing: Any) -> str | None:
    """Return the validation issue reason for water_use=0, or None."""
    columns_by_lower = _columns_by_lower(forcing)
    bulk_wuh_col = columns_by_lower.get("wuh")
    source_cols: list[Any] = []
    for wuh_col in WUH_SURF_COLUMNS:
        source_col = columns_by_lower.get(wuh_col, bulk_wuh_col)
        if source_col is None:
            return "missing"
        source_cols.append(source_col)

    if not hasattr(forcing, "columns"):
        return None

    for source_col in dict.fromkeys(source_cols):
        if not _series_has_valid_data(forcing[source_col]):
            return "all_missing"
    return None


def validate_forcing_columns_against_physics(
    forcing_columns: Any,
    physics: Any,
) -> None:
    """Raise ``ValueError`` if a chosen physics path needs forcing data
    that the loaded forcing does not provide.

    Parameters
    ----------
    forcing_columns : Any
        Either the loaded forcing DataFrame or an iterable of column
        names. DataFrames allow both presence and all-missing sentinel
        checks; iterables only allow case-insensitive presence checks.
    physics : Any
        Object exposing ``.net_radiation`` (and other physics fields as
        the mapping grows). Each attribute may be a bare int, a
        RefValue-style mapping, or any object exposing ``.value``.

    Raises
    ------
    ValueError
        Lists every missing (column, physics field, value) triple found.
    """
    available = _forcing_columns(forcing_columns)
    missing: list[tuple[str, int, str, str]] = []
    for (field_name, value), required_cols in _PHYSICS_REQUIRED_FORCING.items():
        attr = getattr(physics, field_name, None)
        if attr is None:
            continue
        actual_value = _resolve(attr)
        if not _matches_option_value(actual_value, value):
            continue
        for col in required_cols:
            if col.lower() == "lai":
                reason = _lai_validity_issue(forcing_columns)
                if reason is not None:
                    missing.append((field_name, value, col, reason))
            elif col.lower() == "wuh":
                reason = _wuh_validity_issue(forcing_columns)
                if reason is not None:
                    missing.append((field_name, value, col, reason))
            elif col.lower() not in available:
                missing.append((field_name, value, col, "missing"))
            elif not _column_has_valid_data(forcing_columns, col.lower()):
                missing.append((field_name, value, col, "all_missing"))
    if missing:
        details = "; ".join(
            (
                f"forcing column '{col}' is required when {field}={value}"
                if reason == "missing"
                else f"forcing column '{col}' must contain valid data when {field}={value}"
            )
            for field, value, col, reason in missing
        )
        raise ValueError(f"physics/forcing mismatch: {details}")
