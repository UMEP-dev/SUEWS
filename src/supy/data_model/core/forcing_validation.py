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


# (physics_field, value) -> required forcing column names.
# Keep this aligned with supy._check.FORCING_REQUIREMENTS, but use the
# current Pydantic field names emitted by ModelPhysics.model_dump().
_PHYSICS_REQUIRED_FORCING: dict[tuple[str, int], frozenset[str]] = {
    ("net_radiation", 0): frozenset({"qn"}),
    ("net_radiation", 1): frozenset({"ldown"}),
    ("net_radiation", 2): frozenset({"kdown", "fcld"}),
    ("net_radiation", 3): frozenset({"kdown"}),
    ("net_radiation", 11): frozenset({"ldown"}),
    ("net_radiation", 12): frozenset({"kdown", "fcld"}),
    ("net_radiation", 13): frozenset({"kdown"}),
    ("net_radiation", 100): frozenset({"ldown"}),
    ("net_radiation", 200): frozenset({"kdown", "fcld"}),
    ("net_radiation", 300): frozenset({"kdown"}),
    ("net_radiation", 1001): frozenset({"ldown"}),
    ("net_radiation", 1002): frozenset({"kdown", "fcld"}),
    ("net_radiation", 1003): frozenset({"kdown"}),
    ("storage_heat", 0): frozenset({"qs"}),
    ("emissions", 0): frozenset({"qf"}),
    ("soil_moisture_deficit", 1): frozenset({"xsmd"}),
    ("soil_moisture_deficit", 2): frozenset({"xsmd"}),
    ("laimethod", 0): frozenset({"lai"}),
}


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


def _column_has_valid_data(forcing: Any, col: str) -> bool:
    """Return True if ``col`` exists and is not entirely missing/sentinel."""
    if not hasattr(forcing, "columns"):
        return True

    match = next((name for name in forcing.columns if str(name).lower() == col), None)
    if match is None:
        return False

    import pandas as pd

    from ...util._missing import SUEWS_MISSING_THRESHOLD

    series = pd.to_numeric(forcing[match], errors="coerce")
    return bool(((series.notna()) & (series > SUEWS_MISSING_THRESHOLD)).any())


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
            if col.lower() not in available:
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
