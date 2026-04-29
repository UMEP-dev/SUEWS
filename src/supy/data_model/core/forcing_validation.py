"""Cross-checks between physics options and loaded forcing columns (gh#1372).

The helper is intentionally a plain function rather than a Pydantic
``model_validator`` because Pydantic validators run at config-construction
time, before the forcing file has been loaded. Call this helper from the
simulation orchestration layer once both the resolved physics options and
the forcing column set are available.
"""

from __future__ import annotations

from typing import Any


# (physics_field, value) -> required forcing column names.
# Extend cautiously; keep the rationale per entry.
_PHYSICS_REQUIRED_FORCING: dict[tuple[str, int], frozenset[str]] = {
    # net_radiation method 11 (observed downward longwave): ldown required.
    ("net_radiation", 11): frozenset({"ldown"}),
    # net_radiation methods 1 / 2 (cloud-fraction parameterisation): fcld required.
    ("net_radiation", 1): frozenset({"fcld"}),
    ("net_radiation", 2): frozenset({"fcld"}),
}


def _resolve(value: Any) -> Any:
    """Unwrap RefValue-style ``{value: ...}`` mappings and ``.value`` attributes."""
    if hasattr(value, "value"):
        return value.value
    return value


def validate_forcing_columns_against_physics(
    forcing_columns: set[str],
    physics: Any,
) -> None:
    """Raise ``ValueError`` if a chosen physics path needs a forcing column
    that ``forcing_columns`` does not provide.

    Parameters
    ----------
    forcing_columns : set[str]
        Column names present in the loaded forcing DataFrame (canonical
        plus extras). Comparison is case-insensitive.
    physics : Any
        Object exposing ``.net_radiation`` (and other physics fields as
        the mapping grows). Each attribute may be a bare int, a
        RefValue-style mapping, or any object exposing ``.value``.

    Raises
    ------
    ValueError
        Lists every missing (column, physics field, value) triple found.
    """
    available = {col.lower() for col in forcing_columns}
    missing: list[tuple[str, int, str]] = []
    for (field_name, value), required_cols in _PHYSICS_REQUIRED_FORCING.items():
        attr = getattr(physics, field_name, None)
        if attr is None:
            continue
        actual_value = _resolve(attr)
        if actual_value != value:
            continue
        for col in required_cols:
            if col.lower() not in available:
                missing.append((field_name, value, col))
    if missing:
        details = "; ".join(
            f"forcing column '{col}' is required when {field}={value}"
            for field, value, col in missing
        )
        raise ValueError(f"physics/forcing mismatch: {details}")
