"""Orthogonal physics option input forms.

This module widens accepted YAML input while keeping the canonical internal
representation flat. At present only ``model.physics.net_radiation`` has a
defined orthogonal decomposition.
"""

from __future__ import annotations

from collections.abc import Mapping
from typing import Any

_MISSING = object()

_NARP_CODES: dict[tuple[str, str], int] = {
    ("observed", "standard"): 1,
    ("cloud", "standard"): 2,
    ("air", "standard"): 3,
    ("observed", "surface"): 11,
    ("cloud", "surface"): 12,
    ("air", "surface"): 13,
    ("observed", "zenith"): 100,
    ("cloud", "zenith"): 200,
    ("air", "zenith"): 300,
}

_SPARTACUS_CODES: dict[str, int] = {
    "observed": 1001,
    "cloud": 1002,
    "air": 1003,
}


def _token(mapping: Mapping[str, Any], key: str, context: str) -> str | None:
    raw = mapping.get(key, _MISSING)
    if raw is _MISSING:
        return None
    if not isinstance(raw, str) or not raw.strip():
        raise ValueError(f"'{context}.{key}' must be a non-empty string token.")
    return raw.strip().lower()


def _reject_foreign_keys(
    mapping: Mapping[str, Any], allowed: set[str], context: str
) -> None:
    foreign = [repr(key) for key in mapping if key not in allowed]
    if foreign:
        raise ValueError(
            f"'{context}' cannot be combined with sibling keys {foreign}."
        )


def coerce_orthogonal_to_flat(field_name: str, value: Any) -> Any:
    """Collapse supported orthogonal physics input to ``{"value": N}``.

    Unknown fields and non-orthogonal shapes pass through unchanged so the
    existing family-tag and RefValue validators keep their current behaviour.
    """
    if field_name != "net_radiation":
        return value
    if not isinstance(value, Mapping) or "scheme" not in value:
        return value

    scheme = _token(value, "scheme", field_name)
    if scheme is None:
        return value

    if scheme == "forcing":
        _reject_foreign_keys(value, {"scheme", "ref"}, f"{field_name}.{scheme}")
        code = 0
    elif scheme == "narp":
        _reject_foreign_keys(
            value, {"scheme", "ldown", "variant", "ref"}, f"{field_name}.{scheme}"
        )
        ldown = _token(value, "ldown", f"{field_name}.{scheme}")
        if ldown is None:
            raise ValueError(
                f"'{field_name}.{scheme}' requires 'ldown' "
                "(observed, cloud, or air)."
            )
        variant = _token(value, "variant", f"{field_name}.{scheme}") or "standard"
        code = _NARP_CODES.get((ldown, variant))
        if code is None:
            raise ValueError(
                f"'{field_name}.{scheme}' does not support "
                f"ldown={ldown!r}, variant={variant!r}."
            )
    elif scheme == "spartacus":
        _reject_foreign_keys(
            value, {"scheme", "ldown", "ref"}, f"{field_name}.{scheme}"
        )
        ldown = _token(value, "ldown", f"{field_name}.{scheme}")
        if ldown is None:
            raise ValueError(
                f"'{field_name}.{scheme}' requires 'ldown' "
                "(observed, cloud, or air)."
            )
        code = _SPARTACUS_CODES.get(ldown)
        if code is None:
            raise ValueError(
                f"'{field_name}.{scheme}' does not support ldown={ldown!r}."
            )
    else:
        raise ValueError(
            f"'{field_name}.scheme' must be one of "
            "'forcing', 'narp', or 'spartacus'."
        )

    flat: dict[str, Any] = {"value": code}
    ref = value.get("ref")
    if ref is not None:
        flat["ref"] = ref
    return flat
