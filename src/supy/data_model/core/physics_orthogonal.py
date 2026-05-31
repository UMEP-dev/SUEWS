"""Orthogonal physics option input forms.

This module widens accepted YAML input while keeping the canonical internal
representation flat. Supported decompositions live here rather than in the
family-tag registry because they split one integer code across multiple axes.
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

_EMISSIONS_HEAT_CODES: dict[str, int] = {
    "l11": 1,
    "j11": 2,
    "l11_updated": 3,
}

_EMISSIONS_ANTHROPOGENIC_OFFSETS: dict[str, int] = {
    "none": 0,
    "qf_linked": 0,
    "detailed": 3,
}

_EMISSIONS_BIOGENIC_OFFSETS: dict[str, int] = {
    "none": 0,
    "rectangular": 10,
    "bellucco_local": 20,
    "bellucco_general": 30,
    "conductance": 40,
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


def _coerce_net_radiation(value: Mapping[str, Any]) -> dict[str, Any]:
    scheme = _token(value, "scheme", "net_radiation")
    if scheme is None:
        raise ValueError("'net_radiation.scheme' is required.")

    if scheme == "forcing":
        _reject_foreign_keys(value, {"scheme", "ref"}, f"net_radiation.{scheme}")
        code = 0
    elif scheme == "narp":
        _reject_foreign_keys(
            value, {"scheme", "ldown", "variant", "ref"}, f"net_radiation.{scheme}"
        )
        ldown = _token(value, "ldown", f"net_radiation.{scheme}")
        if ldown is None:
            raise ValueError(
                f"'net_radiation.{scheme}' requires 'ldown' "
                "(observed, cloud, or air)."
            )
        variant = _token(value, "variant", f"net_radiation.{scheme}") or "standard"
        code = _NARP_CODES.get((ldown, variant))
        if code is None:
            raise ValueError(
                f"'net_radiation.{scheme}' does not support "
                f"ldown={ldown!r}, variant={variant!r}."
            )
    elif scheme == "spartacus":
        _reject_foreign_keys(
            value, {"scheme", "ldown", "ref"}, f"net_radiation.{scheme}"
        )
        ldown = _token(value, "ldown", f"net_radiation.{scheme}")
        if ldown is None:
            raise ValueError(
                f"'net_radiation.{scheme}' requires 'ldown' "
                "(observed, cloud, or air)."
            )
        code = _SPARTACUS_CODES.get(ldown)
        if code is None:
            raise ValueError(
                f"'net_radiation.{scheme}' does not support ldown={ldown!r}."
            )
    else:
        raise ValueError(
            "'net_radiation.scheme' must be one of "
            "'forcing', 'narp', or 'spartacus'."
        )

    flat: dict[str, Any] = {"value": code}
    ref = value.get("ref")
    if ref is not None:
        flat["ref"] = ref
    return flat


def _emissions_code(heat: str, anthropogenic: str, biogenic: str) -> int:
    if heat == "observed":
        if anthropogenic == "none" and biogenic == "none":
            return 0
        raise ValueError(
            "'emissions.heat=observed' cannot be combined with CO2 axes; "
            "use modelled heat when CO2 is enabled."
        )

    heat_code = _EMISSIONS_HEAT_CODES.get(heat)
    if heat_code is None:
        raise ValueError(
            "'emissions.heat' must be one of 'observed', 'l11', 'j11', "
            "or 'l11_updated'."
        )

    if anthropogenic == "none" and biogenic == "none":
        return heat_code

    anthro_offset = _EMISSIONS_ANTHROPOGENIC_OFFSETS.get(anthropogenic)
    if anthro_offset is None:
        raise ValueError(
            "'emissions.co2.anthropogenic' must be one of 'none', "
            "'qf_linked', or 'detailed'."
        )

    biogenic_offset = _EMISSIONS_BIOGENIC_OFFSETS.get(biogenic)
    if biogenic_offset is None:
        raise ValueError(
            "'emissions.co2.biogenic' must be one of 'none', 'rectangular', "
            "'bellucco_local', 'bellucco_general', or 'conductance'."
        )

    if biogenic == "none":
        raise ValueError(
            "'emissions.co2.anthropogenic' requires a biogenic CO2 family; "
            "flat EmissionsMethod 0-6 disables CO2 flux output."
        )

    if anthropogenic == "none":
        raise ValueError(
            "Biogenic CO2 EmissionsMethod families also calculate "
            "anthropogenic CO2; choose 'qf_linked' or 'detailed'."
        )

    return biogenic_offset + heat_code + anthro_offset


def _coerce_emissions(value: Mapping[str, Any]) -> dict[str, Any]:
    _reject_foreign_keys(value, {"heat", "co2", "ref"}, "emissions")

    heat = _token(value, "heat", "emissions")
    if heat is None:
        raise ValueError("'emissions' orthogonal form requires 'heat'.")

    raw_co2 = value.get("co2", _MISSING)
    if raw_co2 is _MISSING:
        anthropogenic = "none"
        biogenic = "none"
    else:
        if not isinstance(raw_co2, Mapping):
            raise ValueError("'emissions.co2' must be a mapping.")
        _reject_foreign_keys(
            raw_co2, {"anthropogenic", "biogenic"}, "emissions.co2"
        )
        anthropogenic = (
            _token(raw_co2, "anthropogenic", "emissions.co2") or "none"
        )
        biogenic = _token(raw_co2, "biogenic", "emissions.co2") or "none"

    flat: dict[str, Any] = {"value": _emissions_code(heat, anthropogenic, biogenic)}
    ref = value.get("ref")
    if ref is not None:
        flat["ref"] = ref
    return flat


def coerce_orthogonal_to_flat(field_name: str, value: Any) -> Any:
    """Collapse supported orthogonal physics input to ``{"value": N}``.

    Unknown fields and non-orthogonal shapes pass through unchanged so the
    existing family-tag and RefValue validators keep their current behaviour.
    """
    if not isinstance(value, Mapping):
        return value

    if field_name == "net_radiation" and "scheme" in value:
        return _coerce_net_radiation(value)

    if field_name == "emissions" and ("heat" in value or "co2" in value):
        return _coerce_emissions(value)

    return value
