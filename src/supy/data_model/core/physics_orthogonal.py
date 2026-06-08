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

_STORAGE_HEAT_FAMILIES = frozenset(
    {"observed", "ohm", "anohm", "estm", "ehc", "dyohm", "stebbs"}
)
_REFVALUE_KEYS = frozenset({"value", "ref"})
_STORAGE_HEAT_QF_KEYS = ("include_qf", "ohm_inc_qf", "ohmincqf")


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


def fold_storage_heat_ohm_inc_qf(values: dict, class_name: str) -> dict:
    """Move ``storage_heat.ohm_inc_qf`` to the canonical flat field.

    ``OHMIncQF`` is a sub-choice of the storage heat calculation, but the
    stable internal and Fortran-facing surface still stores it as the flat
    ``ohm_inc_qf`` field. This fold lets YAML expose the semantic ownership
    without changing downstream state layout.
    """
    storage = values.get("storage_heat")
    if not isinstance(storage, Mapping):
        return values

    if "ohm" in storage and isinstance(storage["ohm"], Mapping):
        folded = _fold_storage_heat_ohm_block(storage, values, class_name)
        if folded:
            return values

    qf_keys = [key for key in ("ohm_inc_qf", "ohmincqf") if key in storage]
    if not qf_keys:
        return values
    if len(qf_keys) > 1:
        raise ValueError(
            f"{class_name}: storage_heat contains multiple OHMIncQF spellings "
            f"({', '.join(qf_keys)}). Use only 'storage_heat.ohm.include_qf'."
        )
    flat_qf_keys = [key for key in ("ohm_inc_qf", "ohmincqf") if key in values]
    if flat_qf_keys:
        raise ValueError(
            f"{class_name}: both flat '{flat_qf_keys[0]}' and nested "
            "'storage_heat.ohm.include_qf' are present. Use only the nested form."
        )

    block = dict(storage)
    qf_key = qf_keys[0]
    qf_value = block.pop(qf_key)

    if "scheme" in block:
        _reject_foreign_keys(block, {"scheme", "ref"}, "storage_heat")
        folded: dict[str, Any] = {"value": block["scheme"]}
        if "ref" in block:
            folded["ref"] = block["ref"]
        values["ohm_inc_qf"] = qf_value
        values["storage_heat"] = folded
        return values

    keys = set(block)
    if not keys:
        raise ValueError(
            f"{class_name}: nested 'storage_heat.ohm_inc_qf' requires either "
            "'storage_heat.scheme', a storage heat family tag, or "
            "'storage_heat.value'."
        )
    if keys <= _REFVALUE_KEYS:
        values["ohm_inc_qf"] = qf_value
        values["storage_heat"] = block
        return values

    matched = keys & _STORAGE_HEAT_FAMILIES
    if matched:
        foreign = keys - matched - {"ref"}
        if foreign:
            raise ValueError(
                f"{class_name}: 'storage_heat' family form cannot be combined "
                f"with sibling keys {sorted(foreign)}."
            )
        values["ohm_inc_qf"] = qf_value
        values["storage_heat"] = block
        return values

    raise ValueError(
        f"{class_name}: nested 'storage_heat.ohm_inc_qf' requires either "
        "'storage_heat.scheme', a storage heat family tag, or 'storage_heat.value'."
    )


def _fold_storage_heat_ohm_block(
    storage: Mapping[str, Any], values: dict, class_name: str
) -> bool:
    """Fold ``storage_heat.ohm.include_qf`` to storage heat + OHMIncQF."""
    ohm_block = dict(storage["ohm"])
    qf_keys = [key for key in _STORAGE_HEAT_QF_KEYS if key in ohm_block]
    if not qf_keys:
        return False
    if len(qf_keys) > 1:
        raise ValueError(
            f"{class_name}: storage_heat.ohm contains multiple OHMIncQF spellings "
            f"({', '.join(qf_keys)}). Use only 'storage_heat.ohm.include_qf'."
        )
    flat_qf_keys = [key for key in ("ohm_inc_qf", "ohmincqf") if key in values]
    if flat_qf_keys:
        raise ValueError(
            f"{class_name}: both flat '{flat_qf_keys[0]}' and nested "
            "'storage_heat.ohm.include_qf' are present. Use only the nested form."
        )

    outer_foreign = [key for key in storage if key not in {"ohm", "ref"}]
    if outer_foreign:
        raise ValueError(
            f"{class_name}: 'storage_heat.ohm' cannot be combined with sibling "
            f"keys {sorted(outer_foreign)}."
        )

    qf_key = qf_keys[0]
    qf_value = ohm_block.pop(qf_key)
    if qf_key == "include_qf":
        qf_value = _normalise_include_qf(qf_value, class_name)

    inner_foreign = [key for key in ohm_block if key != "ref"]
    if inner_foreign:
        raise ValueError(
            f"{class_name}: 'storage_heat.ohm' cannot be combined with inner "
            f"keys {sorted(inner_foreign)}."
        )

    folded: dict[str, Any] = {"value": "ohm"}
    if "ref" in ohm_block:
        folded["ref"] = ohm_block["ref"]
    elif "ref" in storage:
        folded["ref"] = storage["ref"]
    values["storage_heat"] = folded
    values["ohm_inc_qf"] = qf_value
    return True


def _normalise_include_qf(value: Any, class_name: str) -> Any:
    """Map public ``include_qf`` predicate values to the OHMIncQF selector."""
    if isinstance(value, bool):
        return "include" if value else "exclude"
    if isinstance(value, int) and value in {0, 1}:
        return "include" if value else "exclude"
    if isinstance(value, str):
        token = value.strip().lower()
        if token in {"include", "included", "true", "yes", "y", "on", "1"}:
            return "include"
        if token in {"exclude", "excluded", "false", "no", "n", "off", "0"}:
            return "exclude"
    raise ValueError(
        f"{class_name}: 'storage_heat.ohm.include_qf' expects a boolean or one of "
        "'include'/'exclude'."
    )


def _coerce_net_radiation(value: Mapping[str, Any]) -> dict[str, Any]:
    value = _normalise_net_radiation_shape(value)
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


def _normalise_net_radiation_shape(value: Mapping[str, Any]) -> Mapping[str, Any]:
    """Accept ``net_radiation.<scheme>`` in addition to ``scheme: <name>``."""
    if "scheme" in value:
        return value

    scheme_keys = [key for key in ("forcing", "narp", "spartacus") if key in value]
    if not scheme_keys:
        return value
    if len(scheme_keys) > 1:
        raise ValueError(
            "'net_radiation' received multiple scheme keys "
            f"({', '.join(scheme_keys)}); supply exactly one."
        )

    scheme = scheme_keys[0]
    raw_options = value[scheme]
    if not isinstance(raw_options, Mapping) or "value" in raw_options:
        return value

    foreign = [key for key in value if key not in {scheme, "ref"}]
    if foreign:
        raise ValueError(
            f"'net_radiation.{scheme}' cannot be combined with sibling keys "
            f"{sorted(foreign)}."
        )

    normalised = {"scheme": scheme, **raw_options}
    if "ref" in value and "ref" not in normalised:
        normalised["ref"] = value["ref"]
    return normalised


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

    if field_name == "net_radiation":
        normalised = _normalise_net_radiation_shape(value)
        if "scheme" in normalised:
            return _coerce_net_radiation(normalised)
        return value

    if field_name == "emissions" and ("heat" in value or "co2" in value):
        return _coerce_emissions(value)

    return value
