"""Readable and family-tagged physics option registry (gh#972 follow-up).

Users may supply ``model.physics`` fields with a family tag::

    net_radiation:
      spartacus:
        value: 1001

The family tag is validated against the numeric codes it covers, then
discarded so the canonical internal representation stays flat
(``{value: N}``). YAML round-trip emits the flat form.

Users may also supply registered human-readable method names::

    storage_heat: ohm
    stability: cn98
    stebbs:
      capacitance: provided

Accept-only widening: no schema bump. Every previously-valid YAML
continues to validate and round-trips byte-identically.
"""

from __future__ import annotations

from collections.abc import Mapping
from contextlib import suppress
from enum import Enum
from numbers import Integral, Real
import re
from typing import Any

from .physics_orthogonal import coerce_orthogonal_to_flat, fold_storage_heat_ohm_inc_qf

PHYSICS_FAMILIES: dict[str, dict[str, frozenset[int]]] = {
    "net_radiation": {
        "forcing": frozenset({0}),
        "narp": frozenset({1, 2, 3, 11, 12, 13, 100, 200, 300}),
        "spartacus": frozenset({1001, 1002, 1003}),
    },
    "storage_heat": {
        "observed": frozenset({0}),
        "ohm": frozenset({1}),
        "anohm": frozenset({3}),
        "estm": frozenset({4}),
        "ehc": frozenset({5}),
        "dyohm": frozenset({6}),
        "stebbs": frozenset({7}),
    },
    "emissions": {
        "observed": frozenset({0}),
        "simple": frozenset({1, 2, 3, 4, 5, 6}),
        "biogenic_rectangular": frozenset({11, 12, 13, 14, 15, 16}),
        "biogenic_bellucco_local": frozenset({21, 22, 23, 24, 25, 26}),
        "biogenic_bellucco_general": frozenset({31, 32, 33, 34, 35, 36}),
        "biogenic_conductance": frozenset({41, 42, 43, 44, 45, 46}),
    },
}

PHYSICS_PUBLIC_KEY_ALIASES: dict[str, str] = {
    # Public sample/config surface names that fold to long-standing internal
    # ModelPhysics fields without changing the Fortran-facing state columns.
    "leaf_area_index": "laimethod",
    "snow": "snow_use",
}

STEBBS_PUBLIC_KEY_ALIASES: dict[str, str] = {
    "parameter_source": "parameters",
}

_PHYSICS_NAME_SPECS: dict[str, list[tuple[int, str, tuple[str, ...]]]] = {
    "net_radiation": [
        (0, "observed", ("forcing",)),
        (1, "ldown_observed", ()),
        (2, "ldown_cloud", ()),
        (3, "ldown_air", ()),
        (11, "ldown_surface", ()),
        (12, "ldown_cloud_surface", ()),
        (13, "ldown_air_surface", ()),
        (100, "ldown_zenith", ()),
        (200, "ldown_cloud_zenith", ()),
        (300, "ldown_air_zenith", ()),
        (1001, "ldown_ss_observed", ()),
        (1002, "ldown_ss_cloud", ()),
        (1003, "ldown_ss_air", ()),
    ],
    "emissions": [
        (0, "observed", ()),
        (1, "l11", ()),
        (2, "j11", ()),
        (3, "l11_updated", ()),
        (4, "j19", ()),
        (5, "j19_updated", ()),
        (6, "l11_updated_detailed", ()),
        (11, "biogen_rect_l11", ()),
        (12, "biogen_rect_j11", ()),
        (13, "biogen_rect_l11_updated", ()),
        (14, "biogen_rect_l11_detailed", ()),
        (15, "biogen_rect_j11_detailed", ()),
        (16, "biogen_rect_l11_updated_detailed", ()),
        (21, "biogen_bellucco_local_l11", ()),
        (22, "biogen_bellucco_local_j11", ()),
        (23, "biogen_bellucco_local_l11_updated", ()),
        (24, "biogen_bellucco_local_l11_detailed", ()),
        (25, "biogen_bellucco_local_j11_detailed", ()),
        (26, "biogen_bellucco_local_l11_updated_detailed", ()),
        (31, "biogen_bellucco_general_l11", ()),
        (32, "biogen_bellucco_general_j11", ()),
        (33, "biogen_bellucco_general_l11_updated", ()),
        (34, "biogen_bellucco_general_l11_detailed", ()),
        (35, "biogen_bellucco_general_j11_detailed", ()),
        (36, "biogen_bellucco_general_l11_updated_detailed", ()),
        (41, "biogen_conductance_l11", ()),
        (42, "biogen_conductance_j11", ()),
        (43, "biogen_conductance_l11_updated", ()),
        (44, "biogen_conductance_l11_detailed", ()),
        (45, "biogen_conductance_j11_detailed", ()),
        (46, "biogen_conductance_l11_updated_detailed", ()),
    ],
    "storage_heat": [
        (0, "observed", ()),
        (1, "ohm", ("ohm_without_qf",)),
        (3, "anohm", ("s17",)),
        (4, "estm", ("o05",)),
        (5, "ehc", ()),
        (6, "dyohm", ("l25",)),
        (7, "stebbs", ()),
    ],
    "ohm_inc_qf": [(0, "exclude", ()), (1, "include", ())],
    "roughness_length_momentum": [
        (1, "fixed", ()),
        (2, "variable", ()),
        (3, "macdonald", ("m98",)),
        (4, "lambdap_dependent", ("go99",)),
        (5, "alternative", ()),
    ],
    "roughness_length_heat": [
        (1, "brutsaert", ("b82",)),
        (2, "kawai", ("k09",)),
        (3, "voogt_grimmond", ("vg00",)),
        (4, "kanda", ("k07",)),
        (5, "adaptive", ()),
    ],
    "stability": [
        (0, "not_used", ()),
        (1, "not_used2", ()),
        (2, "hoegstrom", ()),
        (3, "campbell_norman", ("cn98",)),
        (4, "businger_hoegstrom", ("bh71",)),
    ],
    "soil_moisture_deficit": [
        (0, "modelled", ()),
        (1, "observed", ()),
    ],
    "water_use": [(0, "modelled", ()), (1, "observed", ())],
    "laimethod": [(0, "observed", ()), (1, "modelled", ())],
    "roughness_sublayer": [
        (0, "most", ()),
        (1, "rst", ("t19",)),
        (2, "variable", ()),
    ],
    "frontal_area_index": [
        (0, "observed", ("provided", "use_provided")),
        (1, "modelled", ("simple_scheme",)),
    ],
    "roughness_sublayer_level": [
        (0, "none", ()),
        (1, "basic", ()),
        (2, "detailed", ()),
    ],
    "surface_conductance": [
        (1, "jarvi", ("j11",)),
        (2, "ward", ("w16",)),
    ],
    "snow_use": [(0, "disabled", ()), (1, "enabled", ())],
    # Legacy flat STEBBS master toggle; the current public surface is the
    # nested `stebbs.enabled` + `stebbs.parameters` object.
    "stebbs": [(0, "none", ()), (1, "default", ()), (2, "provided", ())],
    "parameters": [(1, "default", ()), (2, "provided", ())],
    # Reading/STEBBS Column-D public selector name: `capacitance`, not
    # `capacitance_method` (Silvia Rognone mapping_names_28May.xlsx, row 115).
    "capacitance": [
        (0, "default", ()),
        (1, "provided", ()),
        (2, "parameterise", ("parameterize",)),
    ],
    "setpoint": [
        (0, "constant", ()),
        (1, "dependent", ()),
        (2, "scheduled", ()),
    ],
    "same_albedo_wall": [(0, "disabled", ()), (1, "enabled", ())],
    "same_albedo_roof": [(0, "disabled", ()), (1, "enabled", ())],
    "same_emissivity_wall": [(0, "disabled", ()), (1, "enabled", ())],
    "same_emissivity_roof": [(0, "disabled", ()), (1, "enabled", ())],
}

MODEL_PHYSICS_ENUM_FIELDS: tuple[str, ...] = (
    "net_radiation",
    "emissions",
    "storage_heat",
    "ohm_inc_qf",
    "roughness_length_momentum",
    "roughness_length_heat",
    "stability",
    "soil_moisture_deficit",
    "water_use",
    "laimethod",
    "roughness_sublayer",
    "frontal_area_index",
    "roughness_sublayer_level",
    "surface_conductance",
    "snow_use",
)

STEBBS_PHYSICS_ENUM_FIELDS: tuple[str, ...] = (
    "parameters",
    "capacitance",
    "setpoint",
    "same_albedo_wall",
    "same_albedo_roof",
    "same_emissivity_wall",
    "same_emissivity_roof",
)

PHYSICS_ENUM_FIELDS: tuple[str, ...] = tuple(_PHYSICS_NAME_SPECS)


def _build_lookup_tables() -> tuple[
    dict[str, dict[int, str]], dict[str, dict[str, int]]
]:
    code_to_canonical: dict[str, dict[int, str]] = {}
    alias_to_code: dict[str, dict[str, int]] = {}
    for field_name, specs in _PHYSICS_NAME_SPECS.items():
        canon: dict[int, str] = {}
        aliases: dict[str, int] = {}
        for code, canonical, extras in specs:
            canon[code] = canonical
            for name in (canonical, *extras):
                key = name.lower()
                if key in aliases and aliases[key] != code:
                    raise ValueError(
                        f"physics_families: alias {key!r} for '{field_name}' "
                        f"maps to both {aliases[key]} and {code}."
                    )
                aliases[key] = code
        code_to_canonical[field_name] = canon
        alias_to_code[field_name] = aliases
    return code_to_canonical, alias_to_code


_CODE_TO_CANONICAL, _ALIAS_TO_CODE = _build_lookup_tables()
_CITATION_ALIAS_RE = re.compile(r"^[a-z]{1,3}\d{2}$")
_CITATION_PREFERRED_FIELDS = frozenset(
    {
        "emissions",
        "roughness_length_momentum",
        "roughness_length_heat",
        "stability",
        "surface_conductance",
    }
)
_COMPATIBILITY_DISPLAY_NAMES = {
    ("soil_moisture_deficit", 2): "observed",
}
_PHYSICS_CANONICAL_TO_PUBLIC = {
    canonical_name: public_name
    for public_name, canonical_name in PHYSICS_PUBLIC_KEY_ALIASES.items()
}
_STEBBS_CANONICAL_TO_PUBLIC = {
    canonical_name: public_name
    for public_name, canonical_name in STEBBS_PUBLIC_KEY_ALIASES.items()
}


def accepted_physics_names(field_name: str) -> tuple[str, ...]:
    """Return registered readable names accepted for one physics selector."""
    return tuple(sorted(_ALIAS_TO_CODE.get(field_name, ())))


def canonical_physics_name(field_name: str, code: int) -> str | None:
    """Return the canonical readable name for a physics selector code."""
    return _CODE_TO_CANONICAL.get(field_name, {}).get(int(code))


def preferred_physics_name(field_name: str, code: int) -> str | None:
    """Return the preferred public readable name for a physics selector code."""
    code_int = int(code)
    if (field_name, code_int) in _COMPATIBILITY_DISPLAY_NAMES:
        return _COMPATIBILITY_DISPLAY_NAMES[(field_name, code_int)]
    if field_name in _CITATION_PREFERRED_FIELDS:
        table = _ALIAS_TO_CODE.get(field_name, {})
        for name, alias_code in table.items():
            if alias_code == code_int and _CITATION_ALIAS_RE.match(name):
                return name.upper()
    return canonical_physics_name(field_name, code_int)


def public_model_physics_key(field_name: str) -> str:
    """Return the preferred public key for a top-level ModelPhysics field."""
    return _PHYSICS_CANONICAL_TO_PUBLIC.get(field_name, field_name)


def public_stebbs_physics_key(field_name: str) -> str:
    """Return the preferred public key for a nested STEBBS physics field."""
    return _STEBBS_CANONICAL_TO_PUBLIC.get(field_name, field_name)


def fold_public_physics_key_aliases(values: dict, class_name: str) -> dict:
    """Fold public-facing physics key aliases to canonical internal fields."""
    for public_name, canonical_name in PHYSICS_PUBLIC_KEY_ALIASES.items():
        if public_name not in values:
            continue
        if canonical_name in values:
            raise ValueError(
                f"{class_name}: both '{public_name}' and '{canonical_name}' are "
                f"present. Use only '{public_name}'."
            )
        values[canonical_name] = values.pop(public_name)
    return values


def fold_stebbs_public_key_aliases(values: dict, class_name: str) -> dict:
    """Fold public-facing nested STEBBS aliases to canonical internal leaves."""
    for public_name, canonical_name in STEBBS_PUBLIC_KEY_ALIASES.items():
        if public_name not in values:
            continue
        if canonical_name in values:
            raise ValueError(
                f"{class_name}: both '{public_name}' and '{canonical_name}' are "
                f"present. Use only '{public_name}'."
            )
        values[canonical_name] = values.pop(public_name)
    return values


def resolve_scalar_name(field_name: str, name: str) -> int:
    """Resolve a registered human-readable physics method name to its code."""
    table = _ALIAS_TO_CODE.get(field_name, {})
    key = name.strip().lower()
    if key in table:
        return table[key]
    valid = sorted(table)
    hint = ""
    if field_name in {"net_radiation", "emissions"}:
        hint = " Orthogonal decomposed mappings are also accepted for this field."
    raise ValueError(
        f"'{field_name}' got unknown scheme name {name!r}; valid names: {valid}.{hint}"
    )


def _coerce_scalar_name(field_name: str, value: Any) -> Any:
    """Collapse bare or RefValue-wrapped readable names to ``{value: code}``."""
    if isinstance(value, str):
        if not value.strip():
            return value
        return {"value": resolve_scalar_name(field_name, value)}
    if isinstance(value, Mapping) and isinstance(value.get("value"), str):
        if not value["value"].strip():
            return value
        flat = dict(value)
        flat["value"] = resolve_scalar_name(field_name, value["value"])
        return flat
    return value


def _coerce_family_tag(field_name: str, value: Mapping[str, Any]) -> Any:
    families = PHYSICS_FAMILIES[field_name]
    matched = [key for key in families if key in value]
    if not matched:
        return value

    if len(matched) > 1:
        raise ValueError(
            f"'{field_name}' received multiple family tags ({matched}); "
            f"supply exactly one of {sorted(families)}."
        )

    family = matched[0]
    foreign = [key for key in value if key not in {family, "ref"}]
    if foreign:
        raise ValueError(
            f"'{field_name}.{family}' cannot be combined with sibling keys "
            f"{foreign}. Move the family tag to the top level or drop the "
            f"other keys."
        )

    inner = value[family]
    if not isinstance(inner, Mapping) or "value" not in inner:
        raise ValueError(
            f"'{field_name}.{family}' must be a mapping with a 'value' key "
            f"(got {type(inner).__name__})."
        )

    code = inner["value"]
    if isinstance(code, Mapping):
        raise ValueError(
            f"'{field_name}.{family}.value' must be a scalar numeric code, "
            f"not a nested mapping."
        )

    if isinstance(code, Enum):
        code = code.value

    if isinstance(code, bool):
        code_int = int(code)
    elif isinstance(code, Integral):
        code_int = int(code)
    elif isinstance(code, Real):
        code_real = float(code)
        if not code_real.is_integer():
            raise ValueError(
                f"'{field_name}.{family}.value' must be an integer code (got {code!r})."
            )
        code_int = int(code_real)
    else:
        raise ValueError(
            f"'{field_name}.{family}.value' must be an integer code (got {code!r})."
        )

    if code_int not in families[family]:
        valid = sorted(families[family])
        other_families = {
            fam: sorted(codes) for fam, codes in families.items() if fam != family
        }
        raise ValueError(
            f"'{field_name}.{family}' expects one of {valid}, got {code_int}. "
            f"Other families for '{field_name}': {other_families}."
        )

    flat: dict[str, Any] = {"value": code_int}
    inner_ref = inner.get("ref") if isinstance(inner, Mapping) else None
    outer_ref = value.get("ref") if isinstance(value, Mapping) else None
    carried = inner_ref if inner_ref is not None else outer_ref
    if carried is not None:
        flat["ref"] = carried
    return flat


def flatten_physics_in_config(data: Any) -> Any:
    """Rewrite supported readable physics inputs to flat ``{value: N}`` mappings.

    The raw-dict validation pipeline predates these readable names and expects
    enum fields to look like RefValue mappings. This mutates and returns
    ``data``; malformed values are left for the normal validators to report.
    """
    if not isinstance(data, Mapping):
        return data
    model = data.get("model")
    if not isinstance(model, Mapping):
        return data
    physics = model.get("physics")
    if not isinstance(physics, dict):
        return data

    fold_public_physics_key_aliases(physics, "ModelPhysics")

    with suppress(TypeError, ValueError):
        fold_storage_heat_ohm_inc_qf(physics, "ModelPhysics")

    for field_name in MODEL_PHYSICS_ENUM_FIELDS:
        if field_name not in physics:
            continue
        try:
            value = coerce_orthogonal_to_flat(field_name, physics[field_name])
            physics[field_name] = coerce_nested_to_flat(field_name, value)
        except (TypeError, ValueError):
            pass

    stebbs = physics.get("stebbs")
    if isinstance(stebbs, dict) and "value" not in stebbs:
        fold_stebbs_public_key_aliases(stebbs, "StebbsPhysics")
        for field_name in STEBBS_PHYSICS_ENUM_FIELDS:
            if field_name not in stebbs:
                continue
            try:
                stebbs[field_name] = coerce_nested_to_flat(
                    field_name,
                    stebbs[field_name],
                )
            except (TypeError, ValueError):
                pass

    return data


def coerce_nested_to_flat(field_name: str, value: Any) -> Any:
    """Collapse accepted readable/family physics inputs to ``{value: N}``.

    Parameters
    ----------
    field_name : str
        ModelPhysics field name (e.g. ``"net_radiation"``).
    value : Any
        Raw input from YAML / dict. Unknown shapes pass through untouched
        so Pydantic can report a consistent error.

    Returns
    -------
    Any
        ``{"value": N}`` (plus any ``ref`` the nested dict carried) when a
        family tag was detected; otherwise ``value`` unchanged.

    Raises
    ------
    ValueError
        When the input carries an unknown scalar name, multiple family tags,
        combines a family tag with other sibling keys, the inner mapping lacks
        ``value``, or the numeric code does not belong to the declared family.
    """
    if field_name not in PHYSICS_ENUM_FIELDS:
        return value

    value = _coerce_scalar_name(field_name, value)
    if not isinstance(value, Mapping):
        return value

    if field_name in PHYSICS_FAMILIES:
        return _coerce_family_tag(field_name, value)

    return value
