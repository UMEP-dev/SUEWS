"""Family-tagged nested physics option registry (gh#972).

Users may supply ``model.physics`` fields with a family tag::

    net_radiation:
      spartacus:
        value: 1001

The family tag is validated against the numeric codes it covers, then
discarded so the canonical internal representation stays flat
(``{value: N}``). YAML round-trip emits the flat form.

Accept-only widening: no schema bump. Every previously-valid YAML
continues to validate and round-trips byte-identically.
"""

from __future__ import annotations

from collections.abc import Mapping
from enum import Enum
from numbers import Integral, Real
from typing import Any


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
    # Biogen families (11-15, 21-25, 41-45) are documented in the
    # EmissionsMethod docstring but not yet declared as enum members —
    # Pydantic would reject them anyway. The registry stays aligned with
    # what the enum actually accepts; extend here when enum members land.
    "emissions": {
        "observed": frozenset({0}),
        "simple": frozenset({1, 2, 3, 4, 5}),
    },
}


def coerce_nested_to_flat(field_name: str, value: Any) -> Any:
    """Collapse a family-tagged nested mapping to the flat ``{value: N}`` form.

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
        When the input carries multiple family tags, combines a family
        tag with other sibling keys, the inner mapping lacks ``value``,
        or the numeric code does not belong to the declared family.
    """
    if field_name not in PHYSICS_FAMILIES:
        return value
    if not isinstance(value, Mapping):
        return value

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
                f"'{field_name}.{family}.value' must be an integer code "
                f"(got {code!r})."
            )
        code_int = int(code_real)
    else:
        raise ValueError(
            f"'{field_name}.{family}.value' must be an integer code "
            f"(got {code!r})."
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
