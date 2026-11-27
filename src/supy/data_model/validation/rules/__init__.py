"""Shared Tier-1 hard-invariant kernel and Tier-2 scientific advisory helpers.

Submodules
----------
- ``hard_invariants``: pure checks shared by ``SUEWSConfig``/core models and Phase B.
- ``typed_view``: read-only typed view over raw/frozen configuration mappings.
- ``advisory``: Tier-2 non-blocking scientific advisories emitted by Phase B.
"""

from .hard_invariants import (
    SAME_SURFACE_TOL,
    SPARTACUS_METHODS,
    InvariantViolation,
    check_dls_doy_pair,
    check_irrigation_doy_pair,
    check_same_surface_property,
    check_spartacus_storage_heat,
    check_stebbs_internal_shading,
    is_modelled_irrigation_active,
    resolve_water_use_code,
)

__all__ = [
    "SAME_SURFACE_TOL",
    "SPARTACUS_METHODS",
    "InvariantViolation",
    "check_dls_doy_pair",
    "check_irrigation_doy_pair",
    "check_same_surface_property",
    "check_spartacus_storage_heat",
    "check_stebbs_internal_shading",
    "is_modelled_irrigation_active",
    "resolve_water_use_code",
]
