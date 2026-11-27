"""Shared Tier-1 hard-invariant kernel and Tier-2 scientific advisory helpers.

Submodules
----------
- ``hard_invariants``: pure checks shared by ``SUEWSConfig``/core models and Phase B.
- ``typed_view``: deeply immutable typed view over raw/normalised YAML mappings.
- ``readonly_model``: deeply immutable attribute proxy over validated Pydantic models.
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
from .readonly_model import ReadOnlyModelProxy, freeze_model
from .typed_view import (
    ControlView,
    LandCoverView,
    PhysicsView,
    SiteView,
    SurfaceView,
    TypedConfigView,
    VerticalLayersView,
)

__all__ = [
    "SAME_SURFACE_TOL",
    "SPARTACUS_METHODS",
    "ControlView",
    "InvariantViolation",
    "LandCoverView",
    "PhysicsView",
    "ReadOnlyModelProxy",
    "SiteView",
    "SurfaceView",
    "TypedConfigView",
    "VerticalLayersView",
    "check_dls_doy_pair",
    "check_irrigation_doy_pair",
    "check_same_surface_property",
    "check_spartacus_storage_heat",
    "check_stebbs_internal_shading",
    "freeze_model",
    "is_modelled_irrigation_active",
    "resolve_water_use_code",
]
