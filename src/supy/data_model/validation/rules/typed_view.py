"""Typed read-only view over normalised YAML configuration mappings.

Provides a single boundary through which Phase B scientific rules inspect
frozen or plain YAML trees without repeating ad-hoc ``isinstance(..., dict)``,
``MappingProxyType``, or ``entry.get("value")`` boilerplate. All unwrapping
delegates to ``pipeline.document`` and ``core.yaml_helpers``.
"""

from __future__ import annotations

from collections.abc import Mapping
from dataclasses import dataclass
from numbers import Integral, Real
from typing import Any

from ...core.physics_families import resolve_scalar_name
from ..core.yaml_helpers import (
    get_ref_or_bare,
    get_stebbs_block,
    get_stebbsmethod_value,
    get_value_safe,
    read_physics_key,
)
from ..pipeline.document import (
    is_ref_only,
    read_scalar,
    site_gridid,
    unwrap_entry,
)
from ..required_fields import BUILDING_PRESENT_SFR_TOL
from .hard_invariants import SPARTACUS_METHODS


def as_float(value: Any) -> float | None:
    """Convert an unwrapped or RefValue-wrapped entry to float, or None."""
    unwrapped = unwrap_entry(value)
    if unwrapped is None:
        return None
    try:
        return float(unwrapped)
    except (TypeError, ValueError):
        return None


def is_absent_doy_mapping(value: Any) -> bool:
    """Return True for empty or ref-only mappings representing a disabled DOY."""
    return isinstance(value, Mapping) and (not value or is_ref_only(value))


def method_code(value: Any, field_name: str | None = None) -> int | None:
    """Return an integer method code from raw, RefValue, enum, or readable input."""
    value = unwrap_entry(value)
    if hasattr(value, "value"):
        value = value.value
    result: Any = value
    if value is None:
        result = None
    elif isinstance(value, (bool, Integral)) or (
        isinstance(value, Real) and float(value).is_integer()
    ):
        result = int(value)
    elif isinstance(value, str):
        stripped = value.strip()
        if not stripped:
            result = None
        else:
            result = (
                resolve_scalar_name(field_name, stripped)
                if field_name is not None
                else int(stripped)
            )
    return result


@dataclass(frozen=True)
class PhysicsView:
    """Read-only typed view over ``model.physics``."""

    raw: Mapping[str, Any]

    @property
    def is_empty(self) -> bool:
        """Return True when the physics mapping is empty."""
        return not self.raw

    @property
    def stebbs_block(self) -> Mapping[str, Any]:
        """Return the folded ``model.physics.stebbs`` mapping."""
        block = get_stebbs_block(self.raw)
        return block if isinstance(block, Mapping) else {}

    @property
    def stebbs_method(self) -> Any:
        """Return the composed STEBBS method value (0, 1, 2, or None)."""
        return get_stebbsmethod_value(self.raw)

    @property
    def stebbs_method_int(self) -> int | None:
        """Return the composed STEBBS method coerced to ``int``, or None."""
        raw_val = self.stebbs_method
        try:
            return int(raw_val)
        except (TypeError, ValueError):
            return None

    @property
    def is_spartacus_enabled(self) -> bool:
        """Return True when ``net_radiation`` selects any SPARTACUS scheme."""
        net_radiation = self.read("net_radiation", 0)
        if isinstance(net_radiation, Mapping):
            unwrapped = unwrap_entry(net_radiation)
            if unwrapped is not net_radiation:
                net_radiation = unwrapped
            elif "spartacus" in net_radiation or net_radiation.get("scheme") == "spartacus":
                return True
        if isinstance(net_radiation, str):
            return "spartacus" in net_radiation.strip().lower()
        try:
            return int(net_radiation) in SPARTACUS_METHODS
        except (TypeError, ValueError):
            return False

    def read(self, key: str, default: Any = None) -> Any:
        """Read a flat or nested-family physics option."""
        return read_physics_key(self.raw, key, default)

    def code(self, key: str, default: Any = None) -> int | None:
        """Read a physics option resolved to its integer method code."""
        return method_code(self.read(key, default), key)

    def stebbs_value(self, key: str, default: Any = None) -> Any:
        """Read a rename-aware value from ``model.physics.stebbs``."""
        return get_value_safe(self.stebbs_block, key, default)


@dataclass(frozen=True)
class SiteView:
    """Read-only typed view over one entry in ``sites``."""

    index: int
    raw: Mapping[str, Any]
    name: str

    @property
    def gridid(self) -> Any:
        """Unwrapped site GRIDID (handles bare scalars and RefValue mappings)."""
        return site_gridid(self.raw)

    @property
    def raw_gridiv(self) -> Any:
        """Raw ``gridiv`` field as stored on the site mapping."""
        return self.raw.get("gridiv")

    @property
    def properties(self) -> Mapping[str, Any]:
        """Site ``properties`` mapping (empty dict when absent)."""
        props = self.raw.get("properties")
        return props if isinstance(props, Mapping) else {}

    @property
    def initial_states(self) -> Mapping[str, Any]:
        """Site ``initial_states`` mapping (empty dict when absent)."""
        states = self.raw.get("initial_states")
        return states if isinstance(states, Mapping) else {}

    @property
    def land_cover(self) -> Mapping[str, Any]:
        """Site ``properties.land_cover`` mapping (empty dict when absent)."""
        lc = unwrap_entry(self.properties.get("land_cover"))
        return lc if isinstance(lc, Mapping) else {}

    @property
    def building_archetype(self) -> Mapping[str, Any]:
        """Site ``properties.building_archetype`` mapping (empty dict when absent)."""
        ba = unwrap_entry(self.properties.get("building_archetype"))
        return ba if isinstance(ba, Mapping) else {}

    @property
    def stebbs(self) -> Mapping[str, Any]:
        """Site ``properties.stebbs`` mapping (empty dict when absent)."""
        st = unwrap_entry(self.properties.get("stebbs"))
        return st if isinstance(st, Mapping) else {}

    @property
    def vertical_layers(self) -> Mapping[str, Any]:
        """Site ``properties.vertical_layers`` mapping (empty dict when absent)."""
        vl = unwrap_entry(self.properties.get("vertical_layers"))
        return vl if isinstance(vl, Mapping) else {}

    @property
    def irrigation(self) -> Mapping[str, Any]:
        """Site ``properties.irrigation`` mapping (empty dict when absent)."""
        irr = unwrap_entry(self.properties.get("irrigation"))
        return irr if isinstance(irr, Mapping) else {}

    @property
    def anthropogenic_emissions(self) -> Mapping[str, Any]:
        """Site ``properties.anthropogenic_emissions`` mapping (empty dict when absent)."""
        anthro = unwrap_entry(self.properties.get("anthropogenic_emissions"))
        return anthro if isinstance(anthro, Mapping) else {}

    def scalar(
        self, section: Mapping[str, Any], key: str, default: Any = None
    ) -> Any:
        """Read a rename-aware unwrapped scalar from *section*."""
        return get_value_safe(section, key, default)

    def exact_scalar(
        self, section: Mapping[str, Any], key: str, default: Any = None
    ) -> Any:
        """Read an exact-key unwrapped scalar from *section*."""
        return read_scalar(section, key, default)

    def surface_sfr(self, surface_name: str) -> float | None:
        """Return the building/surface fraction for *surface_name* as float."""
        surface = unwrap_entry(self.land_cover.get(surface_name))
        if not isinstance(surface, Mapping):
            return None
        return as_float(surface.get("sfr"))

    def surface_fractions(self) -> list[tuple[str, Any]]:
        """Return ``(surface_type, sfr_value)`` for every non-null land-cover surface."""
        out: list[tuple[str, Any]] = []
        for surface_type, surface_props in self.land_cover.items():
            if isinstance(surface_props, Mapping):
                sfr_value = get_ref_or_bare(surface_props, "sfr")
                if sfr_value is not None:
                    out.append((surface_type, sfr_value))
        return out

    def has_buildings(self, tol: float = BUILDING_PRESENT_SFR_TOL) -> bool:
        """Return True if ``land_cover.bldgs.sfr > tol``."""
        sfr = self.surface_sfr("bldgs")
        return sfr is not None and sfr > tol

    def first_vertical_value(self, key: str) -> float | None:
        """Return the first element of a vertical-layers array as float."""
        arr = unwrap_entry(self.vertical_layers.get(key))
        if not isinstance(arr, (list, tuple)) or not arr:
            return None
        return as_float(arr[0])

    def last_nonzero_height(self) -> float | None:
        """Return the last non-zero float in ``vertical_layers.height``."""
        arr = unwrap_entry(self.vertical_layers.get("height"))
        if not isinstance(arr, (list, tuple)) or not arr:
            return None
        last_nz: float | None = None
        for item in arr:
            val = as_float(item)
            if val is not None and val != 0.0:
                last_nz = val
        return last_nz

    def facet_layer_values(self, layers_key: str, field: str) -> list[Any]:
        """Collect non-None values of *field* from ``vertical_layers.<layers_key>``."""
        layers = self.vertical_layers.get(layers_key, ())
        if isinstance(layers, Mapping):
            layers = (layers,)
        if not isinstance(layers, (list, tuple)):
            return []
        found: list[Any] = []
        for layer in layers:
            if isinstance(layer, Mapping):
                val = get_value_safe(layer, field)
                if val is not None:
                    found.append(val)
        return found


@dataclass(frozen=True)
class TypedConfigView:
    """Immutable typed view over a full SUEWS YAML configuration mapping."""

    raw: Mapping[str, Any]
    physics: PhysicsView
    sites: tuple[SiteView, ...]

    @classmethod
    def from_mapping(cls, yaml_data: Any) -> TypedConfigView:
        """Construct a ``TypedConfigView`` from a raw or frozen config mapping."""
        root = yaml_data if isinstance(yaml_data, Mapping) else {}
        model = root.get("model")
        physics_raw = model.get("physics") if isinstance(model, Mapping) else None
        physics = PhysicsView(physics_raw if isinstance(physics_raw, Mapping) else {})

        sites_raw = root.get("sites", ())
        site_views: list[SiteView] = []
        if isinstance(sites_raw, Mapping):
            for idx, (site_key, site_data) in enumerate(sites_raw.items()):
                if isinstance(site_data, Mapping):
                    site_views.append(SiteView(index=idx, raw=site_data, name=str(site_key)))
        elif isinstance(sites_raw, (list, tuple)):
            for idx, site_data in enumerate(sites_raw):
                if isinstance(site_data, Mapping):
                    raw_name = site_data.get("name")
                    name = str(raw_name) if raw_name is not None else "Unknown"
                    site_views.append(SiteView(index=idx, raw=site_data, name=name))

        return cls(raw=root, physics=physics, sites=tuple(site_views))

    @property
    def start_time(self) -> Any:
        """Return unwrapped ``model.control.start_time``, or None."""
        model = self.raw.get("model")
        control = model.get("control") if isinstance(model, Mapping) else None
        if not isinstance(control, Mapping):
            return None
        return get_value_safe(control, "start_time")

    def any_buildings_present(self, tol: float = BUILDING_PRESENT_SFR_TOL) -> bool:
        """Return True if any site has ``land_cover.bldgs.sfr > tol``."""
        return any(site.has_buildings(tol=tol) for site in self.sites)
