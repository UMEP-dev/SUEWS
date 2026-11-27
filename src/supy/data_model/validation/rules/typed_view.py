"""Typed read-only view over normalised YAML configuration mappings.

Provides a single boundary through which Phase B scientific rules inspect
frozen or plain YAML trees without repeating ad-hoc ``isinstance(..., dict)``,
``MappingProxyType``, or ``entry.get("value")`` boilerplate. All unwrapping
delegates to ``pipeline.document`` and ``core.yaml_helpers``.
"""

from __future__ import annotations

from collections.abc import Mapping
from dataclasses import dataclass
import math
from numbers import Integral, Real
from typing import Any

from ...core.field_renames import read_physics_key
from ...core.physics_families import resolve_scalar_name
from ..core.yaml_helpers import (
    get_ref_or_bare,
    get_stebbs_block,
    get_stebbsmethod_value,
    get_value_safe,
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
    """Convert an unwrapped or RefValue-wrapped entry to a finite float, or None."""
    unwrapped = unwrap_entry(value)
    if unwrapped is None or isinstance(unwrapped, bool):
        return None
    try:
        num = float(unwrapped)
    except (TypeError, ValueError, OverflowError):
        return None
    return num if math.isfinite(num) else None


def is_absent_doy_mapping(value: Any) -> bool:
    """Return True for empty or ref-only mappings representing a disabled DOY."""
    return isinstance(value, Mapping) and (not value or is_ref_only(value))


def _coerce_numeric_method_code(value: Any) -> int | None:
    if isinstance(value, Integral):
        return int(value)
    if isinstance(value, Real):
        try:
            fval = float(value)
        except (TypeError, ValueError, OverflowError):
            return None
        return int(fval) if math.isfinite(fval) and fval.is_integer() else None
    return None


def _coerce_string_method_code(stripped: str, field_name: str | None) -> int | None:
    if not stripped:
        return None
    if field_name is not None:
        try:
            resolved = resolve_scalar_name(field_name, stripped)
            if isinstance(resolved, Integral) and not isinstance(resolved, bool):
                return int(resolved)
        except (TypeError, ValueError):
            pass
    try:
        return int(stripped)
    except ValueError:
        return None


def method_code(value: Any, field_name: str | None = None) -> int | None:
    """Return an integer method code from raw, RefValue, enum, or readable input."""
    value = unwrap_entry(value)
    if hasattr(value, "value"):
        value = value.value
    if value is None:
        return None
    if isinstance(value, bool):
        return int(value)
    if isinstance(value, str):
        return _coerce_string_method_code(value.strip(), field_name)
    return _coerce_numeric_method_code(value)


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
        try:
            net_radiation = self.read("net_radiation", 0)
        except (TypeError, ValueError):
            net_radiation = self.raw.get("net_radiation", self.raw.get("netradiationmethod", 0))
        if isinstance(net_radiation, Mapping):
            unwrapped = unwrap_entry(net_radiation)
            if unwrapped is not net_radiation:
                net_radiation = unwrapped
            elif "spartacus" in net_radiation or net_radiation.get("scheme") == "spartacus":
                return True
        code = method_code(net_radiation, "net_radiation")
        if code is not None:
            return code in SPARTACUS_METHODS
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
        for surface_type, raw_surface in self.land_cover.items():
            surface_props = unwrap_entry(raw_surface)
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
        layers = unwrap_entry(self.vertical_layers.get(layers_key, ()))
        if isinstance(layers, Mapping):
            layers = (layers,)
        if not isinstance(layers, (list, tuple)):
            return []
        found: list[Any] = []
        for raw_layer in layers:
            layer = unwrap_entry(raw_layer)
            if isinstance(layer, Mapping):
                val = get_value_safe(layer, field)
                if val is not None:
                    found.append(val)
        return found

    def facet_layer_raw_values(self, layers_key: str, field: str) -> list[Any] | None:
        """Collect per-layer values of *field* preserving ``None`` entries and indices."""
        layers = unwrap_entry(self.vertical_layers.get(layers_key))
        if layers is None:
            return None
        if isinstance(layers, Mapping):
            layers = (layers,)
        if not isinstance(layers, (list, tuple)):
            return None
        out: list[Any] = []
        for raw_layer in layers:
            layer = unwrap_entry(raw_layer)
            if isinstance(layer, Mapping):
                out.append(get_value_safe(layer, field))
            else:
                out.append(None)
        return out


@dataclass(frozen=True)
class TypedConfigView:
    """Immutable typed view over a full SUEWS YAML configuration mapping."""

    raw: Mapping[str, Any]
    physics: PhysicsView
    sites: tuple[SiteView, ...]
    site_structure_errors: tuple[tuple[int | None, str, str], ...] = ()

    @classmethod
    def from_mapping(cls, yaml_data: Any) -> TypedConfigView:
        """Construct a ``TypedConfigView`` from a raw or frozen config mapping."""
        root = yaml_data if isinstance(yaml_data, Mapping) else {}
        model = root.get("model")
        physics_raw = model.get("physics") if isinstance(model, Mapping) else None
        physics = PhysicsView(physics_raw if isinstance(physics_raw, Mapping) else {})

        site_views: list[SiteView] = []
        structure_errors: list[tuple[int | None, str, str]] = []
        if "sites" in root:
            sites_raw = root["sites"]
            if isinstance(sites_raw, Mapping):
                for idx, (site_key, site_data) in enumerate(sites_raw.items()):
                    if isinstance(site_data, Mapping):
                        site_views.append(
                            SiteView(index=idx, raw=site_data, name=str(site_key))
                        )
                    else:
                        structure_errors.append(
                            (
                                idx,
                                f"sites.{site_key}",
                                f"sites.{site_key} must be a mapping, got {type(site_data).__name__}.",
                            )
                        )
            elif isinstance(sites_raw, (list, tuple)):
                for idx, site_data in enumerate(sites_raw):
                    if isinstance(site_data, Mapping):
                        raw_name = site_data.get("name")
                        name = str(raw_name) if raw_name is not None else "Unknown"
                        site_views.append(SiteView(index=idx, raw=site_data, name=name))
                    else:
                        structure_errors.append(
                            (
                                idx,
                                f"sites[{idx}]",
                                f"sites[{idx}] must be a mapping, got {type(site_data).__name__}.",
                            )
                        )
            else:
                structure_errors.append(
                    (
                        None,
                        "sites",
                        f"sites must be a list or mapping of site definitions, got {type(sites_raw).__name__}.",
                    )
                )

        return cls(
            raw=root,
            physics=physics,
            sites=tuple(site_views),
            site_structure_errors=tuple(structure_errors),
        )

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
