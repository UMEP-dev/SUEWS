"""Typed read-only view over normalised YAML configuration mappings.

Provides a single deeply immutable boundary through which validation rules
inspect raw mappings, :class:`ConfigDocument` instances, or validated
``SUEWSConfig`` models without repeating ad-hoc ``isinstance(..., dict)``,
``MappingProxyType``, or ``RefValue`` boilerplate. All unwrapping delegates to
``pipeline.document`` and ``core.yaml_helpers``, and every exposed container
is frozen via ``deep_freeze``.
"""

from __future__ import annotations

from collections.abc import Mapping
from dataclasses import dataclass
import math
from numbers import Integral, Real
from types import MappingProxyType
from typing import TYPE_CHECKING, Any

from ...core.field_renames import read_physics_key
from ...core.physics_families import resolve_scalar_name
from ..core.yaml_helpers import (
    get_ref_or_bare,
    get_stebbs_block,
    get_stebbsmethod_value,
    get_value_safe,
)
from ..pipeline.document import (
    ConfigDocument,
    deep_freeze,
    is_ref_only,
    read_scalar,
    site_gridid,
    unwrap_entry,
)
from ..required_fields import BUILDING_PRESENT_SFR_TOL
from .hard_invariants import SPARTACUS_METHODS

if TYPE_CHECKING:
    from pydantic import BaseModel

    from .readonly_model import ReadOnlyModelProxy

_EMPTY_MAPPING: Mapping[str, Any] = MappingProxyType({})


def _freeze_mapping(value: Any) -> Mapping[str, Any]:
    """Return a deeply frozen ``MappingProxyType`` for *value*, or an empty mapping."""
    if not isinstance(value, Mapping) or not value:
        return _EMPTY_MAPPING
    return deep_freeze(value)


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
class ControlView:
    """Deeply immutable typed view over ``model.control``."""

    raw: Mapping[str, Any] = _EMPTY_MAPPING

    def __post_init__(self) -> None:
        """Deep-freeze the backing control mapping."""
        object.__setattr__(self, "raw", _freeze_mapping(self.raw))

    @property
    def start_time(self) -> Any:
        """Unwrapped ``model.control.start_time``, or None."""
        return get_value_safe(self.raw, "start_time")

    @property
    def end_time(self) -> Any:
        """Unwrapped ``model.control.end_time``, or None."""
        return get_value_safe(self.raw, "end_time")

    @property
    def tstep(self) -> float | None:
        """Unwrapped ``model.control.tstep`` as float, or None."""
        return as_float(get_value_safe(self.raw, "tstep"))

    @property
    def forcing_file(self) -> Any:
        """Unwrapped ``model.control.forcing.file`` (or legacy ``forcing_file``)."""
        forcing = unwrap_entry(self.raw.get("forcing"))
        if isinstance(forcing, Mapping) and "file" in forcing:
            return unwrap_entry(forcing.get("file"))
        if "forcing_file" in self.raw:
            return unwrap_entry(self.raw.get("forcing_file"))
        return None


@dataclass(frozen=True)
class PhysicsView:
    """Deeply immutable typed view over ``model.physics``."""

    raw: Mapping[str, Any] = _EMPTY_MAPPING

    def __post_init__(self) -> None:
        """Deep-freeze the backing physics mapping."""
        object.__setattr__(self, "raw", _freeze_mapping(self.raw))

    @property
    def is_empty(self) -> bool:
        """Whether the physics mapping is empty."""
        return not self.raw

    @property
    def stebbs_block(self) -> Mapping[str, Any]:
        """Deeply frozen folded ``model.physics.stebbs`` mapping."""
        block = get_stebbs_block(self.raw)
        return _freeze_mapping(block)

    @property
    def stebbs_method(self) -> Any:
        """Composed STEBBS method value (0, 1, 2, or None)."""
        return get_stebbsmethod_value(self.raw)

    @property
    def stebbs_method_int(self) -> int | None:
        """Composed STEBBS method coerced to ``int``, or None."""
        raw_val = self.stebbs_method
        try:
            return int(raw_val)
        except (TypeError, ValueError):
            return None

    @property
    def is_spartacus_enabled(self) -> bool:
        """Whether ``net_radiation`` selects any SPARTACUS scheme."""
        try:
            net_radiation = self.read("net_radiation", 0)
        except (TypeError, ValueError):
            net_radiation = self.raw.get(
                "net_radiation", self.raw.get("netradiationmethod", 0)
            )
        if isinstance(net_radiation, Mapping):
            unwrapped = unwrap_entry(net_radiation)
            if unwrapped is not net_radiation:
                net_radiation = unwrapped
            elif (
                "spartacus" in net_radiation
                or net_radiation.get("scheme") == "spartacus"
            ):
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
class SurfaceView:
    """Deeply immutable typed view over one land-cover surface mapping."""

    name: str
    raw: Mapping[str, Any] = _EMPTY_MAPPING

    def __post_init__(self) -> None:
        """Deep-freeze the backing surface mapping."""
        object.__setattr__(self, "raw", _freeze_mapping(self.raw))

    @property
    def sfr(self) -> float | None:
        """Surface fraction as a finite float, or None."""
        return as_float(self.raw.get("sfr"))

    @property
    def raw_sfr(self) -> Any:
        """Unwrapped raw ``sfr`` entry before float coercion."""
        return get_ref_or_bare(self.raw, "sfr")

    def scalar(self, key: str, default: Any = None) -> Any:
        """Read a rename-aware unwrapped scalar from this surface."""
        return get_value_safe(self.raw, key, default)


@dataclass(frozen=True)
class LandCoverView:
    """Deeply immutable typed view over ``site.properties.land_cover``."""

    raw: Mapping[str, Any] = _EMPTY_MAPPING

    def __post_init__(self) -> None:
        """Deep-freeze the backing land-cover mapping."""
        object.__setattr__(self, "raw", _freeze_mapping(self.raw))

    @property
    def surfaces(self) -> Mapping[str, SurfaceView]:
        """Read-only mapping of surface name to :class:`SurfaceView`."""
        views: dict[str, SurfaceView] = {}
        for name, raw_surface in self.raw.items():
            surface_props = unwrap_entry(raw_surface)
            if isinstance(surface_props, Mapping):
                views[str(name)] = SurfaceView(name=str(name), raw=surface_props)
        return MappingProxyType(views)

    def surface(self, name: str) -> SurfaceView | None:
        """Return the :class:`SurfaceView` for *name*, or None."""
        raw_surface = unwrap_entry(self.raw.get(name))
        if not isinstance(raw_surface, Mapping):
            return None
        return SurfaceView(name=name, raw=raw_surface)

    def sfr(self, name: str) -> float | None:
        """Return the numeric surface fraction for *name*, or None."""
        surf = self.surface(name)
        return surf.sfr if surf is not None else None

    def fraction_map(self) -> Mapping[str, Any]:
        """Return a read-only mapping of non-null ``{surface: raw_sfr}`` pairs."""
        out: dict[str, Any] = {}
        for name, raw_surface in self.raw.items():
            surface_props = unwrap_entry(raw_surface)
            if isinstance(surface_props, Mapping):
                sfr_val = get_ref_or_bare(surface_props, "sfr")
                if sfr_val is not None:
                    out[str(name)] = sfr_val
        return MappingProxyType(out)

    def active_surfaces(self, tol: float = 0.0) -> tuple[str, ...]:
        """Return names of surfaces with numeric ``sfr > tol``."""
        active: list[str] = []
        for name, surf in self.surfaces.items():
            if surf.sfr is not None and surf.sfr > tol:
                active.append(name)
        return tuple(active)


@dataclass(frozen=True)
class VerticalLayersView:
    """Deeply immutable typed view over ``site.properties.vertical_layers``."""

    raw: Mapping[str, Any] = _EMPTY_MAPPING

    def __post_init__(self) -> None:
        """Deep-freeze the backing vertical-layers mapping."""
        object.__setattr__(self, "raw", _freeze_mapping(self.raw))

    @property
    def nlayer(self) -> int | None:
        """Finite integral ``nlayer``, or None for missing/non-integral/infinite values."""
        val = read_scalar(self.raw, "nlayer")
        if val is None or isinstance(val, bool):
            return None
        if isinstance(val, Integral):
            return int(val)
        if isinstance(val, Real):
            try:
                fval = float(val)
            except (TypeError, ValueError, OverflowError):
                return None
            return int(fval) if math.isfinite(fval) and fval.is_integer() else None
        if isinstance(val, str):
            text = val.strip()
            if not text:
                return None
            try:
                return int(text)
            except ValueError:
                try:
                    fval = float(text)
                except (ValueError, OverflowError):
                    return None
                return int(fval) if math.isfinite(fval) and fval.is_integer() else None
        return None

    def float_array(self, key: str) -> tuple[float | None, ...]:
        """Return a frozen tuple of finite floats (or None) for array *key*."""
        arr = unwrap_entry(self.raw.get(key))
        if not isinstance(arr, (list, tuple)):
            return ()
        return tuple(as_float(item) for item in arr)

    @property
    def heights(self) -> tuple[float | None, ...]:
        """Frozen tuple of ``vertical_layers.height`` floats."""
        return self.float_array("height")

    @property
    def building_frac(self) -> tuple[float | None, ...]:
        """Frozen tuple of ``vertical_layers.building_frac`` floats."""
        return self.float_array("building_frac")

    @property
    def veg_frac(self) -> tuple[float | None, ...]:
        """Frozen tuple of ``vertical_layers.veg_frac`` floats."""
        return self.float_array("veg_frac")

    @property
    def last_nonzero_height(self) -> float | None:
        """Last non-zero float in ``vertical_layers.height``."""
        last_nz: float | None = None
        for val in self.heights:
            if val is not None and abs(val) > 0.0:
                last_nz = val
        return last_nz


@dataclass(frozen=True)
class SiteView:
    """Deeply immutable typed view over one entry in ``sites``."""

    index: int
    raw: Mapping[str, Any]
    name: str

    def __post_init__(self) -> None:
        """Deep-freeze the backing site mapping."""
        object.__setattr__(self, "raw", _freeze_mapping(self.raw))

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
        """Deeply frozen site ``properties`` mapping (empty mapping when absent)."""
        props = self.raw.get("properties")
        return props if isinstance(props, Mapping) else _EMPTY_MAPPING

    @property
    def initial_states(self) -> Mapping[str, Any]:
        """Deeply frozen site ``initial_states`` mapping (empty mapping when absent)."""
        states = self.raw.get("initial_states")
        return states if isinstance(states, Mapping) else _EMPTY_MAPPING

    @property
    def land_cover(self) -> Mapping[str, Any]:
        """Deeply frozen site ``properties.land_cover`` mapping."""
        lc = unwrap_entry(self.properties.get("land_cover"))
        return lc if isinstance(lc, Mapping) else _EMPTY_MAPPING

    @property
    def land_cover_view(self) -> LandCoverView:
        """Typed read-only :class:`LandCoverView` for this site."""
        return LandCoverView(self.land_cover)

    @property
    def building_archetype(self) -> Mapping[str, Any]:
        """Deeply frozen site ``properties.building_archetype`` mapping."""
        ba = unwrap_entry(self.properties.get("building_archetype"))
        return ba if isinstance(ba, Mapping) else _EMPTY_MAPPING

    @property
    def stebbs(self) -> Mapping[str, Any]:
        """Deeply frozen site ``properties.stebbs`` mapping."""
        st = unwrap_entry(self.properties.get("stebbs"))
        return st if isinstance(st, Mapping) else _EMPTY_MAPPING

    @property
    def vertical_layers(self) -> Mapping[str, Any]:
        """Deeply frozen site ``properties.vertical_layers`` mapping."""
        vl = unwrap_entry(self.properties.get("vertical_layers"))
        return vl if isinstance(vl, Mapping) else _EMPTY_MAPPING

    @property
    def vertical_layers_view(self) -> VerticalLayersView:
        """Typed read-only :class:`VerticalLayersView` for this site."""
        return VerticalLayersView(self.vertical_layers)

    @property
    def irrigation(self) -> Mapping[str, Any]:
        """Deeply frozen site ``properties.irrigation`` mapping."""
        irr = unwrap_entry(self.properties.get("irrigation"))
        return irr if isinstance(irr, Mapping) else _EMPTY_MAPPING

    @property
    def anthropogenic_emissions(self) -> Mapping[str, Any]:
        """Deeply frozen site ``properties.anthropogenic_emissions`` mapping."""
        anthro = unwrap_entry(self.properties.get("anthropogenic_emissions"))
        return anthro if isinstance(anthro, Mapping) else _EMPTY_MAPPING

    @property
    def lat(self) -> float | None:
        """Site latitude as a finite float, or None."""
        return as_float(self.scalar(self.properties, "lat"))

    @property
    def lng(self) -> float | None:
        """Site longitude as a finite float, or None."""
        return as_float(self.scalar(self.properties, "lng"))

    @property
    def alt(self) -> float | None:
        """Site altitude as a finite float, or None."""
        return as_float(self.scalar(self.properties, "alt"))

    @property
    def timezone(self) -> float | None:
        """Site timezone offset as a finite float, or None."""
        return as_float(self.scalar(self.properties, "timezone"))

    @property
    def z(self) -> float | None:
        """Site forcing measurement height ``z`` as a finite float, or None."""
        return as_float(self.scalar(self.properties, "z"))

    @staticmethod
    def scalar(
        section: Mapping[str, Any], key: str, default: Any = None
    ) -> Any:
        """Read a rename-aware unwrapped scalar from *section*."""
        return get_value_safe(section, key, default)

    @staticmethod
    def exact_scalar(
        section: Mapping[str, Any], key: str, default: Any = None
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
        return self.vertical_layers_view.last_nonzero_height

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


_UNSET_CONTROL: Any = object()


@dataclass(frozen=True)
class TypedConfigView:
    """Deeply immutable typed view over a full SUEWS YAML configuration."""

    raw: Mapping[str, Any]
    physics: PhysicsView
    sites: tuple[SiteView, ...]
    site_structure_errors: tuple[tuple[int | None, str, str], ...] = ()
    control: ControlView = _UNSET_CONTROL

    def __post_init__(self) -> None:
        """Deep-freeze the backing root mapping and derive ``control`` if omitted."""
        frozen_raw = _freeze_mapping(self.raw)
        object.__setattr__(self, "raw", frozen_raw)
        if self.control is _UNSET_CONTROL:
            model = frozen_raw.get("model")
            control_raw = model.get("control") if isinstance(model, Mapping) else None
            object.__setattr__(
                self,
                "control",
                ControlView(
                    control_raw if isinstance(control_raw, Mapping) else _EMPTY_MAPPING
                ),
            )

    @classmethod
    def from_mapping(cls, yaml_data: Any) -> TypedConfigView:
        """Construct a deeply immutable ``TypedConfigView`` from any mapping."""
        root = _freeze_mapping(yaml_data)
        model = root.get("model")
        physics_raw = model.get("physics") if isinstance(model, Mapping) else None
        control_raw = model.get("control") if isinstance(model, Mapping) else None
        physics = PhysicsView(
            physics_raw if isinstance(physics_raw, Mapping) else _EMPTY_MAPPING
        )
        control = ControlView(
            control_raw if isinstance(control_raw, Mapping) else _EMPTY_MAPPING
        )

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
            control=control,
        )

    @classmethod
    def from_document(cls, document: ConfigDocument) -> TypedConfigView:
        """Construct a deeply immutable ``TypedConfigView`` from a :class:`ConfigDocument`."""
        return cls.from_mapping(document.data)

    @classmethod
    def from_config(
        cls, config: BaseModel | ReadOnlyModelProxy
    ) -> TypedConfigView:
        """Construct a deeply immutable ``TypedConfigView`` from a ``SUEWSConfig`` or proxy."""
        dumped = config.model_dump(mode="json", round_trip=True)
        return cls.from_mapping(dumped)

    @property
    def start_time(self) -> Any:
        """Unwrapped ``model.control.start_time``, or None."""
        return self.control.start_time

    @property
    def end_time(self) -> Any:
        """Unwrapped ``model.control.end_time``, or None."""
        return self.control.end_time

    def any_buildings_present(self, tol: float = BUILDING_PRESENT_SFR_TOL) -> bool:
        """Return True if any site has ``land_cover.bldgs.sfr > tol``."""
        return any(site.has_buildings(tol=tol) for site in self.sites)
