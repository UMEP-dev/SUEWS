"""Pure scientific patch planners for Phase B validation.

Each planner inspects a read-only configuration mapping and emits a
:class:`PatchPlan` containing declarative :class:`ConfigPatch` records and
human-readable adjustment metadata. Because planners never mutate the input
mapping:

- ``--science-fixes suggest`` runs once with **zero** ``deepcopy`` calls.
- ``--science-fixes apply`` runs once and applies the exact same patch list in
  a single pass via :class:`ConfigDocument`, preserving bare scalars,
  ``RefValue`` wrappers, and ``ref`` metadata uniformly.
"""

from __future__ import annotations

import calendar
from collections.abc import Callable, Mapping, Sequence
from dataclasses import dataclass, field
from datetime import datetime
import math
from typing import Any

from ..core.yaml_helpers import (
    DLSCheck,
    get_ref_or_bare,
    get_stebbs_block,
    get_stebbsmethod_value,
    get_value_safe,
    resolve_dst_schedule,
)
from ..rules.hard_invariants import (
    check_land_cover_fraction_sum,
    coerce_land_cover_fraction,
)
from .document import (
    ConfigDocument,
    ConfigPatch,
    _prepare_working_dict,
    apply_patches_in_place,
    is_ref_only,
    is_ref_wrapper,
    read_scalar,
    site_gridid,
    unwrap_entry,
)
from .patch_normaliser import (
    _strict_equal,
    collect_nullifiable_values,
    invert_patches,
    normalise_owned_patches,
)

SURFACE_TYPES: tuple[str, ...] = (
    "paved",
    "bldgs",
    "evetr",
    "dectr",
    "grass",
    "bsoil",
    "water",
)

BIOGENIC_CO2_PARAMS: tuple[str, ...] = (
    "alpha_bioco2",
    "alpha_bio_co2",
    "alpha_enh_bioco2",
    "beta_bioco2",
    "beta_bio_co2",
    "beta_enh_bioco2",
    "min_res_bioco2",
    "theta_bioco2",
    "theta_bio_co2",
    "resp_a",
    "resp_b",
)

BIOGENIC_SURFACES: tuple[str, ...] = ("dectr", "evetr", "grass")

WINDOW_PARAMS_STEBBS: tuple[str, ...] = (
    "convection_coefficient_window_internal",
    "convection_coefficient_window_external",
)

WINDOW_PARAMS_ARCHETYPE: tuple[str, ...] = (
    "thickness_window",
    "conductivity_window",
    "density_window",
    "specific_heat_capacity_window",
    "emissivity_window_external",
    "emissivity_window_internal",
    "transmissivity_window_external",
    "absorptivity_window_external",
    "reflectivity_window_external",
)

WALL_PARAMS_STEBBS: tuple[str, ...] = (
    "convection_coefficient_wall_external",
    "convection_coefficient_wall_internal",
)

WALL_PARAMS_ARCHETYPE: tuple[str, ...] = (
    "emissivity_wall_external",
    "emissivity_wall_internal",
    "transmissivity_wall_external",
    "absorptivity_wall_external",
    "reflectivity_wall_external",
    "thickness_wall",
    "conductivity_wall",
    "density_wall",
    "specific_heat_capacity_wall",
)


@dataclass
class PlannedAdjustment:
    """Adjustment metadata emitted alongside declarative :class:`ConfigPatch` items."""

    parameter: str
    site_index: int | None = None
    site_gridid: Any = None
    old_value: Any = None
    new_value: Any = None
    reason: str = ""
    patches: tuple[ConfigPatch, ...] = ()


def _patches_equal(left: ConfigPatch, right: ConfigPatch) -> bool:
    """Compare two :class:`ConfigPatch` instances safely even with NumPy array payloads."""
    return (
        left.path == right.path
        and left.action == right.action
        and left.reason == right.reason
        and left.parameter == right.parameter
        and left.site_index == right.site_index
        and left.site_gridid == right.site_gridid
        and left.source == right.source
        and left.skip_keys == right.skip_keys
        and left.default_wrapped == right.default_wrapped
        and left.had_original == right.had_original
        and left.original_style == right.original_style
        and left.original_ref == right.original_ref
        and left.path_tokens == right.path_tokens
        and left.replace_raw == right.replace_raw
        and left.created_ancestor == right.created_ancestor
        and left.write_style == right.write_style
        and left.write_ref == right.write_ref
        and left.write_has_ref == right.write_has_ref
        and _strict_equal(left.new_value, right.new_value)
        and _strict_equal(left.old_value, right.old_value)
        and _strict_equal(left.original_raw, right.original_raw)
    )


@dataclass
class PatchPlan:
    """Ordered collection of pure patches and their human-readable adjustment records."""

    patches: list[ConfigPatch] = field(default_factory=list)
    adjustments: list[PlannedAdjustment] = field(default_factory=list)
    _bound_patch_count: int = field(default=0, repr=False, compare=False)

    def record(
        self, adjustment: PlannedAdjustment, *patches: ConfigPatch
    ) -> None:
        """Append *patches* (if provided) and bind all pending patches to *adjustment*."""
        if patches:
            self.patches.extend(patches)
        if not adjustment.patches:
            adjustment.patches = tuple(self.patches[self._bound_patch_count :])
        self._bound_patch_count = len(self.patches)
        self.adjustments.append(adjustment)

    def extend(self, other: PatchPlan) -> None:
        """Append patches and adjustments from *other* in order."""
        self.patches.extend(other.patches)
        self.adjustments.extend(other.adjustments)
        self._bound_patch_count = len(self.patches)

    def apply_to_document(self, doc: ConfigDocument) -> ConfigDocument:
        """Return a new :class:`ConfigDocument` with all patches applied once."""
        return doc.apply_patches(self.patches)

    def normalised(
        self,
        target: ConfigDocument | Mapping[str, Any],
        *,
        expand_nullify_tree: bool = True,
        drop_noops: bool = True,
        coalesce: bool = True,
    ) -> PatchPlan:
        """Return a new :class:`PatchPlan` whose patches are normalised against *target*.

        Every patch in the returned plan carries its verified original value
        (``old_value``), ``had_original``, ``original_style``, and frozen
        ``original_ref`` metadata, and each surviving :class:`PlannedAdjustment`
        is reconciled against the surviving normalised patches in the plan.
        """
        owner_by_pos: list[int | None] = [None] * len(self.patches)
        claimed: set[int] = set()
        for adj_idx, adj in enumerate(self.adjustments):
            for adj_patch in adj.patches:
                matched_pos: int | None = None
                for pos, plan_patch in enumerate(self.patches):
                    if pos not in claimed and plan_patch is adj_patch:
                        matched_pos = pos
                        break
                if matched_pos is None:
                    for pos, plan_patch in enumerate(self.patches):
                        if pos not in claimed and _patches_equal(plan_patch, adj_patch):
                            matched_pos = pos
                            break
                if matched_pos is not None:
                    owner_by_pos[matched_pos] = adj_idx
                    claimed.add(matched_pos)

        owned_norm = normalise_owned_patches(
            list(zip(owner_by_pos, self.patches)),
            target,
            expand_nullify_tree=expand_nullify_tree,
            drop_noops=drop_noops,
            coalesce=coalesce,
        )
        norm_patches = [patch for _, patch in owned_norm]
        by_owner: dict[int, list[ConfigPatch]] = {}
        for owner_idx, patch in owned_norm:
            if owner_idx is not None:
                by_owner.setdefault(owner_idx, []).append(patch)

        norm_adjustments: list[PlannedAdjustment] = []
        for adj_idx, adj in enumerate(self.adjustments):
            surviving = by_owner.get(adj_idx)
            if not surviving:
                continue
            single_set_match = (
                len(adj.patches) == 1
                and len(surviving) == 1
                and adj.patches[0].path == surviving[0].path
                and surviving[0].action == "set"
            )
            adj_old = surviving[0].old_value if single_set_match else adj.old_value
            adj_new = (
                surviving[0].new_value
                if single_set_match
                and not _strict_equal(surviving[0].new_value, adj.patches[0].new_value)
                else adj.new_value
            )
            norm_adjustments.append(
                PlannedAdjustment(
                    parameter=adj.parameter,
                    site_index=adj.site_index,
                    site_gridid=adj.site_gridid,
                    old_value=adj_old,
                    new_value=adj_new,
                    reason=adj.reason,
                    patches=tuple(surviving),
                )
            )
        return PatchPlan(
            patches=norm_patches,
            adjustments=norm_adjustments,
            _bound_patch_count=len(norm_patches),
        )

    def invert(self) -> list[ConfigPatch]:
        """Return the inverse patch list that undoes this normalised plan."""
        return invert_patches(self.patches)

    def apply_to_dict(
        self, yaml_data: Mapping[str, Any], *, in_place: bool = False
    ) -> dict[str, Any]:
        """Apply all patches to *yaml_data* (returning a single copy unless ``in_place``)."""
        if not in_place:
            copied = _prepare_working_dict(yaml_data)
            apply_patches_in_place(copied, self.patches)
            return copied
        assert isinstance(yaml_data, dict)
        apply_patches_in_place(yaml_data, self.patches)
        return yaml_data


def _collect_ref_wrapper_paths(
    cur: Mapping[str, Any],
    cur_path: str,
    *,
    empty_values: tuple[Any, ...],
    recurse: Callable[[Any, str], None],
    paths: list[str],
) -> None:
    inner = cur.get("value")
    if isinstance(inner, (list, tuple)):
        if any(isinstance(item, (Mapping, list, tuple)) for item in inner):
            recurse(inner, cur_path)
        elif any(item not in empty_values for item in inner):
            paths.append(cur_path)
    elif isinstance(inner, Mapping):
        recurse(inner, cur_path)
    elif inner not in empty_values:
        paths.append(cur_path)


def collect_nullifiable_paths(
    node: Any,
    *,
    prefix: str = "",
    skip_keys: frozenset[str] = frozenset(),
    skip_at_root_only: bool = True,
) -> list[str]:
    """Return paths of non-null leaves that ``nullify_tree_in_place`` would clear.

    Pure read-only traversal (zero deep copies); skips ref-only placeholders
    and sibling ``"ref"`` metadata blocks.
    """
    paths: list[str] = []

    def _walk(cur: Any, cur_path: str, is_root: bool) -> None:
        if isinstance(cur, Mapping):
            if is_ref_only(cur):
                return
            if is_ref_wrapper(cur):
                _collect_ref_wrapper_paths(
                    cur,
                    cur_path,
                    empty_values=(None,),
                    recurse=lambda sub, p: _walk(sub, p, False),
                    paths=paths,
                )
                return
            for key, val in cur.items():
                if key == "ref" or (key in skip_keys and (is_root or not skip_at_root_only)):
                    continue
                child_path = f"{cur_path}.{key}" if cur_path else str(key)
                if isinstance(val, (Mapping, list, tuple)):
                    _walk(val, child_path, False)
                elif val is not None:
                    paths.append(child_path)
        elif isinstance(cur, (list, tuple)):
            for idx, item in enumerate(cur):
                child_path = f"{cur_path}[{idx}]" if cur_path else f"[{idx}]"
                if isinstance(item, (Mapping, list, tuple)):
                    _walk(item, child_path, False)
                elif item is not None:
                    paths.append(child_path)

    _walk(node, prefix, True)
    return paths


def _collect_profile_scalar_paths(
    cur: Mapping[str, Any], cur_path: str
) -> list[str]:
    """Return profile field paths whose values are non-empty non-container scalars."""
    found: list[str] = []
    for profile_key in ("working_day", "holiday"):
        val = cur.get(profile_key)
        if not isinstance(val, (Mapping, list, tuple)) and val not in (None, ""):
            found.append(f"{cur_path}.{profile_key}" if cur_path else profile_key)
    return found


def _collect_co2_nullifiable_paths(node: Any, prefix: str = "") -> list[str]:
    """Pure read-only twin of ``collect_nullified_paths(before, nullify_co2(before))``."""
    paths: list[str] = []

    def _walk(cur: Any, cur_path: str) -> None:
        if isinstance(cur, Mapping):
            if is_ref_only(cur):
                return
            if is_ref_wrapper(cur):
                _collect_ref_wrapper_paths(
                    cur,
                    cur_path,
                    empty_values=(None, ""),
                    recurse=_walk,
                    paths=paths,
                )
                return
            if "working_day" in cur or "holiday" in cur:
                paths.extend(_collect_profile_scalar_paths(cur, cur_path))
                return
            for key, val in cur.items():
                if key == "ref":
                    continue
                child_path = f"{cur_path}.{key}" if cur_path else str(key)
                if isinstance(val, (Mapping, list, tuple)):
                    _walk(val, child_path)
                elif val not in (None, ""):
                    paths.append(child_path)
        elif isinstance(cur, (list, tuple)):
            for idx, item in enumerate(cur):
                child_path = f"{cur_path}[{idx}]" if cur_path else f"[{idx}]"
                if isinstance(item, (Mapping, list, tuple)):
                    _walk(item, child_path)
                elif item not in (None, ""):
                    paths.append(child_path)

    _walk(node, prefix)
    return paths


def get_season_from_doy(doy: int, lat: float) -> str:
    """Determine season string from day-of-year and latitude."""
    abs_lat = abs(lat)
    if abs_lat <= 10:
        return "equatorial"
    if 10 < abs_lat < 23.5:
        return "tropical"

    if lat >= 0:
        if 150 < doy < 250:
            season = "summer"
        elif 60 < doy <= 150:
            season = "spring"
        elif 250 <= doy < 335:
            season = "fall"
        else:
            season = "winter"
    elif 150 < doy < 250:
        season = "winter"
    elif 60 < doy <= 150:
        season = "fall"
    elif 250 <= doy < 335:
        season = "spring"
    else:
        season = "summer"
    return season


def get_season(start_date: str, lat: float) -> str:
    """Determine season string from ``YYYY-MM-DD`` start date and latitude."""
    try:
        start_doy = datetime.strptime(start_date, "%Y-%m-%d").timetuple().tm_yday
    except ValueError as exc:
        raise ValueError("start_date must be in YYYY-MM-DD format") from exc
    return get_season_from_doy(start_doy, lat)


def _plan_element_temperature_patches(
    element: Mapping[str, Any],
    base_path: str,
    avg_temp: float,
) -> tuple[list[ConfigPatch], list[str]]:
    patches: list[ConfigPatch] = []
    updated_params: list[str] = []

    if "temperature" in element:
        current_temp = get_ref_or_bare(element, "temperature")
        if current_temp != [avg_temp] * 5:
            patches.append(
                ConfigPatch(
                    path=f"{base_path}.temperature",
                    action="set",
                    new_value=[avg_temp] * 5,
                    old_value=current_temp,
                )
            )
            updated_params.append("temperature")

    if "tsfc" in element:
        current_tsfc = get_ref_or_bare(element, "tsfc")
        if current_tsfc != avg_temp:
            patches.append(
                ConfigPatch(
                    path=f"{base_path}.tsfc",
                    action="set",
                    new_value=avg_temp,
                    old_value=current_tsfc,
                )
            )
            updated_params.append("tsfc")

    if "tin" in element:
        current_tin = get_ref_or_bare(element, "tin")
        if current_tin != avg_temp:
            patches.append(
                ConfigPatch(
                    path=f"{base_path}.tin",
                    action="set",
                    new_value=avg_temp,
                    old_value=current_tin,
                )
            )
            updated_params.append("tin")

    return patches, updated_params


def _plan_stebbs_temperature_patches(
    plan: PatchPlan,
    site_idx: int,
    gid: Any,
    stebbs: Mapping[str, Any],
    lat: float,
    lng: float,
    month: int,
    avg_temp: float,
    diffmax_val: float | None,
    annual_temp: float | None,
) -> None:
    """Append STEBBS initial and climatological temperature patches for one site."""
    for key in (
        "temperature_air_outdoor_initial",
        "temperature_air_indoor_initial",
    ):
        if key not in stebbs:
            continue
        old_val = get_ref_or_bare(stebbs, key)
        if old_val != avg_temp:
            plan.patches.append(
                ConfigPatch(
                    path=f"sites[{site_idx}].properties.stebbs.{key}",
                    action="set",
                    new_value=avg_temp,
                    old_value=old_val,
                )
            )
            plan.record(
                PlannedAdjustment(
                    parameter=f"stebbs.{key}",
                    site_index=site_idx,
                    site_gridid=gid,
                    old_value=str(old_val),
                    new_value=f"{avg_temp} C",
                    reason=(
                        f"Set from CRU data for coordinates "
                        f"({lat:.2f}, {lng:.2f}) for month {month}"
                    ),
                )
            )

    if diffmax_val is not None and "temperature_air_month_mean_diffmax" in stebbs:
        key = "temperature_air_month_mean_diffmax"
        old_val = get_ref_or_bare(stebbs, key)
        if old_val != diffmax_val:
            plan.patches.append(
                ConfigPatch(
                    path=f"sites[{site_idx}].properties.stebbs.{key}",
                    action="set",
                    new_value=diffmax_val,
                    old_value=old_val,
                )
            )
            plan.record(
                PlannedAdjustment(
                    parameter=f"stebbs.{key}",
                    site_index=site_idx,
                    site_gridid=gid,
                    old_value=str(old_val),
                    new_value=f"{diffmax_val} C",
                    reason=f"Set from CRU data for coordinates ({lat:.2f}, {lng:.2f})",
                )
            )

    if annual_temp is not None and "temperature_air_annual_mean" in stebbs:
        old_annual_val = get_ref_or_bare(stebbs, "temperature_air_annual_mean")
        if old_annual_val != annual_temp:
            plan.patches.append(
                ConfigPatch(
                    path=f"sites[{site_idx}].properties.stebbs.temperature_air_annual_mean",
                    action="set",
                    new_value=annual_temp,
                    old_value=old_annual_val,
                )
            )
            plan.record(
                PlannedAdjustment(
                    parameter="stebbs.temperature_air_annual_mean",
                    site_index=site_idx,
                    site_gridid=gid,
                    old_value=str(old_annual_val),
                    new_value=f"{annual_temp} C",
                    reason=(
                        "Set from CRU annual mean (1991-2020 normals) for "
                        f"coordinates ({lat:.2f}, {lng:.2f})"
                    ),
                )
            )


def _plan_roof_wall_temperature_patches(
    plan: PatchPlan,
    site_idx: int,
    gid: Any,
    initial_states: Mapping[str, Any],
    lat: float,
    lng: float,
    month: int,
    avg_temp: float,
) -> None:
    """Append roof/wall vertical-layer temperature patches for one site."""
    for array_name in ("roofs", "walls"):
        array = initial_states.get(array_name)
        if not isinstance(array, (list, tuple)):
            continue
        for element_idx, element in enumerate(array):
            if not isinstance(element, Mapping):
                continue
            elem_patches, updated_params = _plan_element_temperature_patches(
                element,
                f"sites[{site_idx}].initial_states.{array_name}[{element_idx}]",
                avg_temp,
            )
            if updated_params:
                plan.patches.extend(elem_patches)
                plan.record(
                    PlannedAdjustment(
                        parameter=f"initial_states.{array_name}[{element_idx}]",
                        site_index=site_idx,
                        site_gridid=gid,
                        old_value=", ".join(updated_params),
                        new_value=f"{avg_temp} C",
                        reason=(
                            f"Set from CRU data for coordinates "
                            f"({lat:.2f}, {lng:.2f}) for month {month}"
                        ),
                    )
                )


def _iter_sites(yaml_data: Mapping[str, Any]) -> Sequence[Any]:
    """Return ``sites`` as a sequence, or ``()`` when absent, null, or non-sequence."""
    sites = yaml_data.get("sites")
    if isinstance(sites, (list, tuple)):
        return sites
    return ()


def _read_physics_mapping(yaml_data: Mapping[str, Any]) -> Mapping[str, Any]:
    """Return ``model.physics`` as a mapping, or ``{}`` when absent/malformed."""
    model = yaml_data.get("model")
    if not isinstance(model, Mapping):
        return {}
    physics = model.get("physics")
    return physics if isinstance(physics, Mapping) else {}


def plan_surface_temperature_patches(
    yaml_data: Mapping[str, Any],
    start_date: str,
    *,
    get_monthly_temp: Callable[[float, float, int], float | None],
    get_diffmax_temp: Callable[[float, float], float | None],
    get_annual_temp: Callable[[float, float], float | None],
) -> PatchPlan:
    """Plan climatological initial-temperature patches without mutating *yaml_data*."""
    plan = PatchPlan()
    month = datetime.strptime(start_date, "%Y-%m-%d").month
    physics = _read_physics_mapping(yaml_data)
    stebbs_enabled = get_stebbsmethod_value(physics) != 0

    for site_idx, site in enumerate(_iter_sites(yaml_data)):
        if not isinstance(site, Mapping):
            continue
        props = site.get("properties") or {}
        initial_states = site.get("initial_states") or {}
        stebbs = props.get("stebbs") if isinstance(props, Mapping) else None
        gid = site_gridid(site)

        lat = read_scalar(props, "lat")
        lng = read_scalar(props, "lng")
        if lat is None or lng is None:
            continue

        avg_temp = get_monthly_temp(lat, lng, month)
        if avg_temp is None:
            continue

        for surface_type in SURFACE_TYPES:
            surf = initial_states.get(surface_type)
            if not isinstance(surf, Mapping):
                continue
            elem_patches, updated_params = _plan_element_temperature_patches(
                surf,
                f"sites[{site_idx}].initial_states.{surface_type}",
                avg_temp,
            )
            if updated_params:
                plan.patches.extend(elem_patches)
                plan.record(
                    PlannedAdjustment(
                        parameter=f"initial_states.{surface_type}",
                        site_index=site_idx,
                        site_gridid=gid,
                        old_value=", ".join(updated_params),
                        new_value=f"{avg_temp} C",
                        reason=(
                            f"Set from CRU data for coordinates "
                            f"({lat:.2f}, {lng:.2f}) for month {month}"
                        ),
                    )
                )

        if stebbs_enabled and isinstance(stebbs, Mapping):
            diffmax_val = (
                get_diffmax_temp(lat, lng)
                if "temperature_air_month_mean_diffmax" in stebbs
                else None
            )
            annual_temp = (
                get_annual_temp(lat, lng)
                if "temperature_air_annual_mean" in stebbs
                else None
            )
            _plan_stebbs_temperature_patches(
                plan,
                site_idx,
                gid,
                stebbs,
                lat,
                lng,
                month,
                avg_temp,
                diffmax_val,
                annual_temp,
            )

        _plan_roof_wall_temperature_patches(
            plan, site_idx, gid, initial_states, lat, lng, month, avg_temp
        )

    return plan


def plan_land_cover_fraction_patches(yaml_data: Mapping[str, Any]) -> PatchPlan:
    """Plan auto-correction patches for small floating-point ``sfr`` sum drift."""
    plan = PatchPlan()

    for site_idx, site in enumerate(_iter_sites(yaml_data)):
        if not isinstance(site, Mapping):
            continue
        props = site.get("properties") or {}
        land_cover = props.get("land_cover") if isinstance(props, Mapping) else None
        if not isinstance(land_cover, Mapping) or not land_cover:
            continue
        gid = site_gridid(site)

        surface_fractions: dict[str, Any] = {}
        for surface_type, raw_surface in land_cover.items():
            surface_props = unwrap_entry(raw_surface)
            if isinstance(surface_props, Mapping):
                sfr_value = get_ref_or_bare(surface_props, "sfr")
                if sfr_value is not None:
                    coerced = coerce_land_cover_fraction(sfr_value)
                    surface_fractions[surface_type] = (
                        coerced if coerced is not None else sfr_value
                    )

        if not surface_fractions:
            continue

        sum_check = check_land_cover_fraction_sum(surface_fractions)
        if (
            sum_check.band != "auto_fix"
            or sum_check.max_surface is None
            or sum_check.correction is None
        ):
            continue

        max_surface = sum_check.max_surface
        raw_old_sfr = get_ref_or_bare(unwrap_entry(land_cover[max_surface]), "sfr")
        old_value = float(surface_fractions[max_surface])
        new_value = old_value + sum_check.correction

        plan.patches.append(
            ConfigPatch(
                path=f"sites[{site_idx}].properties.land_cover.{max_surface}.sfr",
                action="set",
                new_value=new_value,
                old_value=raw_old_sfr if raw_old_sfr is not None else old_value,
                had_original=True,
                source="land_cover_fraction",
            )
        )
        plan.record(
            PlannedAdjustment(
                parameter=f"{max_surface}.sfr",
                site_index=site_idx,
                site_gridid=gid,
                old_value=f"{old_value:.6f}",
                new_value=f"{new_value:.6f}",
                reason=(
                    f"Auto-corrected {max_surface}.sfr to have sum from "
                    f"{sum_check.total:.6f} to 1.0 (small floating point error)"
                ),
            )
        )

    return plan


def _plan_disabled_stebbs_patches(
    plan: PatchPlan,
    sites: Any,
) -> None:
    """Append nullification patches for disabled STEBBS blocks across sites."""
    from ...core.site import ArchetypeProperties  # noqa: PLC0415

    bridge_only = frozenset(ArchetypeProperties._BRIDGE_ONLY_FIELDS)
    for site_idx, site in enumerate(sites):
        if not isinstance(site, Mapping):
            continue
        props = site.get("properties") or {}
        gid = site_gridid(site)
        for block_name in ("stebbs", "building_archetype"):
            block = props.get(block_name)
            if not isinstance(block, (Mapping, list, tuple)) or not block:
                continue
            skip_keys = bridge_only if block_name == "building_archetype" else frozenset()
            nullifiable = collect_nullifiable_paths(
                block,
                prefix=block_name,
                skip_keys=skip_keys,
                skip_at_root_only=True,
            )
            if not nullifiable:
                continue
            orig_values = collect_nullifiable_values(
                block,
                prefix="",
                skip_keys=skip_keys,
                skip_at_root_only=True,
            )
            plan.patches.append(
                ConfigPatch(
                    path=f"sites[{site_idx}].properties.{block_name}",
                    action="nullify_tree",
                    old_value=orig_values,
                    skip_keys=tuple(skip_keys),
                    had_original=True,
                    source="disabled_stebbs",
                )
            )
            param_list = ", ".join(nullifiable)
            plan.record(
                PlannedAdjustment(
                    parameter=block_name,
                    site_index=site_idx,
                    site_gridid=gid,
                    old_value=(
                        f"stebbsmethod is switched off, nullified "
                        f"{len(nullifiable)} related parameters - {param_list}"
                    ),
                    new_value="null",
                    reason=(
                        f"stebbsmethod switched off, nullified "
                        f"{len(nullifiable)} related parameters"
                    ),
                )
            )


def _plan_site_disabled_biogenic_patches(
    plan: PatchPlan,
    site_idx: int,
    gid: Any,
    land_cover: Mapping[str, Any],
) -> None:
    """Append nullification patches for populated biogenic CO2 fields on one site."""
    biogenic_patches: list[ConfigPatch] = []
    biogenic_surfaces_hit: list[str] = []
    for surface in BIOGENIC_SURFACES:
        surface_props = land_cover.get(surface)
        if not isinstance(surface_props, Mapping):
            continue
        surface_hit = False
        for param in BIOGENIC_CO2_PARAMS:
            current = get_ref_or_bare(surface_props, param)
            if current is None:
                continue
            if isinstance(current, (list, tuple)):
                if all(item is None for item in current):
                    continue
                cleared: Any = [None] * len(current)
            else:
                cleared = None
            surface_hit = True
            biogenic_patches.append(
                ConfigPatch(
                    path=f"sites[{site_idx}].properties.land_cover.{surface}.{param}",
                    action="set",
                    new_value=cleared,
                    old_value=current,
                )
            )
        if surface_hit:
            biogenic_surfaces_hit.append(surface)
    if not biogenic_patches:
        return
    plan.patches.extend(biogenic_patches)
    if biogenic_surfaces_hit == ["dectr"]:
        bio_param = "land_cover.dectr.biogenic_params"
        bio_old = "biogenic dectr params present"
        bio_reason = (
            "emissionsmethod 0..6 (CO2 disabled) - nullified "
            "dectr biogenic parameters"
        )
    else:
        surf_label = ", ".join(biogenic_surfaces_hit)
        bio_param = f"land_cover.{biogenic_surfaces_hit[0]}.biogenic_params"
        bio_old = f"biogenic params present on {surf_label}"
        bio_reason = (
            "emissionsmethod 0..6 (CO2 disabled) - nullified "
            f"biogenic parameters on {surf_label}"
        )
    plan.record(
        PlannedAdjustment(
            parameter=bio_param,
            site_index=site_idx,
            site_gridid=gid,
            old_value=bio_old,
            new_value="null",
            reason=bio_reason,
        )
    )


def _plan_disabled_co2_patches(
    plan: PatchPlan,
    sites: Any,
) -> None:
    """Append nullification patches for disabled CO2 and biogenic blocks."""
    for site_idx, site in enumerate(sites):
        if not isinstance(site, Mapping):
            continue
        props = site.get("properties") or {}
        gid = site_gridid(site)
        anth_emis = props.get("anthropogenic_emissions") or {}
        co2_block = anth_emis.get("co2") if isinstance(anth_emis, Mapping) else None
        if isinstance(co2_block, Mapping) and co2_block:
            nullifiable = _collect_co2_nullifiable_paths(co2_block)
            if not nullifiable:
                nullifiable = collect_nullifiable_paths(co2_block)
            if nullifiable:
                orig_values = collect_nullifiable_values(co2_block, prefix="")
                plan.patches.append(
                    ConfigPatch(
                        path=f"sites[{site_idx}].properties.anthropogenic_emissions.co2",
                        action="nullify_tree",
                        old_value=orig_values,
                        had_original=True,
                        source="disabled_co2",
                    )
                )
                param_list = ", ".join(nullifiable)
                plan.record(
                    PlannedAdjustment(
                        parameter="anthropogenic_emissions.co2",
                        site_index=site_idx,
                        site_gridid=gid,
                        old_value=(
                            f"emissionsmethod 0..6 (CO2 disabled), nullified "
                            f"{len(nullifiable)} related parameters - {param_list}"
                        ),
                        new_value="null",
                        reason=(
                            f"emissionsmethod 0..6 (CO2 disabled), nullified "
                            f"{len(nullifiable)} related parameters"
                        ),
                    )
                )

        land_cover = props.get("land_cover") or {}
        if isinstance(land_cover, Mapping):
            _plan_site_disabled_biogenic_patches(plan, site_idx, gid, land_cover)


def plan_model_dependent_nullification_patches(
    yaml_data: Mapping[str, Any],
) -> PatchPlan:
    """Plan nullification patches for disabled STEBBS and CO2 options without copying."""
    plan = PatchPlan()
    physics = _read_physics_mapping(yaml_data)
    sites = _iter_sites(yaml_data)

    if get_stebbsmethod_value(physics) == 0:
        _plan_disabled_stebbs_patches(plan, sites)

    emissionsmethod = get_value_safe(physics, "emissions")
    if emissionsmethod is not None and emissionsmethod in (0, 1, 2, 3, 4, 5, 6):
        _plan_disabled_co2_patches(plan, sites)

    return plan


def _classify_doy_entry(
    section: Mapping[str, Any],
    key: str,
    max_dls_doy: int,
    coerce_doy: Callable[[Any, str], int],
) -> tuple[str, int | None]:
    raw = section.get(key)
    if raw is None:
        return "absent", None
    if isinstance(raw, Mapping):
        if not (set(raw.keys()) <= {"value", "ref"}):
            return "invalid", None
        raw = raw.get("value")
        if raw is None:
            return "absent", None
    try:
        val = coerce_doy(raw, key)
    except ValueError:
        return "invalid", None
    return ("valid", val) if 1 <= val <= max_dls_doy else ("invalid", None)


def _plan_site_seasonal_lai_patches(
    plan: PatchPlan,
    site_idx: int,
    gid: Any,
    land_cover: Any,
    initial_states: Mapping[str, Any],
    season: str,
) -> None:
    """Append seasonal deciduous-tree LAI patch for one site."""
    dectr = land_cover.get("dectr") if isinstance(land_cover, Mapping) else None
    if not isinstance(dectr, Mapping) or not dectr:
        return
    sfr = coerce_land_cover_fraction(get_ref_or_bare(dectr, "sfr", 0))
    if sfr is None or sfr <= 0:
        return
    lai = dectr.get("lai") or {}
    laimin = get_value_safe(lai, "lai_min")
    if laimin is None:
        laimin = get_value_safe(lai, "laimin")
    laimax = get_value_safe(lai, "lai_max")
    if laimax is None:
        laimax = get_value_safe(lai, "laimax")
    if laimin is None or laimax is None:
        return

    if season == "winter":
        lai_val = laimin
    elif season in ("spring", "fall"):
        lai_val = (laimax + laimin) / 2
    else:
        lai_val = laimax

    dectr_state = initial_states.get("dectr")
    current_lai = (
        get_value_safe(dectr_state, "lai_id")
        if isinstance(dectr_state, Mapping)
        else None
    )
    if current_lai != lai_val:
        plan.patches.append(
            ConfigPatch(
                path=f"sites[{site_idx}].initial_states.dectr.lai_id",
                action="set",
                new_value=lai_val,
                old_value=current_lai,
            )
        )
        plan.record(
            PlannedAdjustment(
                parameter="dectr.lai_id",
                site_index=site_idx,
                site_gridid=gid,
                old_value=str(current_lai) if current_lai is not None else "undefined",
                new_value=str(lai_val),
                reason=(
                    f"Set seasonal LAI for {season} "
                    f"(laimin={laimin}, laimax={laimax})"
                ),
            )
        )


def _plan_site_seasonal_albedo_patches(
    plan: PatchPlan,
    site_idx: int,
    gid: Any,
    land_cover: Any,
    initial_states: Mapping[str, Any],
    season: str,
) -> None:
    """Append seasonal vegetation albedo patches for one site."""
    if not isinstance(land_cover, Mapping):
        return
    for surf_key, label in (
        ("grass", "grass"),
        ("dectr", "deciduous trees"),
        ("evetr", "evergreen trees"),
    ):
        surf_props = land_cover.get(surf_key)
        if not isinstance(surf_props, Mapping):
            continue
        sfr_val = coerce_land_cover_fraction(get_ref_or_bare(surf_props, "sfr", 0))
        if sfr_val is None or sfr_val <= 0:
            continue
        surf_state = initial_states.get(surf_key, {})
        if not isinstance(surf_state, Mapping):
            continue
        alb_min = get_value_safe(surf_props, "alb_min")
        alb_max = get_value_safe(surf_props, "alb_max")
        alb_id_val = get_value_safe(surf_state, "alb_id")
        if alb_min is None or alb_max is None:
            continue
        if season in ("summer", "tropical", "equatorial"):
            target = alb_min if surf_key == "grass" else alb_max
        elif season == "winter":
            target = alb_max if surf_key == "grass" else alb_min
        else:
            if alb_id_val is None:
                continue
            target = 0.5 * (alb_min + alb_max)
        if alb_id_val is not None and math.isclose(alb_id_val, target):
            continue
        plan.patches.append(
            ConfigPatch(
                path=f"sites[{site_idx}].initial_states.{surf_key}.alb_id",
                action="set",
                new_value=target,
                old_value=alb_id_val,
            )
        )
        plan.record(
            PlannedAdjustment(
                parameter=f"{surf_key}.alb_id",
                site_index=site_idx,
                site_gridid=gid,
                old_value=str(alb_id_val),
                new_value=str(target),
                reason=(
                    f"Set seasonal albedo for {season} on {label} "
                    "based on (alb_min, alb_max)"
                ),
            )
        )


def _plan_site_dls_and_tz_patches(
    plan: PatchPlan,
    site_idx: int,
    gid: Any,
    props: Mapping[str, Any],
    lat: float,
    lng: float,
    model_year: int,
    dls_cls: type,
) -> None:
    """Append DST transition and timezone offset patches for one site."""
    dls = dls_cls(lat=lat, lng=lng, year=model_year)
    schedule = resolve_dst_schedule(dls)
    start_dls = schedule.start
    end_dls = schedule.end
    tz_offset = schedule.utc_offset_hours

    anthro_emissions = props.get("anthropogenic_emissions")
    if (
        isinstance(anthro_emissions, Mapping)
        and anthro_emissions
        and schedule.scan_complete
        and not schedule.has_multiple_periods
        and not schedule.has_non_hourly_dst
        and (start_dls is not None or end_dls is not None)
    ):
        from ...core.human_activity import coerce_integer_doy_scalar  # noqa: PLC0415

        max_dls_doy = (
            366 if (model_year is None or calendar.isleap(model_year)) else 365
        )
        s_state, current_startdls = _classify_doy_entry(
            anthro_emissions, "startdls", max_dls_doy, coerce_integer_doy_scalar
        )
        e_state, current_enddls = _classify_doy_entry(
            anthro_emissions, "enddls", max_dls_doy, coerce_integer_doy_scalar
        )
        if s_state == "valid" and e_state == "absent" and end_dls is not None:
            dls_pairs: tuple[tuple[str, Any, Any, str], ...] = (
                ("enddls", current_enddls, end_dls, "end"),
            )
        elif s_state == "absent" and e_state == "valid" and start_dls is not None:
            dls_pairs = (("startdls", current_startdls, start_dls, "start"),)
        elif (
            s_state == "valid"
            and e_state == "valid"
            and start_dls is not None
            and end_dls is not None
        ):
            # Full-pair overwrite behaviour is retained here and deferred.
            dls_pairs = (
                ("startdls", current_startdls, start_dls, "start"),
                ("enddls", current_enddls, end_dls, "end"),
            )
        else:
            dls_pairs = ()

        for key, current_val, new_val, label in dls_pairs:
            if current_val != new_val:
                plan.patches.append(
                    ConfigPatch(
                        path=f"sites[{site_idx}].properties.anthropogenic_emissions.{key}",
                        action="set",
                        new_value=new_val,
                        old_value=current_val,
                    )
                )
                plan.record(
                    PlannedAdjustment(
                        parameter=f"anthropogenic_emissions.{key}",
                        site_index=site_idx,
                        site_gridid=gid,
                        old_value=str(current_val),
                        new_value=str(new_val),
                        reason=(
                            f"Calculated DLS {label} for coordinates "
                            f"({lat:.2f}, {lng:.2f})"
                        ),
                    )
                )

    if tz_offset is not None:
        current_timezone = get_ref_or_bare(props, "timezone")
        if current_timezone != tz_offset:
            plan.patches.append(
                ConfigPatch(
                    path=f"sites[{site_idx}].properties.timezone",
                    action="set",
                    new_value=tz_offset,
                    old_value=current_timezone,
                )
            )
            plan.record(
                PlannedAdjustment(
                    parameter="timezone",
                    site_index=site_idx,
                    site_gridid=gid,
                    old_value=str(current_timezone),
                    new_value=str(tz_offset),
                    reason=(
                        f"Calculated timezone offset for coordinates "
                        f"({lat:.2f}, {lng:.2f})"
                    ),
                )
            )


def plan_seasonal_patches(
    yaml_data: Mapping[str, Any],
    start_date: str,
    model_year: int,
    *,
    dls_cls: type = DLSCheck,
) -> PatchPlan:
    """Plan seasonal LAI, snowalb, vegetation albedo, DLS, and timezone patches."""
    plan = PatchPlan()
    # Validate start_date once up front so malformed dates surface as errors
    # rather than silently skipping every site.
    datetime.strptime(start_date, "%Y-%m-%d")

    for site_idx, site in enumerate(_iter_sites(yaml_data)):
        if not isinstance(site, Mapping):
            continue
        props = site.get("properties") or {}
        initial_states = site.get("initial_states") or {}
        gid = site_gridid(site)

        lat = read_scalar(props, "lat")
        lng = get_value_safe(props, "lng")
        if lat is None:
            continue

        season = get_season(start_date, lat)

        if season in ("summer", "tropical", "equatorial") and "snowalb" in initial_states:
            current_snowalb = read_scalar(initial_states, "snowalb")
            plan.patches.append(
                ConfigPatch(
                    path=f"sites[{site_idx}].initial_states.snowalb",
                    action="delete",
                    old_value=current_snowalb,
                )
            )
            plan.record(
                PlannedAdjustment(
                    parameter="snowalb",
                    site_index=site_idx,
                    site_gridid=gid,
                    old_value=str(current_snowalb),
                    new_value="omitted (model default applies)",
                    reason=f"Removed for {season} season (no snow expected)",
                )
            )

        land_cover = props.get("land_cover") or {}
        _plan_site_seasonal_lai_patches(
            plan, site_idx, gid, land_cover, initial_states, season
        )
        _plan_site_seasonal_albedo_patches(
            plan, site_idx, gid, land_cover, initial_states, season
        )
        if lng is not None:
            _plan_site_dls_and_tz_patches(
                plan, site_idx, gid, props, lat, lng, model_year, dls_cls
            )

    return plan


def plan_rcmethod_patches(yaml_data: Mapping[str, Any]) -> PatchPlan:
    """Plan outer heat-capacity fraction patches when ``capacitance == 0`` and STEBBS is on."""
    plan = PatchPlan()
    physics = _read_physics_mapping(yaml_data)
    rcmethod_value = get_value_safe(get_stebbs_block(physics), "capacitance")

    if rcmethod_value == 0 and get_stebbsmethod_value(physics) != 0:
        for site_idx, site in enumerate(_iter_sites(yaml_data)):
            if not isinstance(site, Mapping):
                continue
            props = site.get("properties") or {}
            building_archetype = props.get("building_archetype") or {}
            if not isinstance(building_archetype, Mapping):
                continue
            gid = site_gridid(site)

            for key in (
                "fraction_heat_capacity_roof_external",
                "fraction_heat_capacity_wall_external",
            ):
                old_frac = get_ref_or_bare(building_archetype, key)
                if old_frac != 0.5:
                    plan.patches.append(
                        ConfigPatch(
                            path=f"sites[{site_idx}].properties.building_archetype.{key}",
                            action="set",
                            new_value=0.5,
                            old_value=old_frac,
                        )
                    )
                    plan.record(
                        PlannedAdjustment(
                            parameter=f"building_archetype.{key}",
                            site_index=site_idx,
                            site_gridid=gid,
                            old_value=str(old_frac),
                            new_value="0.5",
                            reason=f"capacitance == 0, set {key} to 0.5",
                        )
                    )

    return plan


def plan_setpointmethod_patches(
    yaml_data: Mapping[str, Any], *, skip_when_stebbs_off: bool = False
) -> PatchPlan:
    """Plan setpoint scalar/profile cleanup patches based on ``stebbs.setpoint``."""
    plan = PatchPlan()
    physics = _read_physics_mapping(yaml_data)
    if skip_when_stebbs_off and get_stebbsmethod_value(physics) == 0:
        return plan

    setpointmethod = get_value_safe(get_stebbs_block(physics), "setpoint")
    for site_idx, site in enumerate(_iter_sites(yaml_data)):
        if not isinstance(site, Mapping):
            continue
        props = site.get("properties") or {}
        building_archetype = props.get("building_archetype") or {}
        if not isinstance(building_archetype, Mapping):
            continue
        gid = site_gridid(site)

        if setpointmethod == 2:
            for param in (
                "setpoint_temperature_heating_air",
                "setpoint_temperature_cooling_air",
            ):
                if param in building_archetype:
                    old_val = get_ref_or_bare(building_archetype, param)
                    if old_val is not None:
                        plan.patches.append(
                            ConfigPatch(
                                path=f"sites[{site_idx}].properties.building_archetype.{param}",
                                action="set",
                                new_value=None,
                                old_value=old_val,
                            )
                        )
                        plan.record(
                            PlannedAdjustment(
                                parameter=f"building_archetype.{param}",
                                site_index=site_idx,
                                site_gridid=gid,
                                old_value=str(old_val),
                                new_value="null",
                                reason="setpoint == 2, parameter not needed",
                            )
                        )
        elif setpointmethod in (0, 1):
            for prof_param in (
                "profile_setpoint_temperature_heating_air",
                "profile_setpoint_temperature_cooling_air",
            ):
                profile = building_archetype.get(prof_param)
                if not isinstance(profile, Mapping):
                    continue
                for daytype in ("working_day", "holiday"):
                    day_profile = profile.get(daytype)
                    if not isinstance(day_profile, Mapping):
                        continue
                    if collect_nullifiable_paths(day_profile):
                        orig_values = collect_nullifiable_values(day_profile, prefix="")
                        plan.patches.append(
                            ConfigPatch(
                                path=(
                                    f"sites[{site_idx}].properties.building_archetype."
                                    f"{prof_param}.{daytype}"
                                ),
                                action="nullify_tree",
                                old_value=orig_values,
                                had_original=True,
                                source="setpointmethod",
                            )
                        )
                        plan.record(
                            PlannedAdjustment(
                                parameter=f"building_archetype.{prof_param}.{daytype}",
                                site_index=site_idx,
                                site_gridid=gid,
                                old_value="non-null profile entries",
                                new_value="null",
                                reason="setpoint == 0 or 1, profile entries not needed",
                            )
                        )

    return plan


def plan_stebbsmethod_wwr_patches(yaml_data: Mapping[str, Any]) -> PatchPlan:
    """Plan WWR-dependent window/wall nullification patches when STEBBS is active."""
    plan = PatchPlan()
    physics = _read_physics_mapping(yaml_data)
    if get_stebbsmethod_value(physics) != 1:
        return plan

    for site_idx, site in enumerate(_iter_sites(yaml_data)):
        if not isinstance(site, Mapping):
            continue
        props = site.get("properties") or {}
        stebbs = props.get("stebbs")
        bldgarc = props.get("building_archetype")
        gid = site_gridid(site)

        wwr = read_scalar(bldgarc, "ratio_window_to_wall") if isinstance(bldgarc, Mapping) else None
        if wwr == 0.0:
            stebbs_params = WINDOW_PARAMS_STEBBS
            arch_params = WINDOW_PARAMS_ARCHETYPE
            reason = "WWR == 0, window parameter nullified"
        elif wwr == 1.0:
            stebbs_params = WALL_PARAMS_STEBBS
            arch_params = WALL_PARAMS_ARCHETYPE
            reason = "WWR == 1, external wall parameter nullified"
        else:
            continue

        if isinstance(stebbs, Mapping):
            for param in stebbs_params:
                if param in stebbs:
                    old_val = get_ref_or_bare(stebbs, param)
                    if old_val is not None:
                        plan.patches.append(
                            ConfigPatch(
                                path=f"sites[{site_idx}].properties.stebbs.{param}",
                                action="set",
                                new_value=None,
                                old_value=old_val,
                            )
                        )
                        plan.record(
                            PlannedAdjustment(
                                parameter=f"stebbs.{param}",
                                site_index=site_idx,
                                site_gridid=gid,
                                old_value=str(old_val),
                                new_value="null",
                                reason=reason,
                            )
                        )

        if isinstance(bldgarc, Mapping):
            for param in arch_params:
                if param in bldgarc:
                    old_val = get_ref_or_bare(bldgarc, param)
                    if old_val is not None:
                        plan.patches.append(
                            ConfigPatch(
                                path=f"sites[{site_idx}].properties.building_archetype.{param}",
                                action="set",
                                new_value=None,
                                old_value=old_val,
                            )
                        )
                        plan.record(
                            PlannedAdjustment(
                                parameter=f"building_archetype.{param}",
                                site_index=site_idx,
                                site_gridid=gid,
                                old_value=str(old_val),
                                new_value="null",
                                reason=reason,
                            )
                        )

    return plan
