"""Tier-2 scientific advisory checks for Phase B validation.

Advisory rules emit ``WARNING`` or ``INFO`` guidance in Phase B and never
raise during ``SUEWSConfig`` loading. Keeping them in this dedicated module
separates non-blocking scientific heuristics from the Tier-1 hard invariants
in ``hard_invariants.py``.
"""

from __future__ import annotations

import calendar
from collections.abc import Sequence
from dataclasses import dataclass
from typing import Any

from .typed_view import SiteView


@dataclass(frozen=True)
class AdvisoryFinding:
    """One non-blocking Tier-2 advisory warning or informational note."""

    status: str  # "WARNING" or "INFO"
    category: str
    parameter: str
    message: str
    suggested_value: str | None = None
    site_index: int | None = None
    site_gridid: Any = None


def _is_in_warm_season(doy: int, latitude: float) -> bool:
    """Return True if *doy* falls inside the warm season for *latitude*."""
    if latitude >= 0:
        return 121 <= doy <= 273
    return doy >= 305 or doy <= 90


def check_irrigation_season_advisory(
    ie_start: int,
    ie_end: int,
    lat: float,
    site_name: str,
    *,
    model_year: int | None = None,
) -> list[AdvisoryFinding]:
    """Emit warm-season and hemisphere year-wrap advisories for active irrigation."""
    if abs(lat) < 23.5:
        return []

    findings: list[AdvisoryFinding] = []
    start_in_warm = _is_in_warm_season(ie_start, lat)
    end_in_warm = _is_in_warm_season(ie_end, lat)

    if not start_in_warm or not end_in_warm:
        hemisphere = "Northern" if lat >= 0 else "Southern"
        season_range = (
            "May-September (DOY 121-273)"
            if lat >= 0
            else "November-March (DOY 305-90)"
        )
        findings.append(
            AdvisoryFinding(
                status="WARNING",
                category="IRRIGATION",
                parameter=f"irrigation in site {site_name}",
                message=(
                    f"Irrigation period (DOY {int(ie_start)} to {int(ie_end)}) falls outside "
                    f"the typical warm season for {hemisphere} Hemisphere sites (lat={lat:.2f}). "
                    f"Irrigation typically occurs during warm season: {season_range}. "
                    "Verify this configuration is intentional."
                ),
                suggested_value=f"Consider adjusting to warm season: {season_range}",
            )
        )

    if lat >= 23.5 and ie_start > ie_end:
        days_in_year = (
            366 if (model_year is not None and calendar.isleap(model_year)) else 365
        )
        findings.append(
            AdvisoryFinding(
                status="WARNING",
                category="IRRIGATION",
                parameter=f"irrigation in site {site_name}",
                message=(
                    f"Irrigation period wraps year boundary (DOY {int(ie_start)} to {int(ie_end)}). "
                    f"This means irrigation spans {days_in_year - int(ie_start) + int(ie_end)} days "
                    "including cold season. "
                    "Verify this is intentional for Northern Hemisphere."
                ),
                suggested_value="Check if ie_start should be less than ie_end",
            )
        )
    elif lat <= -23.5 and ie_start < ie_end:
        findings.append(
            AdvisoryFinding(
                status="WARNING",
                category="IRRIGATION",
                parameter=f"irrigation in site {site_name}",
                message=(
                    f"Irrigation period does not wrap year boundary (DOY {int(ie_start)} to {int(ie_end)}). "
                    "In Southern Hemisphere, warm season typically spans November-March (year-wrapping)."
                ),
                suggested_value="Check if ie_start should be greater than ie_end",
            )
        )

    return findings


def check_same_surface_unchecked_advisory(
    site_name: str,
    *,
    param_name: str,
    layers_label: str,
    property_name: str,
    archetype_label: str,
    layer_values: Sequence[Any],
    archetype_value: Any,
    site_index: int | None = None,
    site_gridid: Any = None,
) -> AdvisoryFinding:
    """Emit the advisory warning when ``same_albedo_* == 0`` or ``same_emissivity_* == 0``."""
    return AdvisoryFinding(
        status="WARNING",
        category="MODEL_OPTIONS",
        parameter=param_name,
        site_gridid=site_gridid,
        site_index=site_index,
        message=(
            f"{param_name} == 0 for site {site_name}. No check of consistency between {layers_label} "
            f"{property_name} (found values: {list(layer_values)}) and "
            f"{archetype_label} (found value: {archetype_value})."
        ),
        suggested_value=None,
    )


def check_spartacus_lowest_layer_advisories(
    site: SiteView,
    *,
    sfr_tol: float,
) -> list[AdvisoryFinding]:
    """Emit advisories when SPARTACUS lowest-layer morphology differs from land cover."""
    findings: list[AdvisoryFinding] = []
    site_gridid = site.gridid

    first_building_frac = site.first_vertical_value("building_frac")
    bldgs_sfr = site.surface_sfr("bldgs")
    if (
        first_building_frac is not None
        and bldgs_sfr is not None
        and abs(first_building_frac - bldgs_sfr) > sfr_tol
    ):
        findings.append(
            AdvisoryFinding(
                status="WARNING",
                category="LAND_COVER",
                parameter="land_cover.bldgs.sfr",
                site_index=site.index,
                site_gridid=site_gridid,
                message=(
                    "Building land-cover fraction "
                    f"(land_cover.bldgs.sfr = {bldgs_sfr:.4f}) differs "
                    "from SPARTACUS lowest-layer building fraction "
                    f"(vertical_layers.building_frac[0] = {first_building_frac:.4f}). "
                    "This can be valid when the land-cover and vertical "
                    "morphology data come from different sources. Review "
                    "which source should be trusted before changing either "
                    "land_cover.bldgs.sfr or vertical_layers.building_frac."
                ),
                suggested_value=(
                    "If land_cover.bldgs.sfr is the trusted source, review "
                    "whether the vertical_layers.building_frac profile should "
                    "be rescaled. If vertical_layers.building_frac is the "
                    "trusted source, review land_cover.bldgs.sfr and manually "
                    "rebalance the affected land-cover fractions so the total "
                    "surface fraction remains 1.0."
                ),
            )
        )

    lowest_layer_veg_frac = site.first_vertical_value("veg_frac")
    if lowest_layer_veg_frac is not None:
        dectr_sfr = site.surface_sfr("dectr") or 0.0
        evetr_sfr = site.surface_sfr("evetr") or 0.0
        current_tree_cover = dectr_sfr + evetr_sfr
        if abs(current_tree_cover - lowest_layer_veg_frac) > sfr_tol:
            findings.append(
                AdvisoryFinding(
                    status="WARNING",
                    category="LAND_COVER",
                    parameter="land_cover.tree_sfr",
                    site_index=site.index,
                    site_gridid=site_gridid,
                    message=(
                        "Tree land-cover fraction "
                        f"(dectr.sfr + evetr.sfr = {current_tree_cover:.4f}) "
                        "differs from SPARTACUS lowest-layer tree fraction "
                        f"(vertical_layers.veg_frac[0] = {lowest_layer_veg_frac:.4f}). "
                        "This can be valid when the land-cover and vertical "
                        "morphology data come from different sources or the "
                        "lowest layer is supposed to represent a trunk fraction. "
                        "Review which source should be trusted before changing "
                        "either land cover fractions or vertical_layers.veg_frac."
                    ),
                )
            )

    return findings
