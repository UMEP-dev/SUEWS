"""
Utilities for handling observed soil moisture inputs (``xsmd``).

SuPy accepts observed soil moisture as either volumetric (SMDMethod = 1) or
gravimetric (SMDMethod = 2) measurements. The legacy Fortran reader (`MetRead`)
converts these measurements to a soil moisture deficit (mm) before they enter
the physics core. When SuPy feeds forcing data directly from pandas,
that conversion never happens, which means the kernel receives
raw volumetric/gravimetric values and the moisture stress logic is bypassed.

This module reconstructs the missing conversion using the soil metadata
stored in ``df_state`` so that observed soil moisture can be used as intended.
"""

from __future__ import annotations

from dataclasses import dataclass
import logging
from typing import Dict, Iterable, Optional, Tuple

import numpy as np
import pandas as pd

try:  # pragma: no cover - fallback used only during isolated tests
    from ._env import logger_supy
except ImportError:  # pragma: no cover
    logger_supy = logging.getLogger("SuPy")

MISSING_VALUE = -999.0
# SUEWS surface type indices: 0=paved, 1=buildings, 2=evergreen, 3=deciduous,
# 4=grass, 5=bare soil, 6=water. Soil moisture only applies to surfaces 0-5.
NON_WATER_SURFACE_INDICES: Tuple[int, ...] = tuple(range(6))


@dataclass(frozen=True)
class SoilObservationMetadata:
    """Aggregated metadata describing the observed soil moisture measurement."""

    depth_mm: float
    smcap: float
    soil_density: float
    soil_not_rocks: float


def convert_observed_soil_moisture(
    df_forcing: pd.DataFrame,
    df_state_init: pd.DataFrame,
) -> pd.DataFrame:
    """Convert observed soil moisture to deficits when ``SMDMethod`` > 0.

    Parameters
    ----------
    df_forcing
        Forcing dataframe that contains the ``xsmd`` column (as volumetric or gravimetric values).
    df_state_init
        Initial state dataframe providing surface fractions and soil metadata.

    Returns
    -------
    pandas.DataFrame
        Copy of ``df_forcing`` with ``xsmd`` converted to mm deficits when needed.
    """

    if ("smdmethod", "0") not in df_state_init.columns:
        # Nothing to do if the column is not present (legacy df_state)
        return df_forcing

    smd_methods = df_state_init[("smdmethod", "0")].astype(int)
    active_methods = set(int(m) for m in smd_methods if int(m) > 0)
    if not active_methods:
        return df_forcing

    if len(active_methods) > 1:
        raise ValueError(
            "SuPy currently expects a single `SMDMethod` value across all grids. "
            f"Got multiple values: {sorted(active_methods)}. "
            "Please run grids with different soil moisture configurations separately."
        )

    smd_method = active_methods.pop()

    metadata_per_grid: Dict[int, SoilObservationMetadata] = {}
    for grid, row in df_state_init.iterrows():
        method = int(row[("smdmethod", "0")])
        if method > 0:
            metadata_per_grid[grid] = _extract_soil_obs_metadata(row, grid)

    if not metadata_per_grid:
        # Should not happen, but guard against future regressions
        return df_forcing

    first_meta = next(iter(metadata_per_grid.values()))
    for grid, meta in metadata_per_grid.items():
        if not _metadata_close(meta, first_meta):
            raise ValueError(
                "Observed soil moisture metadata differ between grids, "
                "but SuPy currently feeds a single forcing dataframe to all grids. "
                f"Grid {grid} differs from the baseline. "
                "Solution: Call `run_supy_ser()` separately for each grid group "
                "with matching soil observation settings, or harmonise the metadata "
                "in your soil configuration files (SUEWS_Soil.txt or YAML config)."
            )

    if "xsmd" not in df_forcing.columns:
        raise ValueError(
            "SMDMethod > 0 requires the `xsmd` column in `df_forcing`, "
            "but it is missing."
        )

    df_result = df_forcing.copy()
    original_values = df_result["xsmd"].copy()
    df_result["xsmd"] = _convert_xsmd_series(
        df_result["xsmd"],
        first_meta,
        smd_method,
    )

    # Log conversion summary
    valid_mask = original_values > (MISSING_VALUE + 1)
    if np.any(valid_mask):
        converted = df_result["xsmd"][valid_mask]
        logger_supy.info(
            f"Converted observed soil moisture (`xsmd`) to deficits using "
            f"SMDMethod={smd_method}. depth={first_meta.depth_mm} mm, "
            f"smcap={first_meta.smcap}, soil_density={first_meta.soil_density}, "
            f"soil_not_rocks={first_meta.soil_not_rocks}"
        )
        logger_supy.debug(
            f"Deficit statistics: min={converted.min():.2f} mm, "
            f"max={converted.max():.2f} mm, mean={converted.mean():.2f} mm, "
            f"n_valid={valid_mask.sum()}/{len(original_values)}"
        )
    return df_result


def _metadata_close(a: SoilObservationMetadata, b: SoilObservationMetadata) -> bool:
    """Check whether two metadata objects are numerically equivalent."""

    attrs = ("depth_mm", "smcap", "soil_density", "soil_not_rocks")
    return all(
        np.isclose(getattr(a, att), getattr(b, att), rtol=1e-6, atol=1e-9)
        for att in attrs
    )


def _convert_xsmd_series(
    xsmd: pd.Series,
    meta: SoilObservationMetadata,
    smd_method: int,
) -> pd.Series:
    """Convert volumetric/gravimetric xsmd measurements to a soil moisture deficit.

    Dimensional analysis:
    - SMDMethod=1 (volumetric): [fraction] × [mm] × [fraction] = [mm]
    - SMDMethod=2 (gravimetric): [g/g] × [g/cm³] × [mm] × [fraction] = [mm]
    """

    values = xsmd.to_numpy(dtype=float, copy=True)
    # Tolerance of 1.0 allows for slight numerical variations above -999
    # while ensuring truly missing data (exactly -999) are excluded
    mask_valid = values > (MISSING_VALUE + 1)
    if not np.any(mask_valid):
        return xsmd

    clipped = values[mask_valid].copy()
    if smd_method == 1:
        # Volumetric: deficit [mm] = (θ_max - θ_obs) [fraction] × depth [mm] × soil_fraction
        clipped = np.clip(clipped, 0.0, meta.smcap)
        deficit = (meta.smcap - clipped) * meta.depth_mm * meta.soil_not_rocks
    elif smd_method == 2:
        # Gravimetric: deficit [mm] = (w_max - w_obs) [g/g] × ρ_soil [g/cm³] × depth [mm] × soil_fraction
        clipped = np.clip(clipped, 0.0, meta.smcap)
        deficit = (
            (meta.smcap - clipped)
            * meta.soil_density
            * meta.depth_mm
            * meta.soil_not_rocks
        )
    else:
        raise ValueError(f"Unsupported SMDMethod {smd_method}.")

    values[mask_valid] = deficit
    return pd.Series(values, index=xsmd.index, name=xsmd.name)


def _extract_soil_obs_metadata(row: pd.Series, grid: int) -> SoilObservationMetadata:
    """Aggregate per-surface metadata into a single set of observation parameters."""

    weights = _surface_weights(row, grid)
    depth = _weighted_average(row, "obs_sm_depth", weights)
    smcap = _weighted_average(row, "obs_sm_cap", weights)
    soil_not_rocks = _weighted_average(row, "obs_soil_not_rocks", weights)
    soil_density = _weighted_average(row, "soildensity", weights)

    missing = []
    if depth is None:
        missing.append("obs_sm_depth")
    if smcap is None:
        missing.append("obs_sm_cap")
    if soil_not_rocks is None:
        missing.append("obs_soil_not_rocks")
    if soil_density is None:
        missing.append("soildensity")

    if missing:
        raise ValueError(
            "Observed soil moisture requires the following metadata columns "
            f"for grid {grid}: {', '.join(missing)}. Please provide these values "
            "in the land-cover soil definitions (SUEWS_Soil.txt or YAML config)."
        )

    if depth <= 0:
        raise ValueError(
            f"`obs_sm_depth` must be positive for grid {grid}. Got {depth}."
        )
    if not (0 < soil_not_rocks <= 1):
        raise ValueError(
            f"`obs_soil_not_rocks` must be within (0, 1]. Grid {grid} has {soil_not_rocks}."
        )
    if smcap <= 0:
        raise ValueError(f"`obs_sm_cap` must be positive for grid {grid}. Got {smcap}.")
    if soil_density <= 0:
        raise ValueError(
            f"`soildensity` must be positive for grid {grid}. Got {soil_density}."
        )

    return SoilObservationMetadata(
        depth_mm=float(depth),
        smcap=float(smcap),
        soil_density=float(soil_density),
        soil_not_rocks=float(soil_not_rocks),
    )


def _surface_weights(row: pd.Series, grid: int) -> Dict[int, float]:
    """Return normalised surface fractions for non-water surfaces."""

    weights: Dict[int, float] = {}
    for surf in NON_WATER_SURFACE_INDICES:
        key = ("sfr_surf", f"({surf},)")
        if key in row.index:
            val = row[key]
            if val is not None and not pd.isna(val):
                weights[surf] = float(val)

    total = sum(weights.values())
    if total <= 0:
        raise ValueError(
            f"Could not determine non-water surface fractions for grid {grid}. "
            "Check `sfr_surf` values in the initial state dataframe."
        )
    return {surf: weight / total for surf, weight in weights.items()}


def _weighted_average(
    row: pd.Series,
    column: str,
    weights: Dict[int, float],
) -> Optional[float]:
    """Weighted average across surfaces, ignoring missing values."""

    totals = []
    weight_sum = 0.0
    for surf, weight in weights.items():
        key = (column, f"({surf},)")
        if key not in row.index:
            continue
        val = row[key]
        if val is None or pd.isna(val) or val <= (MISSING_VALUE + 1):
            continue
        totals.append(float(val) * weight)
        weight_sum += weight

    if weight_sum == 0.0:
        return None
    return sum(totals) / weight_sum
