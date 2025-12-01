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
from typing import Dict, Optional

import numpy as np
import pandas as pd

try:  # pragma: no cover - fallback used only during isolated tests
    from .._env import logger_supy
except ImportError:  # pragma: no cover
    logger_supy = logging.getLogger("SuPy")

MISSING_VALUE = -999.0
# Non-water surface indices (0=paved, 1=bldgs, 2=evetr, 3=dectr, 4=grass, 5=bsoil)
# Water (6) is excluded as it has no soil store.
NON_WATER_SURFACE_INDICES = range(6)


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

    # NaN values in smdmethod are treated as 0 (no observation), which is the default
    smd_methods = df_state_init[("smdmethod", "0")].fillna(0).astype(int)
    active_methods = set(m for m in smd_methods if m > 0)
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
    - SMDMethod=1 (volumetric): [m³/m³] × [mm] × [-] = [mm]
    - SMDMethod=2 (gravimetric): [g/g] × [g/cm³] / [g/cm³] × [mm] × [-] = [mm]
      (division by ρ_water=1 g/cm³ is implicit in the formula)
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
        # Gravimetric: deficit = (w_max - w_obs) × (ρ_soil / ρ_water) × depth × soil_fraction
        # Since ρ_water = 1 g/cm³, formula simplifies to: (w_max - w_obs) × ρ_soil × depth × soil_fraction
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
    """Extract soil observation metadata from the first surface with complete data.

    Observed soil moisture is a single point measurement. We search through
    non-water surfaces (0-5) and use the first one with complete observation
    metadata. Users should set their observation parameters on any one surface.
    """

    def get_value(column: str, surf: int) -> Optional[float]:
        key = (column, f"({surf},)")
        if key not in row.index:
            return None
        val = row[key]
        if val is None or pd.isna(val) or val <= (MISSING_VALUE + 1):
            return None
        return float(val)

    def try_surface(surf: int) -> Optional[SoilObservationMetadata]:
        """Try to extract complete metadata from a surface, return None if incomplete."""
        depth = get_value("obs_sm_depth", surf)
        smcap = get_value("obs_sm_cap", surf)
        soil_not_rocks = get_value("obs_soil_not_rocks", surf)
        soil_density = get_value("soildensity", surf)

        # All four must be present for a complete metadata set
        if any(v is None for v in (depth, smcap, soil_not_rocks, soil_density)):
            return None

        return SoilObservationMetadata(
            depth_mm=depth,
            smcap=smcap,
            soil_density=soil_density,
            soil_not_rocks=soil_not_rocks,
        )

    # Search through non-water surfaces for first with complete metadata
    for surf in NON_WATER_SURFACE_INDICES:
        meta = try_surface(surf)
        if meta is not None:
            # Validate the extracted values
            if meta.depth_mm <= 0:
                raise ValueError(
                    f"`obs_sm_depth` must be positive for grid {grid}. Got {meta.depth_mm}."
                )
            if not (0 < meta.soil_not_rocks <= 1):
                raise ValueError(
                    f"`obs_soil_not_rocks` must be in (0, 1]. Grid {grid} has {meta.soil_not_rocks}."
                )
            if meta.smcap <= 0:
                raise ValueError(
                    f"`obs_sm_cap` must be positive for grid {grid}. Got {meta.smcap}."
                )
            if meta.soil_density <= 0:
                raise ValueError(
                    f"`soildensity` must be positive for grid {grid}. Got {meta.soil_density}."
                )
            logger_supy.debug(
                f"Grid {grid}: using soil observation metadata from surface index {surf}"
            )
            return meta

    # No surface had complete metadata
    raise ValueError(
        f"Observed soil moisture requires metadata for grid {grid}. "
        "Set obs_sm_depth, obs_sm_cap, obs_soil_not_rocks, and soildensity "
        "on any non-water surface (0-5) in SUEWS_Soil.txt or YAML config."
    )
