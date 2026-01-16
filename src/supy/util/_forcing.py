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

# Matches the Fortran SUEWS convention for missing/undefined values
MISSING_VALUE = -999.0


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
    # Convert to numeric first to avoid FutureWarning about silent downcasting in fillna
    smd_series = pd.to_numeric(df_state_init[("smdmethod", "0")], errors="coerce")
    smd_methods = smd_series.fillna(0).astype(int)
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
    - SMDMethod=1 (volumetric): [m3/m3] * [mm] * [-] = [mm]
    - SMDMethod=2 (gravimetric): [g/g] * [g/cm3] / [g/cm3] * [mm] * [-] = [mm]
      (division by rho_water=1 g/cm3 is implicit in the formula)
    """

    values = xsmd.to_numpy(dtype=float, copy=True)
    # Tolerance of 1.0 allows for slight numerical variations above -999
    # while ensuring truly missing data (exactly -999) are excluded
    mask_valid = values > (MISSING_VALUE + 1)
    if not np.any(mask_valid):
        return xsmd

    if smd_method == 1:
        # Volumetric: deficit [mm] = (theta_max - theta_obs) [fraction] * depth [mm] * soil_fraction
        clipped = np.clip(values[mask_valid], 0.0, meta.smcap)
        deficit = (meta.smcap - clipped) * meta.depth_mm * meta.soil_not_rocks
    elif smd_method == 2:
        # Gravimetric: deficit = (w_max - w_obs) * (rho_soil / rho_water) * depth * soil_fraction
        # Since rho_water = 1 g/cm3, formula simplifies to: (w_max - w_obs) * rho_soil * depth * soil_fraction
        clipped = np.clip(values[mask_valid], 0.0, meta.smcap)
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
    """Extract soil observation metadata from site-level config.

    Site-level config uses columns with index "0" in df_state.
    This is set via the `soil_observation` block in YAML site properties.
    """

    def get_site_value(column: str) -> Optional[float]:
        """Get value from site-level column (index "0")."""
        key = (column, "0")
        if key not in row.index:
            return None
        val = row[key]
        if val is None or pd.isna(val) or val <= (MISSING_VALUE + 1):
            return None
        return float(val)

    depth = get_site_value("obs_sm_depth")
    smcap = get_site_value("obs_sm_smcap")
    soil_not_rocks = get_site_value("obs_sm_soil_not_rocks")
    soil_density = get_site_value("obs_sm_bulk_density")

    if any(v is None for v in (depth, smcap, soil_not_rocks, soil_density)):
        raise ValueError(
            f"Observed soil moisture requires complete metadata for grid {grid}. "
            "Set `soil_observation` in site properties (YAML config) with: "
            "depth, smcap, soil_not_rocks, and bulk_density."
        )

    meta = SoilObservationMetadata(
        depth_mm=depth,
        smcap=smcap,
        soil_density=soil_density,
        soil_not_rocks=soil_not_rocks,
    )

    # Defence-in-depth validation: Pydantic constraints in SoilObservationConfig
    # already enforce these bounds, but this runtime check catches issues from
    # non-YAML paths (e.g., direct df_state manipulation or legacy table loading).
    if meta.depth_mm <= 0:
        raise ValueError(
            f"`depth` must be positive for grid {grid}. Got {meta.depth_mm}."
        )
    if not (0 < meta.soil_not_rocks <= 1):
        raise ValueError(
            f"`soil_not_rocks` must be in (0, 1]. Grid {grid} has {meta.soil_not_rocks}."
        )
    if meta.smcap <= 0:
        raise ValueError(f"`smcap` must be positive for grid {grid}. Got {meta.smcap}.")
    if meta.soil_density <= 0:
        raise ValueError(
            f"`bulk_density` must be positive for grid {grid}. Got {meta.soil_density}."
        )

    logger_supy.debug(f"Grid {grid}: using site-level soil observation config")
    return meta
