"""Canonical liquid soil content, interpolation and kernel-deficit preparation."""

from __future__ import annotations

from dataclasses import dataclass
import warnings

import numpy as np
import pandas as pd

from ..units import convert, parse_unit

MISSING_VALUE = -999.0
LEGACY_SOIL_MESSAGE = (
    "Legacy soil-moisture input requires explicit migration; xsmd and "
    "SMDMethod=2 are not canonical sm inputs."
)


@dataclass(frozen=True)
class SoilObservationMetadata:
    """Effective non-water store, with water-equivalent capacities in mm."""

    layer_depth: float
    layer_top: float
    capacity: float
    field_capacity: float


def validate_soil_content(df: pd.DataFrame, *, required: bool = False) -> None:
    """Validate public content before interpolation, including unchecked runs."""
    names = {str(c).lower() for c in df.columns}
    if names & {"xsmd", "smd"}:
        if "sm" in names:
            raise ValueError("Both legacy and canonical soil-moisture inputs were supplied; select one contract before conversion.")
        raise ValueError(LEGACY_SOIL_MESSAGE)
    if "sm" not in df:
        if required:
            raise ValueError("Observed soil moisture requires forcing column 'sm' in kg m-2.")
        return
    values = df["sm"].to_numpy(dtype=float)
    missing = np.isnan(values) | (values == MISSING_VALUE)
    invalid = ~missing & (~np.isfinite(values) | (values < 0))
    if invalid.any():
        first = np.flatnonzero(invalid)[0]
        raise ValueError(
            "sm must be finite and non-negative in kg m-2, or a recognised missing value when observations are not required. "
            f"First: {df.index[first]} ({values[first]})."
        )
    if required and missing.any():
        raise ValueError(
            "Observed soil moisture contains missing values; provide every required timestamp before running. "
            f"First: {df.index[np.flatnonzero(missing)[0]]}."
        )


def interpolate_soil_content(values: pd.Series, target: pd.DatetimeIndex) -> pd.Series:
    """Interpolate only between valid adjacent observations; never extrapolate."""
    source = values.astype(float).replace(MISSING_VALUE, np.nan)
    union = source.index.union(target).sort_values()
    expanded = source.reindex(union)
    result = expanded.interpolate(method="time", limit_area="inside")
    # Missing source observations are barriers, not points to fill across.
    left = source.reindex(union, method="ffill")
    right = source.reindex(union, method="bfill")
    result = result.where(left.notna() & right.notna())
    return result.reindex(target).rename("sm")


def _extract_soil_obs_metadata(row: pd.Series, grid: object) -> SoilObservationMetadata:
    """Extract effective geometry, field capacity and AWC from runtime state."""
    def value(name: str, index: str = "0", default: float = np.nan) -> float:
        raw = row.get((name, index), default)
        return float(raw) if raw is not None else np.nan

    if any((name, "0") in row.index for name in ("obs_sm_depth", "obs_sm_smcap")):
        raise ValueError(LEGACY_SOIL_MESSAGE)
    depth = value("obs_sm_layer_depth")
    top = value("obs_sm_layer_top", default=0.0)
    if not np.isfinite(depth) or depth <= 0:
        raise ValueError("soil_observation.layer_depth must be finite and greater than 0 m. " f"Grid {grid}.")
    if not np.isfinite(top) or top < 0:
        raise ValueError("soil_observation.layer_top must be finite and greater than or equal to 0 m. " f"Grid {grid}.")
    fractions = np.array([value("sfr_surf", f"({i},)") for i in range(7)])
    if not np.isfinite(fractions).all() or (fractions < 0).any() or fractions[:6].sum() <= 0:
        raise ValueError("Observed soil moisture requires valid surface fractions and a positive non-water area. " f"Grid {grid}.")
    capacity = field_capacity = 0.0
    for i, fraction in enumerate(fractions[:6]):
        if fraction == 0:
            continue
        d = value("soildepth", f"({i},)")
        c = value("soilstorecap_surf", f"({i},)")
        theta = value("soil_field_capacity", f"({i},)")
        if not np.isfinite([d, c]).all() or min(d, c) <= 0:
            raise ValueError("Observed soil moisture requires finite positive soil_depth and soil_store_capacity for every non-water surface with positive area. " f"Grid {grid}, surface {i}.")
        if not np.isfinite(theta) or not 0 < theta <= 1 or theta * d < c:
            raise ValueError("Observed soil moisture requires soil_field_capacity in (0, 1] with field-capacity content >= soil_store_capacity. " f"Grid {grid}, surface {i}.")
        if not np.isclose(top, 0, rtol=0, atol=1e-6) or not np.isclose(depth, d / 1000, rtol=0, atol=1e-6):
            raise ValueError("Observed soil layer must match the full configured soil store; no layer extrapolation or truncation is applied. " f"Grid {grid}, surface {i}.")
        capacity += fraction * c
        field_capacity += fraction * theta * d
    area = fractions[:6].sum()
    return SoilObservationMetadata(depth, top, capacity / area, field_capacity / area)


def _metadata_close(a: SoilObservationMetadata, b: SoilObservationMetadata) -> bool:
    return all(np.isclose(getattr(a, key), getattr(b, key), rtol=0, atol=1e-6)
               for key in ("layer_depth", "layer_top", "capacity", "field_capacity"))


def convert_observed_soil_moisture(
    df_forcing: pd.DataFrame, df_state_init: pd.DataFrame,
) -> pd.DataFrame:
    """Prepare a fresh kernel frame after content interpolation to model time.

    Public ``sm`` is retained. Only the private ``_soil_deficit`` column is
    copied to the kernel's xsmd slot. Modelled runs never convert content.
    """
    methods = df_state_init.get(("smdmethod", "0"), pd.Series(0, index=df_state_init.index))
    methods = pd.to_numeric(methods, errors="raise").fillna(0)
    if not methods.isin([0, 1]).all():
        raise ValueError(LEGACY_SOIL_MESSAGE)
    active = methods == 1
    validate_soil_content(df_forcing, required=bool(active.any()))
    result = df_forcing.copy()
    if not active.any():
        return result
    metadata = [_extract_soil_obs_metadata(row, grid)
                for grid, row in df_state_init.loc[active].iterrows()]
    first = metadata[0]
    if any(not _metadata_close(first, meta) for meta in metadata[1:]):
        raise ValueError("Shared forcing requires matching soil-observation bounds and bulk soil-store capacity and field capacity; run incompatible grids separately.")
    sm = result["sm"]
    for mask, excess, message in (
        (sm > first.field_capacity, sm - first.field_capacity,
         "Observed soil content exceeds field capacity; derived deficit was limited to 0 mm and input sm was retained."),
        (sm < first.field_capacity - first.capacity, first.field_capacity - first.capacity - sm,
         "Observed soil content is below the wilting reference; derived deficit was limited to available water capacity and input sm was retained."),
    ):
        if mask.any():
            warnings.warn(f"{message} Count={int(mask.sum())}; first timestamp={sm.index[mask][0]}; maximum excess={excess[mask].max():g} mm.", UserWarning, stacklevel=2)
    result["_soil_deficit"] = (first.field_capacity - sm).clip(0, first.capacity)
    return result


def soil_moisture_to_content(
    values, *, source_unit: str, layer_depth: float, layer_top: float = 0.0,
    bulk_density: float | None = None, soil_not_rocks: float = 1.0,
    volume_basis: str = "total_layer",
) -> pd.Series:
    """Convert liquid observations to ``sm`` (kg m-2), retaining layer provenance.

    ``layer_depth`` is a represented layer thickness in m, not sensor depth.
    Density is dry soil mass per declared volume in kg m-3. Missing values
    propagate as NaN. No capacity subtraction or saturation clipping occurs.
    """
    parse_unit(source_unit)
    if source_unit not in {"m3 m-3", "kg kg-1", "mm"}:
        raise ValueError("source_unit must be m3 m-3, kg kg-1 or mm; canonical sm uses kg m-2.")
    if not np.isfinite(layer_depth) or layer_depth <= 0:
        raise ValueError("soil_observation.layer_depth must be finite and greater than 0 m.")
    if not np.isfinite(layer_top) or layer_top < 0:
        raise ValueError("soil_observation.layer_top must be finite and greater than or equal to 0 m.")
    if not np.isfinite(soil_not_rocks) or not 0 < soil_not_rocks <= 1:
        raise ValueError("soil_not_rocks must be finite and in (0, 1].")
    if volume_basis not in {"total_layer", "fine_earth"}:
        raise ValueError("volume_basis must be total_layer or fine_earth.")
    if volume_basis == "total_layer" and soil_not_rocks != 1:
        raise ValueError("Non-unit soil_not_rocks requires explicit fine_earth volume_basis.")
    if source_unit == "mm" and soil_not_rocks != 1:
        raise ValueError("Water-equivalent mm is already layer content; do not apply soil_not_rocks.")
    result = pd.Series(values, dtype=float, copy=True).replace(MISSING_VALUE, np.nan)
    present = result.notna()
    if (~np.isfinite(result[present])).any() or (result[present] < 0).any():
        raise ValueError("Soil moisture must be finite and non-negative, or a recognised missing value.")
    factor = soil_not_rocks if volume_basis == "fine_earth" else 1.0
    if source_unit == "m3 m-3":
        if (result[present] > 1).any():
            raise ValueError("Volumetric soil moisture must be finite and in [0, 1] m3 m-3.")
        result *= 1000 * layer_depth * factor
    elif source_unit == "kg kg-1":
        if bulk_density is None or not np.isfinite(bulk_density) or bulk_density <= 0:
            raise ValueError("Gravimetric conversion requires finite positive dry bulk_density in kg m-3.")
        result *= bulk_density * layer_depth * factor
    else:
        result[:] = convert(result.to_numpy(), "mm", "m") * 1000
    result.name = "sm"
    result.attrs.update(unit="kg m-2", layer_top=layer_top, layer_depth=layer_depth,
                        source_unit=source_unit, volume_basis=volume_basis,
                        soil_not_rocks=soil_not_rocks, bulk_density=bulk_density,
                        water_phase="liquid")
    return result
