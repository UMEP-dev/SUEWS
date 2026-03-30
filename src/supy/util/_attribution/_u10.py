"""
U10 (10m wind speed) attribution functions.

Provides functions to decompose U10 differences between scenarios
into physically attributable components using the logarithmic wind profile.
"""

from typing import Literal

import numpy as np
import pandas as pd

from ._core import shapley_forcing_profile
from ._helpers import (
    align_scenarios,
    detect_anomalies,
    extract_suews_group,
    _group_means,
)
from ._physics import cal_u10_profile_components
from ._result import AttributionResult


def attribute_u10(
    df_output_A: pd.DataFrame,
    df_output_B: pd.DataFrame,
    z_ref: float = 10.0,
    min_ustar: float = 0.01,
) -> AttributionResult:
    """
    Decompose U10 (10m wind speed) differences between two SUEWS scenarios.

    The difference in 10m wind speed is attributed to:
    - Forcing changes (u* - friction velocity / surface stress)
    - Roughness changes (z0m, zd - surface roughness characteristics)
    - Stability changes (psi_m - atmospheric stability correction)

    The attribution uses the logarithmic wind profile:
        U10 = (u*/k) * [ln((z-d)/z0m) - psi_m(zeta)]

    Where:
    - u* = friction velocity (m/s)
    - k = von Karman constant (0.4)
    - z = reference height (10m)
    - d = displacement height (m)
    - z0m = roughness length for momentum (m)
    - psi_m = stability correction for momentum
    - zeta = (z-d)/L = stability parameter

    Parameters
    ----------
    df_output_A : pd.DataFrame
        SUEWS output DataFrame for scenario A (reference/baseline)
    df_output_B : pd.DataFrame
        SUEWS output DataFrame for scenario B (modified/test)
    z_ref : float, optional
        Reference height for wind speed (m). Default 10.0 m.
    min_ustar : float, optional
        Minimum friction velocity threshold (m/s). Timesteps with
        abs(u*) < min_ustar are flagged. Default 0.01.

    Returns
    -------
    AttributionResult
        Container with contributions timeseries, summary statistics, and metadata.

    Notes
    -----
    The Shapley decomposition for the wind profile f = F * (R + S) where:
    - F = u*/k (forcing)
    - R = ln((z-d)/z0m) (roughness)
    - S = -psi_m (stability)

    Yields exact closure:
    - Phi_forcing = dF * (P_A + P_B) / 2,  where P = R + S
    - Phi_roughness = dR * (F_A + F_B) / 2
    - Phi_stability = dS * (F_A + F_B) / 2

    Examples
    --------
    Compare baseline vs. increased roughness scenario:

    >>> result = attribute_u10(df_output_baseline, df_output_urban)
    >>> print(result)
    U10 Attribution Results
    ========================================
    Mean delta_U10: -1.23 m/s

    Component Breakdown:
    ----------------------------------------
      forcing        : -0.42 m/s (34.1%)
      roughness      : -0.65 m/s (52.8%)
      stability      : -0.16 m/s (13.0%)

    >>> result.plot(kind="bar")  # Visualise contributions
    """
    # Required columns for U10 attribution
    required_cols = {"U10", "UStar", "z0m", "zd", "Lob"}

    # Extract SUEWS output group with column validation
    df_A = extract_suews_group(df_output_A, required_cols=required_cols)
    df_B = extract_suews_group(df_output_B, required_cols=required_cols)

    # Align indices
    df_A, df_B, common_idx = align_scenarios(df_A, df_B)

    # Extract required variables
    U10_A = df_A["U10"].values
    U10_B = df_B["U10"].values
    ustar_A = df_A["UStar"].values
    ustar_B = df_B["UStar"].values
    z0m_A = df_A["z0m"].values
    z0m_B = df_B["z0m"].values
    zd_A = df_A["zd"].values
    zd_B = df_B["zd"].values
    Lob_A = df_A["Lob"].values
    Lob_B = df_B["Lob"].values

    # Calculate profile components for each scenario
    # F = u*/k, R = ln((z-d)/z0m), S = -psi_m
    F_A, R_A, S_A = cal_u10_profile_components(ustar_A, z0m_A, zd_A, Lob_A, z_ref=z_ref)
    F_B, R_B, S_B = cal_u10_profile_components(ustar_B, z0m_B, zd_B, Lob_B, z_ref=z_ref)

    # Shapley decomposition for f = F * (R + S)
    Phi_F, Phi_R, Phi_S = shapley_forcing_profile(F_A, F_B, R_A, R_B, S_A, S_B)

    # Build contributions DataFrame
    contributions = pd.DataFrame(
        {
            "delta_total": U10_B - U10_A,
            "forcing": Phi_F,
            "roughness": Phi_R,
            "stability": Phi_S,
        },
        index=common_idx,
    )

    # Add diagnostic columns
    contributions["flag_low_ustar"] = np.abs(ustar_A) < min_ustar

    # Store profile components for advanced users
    contributions["F_A"] = F_A
    contributions["F_B"] = F_B
    contributions["R_A"] = R_A
    contributions["R_B"] = R_B
    contributions["S_A"] = S_A
    contributions["S_B"] = S_B

    # Calculate summary statistics (main components only)
    main_cols = ["delta_total", "forcing", "roughness", "stability", "flag_low_ustar"]
    summary = contributions[main_cols].describe().T[["mean", "std", "min", "max"]]

    # Metadata
    metadata = {
        "n_timesteps": len(common_idx),
        "period_start": str(common_idx.min()),
        "period_end": str(common_idx.max()),
        "z_ref": z_ref,
        "min_ustar_threshold": min_ustar,
        "n_low_ustar_flagged": int(contributions["flag_low_ustar"].sum()),
        "unit": "m/s",
    }

    return AttributionResult(
        variable="U10",
        contributions=contributions,
        summary=summary,
        metadata=metadata,
    )


def diagnose_u10(
    df_output: pd.DataFrame,
    method: Literal["anomaly", "extreme", "diurnal"] = "anomaly",
    threshold: float = 2.0,
    z_ref: float = 10.0,
    min_ustar: float = 0.01,
) -> AttributionResult:
    """
    Automatically identify anomalous U10 values and attribute the causes.

    This convenience function identifies unusual U10 (10m wind speed)
    behaviour within a single simulation run and diagnoses the driving factors
    by comparing aggregate statistics between reference (normal) and target
    (anomaly) periods.

    Parameters
    ----------
    df_output : pd.DataFrame
        SUEWS output DataFrame
    method : str, optional
        Detection method:
        - 'anomaly': Compare timesteps > threshold sigma from daily mean
        - 'extreme': Compare top/bottom 5% of U10 vs. middle 50%
        - 'diurnal': Compare afternoon (12:00-15:00) vs. morning (06:00-10:00)
        Default 'anomaly'.
    threshold : float, optional
        Standard deviation threshold for anomaly detection. Default 2.0.
    z_ref : float, optional
        Reference height for wind speed (m). Default 10.0 m.
    min_ustar : float, optional
        Minimum friction velocity threshold (m/s). Default 0.01.

    Returns
    -------
    AttributionResult
        Attribution decomposition with diagnostic interpretation.

    Examples
    --------
    Quick anomaly diagnosis:

    >>> result = diagnose_u10(df_output, method="anomaly")
    >>> print(result)
    U10 Attribution Results
    ========================================
    Mean delta_U10: +1.85 m/s

    Component Breakdown:
    ----------------------------------------
      forcing        : +0.92 m/s (49.7%)
      roughness      : +0.65 m/s (35.1%)
      stability      : +0.28 m/s (15.1%)

    Closure residual: 1.23e-15 m/s

    >>> result.plot()  # Visualise decomposition
    """
    # Extract SUEWS output
    df = extract_suews_group(df_output)

    if "U10" not in df.columns:
        raise ValueError(
            "U10 not found in SUEWS output. "
            "Ensure SUEWS is configured to output near-surface wind speed."
        )

    u10 = df["U10"]

    anomaly_mask, normal_mask = detect_anomalies(
        u10, method=method, threshold=threshold
    )

    # Store group sizes for metadata
    n_anomaly = anomaly_mask.sum()
    n_normal = normal_mask.sum()

    # Extract data for each group
    df_normal = df.loc[normal_mask]
    df_anomaly = df.loc[anomaly_mask]

    # Get mean values for each group
    output_means = _group_means(
        df_normal, df_anomaly, ["U10", "UStar", "z0m", "zd", "Lob"]
    )
    U10_A, U10_B = output_means["U10"]
    ustar_A, ustar_B = output_means["UStar"]
    z0m_A, z0m_B = output_means["z0m"]
    zd_A, zd_B = output_means["zd"]
    Lob_A, Lob_B = output_means["Lob"]

    # Calculate profile components for each scenario
    F_A, R_A, S_A = cal_u10_profile_components(ustar_A, z0m_A, zd_A, Lob_A, z_ref=z_ref)
    F_B, R_B, S_B = cal_u10_profile_components(ustar_B, z0m_B, zd_B, Lob_B, z_ref=z_ref)

    # Shapley decomposition for f = F * (R + S)
    Phi_F, Phi_R, Phi_S = shapley_forcing_profile(F_A, F_B, R_A, R_B, S_A, S_B)

    # Build single-row contributions DataFrame
    contributions = pd.DataFrame(
        {
            "delta_total": U10_B - U10_A,
            "forcing": Phi_F,
            "roughness": Phi_R,
            "stability": Phi_S,
        },
        index=pd.Index(["aggregate"], name="type"),
    )

    # Add flag for low ustar
    contributions["flag_low_ustar"] = np.abs(ustar_A) < min_ustar

    # Calculate summary (same as contributions for single-row)
    main_cols = ["delta_total", "forcing", "roughness", "stability", "flag_low_ustar"]
    summary = contributions[main_cols].describe().T[["mean", "std", "min", "max"]]

    # Metadata
    metadata = {
        "n_timesteps": 1,  # Aggregate comparison
        "method": method,
        "threshold": threshold if method == "anomaly" else None,
        "n_anomaly": int(n_anomaly),
        "n_reference": int(n_normal),
        "pct_anomaly": 100 * n_anomaly / len(u10),
        "z_ref": z_ref,
        "unit": "m/s",
        "aggregate_comparison": True,
    }

    return AttributionResult(
        variable="U10",
        contributions=contributions,
        summary=summary,
        metadata=metadata,
    )
