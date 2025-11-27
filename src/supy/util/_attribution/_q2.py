"""
q2 (2m specific humidity) attribution functions.

Provides functions to decompose q2 differences between scenarios
into physically attributable components.
"""

from typing import Literal, Optional
import warnings

import numpy as np
import pandas as pd

from .._atm import cal_qa
from ._core import shapley_triple_product
from ._helpers import extract_suews_group
from ._physics import cal_gamma_humidity, cal_r_eff_humidity
from ._result import AttributionResult


def attribute_q2(
    df_output_A: pd.DataFrame,
    df_output_B: pd.DataFrame,
    df_forcing_A: Optional[pd.DataFrame] = None,
    df_forcing_B: Optional[pd.DataFrame] = None,
    min_flux: float = 0.1,
) -> AttributionResult:
    """
    Decompose q2 (2m specific humidity) differences between two SUEWS scenarios.

    The difference in 2m specific humidity is attributed to:
    - Flux changes (Q_E latent heat flux)
    - Resistance changes (turbulent exchange efficiency)
    - Air property changes (rho * L_v)

    Parameters
    ----------
    df_output_A : pd.DataFrame
        SUEWS output DataFrame for scenario A (reference/baseline)
    df_output_B : pd.DataFrame
        SUEWS output DataFrame for scenario B (modified/test)
    df_forcing_A : pd.DataFrame, optional
        Forcing DataFrame for scenario A. If None, extracts from df_output_A.
    df_forcing_B : pd.DataFrame, optional
        Forcing DataFrame for scenario B. If None, extracts from df_output_B.
    min_flux : float, optional
        Minimum flux threshold (W/m2) for resistance calculation.
        Timesteps with |Q_E| < min_flux are flagged. Default 0.1.

    Returns
    -------
    AttributionResult
        Container with contributions timeseries, summary statistics, and metadata.

    Examples
    --------
    Compare baseline vs. irrigated scenario:

    >>> result = attribute_q2(df_output_baseline, df_output_irrigated)
    >>> print(result)
    q2 Attribution Results
    ========================================
    Mean delta_q2: +0.0012 g/kg

    Component Breakdown:
    ----------------------------------------
      flux_total     : +0.0008 g/kg (66.7%)
      resistance     : +0.0003 g/kg (25.0%)
      air_props      : +0.0001 g/kg ( 8.3%)

    >>> result.plot(kind="bar")  # Visualise contributions
    """
    # Extract SUEWS output group with column validation
    required_cols = {"q2", "QE", "T2"}
    df_A = extract_suews_group(df_output_A, required_cols=required_cols)
    df_B = extract_suews_group(df_output_B, required_cols=required_cols)

    # Align indices
    n_A_original = len(df_A.index)
    n_B_original = len(df_B.index)
    common_idx = df_A.index.intersection(df_B.index)

    if len(common_idx) == 0:
        raise ValueError("No overlapping timestamps between scenarios A and B")

    # Warn if significant data loss during alignment
    pct_A_kept = 100 * len(common_idx) / n_A_original
    pct_B_kept = 100 * len(common_idx) / n_B_original
    if pct_A_kept < 90 or pct_B_kept < 90:
        warnings.warn(
            f"Significant data loss during alignment: keeping {pct_A_kept:.1f}% of "
            f"scenario A ({len(common_idx)}/{n_A_original}), {pct_B_kept:.1f}% of "
            f"scenario B ({len(common_idx)}/{n_B_original}). "
            "Check that time indices match between scenarios.",
            UserWarning,
            stacklevel=2,
        )

    df_A = df_A.loc[common_idx]
    df_B = df_B.loc[common_idx]

    # Extract required variables
    q2_A = df_A["q2"].values
    q2_B = df_B["q2"].values
    QE_A = df_A["QE"].values
    QE_B = df_B["QE"].values

    # Get forcing data for reference humidity and air properties
    if df_forcing_A is not None and df_forcing_B is not None:
        # Calculate reference specific humidity from forcing
        T_ref_A = df_forcing_A.loc[common_idx, "Tair"].values
        T_ref_B = df_forcing_B.loc[common_idx, "Tair"].values
        RH_A = df_forcing_A.loc[common_idx, "RH"].values
        RH_B = df_forcing_B.loc[common_idx, "RH"].values
        P_A = df_forcing_A.loc[common_idx, "pres"].values
        P_B = df_forcing_B.loc[common_idx, "pres"].values

        # Calculate reference specific humidity
        theta_A = T_ref_A + 273.15  # Celsius to Kelvin
        theta_B = T_ref_B + 273.15
        q_ref_A = cal_qa(RH_A, theta_A, P_A)
        q_ref_B = cal_qa(RH_B, theta_B, P_B)
    else:
        # Warn user about approximation
        warnings.warn(
            "Forcing data not provided. Using q2 as proxy for reference humidity "
            "and default values for RH (60%) and pressure (1013.25 hPa). "
            "Air property contributions (air_props) may be unreliable. "
            "For accurate attribution, provide df_forcing_A and df_forcing_B.",
            UserWarning,
            stacklevel=2,
        )
        # Use approximations
        T_ref_A = df_A["T2"].values
        T_ref_B = df_B["T2"].values
        RH_A = np.full_like(q2_A, 60.0)  # 60% RH default
        RH_B = np.full_like(q2_B, 60.0)
        P_A = np.full_like(q2_A, 1013.25)  # Standard pressure
        P_B = np.full_like(q2_B, 1013.25)

        # Approximate reference humidity as q2 (simplified)
        q_ref_A = q2_A
        q_ref_B = q2_B

    # Calculate gamma = 1/(rho*Lv)
    gamma_A = cal_gamma_humidity(T_ref_A, RH_A, P_A)
    gamma_B = cal_gamma_humidity(T_ref_B, RH_B, P_B)

    # Back-calculate effective resistance
    r_A = cal_r_eff_humidity(q2_A, q_ref_A, QE_A, gamma_A, min_flux)
    r_B = cal_r_eff_humidity(q2_B, q_ref_B, QE_B, gamma_B, min_flux)

    # Shapley decomposition: delta_q2 = delta(r * QE * gamma)
    Phi_r, Phi_QE, Phi_gamma = shapley_triple_product(
        r_A, r_B, QE_A, QE_B, gamma_A, gamma_B
    )

    # Build contributions DataFrame
    # Convert to g/kg for better readability
    scale = 1000.0  # kg/kg to g/kg
    contributions = pd.DataFrame(
        {
            "delta_total": (q2_B - q2_A) * scale,
            "flux_total": Phi_QE * scale,
            "resistance": Phi_r * scale,
            "air_props": Phi_gamma * scale,
        },
        index=common_idx,
    )

    # Add flags for low-flux timesteps
    contributions["flag_low_flux"] = np.abs(QE_A) < min_flux

    # Calculate summary statistics
    summary = contributions.describe().T[["mean", "std", "min", "max"]]

    # Metadata
    metadata = {
        "n_timesteps": len(common_idx),
        "period_start": str(common_idx.min()),
        "period_end": str(common_idx.max()),
        "hierarchical": False,  # q2 has no flux budget decomposition
        "min_flux_threshold": min_flux,
        "n_low_flux_flagged": int(contributions["flag_low_flux"].sum()),
        "unit": "g/kg",
    }

    return AttributionResult(
        variable="q2",
        contributions=contributions,
        summary=summary,
        metadata=metadata,
    )


def diagnose_q2(
    df_output: pd.DataFrame,
    df_forcing: Optional[pd.DataFrame] = None,
    method: Literal["anomaly", "extreme", "diurnal"] = "anomaly",
    threshold: float = 2.0,
) -> AttributionResult:
    """
    Automatically identify anomalous q2 values and attribute the causes.

    This convenience function identifies unusual q2 (2m specific humidity)
    behaviour within a single simulation run and diagnoses the driving factors.

    Parameters
    ----------
    df_output : pd.DataFrame
        SUEWS output DataFrame
    df_forcing : pd.DataFrame, optional
        Forcing DataFrame. If None, uses approximations.
    method : str, optional
        Detection method:
        - 'anomaly': Compare timesteps > threshold sigma from daily mean
        - 'extreme': Compare top/bottom 5% of q2 vs. middle 50%
        - 'diurnal': Compare afternoon (12:00-15:00) vs. morning (06:00-10:00)
        Default 'anomaly'.
    threshold : float, optional
        Standard deviation threshold for anomaly detection. Default 2.0.

    Returns
    -------
    AttributionResult
        Attribution decomposition with diagnostic interpretation.

    Examples
    --------
    Quick anomaly diagnosis:

    >>> result = diagnose_q2(df_output, method="anomaly")
    >>> print(result)
    q2 Attribution Results
    ========================================
    Mean delta_q2: +0.80 g/kg

    Component Breakdown:
    ----------------------------------------
      flux_total     : +0.53 g/kg (66.7%)
      resistance     : +0.20 g/kg (25.0%)
      air_props      : +0.07 g/kg ( 8.3%)

    Closure residual: 1.23e-15 g/kg

    >>> result.plot()  # Visualise decomposition
    """
    # Extract SUEWS output
    df = extract_suews_group(df_output)

    if "q2" not in df.columns:
        raise ValueError(
            "q2 not found in SUEWS output. "
            "Ensure SUEWS is configured to output near-surface humidity."
        )

    q2 = df["q2"]

    if method == "anomaly":
        # Identify anomalous timesteps (deviation > threshold sigma from daily mean)
        daily_mean = q2.resample("D").transform("mean")
        daily_std = q2.resample("D").transform("std")
        # Handle zero std
        daily_std = daily_std.replace(0, np.nan)
        z_score = (q2 - daily_mean) / daily_std

        anomaly_mask = np.abs(z_score) > threshold
        normal_mask = np.abs(z_score) <= 1.0  # Within 1 sigma = normal

    elif method == "extreme":
        # Compare extreme values vs. middle range
        q05 = q2.quantile(0.05)
        q25 = q2.quantile(0.25)
        q75 = q2.quantile(0.75)
        q95 = q2.quantile(0.95)

        anomaly_mask = (q2 <= q05) | (q2 >= q95)
        normal_mask = (q2 >= q25) & (q2 <= q75)

    elif method == "diurnal":
        # Compare afternoon vs. morning baseline
        hour = q2.index.hour
        anomaly_mask = (hour >= 12) & (hour <= 15)  # Afternoon
        normal_mask = (hour >= 6) & (hour <= 10)  # Morning

    else:
        raise ValueError(
            f"Unknown method: {method}. Use 'anomaly', 'extreme', 'diurnal'"
        )

    # Check we have enough data in each group
    n_anomaly = anomaly_mask.sum()
    n_normal = normal_mask.sum()

    if n_anomaly < 10:
        raise ValueError(
            f"Only {n_anomaly} anomalous timesteps found. "
            f"Try lowering threshold (current: {threshold})"
        )
    if n_normal < 10:
        raise ValueError(
            f"Only {n_normal} reference timesteps found. Cannot establish baseline."
        )

    # Create reference and anomaly DataFrames
    df_normal = df_output.loc[normal_mask]
    df_anomaly = df_output.loc[anomaly_mask]

    if df_forcing is not None:
        df_forcing_normal = df_forcing.loc[normal_mask]
        df_forcing_anomaly = df_forcing.loc[anomaly_mask]
    else:
        df_forcing_normal = None
        df_forcing_anomaly = None

    # Run attribution
    result = attribute_q2(
        df_output_A=df_normal,
        df_output_B=df_anomaly,
        df_forcing_A=df_forcing_normal,
        df_forcing_B=df_forcing_anomaly,
    )

    # Update metadata with diagnostic info
    result.metadata.update({
        "method": method,
        "threshold": threshold if method == "anomaly" else None,
        "n_anomaly": int(n_anomaly),
        "n_reference": int(n_normal),
        "pct_anomaly": 100 * n_anomaly / len(q2),
    })

    return result
