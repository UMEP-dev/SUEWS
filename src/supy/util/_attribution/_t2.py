"""
T2 (2m air temperature) attribution functions.

Provides functions to decompose T2 differences between scenarios
into physically attributable components.
"""

from typing import Literal, Optional
import warnings

import numpy as np
import pandas as pd

from .._atm import cal_qa
from ._core import shapley_triple_product
from ._helpers import extract_suews_group
from ._physics import cal_gamma_heat, cal_r_eff_heat, decompose_flux_budget
from ._result import AttributionResult


def attribute_t2(
    df_output_A: pd.DataFrame,
    df_output_B: pd.DataFrame,
    df_forcing_A: Optional[pd.DataFrame] = None,
    df_forcing_B: Optional[pd.DataFrame] = None,
    hierarchical: bool = True,
    min_flux: float = 0.1,
) -> AttributionResult:
    """
    Decompose T2 differences between two SUEWS scenarios.

    The difference in 2m air temperature is attributed to:
    - Flux changes (Q_H, with optional breakdown into Q*, Q_E, dQ_S, Q_F)
    - Resistance changes (turbulent exchange efficiency)
    - Air property changes (rho * c_p)

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
    hierarchical : bool, optional
        If True, decompose flux contribution into budget components
        (Q*, Q_E, dQ_S, Q_F). Default True.
    min_flux : float, optional
        Minimum flux threshold (W/m2) for resistance calculation.
        Timesteps with |Q_H| < min_flux are flagged. Default 0.1.

    Returns
    -------
    AttributionResult
        Container with contributions timeseries, summary statistics, and metadata.

    Examples
    --------
    Compare baseline vs. green infrastructure scenario:

    >>> result = attribute_t2(df_output_baseline, df_output_green)
    >>> print(result)
    T2 Attribution Results
    ========================================
    Mean delta_T2: -1.47 degC

    Component Breakdown:
    ----------------------------------------
      flux_total     : -0.89 degC (60.5%)
      resistance     : -0.42 degC (28.6%)
      air_props      : -0.16 degC (10.9%)

    >>> result.plot(kind="bar")  # Visualise contributions
    """
    # Extract SUEWS output group with column validation
    required_cols = {"T2", "QH"}
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
    T2_A = df_A["T2"].values
    T2_B = df_B["T2"].values
    QH_A = df_A["QH"].values
    QH_B = df_B["QH"].values

    # Get forcing data for reference temperature and air properties
    if df_forcing_A is not None and df_forcing_B is not None:
        T_ref_A = df_forcing_A.loc[common_idx, "Tair"].values
        T_ref_B = df_forcing_B.loc[common_idx, "Tair"].values
        RH_A = df_forcing_A.loc[common_idx, "RH"].values
        RH_B = df_forcing_B.loc[common_idx, "RH"].values
        P_A = df_forcing_A.loc[common_idx, "pres"].values
        P_B = df_forcing_B.loc[common_idx, "pres"].values
    else:
        # Warn user about approximation
        warnings.warn(
            "Forcing data not provided. Using T2 as proxy for reference temperature "
            "and default values for RH (60%) and pressure (1013.25 hPa). "
            "Air property contributions (air_props) may be unreliable. "
            "For accurate attribution, provide df_forcing_A and df_forcing_B.",
            UserWarning,
            stacklevel=2,
        )
        # Use T2 as proxy for reference temperature (simplified)
        T_ref_A = df_A["T2"].values
        T_ref_B = df_B["T2"].values
        # Use typical values for RH and pressure
        RH_A = np.full_like(T2_A, 60.0)  # 60% RH default
        RH_B = np.full_like(T2_B, 60.0)
        P_A = np.full_like(T2_A, 1013.25)  # Standard pressure
        P_B = np.full_like(T2_B, 1013.25)

    # Calculate gamma = 1/(rho*cp)
    gamma_A = cal_gamma_heat(T_ref_A, RH_A, P_A)
    gamma_B = cal_gamma_heat(T_ref_B, RH_B, P_B)

    # Back-calculate effective resistance
    r_A = cal_r_eff_heat(T2_A, T_ref_A, QH_A, gamma_A, min_flux)
    r_B = cal_r_eff_heat(T2_B, T_ref_B, QH_B, gamma_B, min_flux)

    # Shapley decomposition: delta_T2 = delta(r * QH * gamma)
    Phi_r, Phi_QH, Phi_gamma = shapley_triple_product(
        r_A, r_B, QH_A, QH_B, gamma_A, gamma_B
    )

    # Build contributions DataFrame
    contributions = pd.DataFrame(
        {
            "delta_total": T2_B - T2_A,
            "flux_total": Phi_QH,
            "resistance": Phi_r,
            "air_props": Phi_gamma,
        },
        index=common_idx,
    )

    # Hierarchical flux decomposition
    if hierarchical:
        # Q_H = Q* - Q_E - dQ_S + Q_F (energy balance)
        # Extract flux components
        flux_comps_A = {
            "Qstar": df_A["QN"].values if "QN" in df_A.columns else np.zeros_like(QH_A),
            "QE": -df_A["QE"].values if "QE" in df_A.columns else np.zeros_like(QH_A),
            "dQS": -df_A["QS"].values if "QS" in df_A.columns else np.zeros_like(QH_A),
            "QF": df_A["QF"].values if "QF" in df_A.columns else np.zeros_like(QH_A),
        }
        flux_comps_B = {
            "Qstar": df_B["QN"].values if "QN" in df_B.columns else np.zeros_like(QH_B),
            "QE": -df_B["QE"].values if "QE" in df_B.columns else np.zeros_like(QH_B),
            "dQS": -df_B["QS"].values if "QS" in df_B.columns else np.zeros_like(QH_B),
            "QF": df_B["QF"].values if "QF" in df_B.columns else np.zeros_like(QH_B),
        }

        flux_breakdown = decompose_flux_budget(
            QH_A, QH_B, flux_comps_A, flux_comps_B, Phi_QH
        )

        for name, values in flux_breakdown.items():
            contributions[f"flux_{name}"] = values

    # Add flags for low-flux timesteps
    contributions["flag_low_flux"] = np.abs(QH_A) < min_flux

    # Calculate summary statistics
    summary = contributions.describe().T[["mean", "std", "min", "max"]]

    # Metadata
    metadata = {
        "n_timesteps": len(common_idx),
        "period_start": str(common_idx.min()),
        "period_end": str(common_idx.max()),
        "hierarchical": hierarchical,
        "min_flux_threshold": min_flux,
        "n_low_flux_flagged": int(contributions["flag_low_flux"].sum()),
    }

    return AttributionResult(
        variable="T2",
        contributions=contributions,
        summary=summary,
        metadata=metadata,
    )


def diagnose_t2(
    df_output: pd.DataFrame,
    df_forcing: Optional[pd.DataFrame] = None,
    method: Literal["anomaly", "extreme", "diurnal"] = "anomaly",
    threshold: float = 2.0,
    hierarchical: bool = True,
) -> AttributionResult:
    """
    Automatically identify anomalous T2 values and attribute the causes.

    This convenience function identifies unusual T2 behaviour within a single
    simulation run and diagnoses the driving factors.

    Parameters
    ----------
    df_output : pd.DataFrame
        SUEWS output DataFrame
    df_forcing : pd.DataFrame, optional
        Forcing DataFrame. If None, uses approximations.
    method : str, optional
        Detection method:
        - 'anomaly': Compare timesteps > threshold sigma from daily mean
        - 'extreme': Compare top/bottom 5% of T2 vs. middle 50%
        - 'diurnal': Compare afternoon peak (12:00-15:00) vs. morning (06:00-10:00)
        Default 'anomaly'.
    threshold : float, optional
        Standard deviation threshold for anomaly detection. Default 2.0.
    hierarchical : bool, optional
        Include flux budget breakdown. Default True.

    Returns
    -------
    AttributionResult
        Attribution decomposition with diagnostic interpretation.

    Examples
    --------
    Quick anomaly diagnosis:

    >>> result = diagnose_t2(df_output, method="anomaly")
    >>> print(result)
    T2 Attribution Results
    ========================================
    Mean delta_T2: +2.80 degC

    Component Breakdown:
    ----------------------------------------
      flux_total     : +1.74 degC (62.1%)
      resistance     : +0.78 degC (27.9%)
      air_props      : +0.28 degC (10.0%)

    Closure residual: 1.23e-15 degC

    >>> result.plot()  # Visualise decomposition
    """
    # Extract SUEWS output
    df = extract_suews_group(df_output)
    t2 = df["T2"]

    if method == "anomaly":
        # Identify anomalous timesteps (deviation > threshold sigma from daily mean)
        daily_mean = t2.resample("D").transform("mean")
        daily_std = t2.resample("D").transform("std")
        # Handle zero std
        daily_std = daily_std.replace(0, np.nan)
        z_score = (t2 - daily_mean) / daily_std

        anomaly_mask = np.abs(z_score) > threshold
        normal_mask = np.abs(z_score) <= 1.0  # Within 1 sigma = normal

    elif method == "extreme":
        # Compare extreme values vs. middle range
        q05 = t2.quantile(0.05)
        q25 = t2.quantile(0.25)
        q75 = t2.quantile(0.75)
        q95 = t2.quantile(0.95)

        anomaly_mask = (t2 <= q05) | (t2 >= q95)
        normal_mask = (t2 >= q25) & (t2 <= q75)

    elif method == "diurnal":
        # Compare afternoon peak vs. morning baseline
        hour = t2.index.hour
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
    result = attribute_t2(
        df_output_A=df_normal,
        df_output_B=df_anomaly,
        df_forcing_A=df_forcing_normal,
        df_forcing_B=df_forcing_anomaly,
        hierarchical=hierarchical,
    )

    # Update metadata with diagnostic info
    result.metadata.update({
        "method": method,
        "threshold": threshold if method == "anomaly" else None,
        "n_anomaly": int(n_anomaly),
        "n_reference": int(n_normal),
        "pct_anomaly": 100 * n_anomaly / len(t2),
    })

    return result
