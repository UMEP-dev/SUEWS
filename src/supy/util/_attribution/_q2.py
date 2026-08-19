"""
q2 (2m specific humidity) attribution functions.

Provides functions to decompose q2 differences between scenarios
into physically attributable components.
"""

from typing import Literal

import numpy as np
import pandas as pd

from .._atm import cal_qa
from ._core import shapley_triple_product
from ._helpers import (
    align_scenarios,
    detect_anomalies,
    extract_suews_group,
    _group_means,
    unwrap_forcing,
)
from ._physics import cal_gamma_humidity, cal_r_eff_humidity
from ._result import AttributionResult


def attribute_q2(
    df_output_A: pd.DataFrame,
    df_output_B: pd.DataFrame,
    df_forcing_A: pd.DataFrame,
    df_forcing_B: pd.DataFrame,
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
    df_forcing_A : pd.DataFrame
        Forcing DataFrame for scenario A. Must contain 'Tair', 'RH', 'pres' columns.
    df_forcing_B : pd.DataFrame
        Forcing DataFrame for scenario B. Must contain 'Tair', 'RH', 'pres' columns.
    min_flux : float, optional
        Minimum flux threshold (W/m2) for resistance calculation.
        Timesteps with abs(Q_E) < min_flux are flagged. Default 0.1.

    Returns
    -------
    AttributionResult
        Container with contributions timeseries, summary statistics, and metadata.

    Examples
    --------
    Compare baseline vs. irrigated scenario:

    >>> result = attribute_q2(df_output_baseline, df_output_irrigated,
    ...                       df_forcing_A=df_forcing, df_forcing_B=df_forcing)
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
    # Unwrap OOP wrappers to raw DataFrames
    df_forcing_A = unwrap_forcing(df_forcing_A)
    df_forcing_B = unwrap_forcing(df_forcing_B)

    # Extract SUEWS output group with column validation
    required_cols = {"q2", "QE"}
    df_A = extract_suews_group(df_output_A, required_cols=required_cols)
    df_B = extract_suews_group(df_output_B, required_cols=required_cols)

    # Align indices
    df_A, df_B, common_idx = align_scenarios(df_A, df_B)

    # Extract required variables
    q2_A = df_A["q2"].values
    q2_B = df_B["q2"].values
    QE_A = df_A["QE"].values
    QE_B = df_B["QE"].values

    # Extract forcing data for reference humidity and air properties
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

    # Calculate gamma = 1/(rho*Lv)
    # Ensure numpy arrays (cal_gamma_humidity may return pandas Series)
    gamma_A = np.asarray(cal_gamma_humidity(T_ref_A, RH_A, P_A))
    gamma_B = np.asarray(cal_gamma_humidity(T_ref_B, RH_B, P_B))

    # Back-calculate effective resistance
    r_A = cal_r_eff_humidity(q2_A, q_ref_A, QE_A, gamma_A, min_flux)
    r_B = cal_r_eff_humidity(q2_B, q_ref_B, QE_B, gamma_B, min_flux)

    # Shapley decomposition: delta(q2 - q_ref) = delta(r * QE * gamma)
    Phi_r, Phi_QE, Phi_gamma = shapley_triple_product(
        r_A, r_B, QE_A, QE_B, gamma_A, gamma_B
    )

    # The total q2 change includes reference humidity change:
    # delta_q2 = delta_q_ref + delta(r * QE * gamma)
    Phi_q_ref = q_ref_B - q_ref_A

    # Build contributions DataFrame
    # Convert to g/kg for better readability
    scale = 1000.0  # kg/kg to g/kg
    contributions = pd.DataFrame(
        {
            "delta_total": (q2_B - q2_A) * scale,
            "q_ref": Phi_q_ref * scale,
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
    df_forcing: pd.DataFrame,
    method: Literal["anomaly", "extreme", "diurnal"] = "anomaly",
    threshold: float = 2.0,
    min_flux: float = 0.1,
) -> AttributionResult:
    """
    Automatically identify anomalous q2 values and attribute the causes.

    This convenience function identifies unusual q2 (2m specific humidity)
    behaviour within a single simulation run and diagnoses the driving factors
    by comparing aggregate statistics between reference (normal) and target
    (anomaly) periods.

    Parameters
    ----------
    df_output : pd.DataFrame
        SUEWS output DataFrame
    df_forcing : pd.DataFrame
        Forcing DataFrame. Must contain 'Tair', 'RH', 'pres' columns.
    method : str, optional
        Detection method:
        - 'anomaly': Compare timesteps > threshold sigma from daily mean
        - 'extreme': Compare top/bottom 5% of q2 vs. middle 50%
        - 'diurnal': Compare afternoon (12:00-15:00) vs. morning (06:00-10:00)
        Default 'anomaly'.
    threshold : float, optional
        Standard deviation threshold for anomaly detection. Default 2.0.
    min_flux : float, optional
        Minimum flux threshold (W/m2) for resistance calculation.
        Timesteps with abs(Q_E) < min_flux are excluded. Default 0.1.

    Returns
    -------
    AttributionResult
        Attribution decomposition with diagnostic interpretation.

    Examples
    --------
    Quick anomaly diagnosis:

    >>> result = diagnose_q2(df_output, df_forcing, method="anomaly")
    >>> print(result)
    q2 Attribution Results
    ========================================
    Mean delta_q2: +0.80 g/kg

    Component Breakdown:
    ----------------------------------------
      q_ref          : +0.55 g/kg (68.8%)
      flux_total     : +0.18 g/kg (22.5%)
      resistance     : +0.06 g/kg ( 7.5%)
      air_props      : +0.01 g/kg ( 1.2%)

    Closure residual: 1.23e-15 g/kg

    >>> result.plot()  # Visualise decomposition
    """
    # Unwrap OOP wrappers to raw DataFrames
    df_forcing = unwrap_forcing(df_forcing)

    # Extract SUEWS output
    df = extract_suews_group(df_output)

    if "q2" not in df.columns:
        raise ValueError(
            "q2 not found in SUEWS output. "
            "Ensure SUEWS is configured to output near-surface humidity."
        )

    q2 = df["q2"]

    anomaly_mask, normal_mask = detect_anomalies(
        q2, method=method, threshold=threshold
    )

    # Store group sizes for metadata
    n_anomaly = anomaly_mask.sum()
    n_normal = normal_mask.sum()

    # Extract data for each group
    df_normal = df.loc[normal_mask]
    df_anomaly = df.loc[anomaly_mask]

    # Get mean values for each group
    output_means = _group_means(df_normal, df_anomaly, ["q2", "QE"])
    q2_A, q2_B = output_means["q2"]
    QE_A, QE_B = output_means["QE"]

    # Extract forcing data for each group
    df_forcing_normal = df_forcing.loc[normal_mask]
    df_forcing_anomaly = df_forcing.loc[anomaly_mask]
    forcing_means = _group_means(
        df_forcing_normal, df_forcing_anomaly, ["Tair", "RH", "pres"]
    )
    T_ref_A, T_ref_B = forcing_means["Tair"]
    RH_A, RH_B = forcing_means["RH"]
    P_A, P_B = forcing_means["pres"]

    # Calculate reference specific humidity
    theta_A = T_ref_A + 273.15
    theta_B = T_ref_B + 273.15
    q_ref_A = cal_qa(RH_A, theta_A, P_A)
    q_ref_B = cal_qa(RH_B, theta_B, P_B)

    # Calculate gamma = 1/(rho*Lv)
    # Ensure numpy arrays (cal_gamma_humidity may return pandas Series)
    gamma_A = np.asarray(cal_gamma_humidity(T_ref_A, RH_A, P_A))
    gamma_B = np.asarray(cal_gamma_humidity(T_ref_B, RH_B, P_B))

    # Back-calculate effective resistance from mean values
    r_A = cal_r_eff_humidity(q2_A, q_ref_A, QE_A, gamma_A, min_flux)
    r_B = cal_r_eff_humidity(q2_B, q_ref_B, QE_B, gamma_B, min_flux)

    # Shapley decomposition: delta(q2 - q_ref) = delta(r * QE * gamma)
    Phi_r, Phi_QE, Phi_gamma = shapley_triple_product(
        r_A, r_B, QE_A, QE_B, gamma_A, gamma_B
    )

    # The total q2 change includes reference humidity change:
    # delta_q2 = delta_q_ref + delta(r * QE * gamma)
    Phi_q_ref = q_ref_B - q_ref_A

    # Build single-row contributions DataFrame
    # Convert to g/kg for better readability
    scale = 1000.0  # kg/kg to g/kg
    contributions = pd.DataFrame(
        {
            "delta_total": (q2_B - q2_A) * scale,
            "q_ref": Phi_q_ref * scale,  # Reference humidity contribution
            "flux_total": Phi_QE * scale,
            "resistance": Phi_r * scale,
            "air_props": Phi_gamma * scale,
        },
        index=pd.Index(["aggregate"], name="type"),
    )

    # Calculate summary (same as contributions for single-row)
    summary = contributions.describe().T[["mean", "std", "min", "max"]]

    # Metadata
    metadata = {
        "n_timesteps": 1,  # Aggregate comparison
        "method": method,
        "threshold": threshold if method == "anomaly" else None,
        "n_anomaly": int(n_anomaly),
        "n_reference": int(n_normal),
        "pct_anomaly": 100 * n_anomaly / len(q2),
        "unit": "g/kg",
        "aggregate_comparison": True,
    }

    return AttributionResult(
        variable="q2",
        contributions=contributions,
        summary=summary,
        metadata=metadata,
    )
