"""
T2 (2m air temperature) attribution functions.

Provides functions to decompose T2 differences between scenarios
into physically attributable components.
"""

from typing import Literal

import numpy as np
import pandas as pd

from ._core import shapley_triple_product
from ._helpers import (
    align_scenarios,
    detect_anomalies,
    extract_suews_group,
    _group_means,
    unwrap_forcing,
)
from ._physics import cal_gamma_heat, cal_r_eff_heat, decompose_flux_budget
from ._result import AttributionResult


def _extract_flux_components(
    df: pd.DataFrame,
    flux_like: np.ndarray,
    aggregate: bool = False,
) -> dict[str, np.ndarray]:
    """Extract signed energy-budget components with zero-fill for missing columns."""

    def _extract(col: str, sign: float = 1.0) -> np.ndarray:
        if col not in df.columns:
            return np.zeros_like(flux_like, dtype=float)
        values = np.array([df[col].mean()]) if aggregate else df[col].values
        return sign * values

    return {
        "Qstar": _extract("QN"),
        "QE": _extract("QE", sign=-1.0),
        "dQS": _extract("QS", sign=-1.0),
        "QF": _extract("QF"),
    }


def attribute_t2(
    df_output_A: pd.DataFrame,
    df_output_B: pd.DataFrame,
    df_forcing_A: pd.DataFrame,
    df_forcing_B: pd.DataFrame,
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
    df_forcing_A : pd.DataFrame
        Forcing DataFrame for scenario A. Must contain 'Tair', 'RH', 'pres' columns.
    df_forcing_B : pd.DataFrame
        Forcing DataFrame for scenario B. Must contain 'Tair', 'RH', 'pres' columns.
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

    >>> result = attribute_t2(df_output_baseline, df_output_green,
    ...                       df_forcing_A=df_forcing, df_forcing_B=df_forcing)
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
    # Unwrap OOP wrappers to raw DataFrames
    df_forcing_A = unwrap_forcing(df_forcing_A)
    df_forcing_B = unwrap_forcing(df_forcing_B)

    # Extract SUEWS output group with column validation
    required_cols = {"T2", "QH"}
    df_A = extract_suews_group(df_output_A, required_cols=required_cols)
    df_B = extract_suews_group(df_output_B, required_cols=required_cols)

    # Align indices
    df_A, df_B, common_idx = align_scenarios(df_A, df_B)

    # Extract required variables
    T2_A = df_A["T2"].values
    T2_B = df_B["T2"].values
    QH_A = df_A["QH"].values
    QH_B = df_B["QH"].values

    # Extract forcing data for reference temperature and air properties
    T_ref_A = df_forcing_A.loc[common_idx, "Tair"].values
    T_ref_B = df_forcing_B.loc[common_idx, "Tair"].values
    RH_A = df_forcing_A.loc[common_idx, "RH"].values
    RH_B = df_forcing_B.loc[common_idx, "RH"].values
    P_A = df_forcing_A.loc[common_idx, "pres"].values
    P_B = df_forcing_B.loc[common_idx, "pres"].values

    # Calculate gamma = 1/(rho*cp)
    # Ensure numpy arrays (cal_gamma_heat may return pandas Series)
    gamma_A = np.asarray(cal_gamma_heat(T_ref_A, RH_A, P_A))
    gamma_B = np.asarray(cal_gamma_heat(T_ref_B, RH_B, P_B))

    # Back-calculate effective resistance
    r_A = cal_r_eff_heat(T2_A, T_ref_A, QH_A, gamma_A, min_flux)
    r_B = cal_r_eff_heat(T2_B, T_ref_B, QH_B, gamma_B, min_flux)

    # Shapley decomposition: delta(T2 - T_ref) = delta(r * QH * gamma)
    Phi_r, Phi_QH, Phi_gamma = shapley_triple_product(
        r_A, r_B, QH_A, QH_B, gamma_A, gamma_B
    )

    # The total T2 change includes reference temperature change:
    # delta_T2 = delta_T_ref + delta(r * QH * gamma)
    Phi_T_ref = T_ref_B - T_ref_A

    # Build contributions DataFrame
    contributions = pd.DataFrame(
        {
            "delta_total": T2_B - T2_A,
            "T_ref": Phi_T_ref,
            "flux_total": Phi_QH,
            "resistance": Phi_r,
            "air_props": Phi_gamma,
        },
        index=common_idx,
    )

    # Hierarchical flux decomposition
    if hierarchical:
        # Q_H = Q* - Q_E - dQ_S + Q_F (energy balance)
        flux_comps_A = _extract_flux_components(df_A, QH_A)
        flux_comps_B = _extract_flux_components(df_B, QH_B)

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
    df_forcing: pd.DataFrame,
    method: Literal["anomaly", "extreme", "diurnal"] = "anomaly",
    threshold: float = 2.0,
    hierarchical: bool = True,
    min_flux: float = 0.1,
) -> AttributionResult:
    """
    Automatically identify anomalous T2 values and attribute the causes.

    This convenience function identifies unusual T2 behaviour within a single
    simulation run and diagnoses the driving factors by comparing aggregate
    statistics between reference (normal) and target (anomaly) periods.

    Parameters
    ----------
    df_output : pd.DataFrame
        SUEWS output DataFrame
    df_forcing : pd.DataFrame
        Forcing DataFrame. Must contain 'Tair', 'RH', 'pres' columns.
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
    min_flux : float, optional
        Minimum flux threshold (W/m2) for resistance calculation.
        Timesteps with |Q_H| < min_flux are excluded. Default 0.1.

    Returns
    -------
    AttributionResult
        Attribution decomposition with diagnostic interpretation.

    Examples
    --------
    Quick anomaly diagnosis:

    >>> result = diagnose_t2(df_output, df_forcing, method="anomaly")
    >>> print(result)
    T2 Attribution Results
    ========================================
    Mean delta_T2: +2.80 degC

    Component Breakdown:
    ----------------------------------------
      T_ref          : +2.10 degC (75.0%)
      flux_total     : +0.50 degC (17.9%)
      resistance     : +0.18 degC ( 6.4%)
      air_props      : +0.02 degC ( 0.7%)

    Closure residual: 1.23e-15 degC

    >>> result.plot()  # Visualise decomposition
    """
    # Unwrap OOP wrappers to raw DataFrames
    df_forcing = unwrap_forcing(df_forcing)

    # Extract SUEWS output
    df = extract_suews_group(df_output)
    t2 = df["T2"]

    anomaly_mask, normal_mask = detect_anomalies(
        t2, method=method, threshold=threshold
    )

    # Store group sizes for metadata
    n_anomaly = anomaly_mask.sum()
    n_normal = normal_mask.sum()

    # Extract data for each group
    df_normal = df.loc[normal_mask]
    df_anomaly = df.loc[anomaly_mask]

    # Get mean values for each group
    output_means = _group_means(df_normal, df_anomaly, ["T2", "QH"])
    T2_A, T2_B = output_means["T2"]
    QH_A, QH_B = output_means["QH"]

    # Extract forcing data for each group
    df_forcing_normal = df_forcing.loc[normal_mask]
    df_forcing_anomaly = df_forcing.loc[anomaly_mask]
    forcing_means = _group_means(
        df_forcing_normal, df_forcing_anomaly, ["Tair", "RH", "pres"]
    )
    T_ref_A, T_ref_B = forcing_means["Tair"]
    RH_A, RH_B = forcing_means["RH"]
    P_A, P_B = forcing_means["pres"]

    # Calculate gamma = 1/(rho*cp)
    # Ensure numpy arrays (cal_gamma_heat may return pandas Series)
    gamma_A = np.asarray(cal_gamma_heat(T_ref_A, RH_A, P_A))
    gamma_B = np.asarray(cal_gamma_heat(T_ref_B, RH_B, P_B))

    # Back-calculate effective resistance from mean values
    r_A = cal_r_eff_heat(T2_A, T_ref_A, QH_A, gamma_A, min_flux)
    r_B = cal_r_eff_heat(T2_B, T_ref_B, QH_B, gamma_B, min_flux)

    # Shapley decomposition: delta(T2 - T_ref) = delta(r * QH * gamma)
    Phi_r, Phi_QH, Phi_gamma = shapley_triple_product(
        r_A, r_B, QH_A, QH_B, gamma_A, gamma_B
    )

    # The total T2 change includes reference temperature change:
    # delta_T2 = delta_T_ref + delta(r * QH * gamma)
    Phi_T_ref = T_ref_B - T_ref_A

    # Build single-row contributions DataFrame
    contributions = pd.DataFrame(
        {
            "delta_total": T2_B - T2_A,
            "T_ref": Phi_T_ref,  # Reference temperature contribution
            "flux_total": Phi_QH,
            "resistance": Phi_r,
            "air_props": Phi_gamma,
        },
        index=pd.Index(["aggregate"], name="type"),
    )

    # Hierarchical flux decomposition
    if hierarchical:
        flux_comps_A = _extract_flux_components(df_normal, QH_A, aggregate=True)
        flux_comps_B = _extract_flux_components(df_anomaly, QH_B, aggregate=True)

        flux_breakdown = decompose_flux_budget(
            QH_A, QH_B, flux_comps_A, flux_comps_B, Phi_QH
        )

        for name, values in flux_breakdown.items():
            contributions[f"flux_{name}"] = values

    # Calculate summary (same as contributions for single-row)
    summary = contributions.describe().T[["mean", "std", "min", "max"]]

    # Metadata
    metadata = {
        "n_timesteps": 1,  # Aggregate comparison
        "method": method,
        "threshold": threshold if method == "anomaly" else None,
        "n_anomaly": int(n_anomaly),
        "n_reference": int(n_normal),
        "pct_anomaly": 100 * n_anomaly / len(t2),
        "hierarchical": hierarchical,
        "aggregate_comparison": True,
    }

    return AttributionResult(
        variable="T2",
        contributions=contributions,
        summary=summary,
        metadata=metadata,
    )
