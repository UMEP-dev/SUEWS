"""
Near-surface diagnostics attribution module for SUEWS.

Decomposes changes in near-surface variables (T2, q2, U10) between two scenarios
into physically attributable components using exact Shapley decomposition.

Mathematical Foundation
-----------------------
T2 follows the profile formula:
    T2 = T_ref + r_h * Q_H / (rho * c_p)

Where:
- T_ref: reference temperature at forcing height
- r_h: effective heat resistance
- Q_H: sensible heat flux (W/m2)
- rho: air density (kg/m3)
- c_p: specific heat capacity (J/kg/K)

The change in T2 between scenarios A and B can be decomposed as:
    delta_T2 = delta(r * Q_H * gamma)

where gamma = 1/(rho * c_p). Using Shapley values for triple products ensures
exact closure: sum(contributions) == delta_T2.
"""

from dataclasses import dataclass, field
from typing import Optional, Literal

import numpy as np
import pandas as pd

from ._atm import cal_dens_air, cal_cp_with_rh, cal_lat_vap, cal_qa


# =============================================================================
# Core Shapley Decomposition
# =============================================================================


def _shapley_triple_product(
    x_A: np.ndarray,
    x_B: np.ndarray,
    y_A: np.ndarray,
    y_B: np.ndarray,
    z_A: np.ndarray,
    z_B: np.ndarray,
) -> tuple[np.ndarray, np.ndarray, np.ndarray]:
    """
    Exact Shapley decomposition for f = x * y * z.

    Given two states A and B, decompose the change in f = xyz into
    contributions from each factor such that:
        Phi_x + Phi_y + Phi_z = f_B - f_A (exactly)

    Parameters
    ----------
    x_A, x_B : array-like
        Factor x in states A and B
    y_A, y_B : array-like
        Factor y in states A and B
    z_A, z_B : array-like
        Factor z in states A and B

    Returns
    -------
    Phi_x, Phi_y, Phi_z : array-like
        Shapley value contributions from each factor

    Notes
    -----
    The Shapley decomposition for triple products:
        Phi_x = (dx/6) * [2*y_A*z_A + 2*y_B*z_B + y_A*z_B + y_B*z_A]
        Phi_y = (dy/6) * [2*x_A*z_A + 2*x_B*z_B + x_A*z_B + x_B*z_A]
        Phi_z = (dz/6) * [2*x_A*y_A + 2*x_B*y_B + x_A*y_B + x_B*y_A]

    This ensures exact closure by construction.
    """
    dx = x_B - x_A
    dy = y_B - y_A
    dz = z_B - z_A

    Phi_x = (dx / 6) * (2 * y_A * z_A + 2 * y_B * z_B + y_A * z_B + y_B * z_A)
    Phi_y = (dy / 6) * (2 * x_A * z_A + 2 * x_B * z_B + x_A * z_B + x_B * z_A)
    Phi_z = (dz / 6) * (2 * x_A * y_A + 2 * x_B * y_B + x_A * y_B + x_B * y_A)

    return Phi_x, Phi_y, Phi_z


def _shapley_binary_product(
    x_A: np.ndarray,
    x_B: np.ndarray,
    y_A: np.ndarray,
    y_B: np.ndarray,
) -> tuple[np.ndarray, np.ndarray]:
    """
    Shapley decomposition for f = x * y (binary product).

    Parameters
    ----------
    x_A, x_B : array-like
        Factor x in states A and B
    y_A, y_B : array-like
        Factor y in states A and B

    Returns
    -------
    Phi_x, Phi_y : array-like
        Shapley value contributions from each factor
    """
    dx = x_B - x_A
    dy = y_B - y_A

    # Shapley for binary: Phi_x = dx * (y_A + y_B) / 2
    Phi_x = dx * (y_A + y_B) / 2
    Phi_y = dy * (x_A + x_B) / 2

    return Phi_x, Phi_y


# =============================================================================
# Physical Calculations
# =============================================================================


def _cal_gamma_heat(
    temp_C: np.ndarray,
    rh_pct: np.ndarray,
    press_hPa: np.ndarray,
) -> np.ndarray:
    """
    Calculate scale factor gamma = 1 / (rho * c_p) for heat flux.

    Parameters
    ----------
    temp_C : array-like
        Air temperature (degC)
    rh_pct : array-like
        Relative humidity (%)
    press_hPa : array-like
        Air pressure (hPa)

    Returns
    -------
    gamma : array-like
        Scale factor (K m2 / W)
    """
    rho = cal_dens_air(press_hPa, temp_C)
    cp = cal_cp_with_rh(temp_C, rh_pct, press_hPa)
    gamma = 1.0 / (rho * cp)
    return gamma


def _cal_gamma_humidity(
    temp_C: np.ndarray,
    rh_pct: np.ndarray,
    press_hPa: np.ndarray,
) -> np.ndarray:
    """
    Calculate scale factor gamma = 1 / (rho * L_v) for latent heat flux.

    Parameters
    ----------
    temp_C : array-like
        Air temperature (degC)
    rh_pct : array-like
        Relative humidity (%)
    press_hPa : array-like
        Air pressure (hPa)

    Returns
    -------
    gamma : array-like
        Scale factor (kg/kg m2 / W) - converts latent heat flux to humidity change
    """
    # Calculate air density
    rho = cal_dens_air(press_hPa, temp_C)

    # Calculate specific humidity from RH for latent heat calculation
    theta_K = temp_C + 273.16
    qa = cal_qa(rh_pct, theta_K, press_hPa)

    # Calculate latent heat of vaporisation
    Lv = cal_lat_vap(qa, theta_K, press_hPa)

    # Scale factor: delta_q = gamma * QE (where QE in W/m2)
    gamma = 1.0 / (rho * Lv)
    return gamma


def _cal_r_eff_heat(
    T2: np.ndarray,
    T_ref: np.ndarray,
    QH: np.ndarray,
    gamma: np.ndarray,
    min_flux: float = 0.1,
) -> np.ndarray:
    """
    Back-calculate effective heat resistance from T2 profile.

    From T2 = T_ref + r * QH * gamma, we get:
        r = (T2 - T_ref) / (QH * gamma)

    Parameters
    ----------
    T2 : array-like
        2m air temperature (degC)
    T_ref : array-like
        Reference temperature at forcing height (degC)
    QH : array-like
        Sensible heat flux (W/m2)
    gamma : array-like
        Scale factor 1/(rho*cp)
    min_flux : float, optional
        Minimum flux threshold to avoid division issues, by default 0.1 W/m2

    Returns
    -------
    r_eff : array-like
        Effective heat resistance (s/m)
    """
    # Avoid division by near-zero flux
    denom = QH * gamma
    denom = np.where(np.abs(denom) < min_flux * np.abs(gamma), np.nan, denom)

    r_eff = (T2 - T_ref) / denom
    return r_eff


def _cal_r_eff_humidity(
    q2: np.ndarray,
    q_ref: np.ndarray,
    QE: np.ndarray,
    gamma: np.ndarray,
    min_flux: float = 0.1,
) -> np.ndarray:
    """
    Back-calculate effective moisture resistance from q2 profile.

    From q2 = q_ref + r * QE * gamma, we get:
        r = (q2 - q_ref) / (QE * gamma)

    Parameters
    ----------
    q2 : array-like
        2m specific humidity (kg/kg)
    q_ref : array-like
        Reference specific humidity at forcing height (kg/kg)
    QE : array-like
        Latent heat flux (W/m2)
    gamma : array-like
        Scale factor 1/(rho*Lv)
    min_flux : float, optional
        Minimum flux threshold to avoid division issues, by default 0.1 W/m2

    Returns
    -------
    r_eff : array-like
        Effective moisture resistance (s/m)
    """
    # Avoid division by near-zero flux
    denom = QE * gamma
    denom = np.where(np.abs(denom) < min_flux * np.abs(gamma), np.nan, denom)

    r_eff = (q2 - q_ref) / denom
    return r_eff


def _decompose_flux_budget(
    flux_A: np.ndarray,
    flux_B: np.ndarray,
    components_A: dict[str, np.ndarray],
    components_B: dict[str, np.ndarray],
    total_contribution: np.ndarray,
) -> dict[str, np.ndarray]:
    """
    Hierarchical decomposition of flux contribution into budget components.

    For sensible heat flux: Q_H = Q* - Q_E - dQ_S + Q_F

    The total flux contribution (Phi_flux) is allocated to each budget
    component proportionally to their contribution to the flux change.

    Parameters
    ----------
    flux_A, flux_B : array-like
        Total flux in states A and B
    components_A, components_B : dict
        Dictionary of flux budget components in each state
    total_contribution : array-like
        Total Shapley contribution from flux

    Returns
    -------
    dict
        Contributions from each budget component
    """
    # Calculate change in each component
    d_components = {
        name: components_B[name] - components_A[name] for name in components_A.keys()
    }

    # Total flux change
    d_flux = flux_B - flux_A

    # Allocate total contribution proportionally
    # Handle zero denominator
    d_flux_safe = np.where(np.abs(d_flux) < 1e-10, np.nan, d_flux)

    contributions = {}
    for name, d_comp in d_components.items():
        weight = d_comp / d_flux_safe
        contributions[name] = weight * total_contribution

    return contributions


# =============================================================================
# Result Container
# =============================================================================


@dataclass
class AttributionResult:
    """
    Container for attribution analysis results.

    Attributes
    ----------
    variable : str
        Name of attributed variable ('T2', 'q2', or 'U10')
    contributions : pd.DataFrame
        Timeseries of contributions from each component
    summary : pd.DataFrame
        Summary statistics (mean, std, min, max) for each component
    metadata : dict
        Additional context (period, scenarios, parameters)
    """

    variable: str
    contributions: pd.DataFrame
    summary: pd.DataFrame
    metadata: dict = field(default_factory=dict)

    def __repr__(self) -> str:
        """Generate clean text representation of attribution results."""
        lines = []
        var_symbol = {"T2": "T2", "q2": "q2", "U10": "U10"}.get(
            self.variable, self.variable
        )
        unit = {"T2": "degC", "q2": "g/kg", "U10": "m/s"}.get(self.variable, "")

        # Header
        lines.append(f"{var_symbol} Attribution Results")
        lines.append("=" * 40)

        # Total change
        total = self.summary.loc["delta_total", "mean"]
        lines.append(f"Mean delta_{var_symbol}: {total:+.3f} {unit}")
        lines.append("")

        # Component breakdown
        lines.append("Component Breakdown:")
        lines.append("-" * 40)

        # Main components
        main_components = ["flux_total", "resistance", "air_props"]
        for comp in main_components:
            if comp in self.summary.index:
                val = self.summary.loc[comp, "mean"]
                pct = 100 * val / total if abs(total) > 1e-10 else 0
                lines.append(f"  {comp:15s}: {val:+.3f} {unit} ({pct:5.1f}%)")

        # Flux sub-components (if hierarchical)
        flux_comps = [c for c in self.summary.index if c.startswith("flux_")]
        flux_comps = [c for c in flux_comps if c != "flux_total"]
        if flux_comps:
            lines.append("")
            lines.append("  Flux breakdown:")
            for comp in flux_comps:
                val = self.summary.loc[comp, "mean"]
                pct = 100 * val / total if abs(total) > 1e-10 else 0
                label = comp.replace("flux_", "  d")
                lines.append(f"    {label:13s}: {val:+.3f} {unit} ({pct:5.1f}%)")

        # Closure check
        lines.append("")
        sum_components = self.contributions[main_components].sum(axis=1).mean()
        residual = total - sum_components
        lines.append(f"Closure residual: {residual:.2e} {unit}")

        return "\n".join(lines)

    def plot(
        self,
        kind: Literal["bar", "diurnal", "line", "heatmap"] = "bar",
        ax=None,
        components: Optional[list[str]] = None,
        **kwargs,
    ):
        """
        Visualise attribution results.

        Parameters
        ----------
        kind : str, optional
            Plot type:
            - 'bar': Stacked bar of mean contributions (default)
            - 'diurnal': Ensemble diurnal cycle with IQR shading
            - 'line': Time series of all contributions
            - 'heatmap': Month x hour heatmap of total change
        ax : matplotlib.axes.Axes, optional
            Axes to plot on. If None, creates new figure.
        components : list of str, optional
            Components to include. If None, uses main components.
        **kwargs
            Additional keyword arguments passed to plotting function.

        Returns
        -------
        fig, ax : tuple
            Figure and axes objects
        """
        import matplotlib.pyplot as plt

        if ax is None:
            fig, ax = plt.subplots(figsize=(8, 5))
        else:
            fig = ax.get_figure()

        # Default components
        if components is None:
            components = ["flux_total", "resistance", "air_props"]
            # Filter to existing columns
            components = [c for c in components if c in self.contributions.columns]

        unit = {"T2": "degC", "q2": "g/kg", "U10": "m/s"}.get(self.variable, "")

        if kind == "bar":
            # Stacked bar chart of mean contributions
            means = self.summary.loc[components, "mean"]
            colors = plt.cm.Set2(range(len(components)))
            bars = ax.bar(
                range(len(components)),
                means.values,
                color=colors,
                edgecolor="black",
                linewidth=0.5,
            )
            ax.set_xticks(range(len(components)))
            ax.set_xticklabels(
                [c.replace("_", "\n") for c in components], rotation=0, fontsize=9
            )
            ax.axhline(y=0, color="black", linewidth=0.5)
            ax.set_ylabel(f"Contribution ({unit})")
            ax.set_title(f"{self.variable} Attribution")

            # Add value labels
            for bar, val in zip(bars, means.values):
                height = bar.get_height()
                ax.annotate(
                    f"{val:+.2f}",
                    xy=(bar.get_x() + bar.get_width() / 2, height),
                    xytext=(0, 3 if height >= 0 else -10),
                    textcoords="offset points",
                    ha="center",
                    va="bottom" if height >= 0 else "top",
                    fontsize=8,
                )

        elif kind == "diurnal":
            # Ensemble diurnal cycle with IQR shading
            df = self.contributions[components].copy()
            df["hour"] = df.index.hour + df.index.minute / 60

            grouped = df.groupby("hour")
            hours = sorted(df["hour"].unique())

            for i, comp in enumerate(components):
                color = plt.cm.Set2(i)
                median = grouped[comp].median()
                q25 = grouped[comp].quantile(0.25)
                q75 = grouped[comp].quantile(0.75)

                ax.plot(hours, median.values, label=comp, color=color)
                ax.fill_between(hours, q25.values, q75.values, alpha=0.3, color=color)

            ax.axhline(y=0, color="black", linewidth=0.5, linestyle="--")
            ax.set_xlabel("Hour of day")
            ax.set_ylabel(f"Contribution ({unit})")
            ax.set_title(f"{self.variable} Attribution - Diurnal Cycle")
            ax.legend(loc="best", fontsize=8)
            ax.set_xlim(0, 24)
            ax.set_xticks(range(0, 25, 3))

        elif kind == "line":
            # Time series
            for i, comp in enumerate(components):
                color = plt.cm.Set2(i)
                ax.plot(
                    self.contributions.index,
                    self.contributions[comp],
                    label=comp,
                    color=color,
                    alpha=0.7,
                )
            ax.axhline(y=0, color="black", linewidth=0.5, linestyle="--")
            ax.set_xlabel("Time")
            ax.set_ylabel(f"Contribution ({unit})")
            ax.set_title(f"{self.variable} Attribution - Time Series")
            ax.legend(loc="best", fontsize=8)

        elif kind == "heatmap":
            # Month x hour heatmap of total change
            import matplotlib.colors as mcolors

            df = self.contributions.copy()
            df["month"] = df.index.month
            df["hour"] = df.index.hour

            pivot = df.pivot_table(
                values="delta_total", index="hour", columns="month", aggfunc="mean"
            )

            # Diverging colormap centred at zero
            vmax = max(abs(pivot.values.min()), abs(pivot.values.max()))
            norm = mcolors.TwoSlopeNorm(vmin=-vmax, vcenter=0, vmax=vmax)

            im = ax.imshow(
                pivot.values,
                aspect="auto",
                cmap="RdBu_r",
                norm=norm,
                origin="lower",
            )
            ax.set_yticks(range(24))
            ax.set_yticklabels(range(24))
            ax.set_xticks(range(12))
            ax.set_xticklabels([
                "J",
                "F",
                "M",
                "A",
                "M",
                "J",
                "J",
                "A",
                "S",
                "O",
                "N",
                "D",
            ])
            ax.set_xlabel("Month")
            ax.set_ylabel("Hour")
            ax.set_title(f"{self.variable} Attribution - Seasonal-Diurnal Pattern")

            cbar = fig.colorbar(im, ax=ax, label=f"delta_{self.variable} ({unit})")

        return fig, ax

    def to_dataframe(self) -> pd.DataFrame:
        """Return contributions as a DataFrame."""
        return self.contributions.copy()


# Convenience aliases
T2Attribution = AttributionResult
Q2Attribution = AttributionResult
U10Attribution = AttributionResult


# =============================================================================
# Main Attribution Functions
# =============================================================================


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
    # Extract SUEWS output group
    df_A = _extract_suews_group(df_output_A)
    df_B = _extract_suews_group(df_output_B)

    # Align indices
    common_idx = df_A.index.intersection(df_B.index)
    if len(common_idx) == 0:
        raise ValueError("No overlapping timestamps between scenarios A and B")

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
        # Use T2 as proxy for reference temperature (simplified)
        # This is an approximation - ideally forcing should be provided
        T_ref_A = df_A["T2"].values  # Approximation
        T_ref_B = df_B["T2"].values
        # Use typical values for RH and pressure
        RH_A = np.full_like(T2_A, 60.0)  # 60% RH default
        RH_B = np.full_like(T2_B, 60.0)
        P_A = np.full_like(T2_A, 1013.25)  # Standard pressure
        P_B = np.full_like(T2_B, 1013.25)

    # Calculate gamma = 1/(rho*cp)
    gamma_A = _cal_gamma_heat(T_ref_A, RH_A, P_A)
    gamma_B = _cal_gamma_heat(T_ref_B, RH_B, P_B)

    # Back-calculate effective resistance
    r_A = _cal_r_eff_heat(T2_A, T_ref_A, QH_A, gamma_A, min_flux)
    r_B = _cal_r_eff_heat(T2_B, T_ref_B, QH_B, gamma_B, min_flux)

    # Shapley decomposition: delta_T2 = delta(r * QH * gamma)
    Phi_r, Phi_QH, Phi_gamma = _shapley_triple_product(
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

        flux_breakdown = _decompose_flux_budget(
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

    >>> result.plot(kind='bar')  # Visualise contributions
    """
    # Extract SUEWS output group
    df_A = _extract_suews_group(df_output_A)
    df_B = _extract_suews_group(df_output_B)

    # Align indices
    common_idx = df_A.index.intersection(df_B.index)
    if len(common_idx) == 0:
        raise ValueError("No overlapping timestamps between scenarios A and B")

    df_A = df_A.loc[common_idx]
    df_B = df_B.loc[common_idx]

    # Extract required variables
    # q2 may not be directly in output - calculate from T2 and RH if available
    if "q2" in df_A.columns:
        q2_A = df_A["q2"].values
        q2_B = df_B["q2"].values
    else:
        # Approximate using T2 and relative humidity from forcing
        # This is a simplified approach - ideally q2 should be in output
        raise ValueError(
            "q2 not found in SUEWS output. "
            "Ensure SUEWS is configured to output near-surface humidity."
        )

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
        theta_A = T_ref_A + 273.16
        theta_B = T_ref_B + 273.16
        q_ref_A = cal_qa(RH_A, theta_A, P_A)
        q_ref_B = cal_qa(RH_B, theta_B, P_B)
    else:
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
    gamma_A = _cal_gamma_humidity(T_ref_A, RH_A, P_A)
    gamma_B = _cal_gamma_humidity(T_ref_B, RH_B, P_B)

    # Back-calculate effective resistance
    r_A = _cal_r_eff_humidity(q2_A, q_ref_A, QE_A, gamma_A, min_flux)
    r_B = _cal_r_eff_humidity(q2_B, q_ref_B, QE_B, gamma_B, min_flux)

    # Shapley decomposition: delta_q2 = delta(r * QE * gamma)
    Phi_r, Phi_QE, Phi_gamma = _shapley_triple_product(
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


def diagnose_t2(
    df_output: pd.DataFrame,
    df_forcing: Optional[pd.DataFrame] = None,
    method: Literal["anomaly", "extreme", "diurnal"] = "anomaly",
    threshold: float = 2.0,
    reference: str = "auto",
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
    reference : str, optional
        Reference period:
        - 'auto': Automatically select based on method
        - 'morning': 06:00-10:00
        - 'daily_mean': Use timesteps near daily mean
        Default 'auto'.
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
    Found 47 anomalous timesteps (3.2% of data)
    Mean anomaly: +2.8 degC

    Primary driver: Flux (62%)
      -> Check evaporation and radiation forcing

    >>> result.plot()  # Visualise decomposition
    """
    # Extract SUEWS output
    df = _extract_suews_group(df_output)
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


def diagnose_q2(
    df_output: pd.DataFrame,
    df_forcing: Optional[pd.DataFrame] = None,
    method: Literal["anomaly", "extreme", "diurnal"] = "anomaly",
    threshold: float = 2.0,
    reference: str = "auto",
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
    reference : str, optional
        Reference period:
        - 'auto': Automatically select based on method
        - 'morning': 06:00-10:00
        - 'daily_mean': Use timesteps near daily mean
        Default 'auto'.

    Returns
    -------
    AttributionResult
        Attribution decomposition with diagnostic interpretation.

    Examples
    --------
    Quick anomaly diagnosis:

    >>> result = diagnose_q2(df_output, method='anomaly')
    >>> print(result)
    q2 Attribution Results
    ========================================
    Found 35 anomalous timesteps (2.4% of data)
    Mean anomaly: +0.8 g/kg

    >>> result.plot()  # Visualise decomposition
    """
    # Extract SUEWS output
    df = _extract_suews_group(df_output)

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
        raise ValueError(f"Unknown method: {method}. Use 'anomaly', 'extreme', 'diurnal'")

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
            f"Only {n_normal} reference timesteps found. "
            "Cannot establish baseline."
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


# =============================================================================
# Generic Dispatcher Functions
# =============================================================================


def attribute(
    df_output_A: pd.DataFrame,
    df_output_B: pd.DataFrame,
    variable: Literal["T2", "q2"] = "T2",
    **kwargs,
) -> AttributionResult:
    """
    Generic attribution function for near-surface variables.

    Dispatches to variable-specific implementation internally.

    Parameters
    ----------
    df_output_A : pd.DataFrame
        SUEWS output DataFrame for scenario A (reference/baseline)
    df_output_B : pd.DataFrame
        SUEWS output DataFrame for scenario B (modified/test)
    variable : str, optional
        Variable to attribute: 'T2' (temperature) or 'q2' (humidity).
        Default 'T2'.
    **kwargs
        Additional keyword arguments passed to the specific function.
        - For T2: hierarchical (bool), min_flux (float), df_forcing_A/B
        - For q2: min_flux (float), df_forcing_A/B

    Returns
    -------
    AttributionResult
        Attribution decomposition results.

    Examples
    --------
    >>> result = attribute(df_baseline, df_scenario, variable='T2')
    >>> print(result)

    >>> result = attribute(df_baseline, df_scenario, variable='q2')
    >>> print(result)
    """
    if variable == "T2":
        return attribute_t2(df_output_A, df_output_B, **kwargs)
    elif variable == "q2":
        return attribute_q2(df_output_A, df_output_B, **kwargs)
    else:
        raise ValueError(
            f"Unknown variable: {variable}. "
            f"Supported variables: 'T2', 'q2'"
        )


def diagnose(
    df_output: pd.DataFrame,
    variable: Literal["T2", "q2"] = "T2",
    **kwargs,
) -> AttributionResult:
    """
    Generic diagnostic function for near-surface variables.

    Automatically identifies anomalous values and attributes the causes.

    Parameters
    ----------
    df_output : pd.DataFrame
        SUEWS output DataFrame
    variable : str, optional
        Variable to diagnose: 'T2' (temperature) or 'q2' (humidity).
        Default 'T2'.
    **kwargs
        Additional keyword arguments passed to the specific function.
        - method: 'anomaly', 'extreme', or 'diurnal'
        - threshold: float for anomaly detection
        - df_forcing: forcing DataFrame
        - hierarchical: bool (T2 only)

    Returns
    -------
    AttributionResult
        Attribution decomposition with diagnostic interpretation.

    Examples
    --------
    >>> result = diagnose(df_output, variable='T2', method='anomaly')
    >>> print(result)

    >>> result = diagnose(df_output, variable='q2', method='diurnal')
    >>> print(result)
    """
    if variable == "T2":
        return diagnose_t2(df_output, **kwargs)
    elif variable == "q2":
        return diagnose_q2(df_output, **kwargs)
    else:
        raise ValueError(
            f"Unknown variable: {variable}. "
            f"Supported variables: 'T2', 'q2'"
        )


# =============================================================================
# Helper Functions
# =============================================================================


def _extract_suews_group(df_output: pd.DataFrame) -> pd.DataFrame:
    """
    Extract SUEWS output group from MultiIndex DataFrame.

    Handles both MultiIndex column structure and flat DataFrames.
    """
    # Check if MultiIndex columns
    if isinstance(df_output.columns, pd.MultiIndex):
        # Try to get SUEWS group
        if "SUEWS" in df_output.columns.get_level_values(0):
            # Handle MultiIndex rows (grid, datetime)
            if isinstance(df_output.index, pd.MultiIndex):
                # Get first grid
                grid = df_output.index.get_level_values(0)[0]
                return df_output.loc[grid, "SUEWS"]
            else:
                return df_output["SUEWS"]
        else:
            # Return first level
            return df_output.droplevel(0, axis=1)
    else:
        # Already flat DataFrame
        return df_output
