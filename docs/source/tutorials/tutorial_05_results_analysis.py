"""
Analysing Simulation Results
============================

Comprehensive analysis and validation of SUEWS outputs.

Understanding and analysing SUEWS output is essential for scientific
interpretation and model validation. This tutorial covers:

1. **Output structure** - Navigating the results DataFrame
2. **Statistical analysis** - Energy and water balance calculations
3. **Diagnostic plots** - Visualising model behaviour
4. **Validation** - Comparing with observations
5. **Export** - Saving results for further use

**Prerequisites**: Complete :doc:`tutorial_01_quick_start` first.
"""

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

from supy import SUEWSSimulation

# %%
# Load and Run Simulation
# -----------------------
#
# First, run a simulation to generate results for analysis.

sim = SUEWSSimulation.from_sample_data()
sim.run()

print("Simulation complete!")
print(f"Output period: {sim.results.index[0]} to {sim.results.index[-1]}")
print(f"Time steps: {len(sim.results)}")

# %%
# Understanding Output Structure
# ------------------------------
#
# SUEWS results use MultiIndex columns organised by output groups:
#
# - **SUEWS**: Primary energy and water balance (QN, QH, QE, QS, QF, etc.)
# - **DailyState**: Daily summary variables (LAI, GDD, snow density)
# - **snow**: Detailed snow variables by surface type
# - **RSL**: Roughness sublayer profiles
#
# Access variables using ``get_variable()`` or direct MultiIndex indexing.

results = sim.results

# Method 1: get_variable() - recommended
qh = sim.get_variable("QH", group="SUEWS")
print(f"QH shape: {qh.shape}")

# Method 2: Direct MultiIndex access
qn = results[("SUEWS", "QN")]
print(f"QN shape: {qn.shape}")

# List available groups and variables
print(f"\nAvailable groups: {results.columns.get_level_values('group').unique().tolist()}")
print(f"SUEWS variables (first 10): {results['SUEWS'].columns.tolist()[:10]}")


# %%
# Helper Function for Variable Access
# -----------------------------------
#
# Create a helper to simplify extracting multiple variables.


def get_var(sim, name, group="SUEWS"):
    """Extract a single variable as a Series."""
    return sim.get_variable(name, group=group).iloc[:, 0]


def get_vars(sim, names, group="SUEWS"):
    """Extract multiple variables as a DataFrame."""
    return pd.DataFrame({name: get_var(sim, name, group) for name in names})


# Extract energy balance components
energy_vars = ["QN", "QF", "QS", "QE", "QH"]
energy_df = get_vars(sim, energy_vars)

print("Energy balance components:")
print(energy_df.head())

# %%
# Basic Statistics
# ----------------
#
# Calculate summary statistics for the energy balance.

print("Annual Energy Balance Statistics (W/m2):")
print(energy_df.describe().round(1))

# Seasonal means
seasonal = energy_df.groupby(energy_df.index.quarter).mean()
seasonal.index = ["Winter (DJF)", "Spring (MAM)", "Summer (JJA)", "Autumn (SON)"]
print("\nSeasonal Means (W/m2):")
print(seasonal.round(1))

# %%
# Energy Balance Closure
# ----------------------
#
# Verify that the energy balance closes: QN + QF = QS + QE + QH

energy_in = get_var(sim, "QN") + get_var(sim, "QF")
energy_out = get_var(sim, "QS") + get_var(sim, "QE") + get_var(sim, "QH")
residual = energy_in - energy_out

print("Energy Balance Closure Check:")
print(f"  Mean residual: {residual.mean():.4f} W/m2")
print(f"  Std residual:  {residual.std():.4f} W/m2")
print(f"  Max |residual|: {residual.abs().max():.4f} W/m2")
print("\nNote: SUEWS enforces closure by design. Non-zero residuals")
print("indicate numerical precision limits only.")

# %%
# Water Balance Analysis
# ----------------------
#
# Calculate annual water balance: P + I = E + R + D + dS

rain = get_var(sim, "Rain")
evap = get_var(sim, "Evap")
runoff = get_var(sim, "RO")
drainage = get_var(sim, "Drainage")
irr = get_var(sim, "Irr")
storage_change = get_var(sim, "TotCh")

# Annual totals (mm/year)
print("Annual Water Balance (mm):")
print("  Inputs:")
print(f"    Precipitation: {rain.sum():.1f}")
print(f"    Irrigation:    {irr.sum():.1f}")
print("  Outputs:")
print(f"    Evaporation:   {evap.sum():.1f}")
print(f"    Runoff:        {runoff.sum():.1f}")
print(f"    Drainage:      {drainage.sum():.1f}")
print(f"  Storage change:  {storage_change.sum():.1f}")

water_residual = (rain.sum() + irr.sum()) - evap.sum() - runoff.sum() - drainage.sum() - storage_change.sum()
print(f"  Residual:        {water_residual:.1f}")

# %%
# Energy Balance Time Series
# --------------------------
#
# Visualise energy fluxes over time.

fig, axes = plt.subplots(2, 2, figsize=(14, 10))

# 1. Daily energy fluxes
ax = axes[0, 0]
daily_energy = energy_df.resample("D").mean()
daily_energy.plot(ax=ax)
ax.set_ylabel("Energy Flux (W/m2)")
ax.set_title("Daily Mean Energy Fluxes")
ax.legend(loc="upper right")
ax.axhline(y=0, color="k", linestyle="--", alpha=0.3)

# 2. Monthly energy partitioning
ax = axes[0, 1]
monthly_means = energy_df[["QS", "QE", "QH"]].groupby(energy_df.index.month).mean()
monthly_means.plot(kind="bar", ax=ax)
ax.set_xlabel("Month")
ax.set_ylabel("Energy Flux (W/m2)")
ax.set_title("Monthly Energy Partitioning")
ax.legend(loc="upper right")

# 3. Summer diurnal cycle
ax = axes[1, 0]
summer_mask = energy_df.index.month.isin([6, 7, 8])
summer_energy = energy_df[summer_mask]
hourly_summer = summer_energy.groupby(summer_energy.index.hour).mean()
hourly_summer.plot(ax=ax, marker="o", markersize=3)
ax.set_xlabel("Hour of Day")
ax.set_ylabel("Energy Flux (W/m2)")
ax.set_title("Summer Diurnal Cycle")
ax.legend(loc="upper right")
ax.axhline(y=0, color="k", linestyle="--", alpha=0.3)

# 4. Bowen ratio (QH/QE) over time
ax = axes[1, 1]
bowen = get_var(sim, "QH") / get_var(sim, "QE").replace(0, np.nan)
bowen_daily = bowen.resample("D").mean()
bowen_daily.plot(ax=ax)
ax.set_ylabel("Bowen Ratio (QH/QE)")
ax.set_title("Daily Bowen Ratio")
ax.set_ylim(-2, 5)
ax.axhline(y=1, color="r", linestyle="--", alpha=0.5, label="Bowen=1")
ax.legend()

plt.tight_layout()

# %%
# Temperature Analysis
# --------------------
#
# Analyse air and surface temperature patterns.

t2 = get_var(sim, "T2")
tsurf = get_var(sim, "Tsurf")

fig, axes = plt.subplots(2, 2, figsize=(14, 10))

# 1. Temperature time series
ax = axes[0, 0]
t2.resample("D").mean().plot(ax=ax, label="T2 (2m air)")
tsurf.resample("D").mean().plot(ax=ax, label="Tsurf (surface)")
ax.set_ylabel("Temperature (degC)")
ax.set_title("Daily Mean Temperatures")
ax.legend()

# 2. Temperature distribution
ax = axes[0, 1]
ax.hist(t2.dropna(), bins=50, alpha=0.7, label="T2", density=True)
ax.hist(tsurf.dropna(), bins=50, alpha=0.7, label="Tsurf", density=True)
ax.set_xlabel("Temperature (degC)")
ax.set_ylabel("Density")
ax.set_title("Temperature Distribution")
ax.legend()

# 3. Diurnal temperature cycle by season
ax = axes[1, 0]
for season_name, months in [
    ("Winter", [12, 1, 2]),
    ("Spring", [3, 4, 5]),
    ("Summer", [6, 7, 8]),
    ("Autumn", [9, 10, 11]),
]:
    mask = t2.index.month.isin(months)
    hourly = t2[mask].groupby(t2[mask].index.hour).mean()
    ax.plot(hourly.index, hourly.values, marker="o", markersize=3, label=season_name)
ax.set_xlabel("Hour of Day")
ax.set_ylabel("T2 (degC)")
ax.set_title("Seasonal Diurnal Temperature Cycles")
ax.legend()

# 4. Surface-air temperature difference
ax = axes[1, 1]
delta_t = tsurf - t2
delta_t_hourly = delta_t.groupby(delta_t.index.hour).mean()
ax.plot(delta_t_hourly.index, delta_t_hourly.values, "ko-")
ax.set_xlabel("Hour of Day")
ax.set_ylabel("Tsurf - T2 (degC)")
ax.set_title("Surface-Air Temperature Difference")
ax.axhline(y=0, color="r", linestyle="--", alpha=0.5)
ax.fill_between(delta_t_hourly.index, 0, delta_t_hourly.values, where=delta_t_hourly.values > 0, alpha=0.3, color="red", label="Surface warmer")
ax.fill_between(delta_t_hourly.index, 0, delta_t_hourly.values, where=delta_t_hourly.values < 0, alpha=0.3, color="blue", label="Air warmer")
ax.legend()

plt.tight_layout()

# %%
# Validation Statistics
# ---------------------
#
# Calculate standard validation metrics for model-observation comparison.


def validation_statistics(observed, modelled):
    """Calculate validation statistics.

    Parameters
    ----------
    observed : Series
        Observed values
    modelled : Series
        Modelled values (aligned with observed)

    Returns
    -------
    dict
        Validation statistics including bias, RMSE, R2, and IoA
    """
    from scipy import stats

    # Align data
    obs, mod = observed.align(modelled, join="inner")
    obs = obs.dropna()
    mod = mod.loc[obs.index].dropna()

    # Re-align after dropna
    obs, mod = obs.align(mod, join="inner")

    n = len(obs)
    if n < 3:
        return {"n": n, "error": "Insufficient data"}

    mean_obs = obs.mean()
    mean_mod = mod.mean()

    # Bias
    bias = mean_mod - mean_obs

    # RMSE
    rmse = np.sqrt(((mod - obs) ** 2).mean())

    # Correlation
    r, p = stats.pearsonr(obs, mod)

    # Mean Absolute Error
    mae = (mod - obs).abs().mean()

    # Index of Agreement (Willmott)
    numer = ((mod - obs) ** 2).sum()
    denom = ((mod - mean_obs).abs() + (obs - mean_obs).abs()) ** 2
    ioa = 1 - numer / denom.sum() if denom.sum() > 0 else np.nan

    return {
        "n": n,
        "mean_obs": mean_obs,
        "mean_mod": mean_mod,
        "bias": bias,
        "rmse": rmse,
        "mae": mae,
        "r": r,
        "r2": r**2,
        "p_value": p,
        "ioa": ioa,
    }


# Example: Compare modelled T2 with forcing Tair (as proxy for "observations")
# In practice, you would load actual observation data
tair_forcing = sim.forcing.df["Tair"]
t2_model = get_var(sim, "T2")

stats_t2 = validation_statistics(tair_forcing, t2_model)
print("T2 vs Forcing Tair (demonstration):")
for key, val in stats_t2.items():
    if isinstance(val, float):
        print(f"  {key}: {val:.3f}")
    else:
        print(f"  {key}: {val}")

# %%
# Validation Scatter Plot
# -----------------------
#
# Create a scatter plot comparing model output with observations.


def validation_scatter(observed, modelled, variable_name, units="", ax=None):
    """Create validation scatter plot with statistics."""
    if ax is None:
        fig, ax = plt.subplots(figsize=(8, 8))

    obs, mod = observed.align(modelled, join="inner")
    obs = obs.dropna()
    mod = mod.loc[obs.index].dropna()
    obs, mod = obs.align(mod, join="inner")

    ax.scatter(obs, mod, alpha=0.1, s=5)

    # 1:1 line
    lims = [min(obs.min(), mod.min()), max(obs.max(), mod.max())]
    ax.plot(lims, lims, "k--", label="1:1 line", linewidth=2)

    # Regression line
    slope, intercept = np.polyfit(obs, mod, 1)
    ax.plot(lims, [slope * x + intercept for x in lims], "r-", label=f"Fit: y = {slope:.2f}x + {intercept:.2f}", linewidth=2)

    # Statistics annotation
    stats_dict = validation_statistics(observed, modelled)
    stats_text = f"n = {stats_dict['n']}\n" f"R² = {stats_dict['r2']:.3f}\n" f"RMSE = {stats_dict['rmse']:.2f}\n" f"Bias = {stats_dict['bias']:.2f}"
    ax.text(0.05, 0.95, stats_text, transform=ax.transAxes, verticalalignment="top", fontsize=10, bbox=dict(boxstyle="round", facecolor="wheat", alpha=0.8))

    ax.set_xlabel(f"Observed {variable_name} ({units})")
    ax.set_ylabel(f"Modelled {variable_name} ({units})")
    ax.set_title(f"{variable_name} Validation")
    ax.legend(loc="lower right")
    ax.set_aspect("equal", adjustable="box")

    return ax


fig, ax = plt.subplots(figsize=(8, 8))
validation_scatter(tair_forcing, t2_model, "Air Temperature", "degC", ax=ax)
plt.tight_layout()

# %%
# Exporting Results
# -----------------
#
# Save results in various formats for further analysis.

# Export to CSV
export_vars = ["QN", "QH", "QE", "QS", "T2", "RH2"]
export_df = get_vars(sim, export_vars)
# export_df.to_csv('suews_output.csv')  # Uncomment to save
print(f"Export DataFrame shape: {export_df.shape}")
print(f"Ready to save with: export_df.to_csv('suews_output.csv')")

# Export final state for restart runs
final_state = sim.state_final
# final_state.to_csv('final_state.csv')  # Uncomment to save
print(f"\nFinal state shape: {final_state.shape}")
print("Ready to save with: final_state.to_csv('final_state.csv')")

# %%
# Summary
# -------
#
# Key analysis techniques covered:
#
# 1. **Access variables** with ``get_variable()`` or MultiIndex indexing
# 2. **Check balance closure** - energy and water budgets should close
# 3. **Seasonal patterns** - use ``groupby()`` with month/quarter
# 4. **Diurnal patterns** - use ``groupby()`` with hour
# 5. **Validation metrics** - RMSE, bias, R², Index of Agreement
# 6. **Export results** - CSV for spreadsheets, Parquet for large datasets
#
# For external model coupling, see :doc:`tutorial_06_external_coupling`.
