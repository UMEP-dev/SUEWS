"""
SUEWS Quick Start Tutorial
==========================

Essential SUEWS workflow using the modern SuPy (Python) interface.

This tutorial demonstrates the complete workflow for running urban climate
simulations with SUEWS:

1. Load sample data
2. Run simulation
3. Explore results (statistics, plotting, resampling)

**What is SUEWS?**

SUEWS (Surface Urban Energy and Water Balance Scheme) is an urban climate model
that simulates energy and water fluxes in urban environments. **SuPy** is the
modern Python interface that provides powerful data analysis capabilities and
seamless integration with the scientific Python ecosystem.
"""

import os

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import supy as sp
from supy import SUEWSSimulation

# Detect CI environment for reduced computation
_IS_CI = os.environ.get("CI", "false").lower() == "true"

# %%
# Load Sample Data
# ----------------
#
# SuPy includes built-in sample data to get you started immediately.
# Load sample data for the simulation using the modern OOP API.

sim_sample = SUEWSSimulation.from_sample_data()

# Extract state and forcing for examination
df_state_init = sim_sample.state_init
df_forcing = sim_sample.forcing.df  # Use .df property to get DataFrame

print("Sample data loaded successfully!")
print(f"Grid ID: {df_state_init.index[0]}")
print(f"Forcing period: {df_forcing.index[0]} to {df_forcing.index[-1]}")
print(f"Time steps: {len(df_forcing)}")

# Use reduced data period for CI, full year for local
if _IS_CI:
    df_forcing = df_forcing.loc["2012-01":"2012-03"].iloc[1:]  # 3 months for CI
else:
    df_forcing = df_forcing.loc["2012"].iloc[1:]  # Full year for local

grid = df_state_init.index[0]

# %%
# Understanding Input Data
# ------------------------
#
# SUEWS requires two main input datasets:
#
# - ``df_state_init``: Initial conditions and site configuration
# - ``df_forcing``: Meteorological forcing data (time series)
#
# Let's examine the key parameters.

# Surface characteristics: building and tree heights
print("Building and tree heights:")
print(df_state_init.loc[:, ["bldgh", "evetreeh", "dectreeh"]])

# Surface fractions by land cover type
print("\nSurface fractions:")
print(df_state_init.filter(like="sfr_surf"))

# %%
# Visualise Forcing Data
# ----------------------
#
# The forcing data drives the simulation. Let's plot the key meteorological
# variables for the simulation period.

list_var_forcing = ["kdown", "Tair", "RH", "pres", "U", "rain"]
dict_var_label = {
    "kdown": r"Incoming Solar Radiation ($\mathrm{W\ m^{-2}}$)",
    "Tair": r"Air Temperature ($^\circ$C)",
    "RH": "Relative Humidity (%)",
    "pres": "Air Pressure (hPa)",
    "rain": "Rainfall (mm)",
    "U": r"Wind Speed ($\mathrm{m\ s^{-1}}$)",
}

# Resample to hourly for cleaner plots
df_plot_forcing_x = (
    df_forcing.loc[:, list_var_forcing].copy().shift(-1).dropna(how="any")
)
df_plot_forcing = df_plot_forcing_x.resample("1h").mean()
df_plot_forcing["rain"] = df_plot_forcing_x["rain"].resample("1h").sum()

fig, axes = plt.subplots(6, 1, figsize=(10, 12), sharex=True)
for ax, var in zip(axes, list_var_forcing):
    df_plot_forcing[var].plot(ax=ax, legend=False)
    ax.set_ylabel(dict_var_label[var])
fig.tight_layout()
plt.show()

# %%
# Modify Input Parameters
# -----------------------
#
# Since SuPy uses pandas DataFrames, you can easily modify input parameters.
# Here we demonstrate modifying surface fractions.

# View original surface fractions
print("Original surface fractions:")
print(df_state_init.loc[:, "sfr_surf"])

# Modify surface fractions (example)
df_state_init.loc[:, "sfr_surf"] = [0.1, 0.1, 0.2, 0.3, 0.25, 0.05, 0]

print("\nModified surface fractions:")
print(df_state_init.loc[:, "sfr_surf"])

# %%
# Run Simulation
# --------------
#
# With forcing data and initial conditions ready, run the SUEWS simulation.
# Results are accessible via ``sim.results`` and ``sim.state_final``.

# Update simulation with potentially modified forcing and run
sim_sample.update_forcing(df_forcing)
sim_sample.run()

# Access results
df_output = sim_sample.results
df_state_final = sim_sample.state_final

print(f"Simulation complete: {len(df_output)} timesteps")
print(f"Output groups: {list(df_output.columns.levels[0])}")

# %%
# Explore Results: Statistics
# ---------------------------
#
# Use pandas' built-in methods for quick statistical summaries of the
# energy balance components.

df_output_suews = df_output["SUEWS"]
df_output_suews.loc[:, ["QN", "QS", "QH", "QE", "QF"]].describe()

# %%
# Explore Results: Weekly Energy Balance
# --------------------------------------
#
# Plot the surface energy balance for one week to see diurnal patterns.

dict_var_disp = {
    "QN": r"$Q^*$",
    "QS": r"$\Delta Q_S$",
    "QE": "$Q_E$",
    "QH": "$Q_H$",
    "QF": "$Q_F$",
}

# Select first week of available data for plotting
start_date = df_output_suews.index.get_level_values("datetime")[0]
end_date = start_date + pd.Timedelta(days=7)

fig, ax = plt.subplots(figsize=(10, 4))
(
    df_output_suews.loc[grid]
    .loc[start_date:end_date, ["QN", "QS", "QE", "QH", "QF"]]
    .rename(columns=dict_var_disp)
    .plot(ax=ax)
)
ax.set_xlabel("Date")
ax.set_ylabel(r"Flux ($\mathrm{W\ m^{-2}}$)")
ax.set_title("Surface Energy Balance (One Week)")
ax.legend()
plt.tight_layout()
plt.show()

# sphinx_gallery_thumbnail_number = 2

# %%
# Temporal Resampling: Daily Patterns
# -----------------------------------
#
# SUEWS runs at 5-minute intervals. Resampling to daily values reveals
# seasonal patterns.

rsmp_1d = df_output_suews.loc[grid].resample("1d")
df_1d_mean = rsmp_1d.mean()
df_1d_sum = rsmp_1d.sum()

# Plot daily mean energy balance
fig, ax = plt.subplots(figsize=(10, 4))
(
    df_1d_mean.loc[:, ["QN", "QS", "QE", "QH", "QF"]]
    .rename(columns=dict_var_disp)
    .plot(ax=ax)
)
ax.set_xlabel("Date")
ax.set_ylabel(r"Mean Flux ($\mathrm{W\ m^{-2}}$)")
ax.set_title("Daily Mean Surface Energy Balance")
ax.legend()
plt.tight_layout()
plt.show()

# %%
# Radiation and Water Balance
# ---------------------------
#
# Examine radiation components and water balance using daily aggregates.

dict_var_disp_full = {
    "QN": r"$Q^*$",
    "Kdown": r"$K_{\downarrow}$",
    "Kup": r"$K_{\uparrow}$",
    "Ldown": r"$L_{\downarrow}$",
    "Lup": r"$L_{\uparrow}$",
    "Rain": "$P$",
    "Irr": "$I$",
    "Evap": "$E$",
    "RO": "$R$",
    "TotCh": r"$\Delta S$",
}

fig, axes = plt.subplots(2, 1, figsize=(10, 6), sharex=True)

# Radiation balance
(
    df_1d_mean.loc[:, ["QN", "Kdown", "Kup", "Ldown", "Lup"]]
    .rename(columns=dict_var_disp_full)
    .plot(ax=axes[0])
)
axes[0].set_ylabel(r"Mean Flux ($\mathrm{W\ m^{-2}}$)")
axes[0].set_title("Radiation Balance")
axes[0].legend()

# Water balance
(
    df_1d_sum.loc[:, ["Rain", "Irr", "Evap", "RO", "TotCh"]]
    .rename(columns=dict_var_disp_full)
    .plot(ax=axes[1])
)
axes[1].set_xlabel("Date")
axes[1].set_ylabel("Water Amount (mm)")
axes[1].set_title("Surface Water Balance")
axes[1].legend()

plt.tight_layout()
plt.show()

# %%
# Monthly Patterns
# ----------------
#
# Aggregate to monthly values for seasonal overview using bar charts.

df_plot = df_output_suews.loc[grid].copy()
df_plot.index = df_plot.index.set_names("Month")
rsmp_1M = df_plot.shift(-1).dropna(how="all").resample("1ME")
df_1M_mean = rsmp_1M.mean()
df_1M_sum = rsmp_1M.sum()
# Convert index to period for better month display
df_1M_mean.index = df_1M_mean.index.to_period("M")
df_1M_sum.index = df_1M_sum.index.to_period("M")

# Month names for labels
name_mon = [x.strftime("%b") for x in rsmp_1M.groups]

fig, axes = plt.subplots(2, 1, sharex=True, figsize=(10, 6))

# Monthly energy balance
(
    df_1M_mean.loc[:, ["QN", "QS", "QE", "QH", "QF"]]
    .rename(columns=dict_var_disp)
    .plot(ax=axes[0], kind="bar")
)
axes[0].set_ylabel(r"Mean Flux ($\mathrm{W\ m^{-2}}$)")
axes[0].set_title("Monthly Surface Energy Balance")
axes[0].legend()

# Monthly water balance
(
    df_1M_sum.loc[:, ["Rain", "Irr", "Evap", "RO", "TotCh"]]
    .rename(columns=dict_var_disp_full)
    .plot(ax=axes[1], kind="bar")
)
axes[1].set_xlabel("Month")
axes[1].set_ylabel("Total Water Amount (mm)")
axes[1].set_title("Monthly Surface Water Balance")
axes[1].xaxis.set_ticklabels(name_mon, rotation=0)
axes[1].legend()

plt.tight_layout()
plt.show()

# %%
# Summary
# -------
#
# This tutorial demonstrated the essential SUEWS workflow:
#
# 1. **Load sample data** using ``SUEWSSimulation.from_sample_data()``
# 2. **Run simulation** with ``sim.run()``
# 3. **Explore results** using pandas for statistics, resampling, and plotting
#
# Key concepts covered:
#
# - Energy balance components: Q*, QH, QE, QS, QF
# - Radiation balance: Kdown, Kup, Ldown, Lup
# - Water balance: Rain, Evap, Runoff, Storage change
# - Temporal resampling: 5-min to hourly, daily, monthly
#
# **Next steps:**
#
# - :doc:`plot_02_setup_own_site` - Configure SUEWS for your own site
# - :doc:`plot_03_impact_studies` - Sensitivity analysis and scenario modelling
