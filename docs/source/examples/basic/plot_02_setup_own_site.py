"""
Set Up SUEWS for Your Own Site
==============================

Configure SUEWS parameters for a custom research location.

This tutorial demonstrates how to configure SUEWS for your own site using
external forcing data. We use the US-AR1 site (ARM Southern Great Plains,
Oklahoma, USA) as an example - a grassland flux tower site with high-quality
observations.

You will learn to:

1. Load external forcing data from a file
2. Modify surface parameters (land cover, albedo, LAI)
3. Configure site-specific settings (location, measurement height)
4. Run the simulation and compare with observations

**Important**: This tutorial shows the legacy DataFrame approach. For new projects,
consider using YAML configuration files for better structure and validation.
"""

import os
from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import supy as sp
from supy import SUEWSSimulation

# Detect CI environment for reduced computation
_IS_CI = os.environ.get("CI", "false").lower() == "true"

# %%
# Load Sample Data as Template
# ----------------------------
#
# We start with built-in sample data to get valid default parameters,
# then modify them for our target site.

sim = SUEWSSimulation.from_sample_data()
df_state_init = sim.state_init

# Create a copy for modification
df_state_site = df_state_init.copy()

print("Template data loaded successfully!")

# %%
# Configure Site Location
# -----------------------
#
# Set the geographic coordinates and altitude for the US-AR1 site.
# These affect solar geometry calculations and atmospheric corrections.

# US-AR1: ARM Southern Great Plains, Oklahoma, USA
df_state_site.loc[:, "lat"] = 36.6  # Latitude (degrees)
df_state_site.loc[:, "lng"] = -97.5  # Longitude (degrees)
df_state_site.loc[:, "alt"] = 314.0  # Altitude (metres)
df_state_site.loc[:, "timezone"] = -6  # Central Time (UTC-6)

print(f"Site: US-AR1 (ARM Southern Great Plains)")
print(f"Location: lat={df_state_site.lat.values[0]}, lng={df_state_site.lng.values[0]}")

# %%
# Configure Land Cover Fractions
# ------------------------------
#
# SUEWS divides the urban surface into 7 land cover types:
#
# - 0: Paved surfaces
# - 1: Buildings
# - 2: Evergreen trees
# - 3: Deciduous trees
# - 4: Grass
# - 5: Bare soil
# - 6: Water
#
# Fractions must sum to 1.0.

# View default surface fractions
print("Default surface fractions:")
print(df_state_site.loc[:, "sfr_surf"])

# Configure for a grassland site (100% grass)
df_state_site.loc[:, "sfr_surf"] = 0.0  # Use float to preserve dtype
df_state_site.loc[:, ("sfr_surf", "(4,)")] = 1.0  # 100% grass

print("\nModified surface fractions (grassland):")
print(df_state_site.loc[:, "sfr_surf"])

# %%
# Configure Albedo Parameters
# ---------------------------
#
# Albedo varies seasonally with vegetation state. We set minimum and maximum
# values for grass surfaces.

# Grass albedo range
df_state_site.albmax_grass = 0.25  # Maximum (dry/dormant grass)
df_state_site.albmin_grass = 0.18  # Minimum (green grass)
df_state_site.loc[:, "albgrass_id"] = 0.20  # Initial value

print(f"Grass albedo: min={df_state_site.albmin_grass.values[0]}, max={df_state_site.albmax_grass.values[0]}")

# %%
# Configure LAI and Phenology
# ---------------------------
#
# Leaf Area Index (LAI) controls vegetation transpiration and varies
# seasonally based on temperature accumulation.

# LAI parameters for grass
df_state_site.loc[:, ("laimax", "(2,)")] = 3.0  # Maximum LAI
df_state_site.loc[:, ("laimin", "(2,)")] = 0.5  # Minimum LAI
df_state_site.loc[:, ("lai_id", "(2,)")] = 0.5  # Initial LAI

# Phenology parameters
df_state_site.loc[:, ("baset", "(2,)")] = 5  # Base temperature for growth
df_state_site.loc[:, ("basete", "(2,)")] = 20  # Base temperature for senescence
df_state_site.loc[:, ("gddfull", "(2,)")] = 1000  # Growing degree days for full leaf
df_state_site.loc[:, ("sddfull", "(2,)")] = -1000  # Senescence degree days

print("LAI configured for grass surfaces")

# %%
# Configure Surface Resistance
# ----------------------------
#
# Surface resistance controls evapotranspiration. These parameters define
# how the canopy responds to environmental conditions.

df_state_site.maxconductance = 20.0  # Maximum stomatal conductance
df_state_site.g1 = 1.0  # LAI-related parameter
df_state_site.g2 = 100.0  # Solar radiation parameter
df_state_site.g3 = 0.5  # Vapour pressure deficit parameter
df_state_site.g4 = 0.8  # Soil moisture parameter
df_state_site.g5 = 40.0  # Air temperature parameter
df_state_site.g6 = 0.02  # Kdown parameter

print("Surface resistance parameters configured")

# %%
# Configure Measurement Height
# ----------------------------
#
# Set the height where forcing variables are measured. This affects
# the aerodynamic calculations.

df_state_site.z = 10.0  # Measurement height (metres)

# Disable anthropogenic heat (rural site)
df_state_site.popdensdaytime = 0
df_state_site.popdensnighttime = 0

print(f"Measurement height: {df_state_site.z.values[0]} m")

# %%
# Validate Configuration
# ----------------------
#
# Use the built-in validation to check for parameter issues.

df_state_validated = sp.check_state(df_state_site)
print("Configuration validation complete")

# %%
# Load External Forcing Data
# --------------------------
#
# Load meteorological observations from the US-AR1 site.
# The forcing file contains hourly observations for 2010.

# Determine script directory (works both standalone and in sphinx-gallery)
try:
    _script_dir = Path(__file__).resolve().parent
except NameError:
    # sphinx-gallery context - working directory is set to script's source directory
    _script_dir = Path.cwd()

# Path to forcing data
path_forcing = _script_dir / "data" / "US-AR1_2010_data_60.txt"
df_forcing_raw = sp.util.read_forcing(str(path_forcing), tstep_mod=None)

# Use reduced period for CI builds
if _IS_CI:
    df_forcing_site = df_forcing_raw.loc["2010-01":"2010-03"]
else:
    df_forcing_site = df_forcing_raw.loc["2010-01":"2010-06"]

# Validate forcing data
sp.check_forcing(df_forcing_site)

print(f"Forcing period: {df_forcing_site.index[0]} to {df_forcing_site.index[-1]}")
print(f"Time steps: {len(df_forcing_site)}")

# %%
# Visualise Forcing Data
# ----------------------
#
# Examine the key meteorological variables that drive the simulation.

list_var = ["kdown", "Tair", "RH", "U", "rain"]
dict_labels = {
    "kdown": r"$K_\downarrow$ (W m$^{-2}$)",
    "Tair": r"$T_{air}$ ($^\circ$C)",
    "RH": "RH (%)",
    "U": r"$U$ (m s$^{-1}$)",
    "rain": "Rain (mm)",
}

# Resample to hourly for clearer plots
df_plot = df_forcing_site[list_var].resample("1h").mean()
df_plot["rain"] = df_forcing_site["rain"].resample("1h").sum()

fig, axes = plt.subplots(5, 1, figsize=(10, 10), sharex=True)
for ax, var in zip(axes, list_var):
    df_plot[var].plot(ax=ax)
    ax.set_ylabel(dict_labels[var])
axes[-1].set_xlabel("Date")
fig.suptitle("Forcing Data Overview", fontsize=12, y=1.02)
plt.tight_layout()
plt.show()

# sphinx_gallery_thumbnail_number = 1

# %%
# Run Simulation
# --------------
#
# Create a simulation with the modified configuration and run it.

sim_site = SUEWSSimulation.from_state(df_state_validated).update_forcing(df_forcing_site)
sim_site.run(logging_level=90)

df_output = sim_site.results
df_state_final = sim_site.state_final

print(f"Simulation complete: {len(df_output)} timesteps")

# %%
# Analyse Energy Balance
# ----------------------
#
# Examine the simulated surface energy balance components.

df_suews = df_output["SUEWS"]
grid = df_state_site.index[0]
df_results = df_suews.loc[grid]

# Daily means
df_daily = df_results.resample("1d").mean()

dict_var_disp = {
    "QN": r"$Q^*$",
    "QS": r"$\Delta Q_S$",
    "QE": "$Q_E$",
    "QH": "$Q_H$",
}

fig, ax = plt.subplots(figsize=(10, 4))
df_daily[["QN", "QS", "QE", "QH"]].rename(columns=dict_var_disp).plot(ax=ax)
ax.set_xlabel("Date")
ax.set_ylabel(r"Flux (W m$^{-2}$)")
ax.set_title("Daily Mean Surface Energy Balance")
ax.legend()
plt.tight_layout()
plt.show()

# %%
# Examine LAI Dynamics
# --------------------
#
# Check how LAI evolves through the simulation based on temperature accumulation.

df_daily_state = df_output.loc[grid, "DailyState"].dropna(how="all").resample("1d").mean()

if "LAI_Grass" in df_daily_state.columns:
    fig, ax = plt.subplots(figsize=(10, 3))
    df_daily_state["LAI_Grass"].plot(ax=ax)
    ax.set_xlabel("Date")
    ax.set_ylabel("LAI (m$^2$ m$^{-2}$)")
    ax.set_title("Grass LAI Evolution")
    plt.tight_layout()
    plt.show()
else:
    print("LAI_Grass not available in DailyState output")

# %%
# Surface Resistance Analysis
# ---------------------------
#
# Examine how surface resistance varies with environmental conditions.

ser_rs = df_results["RS"]

# Daily median resistance (filter extreme values)
df_rs_daily = ser_rs.between_time("10:00", "16:00").resample("1d").median()
df_rs_daily = df_rs_daily[df_rs_daily < 500]  # Filter outliers

fig, ax = plt.subplots(figsize=(10, 3))
df_rs_daily.plot(ax=ax)
ax.set_xlabel("Date")
ax.set_ylabel(r"$r_s$ (s m$^{-1}$)")
ax.set_title("Daily Median Surface Resistance (10:00-16:00)")
plt.tight_layout()
plt.show()

# %%
# Summary
# -------
#
# This tutorial demonstrated how to configure SUEWS for a custom site:
#
# 1. **Location**: Set latitude, longitude, and altitude
# 2. **Land cover**: Configure surface fractions (sum to 1.0)
# 3. **Vegetation**: Set albedo, LAI, and phenology parameters
# 4. **Surface resistance**: Configure stomatal conductance parameters
# 5. **Forcing data**: Load and validate meteorological observations
#
# **Key parameters to consider:**
#
# - Surface fractions determine which surface types are active
# - LAI parameters control vegetation transpiration seasonally
# - Surface resistance parameters tune the Jarvis model response
# - Measurement height affects aerodynamic calculations
#
# **Next steps:**
#
# - :doc:`plot_03_impact_studies` - Sensitivity analysis and scenario modelling
