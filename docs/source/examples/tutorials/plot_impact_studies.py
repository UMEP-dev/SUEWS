"""
Impact Studies Using SuPy
=========================

Sensitivity analysis for urban climate modelling with SUEWS.

This tutorial demonstrates how to perform sensitivity analysis using SuPy
in parallel mode to investigate the impacts on urban climate of:

1. **Surface properties**: Physical attributes of land covers (e.g., albedo)
2. **Background climate**: Long-term meteorological conditions (e.g., air temperature)
"""

# %%
# Setup and Load Sample Data
# --------------------------
#
# First, we import the required packages and load the sample dataset.

import supy as sp
from supy import SUEWSSimulation

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

from time import time
from concurrent.futures import ThreadPoolExecutor

# Load sample datasets using the modern OOP interface
sim = SUEWSSimulation.from_sample_data()

# Extract initial state and forcing for impact studies
df_state_init = sim.state_init
df_forcing = sim.forcing

print("Sample data loaded using SUEWSSimulation.from_sample_data() API")
print("Ready for impact studies")

# %%
# Prepare Forcing Data
# --------------------
#
# By default, two years of forcing data are included. To save running time
# for this demonstration, we use only one year.

# Use 2012 data only
df_forcing = df_forcing.loc["2012"].iloc[1:]

# Perform an example run to get output samples for later use
sim.update_forcing(df_forcing)
df_output = sim.run()

# Access results
df_output = sim.results
df_state_final = sim.state_final

# %%
# Surface Properties: Albedo Study
# ================================
#
# We investigate how changes in surface albedo affect urban air temperature.

# %%
# Examine Default Albedo Values
# -----------------------------
#
# First, let's look at the default albedo values from the sample dataset.

print("Default albedo values by surface type:")
print(df_state_init.alb)

# %%
# Configure Test Surface
# ----------------------
#
# Create a test surface with 99% buildings and 1% paved area to isolate
# the effect of building albedo.

df_state_init_test = df_state_init.copy()

# Set surface fractions: 99% buildings, 1% paved
df_state_init_test.sfr_surf = 0
df_state_init_test.loc[:, ("sfr_surf", "(1,)")] = 0.99  # Buildings
df_state_init_test.loc[:, ("sfr_surf", "(0,)")] = 0.01  # Paved

print("Modified surface fractions:")
print(df_state_init_test.sfr_surf)

# %%
# Create Albedo Test Scenarios
# ----------------------------
#
# Construct a DataFrame with multiple albedo values (0.1 to 0.8) to test.

n_test = 10
list_alb_test = np.linspace(0.1, 0.8, n_test).round(2)

df_state_init_x = (
    pd.concat(
        {alb: df_state_init_test for alb in list_alb_test},
        names=["alb", "grid"],
    )
    .droplevel("grid", axis=0)
    .rename_axis(index="grid")
)

# Modify building albedo for each scenario
df_state_init_x.loc[:, ("alb", "(1,)")] = list_alb_test

print("Albedo test scenarios created:")
print(df_state_init_x.alb)

# %%
# Run Albedo Simulations
# ----------------------
#
# Conduct simulations using the OOP approach for January-July 2012.

df_forcing_part = df_forcing.loc["2012 01":"2012 07"]

# Create simulation from modified state and forcing
sim_test = SUEWSSimulation.from_state(df_state_init_x).update_forcing(df_forcing_part)

# Run simulation (suppress logging)
df_res_alb_test = sim_test.run(logging_level=90)

# %%
# Analyse Albedo Results
# ----------------------
#
# Examine the temperature response to albedo changes in July 2012.

# Select July 2012 results
df_res_alb_test_july = df_res_alb_test.SUEWS.unstack(0).loc["2012 7"]

# Calculate temperature statistics
df_res_alb_T2_stat = df_res_alb_test_july.T2.describe()

# Calculate temperature difference from baseline
df_res_alb_T2_diff = df_res_alb_T2_stat.transform(
    lambda x: x - df_res_alb_T2_stat.iloc[:, 0]
)
df_res_alb_T2_diff.columns = list_alb_test - list_alb_test[0]

# %%
# Plot Albedo Impact on Temperature
# ---------------------------------
#
# Visualise how increasing surface albedo reduces air temperature.

fig, ax = plt.subplots(figsize=(8, 5))
df_res_alb_T2_diff.loc[["max", "mean", "min"]].T.plot(ax=ax)
ax.set_ylabel(r"$\Delta T_2$ ($^\circ$C)")
ax.set_xlabel(r"$\Delta\alpha$ (albedo change)")
ax.margins(x=0.2, y=0.2)
ax.set_title("Temperature Response to Albedo Change (July 2012)")
ax.legend(title="Statistic")
plt.tight_layout()
plt.show()

# sphinx_gallery_thumbnail_number = 1

# %%
# Background Climate: Air Temperature Study
# =========================================
#
# We investigate how changes in background air temperature affect
# the simulated 2-metre temperature.

# %%
# Examine Monthly Climatology
# ---------------------------
#
# View the monthly mean air temperature from the sample dataset.

df_plot = df_forcing.Tair.loc["2012"].resample("1m").mean()

fig, ax = plt.subplots(figsize=(8, 5))
df_plot.plot.bar(ax=ax, color="tab:blue")
ax.set_xticklabels([d.strftime("%b") for d in df_plot.index], rotation=0)
ax.set_ylabel(r"Mean Air Temperature ($^\circ$C)")
ax.set_xlabel("Month")
ax.set_title("Monthly Mean Air Temperature (2012)")
plt.tight_layout()
plt.show()

# %%
# Define Parallel Simulation Function
# -----------------------------------
#
# Create a function to run multiple climate scenarios in parallel.
# This uses Python's built-in ``ThreadPoolExecutor`` for efficient execution.
#
# .. note::
#
#    We use threads (not processes) to ensure compatibility with Jupyter notebooks.


def run_supy_mclims(df_state_init, dict_df_forcing_mclims):
    """Run SUEWS simulations for multiple climate scenarios in parallel.

    Parameters
    ----------
    df_state_init : pd.DataFrame
        Initial state DataFrame
    dict_df_forcing_mclims : dict
        Dictionary mapping scenario names to forcing DataFrames

    Returns
    -------
    pd.DataFrame
        Combined results from all scenarios
    """

    def run_sim_oop(key, df_forcing, df_state_init, logging_level=90):
        sim = SUEWSSimulation.from_state(df_state_init)
        sim.update_forcing(df_forcing)
        sim.run(logging_level=logging_level)
        return (key, sim.results)

    # Run simulations in parallel using threads
    with ThreadPoolExecutor() as executor:
        futures = [
            executor.submit(run_sim_oop, k, df, df_state_init, 90)
            for k, df in dict_df_forcing_mclims.items()
        ]
        results = {key: result for key, result in [f.result() for f in futures]}

    df_output_mclims0 = pd.concat(
        results,
        keys=list(dict_df_forcing_mclims.keys()),
        names=["clm"],
    )
    df_output_mclims = df_output_mclims0.reset_index("grid", drop=True)

    return df_output_mclims


# %%
# Create Climate Scenarios
# ------------------------
#
# Construct forcing datasets with different air temperature offsets (0 to +2°C).

# Prepare test data
df_forcing_part_test = df_forcing.loc["2012 1":"2012 7"].copy()
df_state_init_test = df_state_init.copy()

# Create scenarios with temperature increases from 0 to 2°C
n_test = 12  # Can be reduced to save simulation time
list_TairDiff_test = np.linspace(0.0, 2, n_test).round(2)

dict_df_forcing_x = {
    tairdiff: df_forcing_part_test.copy() for tairdiff in list_TairDiff_test
}

# Apply temperature offset to each scenario
for tairdiff in dict_df_forcing_x:
    dict_df_forcing_x[tairdiff].loc[:, "Tair"] += tairdiff

print(f"Created {n_test} climate scenarios with temperature offsets:")
print(f"  Range: {list_TairDiff_test[0]:.2f}°C to {list_TairDiff_test[-1]:.2f}°C")

# %%
# Run Climate Simulations
# -----------------------
#
# Execute parallel simulations for all climate scenarios.

t0 = time()
df_airtemp_test_x = run_supy_mclims(df_state_init_test, dict_df_forcing_x)
t1 = time()

print(f"Execution time: {t1 - t0:.2f} s")

# %%
# Analyse Climate Results
# -----------------------
#
# Calculate temperature differences relative to the baseline scenario.

df_airtemp_test = df_airtemp_test_x.SUEWS.unstack(0)
df_temp_diff = df_airtemp_test.T2.transform(lambda x: x - df_airtemp_test.T2[0.0])

# Focus on July 2012 results
df_temp_diff_ana = df_temp_diff.loc["2012 7"]
df_temp_diff_stat = df_temp_diff_ana.describe().loc[["max", "mean", "min"]].T

# %%
# Plot Climate Impact on Temperature
# ----------------------------------
#
# Visualise the relationship between background air temperature changes
# and simulated 2-metre temperature.

fig, ax = plt.subplots(figsize=(8, 6))
df_temp_diff_stat.plot(ax=ax, marker="o")
ax.set_ylabel(r"$\Delta T_2$ ($^\circ$C)")
ax.set_xlabel(r"$\Delta T_{a}$ ($^\circ$C)")
ax.set_aspect("equal")
ax.set_title("Temperature Response to Background Climate Change (July 2012)")
ax.legend(title="Statistic")
plt.tight_layout()
plt.show()

# %%
# Conclusions
# -----------
#
# The results show that:
#
# 1. **Albedo effect**: Increasing surface albedo reduces urban air temperature,
#    with larger impacts on maximum temperatures than mean or minimum.
#
# 2. **Climate warming effect**: Increased background air temperature (T_a) has
#    different impacts on 2-metre temperature (T_2) metrics:
#
#    - All metrics (min, mean, max) increase linearly with T_a
#    - Maximum T_2 shows the strongest response
#
# These sensitivity analyses demonstrate how SUEWS can be used to evaluate
# urban climate mitigation strategies (e.g., cool roofs) and climate change impacts.
