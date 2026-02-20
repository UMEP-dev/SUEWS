"""
Impact Studies Using SuPy
=========================

Sensitivity analysis for urban climate modelling with SUEWS.

This tutorial demonstrates how to perform sensitivity analysis using SuPy
to investigate the impacts on urban climate of:

1. **Surface properties**: Physical attributes of land covers (e.g., albedo)
2. **Background climate**: Long-term meteorological conditions (e.g., air temperature)

**API approach**: This tutorial uses the :class:`~supy.SUEWSSimulation` OOP interface but
extracts DataFrames for scenario construction. This hybrid pattern is appropriate
for multi-scenario sensitivity analysis where you need to programmatically
modify parameters across many test cases.

**Tutorial structure**:

- Part 1 (Albedo): Build scenario matrix with DataFrame, run all at once
- Part 2 (Climate): Modify forcing in a loop, combine results for analysis
"""

# %%
# Setup and Load Sample Data
# --------------------------
#
# First, we import the required packages and load the sample dataset.

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

from supy import SUEWSSimulation

# Load sample datasets using the OOP interface
sim = SUEWSSimulation.from_sample_data()

# Slice forcing to a shorter period for faster execution (returns SUEWSForcing)
forcing_sliced = sim.forcing["2012-01":"2012-03"].iloc[1:]

print("Sample data loaded using SUEWSSimulation.from_sample_data()")
print(f"Simulation period: {forcing_sliced.df.index[0]} to {forcing_sliced.df.index[-1]}")

# %%
# Part 1: Surface Properties - Albedo Study
# -----------------------------------------
#
# We investigate how changes in surface albedo affect urban air temperature.
# Higher albedo (more reflective surfaces) should reduce absorbed solar
# radiation and lower surface and air temperatures.

# %%
# Examine Default Albedo Values
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# First, let's look at the default albedo values from the sample dataset.
# We access the initial state via the simulation object.

print("Default albedo values by surface type:")
print(sim.state_init.alb)

# %%
# Configure Test Surface
# ~~~~~~~~~~~~~~~~~~~~~~
#
# Create a test surface with 99% buildings and 1% paved area to isolate
# the effect of building albedo. We extract the DataFrame for modification.

# Extract state for scenario construction (DataFrame needed for pd.concat)
df_state_init = sim.state_init.copy()

# Create a modified state for testing
df_state_test = df_state_init.copy()

# Set surface fractions: 99% buildings, 1% paved
df_state_test.sfr_surf = 0.0  # Use float to preserve dtype
df_state_test.loc[:, ("sfr_surf", "(1,)")] = 0.99  # Buildings
df_state_test.loc[:, ("sfr_surf", "(0,)")] = 0.01  # Paved

# Verify fractions sum to 1.0
assert abs(df_state_test.sfr_surf.sum(axis=1).iloc[0] - 1.0) < 1e-6, "Surface fractions must sum to 1.0"

print("Modified surface fractions:")
print(df_state_test.sfr_surf)

# %%
# Build Albedo Scenario Matrix
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Construct a multi-scenario DataFrame with different albedo values.
# This is the key step where DataFrame manipulation is necessary.

# Test 3 albedo values (0.1 to 0.8) for faster tutorial execution
n_albedo = 3
list_albedo = np.linspace(0.1, 0.8, n_albedo).round(2)

# Create scenario matrix by concatenating copies with integer grid IDs
df_state_scenarios = (
    pd.concat(
        {i: df_state_test.copy() for i in range(n_albedo)},
        names=["scenario", "grid"],
    )
    .droplevel("grid", axis=0)
    .rename_axis(index="grid")
)

# Set building albedo for each scenario (explicit per-row assignment for safety)
for idx, alb_val in zip(df_state_scenarios.index, list_albedo):
    df_state_scenarios.loc[idx, ("alb", "(1,)")] = alb_val

print(f"Created {n_albedo} albedo scenarios:")
print(df_state_scenarios.alb)

# %%
# Run Albedo Simulations
# ~~~~~~~~~~~~~~~~~~~~~~
#
# Create a simulation from the scenario matrix and run all scenarios at once.
# This demonstrates efficient batch execution.

# Create simulation from scenario matrix and run
sim_albedo = SUEWSSimulation.from_state(df_state_scenarios).update_forcing(forcing_sliced)
output_albedo = sim_albedo.run(logging_level=90)

print(f"Completed {n_albedo} albedo simulations")

# %%
# Analyse Albedo Results
# ~~~~~~~~~~~~~~~~~~~~~~
#
# Examine the temperature response to albedo changes using the OOP output interface.

# Access results via output.SUEWS (OOP interface)
df_results = output_albedo.SUEWS.unstack(0)

# Select last month for analysis
last_month = f"{df_results.index[-1].year}-{df_results.index[-1].month:02d}"
df_month = df_results.loc[last_month]

# Calculate temperature statistics across scenarios
df_T2_stats = df_month.T2.describe()

# Calculate temperature difference from baseline (lowest albedo)
df_T2_diff = df_T2_stats.transform(lambda x: x - df_T2_stats.iloc[:, 0])
df_T2_diff.columns = list_albedo - list_albedo[0]

print(f"Temperature statistics for {last_month}:")
print(df_T2_stats.loc[["mean", "min", "max"]])

# %%
# Plot Albedo Impact on Temperature
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Visualise how increasing surface albedo reduces air temperature.

fig, ax = plt.subplots(figsize=(8, 5))
df_T2_diff.loc[["max", "mean", "min"]].T.plot(ax=ax)
ax.set_ylabel(r"$\Delta T_2$ ($^\circ$C)")
ax.set_xlabel(r"$\Delta\alpha$ (albedo change)")
ax.margins(x=0.2, y=0.2)
ax.set_title(f"Temperature Response to Albedo Change ({last_month})")
ax.legend(title="Statistic")
plt.tight_layout()

# sphinx_gallery_thumbnail_number = 1

# %%
# Part 2: Background Climate - Air Temperature Study
# --------------------------------------------------
#
# We investigate how changes in background air temperature affect
# the simulated 2-metre temperature. This represents climate warming scenarios.

# %%
# Examine Monthly Climatology
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# View the monthly mean air temperature from the forcing data.
# The resample method returns aggregated values.

df_monthly_temp = forcing_sliced.resample("1ME")["Tair"]

fig, ax = plt.subplots(figsize=(8, 5))
df_monthly_temp.plot.bar(ax=ax, color="tab:blue")
ax.set_xticklabels([d.strftime("%b") for d in df_monthly_temp.index], rotation=0)
ax.set_ylabel(r"Mean Air Temperature ($^\circ$C)")
ax.set_xlabel("Month")
ax.set_title("Monthly Mean Air Temperature (2012)")
plt.tight_layout()

# %%
# Create and Run Climate Scenarios
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Build forcing datasets with different air temperature offsets (0 to +2 deg C)
# and run simulations in a loop.
#
# .. note::
#
#    For larger studies, these simulations could be run in parallel using
#    ``concurrent.futures.ThreadPoolExecutor``. We use a simple loop here
#    for clarity.

# Temperature offsets to test
n_climate = 3
list_temp_offset = np.linspace(0.0, 2.0, n_climate).round(2)

# Extract forcing DataFrame for modification
df_forcing = forcing_sliced.df

# Run scenarios and collect results
list_outputs = []

for temp_offset in list_temp_offset:
    # Create modified forcing with temperature offset
    df_forcing_modified = df_forcing.copy()
    df_forcing_modified["Tair"] += temp_offset

    # Run simulation using OOP interface
    sim_climate = SUEWSSimulation.from_state(df_state_init).update_forcing(
        df_forcing_modified
    )
    output = sim_climate.run(logging_level=90)

    # Store results with scenario label
    list_outputs.append((temp_offset, output))

print(f"Completed {n_climate} climate scenarios")
print(f"Temperature offsets: {list_temp_offset[0]:.1f} to {list_temp_offset[-1]:.1f} deg C")

# %%
# Combine and Analyse Climate Results
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Combine results from all scenarios and calculate temperature differences.

# Combine results into a single DataFrame
dict_results = {}
for temp_offset, output in list_outputs:
    # Access T2 directly from output (OOP interface)
    # T2 is a Series with MultiIndex (grid, datetime) - extract the Series
    dict_results[temp_offset] = output.SUEWS.T2

df_climate_results = pd.concat(dict_results, axis=1, names=["temp_offset"])

# Simplify index for analysis (drop grid level since we have only one grid)
df_climate_results = df_climate_results.droplevel("grid")

# Calculate difference from baseline
df_temp_diff = df_climate_results.transform(lambda x: x - df_climate_results[0.0])

# Focus on last month
last_month_climate = f"{df_temp_diff.index[-1].year}-{df_temp_diff.index[-1].month:02d}"
df_temp_diff_month = df_temp_diff.loc[last_month_climate]
df_temp_diff_stats = df_temp_diff_month.describe().loc[["max", "mean", "min"]].T

print(f"Temperature response statistics for {last_month_climate}:")
print(df_temp_diff_stats)

# %%
# Plot Climate Impact on Temperature
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Visualise the relationship between background air temperature changes
# and simulated 2-metre temperature.

fig, ax = plt.subplots(figsize=(8, 6))
df_temp_diff_stats.plot(ax=ax, marker="o")
ax.set_ylabel(r"$\Delta T_2$ ($^\circ$C)")
ax.set_xlabel(r"$\Delta T_{a}$ ($^\circ$C)")
ax.set_aspect("equal")
ax.set_title(f"Temperature Response to Background Climate Change ({last_month_climate})")
ax.legend(title="Statistic")
plt.tight_layout()

# %%
# Conclusions
# -----------
#
# The results demonstrate two key relationships:
#
# 1. **Albedo effect**: Increasing surface albedo reduces urban air temperature,
#    with larger impacts on maximum temperatures than mean or minimum. This
#    supports cool roof strategies for urban heat mitigation.
#
# 2. **Climate warming effect**: Increased background air temperature (T_a) leads
#    to proportional increases in 2-metre temperature (T_2):
#
#    - All metrics (min, mean, max) increase with warming
#    - The relationship is approximately linear within the tested range
#
# These sensitivity analyses demonstrate how SUEWS can evaluate both mitigation
# strategies (surface property changes) and climate change impacts (background
# warming scenarios).
#
# **Next steps:**
#
# - :doc:`tutorial_06_external_coupling` - Couple SUEWS with external models
