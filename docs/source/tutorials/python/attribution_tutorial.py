# %% [markdown]
# # Understanding Temperature Changes with Attribution Analysis
#
# This tutorial demonstrates the **attribution module** for decomposing
# temperature changes into physically meaningful components.
#
# ## The Problem
#
# When comparing SUEWS simulations, you can easily see that T2 (2m air
# temperature) differs between scenarios. But this doesn't tell you *why*:
#
# - Is the temperature change due to altered radiation balance?
# - Is it from changes in evaporative cooling?
# - Is the turbulent exchange (resistance) different?
# - Are air properties (density, heat capacity) contributing?
#
# The attribution module answers these questions by decomposing temperature
# differences using **Shapley value analysis** - a mathematically exact
# method that guarantees the sum of contributions equals the total change.

# %% [markdown]
# ## Setup

# %%
import supy as sp
from supy import SUEWSSimulation
from supy.util import attribute_t2, diagnose_t2

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

# Set up plotting style
plt.rcParams['figure.figsize'] = (10, 6)
plt.rcParams['font.size'] = 11

# %% [markdown]
# ## Load Sample Data and Run Baseline Simulation

# %%
# Load sample data using modern API
sim_baseline = SUEWSSimulation.from_sample_data()

# Use a shorter period for demonstration
df_forcing = sim_baseline.forcing.loc["2012-06":"2012-08"]
sim_baseline.update_forcing(df_forcing)

# Run baseline simulation
df_output_baseline = sim_baseline.run()

print(f"Simulation period: {df_forcing.index[0]} to {df_forcing.index[-1]}")
print(f"Number of timesteps: {len(df_forcing)}")

# The forcing data contains reference conditions needed for accurate attribution
print(f"\nForcing data available: Tair, RH, pres (needed for attribution)")

# %% [markdown]
# ---
#
# ## Use Case 1: Diagnosing Unexpected T2 Values
#
# A common question when running SUEWS is: "Why does T2 behave unexpectedly
# at certain times?"
#
# The `diagnose_t2()` function automatically identifies anomalous timesteps
# and attributes the causes.

# %% [markdown]
# ### Quick Anomaly Detection
#
# Let's diagnose any unusual T2 behaviour in our simulation:

# %%
# Diagnose T2 anomalies (timesteps > 2 sigma from daily mean)
# Pass forcing data for accurate attribution of air properties
result_anomaly = diagnose_t2(
    df_output_baseline,
    df_forcing=df_forcing,  # Include forcing for reference temperature
    method='anomaly',
    threshold=2.0,
    hierarchical=True
)

print(result_anomaly)

# %% [markdown]
# ### Interpreting the Results
#
# The output shows:
# - **flux_total**: Contribution from sensible heat flux changes
# - **resistance**: Contribution from turbulent exchange efficiency
# - **air_props**: Contribution from air density and heat capacity
#
# If flux dominates, check the energy balance components.
# If resistance dominates, check wind and stability.

# %%
# Visualise the attribution
fig, ax = plt.subplots(figsize=(8, 5))
result_anomaly.plot(kind='bar', ax=ax)
ax.set_title('What Drives T2 Anomalies?')
plt.tight_layout()

# %% [markdown]
# ### Diurnal Cycle Analysis
#
# Compare afternoon peak vs. morning baseline to understand the diurnal
# temperature pattern:

# %%
# Diagnose diurnal pattern
result_diurnal = diagnose_t2(
    df_output_baseline,
    df_forcing=df_forcing,  # Include forcing for reference temperature
    method='diurnal',  # Compare afternoon (12-15h) vs morning (6-10h)
    hierarchical=True
)

print("Diurnal T2 Attribution:")
print(result_diurnal)

# %%
# Plot the diurnal attribution breakdown
# Note: diagnose_t2 with method='diurnal' returns an aggregate comparison,
# so we use kind='bar' to visualise the mean contributions
fig, ax = plt.subplots(figsize=(8, 5))
result_diurnal.plot(kind='bar', ax=ax)
ax.set_title('Attribution of Diurnal T2 Variation (Afternoon vs Morning)')
plt.tight_layout()

# %% [markdown]
# ---
#
# ## Use Case 2: Green Infrastructure Impact Attribution
#
# A fundamental question in urban climate research: *When we add vegetation,
# which physical mechanisms drive the temperature reduction?*
#
# Let's create two scenarios:
# - **Baseline**: Higher building/paved fraction
# - **Green**: Increased vegetation fraction

# %%
# Create greened scenario by modifying surface fractions
sim_green = SUEWSSimulation.from_sample_data()
sim_green.update_forcing(df_forcing)

# Examine current surface fractions
print("Current surface fractions:")
print(sim_green.state_init.sfr_surf)

# %%
# Modify surface fractions: increase grass, decrease paved
# Surface order: Paved(0), Bldgs(1), EveTr(2), DecTr(3), Grass(4), BSoil(5), Water(6)
df_state_green = sim_green.state_init.copy()

# Increase grass by 0.15, decrease paved by 0.15
df_state_green.loc[:, ('sfr_surf', '(0,)')] -= 0.15  # Reduce paved
df_state_green.loc[:, ('sfr_surf', '(4,)')] += 0.15  # Increase grass

print("Modified surface fractions:")
print(df_state_green.sfr_surf)

# %%
# Create new simulation with modified state
sim_green = SUEWSSimulation.from_state(df_state_green)
sim_green.update_forcing(df_forcing)

# Run greened scenario
df_output_green = sim_green.run()

print("Both scenarios simulated successfully")

# %% [markdown]
# ### Traditional Comparison: What Changed?

# %%
# Extract T2 from both scenarios
def get_t2(df_output):
    if isinstance(df_output.columns, pd.MultiIndex):
        grid = df_output.index.get_level_values(0)[0]
        return df_output.loc[grid, 'SUEWS']['T2']
    return df_output['T2']

t2_baseline = get_t2(df_output_baseline)
t2_green = get_t2(df_output_green)

delta_t2 = t2_green - t2_baseline

print(f"Mean T2 change: {delta_t2.mean():.2f} degC")
print(f"Max cooling: {delta_t2.min():.2f} degC")
print(f"Max warming: {delta_t2.max():.2f} degC")

# %%
# Plot T2 difference
fig, ax = plt.subplots(figsize=(12, 4))
delta_t2.plot(ax=ax, alpha=0.7)
ax.axhline(0, color='black', linestyle='--', linewidth=0.5)
ax.set_ylabel('$\\Delta T_2$ (degC)')
ax.set_title('T2 Difference: Green Scenario - Baseline')
plt.tight_layout()

# %% [markdown]
# ### Attribution Analysis: Why Did It Change?
#
# Now use the attribution module to understand *which physical mechanisms*
# caused the temperature change:

# %%
# Run attribution analysis with forcing data for accurate decomposition
# Both scenarios use the same forcing, so pass df_forcing for both
result_green = attribute_t2(
    df_output_A=df_output_baseline,
    df_output_B=df_output_green,
    df_forcing_A=df_forcing,  # Reference conditions for baseline
    df_forcing_B=df_forcing,  # Same forcing for green scenario
    hierarchical=True  # Include flux budget breakdown
)

print(result_green)

# %% [markdown]
# ### Visualise the Attribution
#
# The bar chart shows mean contributions from each mechanism:

# %%
fig, axes = plt.subplots(1, 2, figsize=(14, 5))

# Bar chart of mean contributions
result_green.plot(kind='bar', ax=axes[0])
axes[0].set_title('Attribution of $\\Delta T_2$ to Physical Mechanisms')

# Diurnal cycle
result_green.plot(kind='diurnal', ax=axes[1])
axes[1].set_title('Diurnal Pattern of Attribution Components')

plt.tight_layout()

# %% [markdown]
# ### Flux Budget Breakdown
#
# If you enabled `hierarchical=True`, you can see how much each energy
# balance component contributes:

# %%
# Show flux breakdown if available
flux_cols = [c for c in result_green.contributions.columns
             if c.startswith('flux_') and c != 'flux_total']

if flux_cols:
    fig, ax = plt.subplots(figsize=(10, 5))
    result_green.plot(kind='bar', ax=ax, components=flux_cols)
    ax.set_title('Flux Budget Breakdown: Which Energy Balance Component Matters Most?')
    plt.tight_layout()
else:
    print("Flux breakdown not available - run with hierarchical=True")

# %% [markdown]
# ---
#
# ## Key Findings
#
# The attribution analysis reveals:
#
# 1. **Which mechanism dominates**: Is the temperature change primarily from
#    changed fluxes, modified turbulent exchange, or altered air properties?
#
# 2. **Flux budget breakdown**: Within the flux contribution, how much comes
#    from radiation (Q*), evaporation (QE), storage (dQS), or anthropogenic
#    heat (QF)?
#
# 3. **Temporal patterns**: Do the mechanisms vary throughout the day? Some
#    may dominate during daytime, others at night.
#
# ## Practical Applications
#
# - **Urban planning**: Understand *why* green infrastructure works, not
#   just that it works
# - **Model debugging**: Identify which process is causing unexpected behaviour
# - **Sensitivity analysis**: Quantify the relative importance of different
#   physical processes
# - **Climate adaptation**: Design interventions that target the most
#   effective mechanisms

# %% [markdown]
# ---
#
# ## Summary
#
# The attribution module provides:
#
# | Function | Purpose |
# |----------|----------|
# | `attribute_t2()` | Compare two scenarios, decompose the difference |
# | `diagnose_t2()` | Automatically detect and diagnose anomalies in a single run |
#
# Both functions return an `AttributionResult` object with:
# - `.contributions`: Full timeseries of each component
# - `.summary`: Summary statistics (mean, std, min, max)
# - `.plot()`: Built-in visualisation (bar, diurnal, line, heatmap)
# - `print()`: Clean text summary with percentages
