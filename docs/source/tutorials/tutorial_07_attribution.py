"""
Temperature Attribution Analysis
=================================

Decomposing temperature changes into physically meaningful components.

When comparing SUEWS simulations, you can easily see that T2 (2m air
temperature) differs between scenarios. But this doesn't tell you *why*.
The attribution module answers these questions by decomposing temperature
differences using **Shapley value analysis** :cite:`S53,O72` -- a mathematically exact
method that guarantees the sum of contributions equals the total change.

This tutorial demonstrates:

1. **Single-run diagnostics** -- detect and attribute T2 anomalies
2. **Two-scenario comparison** -- decompose the temperature difference
   from a green-infrastructure intervention
3. **Flux budget breakdown** -- identify which energy balance component
   drives the change

**Prerequisites**: Complete :doc:`tutorial_01_quick_start` first.

**API approach**: This tutorial uses the :class:`~supy.SUEWSSimulation` OOP interface but
extracts DataFrames for scenario comparison. This hybrid pattern is appropriate
for attribution studies where output DataFrames from two runs must be compared.
"""

# %%
# Setup
# -----

import matplotlib.pyplot as plt
import pandas as pd

from supy import SUEWSSimulation
from supy.util import attribute_t2, diagnose_t2

# %%
# Load Sample Data and Run Baseline Simulation
# ---------------------------------------------
#
# Load the built-in sample dataset and slice to a summer period.
# Drop the first row with ``.iloc[1:]`` because accumulated variables
# (e.g. rainfall) for the partial period at the slice boundary are
# incomplete, making that row invalid as forcing input.

sim_baseline = SUEWSSimulation.from_sample_data()

# Use a shorter period for demonstration
df_forcing = sim_baseline.forcing["2012-06":"2012-08"].iloc[1:]
sim_baseline.update_forcing(df_forcing)

# Run baseline simulation
df_output_baseline = sim_baseline.run()

print(f"Simulation period: {df_forcing.index[0]} to {df_forcing.index[-1]}")
print(f"Number of timesteps: {len(df_forcing)}")
print("\nForcing variables available: Tair, RH, pres (needed for attribution)")

# %%
# Use Case 1: Diagnosing Unexpected T2 Values
# --------------------------------------------
#
# A common question when running SUEWS is: "Why does T2 behave unexpectedly
# at certain times?" The :func:`~supy.util.diagnose_t2` function
# automatically identifies anomalous timesteps and attributes the causes.

# %%
# Quick Anomaly Detection
# ~~~~~~~~~~~~~~~~~~~~~~~
#
# Detect timesteps where T2 exceeds 2 standard deviations from the
# daily mean. Passing ``df_forcing`` enables accurate attribution of
# air-property contributions.

result_anomaly = diagnose_t2(
    df_output_baseline,
    df_forcing=df_forcing,
    method="anomaly",
    threshold=2.0,
    hierarchical=True,
)

print(result_anomaly)

# %%
# Interpreting the Results
# ~~~~~~~~~~~~~~~~~~~~~~~~
#
# The output shows three top-level contributions:
#
# - **flux_total** -- sensible heat flux changes
# - **resistance** -- turbulent exchange efficiency
# - **air_props** -- air density and heat capacity
#
# If flux dominates, investigate the energy balance components.
# If resistance dominates, check wind speed and stability conditions.

fig, ax = plt.subplots(figsize=(8, 5))
result_anomaly.plot(kind="bar", ax=ax)
ax.set_title("What Drives T2 Anomalies?")
plt.tight_layout()

# %%
# Diurnal Cycle Analysis
# ~~~~~~~~~~~~~~~~~~~~~~
#
# Compare afternoon peak vs. morning baseline to understand the diurnal
# temperature pattern:

result_diurnal = diagnose_t2(
    df_output_baseline,
    df_forcing=df_forcing,
    method="diurnal",
    hierarchical=True,
)

print("Diurnal T2 Attribution:")
print(result_diurnal)

# %%
# The diurnal method returns an aggregate comparison (afternoon 12--15 h
# vs morning 6--10 h), so ``kind='bar'`` shows the mean contributions.

fig, ax = plt.subplots(figsize=(8, 5))
result_diurnal.plot(kind="bar", ax=ax)
ax.set_title("Attribution of Diurnal T2 Variation (Afternoon vs Morning)")
plt.tight_layout()

# %%
# Use Case 2: Green Infrastructure Impact Attribution
# ---------------------------------------------------
#
# A fundamental question in urban climate research: *when we add vegetation,
# which physical mechanisms drive the temperature reduction?*
#
# We compare two scenarios:
#
# - **Baseline** -- higher building/paved fraction
# - **Green** -- increased vegetation fraction

sim_green = SUEWSSimulation.from_sample_data()
sim_green.update_forcing(df_forcing)

# Access the land cover configuration
lc = sim_green.config.sites[0].properties.land_cover
surface_types = ["paved", "bldgs", "evetr", "dectr", "grass", "bsoil", "water"]

print("Current surface fractions:")
for name in surface_types:
    print(f"  {name}: {getattr(lc, name).sfr}")

# %%
# Modify Surface Fractions
# ~~~~~~~~~~~~~~~~~~~~~~~~
#
# Use ``update_config`` with a dictionary that mirrors the YAML structure.
# This is the recommended approach for parameter changes -- it keeps the
# interaction at the configuration level rather than exposing internal
# DataFrames.
#
# Reduce paved fraction by 0.15 and increase grass by 0.15.

sim_green.update_config(
    {
        "sites": {
            "properties": {
                "land_cover": {
                    "paved": {"sfr": lc.paved.sfr.value - 0.15},
                    "grass": {"sfr": lc.grass.sfr.value + 0.15},
                }
            }
        }
    }
)

print("Modified surface fractions:")
for name in surface_types:
    print(f"  {name}: {getattr(lc, name).sfr}")

# %%
# Run Greened Scenario
# ~~~~~~~~~~~~~~~~~~~~

df_output_green = sim_green.run()

print("Both scenarios simulated successfully")

# %%
# Traditional Comparison: What Changed?
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Extract T2 from both scenarios and compute the difference.
# This tells us *what* changed but not *why*.


def get_t2(output):
    """Extract T2 series from SUEWSOutput."""
    return output.get_variable("T2", group="SUEWS").iloc[:, 0]


t2_baseline = get_t2(df_output_baseline)
t2_green = get_t2(df_output_green)
delta_t2 = t2_green - t2_baseline

print(f"Mean T2 change: {delta_t2.mean():.2f} degC")
print(f"Max cooling: {delta_t2.min():.2f} degC")
print(f"Max warming: {delta_t2.max():.2f} degC")

# %%
# Plot the raw temperature difference time series.

fig, ax = plt.subplots(figsize=(12, 4))
delta_t2.plot(ax=ax, alpha=0.7)
ax.axhline(0, color="black", linestyle="--", linewidth=0.5)
ax.set_ylabel(r"$\Delta T_2$ (degC)")
ax.set_title("T2 Difference: Green Scenario - Baseline")
plt.tight_layout()

# %%
# Attribution Analysis: Why Did It Change?
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Now use :func:`~supy.util.attribute_t2` to decompose the temperature
# difference into physically meaningful contributions. Both scenarios
# share the same forcing, so we pass ``df_forcing`` for both.

result_green = attribute_t2(
    df_output_A=df_output_baseline,
    df_output_B=df_output_green,
    df_forcing_A=df_forcing,
    df_forcing_B=df_forcing,
    hierarchical=True,
)

print(result_green)

# %%
# Visualise the Attribution
# ~~~~~~~~~~~~~~~~~~~~~~~~~
#
# The bar chart (left) shows mean contributions from each mechanism;
# the diurnal plot (right) reveals how they vary through the day.

fig, axes = plt.subplots(1, 2, figsize=(14, 5))

result_green.plot(kind="bar", ax=axes[0])
axes[0].set_title(r"Attribution of $\Delta T_2$ to Physical Mechanisms")

result_green.plot(kind="diurnal", ax=axes[1])
axes[1].set_title("Diurnal Pattern of Attribution Components")

plt.tight_layout()

# sphinx_gallery_thumbnail_number = 3

# %%
# Flux Budget Breakdown
# ~~~~~~~~~~~~~~~~~~~~~
#
# With ``hierarchical=True``, you can see how much each energy
# balance component contributes:

flux_cols = [
    c
    for c in result_green.contributions.columns
    if c.startswith("flux_") and c != "flux_total"
]

if flux_cols:
    fig, ax = plt.subplots(figsize=(10, 5))
    result_green.plot(kind="bar", ax=ax, components=flux_cols)
    ax.set_title("Flux Budget Breakdown: Which Energy Balance Component Matters Most?")
    plt.tight_layout()
else:
    print("Flux breakdown not available - run with hierarchical=True")

# %%
# Summary
# -------
#
# This tutorial demonstrated how to decompose temperature changes into
# physically meaningful components:
#
# 1. **Single-run diagnostics** with :func:`~supy.util.diagnose_t2` --
#    detect anomalous timesteps and attribute them to flux, resistance,
#    or air-property contributions
# 2. **Two-scenario comparison** with :func:`~supy.util.attribute_t2` --
#    decompose the temperature difference between a baseline and a
#    green-infrastructure scenario
# 3. **Flux budget breakdown** -- identify whether radiation, evaporation,
#    storage, or anthropogenic heat drives the change
#
# Both functions return an ``AttributionResult`` object with:
#
# - ``.contributions`` -- full timeseries of each component
# - ``.summary`` -- summary statistics (mean, std, min, max)
# - ``.plot()`` -- built-in visualisation (bar, diurnal, line, heatmap)
# - ``print()`` -- clean text summary with percentages
#
# **Practical applications**:
#
# - **Urban planning** -- understand *why* green infrastructure works,
#   not just that it works
# - **Model debugging** -- identify which process causes unexpected behaviour
# - **Sensitivity analysis** -- quantify the relative importance of different
#   physical processes
# - **Climate adaptation** -- design interventions that target the most
#   effective mechanisms
#
# **Next steps:**
#
# - :doc:`tutorial_04_impact_studies` -- sensitivity analysis across scenarios
# - :doc:`tutorial_05_results_analysis` -- validation and export of results
