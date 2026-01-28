"""
Preparing Initial Conditions
============================

Set up proper initial states for realistic SUEWS simulations.

Initial conditions define the starting state of your simulation, including
soil moisture, vegetation phenology, surface temperatures, and snow conditions.
Proper initialisation is critical for realistic results.

You will learn:

1. **Understanding state variables** - What SUEWS tracks and why it matters
2. **Spin-up strategies** - Equilibrating the model before analysis
3. **Seasonal adjustments** - Setting appropriate states for different start dates
4. **Common pitfalls** - Avoiding unrealistic initial conditions

**Prerequisites**: Complete :doc:`tutorial_01_quick_start` and
:doc:`tutorial_02_setup_own_site` first.
"""

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

from supy import SUEWSSimulation

# %%
# Understanding Initial States
# ----------------------------
#
# SUEWS tracks state variables for each land cover type. The key variables are:
#
# - **soilstore**: Soil moisture storage [mm] - critical for evapotranspiration
# - **lai_id**: Leaf area index [m²/m²] - controls transpiration
# - **gdd_id/sdd_id**: Growing/senescence degree days [°C·d] - phenology tracking
# - **temperature**: Thermal layer temperatures [°C] - subsurface heat storage
#
# Let's examine the initial state from sample data.

sim = SUEWSSimulation.from_sample_data()

print("Initial state structure:")
print(f"Columns: {sim.state_init.columns.tolist()[:10]}...")
print(f"\nSoil moisture columns:")
soil_cols = [c for c in sim.state_init.columns if "soilstore" in str(c).lower()]
print(soil_cols[:5])

# %%
# Spin-Up: Why It Matters
# -----------------------
#
# Initial conditions are often uncertain. The **spin-up** approach runs the
# model for a period before analysis to allow state variables to equilibrate.
#
# Without spin-up, unrealistic initial soil moisture or vegetation states
# can cause artifacts in the first weeks/months of simulation.

# Run a short simulation to see state evolution
_ = sim.run()

# Compare initial and final states for soil moisture
print("State evolution during simulation:")
print(f"  Initial soil moisture (mean): {sim.state_init.filter(like='soilstore').mean().mean():.1f} mm")
print(f"  Final soil moisture (mean): {sim.state_final.filter(like='soilstore').mean().mean():.1f} mm")

# %%
# Method 1: Full Year Spin-Up
# ---------------------------
#
# Run one year before your analysis period and use the final state as
# initial conditions. This is the most robust approach.

# Step 1: Run spin-up year
sim_spinup = SUEWSSimulation.from_sample_data()
_ = sim_spinup.run()

# Step 2: Get equilibrated state
state_equilibrated = sim_spinup.state_final.copy()

print("Spin-up complete!")
print(f"Equilibrated state ready for analysis period")

# Step 3: Use for analysis (in practice, you'd load new forcing data)
# sim_analysis = SUEWSSimulation.from_state(state_equilibrated)
# sim_analysis.update_forcing('forcing_2015.txt')
# sim_analysis.run()

# %%
# Method 2: Repeated Year Spin-Up
# -------------------------------
#
# For limited forcing data, repeat the same year until states converge.
# Typically 2-3 iterations are sufficient.

# Track soil moisture convergence
soil_history = []
sim = SUEWSSimulation.from_sample_data()

# Initial run
_ = sim.run()
soil_history.append(sim.state_final.filter(like="soilstore").mean().mean())

# Capture forcing once — it stays the same across all spin-up iterations.
forcing_data = sim.forcing

# Spin-up iterations: reuse the same forcing but transfer evolved state.
# Typically 2-3 iterations suffice for convergence (change < 1 mm).
n_spinup = 3
for i in range(n_spinup):
    # Create new simulation from final state and re-attach forcing
    sim_next = SUEWSSimulation.from_state(sim.state_final)
    sim_next.update_forcing(forcing_data)
    _ = sim_next.run()
    soil_history.append(sim_next.state_final.filter(like="soilstore").mean().mean())

    # Check convergence
    change = abs(soil_history[-1] - soil_history[-2])
    print(f"Spin-up {i+1}: Soil moisture = {soil_history[-1]:.1f} mm (change: {change:.2f} mm)")

    # Carry forward for next iteration
    sim = sim_next

# %%
# Visualise Spin-Up Convergence
# -----------------------------
#
# Plot how soil moisture evolves across spin-up iterations.

fig, ax = plt.subplots(figsize=(8, 5))
ax.plot(range(len(soil_history)), soil_history, "bo-", markersize=10, linewidth=2)
ax.set_xlabel("Spin-up Iteration")
ax.set_ylabel("Mean Soil Moisture (mm)")
ax.set_title("Soil Moisture Convergence During Spin-Up")
ax.set_xticks(range(len(soil_history)))
ax.set_xticklabels(["Initial"] + [f"Year {i+1}" for i in range(n_spinup)])
ax.grid(True, alpha=0.3)
plt.tight_layout()

# %%
# Seasonal Initial Conditions
# ---------------------------
#
# Different seasons require different initial vegetation states.
# Here's how to estimate initial LAI based on the simulation start month.


def get_initial_lai(month, laimin=1.0, laimax=5.5):
    """Estimate initial LAI for a given month (deciduous trees).

    Uses a simple piecewise-linear interpolation between winter minimum
    and summer maximum LAI. This schedule assumes **northern hemisphere
    mid-latitude** phenology (winter = DJF, summer = JJA). For southern
    hemisphere sites, shift months by 6 before calling this function.

    Only appropriate for **deciduous vegetation**. Evergreen trees and
    grass surfaces have different phenology and should use separate LAI
    curves (see SUEWS documentation on vegetation parameters).

    Parameters
    ----------
    month : int
        Start month (1-12). Northern hemisphere convention.
    laimin : float
        Minimum LAI (winter, leaves off)
    laimax : float
        Maximum LAI (summer, full canopy)

    Returns
    -------
    float
        Estimated initial LAI
    """
    if month in [12, 1, 2]:  # Winter
        return laimin
    elif month in [6, 7, 8]:  # Summer
        return laimax
    else:  # Spring/Autumn - interpolate
        if month in [3, 4, 5]:
            frac = (month - 2) / 4  # Spring green-up
        else:
            frac = 1 - (month - 8) / 4  # Autumn senescence
        return laimin + frac * (laimax - laimin)


# Demonstrate for each month
months = range(1, 13)
lai_values = [get_initial_lai(m) for m in months]

fig, ax = plt.subplots(figsize=(10, 5))
ax.bar(months, lai_values, color="forestgreen", alpha=0.7, edgecolor="darkgreen")
ax.set_xlabel("Month")
ax.set_ylabel("Initial LAI (m²/m²)")
ax.set_title("Recommended Initial LAI by Start Month (Deciduous Trees)")
ax.set_xticks(months)
ax.set_xticklabels(["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"])
ax.axhline(y=1.0, color="brown", linestyle="--", alpha=0.5, label="LAI min (winter)")
ax.axhline(y=5.5, color="green", linestyle="--", alpha=0.5, label="LAI max (summer)")
ax.legend()
plt.tight_layout()

# %%
# Common Pitfalls
# ---------------
#
# **Pitfall 1: Unrealistic Soil Moisture**
#
# Starting with soil too wet or dry causes unrealistic early evaporation.
# Use typical values for your climate:
#
# - Humid climates: 100-150 mm
# - Semi-arid: 50-100 mm
#
# **Pitfall 2: Wrong Vegetation State for Season**
#
# Summer LAI in winter causes excessive transpiration. Always match
# ``lai_id`` to your start date using the function above.
#
# **Pitfall 3: Thermal Layer Mismatch**
#
# Temperature values must match the number of thermal layers in your
# configuration. Check ``thermal_layers.dz`` and provide matching values.

# Example: Check thermal layer configuration
print("Typical thermal layer setup:")
print("  Layer depths: [0.2, 0.15, 0.01, 0.01, 0.01] m (5 layers)")
print("  Initial temps: [15.0, 14.0, 13.0, 12.0, 11.0] degC (must have 5 values)")

# %%
# Automatic Temperature Initialisation
# ------------------------------------
#
# The SUEWS validation tool can automatically set initial temperatures
# based on climate data. Run from command line:
#
# .. code-block:: bash
#
#    suews-validate config.yml
#
# The validator uses CRU climate data to set appropriate temperatures
# based on your site's coordinates and simulation start month.

# %%
# Summary
# -------
#
# Key points for initial conditions:
#
# 1. **Always use spin-up** for production runs - at least one year
# 2. **Match vegetation state to season** - use ``get_initial_lai()`` helper
# 3. **Check soil moisture bounds** - cannot exceed ``soilstorecap``
# 4. **Thermal layers must match** - same number of values as layer depths
# 5. **Use the validator** - ``suews-validate`` handles many details automatically
#
# Next: Learn about :doc:`impact studies <tutorial_04_impact_studies>` for
# sensitivity analysis and climate scenarios.
