"""
External Model Coupling
=======================

Coupling SUEWS with external models for anthropogenic heat flux.

This example demonstrates how SUEWS can be coupled to external models that
provide forcing data. We use a simple anthropogenic heat flux (Q_F) model
driven by outdoor air temperature to show the coupling approach.

**Key concepts:**

- Anthropogenic heat flux depends on outdoor temperature
- SUEWS can receive external Q_F as forcing input
- One-way coupling: external model provides forcing to SUEWS
- Temperature feedback affects urban energy balance

**API approach**: This tutorial uses the `SUEWSSimulation` OOP interface but
extracts DataFrames for forcing modification. This hybrid pattern is required
for external model coupling where forcing variables must be modified at runtime.
"""

import os

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

from supy import SUEWSSimulation

# Detect CI environment for reduced computation
_IS_CI = os.environ.get("CI", "false").lower() == "true"

# %%
# Simple Anthropogenic Heat Model
# -------------------------------
#
# We define a simple Q_F model based on building heating and cooling:
#
# - Heating activates when T < 10°C
# - Cooling activates when T > 20°C
# - Between 10-20°C: baseline Q_F = 0
#
# This simplified model captures the temperature-dependent nature of
# building energy use without requiring detailed building simulation.


def QF_simple(T2):
    """Calculate anthropogenic heat flux from air temperature.

    Parameters
    ----------
    T2 : float
        2-metre air temperature (°C)

    Returns
    -------
    float
        Anthropogenic heat flux (W m⁻²)
    """
    T_cool = 20  # Cooling threshold (°C)
    T_heat = 10  # Heating threshold (°C)
    C_B = 5  # Cooling rate (W m⁻² K⁻¹)
    H_B = 10  # Heating rate (W m⁻² K⁻¹)
    scale = 0.3  # Scaling factor

    if T2 > T_cool:
        qf = (T2 - T_cool) * C_B
    elif T2 < T_heat:
        qf = (T_heat - T2) * H_B
    else:
        qf = 0

    return qf * scale


# %%
# Visualise the Q_F Model
# -----------------------
#
# Plot the Q_F response to temperature to understand the model behaviour.

temp_range = np.arange(-5, 45, 0.5)
qf_values = [QF_simple(t) for t in temp_range]

# Create DataFrames for plotting by region
df_qf = pd.DataFrame({"Temperature": temp_range, "QF": qf_values})

fig, ax = plt.subplots(figsize=(8, 5))

# Heating region (T < 10°C)
mask_heat = temp_range < 10
ax.plot(temp_range[mask_heat], np.array(qf_values)[mask_heat], "C1-", label="Heating", linewidth=2)

# Neutral region (10 <= T <= 20°C)
mask_neutral = (temp_range >= 10) & (temp_range <= 20)
ax.plot(
    temp_range[mask_neutral], np.array(qf_values)[mask_neutral], "C2-", label="Baseline", linewidth=2
)

# Cooling region (T > 20°C)
mask_cool = temp_range > 20
ax.plot(temp_range[mask_cool], np.array(qf_values)[mask_cool], "C0-", label="Cooling", linewidth=2)

# Annotations
ax.axvline(10, color="grey", linestyle="--", alpha=0.5)
ax.axvline(20, color="grey", linestyle="--", alpha=0.5)
ax.annotate("$T_H$ = 10°C", xy=(10, 5), fontsize=10)
ax.annotate("$T_C$ = 20°C", xy=(20, 5), fontsize=10)

ax.set_xlabel("Air Temperature ($^\\circ$C)")
ax.set_ylabel("$Q_F$ (W m$^{-2}$)")
ax.set_title("Simple Anthropogenic Heat Flux Model")
ax.legend()
ax.grid(True, alpha=0.3)
plt.tight_layout()
plt.show()

# sphinx_gallery_thumbnail_number = 1

# %%
# Load Sample Data
# ----------------
#
# Load the sample dataset and prepare for coupled simulation.

sim = SUEWSSimulation.from_sample_data()
df_state_init = sim.state_init.copy()
df_forcing = sim.forcing.df  # Use .df property to get DataFrame

# Disable snow module (not needed for this site)
df_state_init.loc[:, "snowuse"] = 0

# Use reduced period for CI
if _IS_CI:
    df_forcing = df_forcing.loc["2012-01":"2012-02"].iloc[1:]
else:
    df_forcing = df_forcing.loc["2012-06":"2012-08"].iloc[1:]

print(f"Simulation period: {df_forcing.index[0]} to {df_forcing.index[-1]}")
print(f"Time steps: {len(df_forcing)}")

# %%
# Calculate External Q_F from Forcing Data
# ----------------------------------------
#
# Apply the simple Q_F model to the air temperature from the forcing data.
# This is a one-way coupling: the external model uses prescribed temperatures.

# Calculate Q_F for each timestep
qf_external = df_forcing["Tair"].apply(QF_simple)

# Create modified forcing with external Q_F
df_forcing_with_qf = df_forcing.copy()
df_forcing_with_qf["qf"] = qf_external

# Configure SUEWS to use external Q_F
df_state_qf = df_state_init.copy()
df_state_qf.loc[:, "emissionsmethod"] = 0  # Use prescribed Q_F

print("Q_F statistics:")
print(f"  Mean: {qf_external.mean():.2f} W/m²")
print(f"  Max:  {qf_external.max():.2f} W/m²")
print(f"  Min:  {qf_external.min():.2f} W/m²")

# %%
# Run Baseline Simulation (Q_F = 0)
# ---------------------------------
#
# First, run a simulation without anthropogenic heat for comparison.

df_forcing_baseline = df_forcing.copy()
df_forcing_baseline["qf"] = 0

df_state_baseline = df_state_init.copy()
df_state_baseline.loc[:, "emissionsmethod"] = 0

sim_baseline = SUEWSSimulation.from_state(df_state_baseline).update_forcing(df_forcing_baseline)
output_baseline = sim_baseline.run(logging_level=90)

grid = df_state_init.index[0]
df_suews_baseline = output_baseline.SUEWS.loc[grid]

print("Baseline simulation complete (Q_F = 0)")

# %%
# Run Simulation with External Q_F
# --------------------------------
#
# Now run with the temperature-dependent Q_F from our simple model.

sim_with_qf = SUEWSSimulation.from_state(df_state_qf).update_forcing(df_forcing_with_qf)
output_qf = sim_with_qf.run(logging_level=90)

df_suews_qf = output_qf.SUEWS.loc[grid]

print("Coupled simulation complete (with external Q_F)")

# %%
# Compare Q_F Values
# ------------------
#
# Compare the prescribed Q_F with the baseline (zero) case.

fig, axes = plt.subplots(2, 1, figsize=(10, 6), sharex=True)

# Plot Q_F time series
df_suews_qf["QF"].resample("1h").mean().plot(ax=axes[0], label="With external Q_F")
df_suews_baseline["QF"].resample("1h").mean().plot(ax=axes[0], label="Baseline (Q_F=0)")
axes[0].set_ylabel("$Q_F$ (W m$^{-2}$)")
axes[0].set_title("Anthropogenic Heat Flux")
axes[0].legend()

# Plot difference
diff_qf = (df_suews_qf["QF"] - df_suews_baseline["QF"]).resample("1h").mean()
diff_qf.plot(ax=axes[1], color="C2")
axes[1].set_ylabel("$\\Delta Q_F$ (W m$^{-2}$)")
axes[1].set_xlabel("Date")
axes[1].set_title("Q_F Difference (Coupled - Baseline)")
axes[1].axhline(0, color="grey", linestyle="--", alpha=0.5)

plt.tight_layout()
plt.show()

# %%
# Temperature Feedback
# --------------------
#
# Examine how the external Q_F affects the simulated 2-metre temperature.

fig, axes = plt.subplots(2, 1, figsize=(10, 6), sharex=True)

# Plot T2 time series
df_suews_qf["T2"].resample("1h").mean().plot(ax=axes[0], label="With external Q_F")
df_suews_baseline["T2"].resample("1h").mean().plot(ax=axes[0], label="Baseline")
axes[0].set_ylabel("$T_2$ ($^\\circ$C)")
axes[0].set_title("2-Metre Air Temperature")
axes[0].legend()

# Plot temperature difference
diff_t2 = (df_suews_qf["T2"] - df_suews_baseline["T2"]).resample("1h").mean()
diff_t2.plot(ax=axes[1], color="C3")
axes[1].set_ylabel("$\\Delta T_2$ ($^\\circ$C)")
axes[1].set_xlabel("Date")
axes[1].set_title("Temperature Difference (Coupled - Baseline)")
axes[1].axhline(0, color="grey", linestyle="--", alpha=0.5)

plt.tight_layout()
plt.show()

# %%
# Q_F vs Temperature Response
# ---------------------------
#
# Analyse the relationship between Q_F change and temperature change.

# Daily statistics
df_daily_qf = df_suews_qf.resample("1d").mean()
df_daily_baseline = df_suews_baseline.resample("1d").mean()

diff_qf_daily = df_daily_qf["QF"] - df_daily_baseline["QF"]
diff_t2_daily = df_daily_qf["T2"] - df_daily_baseline["T2"]

fig, ax = plt.subplots(figsize=(6, 5))
ax.scatter(diff_qf_daily, diff_t2_daily, alpha=0.7, s=30)
ax.set_xlabel("$\\Delta Q_F$ (W m$^{-2}$)")
ax.set_ylabel("$\\Delta T_2$ ($^\\circ$C)")
ax.set_title("Q_F - Temperature Feedback (Daily Means)")

# Add trend line
if len(diff_qf_daily.dropna()) > 2:
    z = np.polyfit(diff_qf_daily.dropna(), diff_t2_daily.dropna(), 1)
    p = np.poly1d(z)
    x_line = np.linspace(diff_qf_daily.min(), diff_qf_daily.max(), 100)
    ax.plot(x_line, p(x_line), "r--", alpha=0.7, label=f"Slope: {z[0]:.3f} °C/(W/m²)")
    ax.legend()

ax.grid(True, alpha=0.3)
plt.tight_layout()
plt.show()

# %%
# Summary
# -------
#
# This example demonstrated one-way coupling between SUEWS and an external
# anthropogenic heat flux model:
#
# 1. **Simple Q_F model**: Temperature-dependent heating/cooling emissions
# 2. **One-way coupling**: External Q_F prescribed from air temperature
# 3. **Temperature feedback**: Q_F affects simulated air temperature
#
# **Key findings:**
#
# - Anthropogenic heat adds energy to the urban surface
# - Higher Q_F leads to elevated air temperatures
# - The feedback magnitude depends on atmospheric conditions
#
# **Extensions:**
#
# - Two-way coupling: SUEWS T2 drives external model, which provides Q_F
# - Building energy models for detailed Q_F estimation
# - Traffic and metabolic heat contributions
#
# .. note::
#
#    For tight two-way coupling (timestep-by-timestep interaction),
#    low-level SUEWS functions are required. See the source notebook
#    for implementation details.
