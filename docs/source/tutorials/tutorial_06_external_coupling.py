r"""
External Model Coupling
=======================

Coupling SUEWS with external models for anthropogenic heat flux.

This example demonstrates how SUEWS can be coupled to external models that
provide forcing data. We use a simple anthropogenic heat flux (:math:`Q_F`) model
driven by outdoor air temperature to show the coupling approach.

**Key concepts:**

- Anthropogenic heat flux depends on outdoor temperature
- SUEWS can receive external :math:`Q_F` as forcing input
- One-way coupling: external model provides forcing to SUEWS
- Temperature feedback affects urban energy balance

**API approach**: This tutorial uses the :class:`~supy.SUEWSSimulation` OOP interface but
extracts DataFrames for forcing modification. This hybrid pattern is required
for external model coupling where forcing variables must be modified at runtime.
"""

import os

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

from supy import SUEWSSimulation

_ON_RTD = os.environ.get("READTHEDOCS") == "True"

# %%
# .. note::
#
#    **RTD build note**: This tutorial uses reduced simulation parameters
#    on ReadTheDocs to fit within build resource limits. Coupling is
#    demonstrated over 1 month on RTD (2 months locally).
#    Temperature-QF feedback statistics use fewer data points. Run the
#    script locally for complete results.

# %%
# Simple Anthropogenic Heat Model
# -------------------------------
#
# We define a simple :math:`Q_F` model based on building heating and cooling:
#
# - Heating activates when T < 10 :math:`^{\circ}C`
# - Cooling activates when T > 20 :math:`^{\circ}C`
# - Between 10-20 :math:`^{\circ}C`: baseline :math:`Q_F` = 0
#
# This simplified model captures the temperature-dependent nature of
# building energy use without requiring detailed building simulation.


def QF_simple(T2):
    """Calculate anthropogenic heat flux from air temperature.

    Parameters
    ----------
    T2 : float
        2-metre air temperature (:math:`^{\circ}C`)

    Returns
    -------
    float
        Anthropogenic heat flux (W m\ :sup:`-2`)
    """
    T_cool = 20  # Cooling threshold (degC)
    T_heat = 10  # Heating threshold (degC)
    C_B = 5  # Cooling rate (W m^-2 K^-1)
    H_B = 10  # Heating rate (W m^-2 K^-1)
    scale = 0.3  # Scaling factor

    if T2 > T_cool:  # noqa: SIM300
        qf = (T2 - T_cool) * C_B
    elif T2 < T_heat:  # noqa: SIM300
        qf = (T_heat - T2) * H_B
    else:
        qf = 0

    return qf * scale


# %%
# Visualise the :math:`Q_F` Model
# -------------------------------
#
# Plot the :math:`Q_F` response to temperature to understand the model behaviour.

temp_range = np.arange(-5, 45, 0.5)
qf_values = [QF_simple(t) for t in temp_range]

# Create DataFrames for plotting by region
df_qf = pd.DataFrame({"Temperature": temp_range, "QF": qf_values})

fig, ax = plt.subplots(figsize=(8, 5))

# Heating region (T < 10 degC)
mask_heat = temp_range < 10
ax.plot(temp_range[mask_heat], np.array(qf_values)[mask_heat], "C1-", label="Heating", linewidth=2)

# Neutral region (10 <= T <= 20 degC)
mask_neutral = (temp_range >= 10) & (temp_range <= 20)
ax.plot(
    temp_range[mask_neutral], np.array(qf_values)[mask_neutral], "C2-", label="Baseline", linewidth=2
)

# Cooling region (T > 20 degC)
mask_cool = temp_range > 20
ax.plot(temp_range[mask_cool], np.array(qf_values)[mask_cool], "C0-", label="Cooling", linewidth=2)

# Annotations
ax.axvline(10, color="grey", linestyle="--", alpha=0.5)
ax.axvline(20, color="grey", linestyle="--", alpha=0.5)
ax.annotate("$T_H$ = 10$^{\\circ}$C", xy=(10, 5), fontsize=10)
ax.annotate("$T_C$ = 20$^{\\circ}$C", xy=(20, 5), fontsize=10)

ax.set_xlabel("Air Temperature ($^\\circ$C)")
ax.set_ylabel("$Q_F$ (W m$^{-2}$)")
ax.set_title("Simple Anthropogenic Heat Flux Model")
ax.legend()
ax.grid(True, alpha=0.3)
plt.tight_layout()

# sphinx_gallery_thumbnail_number = 1

# %%
# Load Sample Data
# ----------------
#
# Load the sample dataset and prepare for coupled simulation.

sim = SUEWSSimulation.from_sample_data()
df_state_init = sim.state_init.copy()

# Disable snow module (not needed for this site)
df_state_init.loc[:, "snowuse"] = 0

# Slice forcing by time (returns SUEWSForcing object)
# Use shorter period for faster tutorial execution
_end_month_06 = "2012-01" if _ON_RTD else "2012-02"
forcing_sliced = sim.forcing["2012-01":_end_month_06].iloc[1:]

# %%
# .. important::
#
#    **External coupling requires DataFrame extraction**: When injecting
#    variables from external models (here: :math:`Q_F`), you must extract the
#    DataFrame with ``.df``, add the new column, then pass it to the
#    simulation. This is the expected pattern for model coupling.

# Get DataFrame for modification (adding external :math:`Q_F`)
df_forcing = forcing_sliced.df

print(f"Simulation period: {df_forcing.index[0]} to {df_forcing.index[-1]}")
print(f"Time steps: {len(df_forcing)}")

# %%
# Calculate External :math:`Q_F` from Forcing Data
# ------------------------------------------------
#
# Apply the simple :math:`Q_F` model to the air temperature from the forcing data.
# This is a one-way coupling: the external model uses prescribed temperatures.

# Calculate :math:`Q_F` for each timestep
qf_external = df_forcing["Tair"].apply(QF_simple)

# Create modified forcing with external :math:`Q_F`
df_forcing_with_qf = df_forcing.copy()
df_forcing_with_qf["qf"] = qf_external

# Configure SUEWS to use external :math:`Q_F`
df_state_qf = df_state_init.copy()
df_state_qf.loc[:, "emissionsmethod"] = 0  # Use prescribed Q_F

print("Q_F statistics:")
print(f"  Mean: {qf_external.mean():.2f} W/m^2")
print(f"  Max:  {qf_external.max():.2f} W/m^2")
print(f"  Min:  {qf_external.min():.2f} W/m^2")

# %%
# Run Baseline Simulation (:math:`Q_F` = 0)
# -----------------------------------------
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
# Run Simulation with External :math:`Q_F`
# ----------------------------------------
#
# Now run with the temperature-dependent :math:`Q_F` from our simple model.

sim_with_qf = SUEWSSimulation.from_state(df_state_qf).update_forcing(df_forcing_with_qf)
output_qf = sim_with_qf.run(logging_level=90)

df_suews_qf = output_qf.SUEWS.loc[grid]

print("Coupled simulation complete (with external Q_F)")

# %%
# Compare :math:`Q_F` Values
# --------------------------
#
# Compare the prescribed :math:`Q_F` with the baseline (zero) case.

fig, axes = plt.subplots(2, 1, figsize=(10, 6), sharex=True)

# Plot Q_F time series
df_suews_qf["QF"].resample("1h").mean().plot(ax=axes[0], label="With external $Q_F$")
df_suews_baseline["QF"].resample("1h").mean().plot(ax=axes[0], label="Baseline ($Q_F$=0)")
axes[0].set_ylabel("$Q_F$ (W m$^{-2}$)")
axes[0].set_title("Anthropogenic Heat Flux")
axes[0].legend()

# Plot difference
diff_qf = (df_suews_qf["QF"] - df_suews_baseline["QF"]).resample("1h").mean()
diff_qf.plot(ax=axes[1], color="C2")
axes[1].set_ylabel("$\\Delta Q_F$ (W m$^{-2}$)")
axes[1].set_xlabel("Date")
axes[1].set_title("$Q_F$ Difference (Coupled - Baseline)")
axes[1].axhline(0, color="grey", linestyle="--", alpha=0.5)

plt.tight_layout()

# %%
# Temperature Feedback
# --------------------
#
# Examine how the external :math:`Q_F` affects the simulated 2-metre temperature.

fig, axes = plt.subplots(2, 1, figsize=(10, 6), sharex=True)

# Plot T2 time series
df_suews_qf["T2"].resample("1h").mean().plot(ax=axes[0], label="With external $Q_F$")
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

# %%
# :math:`Q_F` vs Temperature Response
# -----------------------------------
#
# Analyse the relationship between :math:`Q_F` change and temperature change.

# Daily statistics
df_daily_qf = df_suews_qf.resample("1D").mean()
df_daily_baseline = df_suews_baseline.resample("1D").mean()

diff_qf_daily = df_daily_qf["QF"] - df_daily_baseline["QF"]
diff_t2_daily = df_daily_qf["T2"] - df_daily_baseline["T2"]

fig, ax = plt.subplots(figsize=(6, 5))
ax.scatter(diff_qf_daily, diff_t2_daily, alpha=0.7, s=30)
ax.set_xlabel("$\\Delta Q_F$ (W m$^{-2}$)")
ax.set_ylabel("$\\Delta T_2$ ($^\\circ$C)")
ax.set_title("$Q_F$ - Temperature Feedback (Daily Means)")

# Add trend line
if len(diff_qf_daily.dropna()) > 2:
    z = np.polyfit(diff_qf_daily.dropna(), diff_t2_daily.dropna(), 1)
    p = np.poly1d(z)
    x_line = np.linspace(diff_qf_daily.min(), diff_qf_daily.max(), 100)
    ax.plot(x_line, p(x_line), "r--", alpha=0.7, label=f"Slope: {z[0]:.3f} $^{{\\circ}}$C/(W/m$^2$)")
    ax.legend()

ax.grid(True, alpha=0.3)
plt.tight_layout()

# %%
# Summary
# -------
#
# This example demonstrated one-way coupling between SUEWS and an external
# anthropogenic heat flux model:
#
# 1. Simple :math:`Q_F` model: Temperature-dependent heating/cooling emissions
# 2. One-way coupling: External :math:`Q_F` prescribed from air temperature
# 3. Temperature feedback: :math:`Q_F` affects simulated air temperature
#
# **Key findings:**
#
# - Anthropogenic heat adds energy to the urban surface
# - Higher :math:`Q_F` leads to elevated air temperatures
# - The feedback magnitude depends on atmospheric conditions
#
# **Extensions:**
#
# - Two-way coupling: SUEWS T2 drives external model, which provides :math:`Q_F`
# - Building energy models for detailed :math:`Q_F` estimation
# - Traffic and metabolic heat contributions
#
# .. note::
#
#    For tight two-way coupling (timestep-by-timestep interaction),
#    low-level SUEWS functions are required. See the source notebook
#    for implementation details.
