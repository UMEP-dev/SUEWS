"""
Parallel Multi-Grid Execution
=============================

Run several SUEWS grid cells in one simulation, with parallelism controlled by
``n_jobs``.

Many urban-climate workflows need more than one site at a time: comparing
neighbourhoods that differ in land cover, evaluating scenarios over a city's
grid cells, or producing a small sensitivity ensemble. SUEWS handles this
natively. A configuration with multiple sites is dispatched through the
Rust/Rayon bridge; you supply the sites, call ``sim.run(n_jobs=...)``, and
choose whether to use the default thread pool, serial execution, or a capped
number of worker threads.

This tutorial covers two things:

- Part 1 -- a four-site ensemble that varies surface composition, run in one
  ``sim.run(n_jobs=-1)`` call.
- Part 2 -- a short timing appendix measuring wall-clock time at
  N = 1, 4, 8, 16 grids, so you can calibrate expectations on your own
  machine.

**API approach**: This tutorial uses the :class:`~supy.SUEWSSimulation` OOP
interface. The bundled sample data contains one site, so the tutorial copies
that site configuration into a small multi-grid ensemble and assigns each copy
a unique ``gridiv``. Real multi-grid workflows use the same shape: one entry in
``config.sites`` per grid cell.
"""

# %%
# Setup
# -----
#
# Start from the bundled sample dataset, then build a small multi-site
# configuration by copying the sample site. The shared sample forcing is reused
# for all four sites.

import time

import matplotlib.pyplot as plt
import numpy as np

from supy import SUEWSSimulation


def make_multi_site_simulation(n_sites: int) -> SUEWSSimulation:
    """Create an N-site sample simulation with unique names and grid IDs."""
    if n_sites < 1:
        raise ValueError("n_sites must be at least 1")

    base = SUEWSSimulation.from_sample_data()
    config = base.config.model_copy(deep=True)
    template_site = config.sites[0]

    sites = []
    for idx in range(n_sites):
        site = template_site.model_copy(deep=True)
        site.name = f"KCL_{idx + 1:02d}"
        site.gridiv = idx + 1
        sites.append(site)

    config.sites = sites

    sim = SUEWSSimulation(config)
    sim.update_forcing(base.forcing.df.copy())
    return sim


sim = make_multi_site_simulation(4)

# Slice the forcing to the first month for a fast tutorial run.
forcing_one_month = sim.forcing.df["2012-01":"2012-01"].iloc[1:]
sim.update_forcing(forcing_one_month)

print(f"Configured {len(sim.config.sites)} sites:")
for site in sim.config.sites:
    print(f"  name={site.name}  gridiv={site.gridiv}")

# %%
# Part 1: Four-Site Land-Cover Ensemble
# -------------------------------------
#
# Vary surface composition across the four sites to build a small land-cover
# gradient: from a paved-dominated site through to a grass-dominated one. This
# stands in for comparing four neighbourhoods or four idealised land-cover
# archetypes.

# Each row is (paved, bldgs, grass) for one site; the remaining surface
# fractions are zeroed for clarity. The four mixes span concrete-heavy through
# to vegetation-heavy.
land_cover_mix = [
    (0.80, 0.10, 0.10),  # KCL_01: heavily paved
    (0.50, 0.30, 0.20),  # KCL_02: mixed urban
    (0.30, 0.20, 0.50),  # KCL_03: greener
    (0.10, 0.10, 0.80),  # KCL_04: grass-dominated
]

for site, (paved, bldgs, grass) in zip(sim.config.sites, land_cover_mix):
    lc = site.properties.land_cover
    lc.paved.sfr = paved
    lc.bldgs.sfr = bldgs
    lc.grass.sfr = grass
    lc.evetr.sfr = 0.0
    lc.dectr.sfr = 0.0
    lc.bsoil.sfr = 0.0
    lc.water.sfr = 0.0

# %%
# Run All Four Sites in One Call
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# A single ``sim.run(n_jobs=-1)`` dispatches all four sites using the
# Rust/Rayon default thread pool. The forcing is cloned per thread so writes
# from the Fortran kernel stay isolated. Use ``n_jobs=1`` for a serial run, or
# a positive value such as ``n_jobs=4`` to cap the Rayon worker count.

output = sim.run(n_jobs=-1)

print(f"Output grids: {output.grids}")
print(
    f"Output shape: {output.df.shape}  # rows = grids x timesteps, columns = variables"
)

# %%
# Plot Per-Site Air Temperature
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# ``output.SUEWS.T2`` returns a Series with a ``(grid, datetime)`` MultiIndex.
# ``unstack`` gives one column per grid, which plots cleanly.

df_t2 = output.SUEWS.T2.unstack(level="grid")
df_t2.columns = [
    f"{site.name} ({pv:.0%} paved, {gr:.0%} grass)"
    for site, (pv, _, gr) in zip(sim.config.sites, land_cover_mix)
]

# Daily mean for readability on a one-month plot.
df_t2_daily = df_t2.resample("1D").mean()

fig, ax = plt.subplots(figsize=(9, 5))
df_t2_daily.plot(ax=ax)
ax.set_ylabel(r"Daily mean $T_2$ ($^\circ$C)")
ax.set_xlabel("Date")
ax.set_title("Air temperature across a four-site land-cover ensemble")
ax.legend(title="Site", loc="best", fontsize=9)
ax.grid(alpha=0.3)
plt.tight_layout()

# sphinx_gallery_thumbnail_number = 1

# %%
# The grass-dominated site (``KCL_04``) is consistently cooler than the
# paved-dominated site (``KCL_01``) by daytime evapotranspiration: the same
# contrast you would get from running four single-site simulations in a loop,
# but without the loop.

# %%
# Part 2: Timing Appendix
# -----------------------
#
# How much faster is multi-site execution as the number of grids grows? We time
# the same forcing window at N = 1, 4, 8, 16 sites. The exact numbers depend on
# your machine's core count and what else it is doing; the shape of the curve is
# what matters.
#
# Each iteration recreates the simulation so the timing reflects a clean
# end-to-end run rather than an in-place re-execution.


def time_run(n_sites: int, n_steps: int = 576, n_jobs: int = -1) -> float:
    """Measure wall-clock seconds for a fresh N-site run."""
    sim_n = make_multi_site_simulation(n_sites)
    forcing_short = sim_n.forcing.df.iloc[1 : n_steps + 1]  # about two days
    sim_n.update_forcing(forcing_short)

    t0 = time.perf_counter()
    sim_n.run(n_jobs=n_jobs)
    return time.perf_counter() - t0


grid_counts = [1, 4, 8, 16]
wall_seconds = [time_run(n, n_jobs=-1) for n in grid_counts]

for n, elapsed in zip(grid_counts, wall_seconds):
    print(
        f"  N={n:>3d} grids, n_jobs=-1: {elapsed:6.2f}s  ({n / elapsed:5.1f} grids/s)"
    )

# %%
# Plot Wall-Clock vs Grid Count
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

fig, ax = plt.subplots(figsize=(7, 5))
ax.plot(grid_counts, wall_seconds, marker="o")
ax.set_xlabel("Number of grids")
ax.set_ylabel("Wall-clock time (s)")
ax.set_title("Multi-grid execution time (2-day forcing)")
ax.set_xscale("log", base=2)
ax.set_xticks(grid_counts)
ax.set_xticklabels([str(n) for n in grid_counts])
ax.grid(alpha=0.3, which="both")

# Reference: linear scaling from the N=1 baseline.
linear_ref = np.array(wall_seconds[0]) * np.array(grid_counts)
ax.plot(
    grid_counts,
    linear_ref,
    linestyle="--",
    alpha=0.5,
    label="serial scaling (N x baseline)",
)
ax.legend()
plt.tight_layout()

# %%
# On a typical multi-core laptop the curve sits well below the dashed
# linear-scaling reference: doubling the grid count costs noticeably less than
# double the wall-clock time, up to roughly your physical core count. Beyond
# that the gains taper off as threads compete for cores.

# %%
# Caveats
# -------
#
# A few things to keep in mind when scaling up:
#
# 1. **Memory scales with grid count**. The forcing array is cloned per thread
#    because the Fortran kernel mutates it in place, so peak memory use grows
#    roughly linearly with the number of sites in flight.
# 2. **Each site needs its own config entry**. ``config.sites`` is the unit of
#    parallelism: one entry per grid, with a unique ``gridiv``.
# 3. **``n_jobs`` controls the per-call worker policy**. ``n_jobs=-1`` uses the
#    Rust/Rayon default and is the normal choice for production runs.
#    ``n_jobs=1`` forces serial execution for debugging or benchmarking, while
#    values greater than 1 cap the Rayon worker count.
#
# **Next steps**:
#
# - :doc:`tutorial_04_impact_studies` -- scenario sweeps that combine naturally
#   with multi-site setups.
# - :doc:`tutorial_05_results_analysis` -- MultiIndex result handling (the
#   ``unstack(level="grid")`` pattern used here).
