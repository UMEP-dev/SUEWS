"""Independent-grid locality witnesses for multi-grid simulations (W2-4b).

Every grid cell in a multi-grid ``SUEWSSimulation`` must evolve independently
of its neighbours: no Fortran module state, no Rayon thread state, no shared
output buffer, and no config serialisation shortcut may leak one site's
parameters or initial state into another site's outputs or native checkpoint.

These witnesses verify:
1. Full 8-case batch-vs-single equivalence across 2 non-consecutive grid ID
   orders (``(41, 73)`` and ``(73, 41)``) x 2 execution modes (``n_jobs=1``
   serial and ``n_jobs=2`` Rayon parallel) x 2 states (baseline and Grid 73
   latitude perturbed ``53 -> 61`` deg, verified to shift solar ``Zenith`` by
   ~8 deg), asserting bit-exact equality across ALL output groups and columns
   as well as exact native checkpoint JSON equality.
2. Multi-subsystem parameter + initial-state perturbation locality (albedo,
   anthropogenic heat, surface fractions, surface water state, soil moisture,
   LAI, surface temperature) across both grid orders, serial/parallel workers,
   and multi-chunk (``chunk_day=1``) checkpoint continuation: Grid 202 fluxes
   change substantially while Grid 101 outputs and native checkpoint remain
   bit-identical (``max_abs_diff == 0.0``) to the single-grid reference.
"""

from __future__ import annotations

from collections.abc import Callable

from conftest import TIMESTEPS_PER_DAY
from layer5_checks import assert_simulation_output_finiteness, strict_max_abs
import pandas as pd
import pytest

from supy import SUEWSSimulation
from supy.data_model import SUEWSConfig

pytestmark = pytest.mark.physics

N_DAYS = 2
N_STEPS = TIMESTEPS_PER_DAY * N_DAYS


def _run_sites(
    base_config: SUEWSConfig,
    df_forcing: pd.DataFrame,
    order: tuple[int, ...],
    mutators: dict[int, Callable[[object], None]],
    *,
    n_jobs: int,
    chunk_day: int = 3660,
) -> dict[int, tuple[pd.DataFrame, str]]:
    """Run ``order`` grids through ``SUEWSSimulation`` and return per-grid output and checkpoint."""
    cfg = base_config.model_copy(deep=True)
    template = cfg.sites[0]
    sites = []
    for grid_id in order:
        site = template.model_copy(deep=True)
        site.gridiv = grid_id
        site.name = f"locality-{grid_id}"
        if grid_id in mutators:
            mutators[grid_id](site)
        sites.append(site)
    cfg.sites = sites

    sim = SUEWSSimulation()
    sim.update_config(cfg, auto_load_forcing=False)
    sim.update_forcing(df_forcing.copy())
    output = sim.run(
        start_date=df_forcing.index[0],
        end_date=df_forcing.index[-1],
        n_jobs=n_jobs,
        chunk_day=chunk_day,
    )
    assert set(output.df.index.get_level_values("grid")) == set(order)
    assert set(sim.checkpoint.grid_states) == set(order)
    assert_simulation_output_finiteness(
        output.df,
        expected_grids=order,
        expected_timestamps=df_forcing.index,
        tstep=300,
    )
    return {
        grid_id: (
            output.df.xs(grid_id, level="grid"),
            sim.checkpoint.grid_states[grid_id],
        )
        for grid_id in order
    }


def _set_latitude(lat_deg: float) -> Callable[[object], None]:
    def _mutate(site) -> None:
        site.properties.lat.value = lat_deg

    return _mutate


def _perturb_parameters_and_initial_state(site) -> None:
    """Strongly perturb both site parameters and initial states on one grid."""
    props = site.properties
    props.land_cover.paved.alb.value = 0.35
    props.land_cover.bldgs.alb.value = 0.35
    props.land_cover.paved.sfr.value = 0.25
    props.land_cover.grass.sfr.value = 0.21
    props.anthropogenic_emissions.heat.qf_a.working_day = 0.85
    props.anthropogenic_emissions.heat.qf_a.holiday = 0.85

    init = site.initial_states
    init.paved.state.value = 0.40
    init.grass.soilstore.value = 60.0
    init.grass.lai_id.value = 3.5
    init.paved.tsfc.value = 25.0


@pytest.fixture(scope="module")
def latitude_locality_controls(sample_config_loaded, sample_data_loaded):
    """Independent single-grid reference runs for the 8-case latitude witness."""
    _, df_forcing_all = sample_data_loaded
    df_forcing = df_forcing_all.iloc[:N_STEPS].copy()
    assert len(df_forcing) == N_STEPS
    assert df_forcing.index[-1] - df_forcing.index[0] == pd.Timedelta(
        hours=48, minutes=-5
    )

    baseline_lats = {41: 42.0, 73: 53.0}
    altered_lats = {41: 42.0, 73: 61.0}
    controls: dict[tuple[int, float], tuple[pd.DataFrame, str]] = {}
    for grid_id, lat_deg in ((41, 42.0), (73, 53.0), (73, 61.0)):
        controls[grid_id, lat_deg] = _run_sites(
            sample_config_loaded,
            df_forcing,
            (grid_id,),
            {grid_id: _set_latitude(lat_deg)},
            n_jobs=1,
        )[grid_id]

    # Positive control: Grid 73's latitude change must move solar Zenith markedly.
    df_73_base, _ = controls[73, 53.0]
    df_73_alt, _ = controls[73, 61.0]
    (zenith_col,) = [
        col for col in df_73_base.columns if col[-1].casefold() == "zenith"
    ]
    peak_zenith_diff = strict_max_abs(
        df_73_base[zenith_col] - df_73_alt[zenith_col], label="dZenith_grid73"
    )
    assert peak_zenith_diff > 1.0, (
        f"Latitude perturbation 53 -> 61 deg moved Zenith by only {peak_zenith_diff} deg"
    )
    return sample_config_loaded, df_forcing, controls, baseline_lats, altered_lats


@pytest.mark.core
@pytest.mark.parametrize("order", [(41, 73), (73, 41)], ids=["41-73", "73-41"])
@pytest.mark.parametrize("n_jobs", [1, 2], ids=["serial", "rayon"])
@pytest.mark.parametrize(
    "perturb_grid_73", [False, True], ids=["baseline", "perturbed-73"]
)
def test_multigrid_latitude_locality_matches_single_grid_runs(
    latitude_locality_controls, order, n_jobs, perturb_grid_73
):
    """Multi-grid outputs and checkpoints must match single-grid runs bit-for-bit.

    Covers both site list orders, serial and Rayon parallel execution, and
    single-grid perturbation of Grid 73 while Grid 41 stays untouched. Any
    config-broadcast shortcut or shared-state bleed across grids fails at least
    one of the 8 cases.
    """
    # ARRANGE
    cfg, df_forcing, controls, baseline_lats, altered_lats = latitude_locality_controls
    latitudes = altered_lats if perturb_grid_73 else baseline_lats
    mutators = {grid_id: _set_latitude(lat) for grid_id, lat in latitudes.items()}

    # ACT
    results = _run_sites(cfg, df_forcing, order, mutators, n_jobs=n_jobs)

    # ASSERT
    for grid_id in order:
        measured, checkpoint = results[grid_id]
        expected, expected_checkpoint = controls[grid_id, latitudes[grid_id]]
        assert len(expected) == N_STEPS
        pd.testing.assert_frame_equal(
            measured,
            expected,
            check_exact=True,
            obj=f"grid {grid_id} (order={order}, n_jobs={n_jobs})",
        )
        assert checkpoint == expected_checkpoint, (
            f"Native checkpoint for grid {grid_id} varied with neighbour configuration "
            f"(order={order}, n_jobs={n_jobs})"
        )


@pytest.fixture(scope="module")
def state_locality_controls(sample_config_loaded, sample_data_loaded):
    """Single-grid reference runs for parameter + initial-state locality checks."""
    _, df_forcing_all = sample_data_loaded
    df_forcing = df_forcing_all.iloc[:N_STEPS].copy()
    ctrl_101 = _run_sites(sample_config_loaded, df_forcing, (101,), {}, n_jobs=1)[101]
    ctrl_202_base = _run_sites(sample_config_loaded, df_forcing, (202,), {}, n_jobs=1)[
        202
    ]
    ctrl_202_pert = _run_sites(
        sample_config_loaded,
        df_forcing,
        (202,),
        {202: _perturb_parameters_and_initial_state},
        n_jobs=1,
    )[202]
    return sample_config_loaded, df_forcing, ctrl_101, ctrl_202_base, ctrl_202_pert


@pytest.mark.core
@pytest.mark.parametrize("order", [(101, 202), (202, 101)], ids=["101-202", "202-101"])
@pytest.mark.parametrize(
    ("n_jobs", "chunk_day"),
    [(1, 3660), (2, 3660), (2, 1)],
    ids=["serial-whole", "rayon-whole", "rayon-chunked"],
)
def test_multigrid_parameter_and_initial_state_perturbation_locality(
    state_locality_controls, order, n_jobs, chunk_day
):
    """Perturbing Grid 202 parameters and initial states must leave Grid 101 bit-identical."""
    # ARRANGE
    cfg, df_forcing, ctrl_101, ctrl_202_base, ctrl_202_pert = state_locality_controls
    tag = f"order={order}, n_jobs={n_jobs}, chunk_day={chunk_day}"

    # ACT
    results = _run_sites(
        cfg,
        df_forcing,
        order,
        {202: _perturb_parameters_and_initial_state},
        n_jobs=n_jobs,
        chunk_day=chunk_day,
    )

    # ASSERT: Grid 202 responds strongly across energy and water fluxes.
    for col in ("QN", "Kup", "QF", "QH", "QE", "State", "Evap"):
        max_diff = strict_max_abs(
            results[202][0]["SUEWS", col] - ctrl_202_base[0]["SUEWS", col],
            label=f"d{col}_grid202",
        )
        assert max_diff > 1e-4, (
            f"Grid 202 perturbation failed to move SUEWS.{col} (max_diff={max_diff})"
        )
    pd.testing.assert_frame_equal(
        results[202][0], ctrl_202_pert[0], check_exact=True, obj=f"grid 202 ({tag})"
    )
    assert results[202][1] == ctrl_202_pert[1]

    # ASSERT: Grid 101 remains bit-identical across every output column and checkpoint.
    pd.testing.assert_frame_equal(
        results[101][0], ctrl_101[0], check_exact=True, obj=f"grid 101 ({tag})"
    )
    assert results[101][1] == ctrl_101[1]
