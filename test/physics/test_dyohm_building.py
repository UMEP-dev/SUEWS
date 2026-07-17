"""Regression tests for the DyOHM-building storage-heat option."""

from importlib import import_module
import logging
from pathlib import Path
import warnings

import numpy as np
import pandas as pd
import pytest

import supy as sp

pytestmark = [pytest.mark.physics, pytest.mark.core]

QS_SURF_COLUMNS = [
    "QS_Paved",
    "QS_Bldgs",
    "QS_EveTr",
    "QS_DecTr",
    "QS_Grass",
    "QS_BSoil",
    "QS_Water",
]
TS_DYOHM_COLUMNS = [
    "Ts_Paved_dyohm",
    "Ts_Bldgs_dyohm",
    "Ts_EveTr_dyohm",
    "Ts_DecTr_dyohm",
    "Ts_Grass_dyohm",
    "Ts_BSoil_dyohm",
    "Ts_Water_dyohm",
]


def _rust_library_available() -> bool:
    """Return True when the Rust Python bridge exposes run_suews()."""
    for module_name in ("supy.suews_bridge", "suews_bridge"):
        try:
            module = import_module(module_name)
        except Exception:
            continue
        if hasattr(module, "run_suews"):
            return True
    return False


def _state_for_storage_method(df_state_init, method: int, building_fraction: float):
    df_state = df_state_init.iloc[[0]].copy()

    fractions = [0.0] * 7
    fractions[0] = 1.0 - building_fraction
    fractions[1] = building_fraction
    for idx, value in enumerate(fractions):
        df_state.loc[:, ("sfr_surf", f"({idx},)")] = value

    df_state.loc[:, ("storageheatmethod", "0")] = method
    df_state.loc[:, ("ohmincqf", "0")] = 0

    # Make the aggregate and surface OHM histories identical at startup so a
    # paved-only method-8 run can collapse exactly to traditional OHM.
    qn_av = df_state.loc[:, ("qn_av", "0")].iloc[0]
    dqndt = df_state.loc[:, ("dqndt", "0")].iloc[0]
    for idx in range(7):
        df_state.loc[:, ("qn_surfs", f"({idx},)")] = qn_av
        df_state.loc[:, ("dqndt_surf", f"({idx},)")] = dqndt

    return df_state


def _run_storage_case(df_state_init, df_forcing, method: int, building_fraction: float):
    df_state = _state_for_storage_method(df_state_init, method, building_fraction)
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        # check_input=False: the state is hand-edited above (surface fractions,
        # storage-heat method) and deliberately bypasses YAML-level validation;
        # the physics contract under test does not depend on it.
        df_output, _ = sp.run_supy(
            df_forcing,
            df_state,
            logging_level=logging.CRITICAL,
            check_input=False,
            save_state=False,
        )
    return df_output.SUEWS


def _set_outer_material(df_state, surface_kind: str, first_index: int, values):
    """Return state with one aggregate/facet outer material layer replaced."""
    updated = df_state.copy()
    for field, value in zip(("dz", "cp", "k"), values):
        updated.loc[:, (f"{field}_{surface_kind}", f"({first_index}, 0)")] = value
    return updated


@pytest.mark.skipif(
    not _rust_library_available(),
    reason="Rust library backend not available (install src/suews_bridge with physics feature)",
)
def test_dyohm_building_qs_behavior_relative_to_ohm():
    df_state_init, df_forcing_all = sp.load_SampleData()
    df_forcing = df_forcing_all.loc["2012-06-01 00:05:00":"2012-06-03 00:00:00"]

    ohm_paved = _run_storage_case(df_state_init, df_forcing, 1, building_fraction=0.0)
    dyohm_paved = _run_storage_case(df_state_init, df_forcing, 8, building_fraction=0.0)
    np.testing.assert_allclose(
        dyohm_paved["QS"].to_numpy(),
        ohm_paved["QS"].to_numpy(),
        rtol=0.0,
        atol=1.0e-7,
    )

    ohm_mixed = _run_storage_case(df_state_init, df_forcing, 1, building_fraction=0.3)
    dyohm_mixed = _run_storage_case(df_state_init, df_forcing, 8, building_fraction=0.3)

    qs_delta = np.abs(dyohm_mixed["QS"].to_numpy() - ohm_mixed["QS"].to_numpy())
    warm_start = df_forcing.index[0] + pd.Timedelta(days=1)
    warm_mask = dyohm_mixed.index.get_level_values("datetime") >= warm_start
    max_qs_delta = float(np.nanmax(qs_delta[warm_mask]))
    assert max_qs_delta > 1.0e-4, (
        "storage_heat=8 should alter storage heat flux when buildings are present; "
        f"max |delta QS| after DyOHM coefficient spin-up was {max_qs_delta:.3e}"
    )

    # Per-surface storage heat fluxes must close against the grid flux:
    # QS = SUM(sfr_i * QS_i) holds by construction for method 8 (snow-free),
    # because non-building surfaces carry their own static-OHM fluxes and the
    # building carries the DyOHM flux used in the grid replacement term.
    building_fraction = 0.3
    sfr = np.array([1.0 - building_fraction, building_fraction, 0, 0, 0, 0, 0])
    qs_weighted = dyohm_mixed[QS_SURF_COLUMNS].to_numpy() @ sfr
    np.testing.assert_allclose(
        qs_weighted,
        dyohm_mixed["QS"].to_numpy(),
        rtol=0.0,
        atol=1.0e-6,
        err_msg="per-surface QS does not close against grid QS under method 8",
    )


@pytest.mark.skipif(
    not _rust_library_available(),
    reason="Rust library backend not available (install src/suews_bridge with physics feature)",
)
@pytest.mark.parametrize("storage_heat_method", [6, 8])
def test_dyohm_uses_building_material_not_wall(storage_heat_method):
    df_state_init, df_forcing_all = sp.load_SampleData()
    df_forcing = df_forcing_all.loc["2012-06-01 00:05:00":"2012-06-03 00:00:00"]

    baseline = _run_storage_case(
        df_state_init, df_forcing, storage_heat_method, building_fraction=0.3
    )
    changed_building_state = _set_outer_material(
        df_state_init, "surf", 1, (0.35, 1_800_000.0, 0.7)
    )
    changed_wall_state = _set_outer_material(
        df_state_init, "wall", 0, (0.35, 1_800_000.0, 0.7)
    )
    changed_building = _run_storage_case(
        changed_building_state,
        df_forcing,
        storage_heat_method,
        building_fraction=0.3,
    )
    changed_wall = _run_storage_case(
        changed_wall_state,
        df_forcing,
        storage_heat_method,
        building_fraction=0.3,
    )

    np.testing.assert_allclose(
        changed_wall["QS"].to_numpy(),
        baseline["QS"].to_numpy(),
        rtol=0.0,
        atol=1.0e-7,
        err_msg="DyOHM-building must not read SPARTACUS wall material properties",
    )

    qs_delta = np.abs(changed_building["QS"].to_numpy() - baseline["QS"].to_numpy())
    assert float(np.nanmax(qs_delta)) > 1.0e-4, (
        "DyOHM-building did not respond to land_cover.bldgs material layer 0"
    )


@pytest.mark.skipif(
    not _rust_library_available(),
    reason="Rust library backend not available (install src/suews_bridge with physics feature)",
)
def test_dyohm_tsurf_diagnostic_scope():
    """Ts_*_dyohm evolves for pre-existing methods; method 8 skips it.

    Regression for the driver guard in suews_update_tsurf_dyohm: an early
    RETURN keyed too broadly would freeze the Ts_*_dyohm diagnostic columns
    for ordinary OHM runs, silently diverging from the vendored reference
    outputs.
    """
    df_state_init, df_forcing_all = sp.load_SampleData()
    df_forcing = df_forcing_all.loc["2012-06-01 00:05:00":"2012-06-02 00:00:00"]

    ohm_run = _run_storage_case(df_state_init, df_forcing, 1, building_fraction=0.3)
    for col in TS_DYOHM_COLUMNS:
        assert ohm_run[col].nunique() > 1, (
            f"{col} is frozen under storage_heat=1; the DyOHM surface-temperature "
            "diagnostic must keep evolving for pre-existing storage-heat methods"
        )

    dyohm_bldg_run = _run_storage_case(df_state_init, df_forcing, 8, building_fraction=0.3)
    for col in TS_DYOHM_COLUMNS:
        assert dyohm_bldg_run[col].nunique() == 1, (
            f"{col} varies under storage_heat=8; dyohm_building does not "
            "calculate DyOHM conductive surface temperatures by design"
        )


@pytest.mark.skipif(
    not _rust_library_available(),
    reason="Rust library backend not available (install src/suews_bridge with physics feature)",
)
def test_method7_stebbs_owns_building_temperature_and_materials():
    config_path = (
        Path(__file__).parents[1]
        / "fixtures"
        / "data_test"
        / "stebbs_test"
        / "sample_config.yml"
    )
    df_state = sp.init_supy(str(config_path))
    df_forcing_all = sp.load_forcing_grid(
        str(config_path), df_state.index[0], df_state_init=df_state
    )
    df_forcing = df_forcing_all.loc["2017-08-26"].iloc[:12]
    changed_building_state = _set_outer_material(
        df_state, "surf", 1, (0.0, 0.0, 0.0)
    )

    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        baseline, _ = sp.run_supy(
            df_forcing,
            df_state,
            logging_level=logging.CRITICAL,
            check_input=False,
            save_state=False,
        )
        changed_building, _ = sp.run_supy(
            df_forcing,
            changed_building_state,
            logging_level=logging.CRITICAL,
            check_input=False,
            save_state=False,
        )

    np.testing.assert_allclose(
        changed_building.to_numpy(),
        baseline.to_numpy(),
        rtol=0.0,
        atol=1.0e-10,
        equal_nan=True,
        err_msg="method 7 must not read land_cover.bldgs material properties",
    )
    assert baseline.SUEWS["Ts_Bldgs_dyohm"].nunique() == 1
    assert baseline.SUEWS["Ts_Paved_dyohm"].nunique() > 1
