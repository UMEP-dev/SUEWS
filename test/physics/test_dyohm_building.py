"""Regression tests for the DyOHM-building storage-heat option."""

from importlib import import_module
import logging
import warnings

import numpy as np
import pytest

import supy as sp

pytestmark = [pytest.mark.physics, pytest.mark.core]


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
    # paved-only method-16 run can collapse exactly to traditional OHM.
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
        df_output, _ = sp.run_supy(
            df_forcing,
            df_state,
            logging_level=logging.CRITICAL,
            check_input=False,
            save_state=False,
        )
    return df_output.SUEWS


@pytest.mark.skipif(
    not _rust_library_available(),
    reason="Rust library backend not available (install src/suews_bridge with physics feature)",
)
def test_dyohm_building_is_storage_only_relative_to_ohm():
    df_state_init, df_forcing_all = sp.load_SampleData()
    start = int(np.flatnonzero(df_forcing_all["kdown"].to_numpy() > 100)[0])
    df_forcing = df_forcing_all.iloc[start : start + 24]

    ohm_paved = _run_storage_case(df_state_init, df_forcing, 1, building_fraction=0.0)
    dyohm_paved = _run_storage_case(df_state_init, df_forcing, 16, building_fraction=0.0)
    np.testing.assert_allclose(
        dyohm_paved["QS"].to_numpy(),
        ohm_paved["QS"].to_numpy(),
        rtol=0.0,
        atol=1.0e-7,
    )

    ohm_mixed = _run_storage_case(df_state_init, df_forcing, 1, building_fraction=0.3)
    dyohm_mixed = _run_storage_case(df_state_init, df_forcing, 16, building_fraction=0.3)

    np.testing.assert_allclose(
        dyohm_mixed["QN"].to_numpy(),
        ohm_mixed["QN"].to_numpy(),
        rtol=0.0,
        atol=1.0e-7,
    )
    qs_delta = np.abs(dyohm_mixed["QS"].to_numpy() - ohm_mixed["QS"].to_numpy())
    assert np.max(qs_delta) > 1.0e-4
