"""Regression tests for non-urban SPARTACUS paths."""

import logging
import warnings

import numpy as np
import pytest

import supy as sp

pytestmark = [pytest.mark.physics, pytest.mark.core]


def _forest_only_direct_albedo_state(df_state_init):
    df_state = df_state_init.iloc[[0]].copy()

    for idx in range(7):
        df_state.loc[:, ("sfr_surf", f"({idx},)")] = 0.0
    df_state.loc[:, ("sfr_surf", "(2,)")] = 1.0

    for idx in range(3):
        df_state.loc[:, ("building_frac", f"({idx},)")] = 0.0
        df_state.loc[:, ("veg_frac", f"({idx},)")] = 0.0
    df_state.loc[:, ("veg_frac", "(0,)")] = 1.0

    df_state.loc[:, ("bldgh", "0")] = 0.0
    df_state.loc[:, ("faibldg", "0")] = 0.0
    df_state.loc[:, ("evetreeh", "0")] = 13.1
    df_state.loc[:, ("faievetree", "0")] = 0.3
    df_state.loc[:, ("dectreeh", "0")] = 0.0
    df_state.loc[:, ("faidectree", "0")] = 0.0

    df_state.loc[:, ("netradiationmethod", "0")] = 1001
    df_state.loc[:, ("use_sw_direct_albedo", "0")] = 1.0

    return df_state


def test_non_urban_spartacus_direct_albedo_runs_without_urban_arrays():
    df_state_init, df_forcing = sp.load_SampleData()
    df_state = _forest_only_direct_albedo_state(df_state_init)
    df_forcing_daytime = df_forcing.loc[df_forcing["kdown"] > 100].iloc[:2]

    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        df_output, _ = sp.run_supy(
            df_forcing_daytime,
            df_state,
            logging_level=logging.CRITICAL,
            check_input=False,
            save_state=False,
        )

    assert "SPARTACUS" in df_output.columns.get_level_values(0)

    spartacus = df_output.xs("SPARTACUS", level=0, axis=1)
    forest_fluxes = spartacus[
        ["KTopDnDir", "KTopDnDif", "Kup", "Lup", "KNet_Grnd", "LUp_Grnd"]
    ]

    assert (spartacus["KTopDnDir"] > 0).any()
    assert np.isfinite(forest_fluxes.to_numpy()).all()
