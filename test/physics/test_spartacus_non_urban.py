"""Regression tests for non-urban SPARTACUS paths."""

import logging
import warnings

import numpy as np
import pytest

import supy as sp

pytestmark = [pytest.mark.physics, pytest.mark.core]


def _set_columns(df_state, name: str, value: float) -> None:
    for column in df_state.columns:
        if column[0] == name:
            df_state.loc[:, column] = value


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


def _low_first_layer_geometry_state(df_state_init):
    df_state = df_state_init.iloc[[0]].copy()

    _set_columns(df_state, "sfr_surf", 0.0)
    df_state.loc[:, ("sfr_surf", "(0,)")] = 0.3
    df_state.loc[:, ("sfr_surf", "(1,)")] = 0.2
    df_state.loc[:, ("sfr_surf", "(2,)")] = 0.2
    df_state.loc[:, ("sfr_surf", "(3,)")] = 0.3

    _set_columns(df_state, "building_frac", 0.0)
    df_state.loc[:, ("building_frac", "(0,)")] = 0.1
    _set_columns(df_state, "sfr_roof", 0.0)
    df_state.loc[:, ("sfr_roof", "(0,)")] = 0.1
    _set_columns(df_state, "sfr_wall", 0.0)
    df_state.loc[:, ("sfr_wall", "(0,)")] = 0.1
    _set_columns(df_state, "veg_frac", 0.0)
    df_state.loc[:, ("veg_frac", "(0,)")] = 0.1

    df_state.loc[:, ("bldgh", "0")] = 12.0
    df_state.loc[:, ("faibldg", "0")] = 0.4
    df_state.loc[:, ("evetreeh", "0")] = 13.1
    df_state.loc[:, ("faievetree", "0")] = 0.3
    df_state.loc[:, ("dectreeh", "0")] = 12.0
    df_state.loc[:, ("faidectree", "0")] = 0.3

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


def test_spartacus_runs_when_first_layer_geometry_below_land_cover():
    df_state_init, df_forcing = sp.load_SampleData()
    df_state = _low_first_layer_geometry_state(df_state_init)
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
    fluxes = spartacus[
        ["KTopDnDir", "KTopDnDif", "Kup", "Lup", "KNet_Grnd", "LUp_Grnd"]
    ]

    assert (spartacus["KTopDnDir"] > 0).any()
    assert np.isfinite(fluxes.to_numpy()).all()
