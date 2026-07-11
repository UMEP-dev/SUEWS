"""Regression tests for smooth OHM coefficient transitions (gh#473)."""

import logging
import warnings

import numpy as np
import pytest

import supy as sp

pytestmark = [pytest.mark.physics, pytest.mark.core]

SUMMER_WET = 0
SUMMER_DRY = 1
WINTER_WET = 2
WINTER_DRY = 3
PAVED = 0
GRASS = 4


def _set_vector(state, variable, values):
    for index, value in enumerate(values):
        state.loc[:, (variable, f"({index},)")] = value


def _set_ohm_coefficients(state, surface, regime_values):
    """Set a3 by regime while keeping a1/a2 fixed to isolate blending."""
    for regime, a3 in regime_values.items():
        for coefficient, value in enumerate((0.5, 0.2, a3)):
            state.loc[:, ("ohm_coef", f"({surface}, {regime}, {coefficient})")] = (
                value
            )


def _run_ohm_case(
    sample_data_loaded,
    *,
    surface=PAVED,
    five_day_temperature=10.0,
    surface_wetness=0.0,
    soil_moisture_ratio=0.0,
):
    state_initial, forcing_all = sample_data_loaded
    state = state_initial.iloc[[0]].copy()

    fractions = [0.0] * 7
    fractions[surface] = 1.0
    _set_vector(state, "sfr_surf", fractions)
    _set_vector(state, "hdd_id", [five_day_temperature] * 12)
    _set_vector(state, "ohm_threshsw", [10.0] * 8)
    _set_vector(state, "ohm_threshwd", [0.9] * 8)

    wetness = [0.0] * 7
    wetness[surface] = surface_wetness
    _set_vector(state, "state_surf", wetness)

    soil_capacity = [100.0] * 7
    soil_store = [10.0] * 7
    if surface == GRASS:
        soil_store[surface] = soil_moisture_ratio * soil_capacity[surface]
    _set_vector(state, "soilstorecap_surf", soil_capacity)
    _set_vector(state, "soilstore_surf", soil_store)

    state.loc[:, ("storageheatmethod", "0")] = 1
    state.loc[:, ("ohmincqf", "0")] = 0
    _set_ohm_coefficients(
        state,
        surface,
        {
            SUMMER_WET: 30.0,
            SUMMER_DRY: -30.0,
            WINTER_WET: 10.0,
            WINTER_DRY: -10.0,
        },
    )

    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        output, _ = sp.run_supy(
            forcing_all.iloc[:2],
            state,
            logging_level=logging.CRITICAL,
            check_input=False,
            save_state=False,
        )
    return output.SUEWS["QS"].to_numpy()


def test_temperature_threshold_is_continuous(sample_data_loaded):
    """Tiny platform-level temperature differences must not switch regimes."""
    epsilon = 1.0e-10
    below = _run_ohm_case(
        sample_data_loaded, five_day_temperature=10.0 - epsilon
    )
    above = _run_ohm_case(
        sample_data_loaded, five_day_temperature=10.0 + epsilon
    )

    np.testing.assert_allclose(above, below, rtol=0.0, atol=1.0e-7)


def test_temperature_blending_recovers_far_regimes(sample_data_loaded):
    """Temperatures outside the transition zone retain legacy coefficients."""
    winter_below = _run_ohm_case(
        sample_data_loaded, five_day_temperature=7.0
    )
    winter_edge = _run_ohm_case(
        sample_data_loaded, five_day_temperature=8.0
    )
    summer_edge = _run_ohm_case(
        sample_data_loaded, five_day_temperature=12.0
    )
    summer_above = _run_ohm_case(
        sample_data_loaded, five_day_temperature=13.0
    )

    np.testing.assert_allclose(winter_edge, winter_below, rtol=0.0, atol=1.0e-7)
    np.testing.assert_allclose(summer_edge, summer_above, rtol=0.0, atol=1.0e-7)


def test_soil_moisture_threshold_is_continuous(sample_data_loaded):
    """Tiny soil-moisture differences must not switch wet/dry regimes."""
    epsilon = 1.0e-10
    below = _run_ohm_case(
        sample_data_loaded,
        surface=GRASS,
        five_day_temperature=13.0,
        soil_moisture_ratio=0.9 - epsilon,
    )
    above = _run_ohm_case(
        sample_data_loaded,
        surface=GRASS,
        five_day_temperature=13.0,
        soil_moisture_ratio=0.9 + epsilon,
    )

    np.testing.assert_allclose(above, below, rtol=0.0, atol=1.0e-7)


def test_surface_wetness_zero_is_continuous(sample_data_loaded):
    """A trace surface store must not switch directly to wet coefficients."""
    epsilon = 1.0e-10
    dry = _run_ohm_case(
        sample_data_loaded, five_day_temperature=13.0, surface_wetness=0.0
    )
    trace_wetness = _run_ohm_case(
        sample_data_loaded,
        five_day_temperature=13.0,
        surface_wetness=epsilon,
    )

    np.testing.assert_allclose(trace_wetness, dry, rtol=0.0, atol=1.0e-7)
