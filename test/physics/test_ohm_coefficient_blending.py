"""Regression tests for smooth OHM coefficient transitions (gh#473)."""

import json
import logging
import subprocess
import sys
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
RESULT_PREFIX = "OHM_CASE_RESULTS="


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


def _run_ohm_case_in_process(
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


def _run_ohm_cases(cases):
    """Run a group of cases without leaking Fortran state to other tests."""
    completed = subprocess.run(
        [sys.executable, __file__, json.dumps(cases)],
        capture_output=True,
        check=False,
        text=True,
        timeout=120,
    )
    if completed.returncode != 0:
        pytest.fail(
            "OHM subprocess failed with exit code "
            f"{completed.returncode}.\nstdout:\n{completed.stdout[-2000:]}"
            f"\nstderr:\n{completed.stderr[-2000:]}"
        )

    result_line = next(
        (
            line
            for line in reversed(completed.stdout.splitlines())
            if line.startswith(RESULT_PREFIX)
        ),
        None,
    )
    if result_line is None:
        pytest.fail(
            "OHM subprocess did not return results.\n"
            f"stdout:\n{completed.stdout[-2000:]}\n"
            f"stderr:\n{completed.stderr[-2000:]}"
        )

    return [
        np.asarray(values, dtype=float)
        for values in json.loads(result_line.removeprefix(RESULT_PREFIX))
    ]


def _subprocess_main():
    cases = json.loads(sys.argv[1])
    sample_data_loaded = sp.load_SampleData()
    results = [
        _run_ohm_case_in_process(sample_data_loaded, **case).tolist()
        for case in cases
    ]
    print(f"{RESULT_PREFIX}{json.dumps(results)}")


def test_temperature_threshold_is_continuous():
    """Tiny platform-level temperature differences must not switch regimes."""
    epsilon = 1.0e-10
    below, above = _run_ohm_cases(
        [
            {"five_day_temperature": 10.0 - epsilon},
            {"five_day_temperature": 10.0 + epsilon},
        ]
    )

    np.testing.assert_allclose(above, below, rtol=0.0, atol=1.0e-7)


def test_temperature_blending_recovers_far_regimes():
    """Temperatures outside the transition zone retain legacy coefficients."""
    winter_below, winter_edge, summer_edge, summer_above = _run_ohm_cases(
        [
            {"five_day_temperature": 7.0},
            {"five_day_temperature": 8.0},
            {"five_day_temperature": 12.0},
            {"five_day_temperature": 13.0},
        ]
    )

    np.testing.assert_allclose(winter_edge, winter_below, rtol=0.0, atol=1.0e-7)
    np.testing.assert_allclose(summer_edge, summer_above, rtol=0.0, atol=1.0e-7)


def test_temperature_midpoint_blends_regimes():
    """The configured threshold must produce an interior coefficient blend."""
    winter, midpoint, summer = _run_ohm_cases(
        [
            {"five_day_temperature": 8.0},
            {"five_day_temperature": 10.0},
            {"five_day_temperature": 12.0},
        ]
    )

    lower = np.minimum(winter, summer)
    upper = np.maximum(winter, summer)
    assert np.all((lower < midpoint) & (midpoint < upper))


def test_soil_moisture_threshold_is_continuous():
    """Tiny soil-moisture differences must not switch wet/dry regimes."""
    epsilon = 1.0e-10
    below, above = _run_ohm_cases(
        [
            {
                "surface": GRASS,
                "five_day_temperature": 13.0,
                "soil_moisture_ratio": 0.9 - epsilon,
            },
            {
                "surface": GRASS,
                "five_day_temperature": 13.0,
                "soil_moisture_ratio": 0.9 + epsilon,
            },
        ]
    )

    np.testing.assert_allclose(above, below, rtol=0.0, atol=1.0e-7)


def test_surface_wetness_zero_is_continuous():
    """A trace surface store must not switch directly to wet coefficients."""
    epsilon = 1.0e-10
    dry, trace_wetness = _run_ohm_cases(
        [
            {"five_day_temperature": 13.0, "surface_wetness": 0.0},
            {"five_day_temperature": 13.0, "surface_wetness": epsilon},
        ]
    )

    np.testing.assert_allclose(trace_wetness, dry, rtol=0.0, atol=1.0e-7)


if __name__ == "__main__":
    _subprocess_main()
