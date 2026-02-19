"""
Test STEBBS vessel water volume limits (GH-340).

Validates that the maximum volume constraint for domestic hot water (DHW) vessels
works correctly, preventing unbounded accumulation when supply > drain flow rates.

The implementation uses a two-layer approach:
1. Preventive (suews_phys_stebbs.f95:1774-1777): Adjusts supply flow rate to match
   drain flow rate when volume is at or above maximum
2. Corrective (suews_phys_stebbs.f95:1955-1958): Safety cap that clamps volume
   if it exceeds maximum after calculation
"""

import os
from pathlib import Path

import numpy as np
import pytest

import supy as sp


# Tolerance justification:
# - DHW volumes typically measured to 0.001 m3 (1 litre) precision
# - Timestep is 300s, flow rates ~0.001 m3/s = 0.3 m3/timestep max change
# - Using 0.01 m3 tolerance accounts for floating-point accumulation over simulation
VOLUME_TOLERANCE_M3 = 0.01
FAIL_FAST_STEPS_ENV = "SUEWS_FAIL_FAST_STEPS"
# Default to full-window validation; set SUEWS_FAIL_FAST_STEPS>0 for fast debugging.
DEFAULT_FAIL_FAST_STEPS = 0


def _load_stebbs_test_config():
    """Load STEBBS test configuration and forcing data."""
    stebbs_test_dir = (
        Path(__file__).parent.parent / "fixtures" / "data_test" / "stebbs_test"
    )
    return stebbs_test_dir / "sample_config.yml"


def _get_fail_fast_steps(default_steps: int = DEFAULT_FAIL_FAST_STEPS) -> int:
    """Return number of timesteps for fail-fast execution."""
    raw = os.environ.get(FAIL_FAST_STEPS_ENV)
    if raw is None or raw == "":
        return default_steps
    try:
        return int(raw)
    except ValueError as exc:
        raise ValueError(
            f"{FAIL_FAST_STEPS_ENV} must be an integer, got: {raw!r}"
        ) from exc


def _select_forcing_window(df_forcing_full):
    """Return one-day forcing truncated to fail-fast timesteps."""
    forcing_day = df_forcing_full.loc["2017-08-26":"2017-08-26"]
    requested_steps = _get_fail_fast_steps()
    if requested_steps <= 0:
        return forcing_day
    steps = min(requested_steps, len(forcing_day))
    return forcing_day.iloc[:steps]


@pytest.mark.core
@pytest.mark.slow
def test_maximum_volume_capping():
    """
    Test that vessel water volume is capped at MaximumVolumeOfDHWinUse.

    This test validates the GH-340 fix by setting a low maximum volume
    threshold and verifying Vwater_vessel never exceeds it.

    The fixture has initial DHWWaterVolume=15.0 m3, so setting max=2.0
    tests that the safety cap immediately constrains the volume.
    """
    # ARRANGE
    config_path = _load_stebbs_test_config()
    df_state_init = sp.init_supy(str(config_path))

    # Set a low maximum volume to test capping behaviour
    # Initial volume is 15.0 m3 in fixture, so this tests immediate capping
    max_volume = 2.0
    df_state_init.loc[:, ("maximumvolumeofdhwinuse", "0")] = max_volume

    df_forcing_full = sp.load_forcing_grid(
        str(config_path), df_state_init.index[0], df_state_init=df_state_init
    )
    df_forcing = _select_forcing_window(df_forcing_full)

    # ACT
    df_output, df_state = sp.run_supy(df_forcing, df_state_init)

    # ASSERT
    vwater_vessel = df_output.STEBBS["Vwater_vessel"]

    # Verify output is valid (not all NaN)
    assert not vwater_vessel.isna().all(), (
        "Vwater_vessel is all NaN - STEBBS may not be running correctly"
    )

    # Verify volume never exceeds maximum
    max_observed = vwater_vessel.max()
    assert max_observed <= max_volume + VOLUME_TOLERANCE_M3, (
        f"Vessel water volume ({max_observed:.4f} m3) exceeded maximum "
        f"({max_volume} m3). GH-340 fix not working correctly."
    )


@pytest.mark.core
@pytest.mark.slow
def test_volume_accumulation_with_supply_greater_than_drain():
    """
    Test volume capping when supply flow rate exceeds drain flow rate.

    This test verifies the preventive mechanism (flow rate adjustment)
    works alongside the corrective cap when there is net water accumulation.
    Sets non-zero flow rates with supply > drain to trigger accumulation.
    """
    # ARRANGE
    config_path = _load_stebbs_test_config()
    df_state_init = sp.init_supy(str(config_path))

    # Set a moderate maximum volume
    max_volume = 5.0
    df_state_init.loc[:, ("maximumvolumeofdhwinuse", "0")] = max_volume

    # Set initial volume below max to allow accumulation testing
    initial_volume = 4.0
    df_state_init.loc[:, ("dhwwatervolume", "0")] = initial_volume

    # Set non-zero flow rates where supply > drain (causes accumulation)
    # These are in m3/s, 300s timestep means 0.001 m3/s = 0.3 m3/timestep
    supply_rate = 0.001  # m3/s
    drain_rate = 0.0005  # m3/s (half of supply)
    df_state_init.loc[:, ("hotwaterflowrate", "0")] = supply_rate
    df_state_init.loc[:, ("dhwdrainflowrate", "0")] = drain_rate

    df_forcing_full = sp.load_forcing_grid(
        str(config_path), df_state_init.index[0], df_state_init=df_state_init
    )
    df_forcing = _select_forcing_window(df_forcing_full)

    # ACT
    df_output, df_state = sp.run_supy(df_forcing, df_state_init)

    # ASSERT
    vwater_vessel = df_output.STEBBS["Vwater_vessel"]

    assert not vwater_vessel.isna().all(), (
        "Vwater_vessel is all NaN - STEBBS may not be running correctly"
    )

    # Volume should never exceed max despite positive net accumulation rate
    max_observed = vwater_vessel.max()
    assert max_observed <= max_volume + VOLUME_TOLERANCE_M3, (
        f"Vessel water volume ({max_observed:.4f} m3) exceeded maximum "
        f"({max_volume} m3) despite supply > drain flow rates."
    )

    # Verify volume reached/approached maximum (accumulation was attempted)
    # This confirms the capping was actually triggered, not just avoided
    #assert max_observed >= max_volume - 0.5, (
    #    f"Volume ({max_observed:.4f} m3) never approached maximum ({max_volume} m3). "
    #    "Test may not be exercising the capping mechanism."
    #YL: currently STEBBS assume water supply==drain, so water volume will keep unchanged, above code may be recovered when we consider the difference between water supply and drain
    assert vwater_vessel.nunique() == 1, (
        "Vessel water volume values are not constant, supply!=drain. "
        f"Observed values: {vwater_vessel.unique()}"
    )

@pytest.mark.core
@pytest.mark.slow
def test_maximum_volume_disabled_when_zero():
    """
    Test that maximum volume constraint is disabled when set to 0.0.

    Per the implementation, MaximumVolumeOfDHWinUse = 0.0 means
    the upper limit is disabled (condition `maxVwater_vessel > 0.0D0`).
    """
    # ARRANGE
    config_path = _load_stebbs_test_config()
    df_state_init = sp.init_supy(str(config_path))

    # Set maximum volume to 0.0 to disable the cap
    df_state_init.loc[:, ("maximumvolumeofdhwinuse", "0")] = 0.0

    # Keep initial volume at fixture default (15.0 m3)
    # With cap disabled, this should remain unchanged

    df_forcing_full = sp.load_forcing_grid(
        str(config_path), df_state_init.index[0], df_state_init=df_state_init
    )
    df_forcing = _select_forcing_window(df_forcing_full)

    # ACT
    df_output, df_state = sp.run_supy(df_forcing, df_state_init)

    # ASSERT
    vwater_vessel = df_output.STEBBS["Vwater_vessel"]

    assert not vwater_vessel.isna().all(), (
        "Vwater_vessel is all NaN when max volume disabled"
    )

    # With cap disabled and zero flow rates, volume should stay at initial
    # (fixture has HotWaterFlowRate=0.0 and DHWDrainFlowRate=0.0)
    initial_vol = df_state_init.loc[:, ("dhwwatervolume", "0")].iloc[0]
    mean_vol = vwater_vessel.mean()

    # Volume should be approximately unchanged (no capping occurred)
    assert abs(mean_vol - initial_vol) < 1.0, (
        f"Volume changed unexpectedly when cap disabled. "
        f"Initial: {initial_vol}, Mean: {mean_vol:.4f}"
    )


@pytest.mark.core
@pytest.mark.slow
def test_minimum_volume_still_enforced():
    """
    Test that minimum volume constraint still works alongside maximum.

    Ensures the new maximum volume logic does not break the existing
    minimum volume constraint (MinimumVolumeOfDHWinUse).
    """
    # ARRANGE
    config_path = _load_stebbs_test_config()
    df_state_init = sp.init_supy(str(config_path))

    # Get minimum volume from config (typically 1.0 m3 in test fixtures)
    min_volume = df_state_init.loc[:, ("minimumvolumeofdhwinuse", "0")].iloc[0]

    # Set a reasonable maximum that will not interfere
    df_state_init.loc[:, ("maximumvolumeofdhwinuse", "0")] = 50.0

    # Set initial volume near minimum to test constraint
    df_state_init.loc[:, ("dhwwatervolume", "0")] = min_volume + 0.5

    # Set drain > supply to cause volume decrease
    df_state_init.loc[:, ("hotwaterflowrate", "0")] = 0.0005
    df_state_init.loc[:, ("dhwdrainflowrate", "0")] = 0.001

    df_forcing_full = sp.load_forcing_grid(
        str(config_path), df_state_init.index[0], df_state_init=df_state_init
    )
    df_forcing = _select_forcing_window(df_forcing_full)

    # ACT
    df_output, df_state = sp.run_supy(df_forcing, df_state_init)

    # ASSERT
    vwater_vessel = df_output.STEBBS["Vwater_vessel"]

    assert not vwater_vessel.isna().all(), (
        "Vwater_vessel is all NaN - minimum constraint test failed"
    )

    # Verify volume never goes below minimum
    min_observed = vwater_vessel.min()
    assert min_observed >= min_volume - VOLUME_TOLERANCE_M3, (
        f"Vessel water volume ({min_observed:.4f} m3) went below minimum "
        f"({min_volume} m3). Minimum constraint may be broken."
    )


@pytest.mark.core
@pytest.mark.slow
def test_volume_stability_at_maximum():
    """
    Test that volume remains stable when starting at maximum.

    When volume equals max and supply > drain, the preventive mechanism
    should equalise flow rates, keeping volume stable at maximum.
    This tests the preventive layer (not just the corrective cap).
    """
    # ARRANGE
    config_path = _load_stebbs_test_config()
    df_state_init = sp.init_supy(str(config_path))

    # Set max volume and initialise at exactly max
    max_volume = 10.0
    df_state_init.loc[:, ("maximumvolumeofdhwinuse", "0")] = max_volume
    df_state_init.loc[:, ("dhwwatervolume", "0")] = max_volume

    # Set supply > drain (would cause accumulation without preventive mechanism)
    df_state_init.loc[:, ("hotwaterflowrate", "0")] = 0.002
    df_state_init.loc[:, ("dhwdrainflowrate", "0")] = 0.001

    df_forcing_full = sp.load_forcing_grid(
        str(config_path), df_state_init.index[0], df_state_init=df_state_init
    )
    df_forcing = _select_forcing_window(df_forcing_full)

    # ACT
    df_output, df_state = sp.run_supy(df_forcing, df_state_init)

    # ASSERT
    vwater_vessel = df_output.STEBBS["Vwater_vessel"]

    assert not vwater_vessel.isna().all(), (
        "Vwater_vessel is all NaN - stability test failed"
    )

    # Volume should remain at maximum (preventive mechanism equalises flows)
    max_observed = vwater_vessel.max()
    min_observed = vwater_vessel.min()

    assert max_observed <= max_volume + VOLUME_TOLERANCE_M3, (
        f"Volume ({max_observed:.4f} m3) exceeded maximum ({max_volume} m3)"
    )

    # Volume should be stable (not oscillating significantly)
    volume_range = max_observed - min_observed
    assert volume_range < 0.5, (
        f"Volume unstable at maximum. Range: {volume_range:.4f} m3 "
        f"(min={min_observed:.4f}, max={max_observed:.4f}). "
        "Preventive mechanism may not be working correctly."
    )


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
