"""Regression tests for the STEBBS hot-water vessel energy balance."""

from importlib import import_module
from pathlib import Path
import warnings

import pytest

import supy as sp

pytestmark = [pytest.mark.physics, pytest.mark.core]

STEBBS_CONFIG = (
    Path(__file__).parents[1]
    / "fixtures"
    / "data_test"
    / "stebbs_test"
    / "sample_config.yml"
)

FLOW_RATE = 1.0e-5  # hot water drawn through the vessel [m3 s-1]


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


def _run_vessel_probe(volume):
    """Run one-second STEBBS updates with hot water flowing through the vessel.

    With a one-second timestep each output row is a single STEBBS substep, so
    each row can be checked against the vessel update applied in that substep.
    """
    simulation = sp.SUEWSSimulation(STEBBS_CONFIG)
    simulation.config.model.control.tstep = 1
    properties = simulation.config.sites[0].properties
    stebbs = properties.stebbs
    stebbs.volume_hot_water.value = volume
    stebbs.rate_flow_hot_water.value = FLOW_RATE
    profile = stebbs.profile_flow_hot_water
    for day in (profile.working_day, profile.holiday):
        for slot in day:
            day[slot] = 1.0
    simulation._df_state_init = simulation.config.to_df_state()

    forcing = simulation.forcing.df.loc["2017-08-26"].iloc[:8].copy()
    simulation._df_forcing = forcing
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        output = simulation.run(
            start_date=forcing.index[0],
            end_date=forcing.index[-1],
            n_jobs=1,
            _validate_forcing=False,
        )
    return output.df.STEBBS, properties


@pytest.mark.skipif(
    not _rust_library_available(),
    reason="Rust library backend not available (install src/suews_bridge with physics feature)",
)
@pytest.mark.parametrize("volume", [0.3, 1.5])
def test_vessel_temperature_change_scales_inversely_with_volume(volume):
    """The vessel water warms by Q dt / (rho c V), whatever the vessel volume.

    In each substep STEBBS first applies the convective exchange with the vessel
    wall to the water, then mixes in water from the tank at the draw rate. The
    test undoes the mixing step to recover the temperature change due to the
    wall exchange alone and compares it with the lumped-capacitance result.
    Multiplying by the volume instead of dividing by it would be off by a
    factor of volume squared (0.09 and 2.25 for the two volumes tested).
    """
    output, properties = _run_vessel_probe(volume)
    stebbs = properties.stebbs
    footprint = properties.building_archetype.area_footprint.value
    heat_capacity = (
        stebbs.density_hot_water.value
        * stebbs.specific_heat_capacity_hot_water.value
        * volume
    )
    drawn = FLOW_RATE * 1.0  # volume replaced in a one-second substep [m3]

    assert output["Vwater_vessel"].to_numpy() == pytest.approx(volume)
    for previous_index in range(1, len(output) - 1):
        previous = output.iloc[previous_index]
        current = output.iloc[previous_index + 1]

        # Undo the mixing: the drawn volume is replaced with tank water at the
        # tank temperature from the end of the previous substep.
        before_mixing = (
            volume * current["Twater_vessel"] - drawn * previous["Twater_tank"]
        ) / (volume - drawn)
        wall_exchange_change = before_mixing - previous["Twater_vessel"]

        heat_to_water = -current["QHconv_intvesselwall_FA"] * footprint
        expected_change = heat_to_water * 1.0 / heat_capacity

        # The change is small (1e-8 to 1e-7 K in a second), but the mixing is
        # undone in double precision to about 1e-13 K, so rel=1e-3 is ample
        # while a volume-squared error would be off by a factor of 0.09 or 2.25.
        assert abs(expected_change) > 1.0e-10
        assert wall_exchange_change == pytest.approx(expected_change, rel=1.0e-3)
