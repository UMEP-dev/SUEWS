"""Regression tests for STEBBS physics parameter wiring."""

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


def _run_one_second_stebbs_probe():
    """Run a short STEBBS case with one internal update per forcing row."""
    simulation = sp.SUEWSSimulation(STEBBS_CONFIG)
    simulation.config.model.control.tstep = 1
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
    return output.df.STEBBS


@pytest.mark.skipif(
    not _rust_library_available(),
    reason="Rust library backend not available (install src/suews_bridge with physics feature)",
)
def test_roof_convection_uses_roof_temperature_and_orientation():
    """The roof flux must use the roof, rather than window, TARP coefficient."""
    output = _run_one_second_stebbs_probe()
    previous = output.iloc[-2]
    current = output.iloc[-1]

    roof_surface_to_air = previous["Tintroof"] - previous["Tair_ind"]
    assert roof_surface_to_air < 0.0

    # TARP's stable downward-facing correlation for an internal roof surface.
    roof_coefficient = (
        1.810 * abs(roof_surface_to_air) ** (1.0 / 3.0) / 1.382
    )
    expected_flux = roof_coefficient * -roof_surface_to_air

    assert current["QHconv_introof_FA"] == pytest.approx(
        expected_flux,
        rel=2.0e-6,
        abs=1.0e-10,
    )
