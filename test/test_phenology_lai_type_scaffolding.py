"""GH-1292 PR1 end-to-end scaffolding contract.

The bundled sample config has ``laitype: 1`` for every vegetation surface
(see ``src/supy/sample_data/sample_config.yml``), so "unchanged" is not
an ``LAIType = 0`` baseline. This test explicitly mutates ``laitype`` on
a clone of the sample state and asserts that the ``LAIType = 2`` branch
(PR1 no-op) is bit-identical to ``LAIType = 0`` on the full year run.
PR2 will replace the no-op with Design C numerics -- when that happens
this test is expected to break, at which point the assertion migrates
to a site-scale scientific acceptance test driven by the FLUXNET2015
archive (see ``scripts/verify/moisture_phenology_site.py``).
"""

from __future__ import annotations

import numpy as np
import pandas as pd
import pytest

from supy import SUEWSSimulation

LAI_TYPE_COLS = [
    ("laitype", "(0,)"),
    ("laitype", "(1,)"),
    ("laitype", "(2,)"),
]


def _run_with_laitype(value: int) -> pd.DataFrame:
    """Return the simulated LAI DataFrame for the sample run with all three veg surfaces set to ``value``."""

    sim = SUEWSSimulation.from_sample_data()
    state_init = sim.state_init.copy()
    for col in LAI_TYPE_COLS:
        state_init.loc[:, col] = value

    scenario = SUEWSSimulation.from_state(state_init)
    scenario.update_forcing(sim.forcing)
    output = scenario.run()
    return output.SUEWS[["LAI"]].copy()


@pytest.fixture(scope="module")
def lai_baseline_0() -> pd.DataFrame:
    return _run_with_laitype(0)


@pytest.fixture(scope="module")
def lai_variant_2() -> pd.DataFrame:
    return _run_with_laitype(2)


def test_laitype_2_matches_laitype_0_bit_identically(
    lai_baseline_0: pd.DataFrame, lai_variant_2: pd.DataFrame
) -> None:
    """PR1 scaffolding contract: LAIType=2 must reproduce LAIType=0 outputs exactly.

    Once PR2 enables the Jarvis factor + CLM5 trigger this test is
    expected to fail, at which point it should be ported to the
    site-scale harness (scripts/verify/moisture_phenology_site.py) and
    replaced with a scientific acceptance test against MODIS LAI.
    """

    assert lai_baseline_0.shape == lai_variant_2.shape
    np.testing.assert_array_equal(lai_baseline_0.values, lai_variant_2.values)


def test_sample_run_laitype_1_completes_without_nan() -> None:
    """Legacy laitype=1 path must still run to completion without NaNs."""

    sim = SUEWSSimulation.from_sample_data()
    output = sim.run()
    lai = output.SUEWS[["LAI"]]
    assert len(lai) > 0
    assert not lai.isna().any().any()
