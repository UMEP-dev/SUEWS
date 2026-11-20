"""Exercise explicit drainage coefficients on the original sample option 3."""

import numpy as np
import pandas as pd
import pytest

from supy import SUEWSSimulation

pytestmark = pytest.mark.physics


@pytest.fixture(scope="module")
def drainage_runs():
    outputs = {}
    for variant in ("baseline", "zero_coef1", "zero_coef2", "absent_evetr"):
        sim = SUEWSSimulation.from_sample_data()
        land = sim.config.sites[0].properties.land_cover
        assert land.paved.storage_drain_params.drain_eq.value == 3
        if variant == "zero_coef1":
            land.paved.storage_drain_params.drain_coef_1.value = 0.0
        elif variant == "zero_coef2":
            land.paved.storage_drain_params.drain_coef_2.value = 0.0
        elif variant == "absent_evetr":
            assert land.evetr.sfr.value == 0.0
            land.evetr.storage_drain_params.drain_coef_1 = None
            land.evetr.storage_drain_params.drain_coef_2 = None
        forcing = sim.forcing.df.iloc[:576].copy()
        assert forcing["rain"].sum() > 0.0
        sim.update_forcing(forcing)
        output = sim.run(end_date=forcing.index[-1])
        outputs[variant] = output.xs("SUEWS", level="group", axis=1).xs(1, level="grid")
    return outputs


@pytest.mark.parametrize("variant", ["zero_coef1", "zero_coef2"])
def test_explicit_zero_has_observable_runoff_effect(drainage_runs, variant):
    baseline = drainage_runs["baseline"]
    changed = drainage_runs[variant]
    assert baseline.index.equals(changed.index)
    for frame in (baseline, changed):
        assert np.isfinite(frame[["RO", "QE"]].to_numpy()).all()
    # A same-build comparison needs no cross-platform reference tolerance.
    # This lower bound is far below the observed runoff response and above
    # floating-point noise; it tests that the original option is exercised.
    assert abs(changed["RO"].sum() - baseline["RO"].sum()) > 0.01


def test_absent_evergreen_coefficients_are_output_neutral(drainage_runs):
    pd.testing.assert_frame_equal(
        drainage_runs["baseline"], drainage_runs["absent_evetr"], check_exact=True
    )
