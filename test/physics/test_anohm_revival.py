"""Regression tests for the revived AnOHM storage-heat scheme (StorageHeatMethod=3).

AnOHM (Sun et al., 2017) was disabled for plumbing reasons (a future-data forcing
dependency, then the removal of its minpack solver). It is revived here with a
rolling trailing-day forcing buffer and minpack-free solvers. These tests guard
that method 3 runs end-to-end and produces a finite, engaged storage-heat flux.

AnOHM remains an internal / not-recommended option (StorageHeatMethod._internal).
"""

import numpy as np
import pandas as pd
import pytest

import supy as sp
from supy.data_model.core.model import StorageHeatMethod

pytestmark = [
    pytest.mark.physics,
    pytest.mark.core,
    pytest.mark.smoke,
]


def _run_storage_method(
    method: int, days: int = 4, start_hour: int = 0
) -> np.ndarray:
    """Run the sample site for a few days with the given StorageHeatMethod."""
    sim = sp.SUEWSSimulation.from_sample_data()
    sim.update_config(
        {"model": {"physics": {"net_radiation": {"value": 3},
                               "storage_heat": {"value": method}}}}
    )
    forcing = sim.forcing
    assert forcing is not None
    index = forcing.df.index
    steps_per_day = int(pd.Timedelta(days=1) / (index[1] - index[0]))
    start_pos = start_hour * steps_per_day // 24
    end_pos = start_pos + days * steps_per_day - 1
    return sim.run(start_date=index[start_pos], end_date=index[end_pos]).df[
        "SUEWS", "QS"
    ].to_numpy()


def test_anohm_runs_and_is_finite():
    """Method 3 runs end-to-end with a fully finite storage-heat flux."""
    qs = _run_storage_method(3)
    assert qs.size > 0
    assert np.isfinite(qs).all(), "AnOHM produced non-finite QS"
    assert not np.any(qs <= -900.0), "AnOHM produced -999 sentinel QS"
    # physically sane bound for a single grid (not an absurd magnitude)
    assert np.nanmax(np.abs(qs)) < 1000.0


def test_anohm_path_is_engaged():
    """Once the trailing-day buffer fills, AnOHM differs from the OHM fallback.

    Guards against the diagnosed coefficients silently collapsing to the OHM
    spin-up branch for the whole run.
    """
    qs_anohm = _run_storage_method(3)
    qs_ohm = _run_storage_method(1)
    # compare a settled day (day 3), after the buffer has rolled over
    steps_per_day = qs_anohm.size // 4
    day3 = slice(steps_per_day * 2, steps_per_day * 3)
    assert np.nanmax(np.abs(qs_anohm[day3] - qs_ohm[day3])) > 1.0, (
        "AnOHM did not diverge from OHM -- the AnOHM path may not be engaged"
    )


def test_anohm_marked_internal():
    """AnOHM stays an internal / not-recommended option (not user-facing)."""
    assert StorageHeatMethod.ANOHM._internal is True


def test_anohm_midday_start_does_not_promote_sparse_buffer():
    """A partial first day does not treat unfilled hour slots as observations."""
    qs = _run_storage_method(3, days=2, start_hour=12)
    assert np.isfinite(qs).all(), "AnOHM promoted a sparse trailing-day buffer"
