"""Tests for `SUEWSSimulation.run_dts()` output parity with `run()`.

Acceptance criteria:
- `run_dts()` produces identical outputs to `run()`.
- `run_dts()` exposes the final DTS state via `output.dts_state`.
"""

from pathlib import Path

import pandas as pd
import pytest

from supy import SUEWSSimulation
from supy.suews_output import SUEWSOutput


@pytest.fixture
def sample_config_path() -> Path:
    return Path(__file__).parent.parent.parent / "src" / "supy" / "sample_data" / "sample_config.yml"


@pytest.fixture
def sample_forcing_path() -> Path:
    return Path(__file__).parent.parent.parent / "src" / "supy" / "sample_data" / "Kc_2012_data_60.txt"


@pytest.mark.core
def test_run_dts_produces_suewsoutput(sample_config_path: Path, sample_forcing_path: Path):
    sim = SUEWSSimulation(str(sample_config_path))
    sim.update_forcing(str(sample_forcing_path))
    out = sim.run_dts(start_date="2012-01-01 00:05", end_date="2012-01-01 06:00")
    assert isinstance(out, SUEWSOutput)
    assert not out.df.empty
    assert "SUEWS" in out.groups


@pytest.mark.core
def test_run_dts_matches_run_exactly(sample_config_path: Path, sample_forcing_path: Path):
    sim_dts = SUEWSSimulation(str(sample_config_path))
    sim_dts.update_forcing(str(sample_forcing_path))
    out_dts = sim_dts.run_dts(start_date="2012-01-01 00:05", end_date="2012-01-01 06:00")

    sim_legacy = SUEWSSimulation(str(sample_config_path))
    sim_legacy.update_forcing(str(sample_forcing_path))
    out_legacy = sim_legacy.run(start_date="2012-01-01 00:05", end_date="2012-01-01 06:00")

    pd.testing.assert_frame_equal(out_dts.df, out_legacy.df)
    pd.testing.assert_frame_equal(out_dts.state_final, out_legacy.state_final)


@pytest.mark.core
def test_run_dts_exposes_final_dts_state(sample_config_path: Path, sample_forcing_path: Path):
    sim = SUEWSSimulation(str(sample_config_path))
    sim.update_forcing(str(sample_forcing_path))
    out = sim.run_dts(start_date="2012-01-01 00:05", end_date="2012-01-01 06:00")

    assert out.dts_state is not None
    assert isinstance(out.dts_state, dict)
    assert len(out.dts_state) >= 1

    # If this is a single-grid run, `get_dts_state()` should succeed without a selector.
    if len(out.dts_state) == 1:
        state = out.get_dts_state()
        assert state is not None

