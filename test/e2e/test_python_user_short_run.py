"""Scenario: python-user-short-run."""

from helpers import as_dataframe, assert_non_empty_files
import pytest

from supy import SUEWSSimulation

pytestmark = [pytest.mark.api, pytest.mark.e2e]


def test_python_user_short_run_saves_outputs(tmp_path):
    """Scenario: python-user-short-run.

    Persona: urban climate researcher using the Python API.
    Starting condition: bundled sample data are available from the installed package.
    User action: create a sample simulation, run a short deterministic window, save results.
    Expected warning/error: none.
    Expected result: SUEWS output variables, final state, and non-empty saved files.
    """
    sim = SUEWSSimulation.from_sample_data()

    output = sim.run(end_date=sim.forcing.index[23])
    df_output = as_dataframe(output)
    saved_paths = sim.save(tmp_path)

    assert sim.state_final is not None
    assert not sim.state_final.empty
    assert not df_output.empty
    assert "SUEWS" in set(df_output.columns.get_level_values("group"))
    assert {"QH", "QE"} <= set(df_output.columns.get_level_values("var"))
    assert_non_empty_files(saved_paths)
