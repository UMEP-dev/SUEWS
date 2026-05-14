"""Scenario: legacy-table-migration-run."""

from click.testing import CliRunner
import pytest

from supy import SUEWSSimulation
from supy.cmd.table_converter import convert_table_cmd

from helpers import FIXTURES, as_dataframe


pytestmark = [pytest.mark.api, pytest.mark.e2e]


def test_legacy_table_conversion_output_can_run(tmp_path):
    """Scenario: legacy-table-migration-run.

    Persona: user migrating an old namelist table setup.
    Starting condition: legacy `RunControl.nml` tables are available.
    User action: convert to YAML, load the YAML, attach tiny forcing, run SUEWS.
    Expected warning/error: none.
    Expected result: converted YAML parses and produces QH/QE output variables.
    """
    input_file = FIXTURES / "data_test" / "AVL_1_LDN1" / "RunControl.nml"
    forcing_file = FIXTURES / "benchmark1" / "forcing" / "Kc1_2011_data_5_tiny.txt"
    output_file = tmp_path / "converted.yml"

    result = CliRunner().invoke(
        convert_table_cmd,
        ["--input", str(input_file), "--output", str(output_file)],
        catch_exceptions=False,
    )

    assert result.exit_code == 0, result.output
    assert output_file.exists()

    sim = SUEWSSimulation(str(output_file))
    sim.update_forcing(str(forcing_file))
    df_output = as_dataframe(sim.run())

    assert not df_output.empty
    assert {"QH", "QE"} <= set(df_output.columns.get_level_values("var"))
