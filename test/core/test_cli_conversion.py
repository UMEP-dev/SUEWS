"""Streamlined SUEWS CLI conversion tests.

Focuses on end-to-end testing that verifies converted configs can actually run,
reducing redundancy by replacing multiple low-level tests with comprehensive
simulation tests.

Uses Click's CliRunner for in-process testing to avoid subprocess overhead.
This is especially important for editable installs where each subprocess
triggers a ninja rebuild check (10-20+ seconds).
"""

import subprocess
import sys
import tempfile
from pathlib import Path
import pytest
from click.testing import CliRunner

# Import for simulation testing and CLI
try:
    from supy import SUEWSSimulation
    from supy.util.converter import detect_table_version
    from supy.cmd.table_converter import convert_table_cmd

    SUPY_AVAILABLE = True
except ImportError:
    SUPY_AVAILABLE = False
    convert_table_cmd = None


class _CliResult:
    """Adapter to make CliRunner results look like subprocess.CompletedProcess."""

    def __init__(self, click_result):
        self._result = click_result
        self.returncode = click_result.exit_code
        self.stdout = click_result.output or ""
        self.stderr = ""
        # Extract error messages if present
        if click_result.output and "Error" in click_result.output:
            self.stderr = click_result.output


class TestCLIConversion:
    """Streamlined CLI conversion tests - end-to-end focus."""

    @pytest.fixture
    def runner(self):
        """Create a Click CliRunner for in-process testing."""
        return CliRunner()

    @pytest.fixture
    def test_data_dir(self):
        """Get the path to test data fixtures."""
        return Path(__file__).parent.parent / "fixtures/data_test"

    @pytest.fixture
    def legacy_format_dir(self):
        """Get the path to legacy format fixtures."""
        return Path(__file__).parent.parent / "fixtures/legacy_format"

    def run_suews_convert(self, runner, *args):
        """Run suews-convert command in-process using CliRunner.

        Parameters
        ----------
        runner : CliRunner
            Click test runner
        *args : str
            Command-line arguments

        Returns
        -------
        _CliResult
            Result with returncode, stdout, stderr (subprocess-compatible)
        """
        if convert_table_cmd is None:
            pytest.skip("supy.cmd.table_converter not available")
        click_result = runner.invoke(convert_table_cmd, args, catch_exceptions=False)
        return _CliResult(click_result)

    @staticmethod
    def run_suews_convert_subprocess(*args):
        """Run suews-convert as subprocess (fallback for tests needing isolation)."""
        result = subprocess.run(
            [sys.executable, "-m", "supy.cmd.table_converter", *args],
            capture_output=True,
            text=True,
            timeout=60,
        )
        return result

    # ========== BASIC CLI INTERFACE TESTS ==========

    def test_cli_help(self, runner):
        """Test that the CLI help works."""
        result = self.run_suews_convert(runner, "--help")
        assert result.returncode == 0
        assert "Convert SUEWS tables" in result.stdout or "Usage:" in result.stdout

    def test_invalid_input_via_cli(self, runner):
        """Test that CLI handles invalid input gracefully."""
        with tempfile.TemporaryDirectory() as tmpdir:
            # Click validates path exists, so this will fail at Click level
            click_result = runner.invoke(
                convert_table_cmd,
                ["-i", "/nonexistent/file.nml", "-o", str(Path(tmpdir) / "output.yml")],
            )
            assert click_result.exit_code != 0
            combined = click_result.output or ""
            assert (
                "Error" in combined
                or "not found" in combined.lower()
                or "does not exist" in combined.lower()
            )

    # ========== END-TO-END SIMULATION TESTS ==========
    # These replace redundant conversion/validation tests

    @pytest.mark.skipif(not SUPY_AVAILABLE, reason="SuPy not available")
    @pytest.mark.skipif(
        not (Path(__file__).parent.parent / "fixtures/data_test/AVL_1_LDN1").exists(),
        reason="Single-layer test data not available",
    )
    def test_single_layer_end_to_end(self, runner, test_data_dir):
        """End-to-end test: nlayer=1 conversion → load → simulate.

        Replaces:
        - test_single_layer_conversion_via_cli (conversion check)
        - test_single_layer_yaml_validation (validation check)

        By actually running a simulation, we verify:
        - CLI conversion works
        - YAML structure is valid
        - Roofs/walls arrays populated correctly
        - Config can be loaded
        - Simulation executes successfully

        Tests fix for issues #707, #708.
        """
        input_file = test_data_dir / "AVL_1_LDN1/RunControl.nml"
        forcing_file = (
            test_data_dir.parent / "benchmark1/forcing/Kc1_2011_data_5_tiny.txt"
        )

        with tempfile.TemporaryDirectory() as tmpdir:
            output_file = Path(tmpdir) / "single_layer.yml"

            # Convert NML → YAML (in-process)
            result = self.run_suews_convert(
                runner, "-i", str(input_file), "-o", str(output_file)
            )
            assert result.returncode == 0, f"Conversion failed: {result.stderr}"
            assert output_file.exists()

            # Load and validate
            sim = SUEWSSimulation(str(output_file))
            nlayer = sim.config.sites[0].properties.vertical_layers.nlayer.value
            assert nlayer == 1

            # Verify roofs/walls (core bug fix)
            roofs = sim.config.sites[0].initial_states.roofs
            walls = sim.config.sites[0].initial_states.walls
            assert len(roofs) == 1, "Bug #707: Empty roofs array!"
            assert len(walls) == 1, "Bug #707: Empty walls array!"

            # Run simulation - the ultimate test
            sim.update_forcing(str(forcing_file))
            results = sim.run()

            assert results is not None
            assert len(results) > 0
            output_vars = results.columns.get_level_values("var")
            assert "QH" in output_vars
            assert "QE" in output_vars

    @pytest.mark.skipif(not SUPY_AVAILABLE, reason="SuPy not available")
    @pytest.mark.skipif(
        not (Path(__file__).parent.parent / "fixtures/data_test/AVL_6_310").exists(),
        reason="Multi-layer test data not available",
    )
    def test_multi_layer_end_to_end(self, runner, test_data_dir):
        """End-to-end test: nlayer=7 conversion → load → simulate.

        Replaces:
        - test_multi_layer_conversion_via_cli (conversion check)
        - test_multi_layer_yaml_validation (validation check)
        - test_version_detection_via_cli (auto-detection covered)

        By actually running a simulation, we verify:
        - CLI conversion works
        - Version auto-detection works
        - YAML structure is valid
        - Roofs/walls arrays match nlayer count
        - Config can be loaded
        - Simulation executes successfully

        Tests fix for issues #706, #708.
        """
        input_file = test_data_dir / "AVL_6_310/RunControl.nml"
        forcing_file = (
            test_data_dir.parent / "benchmark1/forcing/Kc1_2011_data_5_tiny.txt"
        )

        with tempfile.TemporaryDirectory() as tmpdir:
            output_file = Path(tmpdir) / "multi_layer.yml"

            # Convert NML → YAML (in-process, auto-detects version)
            result = self.run_suews_convert(
                runner, "-i", str(input_file), "-o", str(output_file)
            )
            assert result.returncode == 0, f"Conversion failed: {result.stderr}"
            assert output_file.exists()

            # Load and validate
            sim = SUEWSSimulation(str(output_file))
            nlayer = sim.config.sites[0].properties.vertical_layers.nlayer.value
            assert nlayer > 1

            # Verify roofs/walls match nlayer (core bug fix)
            roofs = sim.config.sites[0].initial_states.roofs
            walls = sim.config.sites[0].initial_states.walls
            assert len(roofs) == nlayer, (
                f"Bug #708: Expected {nlayer} roofs, got {len(roofs)}"
            )
            assert len(walls) == nlayer, (
                f"Bug #708: Expected {nlayer} walls, got {len(walls)}"
            )

            # Run simulation - the ultimate test
            sim.update_forcing(str(forcing_file))
            results = sim.run()

            assert results is not None
            assert len(results) > 0
            output_vars = results.columns.get_level_values("var")
            assert "QH" in output_vars
            assert "QE" in output_vars

    # ========== LEGACY FORMAT MIGRATION TESTS ==========

    @pytest.mark.skipif(not SUPY_AVAILABLE, reason="SuPy not available")
    @pytest.mark.parametrize("version", ["2020a", "2019a", "2018a"])
    def test_legacy_version_auto_detection(self, runner, version, legacy_format_dir):
        """Test auto-detection of legacy table versions."""
        test_dir = legacy_format_dir / version
        if not test_dir.exists():
            pytest.skip(f"Test data for {version} not available")

        with tempfile.TemporaryDirectory() as tmpdir:
            output_file = Path(tmpdir) / f"{version}_detected.yml"

            # Find RunControl.nml
            nml_files = list(test_dir.glob("**/RunControl.nml"))
            if not nml_files:
                pytest.skip(f"No RunControl.nml found for {version}")

            result = self.run_suews_convert(
                runner, "-i", str(nml_files[0]), "-o", str(output_file)
            )
            assert result.returncode == 0
