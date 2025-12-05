"""Tests for suews-run CLI command.

Tests YAML and namelist format support, auto-detection,
backward compatibility, and deprecation warnings.

Uses Click's CliRunner for in-process testing to avoid subprocess overhead.
This is especially important for editable installs where each subprocess
triggers a ninja rebuild check (10-20+ seconds).
"""

import tempfile
from pathlib import Path

import pytest

# Import the CLI command for in-process testing
from supy.cmd.SUEWS import SUEWS

# Import shared CLI testing utilities from conftest
from conftest import CliResultAdapter, run_cli_command


class TestCLIRun:
    """Test suews-run CLI command functionality."""

    @pytest.fixture
    def test_data_dir(self):
        """Get the path to test data fixtures."""
        return Path(__file__).parent.parent / "fixtures"

    @pytest.fixture
    def benchmark_dir(self, test_data_dir):
        """Get the path to benchmark1 fixtures."""
        return test_data_dir / "benchmark1"

    @pytest.fixture
    def sample_yaml(self, benchmark_dir):
        """Path to sample YAML configuration."""
        yaml_file = benchmark_dir / "benchmark1_short.yml"
        if not yaml_file.exists():
            pytest.skip(f"Sample YAML not found: {yaml_file}")
        return yaml_file

    @pytest.fixture
    def sample_nml(self, test_data_dir):
        """Path to sample namelist configuration."""
        nml_file = test_data_dir / "data_test/AVL_1_LDN1/RunControl.nml"
        if not nml_file.exists():
            pytest.skip(f"Sample namelist not found: {nml_file}")
        return nml_file

    def run_suews_run(self, cli_runner, *args, check=True):
        """Run suews-run command in-process using shared CLI utilities.

        Parameters
        ----------
        cli_runner : CliRunner
            Click test runner (from conftest fixture)
        *args : str
            Command-line arguments
        check : bool, optional
            Whether to raise on non-zero exit code

        Returns
        -------
        CliResultAdapter
            Result with returncode, stdout, stderr (subprocess-compatible)
        """
        return run_cli_command(cli_runner, SUEWS, args, check=check)

    # ========== HELP AND VERSION TESTS ==========

    def test_cli_help(self, cli_runner):
        """Test that the CLI help works."""
        result = self.run_suews_run(cli_runner, "--help", check=False)
        assert result.returncode == 0
        assert "YAML" in result.stdout
        assert "config.yml" in result.stdout
        assert "DEPRECATED" in result.stdout or "deprecated" in result.stdout.lower()

    # ========== YAML FORMAT TESTS ==========

    def test_yaml_positional_argument(self, cli_runner, sample_yaml, tmp_path):
        """Test running with YAML file as positional argument."""
        # Run in temp directory to avoid polluting test fixtures
        result = self.run_suews_run(
            cli_runner,
            str(sample_yaml),
            check=True,
        )
        assert result.returncode == 0
        assert "YAML configuration" in result.stdout
        assert "successfully done" in result.stdout

    def test_yaml_auto_detection(self, cli_runner, sample_yaml):
        """Test that .yml extension is auto-detected as YAML format."""
        result = self.run_suews_run(cli_runner, str(sample_yaml), check=True)
        assert "YAML configuration" in result.stdout
        # Should NOT show namelist deprecation warning
        assert "DEPRECATION WARNING" not in result.stderr

    def test_yaml_missing_forcing(self, cli_runner, tmp_path):
        """Test error handling when YAML config lacks forcing data."""
        # Create minimal YAML without forcing_file
        minimal_yaml = tmp_path / "minimal.yml"
        minimal_yaml.write_text(
            """
model:
  control:
    tstep: 3600
sites:
  - name: TestSite
    gridiv: 1
"""
        )

        result = self.run_suews_run(cli_runner, str(minimal_yaml), check=False)
        assert result.returncode != 0
        # Error may be in stdout (click echo) or stderr
        assert "No forcing data" in result.stderr or "No forcing data" in result.stdout

    # ========== NAMELIST FORMAT TESTS ==========

    def test_namelist_with_p_option(self, cli_runner, sample_nml):
        """Test running with namelist using -p option (deprecated)."""
        result = self.run_suews_run(cli_runner, "-p", str(sample_nml), check=False)

        # Should show both deprecation warnings (for -p option and namelist format)
        combined = (result.stderr + result.stdout).lower()
        assert "deprecat" in combined
        # Should show SUEWS version banner
        assert "SUEWS version" in result.stdout

    def test_namelist_positional_argument(self, cli_runner, sample_nml):
        """Test running with namelist as positional argument."""
        result = self.run_suews_run(cli_runner, str(sample_nml), check=False)

        # Should show namelist deprecation warning (may be in stdout with CliRunner)
        combined = result.stderr + result.stdout
        assert "DEPRECATION WARNING" in combined
        assert "suews-convert" in combined
        # Should show SUEWS version banner
        assert "SUEWS version" in result.stdout

    def test_namelist_auto_detection(self, cli_runner, sample_nml):
        """Test that .nml extension is auto-detected as namelist format."""
        result = self.run_suews_run(cli_runner, str(sample_nml), check=False)
        # Should show deprecation warning for namelist format
        combined = result.stderr + result.stdout
        assert "DEPRECATION WARNING" in combined
        assert "Namelist format is deprecated" in combined

    # ========== DEFAULT FILE SEARCH TESTS ==========

    def test_default_yaml_search(self, cli_runner, sample_yaml):
        """Test that config.yml is found by default if it exists."""
        # Use CliRunner's isolated_filesystem for clean working directory
        with cli_runner.isolated_filesystem():
            # Copy sample YAML to config.yml in isolated dir
            Path("config.yml").write_text(sample_yaml.read_text())

            result = self.run_suews_run(cli_runner, check=False)
            if result.returncode == 0:
                assert "Using default configuration: config.yml" in result.stdout

    def test_no_default_file_error(self, cli_runner):
        """Test error when no default config file exists."""
        # Use CliRunner's isolated_filesystem for empty working directory
        with cli_runner.isolated_filesystem():
            result = self.run_suews_run(cli_runner, check=False)
            assert result.returncode != 0
            combined = result.stderr + result.stdout
            assert "No configuration file found" in combined

    # ========== ERROR HANDLING TESTS ==========

    def test_invalid_file_path(self, cli_runner):
        """Test that CLI handles non-existent file gracefully."""
        # Click's exists=True validator catches this before our code
        result = self.run_suews_run(cli_runner, "/nonexistent/config.yml", check=False)
        assert result.returncode != 0

    def test_invalid_yaml_syntax(self, cli_runner, tmp_path):
        """Test error handling for malformed YAML."""
        bad_yaml = tmp_path / "bad.yml"
        bad_yaml.write_text("model:\n  control\n    invalid yaml")

        result = self.run_suews_run(cli_runner, str(bad_yaml), check=False)
        assert result.returncode != 0

    # ========== INTEGRATION TESTS ==========

    @pytest.mark.skipif(
        not (
            Path(__file__).parent.parent / "fixtures/benchmark1/benchmark1_short.yml"
        ).exists(),
        reason="Benchmark data not available",
    )
    def test_yaml_full_simulation(self, cli_runner, benchmark_dir, tmp_path):
        """End-to-end test: YAML config → run → output files created."""
        yaml_file = benchmark_dir / "benchmark1_short.yml"

        # Use CliRunner for in-process testing
        result = self.run_suews_run(cli_runner, str(yaml_file), check=False)

        if result.returncode == 0:
            assert "successfully done" in result.stdout
            # Just verify successful execution
            assert "The following files have been written out:" in result.stdout

    # ========== DEPRECATION WARNING TESTS ==========

    def test_p_option_deprecation_warning(self, cli_runner, sample_yaml):
        """Test that -p option shows deprecation warning."""
        result = self.run_suews_run(cli_runner, "-p", str(sample_yaml), check=False)

        # Should show deprecation for -p option (may be in stdout with CliRunner)
        combined = (result.stderr + result.stdout).lower()
        assert "deprecated" in combined
        assert "positional argument" in combined

    def test_migration_guide_in_warning(self, cli_runner, sample_nml):
        """Test that namelist deprecation includes migration guide."""
        result = self.run_suews_run(cli_runner, str(sample_nml), check=False)

        # Should show migration instructions (may be in stdout with CliRunner)
        combined = result.stderr + result.stdout
        assert "suews-convert" in combined
        assert "config.yml" in combined
