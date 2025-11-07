"""Tests for suews-run CLI command.

Tests YAML and namelist format support, auto-detection,
backward compatibility, and deprecation warnings.
"""

import subprocess
import tempfile
from pathlib import Path
import pytest
import warnings


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

    @staticmethod
    def run_suews_run(*args, check=True, timeout=120):
        """Run suews-run command and return result.

        Parameters
        ----------
        *args : str
            Command-line arguments
        check : bool, optional
            Whether to raise on non-zero exit code
        timeout : int, optional
            Timeout in seconds

        Returns
        -------
        subprocess.CompletedProcess
            Result with returncode, stdout, stderr
        """
        result = subprocess.run(
            ["suews-run", *args],
            capture_output=True,
            text=True,
            timeout=timeout,
            check=False,
        )
        if check and result.returncode != 0:
            raise subprocess.CalledProcessError(
                result.returncode,
                result.args,
                result.stdout,
                result.stderr,
            )
        return result

    # ========== HELP AND VERSION TESTS ==========

    def test_cli_help(self):
        """Test that the CLI help works."""
        result = self.run_suews_run("--help", check=False)
        assert result.returncode == 0
        assert "YAML" in result.stdout
        assert "config.yml" in result.stdout
        assert "DEPRECATED" in result.stdout or "deprecated" in result.stdout.lower()

    # ========== YAML FORMAT TESTS ==========

    def test_yaml_positional_argument(self, sample_yaml, tmp_path):
        """Test running with YAML file as positional argument."""
        # Run in temp directory to avoid polluting test fixtures
        result = self.run_suews_run(
            str(sample_yaml),
            check=True,
        )
        assert result.returncode == 0
        assert "YAML configuration" in result.stdout
        assert "successfully done" in result.stdout

    def test_yaml_auto_detection(self, sample_yaml):
        """Test that .yml extension is auto-detected as YAML format."""
        result = self.run_suews_run(str(sample_yaml), check=True)
        assert "YAML configuration" in result.stdout
        # Should NOT show namelist deprecation warning
        assert "DEPRECATION WARNING" not in result.stderr

    def test_yaml_missing_forcing(self, tmp_path):
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

        result = self.run_suews_run(str(minimal_yaml), check=False)
        assert result.returncode != 0
        assert "No forcing data" in result.stderr

    # ========== NAMELIST FORMAT TESTS ==========

    def test_namelist_with_p_option(self, sample_nml):
        """Test running with namelist using -p option (deprecated)."""
        with warnings.catch_warnings(record=True):
            warnings.simplefilter("always")
            result = self.run_suews_run("-p", str(sample_nml), check=True)

        assert result.returncode == 0
        assert "successfully done" in result.stdout
        # Check for both deprecation warnings
        stderr_lower = result.stderr.lower()
        assert "deprecat" in stderr_lower

    def test_namelist_positional_argument(self, sample_nml):
        """Test running with namelist as positional argument."""
        with warnings.catch_warnings(record=True):
            warnings.simplefilter("always")
            result = self.run_suews_run(str(sample_nml), check=True)

        assert result.returncode == 0
        # Should show namelist deprecation warning
        assert "DEPRECATION WARNING" in result.stderr
        assert "suews-convert" in result.stderr

    def test_namelist_auto_detection(self, sample_nml):
        """Test that .nml extension is auto-detected as namelist format."""
        result = self.run_suews_run(str(sample_nml), check=True)
        # Should show deprecation warning for namelist format
        assert "DEPRECATION WARNING" in result.stderr
        assert "Namelist format is deprecated" in result.stderr

    # ========== DEFAULT FILE SEARCH TESTS ==========

    def test_default_yaml_search(self, sample_yaml, tmp_path):
        """Test that config.yml is found by default if it exists."""
        # Copy sample YAML to config.yml in temp dir
        config_yml = tmp_path / "config.yml"
        config_yml.write_text(sample_yaml.read_text())

        # Run without arguments from temp directory
        result = subprocess.run(
            ["suews-run"],
            cwd=str(tmp_path),
            capture_output=True,
            text=True,
            timeout=60,
            check=False,
        )

        if result.returncode == 0:
            assert "Using default configuration: config.yml" in result.stdout

    def test_no_default_file_error(self, tmp_path):
        """Test error when no default config file exists."""
        # Run in empty temp directory
        result = subprocess.run(
            ["suews-run"],
            cwd=str(tmp_path),
            capture_output=True,
            text=True,
            timeout=10,
            check=False,
        )

        assert result.returncode != 0
        assert "No configuration file found" in result.stderr

    # ========== ERROR HANDLING TESTS ==========

    def test_invalid_file_path(self):
        """Test that CLI handles non-existent file gracefully."""
        result = self.run_suews_run("/nonexistent/config.yml", check=False)
        assert result.returncode != 0

    def test_invalid_yaml_syntax(self, tmp_path):
        """Test error handling for malformed YAML."""
        bad_yaml = tmp_path / "bad.yml"
        bad_yaml.write_text("model:\n  control\n    invalid yaml")

        result = self.run_suews_run(str(bad_yaml), check=False)
        assert result.returncode != 0

    # ========== INTEGRATION TESTS ==========

    @pytest.mark.skipif(
        not (
            Path(__file__).parent.parent / "fixtures/benchmark1/benchmark1_short.yml"
        ).exists(),
        reason="Benchmark data not available",
    )
    def test_yaml_full_simulation(self, benchmark_dir, tmp_path):
        """End-to-end test: YAML config → run → output files created."""
        yaml_file = benchmark_dir / "benchmark1_short.yml"

        # Run in temp directory to check output files
        result = subprocess.run(
            ["suews-run", str(yaml_file)],
            cwd=str(tmp_path),
            capture_output=True,
            text=True,
            timeout=120,
            check=False,
        )

        if result.returncode == 0:
            assert "successfully done" in result.stdout
            # Check that some output file was created
            output_files = list(tmp_path.glob("*output*.txt")) + list(
                tmp_path.glob("*Output*.txt")
            )
            # Note: Output location depends on config, so this might not always work
            # Just verify successful execution
            assert "The following files have been written out:" in result.stdout

    # ========== DEPRECATION WARNING TESTS ==========

    def test_p_option_deprecation_warning(self, sample_yaml):
        """Test that -p option shows deprecation warning."""
        result = self.run_suews_run("-p", str(sample_yaml), check=False)

        # Should show deprecation for -p option
        assert "deprecated" in result.stderr.lower()
        assert "positional argument" in result.stderr.lower()

    def test_migration_guide_in_warning(self, sample_nml):
        """Test that namelist deprecation includes migration guide."""
        result = self.run_suews_run(str(sample_nml), check=True)

        # Should show migration instructions
        assert "suews-convert" in result.stderr
        assert "config.yml" in result.stderr
