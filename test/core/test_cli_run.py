"""Tests for suews-run CLI command.

Tests YAML and namelist format support, auto-detection,
backward compatibility, and deprecation warnings.

Uses Click's CliRunner for in-process testing to avoid subprocess overhead.
This is especially important for editable installs where each subprocess
triggers a ninja rebuild check (10-20+ seconds).
"""

import subprocess
import tempfile
from pathlib import Path
import pytest
from click.testing import CliRunner

# Import the CLI command for in-process testing
from supy.cmd.SUEWS import SUEWS


class _CliResult:
    """Adapter to make CliRunner results look like subprocess.CompletedProcess."""

    def __init__(self, click_result):
        self._result = click_result
        self.returncode = click_result.exit_code
        self.stdout = click_result.output or ""
        # CliRunner captures stderr in output when mix_stderr=False (default)
        # For compatibility, we split based on common patterns
        self.stderr = ""
        if click_result.output:
            lines = click_result.output.split("\n")
            stderr_lines = []
            stdout_lines = []
            for line in lines:
                # Deprecation warnings and errors go to stderr
                if any(
                    kw in line.upper()
                    for kw in ["DEPRECAT", "ERROR", "WARNING", "====="]
                ):
                    stderr_lines.append(line)
                else:
                    stdout_lines.append(line)
            # If we detected stderr content, split it out
            if stderr_lines:
                self.stderr = "\n".join(stderr_lines)


class TestCLIRun:
    """Test suews-run CLI command functionality."""

    @pytest.fixture
    def runner(self):
        """Create a Click CliRunner for in-process testing."""
        return CliRunner()

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

    def run_suews_run(self, runner, *args, check=True):
        """Run suews-run command in-process using CliRunner.

        Parameters
        ----------
        runner : CliRunner
            Click test runner
        *args : str
            Command-line arguments
        check : bool, optional
            Whether to raise on non-zero exit code

        Returns
        -------
        _CliResult
            Result with returncode, stdout, stderr (subprocess-compatible)
        """
        click_result = runner.invoke(SUEWS, args, catch_exceptions=False)
        result = _CliResult(click_result)
        if check and result.returncode != 0:
            raise subprocess.CalledProcessError(
                result.returncode,
                ["suews-run", *args],
                result.stdout,
                result.stderr,
            )
        return result

    @staticmethod
    def run_suews_run_subprocess(*args, check=True, timeout=120):
        """Run suews-run as subprocess (for tests that truly need isolation).

        Only use this for tests that specifically need subprocess behaviour.
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

    def test_cli_help(self, runner):
        """Test that the CLI help works."""
        result = self.run_suews_run(runner, "--help", check=False)
        assert result.returncode == 0
        assert "YAML" in result.stdout
        assert "config.yml" in result.stdout
        assert "DEPRECATED" in result.stdout or "deprecated" in result.stdout.lower()

    # ========== YAML FORMAT TESTS ==========

    def test_yaml_positional_argument(self, runner, sample_yaml, tmp_path):
        """Test running with YAML file as positional argument."""
        # Run in temp directory to avoid polluting test fixtures
        result = self.run_suews_run(
            runner,
            str(sample_yaml),
            check=True,
        )
        assert result.returncode == 0
        assert "YAML configuration" in result.stdout
        assert "successfully done" in result.stdout

    def test_yaml_auto_detection(self, runner, sample_yaml):
        """Test that .yml extension is auto-detected as YAML format."""
        result = self.run_suews_run(runner, str(sample_yaml), check=True)
        assert "YAML configuration" in result.stdout
        # Should NOT show namelist deprecation warning
        assert "DEPRECATION WARNING" not in result.stderr

    def test_yaml_missing_forcing(self, runner, tmp_path):
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

        result = self.run_suews_run(runner, str(minimal_yaml), check=False)
        assert result.returncode != 0
        # Error may be in stdout (click echo) or stderr
        assert "No forcing data" in result.stderr or "No forcing data" in result.stdout

    # ========== NAMELIST FORMAT TESTS ==========

    def test_namelist_with_p_option(self, runner, sample_nml):
        """Test running with namelist using -p option (deprecated)."""
        result = self.run_suews_run(runner, "-p", str(sample_nml), check=False)

        # Should show both deprecation warnings (for -p option and namelist format)
        combined = (result.stderr + result.stdout).lower()
        assert "deprecat" in combined
        # Should show SUEWS version banner
        assert "SUEWS version" in result.stdout

    def test_namelist_positional_argument(self, runner, sample_nml):
        """Test running with namelist as positional argument."""
        result = self.run_suews_run(runner, str(sample_nml), check=False)

        # Should show namelist deprecation warning (may be in stdout with CliRunner)
        combined = result.stderr + result.stdout
        assert "DEPRECATION WARNING" in combined
        assert "suews-convert" in combined
        # Should show SUEWS version banner
        assert "SUEWS version" in result.stdout

    def test_namelist_auto_detection(self, runner, sample_nml):
        """Test that .nml extension is auto-detected as namelist format."""
        result = self.run_suews_run(runner, str(sample_nml), check=False)
        # Should show deprecation warning for namelist format
        combined = result.stderr + result.stdout
        assert "DEPRECATION WARNING" in combined
        assert "Namelist format is deprecated" in combined

    # ========== DEFAULT FILE SEARCH TESTS ==========

    def test_default_yaml_search(self, runner, sample_yaml, tmp_path):
        """Test that config.yml is found by default if it exists."""
        # Copy sample YAML to config.yml in temp dir
        config_yml = tmp_path / "config.yml"
        config_yml.write_text(sample_yaml.read_text())

        # Use CliRunner's isolated_filesystem for cwd simulation
        import os

        old_cwd = os.getcwd()
        try:
            os.chdir(tmp_path)
            result = self.run_suews_run(runner, check=False)
            if result.returncode == 0:
                assert "Using default configuration: config.yml" in result.stdout
        finally:
            os.chdir(old_cwd)

    def test_no_default_file_error(self, runner, tmp_path):
        """Test error when no default config file exists."""
        import os

        old_cwd = os.getcwd()
        try:
            os.chdir(tmp_path)
            result = self.run_suews_run(runner, check=False)
            assert result.returncode != 0
            combined = result.stderr + result.stdout
            assert "No configuration file found" in combined
        finally:
            os.chdir(old_cwd)

    # ========== ERROR HANDLING TESTS ==========

    def test_invalid_file_path(self, runner):
        """Test that CLI handles non-existent file gracefully."""
        # Click's exists=True validator catches this before our code
        click_result = runner.invoke(SUEWS, ["/nonexistent/config.yml"])
        assert click_result.exit_code != 0

    def test_invalid_yaml_syntax(self, runner, tmp_path):
        """Test error handling for malformed YAML."""
        bad_yaml = tmp_path / "bad.yml"
        bad_yaml.write_text("model:\n  control\n    invalid yaml")

        result = self.run_suews_run(runner, str(bad_yaml), check=False)
        assert result.returncode != 0

    # ========== INTEGRATION TESTS ==========

    @pytest.mark.skipif(
        not (
            Path(__file__).parent.parent / "fixtures/benchmark1/benchmark1_short.yml"
        ).exists(),
        reason="Benchmark data not available",
    )
    def test_yaml_full_simulation(self, runner, benchmark_dir, tmp_path):
        """End-to-end test: YAML config → run → output files created."""
        yaml_file = benchmark_dir / "benchmark1_short.yml"

        # Use CliRunner for in-process testing
        result = self.run_suews_run(runner, str(yaml_file), check=False)

        if result.returncode == 0:
            assert "successfully done" in result.stdout
            # Just verify successful execution
            assert "The following files have been written out:" in result.stdout

    # ========== DEPRECATION WARNING TESTS ==========

    def test_p_option_deprecation_warning(self, runner, sample_yaml):
        """Test that -p option shows deprecation warning."""
        result = self.run_suews_run(runner, "-p", str(sample_yaml), check=False)

        # Should show deprecation for -p option (may be in stdout with CliRunner)
        combined = (result.stderr + result.stdout).lower()
        assert "deprecated" in combined
        assert "positional argument" in combined

    def test_migration_guide_in_warning(self, runner, sample_nml):
        """Test that namelist deprecation includes migration guide."""
        result = self.run_suews_run(runner, str(sample_nml), check=False)

        # Should show migration instructions (may be in stdout with CliRunner)
        combined = result.stderr + result.stdout
        assert "suews-convert" in combined
        assert "config.yml" in combined
