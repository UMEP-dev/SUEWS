"""Tests for the ``suews run`` CLI command.

Tests YAML and namelist format support, auto-detection,
backward compatibility, and deprecation warnings.

Uses Click's CliRunner for in-process testing to avoid subprocess overhead.
This is especially important for editable installs where each subprocess
triggers a ninja rebuild check (10-20+ seconds).
"""

from pathlib import Path
import shutil

from conftest import run_cli_command
import pandas as pd
import pytest

from supy.cmd.SUEWS import SUEWS

pytestmark = pytest.mark.api


@pytest.fixture(autouse=True)
def isolated_working_directory(tmp_path, monkeypatch):
    """Keep default-file searches and simulation outputs local to each test."""
    monkeypatch.chdir(tmp_path)


class TestCLIRun:
    """Test ``suews run`` CLI command functionality."""

    @pytest.fixture
    def test_data_dir(self):
        """Get the path to test data fixtures."""
        return Path(__file__).parent.parent / "fixtures"

    @pytest.fixture
    def benchmark_dir(self, test_data_dir):
        """Get the path to benchmark1 fixtures."""
        return test_data_dir / "benchmark1"

    @pytest.fixture
    def sample_yaml(self, benchmark_dir, tmp_path):
        """Copy the committed short case with its relative forcing dependency."""
        yaml_file = tmp_path / "benchmark1_short.yml"
        shutil.copyfile(benchmark_dir / yaml_file.name, yaml_file)
        forcing_dir = tmp_path / "forcing"
        forcing_dir.mkdir()
        forcing_name = "Kc1_2011_data_5_short.txt"
        shutil.copyfile(benchmark_dir / "forcing" / forcing_name, forcing_dir / forcing_name)
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
""",
            encoding="utf-8",
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
        assert "suews convert -i" in combined
        # Should show SUEWS version banner
        assert "SUEWS version" in result.stdout

    def test_namelist_auto_detection(self, cli_runner, sample_nml):
        """Test that .nml extension is auto-detected as namelist format."""
        result = self.run_suews_run(cli_runner, str(sample_nml), check=False)
        # Should show deprecation warning for namelist format
        combined = result.stderr + result.stdout
        assert "DEPRECATION WARNING" in combined
        assert "Namelist format is deprecated" in combined

    # Exercises the legacy DFState adapter, which deprecates itself on use.
    @pytest.mark.filterwarnings("default::DeprecationWarning:supy")
    def test_namelist_frame_adapter_preserves_unvalidated_forcing(
        self, sample_data_loaded
    ):
        """The legacy CLI must not impose the OOP forcing-validation contract."""
        from supy.cmd.SUEWS import _run_namelist_frames

        df_state, df_forcing = sample_data_loaded
        df_forcing_legacy = df_forcing.iloc[:2].copy()
        df_forcing_legacy.loc[:, "rain"] = -999.0

        df_output, df_state_final = _run_namelist_frames(
            df_forcing_legacy,
            df_state.copy(),
        )

        assert len(df_output) == len(df_forcing_legacy)
        assert not df_state_final.empty

    # ========== DEFAULT FILE SEARCH TESTS ==========

    def test_default_yaml_search(self, cli_runner, sample_yaml):
        """A default config must be discovered and complete its simulation."""
        sample_yaml.rename(sample_yaml.with_name("config.yml"))

        result = self.run_suews_run(cli_runner)

        assert "Using default configuration: config.yml" in result.stdout
        assert "successfully done" in result.stdout

    def test_no_default_file_error(self, cli_runner):
        """Test error when no default config file exists."""
        result = self.run_suews_run(cli_runner, check=False)
        assert result.returncode != 0
        assert "No configuration file found" in result.stderr
        assert "config.yml" in result.stderr
        assert "No configuration file found" not in result.stdout

    # ========== ERROR HANDLING TESTS ==========

    def test_invalid_file_path(self, cli_runner):
        """Test that CLI handles non-existent file gracefully."""
        # Click's exists=True validator catches this before our code
        result = self.run_suews_run(cli_runner, "/nonexistent/config.yml", check=False)
        assert result.returncode != 0

    def test_invalid_yaml_syntax(self, cli_runner, tmp_path):
        """Test error handling for malformed YAML."""
        bad_yaml = tmp_path / "bad.yml"
        bad_yaml.write_text("model:\n  control\n    invalid yaml", encoding="utf-8")

        result = self.run_suews_run(cli_runner, str(bad_yaml), check=False)
        assert result.returncode != 0

    # ========== INTEGRATION TESTS ==========

    def test_yaml_full_simulation(self, cli_runner, sample_yaml, tmp_path):
        """An explicit YAML path is detected, run successfully, and saved."""
        result = self.run_suews_run(cli_runner, str(sample_yaml))

        # Cover positional arguments and format detection in this same run.
        assert "YAML configuration" in result.stdout
        assert "DEPRECATION WARNING" not in result.stderr
        assert "successfully done" in result.stdout
        assert "The following files have been written out:" in result.stdout
        output_files = list(tmp_path.glob("*_SUEWS_output.parquet"))
        assert len(output_files) == 1
        output = pd.read_parquet(output_files[0])
        assert not output.empty
        assert output_files[0].name in result.stdout

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
        assert "suews convert -i" in combined
        assert "config.yml" in combined
