"""
Test for suews-convert yaml subcommand.

This test verifies that the converter can successfully convert
both the oldest supported format (2016a) and the most recent table
format (2024a) to latest YAML format.
"""

import pytest
from pathlib import Path
import tempfile
import yaml
from click.testing import CliRunner

# Import the command
from supy.cmd.table_converter import convert_table_cmd

# Import supy for end-to-end testing
try:
    import supy as sp
    from supy.data_model.core import SUEWSConfig

    SUPY_AVAILABLE = True
except ImportError:
    SUPY_AVAILABLE = False


class TestTableToYamlConversion:
    """Test table to YAML conversion."""

    @pytest.fixture
    def runner(self):
        """Click test runner."""
        return CliRunner()

    @pytest.fixture
    def legacy_2016a_dir(self):
        """Path to 2016a format test fixtures."""
        return Path(__file__).parent.parent / "fixtures" / "legacy_format" / "2016a"

    @pytest.fixture
    def legacy_2024a_dir(self):
        """Path to 2024a format test fixtures."""
        return Path(__file__).parent.parent / "fixtures" / "legacy_format" / "2024a"

    @pytest.mark.skipif(not SUPY_AVAILABLE, reason="SuPy not available")
    @pytest.mark.xfail(
        reason="2016a fixtures incomplete - missing proper column headers"
    )
    def test_2016a_to_latest_yaml(self, runner, legacy_2016a_dir):
        """Test that 2016a (oldest supported format) converts to latest YAML.

        This ensures the complete conversion chain from 2016a through all
        intermediate versions to YAML works correctly.

        Note: Currently marked as expected failure due to incomplete 2016a test fixtures
        that lack proper column headers required for the data model.
        """
        with tempfile.TemporaryDirectory() as tmpdir:
            output_file = Path(tmpdir) / "converted_2016a.yml"

            # Convert 2016a to latest YAML
            result = runner.invoke(
                convert_table_cmd,
                [
                    "-f",
                    "2016a",
                    "-t",
                    "latest",  # Should use current version
                    "-i",
                    str(legacy_2016a_dir),
                    "-o",
                    str(output_file),
                ],
            )

            # Check conversion succeeded
            assert result.exit_code == 0, f"Conversion failed: {result.output}"
            assert "Converting to latest" in result.output
            assert output_file.exists(), "Output YAML file was not created"

            # Verify YAML is valid
            with open(output_file, "r") as f:
                yaml_data = yaml.safe_load(f)

            assert yaml_data is not None, "YAML file is empty"
            assert "model" in yaml_data, "Missing model section"
            assert "sites" in yaml_data, "Missing sites section"

            # Load with SUEWSConfig to validate structure
            try:
                config = SUEWSConfig.from_yaml(output_file)

                # Basic validation - just check the config loads
                assert config is not None
                assert config.model is not None
                assert len(config.sites) > 0

                print("✓ 2016a successfully converted to latest YAML and validated")

            except Exception as e:
                pytest.fail(f"Converted YAML failed validation: {str(e)}")

    @pytest.mark.skipif(not SUPY_AVAILABLE, reason="SuPy not available")
    def test_2024a_to_latest_yaml(self, runner, legacy_2024a_dir):
        """Test that 2024a (last table format) converts to latest YAML.

        This ensures the table-to-YAML transition works correctly.
        """
        with tempfile.TemporaryDirectory() as tmpdir:
            output_file = Path(tmpdir) / "converted_2024a.yml"

            # Convert 2024a to latest YAML
            result = runner.invoke(
                convert_table_cmd,
                [
                    "-f",
                    "2024a",
                    "-t",
                    "latest",  # Should use current version
                    "-i",
                    str(legacy_2024a_dir),
                    "-o",
                    str(output_file),
                ],
            )

            # Check conversion succeeded
            assert result.exit_code == 0, f"Conversion failed: {result.output}"
            assert "Converting to latest" in result.output
            assert output_file.exists(), "Output YAML file was not created"

            # Verify YAML is valid
            with open(output_file, "r") as f:
                yaml_data = yaml.safe_load(f)

            assert yaml_data is not None, "YAML file is empty"
            assert "model" in yaml_data, "Missing model section"
            assert "sites" in yaml_data, "Missing sites section"

            # Load with SUEWSConfig to validate structure
            try:
                config = SUEWSConfig.from_yaml(output_file)

                # Basic validation - just check the config loads
                assert config is not None
                assert config.model is not None
                assert len(config.sites) > 0

                print("✓ 2024a successfully converted to latest YAML and validated")

            except Exception as e:
                pytest.fail(f"Converted YAML failed validation: {str(e)}")
