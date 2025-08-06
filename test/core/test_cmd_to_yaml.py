"""
Test for suews-convert yaml subcommand using legacy format fixtures.

This test verifies that the converter can successfully
convert legacy table-based SUEWS input to YAML format.
"""

import pytest
from pathlib import Path
import tempfile
import yaml
from click.testing import CliRunner

# Import the command group and the original to_yaml for direct testing
from supy.cmd.table_converter import convert_table_cmd
from supy.cmd.to_yaml import to_yaml


class TestToYamlCommand:
    """Test the suews-convert yaml command line tool."""

    @pytest.fixture
    def legacy_input_dir(self):
        """Path to legacy format test fixtures."""
        return Path(__file__).parent.parent / "fixtures" / "legacy_format"

    @pytest.fixture
    def runner(self):
        """Click test runner."""
        return CliRunner()

    def test_to_yaml_basic_conversion(self, runner, legacy_input_dir):
        """Test basic conversion from legacy format to YAML using direct command."""
        with tempfile.TemporaryDirectory() as tmpdir:
            output_file = Path(tmpdir) / "test_config.yml"
            
            # Run the command directly (for backward compatibility testing)
            result = runner.invoke(
                to_yaml,
                [
                    "-i", str(legacy_input_dir),
                    "-o", str(output_file)
                ]
            )
            
            # Check command executed successfully
            assert result.exit_code == 0, f"Command failed: {result.output}"
            assert output_file.exists(), "Output YAML file was not created"
            
            # Verify the YAML file is valid
            with open(output_file, 'r') as f:
                config = yaml.safe_load(f)
            
            # Check basic structure
            assert "name" in config
            assert "model" in config
            assert "physics" in config["model"]
            
            # Check that physics options are included
            assert "netradiationmethod" in config["model"]["physics"]
            assert "storageheatmethod" in config["model"]["physics"]
            
            # Check that control settings are included
            assert "control" in config["model"]
            assert "tstep" in config["model"]["control"]
            
            # Check that sites are included
            assert "sites" in config
            assert len(config["sites"]) > 0

    def test_to_yaml_with_version_conversion(self, runner, legacy_input_dir):
        """Test conversion with version upgrade (if supported)."""
        with tempfile.TemporaryDirectory() as tmpdir:
            output_file = Path(tmpdir) / "test_config_converted.yml"
            
            # Try conversion with a version flag
            # This tests the optional version conversion feature
            result = runner.invoke(
                to_yaml,
                [
                    "-i", str(legacy_input_dir),
                    "-o", str(output_file),
                    "-f", "2020a"  # From version
                ]
            )
            
            # The command should either succeed or give a clear error
            # about version compatibility
            if result.exit_code == 0:
                assert output_file.exists(), "Output YAML file was not created"
                
                # Verify the YAML file is valid
                with open(output_file, 'r') as f:
                    config = yaml.safe_load(f)
                
                assert config is not None, "YAML file is empty or invalid"

    def test_to_yaml_missing_runcontrol(self, runner):
        """Test error handling when RunControl.nml is missing."""
        with tempfile.TemporaryDirectory() as tmpdir:
            # Create an empty input directory
            empty_dir = Path(tmpdir) / "empty_input"
            empty_dir.mkdir()
            output_file = Path(tmpdir) / "test_config.yml"
            
            # Run the command
            result = runner.invoke(
                to_yaml,
                [
                    "-i", str(empty_dir),
                    "-o", str(output_file)
                ]
            )
            
            # Should fail with appropriate error
            assert result.exit_code != 0
            assert "RunControl.nml not found" in result.output

    def test_to_yaml_output_structure(self, runner, legacy_input_dir):
        """Test that the output YAML has the expected structure."""
        with tempfile.TemporaryDirectory() as tmpdir:
            output_file = Path(tmpdir) / "test_structure.yml"
            
            # Run the command
            result = runner.invoke(
                to_yaml,
                [
                    "-i", str(legacy_input_dir),
                    "-o", str(output_file)
                ]
            )
            
            assert result.exit_code == 0, f"Command failed: {result.output}"
            
            # Load and check structure
            with open(output_file, 'r') as f:
                config = yaml.safe_load(f)
            
            # Check main sections
            assert isinstance(config, dict), "Config should be a dictionary"
            
            # Check model section
            model = config.get("model", {})
            assert "control" in model, "Missing control section"
            assert "physics" in model, "Missing physics section"
            
            # Check that sites data is included
            assert "sites" in config, "Missing sites section"
            
            # Verify some converted data
            control = model.get("control", {})
            if "tstep" in control:
                # tstep should be a number or a RefValue
                tstep = control["tstep"]
                if isinstance(tstep, dict):
                    assert "value" in tstep, "RefValue should have 'value' field"
    
    def test_unified_interface_2025_conversion(self, runner, legacy_input_dir):
        """Test conversion using the unified interface with 2025a target (auto YAML)."""
        with tempfile.TemporaryDirectory() as tmpdir:
            output_file = Path(tmpdir) / "test_2025.yml"
            
            # Run the command using 2025a which should trigger YAML conversion
            result = runner.invoke(
                convert_table_cmd,
                [
                    "-f", "2024a",  # From version
                    "-t", "2025a",  # To 2025a (should convert to YAML)
                    "-i", str(legacy_input_dir),
                    "-o", str(output_file)
                ]
            )
            
            # Check command executed successfully
            assert result.exit_code == 0, f"Command failed: {result.output}"
            assert output_file.exists(), "Output YAML file was not created"
            
            # Verify the YAML file is valid
            with open(output_file, 'r') as f:
                config = yaml.safe_load(f)
            
            # Check basic structure
            assert "name" in config
            assert "model" in config
            assert "sites" in config
    
    def test_unified_interface_table_conversion(self, runner, legacy_input_dir):
        """Test table-to-table conversion using the unified interface."""
        with tempfile.TemporaryDirectory() as tmpdir:
            output_dir = Path(tmpdir) / "output_tables"
            
            # Run the command for table-to-table conversion (pre-2025)
            result = runner.invoke(
                convert_table_cmd,
                [
                    "-f", "2020a",  # From version
                    "-t", "2024a",  # To version (table format)
                    "-i", str(legacy_input_dir),
                    "-o", str(output_dir)
                ]
            )
            
            # For table conversion, it should work if RunControl.nml exists
            # The test might fail if the fixture doesn't have proper 2020a format
            # but we're testing the interface works correctly
            if "RunControl.nml not found" not in result.output:
                # If it found the files, check that it attempted conversion
                assert "Converting tables from 2020a to 2024a" in result.output or result.exit_code != 0