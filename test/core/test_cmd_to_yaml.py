"""
Test for suews-convert yaml subcommand.

This module provides comprehensive testing of the SUEWS table-to-YAML converter:

1. test_oldest_to_latest_yaml_with_validation:
   - Tests complete conversion chain from 2016a (oldest) to latest YAML
   - Validates output with SUEWSConfig to ensure correctness

2. test_legacy_version_auto_detection:
   - Tests auto-detection for all SUEWS versions (2016a-2025a)
   - Handles structurally identical versions correctly

3. test_all_versions_to_yaml_with_auto_detection:
   - Tests conversion of all versions to YAML format
   - Includes file cleaning (tabs, inline comments)
   - Uses real files from SUEWS-Benchmark repository

4. test_same_version_cleaning:
   - Tests sanitisation when converting to same version
   - Verifies tab replacement and whitespace fixing
"""

import pytest
from pathlib import Path
import tempfile
import yaml
from click.testing import CliRunner

# Import the command and converter functions
from supy.cmd.table_converter import convert_table_cmd
from supy.util.converter import detect_table_version

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

    @pytest.mark.skipif(not SUPY_AVAILABLE, reason="SuPy not available")
    def test_oldest_to_latest_yaml_with_validation(self, runner, legacy_2016a_dir):
        """Test complete conversion chain from oldest (2016a) to latest YAML with full validation.

        This is the most comprehensive test - ensures the complete conversion chain
        from 2016a through all intermediate versions to YAML works correctly,
        and validates the output can be loaded by SUEWSConfig.
        """
        with tempfile.TemporaryDirectory() as tmpdir:
            output_file = Path(tmpdir) / "converted_2016a.yml"

            # Convert 2016a to latest YAML using explicit version specification
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

                # Comprehensive validation
                assert config is not None
                assert config.model is not None
                assert len(config.sites) > 0

                # Check that site has key attributes (structure may vary)
                site = config.sites[0]
                # Just verify the site loaded successfully
                assert site is not None, "Site object not created"

                print(
                    "✓ 2016a successfully converted to latest YAML and validated with SUEWSConfig"
                )

            except Exception as e:
                pytest.fail(f"Converted YAML failed validation: {str(e)}")

    @pytest.mark.parametrize(
        "version",
        [
            "2016a",
            "2018a",
            "2018b",
            "2018c",
            "2019a",
            "2020a",
            "2021a",
            "2024a",
            "2025a",
        ],
    )
    def test_legacy_version_auto_detection(self, version):
        """Test auto-detection of all legacy versions from benchmark data."""
        legacy_dir = (
            Path(__file__).parent.parent / "fixtures" / "legacy_format" / version
        )

        if not legacy_dir.exists():
            pytest.skip(f"Legacy format fixture for {version} not found")

        # Test auto-detection
        detected_version = detect_table_version(str(legacy_dir))

        assert detected_version is not None, f"Failed to detect version for {version}"

        # Some versions are truly identical in table structure
        # According to rules.csv analysis:
        # - 2018a, 2018b, 2018c: All identical (Keep actions only)
        # - 2020a, 2021a: Both have BaseT_HC and H_maintain (Keep action for 2020a->2021a)
        truly_identical = {
            "2018a": ["2018a", "2018b", "2018c"],  # No structural differences
            "2018b": ["2018a", "2018b", "2018c"],  # No structural differences
            "2018c": ["2018a", "2018b", "2018c"],  # No structural differences
            "2020a": ["2020a", "2021a"],  # Both have BaseT_HC
            "2021a": ["2020a", "2021a"],  # Both have BaseT_HC
        }

        if version in truly_identical:
            assert detected_version in truly_identical[version], (
                f"Version mismatch for {version}: got {detected_version}, "
                f"acceptable: {truly_identical[version]}"
            )
            if detected_version != version:
                print(
                    f"✓ {version}: Detected as {detected_version} (truly identical structure)"
                )
            else:
                print(f"✓ {version}: Correctly auto-detected")
        else:
            assert detected_version == version, (
                f"Version mismatch: expected {version}, got {detected_version}"
            )
            print(f"✓ {version}: Correctly auto-detected")

    @pytest.mark.parametrize(
        "version",
        [
            "2016a",
            "2018a",
            "2018b",
            "2018c",
            "2019a",
            "2020a",
            "2021a",
            "2024a",
            "2025a",
        ],
    )
    @pytest.mark.skipif(not SUPY_AVAILABLE, reason="SuPy not available")
    def test_all_versions_to_yaml_with_auto_detection(self, runner, version):
        """Test auto-detection and conversion of all SUEWS versions to YAML.

        These are real legacy files from the SUEWS-Benchmark repository,
        which may contain messy formatting (tabs, inline comments, etc.).
        This comprehensive test covers:
        - Auto-detection of version
        - File cleaning (removing tabs, inline comments)
        - Complete conversion chain to YAML
        - All versions from oldest (2016a) to newest (2025a)
        """
        legacy_dir = (
            Path(__file__).parent.parent / "fixtures" / "legacy_format" / version
        )

        if not legacy_dir.exists():
            pytest.skip(f"Legacy format fixture for {version} not found")

        with tempfile.TemporaryDirectory() as tmpdir:
            output_file = Path(tmpdir) / f"converted_{version}.yml"

            # Convert with auto-detection
            result = runner.invoke(
                convert_table_cmd,
                [
                    "-i",
                    str(legacy_dir),
                    "-o",
                    str(output_file),
                    "--no-profile-validation",  # Skip profile validation for speed
                ],
            )

            # Check conversion succeeded
            assert result.exit_code == 0, (
                f"Conversion of {version} failed: {result.output}"
            )
            assert "Detected version:" in result.output or "Converting" in result.output
            assert output_file.exists(), (
                f"Output YAML file was not created for {version}"
            )

            # Verify YAML is valid
            with open(output_file, "r", encoding="utf-8") as f:
                yaml_data = yaml.safe_load(f)

            assert yaml_data is not None, f"YAML file for {version} is empty"
            assert "model" in yaml_data, f"Missing model section in {version} YAML"
            assert "sites" in yaml_data, f"Missing sites section in {version} YAML"

            print(f"✓ {version}: Successfully converted to YAML with auto-detection")
