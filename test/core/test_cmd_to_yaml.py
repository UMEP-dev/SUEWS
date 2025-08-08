"""
Test for suews-convert yaml subcommand.

This test verifies that the converter can successfully convert
various legacy SUEWS formats to YAML, including:
- The oldest supported format (2016a)
- Intermediate versions from SUEWS-Benchmark repository
- The most recent table format (2025a)

It also tests auto-detection and cleaning of messy legacy files.
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

    @pytest.fixture
    def legacy_2024a_dir(self):
        """Path to 2024a format test fixtures."""
        return Path(__file__).parent.parent / "fixtures" / "legacy_format" / "2024a"

    @pytest.mark.skipif(not SUPY_AVAILABLE, reason="SuPy not available")
    def test_2016a_to_latest_yaml(self, runner, legacy_2016a_dir):
        """Test that 2016a (oldest supported format) converts to latest YAML.

        This ensures the complete conversion chain from 2016a through all
        intermediate versions to YAML works correctly.
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
        "version", ["2016a", "2018a", "2018b", "2018c", "2019a", "2020a", "2021a"]
    )
    @pytest.mark.skipif(not SUPY_AVAILABLE, reason="SuPy not available")
    def test_benchmark_versions_to_yaml(self, runner, version):
        """Test conversion of all benchmark versions to YAML.

        These are real legacy files from the SUEWS-Benchmark repository,
        which may contain messy formatting (tabs, inline comments, etc.).
        This tests both the cleaning functionality and conversion chain.
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

    def test_messy_file_cleaning(self, runner):
        """Test that messy legacy files are properly cleaned during conversion.

        The 2016a files from SUEWS-Benchmark contain tabs and inline comments
        that need to be cleaned during conversion.
        """
        legacy_dir = (
            Path(__file__).parent.parent / "fixtures" / "legacy_format" / "2016a"
        )

        if not legacy_dir.exists():
            pytest.skip("2016a legacy format fixture not found")

        with tempfile.TemporaryDirectory() as tmpdir:
            # Convert to same version (clean only)
            output_dir = Path(tmpdir) / "cleaned_2016a"

            result = runner.invoke(
                convert_table_cmd,
                [
                    "-f",
                    "2016a",
                    "-t",
                    "2016a",  # Same version = sanitization only
                    "-i",
                    str(legacy_dir),
                    "-o",
                    str(output_dir),
                    "--no-profile-validation",
                ],
            )

            assert result.exit_code == 0, f"Cleaning failed: {result.output}"
            assert "Same version specified" in result.output
            assert "sanitization" in result.output.lower()

            # Check that files were cleaned
            site_select = output_dir / "Input" / "SUEWS_SiteSelect.txt"
            if site_select.exists():
                with open(site_select, "r", encoding="utf-8") as f:
                    content = f.read()

                # Check that tabs were replaced
                assert "\t" not in content, "Tabs were not replaced in cleaned file"

                # Check for inline comments in data lines
                lines = content.split("\n")
                data_lines = [
                    l for l in lines if l.strip() and not l.strip().startswith("!")
                ]
                for line in data_lines[2:]:  # Skip header lines
                    # Allow ! in the last column (site description)
                    # but not in other data columns
                    if "!" in line:
                        # Check if it's in the last field
                        parts = line.split()
                        if len(parts) > 0 and "!" not in " ".join(parts[:-3]):
                            # ! should only be in the last few columns (site info)
                            pass
                        else:
                            pytest.fail(f"Unexpected inline comment: {line}")

                print("✓ File cleaning: Successfully removed tabs and handled comments")
