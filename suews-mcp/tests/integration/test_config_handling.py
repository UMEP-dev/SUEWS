"""Integration tests for configuration handling and file I/O operations in SUEWS MCP Server."""

import pytest
import pandas as pd
import numpy as np
from pathlib import Path
from unittest.mock import Mock, AsyncMock, patch, mock_open
import tempfile
import yaml
import json
import os
import sys
from datetime import datetime

# Add src to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent.parent / "src"))

from suews_mcp.config import MCPServerConfig
from suews_mcp.handlers import SUEWSMCPHandlers
from suews_mcp.preprocessing import ConfigValidator, DataFormatConverter
from suews_mcp.validation import validate_configuration, ValidationResult


class TestConfigurationFileHandling:
    """Test configuration file I/O operations and format handling."""

    @pytest.fixture
    def config_handler_setup(self):
        """Setup for configuration handling tests."""
        return MCPServerConfig(
            server_name="config-test-server",
            server_version="0.1.0-test",
            enable_validation_tool=True,
            enable_simulation_tool=True,
            enable_analysis_tool=True,
        )

    @pytest.fixture
    def complete_suews_config(self):
        """Complete SUEWS configuration for testing."""
        return {
            "name": "comprehensive_test",
            "description": "Comprehensive SUEWS configuration for integration testing",
            "metadata": {
                "created": "2024-01-01T00:00:00Z",
                "version": "2024.1",
                "author": "Integration Test Suite",
            },
            "model": {
                "control": {
                    "tstep": 3600,
                    "forcing_file": {"value": "forcing/met_data.txt"},
                    "start_doy": {"value": 1},
                    "end_doy": {"value": 365},
                    "year": {"value": 2024},
                    "resolutionfilesdisaggr": {"value": 60},
                    "outputformat": {"value": 1},
                },
                "physics": {
                    "netradiationmethod": {"value": 3},
                    "roughlenmommethod": {"value": 1},
                    "conductancemethod": {"value": 4},
                    "stabilitymethod": {"value": 3},
                    "storedrainprm": {"value": 1},
                    "anthropogenic": {"value": 1},
                    "snowuse": {"value": 1},
                    "soildepth": {"value": 350.0},
                    "drainagemethod": {"value": 1},
                },
                "output": {
                    "writeout_interval": {"value": 60},
                    "output_line_format": {"value": 1},
                    "keep_suews_ctrl": {"value": 1},
                },
            },
            "sites": [
                {
                    "name": "urban_test_site",
                    "properties": {
                        "lat": {"value": 51.5074, "units": "degrees"},
                        "lng": {"value": -0.1278, "units": "degrees"},
                        "alt": {"value": 50.0, "units": "m"},
                        "timezone": {"value": 0, "units": "hours"},
                        "popdensdaytime": {"value": 100.0, "units": "people/ha"},
                        "popdensnighttime": {"value": 50.0, "units": "people/ha"},
                    },
                    "land_cover": {
                        "paved": {
                            "sfr": {"value": 0.35},
                            "emis": {"value": 0.95},
                            "roughlenmmom": {"value": 0.1, "units": "m"},
                            "roughlenmdish": {"value": 0.01, "units": "m"},
                            "albedo": {"value": 0.12},
                            "cpanth": {"value": 0.28, "units": "MJ/m3/K"},
                            "kcpanth": {"value": 0.9, "units": "W/m/K"},
                        },
                        "bldgs": {
                            "sfr": {"value": 0.35},
                            "emis": {"value": 0.92},
                            "roughlenmmom": {"value": 1.0, "units": "m"},
                            "roughlenmdish": {"value": 0.1, "units": "m"},
                            "albedo": {"value": 0.15},
                            "bldgh": {"value": 15.0, "units": "m"},
                            "cpanth": {"value": 1.2, "units": "MJ/m3/K"},
                            "kcpanth": {"value": 1.51, "units": "W/m/K"},
                        },
                        "grass": {
                            "sfr": {"value": 0.30},
                            "emis": {"value": 0.96},
                            "roughlenmmom": {"value": 0.05, "units": "m"},
                            "roughlenmdish": {"value": 0.005, "units": "m"},
                            "albedo": {"value": 0.18},
                            "lai": {"value": 3.5, "units": "m2/m2"},
                            "porosity_min": {"value": 0.3},
                        },
                    },
                    "initial_conditions": {
                        "soilstore_id": {"value": 150.0, "units": "mm"},
                        "soilstore_surf": {"value": 10.0, "units": "mm"},
                        "snow_frac_vis": {"value": 0.0},
                        "snow_pack": {"value": 0.0, "units": "mm"},
                    },
                    "irrigation": {
                        "wateruse_ap": {"value": 0.0, "units": "mm/day"},
                        "wateruse_auto": {"value": 0.4, "units": "mm/day"},
                        "ie_a1": {"value": 0.3},
                        "ie_a2": {"value": 0.3},
                        "ie_a3": {"value": 0.3},
                    },
                },
                {
                    "name": "suburban_test_site",
                    "properties": {
                        "lat": {"value": 51.4584},
                        "lng": {"value": -0.1048},
                        "alt": {"value": 75.0},
                        "timezone": {"value": 0},
                        "popdensdaytime": {"value": 25.0},
                        "popdensnighttime": {"value": 40.0},
                    },
                    "land_cover": {
                        "paved": {"sfr": {"value": 0.25}},
                        "bldgs": {"sfr": {"value": 0.25}},
                        "grass": {"sfr": {"value": 0.50}},
                    },
                    "initial_conditions": {
                        "soilstore_id": {"value": 200.0},
                        "soilstore_surf": {"value": 15.0},
                    },
                },
            ],
        }

    async def test_yaml_config_loading_and_validation(
        self, complete_suews_config, tmp_path
    ):
        """Test loading and validating YAML configuration files."""

        # Test different YAML formats
        config_variants = {
            "standard": complete_suews_config,
            "compact": {
                "name": "compact_test",
                "model": {
                    "control": {"tstep": 3600},
                    "physics": {"netradiationmethod": {"value": 3}},
                },
                "sites": [{"name": "site1", "properties": {"lat": {"value": 51.5}}}],
            },
            "expanded": complete_suews_config.copy(),
        }

        # Add extra metadata to expanded version
        config_variants["expanded"]["model"]["advanced"] = {
            "anthropogenic_heat_method": {"value": 1},
            "storage_heat_method": {"value": 4},
            "water_use_method": {"value": 2},
        }

        validator = ConfigValidator()

        for variant_name, config_data in config_variants.items():
            config_file = tmp_path / f"{variant_name}_config.yml"

            # Test saving in different YAML styles
            with open(config_file, "w") as f:
                if variant_name == "compact":
                    yaml.dump(config_data, f, default_flow_style=False, width=120)
                else:
                    yaml.dump(config_data, f, default_flow_style=False, indent=2)

            # Validate each variant
            result = validator.validate_config(
                config_path=config_file, strict_mode=False, check_file_paths=False
            )

            assert (
                result.success is True
            ), f"Validation failed for {variant_name} config"
            assert result.data is None  # Validator doesn't return modified data
            assert "config_file" in result.metadata

            # Test that file can be reloaded
            with open(config_file, "r") as f:
                reloaded = yaml.safe_load(f)
                assert reloaded["name"] == config_data["name"]

        print(
            f"✅ YAML config loading and validation completed for {len(config_variants)} variants"
        )

    async def test_json_config_support(self, complete_suews_config, tmp_path):
        """Test JSON configuration file support."""

        # Convert to JSON-compatible format (no complex objects)
        json_config = json.loads(json.dumps(complete_suews_config))

        json_file = tmp_path / "test_config.json"
        with open(json_file, "w") as f:
            json.dump(json_config, f, indent=2)

        # Test custom JSON loader in ConfigValidator
        validator = ConfigValidator()

        # Patch to support JSON loading
        with patch.object(validator, "_load_config") as mock_load:
            mock_load.return_value = json_config

            result = validator.validate_config(
                config_path=json_file, strict_mode=False, check_file_paths=False
            )

            assert result.success is True
            mock_load.assert_called_once_with(json_file)

    async def test_config_file_permissions_and_access(self, tmp_path):
        """Test configuration file access permissions and error handling."""

        config_data = {
            "name": "permission_test",
            "model": {"control": {"tstep": 3600}},
            "sites": [{"name": "test_site"}],
        }

        # Test read-only file
        readonly_file = tmp_path / "readonly_config.yml"
        with open(readonly_file, "w") as f:
            yaml.dump(config_data, f)

        # Make file read-only
        readonly_file.chmod(0o444)

        validator = ConfigValidator()
        result = validator.validate_config(
            config_path=readonly_file, strict_mode=False, check_file_paths=False
        )

        # Should still be able to read
        assert result.success is True

        # Test non-existent file
        nonexistent_file = tmp_path / "nonexistent_config.yml"

        result = validator.validate_config(
            config_path=nonexistent_file, strict_mode=False, check_file_paths=False
        )

        # Should fail gracefully
        assert result.success is False
        assert len(result.issues) > 0
        assert any("file" in issue.message.lower() for issue in result.issues)

        # Test corrupted file
        corrupted_file = tmp_path / "corrupted_config.yml"
        with open(corrupted_file, "w") as f:
            f.write("invalid: yaml: content: [broken\nno closing bracket")

        result = validator.validate_config(
            config_path=corrupted_file, strict_mode=False, check_file_paths=False
        )

        assert result.success is False
        assert len(result.issues) > 0

    async def test_config_validation_with_references(self, tmp_path):
        """Test configuration validation with file path references."""

        # Create supporting files
        forcing_file = tmp_path / "forcing_data.txt"
        forcing_data = pd.DataFrame(
            {
                "iy": [2024] * 24,
                "id": [1] * 24,
                "it": list(range(24)),
                "imin": [0] * 24,
                "Tair": np.random.normal(15, 3, 24),
                "U": np.random.uniform(2, 8, 24),
                "RH": np.random.uniform(50, 85, 24),
            }
        )
        forcing_data.to_csv(forcing_file, sep=" ", index=False)

        profile_file = tmp_path / "profile_data.txt"
        with open(profile_file, "w") as f:
            f.write("# Profile data\n0.1 0.2 0.3\n0.4 0.5 0.6\n")

        # Configuration with file references
        config_with_refs = {
            "name": "reference_test",
            "model": {
                "control": {
                    "tstep": 3600,
                    "forcing_file": {"value": str(forcing_file)},
                    "profile_file": {"value": str(profile_file)},
                },
                "physics": {"netradiationmethod": {"value": 3}},
            },
            "sites": [
                {
                    "name": "ref_site",
                    "properties": {"lat": {"value": 51.5}, "lng": {"value": -0.1}},
                    "land_cover": {
                        "paved": {"sfr": {"value": 0.4}},
                        "bldgs": {"sfr": {"value": 0.3}},
                        "grass": {"sfr": {"value": 0.3}},
                    },
                }
            ],
        }

        config_file = tmp_path / "config_with_refs.yml"
        with open(config_file, "w") as f:
            yaml.dump(config_with_refs, f)

        validator = ConfigValidator()

        # Test with file path checking enabled
        result = validator.validate_config(
            config_path=config_file, strict_mode=False, check_file_paths=True
        )

        assert result.success is True
        assert len([i for i in result.issues if "file" in i.issue_type.lower()]) == 0

        # Test with missing referenced file
        config_with_refs["model"]["control"]["forcing_file"]["value"] = str(
            tmp_path / "missing.txt"
        )

        with open(config_file, "w") as f:
            yaml.dump(config_with_refs, f)

        result = validator.validate_config(
            config_path=config_file, strict_mode=False, check_file_paths=True
        )

        # Should find missing file issue
        file_issues = [i for i in result.issues if "file" in i.issue_type.lower()]
        assert len(file_issues) > 0

    async def test_config_merging_and_inheritance(self, tmp_path):
        """Test configuration merging and template inheritance."""

        # Base template configuration
        base_config = {
            "metadata": {"template": "base_urban"},
            "model": {
                "control": {"tstep": 3600, "year": {"value": 2024}},
                "physics": {
                    "netradiationmethod": {"value": 3},
                    "roughlenmommethod": {"value": 1},
                    "conductancemethod": {"value": 4},
                },
            },
            "sites": [
                {
                    "name": "base_site",
                    "land_cover": {
                        "paved": {"sfr": {"value": 0.4}, "emis": {"value": 0.95}},
                        "bldgs": {"sfr": {"value": 0.3}, "emis": {"value": 0.92}},
                        "grass": {"sfr": {"value": 0.3}, "emis": {"value": 0.96}},
                    },
                }
            ],
        }

        # Specific site configuration
        site_config = {
            "name": "london_site",
            "description": "London urban site configuration",
            "extends": "base_urban",
            "sites": [
                {
                    "name": "london_central",
                    "properties": {
                        "lat": {"value": 51.5074},
                        "lng": {"value": -0.1278},
                        "alt": {"value": 50.0},
                    },
                    "initial_conditions": {
                        "soilstore_id": {"value": 150.0},
                        "soilstore_surf": {"value": 10.0},
                    },
                }
            ],
        }

        base_file = tmp_path / "base_config.yml"
        site_file = tmp_path / "london_config.yml"

        with open(base_file, "w") as f:
            yaml.dump(base_config, f)
        with open(site_file, "w") as f:
            yaml.dump(site_config, f)

        # Test merging logic (would be implemented in a config merger)
        validator = ConfigValidator()

        # Validate base config
        base_result = validator.validate_config(
            config_path=base_file, strict_mode=False, check_file_paths=False
        )
        assert base_result.success is True

        # Validate site config (without inheritance resolution for now)
        site_result = validator.validate_config(
            config_path=site_file, strict_mode=False, check_file_paths=False
        )
        # May have some issues due to missing model section, but should not crash
        assert site_result is not None

    async def test_config_schema_validation(self, complete_suews_config, tmp_path):
        """Test configuration against schema validation."""

        config_file = tmp_path / "schema_test_config.yml"
        with open(config_file, "w") as f:
            yaml.dump(complete_suews_config, f)

        validator = ConfigValidator()

        # Test strict schema validation
        strict_result = validator.validate_config(
            config_path=config_file, strict_mode=True, check_file_paths=False
        )

        # Should pass strict validation with complete config
        errors = [i for i in strict_result.issues if i.severity == "error"]
        assert len(errors) == 0

        # Test with missing required fields
        minimal_config = {
            "name": "minimal_test",
            "sites": [{"name": "minimal_site"}],
            # Missing model section
        }

        minimal_file = tmp_path / "minimal_config.yml"
        with open(minimal_file, "w") as f:
            yaml.dump(minimal_config, f)

        minimal_result = validator.validate_config(
            config_path=minimal_file, strict_mode=True, check_file_paths=False
        )

        # Should find missing required sections
        errors = [i for i in minimal_result.issues if i.severity == "error"]
        assert len(errors) > 0

        missing_section_errors = [
            i for i in errors if "missing" in i.issue_type.lower()
        ]
        assert len(missing_section_errors) > 0

    async def test_configuration_backup_and_versioning(
        self, complete_suews_config, tmp_path
    ):
        """Test configuration backup and versioning functionality."""

        config_file = tmp_path / "versioned_config.yml"

        # Create initial version
        version_1_config = complete_suews_config.copy()
        version_1_config["metadata"]["version"] = "1.0"
        version_1_config["metadata"]["created"] = "2024-01-01T00:00:00Z"

        with open(config_file, "w") as f:
            yaml.dump(version_1_config, f)

        # Simulate configuration modification
        version_2_config = complete_suews_config.copy()
        version_2_config["metadata"]["version"] = "2.0"
        version_2_config["metadata"]["modified"] = "2024-01-15T12:00:00Z"
        version_2_config["model"]["control"]["tstep"] = 1800  # Changed timestep

        # Create backup before modification
        backup_file = tmp_path / "versioned_config.yml.backup"
        with open(backup_file, "w") as f:
            yaml.dump(version_1_config, f)

        # Save new version
        with open(config_file, "w") as f:
            yaml.dump(version_2_config, f)

        # Validate both versions
        validator = ConfigValidator()

        current_result = validator.validate_config(
            config_path=config_file, strict_mode=False, check_file_paths=False
        )

        backup_result = validator.validate_config(
            config_path=backup_file, strict_mode=False, check_file_paths=False
        )

        assert current_result.success is True
        assert backup_result.success is True

        # Verify version differences
        with open(config_file, "r") as f:
            current_config = yaml.safe_load(f)
        with open(backup_file, "r") as f:
            backup_config = yaml.safe_load(f)

        assert (
            current_config["metadata"]["version"]
            != backup_config["metadata"]["version"]
        )
        assert (
            current_config["model"]["control"]["tstep"]
            != backup_config["model"]["control"]["tstep"]
        )


class TestFileIOOperations:
    """Test file I/O operations and format handling."""

    async def test_large_file_handling(self, tmp_path):
        """Test handling of large configuration and data files."""

        # Create large configuration with many sites
        large_config = {
            "name": "large_config_test",
            "description": "Configuration with many sites for large file testing",
            "model": {
                "control": {"tstep": 3600, "year": {"value": 2024}},
                "physics": {"netradiationmethod": {"value": 3}},
            },
            "sites": [],
        }

        # Generate 50 sites
        for i in range(50):
            site = {
                "name": f"site_{i:03d}",
                "properties": {
                    "lat": {"value": 51.0 + i * 0.01},
                    "lng": {"value": -0.5 + i * 0.02},
                    "alt": {"value": 50.0 + i * 2.0},
                },
                "land_cover": {
                    "paved": {"sfr": {"value": 0.3 + i * 0.01}},
                    "bldgs": {"sfr": {"value": 0.3}},
                    "grass": {"sfr": {"value": 0.4 - i * 0.01}},
                },
                "initial_conditions": {
                    "soilstore_id": {"value": 100.0 + i * 5.0},
                    "soilstore_surf": {"value": 10.0 + i * 0.5},
                },
            }
            large_config["sites"].append(site)

        # Save large configuration
        large_config_file = tmp_path / "large_config.yml"
        start_time = datetime.now()

        with open(large_config_file, "w") as f:
            yaml.dump(large_config, f, default_flow_style=False)

        write_time = (datetime.now() - start_time).total_seconds()
        file_size = large_config_file.stat().st_size

        # Test loading large configuration
        validator = ConfigValidator()
        start_time = datetime.now()

        result = validator.validate_config(
            config_path=large_config_file, strict_mode=False, check_file_paths=False
        )

        validation_time = (datetime.now() - start_time).total_seconds()

        assert result.success is True
        assert len(large_config["sites"]) == 50
        assert write_time < 5.0  # Should write within 5 seconds
        assert validation_time < 10.0  # Should validate within 10 seconds
        assert file_size > 10 * 1024  # Should be larger than 10KB

        print(f"✅ Large file handling completed:")
        print(f"   • Sites: {len(large_config['sites'])}")
        print(f"   • File size: {file_size / 1024:.1f} KB")
        print(f"   • Write time: {write_time:.2f} seconds")
        print(f"   • Validation time: {validation_time:.2f} seconds")

    async def test_concurrent_file_access(self, tmp_path):
        """Test concurrent access to configuration files."""

        config_data = {
            "name": "concurrent_test",
            "model": {"control": {"tstep": 3600}},
            "sites": [{"name": "concurrent_site"}],
        }

        config_file = tmp_path / "concurrent_config.yml"
        with open(config_file, "w") as f:
            yaml.dump(config_data, f)

        # Test concurrent read operations
        async def read_config():
            validator = ConfigValidator()
            return validator.validate_config(
                config_path=config_file, strict_mode=False, check_file_paths=False
            )

        # Run multiple concurrent validations
        tasks = [read_config() for _ in range(5)]
        results = await asyncio.gather(*tasks)

        # All should succeed
        assert all(result.success for result in results)
        assert len(results) == 5

        print(
            f"✅ Concurrent file access completed: {len(results)} concurrent operations"
        )

    async def test_file_format_conversion_integration(self, tmp_path):
        """Test integration between different file format conversions."""

        # Test data in multiple input formats
        test_data = pd.DataFrame(
            {
                "timestamp": pd.date_range("2024-01-01", periods=48, freq="h"),
                "temperature": np.random.normal(15, 3, 48),
                "humidity": np.random.uniform(50, 85, 48),
                "wind_speed": np.random.uniform(2, 8, 48),
                "pressure": np.random.normal(101.3, 1, 48),
            }
        )

        converter = DataFormatConverter()

        # Test CSV input
        csv_input = tmp_path / "input_data.csv"
        test_data.to_csv(csv_input, index=False)

        # Convert CSV -> TXT
        txt_output = tmp_path / "converted_data.txt"
        csv_to_txt_result = converter.convert_format(
            input_path=csv_input,
            output_path=txt_output,
            input_format="csv",
            output_format="txt",
        )

        assert csv_to_txt_result.success is True
        assert txt_output.exists()

        # Convert TXT -> SUEWS format
        suews_output = tmp_path / "suews_data.txt"

        # Add required SUEWS columns and mapping
        extended_data = test_data.copy()
        extended_data["year"] = 2024
        extended_data["day_of_year"] = extended_data["timestamp"].dt.dayofyear
        extended_data["hour"] = extended_data["timestamp"].dt.hour
        extended_data["minute"] = 0

        csv_extended = tmp_path / "extended_data.csv"
        extended_data.to_csv(csv_extended, index=False)

        column_mapping = {
            "year": "iy",
            "day_of_year": "id",
            "hour": "it",
            "minute": "imin",
            "temperature": "Tair",
            "humidity": "RH",
            "wind_speed": "U",
            "pressure": "pres",
        }

        csv_to_suews_result = converter.convert_format(
            input_path=csv_extended,
            output_path=suews_output,
            input_format="csv",
            output_format="suews_txt",
            column_mapping=column_mapping,
        )

        assert csv_to_suews_result.success is True
        assert suews_output.exists()

        # Verify SUEWS format
        suews_data = pd.read_csv(suews_output, sep=" ")
        required_cols = ["iy", "id", "it", "imin", "Tair", "RH", "U", "pres"]
        for col in required_cols:
            assert col in suews_data.columns

        print(f"✅ File format conversion integration completed:")
        print(f"   • CSV -> TXT: {csv_to_txt_result.success}")
        print(f"   • CSV -> SUEWS: {csv_to_suews_result.success}")

    async def test_file_error_recovery(self, tmp_path):
        """Test error recovery in file operations."""

        # Test recovery from corrupted files
        corrupted_yaml = tmp_path / "corrupted.yml"
        with open(corrupted_yaml, "w") as f:
            f.write("invalid: yaml: content: [broken\nno closing bracket")

        validator = ConfigValidator()
        result = validator.validate_config(
            config_path=corrupted_yaml, strict_mode=False, check_file_paths=False
        )

        # Should handle gracefully
        assert result.success is False
        assert len(result.issues) > 0
        error_issues = [i for i in result.issues if i.severity == "error"]
        assert len(error_issues) > 0

        # Test recovery from permission errors (simulate)
        with patch("builtins.open", mock_open()) as mock_file:
            mock_file.side_effect = PermissionError("Permission denied")

            result = validator.validate_config(
                config_path=tmp_path / "permission_denied.yml",
                strict_mode=False,
                check_file_paths=False,
            )

            assert result.success is False
            assert len(result.issues) > 0

    async def test_configuration_export_formats(self, complete_suews_config, tmp_path):
        """Test exporting configuration to different formats."""

        # Test YAML export (default)
        yaml_file = tmp_path / "export_test.yml"
        with open(yaml_file, "w") as f:
            yaml.dump(complete_suews_config, f, default_flow_style=False, indent=2)

        # Test compact YAML export
        compact_yaml_file = tmp_path / "export_compact.yml"
        with open(compact_yaml_file, "w") as f:
            yaml.dump(complete_suews_config, f, default_flow_style=False, width=120)

        # Test JSON export
        json_file = tmp_path / "export_test.json"
        with open(json_file, "w") as f:
            json.dump(complete_suews_config, f, indent=2, default=str)

        # Verify all formats are readable
        validator = ConfigValidator()

        # YAML validation
        yaml_result = validator.validate_config(
            config_path=yaml_file, strict_mode=False, check_file_paths=False
        )
        assert yaml_result.success is True

        # Compact YAML validation
        compact_result = validator.validate_config(
            config_path=compact_yaml_file, strict_mode=False, check_file_paths=False
        )
        assert compact_result.success is True

        # JSON validation (with custom loader)
        with patch.object(validator, "_load_config") as mock_load:
            with open(json_file, "r") as f:
                json_data = json.load(f)
            mock_load.return_value = json_data

            json_result = validator.validate_config(
                config_path=json_file, strict_mode=False, check_file_paths=False
            )
            assert json_result.success is True

        # Check file sizes
        yaml_size = yaml_file.stat().st_size
        compact_size = compact_yaml_file.stat().st_size
        json_size = json_file.stat().st_size

        assert yaml_size > 0
        assert compact_size > 0
        assert json_size > 0

        print(f"✅ Configuration export formats completed:")
        print(f"   • YAML: {yaml_size} bytes")
        print(f"   • Compact YAML: {compact_size} bytes")
        print(f"   • JSON: {json_size} bytes")


class TestConfigurationValidationIntegration:
    """Test integration between configuration validation and other components."""

    async def test_validation_with_mcp_handlers(self, complete_suews_config, tmp_path):
        """Test configuration validation integrated with MCP handlers."""

        config_file = tmp_path / "handler_test_config.yml"
        with open(config_file, "w") as f:
            yaml.dump(complete_suews_config, f)

        # Test with MCP handlers
        mcp_config = MCPServerConfig(
            server_name="validation-integration-test", enable_validation_tool=True
        )

        handlers = SUEWSMCPHandlers(mcp_config)

        # Mock validation tool call
        validation_args = {"config_file": str(config_file), "strict": True}

        with patch.object(handlers, "_call_validation_tool") as mock_validation:
            mock_validation.return_value = {
                "content": [
                    {
                        "type": "text",
                        "text": "Configuration validation completed successfully!\n"
                        f"Config file: {config_file}\n"
                        "Validation mode: strict\n"
                        "Issues found: 0 errors, 2 warnings\n"
                        "Overall status: VALID",
                    }
                ]
            }

            result = await handlers.handle_call_tool(
                "validate_suews_config", validation_args
            )

            assert result is not None
            mock_validation.assert_called_once_with(
                "validate_suews_config", validation_args
            )

    async def test_validation_error_reporting_integration(self, tmp_path):
        """Test integration of validation error reporting with error handling system."""

        # Create problematic configuration
        problematic_config = {
            "name": "error_reporting_test",
            "model": {
                "control": {"tstep": -100},  # Invalid timestep
                "physics": {"netradiationmethod": {"value": 99}},  # Invalid method
            },
            "sites": [
                {
                    "properties": {"lat": {"value": 200.0}},  # Invalid latitude
                    "land_cover": {
                        "paved": {"sfr": {"value": 0.8}},  # Fractions don't sum to 1
                        "bldgs": {"sfr": {"value": 0.5}},
                    },
                }
            ],
        }

        config_file = tmp_path / "problematic_config.yml"
        with open(config_file, "w") as f:
            yaml.dump(problematic_config, f)

        # Test validation error reporting
        validation_result = validate_configuration(
            config_path=config_file, check_file_paths=False, strict_mode=True
        )

        assert validation_result is not None
        assert validation_result.success is False
        assert len(validation_result.issues) > 0

        # Check error categorization
        error_types = {issue.severity for issue in validation_result.issues}
        assert "error" in error_types

        # Check that error messages are informative
        error_messages = [
            issue.message
            for issue in validation_result.issues
            if issue.severity == "error"
        ]
        assert len(error_messages) > 0

        # Messages should contain specific problem descriptions
        combined_messages = " ".join(error_messages).lower()
        assert any(
            term in combined_messages for term in ["invalid", "range", "missing", "sum"]
        )


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
