"""Test version tracking functionality for YAML configs."""

import pytest
import yaml
import tempfile
from pathlib import Path
import warnings
from unittest.mock import patch, MagicMock
import sys

# Add parent directory to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

from supy.data_model.core import SUEWSConfig
from supy.util.update_config_version import (
    update_yaml_config,
    increment_config_version,
    get_current_version,
)


class TestVersionTracking:
    """Test suite for YAML configuration version tracking."""

    def test_config_with_version_fields(self):
        """Test that SUEWSConfig accepts version fields."""
        config_data = {
            "name": "test_config",
            "version": "2025.8.1.dev0",
            "config_version": "v1.0",
            "description": "Test configuration",
            "model": {},
            "sites": [{"name": "test_site", "gridiv": 1}],
        }

        config = SUEWSConfig(**config_data)
        assert config.version == "2025.8.1.dev0"
        assert config.config_version == "v1.0"

    def test_config_without_version_fields(self):
        """Test that SUEWSConfig works without version fields (backward compatibility)."""
        config_data = {
            "name": "test_config",
            "description": "Test configuration",
            "model": {},
            "sites": [{"name": "test_site", "gridiv": 1}],
        }

        config = SUEWSConfig(**config_data)
        assert config.version is None
        assert config.config_version is None

    def test_version_mismatch_warning(self):
        """Test that version mismatch triggers a warning."""
        config_data = {
            "name": "test_config",
            "version": "2024.1.0",  # Old version
            "config_version": "v1.0",
            "description": "Test configuration",
            "model": {},
            "sites": [{"name": "test_site", "gridiv": 1}],
        }

        with warnings.catch_warnings(record=True) as w:
            warnings.simplefilter("always")
            config = SUEWSConfig(**config_data)
            config.validate_version_compatibility()

            # Check if warning was issued
            assert any(
                "version mismatch" in str(warning.message).lower() for warning in w
            )

    def test_version_match_no_warning(self):
        """Test that matching versions don't trigger warnings."""
        # Mock the current version to match config
        with patch("supy.data_model.core.__version__", "2025.8.1.dev0"):
            config_data = {
                "name": "test_config",
                "version": "2025.8.1.dev0",
                "config_version": "v1.0",
                "description": "Test configuration",
                "model": {},
                "sites": [{"name": "test_site", "gridiv": 1}],
            }

            with warnings.catch_warnings(record=True) as w:
                warnings.simplefilter("always")
                config = SUEWSConfig(**config_data)
                config.validate_version_compatibility()

                # Check no version mismatch warning
                assert not any(
                    "version mismatch" in str(warning.message).lower() for warning in w
                )

    def test_from_yaml_logs_version(self):
        """Test that loading from YAML logs version information."""
        # Create a temporary YAML file
        with tempfile.NamedTemporaryFile(mode="w", suffix=".yml", delete=False) as f:
            yaml_content = {
                "name": "test_config",
                "version": "2025.8.1.dev0",
                "config_version": "v1.0",
                "description": "Test configuration",
                "model": {},
                "sites": [{"name": "test_site", "gridiv": 1}],
            }
            yaml.dump(yaml_content, f)
            yaml_path = f.name

        try:
            # Mock the logger to capture log messages
            with patch("supy.data_model.core.logger_supy") as mock_logger:
                config = SUEWSConfig.from_yaml(yaml_path)

                # Check that version info was logged
                mock_logger.info.assert_any_call(
                    "Loading config designed for model version: 2025.8.1.dev0"
                )
                mock_logger.info.assert_any_call("Config schema version: v1.0")
        finally:
            Path(yaml_path).unlink()

    def test_increment_config_version(self):
        """Test config version incrementing."""
        assert increment_config_version("v1.0") == "v1.1"
        assert increment_config_version("v1.9") == "v1.10"
        assert increment_config_version("v2.5") == "v2.6"
        assert increment_config_version("invalid") == "v1.0"

    def test_update_yaml_config(self):
        """Test updating YAML config with version information."""
        # Create a temporary YAML file
        with tempfile.NamedTemporaryFile(mode="w", suffix=".yml", delete=False) as f:
            yaml_content = {
                "name": "test_config",
                "description": "Test configuration",
                "model": {},
                "sites": [],
            }
            yaml.dump(yaml_content, f)
            yaml_path = Path(f.name)

        try:
            # Update the config with version info
            result = update_yaml_config(
                yaml_path,
                model_version="2025.8.1.dev0",
                config_version="v1.0",
                update_name=True,
            )

            assert result is True

            # Read back and verify
            with open(yaml_path, "r") as f:
                updated = yaml.safe_load(f)

            assert updated["version"] == "2025.8.1.dev0"
            assert updated["config_version"] == "v1.0"
            assert updated["name"] == "test_config_v1.0"
            assert "2025.8.1.dev0" in updated["description"]
        finally:
            yaml_path.unlink()

    def test_update_yaml_auto_increment(self):
        """Test auto-incrementing config version."""
        # Create a temporary YAML file with existing version
        with tempfile.NamedTemporaryFile(mode="w", suffix=".yml", delete=False) as f:
            yaml_content = {
                "name": "test_config_v1.0",
                "version": "2025.8.0",
                "config_version": "v1.0",
                "description": "Test configuration",
                "model": {},
                "sites": [],
            }
            yaml.dump(yaml_content, f)
            yaml_path = Path(f.name)

        try:
            # Update with auto-increment
            result = update_yaml_config(
                yaml_path,
                model_version="2025.8.1.dev0",
                auto_increment=True,
                update_name=True,
            )

            assert result is True

            # Read back and verify
            with open(yaml_path, "r") as f:
                updated = yaml.safe_load(f)

            assert updated["version"] == "2025.8.1.dev0"
            assert updated["config_version"] == "v1.1"  # Incremented
            assert updated["name"] == "test_config_v1.1"
        finally:
            yaml_path.unlink()


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
