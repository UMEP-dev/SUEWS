"""Test schema versioning functionality for YAML configs."""

import pytest
import yaml
import tempfile
from pathlib import Path
import warnings
from unittest.mock import patch, MagicMock
import sys

# Add parent directory to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

# Import package resource handling
try:
    from importlib.resources import files
except ImportError:
    # backport for python < 3.9
    from importlib_resources import files


def load_supy_resource(resource_path: str) -> str:
    """
    Load a resource file from the supy package.
    
    Args:
        resource_path: Path relative to supy package root (e.g., "sample_data/sample_config.yml")
    
    Returns:
        Content of the resource file as string
    """
    supy_resources = files("supy")
    parts = resource_path.split("/")
    resource = supy_resources
    for part in parts:
        resource = resource / part
    return resource.read_text()


from supy.data_model.core import SUEWSConfig
from supy.data_model.schema import (
    CURRENT_SCHEMA_VERSION,
    is_schema_compatible,
    get_schema_compatibility_message,
    validate_schema_version,
    SchemaMigrator,
    migrate_config_file,
    increment_schema_version,
    update_yaml_schema_version,
)


class TestSchemaVersioning:
    """Test suite for YAML configuration schema versioning."""

    def test_config_with_schema_version(self):
        """Test that SUEWSConfig accepts schema_version field."""
        config_data = {
            "name": "test_config",
            "schema_version": "1.0",
            "description": "Test configuration",
            "model": {},
            "sites": [{"name": "test_site", "gridiv": 1}],
        }

        config = SUEWSConfig(**config_data)
        assert config.schema_version == "1.0"

    def test_config_without_schema_version_gets_default(self):
        """Test that SUEWSConfig gets default schema version when not specified."""
        config_data = {
            "name": "test_config",
            "description": "Test configuration",
            "model": {},
            "sites": [{"name": "test_site", "gridiv": 1}],
        }

        config = SUEWSConfig(**config_data)
        assert config.schema_version == CURRENT_SCHEMA_VERSION

    def test_schema_compatibility_check(self):
        """Test schema compatibility checking."""
        # Same version - compatible
        assert is_schema_compatible("1.0", "1.0") is True

        # Future: test minor version compatibility
        # assert is_schema_compatible("1.0", "1.1") is True  # 1.1 backward compatible with 1.0

    def test_compatibility_messages(self):
        """Test schema compatibility message generation."""
        # No message for current version
        assert get_schema_compatibility_message(CURRENT_SCHEMA_VERSION) is None

        # No message for None (assumes current)
        assert get_schema_compatibility_message(None) is None

        # Message for older version
        msg = get_schema_compatibility_message("0.9")
        assert msg is not None
        assert "older schema" in msg

        # Message for newer version
        msg = get_schema_compatibility_message("2.0")
        assert msg is not None
        assert "newer schema" in msg

    def test_validate_schema_version_strict(self):
        """Test strict schema validation."""
        # Current version - no error
        validate_schema_version(CURRENT_SCHEMA_VERSION, strict=True)

        # Incompatible version - raises error in strict mode
        with pytest.raises(ValueError) as exc:
            validate_schema_version("2.0", strict=True)
        assert "incompatible" in str(exc.value).lower()

    def test_validate_schema_version_warnings(self):
        """Test schema validation warnings in non-strict mode."""
        with warnings.catch_warnings(record=True) as w:
            warnings.simplefilter("always")
            validate_schema_version("0.9", strict=False)

            # Should issue warning for incompatible version
            assert len(w) > 0
            assert "schema" in str(w[0].message).lower()

    def test_from_yaml_with_schema_version(self):
        """Test loading YAML with schema version."""
        # Create a temporary YAML file
        with tempfile.NamedTemporaryFile(mode="w", suffix=".yml", delete=False) as f:
            yaml_content = {
                "name": "test_config",
                "schema_version": "1.0",
                "description": "Test configuration",
                "model": {},
                "sites": [{"name": "test_site", "gridiv": 1}],
            }
            yaml.dump(yaml_content, f)
            yaml_path = f.name

        try:
            # Mock the logger to capture log messages
            with patch("supy.data_model.core.config.logger_supy") as mock_logger:
                config = SUEWSConfig.from_yaml(yaml_path)

                # Check that schema version was logged
                mock_logger.info.assert_any_call(
                    "Loading config with schema version: 1.0"
                )

                assert config.schema_version == "1.0"
        finally:
            Path(yaml_path).unlink()


class TestSchemaMigration:
    """Test schema migration functionality."""

    def test_auto_detect_version(self):
        """Test automatic schema version detection."""
        migrator = SchemaMigrator()

        # Explicit schema_version
        config = {"schema_version": "1.0"}
        assert migrator.auto_detect_version(config) == "1.0"

        # Old dual-version system
        config = {"version": "2025.8.1", "config_version": "v1.0"}
        assert migrator.auto_detect_version(config) == "0.9"

        # No version - defaults to current
        config = {"name": "test"}
        assert migrator.auto_detect_version(config) == CURRENT_SCHEMA_VERSION

    def test_migrate_same_version(self):
        """Test migration when versions are the same."""
        migrator = SchemaMigrator()
        config = {"schema_version": "1.0", "name": "test"}

        result = migrator.migrate(config, from_version="1.0", to_version="1.0")
        assert result["schema_version"] == "1.0"
        assert result["name"] == "test"

    def test_version_parsing(self):
        """Test version string parsing."""
        migrator = SchemaMigrator()

        assert migrator._parse_version("1.0") == (1, 0)
        assert migrator._parse_version("2.3") == (2, 3)
        assert migrator._parse_version("0.9") == (0, 9)

    def test_migration_path_finding(self):
        """Test finding migration paths between versions."""
        migrator = SchemaMigrator()

        # Direct path from 0.9 to 1.0
        path = migrator._find_migration_path("0.9", "1.0")
        assert path == ["1.0"]

        # No path needed for same version
        path = migrator._find_migration_path("1.0", "1.0")
        assert path is None or path == []


class TestSchemaVersionUtility:
    """Test schema version update utility."""

    def test_increment_schema_version(self):
        """Test schema version incrementing."""
        # Minor increments
        assert increment_schema_version("1.0", "minor") == "1.1"
        assert increment_schema_version("1.9", "minor") == "1.10"

        # Major increments
        assert increment_schema_version("1.5", "major") == "2.0"
        assert increment_schema_version("2.3", "major") == "3.0"

        # Invalid format returns default
        assert increment_schema_version("invalid", "minor") == "1.0"

    def test_update_yaml_schema_version(self):
        """Test updating schema version in YAML file."""
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
            # Update with specific version
            result = update_yaml_schema_version(yaml_path, schema_version="1.0")
            assert result is True

            # Read back and verify
            with open(yaml_path, "r") as f:
                updated = yaml.safe_load(f)

            assert updated["schema_version"] == "1.0"

            # Try updating to same version - should return False
            result = update_yaml_schema_version(yaml_path, schema_version="1.0")
            assert result is False
        finally:
            yaml_path.unlink()

    def test_migrate_from_dual_version(self):
        """Test migration from old dual-version system."""
        # Create a file with old dual-version system
        with tempfile.NamedTemporaryFile(mode="w", suffix=".yml", delete=False) as f:
            yaml_content = {
                "name": "test_config_v1.0",
                "version": "2025.8.1.dev0",
                "config_version": "v1.0",
                "description": "Sample config v1.0 designed for supy version 2025.8.1",
                "model": {},
                "sites": [],
            }
            yaml.dump(yaml_content, f)
            yaml_path = Path(f.name)

        try:
            # Update to new schema version
            result = update_yaml_schema_version(yaml_path, schema_version="1.0")
            assert result is True

            # Read back and verify
            with open(yaml_path, "r") as f:
                updated = yaml.safe_load(f)

            # New field added
            assert updated["schema_version"] == "1.0"

            # Old fields removed
            assert "version" not in updated
            assert "config_version" not in updated

            # Name simplified (version suffix removed)
            assert updated["name"] == "test_config"

            # Description should be kept as-is (cleaning only happens with specific patterns)
            assert updated["description"] == "Sample config v1.0 designed for supy version 2025.8.1"
        finally:
            yaml_path.unlink()


class TestSampleConfig:
    """Test that sample config has correct schema version."""

    def test_sample_config_has_schema_version(self):
        """Test that sample_config.yml has schema_version field."""
        # Use package resources to load sample_config.yml
        # This works regardless of installation method (editable, wheel, etc.)
        config_content = load_supy_resource("sample_data/sample_config.yml")
        config = yaml.safe_load(config_content)

        assert "schema_version" in config, (
            "sample_config.yml should have 'schema_version' field"
        )
        assert config["schema_version"] == "1.0"

        # Should not have old version fields
        assert "version" not in config, (
            "sample_config.yml should not have 'version' field"
        )
        assert "config_version" not in config, (
            "sample_config.yml should not have 'config_version' field"
        )

        print(f"✓ sample_config.yml has schema_version: {config['schema_version']}")


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
