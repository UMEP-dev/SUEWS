"""
Schema Migration Framework for SUEWS YAML Configurations

This module provides tools for migrating YAML configurations between
different schema versions. It includes automatic detection, migration
handlers, and validation.
"""

from typing import Dict, Any, Optional, Callable
from pathlib import Path
import yaml
import logging
from copy import deepcopy

from .version import CURRENT_SCHEMA_VERSION, SCHEMA_VERSIONS

logger = logging.getLogger(__name__)


class SchemaMigrator:
    """
    Handle schema version migrations for SUEWS configurations.

    This class provides methods to detect schema versions and migrate
    configurations between different schema versions.
    """

    def __init__(self):
        """Initialize the migrator with available migration handlers."""
        # Map of migration paths: {(from_version, to_version): handler_function}
        self.migration_handlers: Dict[tuple, Callable] = {
            # Example future migrations:
            ("0.0", "0.1"): self._migrate_0_0_to_0_1,
            # ("1.0", "1.1"): self._migrate_1_0_to_1_1,
            # ("1.1", "2.0"): self._migrate_1_1_to_2_0,
        }

    def auto_detect_version(self, config_dict: Dict[str, Any]) -> str:
        """
        Automatically detect the schema version from configuration structure.

        Args:
            config_dict: Configuration dictionary from YAML

        Returns:
            Detected schema version string
        """
        # Check for explicit schema_version field
        if "schema_version" in config_dict:
            return config_dict["schema_version"]

        # Check for old dual-version fields (from previous implementation)
        if "version" in config_dict or "config_version" in config_dict:
            # This is from the old dual-version implementation
            return "0.0"  # Pre-schema version

        # Heuristics for detecting version from structure
        # Add more heuristics as schema evolves

        # Default to current version if no indicators found
        return CURRENT_SCHEMA_VERSION

    def migrate(
        self,
        config_dict: Dict[str, Any],
        from_version: Optional[str] = None,
        to_version: str = CURRENT_SCHEMA_VERSION,
    ) -> Dict[str, Any]:
        """
        Migrate a configuration from one schema version to another.

        Args:
            config_dict: Configuration dictionary to migrate
            from_version: Source schema version (auto-detected if None)
            to_version: Target schema version (default: current)

        Returns:
            Migrated configuration dictionary

        Raises:
            ValueError: If migration path is not available
        """
        # Make a deep copy to avoid modifying original
        config = deepcopy(config_dict)

        # Auto-detect source version if not provided
        if from_version is None:
            from_version = self.auto_detect_version(config)

        # No migration needed if versions match
        if from_version == to_version:
            logger.info(f"Configuration already at schema version {to_version}")
            return config

        # Find migration path
        migration_path = self._find_migration_path(from_version, to_version)
        if not migration_path:
            raise ValueError(
                f"No migration path available from schema {from_version} to {to_version}"
            )

        # Apply migrations in sequence
        current_version = from_version
        for next_version in migration_path:
            handler_key = (current_version, next_version)
            if handler_key in self.migration_handlers:
                logger.info(f"Migrating from {current_version} to {next_version}")
                config = self.migration_handlers[handler_key](config)
                current_version = next_version
            else:
                # Provide generic migration for compatible versions
                logger.info(
                    f"Using generic migration from {current_version} to {next_version}"
                )
                config = self._generic_migration(config, current_version, next_version)
                current_version = next_version

        # Set the target schema version
        config["schema_version"] = to_version

        return config

    def _find_migration_path(
        self, from_version: str, to_version: str
    ) -> Optional[list]:
        """
        Find the migration path between two schema versions.

        Args:
            from_version: Starting version
            to_version: Target version

        Returns:
            List of intermediate versions, or None if no path exists
        """
        # For now, simple direct path
        # In future, could implement graph traversal for complex paths

        # Parse versions
        try:
            from_major, from_minor = self._parse_version(from_version)
            to_major, to_minor = self._parse_version(to_version)
        except ValueError:
            return None

        path = []

        # Handle special case of pre-0.1 versions
        if from_version == "0.9":
            path.append("0.1")
            from_major, from_minor = 0, 1

        # If major versions differ, need breaking change migration
        if from_major < to_major:
            # Add intermediate major versions
            for major in range(from_major + 1, to_major + 1):
                path.append(f"{major}.0")
        elif from_major == to_major and from_minor < to_minor:
            # Add intermediate minor versions
            for minor in range(from_minor + 1, to_minor + 1):
                path.append(f"{from_major}.{minor}")

        return path if path else None

    def _parse_version(self, version: str) -> tuple:
        """Parse version string into major and minor components."""
        if version == "0.9":  # Special case for pre-release
            return (0, 9)

        parts = version.split(".")
        major = int(parts[0])
        minor = int(parts[1]) if len(parts) > 1 else 0
        return (major, minor)

    def _generic_migration(
        self, config: Dict[str, Any], from_version: str, to_version: str
    ) -> Dict[str, Any]:
        """
        Generic migration for compatible versions.

        This handles minor version bumps that don't require specific migrations.
        """
        # For backward-compatible changes, just update the version
        logger.info(f"Applying generic migration from {from_version} to {to_version}")
        return config

    # Example migration handlers (to be implemented as schema evolves)

    def _migrate_0_0_to_0_1(self, config: Dict[str, Any]) -> Dict[str, Any]:
        """
        Migrate from dual-version system (0.0) to single schema_version (0.1).

        This handles configurations from the previous implementation that had
        both 'version' and 'config_version' fields.
        """
        # Remove old version fields
        config.pop("version", None)
        config.pop("config_version", None)

        # Add new schema_version field
        config["schema_version"] = "0.1"

        logger.info("Migrated from dual-version system to schema version 0.1")
        return config

    # Future migration examples:
    # def _migrate_0_1_to_1_0(self, config: Dict[str, Any]) -> Dict[str, Any]:
    #     """Migrate from schema 0.1 to 1.0 (example)."""
    #     # Add new optional field with default value
    #     if "model" in config and "new_field" not in config["model"]:
    #         config["model"]["new_field"] = "default_value"
    #     return config

    # def _migrate_1_1_to_2_0(self, config: Dict[str, Any]) -> Dict[str, Any]:
    #     """Migrate from schema 1.1 to 2.0 (breaking change example)."""
    #     # Rename field
    #     if "old_field_name" in config:
    #         config["new_field_name"] = config.pop("old_field_name")
    #     # Restructure data
    #     if "flat_structure" in config:
    #         config["nested"] = {"structure": config.pop("flat_structure")}
    #     return config


def migrate_config_file(
    input_path: str,
    output_path: Optional[str] = None,
    to_version: str = CURRENT_SCHEMA_VERSION,
) -> None:
    """
    Migrate a YAML configuration file to a different schema version.

    Args:
        input_path: Path to input YAML file
        output_path: Path to output YAML file (overwrites input if None)
        to_version: Target schema version
    """
    input_file = Path(input_path)
    if not input_file.exists():
        raise FileNotFoundError(f"Input file not found: {input_path}")

    # Load configuration
    with open(input_file, "r") as f:
        config = yaml.safe_load(f)

    # Migrate
    migrator = SchemaMigrator()
    migrated = migrator.migrate(config, to_version=to_version)

    # Save
    output_file = Path(output_path) if output_path else input_file
    with open(output_file, "w") as f:
        yaml.dump(migrated, f, default_flow_style=False, sort_keys=False)

    logger.info(f"Migrated configuration saved to {output_file}")


def check_migration_needed(config_path: str) -> bool:
    """
    Check if a configuration file needs migration.

    Args:
        config_path: Path to YAML configuration file

    Returns:
        True if migration is needed, False otherwise
    """
    with open(config_path, "r") as f:
        config = yaml.safe_load(f)

    migrator = SchemaMigrator()
    detected_version = migrator.auto_detect_version(config)

    return detected_version != CURRENT_SCHEMA_VERSION
