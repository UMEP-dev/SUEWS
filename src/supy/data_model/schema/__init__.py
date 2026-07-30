"""
SUEWS Schema Management Module

This module consolidates all schema-related functionality:
- Version management
- Migration between versions
- Schema publishing to JSON
- Utilities for updating schemas

This provides a single, cohesive interface for schema operations.
"""

# Core version management
from .version import (
    CURRENT_SCHEMA_VERSION,
    SCHEMA_VERSIONS,
    is_schema_compatible,
    get_schema_compatibility_message,
    validate_schema_version,
)

# Migration functionality
from .migration import (
    SchemaMigrator,
    migrate_config_file,
)

# Update utilities
from .updater import (
    increment_schema_version,
    update_yaml_schema_version,
)

# Publishing functionality
from .publisher import (
    generate_json_schema,
    save_schema,
    create_schema_bundle,
    validate_config_against_schema,
)

# Export functionality
from .exporter import export_schema
from .interfaces import (
    DATA_INTERFACE_VERSION,
    export_data_interface_artifacts,
    generate_data_interface_catalogue,
    generate_data_interface_schema,
)

__all__ = [
    # Version management
    "CURRENT_SCHEMA_VERSION",
    "SCHEMA_VERSIONS",
    "is_schema_compatible",
    "get_schema_compatibility_message",
    "validate_schema_version",
    # Migration
    "SchemaMigrator",
    "migrate_config_file",
    # Updates
    "increment_schema_version",
    "update_yaml_schema_version",
    # Publishing
    "generate_json_schema",
    "save_schema",
    "create_schema_bundle",
    "validate_config_against_schema",
    # Export
    "export_schema",
    # Data interfaces
    "DATA_INTERFACE_VERSION",
    "export_data_interface_artifacts",
    "generate_data_interface_catalogue",
    "generate_data_interface_schema",
]
