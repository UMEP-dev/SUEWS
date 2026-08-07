"""
SUEWS Configuration Schema Management Module

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

__all__ = [
    "CURRENT_SCHEMA_VERSION",
    "SCHEMA_VERSIONS",
    "SchemaMigrator",
    "create_schema_bundle",
    "export_schema",
    "generate_json_schema",
    "get_schema_compatibility_message",
    "increment_schema_version",
    "is_schema_compatible",
    "migrate_config_file",
    "save_schema",
    "update_yaml_schema_version",
    "validate_config_against_schema",
    "validate_schema_version",
]
