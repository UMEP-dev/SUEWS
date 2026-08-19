"""Compatibility wrapper for configuration-schema version management."""

from ..configuration.version import (
    CURRENT_SCHEMA_VERSION,
    SCHEMA_VERSIONS,
    get_schema_compatibility_message,
    is_schema_compatible,
    validate_schema_version,
)

__all__ = [
    "CURRENT_SCHEMA_VERSION",
    "SCHEMA_VERSIONS",
    "get_schema_compatibility_message",
    "is_schema_compatible",
    "validate_schema_version",
]
