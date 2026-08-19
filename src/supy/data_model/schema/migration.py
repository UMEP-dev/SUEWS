"""Compatibility wrapper for configuration-schema migrations."""

from ..configuration.migration import (
    SchemaMigrator,
    check_migration_needed,
    migrate_config_file,
)

__all__ = [
    "SchemaMigrator",
    "check_migration_needed",
    "migrate_config_file",
]
