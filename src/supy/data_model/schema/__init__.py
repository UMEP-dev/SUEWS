"""Compatibility namespace for configuration-schema management.

The canonical implementation lives in :mod:`supy.data_model.configuration`.
This namespace remains supported so existing integrations keep working.
"""

from ..configuration import (
    CURRENT_SCHEMA_VERSION,
    SCHEMA_VERSIONS,
    SchemaMigrator,
    create_schema_bundle,
    export_schema,
    generate_json_schema,
    get_schema_compatibility_message,
    increment_schema_version,
    is_schema_compatible,
    migrate_config_file,
    save_schema,
    update_yaml_schema_version,
    validate_config_against_schema,
    validate_schema_version,
)

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
