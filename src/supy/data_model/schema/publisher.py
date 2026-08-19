"""Compatibility wrapper for configuration-schema publishing."""

from ..configuration.publisher import (
    create_schema_bundle,
    generate_json_schema,
    main,
    save_schema,
    validate_config_against_schema,
)

__all__ = [
    "create_schema_bundle",
    "generate_json_schema",
    "main",
    "save_schema",
    "validate_config_against_schema",
]


if __name__ == "__main__":
    main()
