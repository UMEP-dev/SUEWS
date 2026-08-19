"""Compatibility wrapper for configuration-schema update utilities."""

import sys

from ..configuration.updater import (
    find_yaml_configs,
    increment_schema_version,
    main,
    update_yaml_schema_version,
)

__all__ = [
    "find_yaml_configs",
    "increment_schema_version",
    "main",
    "update_yaml_schema_version",
]


if __name__ == "__main__":
    sys.exit(main())
