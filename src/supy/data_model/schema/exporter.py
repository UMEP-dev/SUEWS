"""Compatibility wrapper for configuration-schema export."""

from ..configuration.exporter import BASE_URL, export_schema, main

__all__ = ["BASE_URL", "export_schema", "main"]


if __name__ == "__main__":
    main()
