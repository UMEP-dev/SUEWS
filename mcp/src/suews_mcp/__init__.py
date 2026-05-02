"""SUEWS MCP server package.

A thin Model Context Protocol bridge between AI clients and the unified
``suews`` CLI. The server never reimplements physics, validation, schema,
or run logic — it delegates to ``suews <subcommand>`` over a fixed
allow-list and returns the standard JSON envelope.
"""

from ._version import __version__, __version_tuple__, version, version_tuple

__all__ = ["__version__", "__version_tuple__", "version", "version_tuple"]
