"""SUEWS MCP server package.

A thin Model Context Protocol bridge between AI clients and the unified
``suews`` CLI. The server never reimplements physics, validation, schema,
or run logic — it delegates to ``suews <subcommand>`` over a fixed
allow-list and returns the standard JSON envelope.
"""

__version__ = "0.1.0"
