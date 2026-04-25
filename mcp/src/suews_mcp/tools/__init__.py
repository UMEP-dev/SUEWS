"""Phase-1 read-only tools.

Each tool is a plain function that returns the standard SUEWS JSON envelope
``{status, data, errors, warnings, meta}``. They are registered with the
FastMCP server in :mod:`suews_mcp.server`. Importing this package does not
register them — registration is explicit so that tests can import individual
tool functions in isolation.
"""

from .examples import list_examples, read_example
from .inspect import inspect_config
from .schema_search import search_schema
from .validate import validate_config

__all__ = [
    "inspect_config",
    "list_examples",
    "read_example",
    "search_schema",
    "validate_config",
]
