"""SUEWS MCP tools.

Each tool is a plain function that returns the standard SUEWS JSON envelope
``{status, data, errors, warnings, meta}``. They are registered with the
FastMCP server in :mod:`suews_mcp.server`. Importing this package does not
register them — registration is explicit so that tests can import individual
tool functions in isolation.
"""

from .compare import compare_runs
from .convert import convert_config
from .diagnose import diagnose_run
from .examples import list_examples, read_example
from .init_case import init_case
from .inspect import inspect_config
from .knowledge import query_knowledge, read_knowledge_manifest
from .schema_search import search_schema
from .summarise import summarise_run
from .validate import validate_config

__all__ = [
    "compare_runs",
    "convert_config",
    "diagnose_run",
    "init_case",
    "inspect_config",
    "list_examples",
    "query_knowledge",
    "read_example",
    "read_knowledge_manifest",
    "search_schema",
    "summarise_run",
    "validate_config",
]
