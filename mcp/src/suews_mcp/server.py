"""FastMCP server entry point.

Registers the Phase-1 read-only tools and resources, then runs over stdio.
The ``mcp`` SDK is imported lazily so this module remains importable for
unit testing the tool functions even when the SDK is not installed.
"""

from __future__ import annotations

import json
import sys
from typing import Any


def _build_server() -> Any:
    """Build and return a configured FastMCP server.

    Importing the ``mcp`` SDK at function scope means this module loads even
    in environments where the SDK is unavailable (CI for the tools/resources
    layer, for example).
    """
    try:
        from mcp.server.fastmcp import FastMCP
    except ImportError as exc:  # pragma: no cover - exercised manually
        raise SystemExit(
            "The 'mcp' SDK is not installed. Install with: "
            "pip install 'suews-mcp[dev]' or pip install mcp"
        ) from exc

    from .resources import (
        read_doc,
        read_example_resource,
        read_run_resource,
        read_schema_resource,
    )
    from .tools import (
        inspect_config,
        list_examples,
        read_example,
        search_schema,
        validate_config,
    )

    server = FastMCP("suews-mcp")

    # Tools
    server.tool(name="validate_config")(validate_config)
    server.tool(name="inspect_config")(inspect_config)
    server.tool(name="search_schema")(search_schema)
    server.tool(name="list_examples")(list_examples)
    server.tool(name="read_example")(read_example)

    # Resources (URI templates). FastMCP uses a function-with-URI decorator.
    @server.resource("suews://schema/{version}")
    def _schema(version: str = "current") -> str:
        return json.dumps(read_schema_resource(version))

    @server.resource("suews://examples/{name}")
    def _examples(name: str) -> str:
        return json.dumps(read_example_resource(name))

    @server.resource("suews://docs/{slug}")
    def _docs(slug: str) -> str:
        return json.dumps(read_doc(slug))

    @server.resource("suews://runs/{run_id}/{kind}")
    def _runs(run_id: str, kind: str) -> str:
        return json.dumps(read_run_resource(run_id, kind))

    return server


def main() -> None:
    """Console-script entry point: ``suews-mcp``."""
    server = _build_server()
    server.run()


if __name__ == "__main__":  # pragma: no cover
    main()
