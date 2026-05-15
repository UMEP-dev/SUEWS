"""FastMCP server entry point.

Registers the read-only tools and resources, then runs over stdio. The
``mcp`` SDK is imported lazily so this module remains importable for unit
testing the tool functions even when the SDK is not installed.
"""

from __future__ import annotations

import json
import sys  # noqa: F401  (kept for future use; FastMCP.run() handles streams)
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
        read_knowledge_manifest_resource,
        read_knowledge_query_resource,
        read_run_resource,
        read_schema_resource,
    )
    from .tools import (
        compare_runs,
        convert_config,
        diagnose_run,
        init_case,
        inspect_config,
        list_examples,
        query_knowledge,
        read_example,
        read_knowledge_manifest,
        search_schema,
        summarise_run,
        validate_config,
    )

    server = FastMCP("suews-mcp")

    # Tools — config & schema (read-only)
    server.tool(name="validate_config")(validate_config)
    server.tool(name="inspect_config")(inspect_config)
    server.tool(name="search_schema")(search_schema)
    server.tool(name="list_examples")(list_examples)
    server.tool(name="read_example")(read_example)

    # Tools — workflow (init / convert)
    server.tool(name="init_case")(init_case)
    server.tool(name="convert_config")(convert_config)

    # Tools — post-run (summarise / compare / diagnose)
    server.tool(name="summarise_run")(summarise_run)
    server.tool(name="compare_runs")(compare_runs)
    server.tool(name="diagnose_run")(diagnose_run)

    # Tools — versioned knowledge pack
    server.tool(name="query_knowledge")(query_knowledge)
    server.tool(name="read_knowledge_manifest")(read_knowledge_manifest)

    # Resources (URI templates).
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

    @server.resource("suews://knowledge/manifest")
    def _knowledge_manifest() -> str:
        return json.dumps(read_knowledge_manifest_resource())

    @server.resource("suews://knowledge/query/{question}")
    def _knowledge_query(question: str) -> str:
        return json.dumps(read_knowledge_query_resource(question))

    return server


def main() -> None:
    """Console-script entry point: ``suews-mcp``."""
    server = _build_server()
    server.run()


if __name__ == "__main__":  # pragma: no cover
    main()
