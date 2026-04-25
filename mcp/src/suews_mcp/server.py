"""FastMCP server entry point for SUEWS tools."""

from __future__ import annotations

import logging
import sys
from typing import Any

from mcp.server.fastmcp import FastMCP

from .backend.base import SUEWSBackend
from .backend.local import LocalBackend
from .tools.execute import handle_execute
from .tools.explain import handle_explain
from .tools.search import handle_search

logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s %(levelname)s %(name)s: %(message)s",
    stream=sys.stderr,
)


def create_server(backend: SUEWSBackend | None = None) -> FastMCP:
    """Create and configure the FastMCP app instance."""
    backend = backend or LocalBackend()
    app = FastMCP(
        name="suews",
        instructions=(
            "Use search() to inspect SUEWS types, forcing requirements, sample data, "
            "and external data sources. Use execute() to run simulations from YAML. "
            "Use explain() for physics concepts, source code, and diagnostic guidance."
        ),
    )

    @app.tool(
        name="search",
        description=(
            "Search SUEWS types, forcing requirements, sample data, and external sources. "
            "Without type_name, searches the type index and data catalogue together. "
            "With type_name, returns detailed schema for that type."
        ),
    )
    async def search(
        query: str,
        type_name: str | None = None,
        detail_level: str = "index",
    ) -> dict:
        return await handle_search(
            backend=backend,
            query=query,
            type_name=type_name,
            detail_level=detail_level,
        )

    @app.tool(
        name="execute",
        description=(
            "Run a SUEWS simulation from YAML config text and return summary metrics."
        ),
    )
    async def execute(config_yaml: str, description: str | None = None) -> dict:
        return await handle_execute(
            backend=backend,
            config_yaml=config_yaml,
            description=description,
        )

    @app.tool(
        name="explain",
        description=(
            "Explain SUEWS concepts, physics, or source code. "
            "Pass context={} with simulation output metrics for diagnostics. "
            "Use show_source=true for Fortran source excerpts."
        ),
    )
    async def explain(
        topic: str,
        show_source: bool = False,
        context: dict[str, Any] | None = None,
    ) -> dict:
        return await handle_explain(
            backend=backend,
            topic=topic,
            show_source=show_source,
            context=context,
        )

    return app


APP = create_server()


def main() -> None:
    """Run MCP server with stdio transport."""
    APP.run(transport="stdio")


if __name__ == "__main__":
    main()
