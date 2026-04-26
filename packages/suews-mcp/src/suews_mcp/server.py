"""FastMCP server construction for SUEWS."""

from __future__ import annotations

import logging
from pathlib import Path
from typing import Any

from . import prompts, resources, tools


def _make_fastmcp():
    """Create a FastMCP instance using the installed MCP SDK."""
    from mcp.server.fastmcp import FastMCP  # noqa: PLC0415

    try:
        return FastMCP("SUEWS", json_response=True)
    except TypeError:
        return FastMCP("SUEWS")


def create_server(root: Path, logger: logging.Logger | None = None):
    """Create and register the SUEWS MCP server."""
    path_root = tools.resolve_root(root)
    log = logger or logging.getLogger("suews_mcp")
    mcp = _make_fastmcp()

    @mcp.resource("suews://schema/current")
    def schema_current() -> str:
        """Return the current SUEWS YAML schema."""
        return resources.schema_json("current")

    @mcp.resource("suews://schema/{version}")
    def schema_version(version: str) -> str:
        """Return a specific SUEWS YAML schema version."""
        return resources.schema_json(version)

    @mcp.resource("suews://examples/sample-config")
    def example_sample_config() -> str:
        """Return the packaged SUEWS sample configuration."""
        return resources.sample_config()

    @mcp.resource("suews://docs/yaml-config")
    def docs_yaml_config() -> str:
        """Return concise YAML configuration guidance."""
        return resources.yaml_config_docs()

    @mcp.resource("suews://docs/forcing-data")
    def docs_forcing_data() -> str:
        """Return concise forcing-data guidance."""
        return resources.forcing_data_docs()

    @mcp.resource("suews://cli/help/{command}")
    def cli_help(command: str) -> str:
        """Return allowlisted SUEWS CLI help text."""
        return resources.cli_help(command, path_root)

    @mcp.tool(structured_output=True)
    def validate_config(
        config_path: str,
        schema_version: str | None = None,
        timeout_s: float = tools.DEFAULT_TIMEOUT_S,
    ) -> dict[str, Any]:
        """Validate a SUEWS YAML configuration through the SUEWS CLI."""
        return tools.validate_config(
            path_root,
            config_path,
            schema_version=schema_version,
            timeout_s=timeout_s,
            logger=log,
        )

    for name in prompts.PROMPTS:
        prompt_text = prompts.get_prompt(name)

        def prompt_factory(text: str):
            def workflow_prompt() -> str:
                return text

            return workflow_prompt

        prompt_func = prompt_factory(prompt_text)
        prompt_func.__name__ = name
        prompt_func.__doc__ = f"Return the {name} SUEWS workflow prompt."
        mcp.prompt()(prompt_func)

    return mcp
