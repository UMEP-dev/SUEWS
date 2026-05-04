"""FastMCP server entry point.

Registers the read-only tools and resources, then runs over stdio. The
``mcp`` SDK is imported lazily so this module remains importable for unit
testing the tool functions even when the SDK is not installed.
"""

from __future__ import annotations

import argparse
import json
import os
import sys  # noqa: F401  (kept for future use; FastMCP.run() handles streams)
from pathlib import Path
from typing import Any, Optional, Sequence

from .constants import ENV_PROJECT_ROOT


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

    # Plumb the package version into the MCP `initialize` handshake so
    # `serverInfo.version` agrees with `pip show suews-mcp` (gh#1401).
    # FastMCP's constructor does not accept a version directly, but the
    # underlying lowlevel `Server` carries a settable `version` field
    # which `initialize` reads. Without this override the SDK reports
    # its own library version (e.g. "1.27.0"), weakening the
    # provenance story for clients that log `serverInfo.version`.
    from . import __version__ as _suews_mcp_version

    server._mcp_server.version = _suews_mcp_version

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


def _check_knowledge_pack_freshness() -> Optional[str]:
    """Return a staleness warning for the installed knowledge pack, or
    ``None`` when the pack is fresh / unreadable (gh#1406).

    Compares the pack manifest's ``suews_version`` against the running
    ``supy.__version__``. A mismatch means the pack was built at a
    different revision — typically because ``data_model/`` or
    ``cmd/`` changed without a meson dep refresh, or because the user
    is on an older wheel than the installed supy. Returns ``None``
    when supy or the pack is unavailable so a degraded environment
    does not crash server startup.
    """
    try:
        from supy import __version__ as supy_version  # type: ignore[import-not-found]
        from supy.knowledge import load_manifest  # type: ignore[import-not-found]
    except Exception:
        return None
    try:
        manifest = load_manifest()
    except Exception:
        return None
    pack_version = manifest.get("suews_version")
    pack_sha = manifest.get("git_sha")
    if not pack_version or pack_version == supy_version:
        return None
    return (
        f"[suews-mcp] knowledge pack staleness: pack built for supy "
        f"{pack_version} (git_sha {pack_sha[:10] if pack_sha else 'unknown'}) "
        f"but running supy is {supy_version}. `query_knowledge` may surface "
        f"chunks that no longer match the live schema; rebuild via "
        f"`suews knowledge build` before relying on it for user-facing "
        f"YAML answers."
    )


def _parse_args(argv: Optional[Sequence[str]] = None) -> argparse.Namespace:
    """Parse CLI args for the ``suews-mcp`` console script (gh#1405).

    The MCP server has long honoured ``SUEWS_MCP_PROJECT_ROOT`` but the
    plugin-host launch idiom is ``suews-mcp --root <path>``; without the
    flag the sandbox falls back to ``os.getcwd()``, which on a
    Conductor-isolated launch points at a temp directory rather than
    the workspace. The agent then sees its own absolute paths rejected
    with a confusing "outside the project root" message that names
    the temp dir.
    """
    parser = argparse.ArgumentParser(
        prog="suews-mcp",
        description=(
            "SUEWS Model Context Protocol server (read-only Phase-1)."
        ),
    )
    parser.add_argument(
        "--root",
        type=str,
        default=None,
        help=(
            "Project root directory. Tools resolve relative paths under "
            "this root, and absolute paths must lie inside it. Overrides "
            "the SUEWS_MCP_PROJECT_ROOT environment variable. Defaults "
            "to SUEWS_MCP_PROJECT_ROOT if set, otherwise the current "
            "working directory."
        ),
    )
    return parser.parse_args(list(argv) if argv is not None else None)


def main(argv: Optional[Sequence[str]] = None) -> None:
    """Console-script entry point: ``suews-mcp``."""
    args = _parse_args(argv)
    if args.root is not None:
        # Anchor the sandbox before any tool's ProjectRoot instance is
        # constructed. ProjectRoot reads the env var at instantiation,
        # so setting it here propagates to every subsequent tool call.
        resolved_root = str(Path(args.root).expanduser().resolve(strict=False))
        os.environ[ENV_PROJECT_ROOT] = resolved_root

    # Surface knowledge-pack staleness on stderr at startup (gh#1406).
    # MCP hosts route stderr to their plugin log so the user sees this
    # without it polluting the JSON-RPC stdio channel.
    warning = _check_knowledge_pack_freshness()
    if warning:
        sys.stderr.write(warning + "\n")

    server = _build_server()
    server.run()


if __name__ == "__main__":  # pragma: no cover
    main()
