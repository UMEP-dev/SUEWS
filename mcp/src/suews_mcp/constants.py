"""Shared constants for the SUEWS MCP server.

Single source of truth for the CLI allow-list, URI prefixes, and resource
limits. Importing this module must never trigger heavy work.
"""

from __future__ import annotations

import os

#: Subcommands the MCP server is permitted to invoke via ``backend.cli``.
#: Anything not in this set raises ``SUEWSMCPError`` at construction time.
ALLOWED_SUBCOMMANDS: frozenset[str] = frozenset({
    "validate",
    "schema",
    "convert",
    "run",
    "inspect",
    "diagnose",
    "summarise",
    "compare",
    "init",
})

#: URI scheme for MCP resources exposed by this server.
URI_SCHEME = "suews://"

URI_SCHEMA_PREFIX = "suews://schema/"
URI_EXAMPLES_PREFIX = "suews://examples/"
URI_DOCS_PREFIX = "suews://docs/"
URI_RUNS_PREFIX = "suews://runs/"

#: Default subprocess timeout (seconds). Overridable via env var.
DEFAULT_TIMEOUT_SECONDS = int(os.environ.get("SUEWS_MCP_TIMEOUT_SECONDS", "600"))

#: Maximum bytes a tool may return as JSON payload. Larger payloads are
#: truncated and an explicit warning is added to the envelope.
MAX_JSON_RESPONSE_BYTES = int(
    os.environ.get("SUEWS_MCP_MAX_JSON_BYTES", str(25 * 1024 * 1024))
)

#: Environment variable that names the per-session project root.
ENV_PROJECT_ROOT = "SUEWS_MCP_PROJECT_ROOT"
