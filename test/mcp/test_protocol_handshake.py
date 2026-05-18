"""Layer-1 MCP protocol-level E2E test.

Spawns ``suews-mcp`` as a subprocess and exercises the JSON-RPC stdio
contract a real client (Claude Code, Codex) speaks. The other tests in
this directory import the tool functions directly and bypass FastMCP
entirely; this file is the only path that proves clients can actually
discover and invoke the tools through the protocol.

If ``suews-mcp`` is not installed (fresh venv, no editable install),
the test is skipped with a clear reason rather than failing — fixing
the install is a separate concern owned by ``test_version.py`` and the
``mcp/pyproject.toml`` dynamic-version setup.
"""

from __future__ import annotations

import asyncio
import shutil
from pathlib import Path

import pytest

pytestmark = pytest.mark.api


REPO_ROOT = Path(__file__).resolve().parents[2]


pytest.importorskip(
    "mcp",
    reason="The 'mcp' SDK is not installed. Install with `uv pip install -e mcp/` or `pip install mcp>=1.2`.",
)


def _suews_mcp_resolvable() -> bool:
    return shutil.which("suews-mcp") is not None


pytestmark_skipif = pytest.mark.skipif(
    not _suews_mcp_resolvable(),
    reason=(
        "`suews-mcp` is not on PATH. Run `uv pip install -e mcp/` (or "
        "`pip install -e mcp/`) from the repo root before running this "
        "test."
    ),
)


# Expected surface — keep in sync with `mcp/src/suews_mcp/server.py`.
EXPECTED_TOOLS = frozenset({
    "validate_config",
    "inspect_config",
    "search_schema",
    "list_examples",
    "read_example",
    "init_case",
    "convert_config",
    "summarise_run",
    "compare_runs",
    "diagnose_run",
    "query_knowledge",
    "read_knowledge_manifest",
})

# Resources with `{var}` placeholders surface as URI templates in MCP;
# resources with no placeholders surface as static resources. FastMCP
# splits the registration accordingly, so the test queries both endpoints
# and asserts the union matches.
EXPECTED_RESOURCE_TEMPLATES = frozenset({
    "suews://schema/{version}",
    "suews://examples/{name}",
    "suews://docs/{slug}",
    "suews://runs/{run_id}/{kind}",
    "suews://knowledge/query/{question}",
})

EXPECTED_STATIC_RESOURCES = frozenset({
    "suews://knowledge/manifest",
})


async def _run_handshake() -> dict:
    """Spawn `suews-mcp`, do the JSON-RPC handshake, return what we discovered."""
    from mcp import ClientSession
    from mcp.client.stdio import StdioServerParameters, stdio_client

    server_params = StdioServerParameters(
        command="suews-mcp",
        env={"SUEWS_MCP_PROJECT_ROOT": str(REPO_ROOT)},
    )

    async with stdio_client(server_params) as (read, write):
        async with ClientSession(read, write) as session:
            init_result = await session.initialize()

            tools_result = await session.list_tools()
            resource_templates_result = await session.list_resource_templates()
            static_resources_result = await session.list_resources()

            manifest_result = await session.call_tool(
                "read_knowledge_manifest", arguments={}
            )

            return {
                "server_name": getattr(
                    init_result.serverInfo, "name", None
                ),
                "tool_names": frozenset(t.name for t in tools_result.tools),
                "resource_templates": frozenset(
                    str(r.uriTemplate)
                    for r in resource_templates_result.resourceTemplates
                ),
                "static_resources": frozenset(
                    str(r.uri) for r in static_resources_result.resources
                ),
                "manifest_envelope": manifest_result,
            }


@pytestmark_skipif
def test_initialize_advertises_suews_mcp() -> None:
    """Server identifies itself as `suews-mcp` after the JSON-RPC handshake."""
    result = asyncio.run(_run_handshake())
    assert result["server_name"] == "suews-mcp", (
        f"Expected serverInfo.name == 'suews-mcp', got {result['server_name']!r}. "
        "Check FastMCP server name in mcp/src/suews_mcp/server.py."
    )


@pytestmark_skipif
def test_tools_list_advertises_all_twelve() -> None:
    """All 12 tools registered in `server.py` are advertised through MCP."""
    result = asyncio.run(_run_handshake())
    advertised = result["tool_names"]

    missing = EXPECTED_TOOLS - advertised
    extra = advertised - EXPECTED_TOOLS

    assert not missing, (
        f"Tools missing from MCP advertisement: {sorted(missing)}. "
        "Check `server.tool(...)` registrations in server.py."
    )
    assert not extra, (
        f"Unexpected tools advertised: {sorted(extra)}. Update "
        "EXPECTED_TOOLS in this test if a new tool was added intentionally."
    )


@pytestmark_skipif
def test_resources_advertise_all_six() -> None:
    """All 6 resources registered in `server.py` surface through MCP.

    FastMCP routes URI patterns with `{var}` placeholders to
    `resources/templates/list` and patterns without placeholders to
    `resources/list`. The total advertised across both endpoints must
    equal the registered set.
    """
    result = asyncio.run(_run_handshake())
    expected_all = EXPECTED_RESOURCE_TEMPLATES | EXPECTED_STATIC_RESOURCES
    advertised_all = result["resource_templates"] | result["static_resources"]

    missing = expected_all - advertised_all
    extra = advertised_all - expected_all

    assert not missing, (
        f"Resources missing from MCP advertisement: {sorted(missing)}. "
        "Check `@server.resource(...)` decorators in server.py."
    )
    assert not extra, (
        f"Unexpected resources advertised: {sorted(extra)}. Update "
        "EXPECTED_RESOURCE_TEMPLATES / EXPECTED_STATIC_RESOURCES in this "
        "test if added intentionally."
    )


async def _run_serial_calls(
    sequence: list[tuple[str, dict]],
    per_call_timeout: float,
) -> list:
    """Spawn `suews-mcp` and issue ``sequence`` of ``tools/call``s on the
    same session, asserting every call returns inside ``per_call_timeout``
    seconds. Returns the parsed envelopes.

    The bug fixed in gh#1412 is that a sync ``subprocess.run`` inside a
    tool wrapper blocks the FastMCP asyncio event loop, so the second
    call in a single session never returns within the host's 60 s
    deadline. ``asyncio.wait_for`` per call is the precise regression
    guard: under the bug the second ``call_tool`` future never
    completes.
    """
    from mcp import ClientSession
    from mcp.client.stdio import StdioServerParameters, stdio_client

    server_params = StdioServerParameters(
        command="suews-mcp",
        env={"SUEWS_MCP_PROJECT_ROOT": str(REPO_ROOT)},
    )

    async with stdio_client(server_params) as (read, write):
        async with ClientSession(read, write) as session:
            await session.initialize()
            envelopes = []
            for tool_name, arguments in sequence:
                envelope = await asyncio.wait_for(
                    session.call_tool(tool_name, arguments=arguments),
                    timeout=per_call_timeout,
                )
                envelopes.append(envelope)
            return envelopes


@pytestmark_skipif
def test_serial_query_knowledge_does_not_block_event_loop() -> None:
    """Two consecutive ``query_knowledge`` calls in one MCP session both
    return inside a generous per-call deadline.

    Regression guard for gh#1412: under the bug the second call never
    returned within 60 s because the synchronous ``subprocess.run`` in
    the first call's tool wrapper held the FastMCP event loop. The fix
    wraps every tool registration with ``_async_offload`` (in
    ``server.py``) which routes the blocking body through
    ``anyio.to_thread.run_sync``.

    The per-call timeout is intentionally generous (90 s) to absorb the
    real CLI cost of loading the knowledge pack on a slow CI host;
    under the bug the call would time out regardless of how patient
    the budget was.
    """
    envelopes = asyncio.run(
        _run_serial_calls(
            sequence=[
                (
                    "query_knowledge",
                    {
                        "question": "compare model output to air temperature observations",
                        "limit": 3,
                    },
                ),
                (
                    "query_knowledge",
                    {
                        "question": "site characterisation parameters land cover",
                        "limit": 3,
                    },
                ),
            ],
            per_call_timeout=90.0,
        )
    )
    assert len(envelopes) == 2, (
        "Expected two envelopes from the serial-call sequence; got "
        f"{len(envelopes)}. If the second call timed out, gh#1412 has "
        "regressed."
    )
    for idx, envelope in enumerate(envelopes):
        assert envelope.content, (
            f"Serial call {idx + 1}/2 returned without content; "
            "FastMCP dispatch likely failed."
        )


@pytestmark_skipif
def test_interleaved_query_knowledge_and_search_schema() -> None:
    """``query_knowledge`` followed by a different CLI-backed tool in the
    same session also returns on time.

    The gh#1412 report listed ``search_schema``, ``read_knowledge_manifest``
    and ``list_examples`` as showing the same second-call delay when
    issued after a ``query_knowledge``. This test pins the variant the
    report flagged most prominently: ``query_knowledge`` then
    ``search_schema``.
    """
    envelopes = asyncio.run(
        _run_serial_calls(
            sequence=[
                (
                    "query_knowledge",
                    {"question": "STEBBS heating demand", "limit": 2},
                ),
                (
                    "search_schema",
                    {"query": "sfr"},
                ),
            ],
            per_call_timeout=90.0,
        )
    )
    assert len(envelopes) == 2
    for idx, envelope in enumerate(envelopes):
        assert envelope.content, (
            f"Interleaved call {idx + 1}/2 returned without content; "
            "FastMCP dispatch likely failed."
        )


@pytestmark_skipif
def test_read_knowledge_manifest_returns_provenance() -> None:
    """Calling `read_knowledge_manifest` over MCP returns pack provenance.

    The Layer-3 evidence contract requires `pack_version`, `schema_version`,
    and `git_sha` so that downstream answers can cite the exact revision.
    A manifest call that returns success but lacks any of these breaks
    every downstream cited-evidence assertion.
    """
    import json

    result = asyncio.run(_run_handshake())
    envelope = result["manifest_envelope"]

    # The MCP SDK wraps the tool's return value in `content[].text` for
    # JSON-serialisable returns. Parse the first content block.
    assert envelope.content, "Manifest call returned empty content."
    text = envelope.content[0].text
    payload = json.loads(text)

    assert payload.get("status") == "success", (
        f"Manifest envelope status != 'success': {payload.get('status')!r}, "
        f"errors={payload.get('errors')}"
    )

    data = payload.get("data") or {}
    manifest = data.get("manifest") or data
    for required in ("pack_version", "schema_version", "git_sha"):
        assert required in manifest and manifest[required], (
            f"Manifest missing required provenance field {required!r}. "
            f"Got keys: {sorted(manifest.keys())}"
        )
