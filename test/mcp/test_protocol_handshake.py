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


async def _run_baseline_then_concurrent(
    baseline_call: tuple[str, dict],
    sequence: list[tuple[str, dict]],
    per_task_timeout_factor: float,
) -> tuple[float, list, float]:
    """Spawn `suews-mcp` and (a) run a single warm-up tool call to capture a
    per-host baseline cost, then (b) issue ``sequence`` of ``tools/call``s
    **concurrently** via ``asyncio.gather`` on the *same* session.

    Returns ``(baseline_seconds, envelopes, concurrent_seconds)``. The caller
    asserts ``concurrent_seconds`` is bounded by a multiple of
    ``baseline_seconds`` rather than against a hard-coded constant.

    Why baseline-relative. The gh#1412 bug is that the synchronous
    ``subprocess.run`` inside a tool wrapper blocks the FastMCP asyncio event
    loop, so the second request on one session cannot be read while subprocess
    1 is running. With **sequential** awaits the loop is idle between calls
    and the bug does not manifest. With **concurrent** in-flight requests
    (the real plugin-host shape) the two CLI calls either overlap on worker
    threads (~``max(t1,t2)``, fix in place) or serialise on the event loop
    (~``t1+t2``, bug present). A hard-coded wall-clock budget (e.g. 18 s) sits
    in the false-negative zone whenever the CLI is fast enough that
    ``t1+t2 < budget``. Comparing against the same-session baseline removes
    that zone entirely: the threshold scales with whatever the host's actual
    CLI cost happens to be. ``per_task_timeout_factor`` derives the per-task
    ``asyncio.wait_for`` deadline from the same baseline so the timeout is
    self-calibrating too.
    """
    import time

    from mcp import ClientSession
    from mcp.client.stdio import StdioServerParameters, stdio_client

    server_params = StdioServerParameters(
        command="suews-mcp",
        env={"SUEWS_MCP_PROJECT_ROOT": str(REPO_ROOT)},
    )

    async with stdio_client(server_params) as (read, write):
        async with ClientSession(read, write) as session:
            await session.initialize()

            # 1. Warm-up + baseline measurement. This call also primes any
            # one-shot startup cost (knowledge-pack chunk load) so the
            # concurrent gather below measures steady-state behaviour.
            baseline_name, baseline_args = baseline_call
            t_b = time.monotonic()
            await session.call_tool(baseline_name, arguments=baseline_args)
            baseline_seconds = time.monotonic() - t_b
            # Floor the baseline to avoid pathological tiny values producing
            # a sub-second `per_task_timeout` that would itself flake. 1 s is
            # well below any realistic CLI cost on this surface.
            baseline_seconds = max(baseline_seconds, 1.0)

            per_task_timeout = baseline_seconds * per_task_timeout_factor

            # 2. Concurrent gather. Under the bug the second task hits its
            # baseline-derived deadline because subprocess 1 still holds
            # the loop; under the fix both threads overlap.
            t0 = time.monotonic()
            envelopes = await asyncio.gather(
                *(
                    asyncio.wait_for(
                        session.call_tool(name, arguments=args),
                        timeout=per_task_timeout,
                    )
                    for name, args in sequence
                )
            )
            concurrent_seconds = time.monotonic() - t0
            return baseline_seconds, list(envelopes), concurrent_seconds


# Wall-clock factor for the concurrent gather, relative to a single-call
# baseline captured in the same session. With the fix in place the gather
# resolves at ``max(t1, t2)`` ≈ baseline; without the fix it serialises to
# ``~2 * baseline``. 1.5 sits cleanly between the two — generous enough that
# normal jitter does not flake, tight enough that any return to serialisation
# fails the assertion.
_CONCURRENT_WALLCLOCK_FACTOR = 1.5
# Per-task ``asyncio.wait_for`` factor: under the bug the second task takes
# ~``2 * baseline``; this catches it well before the gather budget.
_PER_TASK_TIMEOUT_FACTOR = 1.8


@pytest.mark.slow
@pytestmark_skipif
def test_concurrent_query_knowledge_does_not_block_event_loop() -> None:
    """Two ``query_knowledge`` calls issued concurrently via
    ``asyncio.gather`` on one MCP session both complete inside a tight
    per-task and total wall-clock budget.

    Regression guard for gh#1412. Sequential ``await`` x 2 masks the
    bug because the event loop is idle between calls;
    ``asyncio.gather`` replays the real plugin-host shape where two
    requests are in-flight on the same stdio session. Under the bug
    the loop cannot read the second request from stdin while
    subprocess 1 is running, so the two CLI calls serialise (and may
    deadlock under enough pipe pressure). With the fix
    (``_async_offload`` + ``anyio.to_thread.run_sync``) both
    subprocesses overlap and the gather resolves at ``max(t1, t2)``.
    """
    baseline, envelopes, elapsed = asyncio.run(
        _run_baseline_then_concurrent(
            baseline_call=(
                "query_knowledge",
                {"question": "warm-up baseline call", "limit": 1},
            ),
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
            per_task_timeout_factor=_PER_TASK_TIMEOUT_FACTOR,
        )
    )
    assert len(envelopes) == 2, (
        "Expected two envelopes from the concurrent gather; got "
        f"{len(envelopes)}. If a task timed out, gh#1412 has regressed."
    )
    for idx, envelope in enumerate(envelopes):
        assert envelope.content, (
            f"Concurrent call {idx + 1}/2 returned without content; "
            "FastMCP dispatch likely failed."
        )
    budget = baseline * _CONCURRENT_WALLCLOCK_FACTOR
    assert elapsed < budget, (
        f"Two concurrent query_knowledge calls took {elapsed:.1f}s "
        f"wall-clock; expected <{budget:.1f}s "
        f"(= {_CONCURRENT_WALLCLOCK_FACTOR}× a {baseline:.1f}s baseline "
        "captured in the same session). The worker-thread offload has "
        "regressed: calls are serialising on the event loop instead of "
        "overlapping on threads."
    )


@pytest.mark.slow
@pytestmark_skipif
def test_concurrent_query_knowledge_and_search_schema() -> None:
    """``query_knowledge`` concurrent with ``search_schema`` on one MCP
    session — the cross-tool variant the gh#1412 report listed
    explicitly (``query_knowledge`` then ``search_schema`` /
    ``read_knowledge_manifest`` / ``list_examples`` all showed the
    same second-call delay).
    """
    baseline, envelopes, elapsed = asyncio.run(
        _run_baseline_then_concurrent(
            baseline_call=(
                "query_knowledge",
                {"question": "warm-up baseline call", "limit": 1},
            ),
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
            per_task_timeout_factor=_PER_TASK_TIMEOUT_FACTOR,
        )
    )
    assert len(envelopes) == 2
    for idx, envelope in enumerate(envelopes):
        assert envelope.content, (
            f"Concurrent cross-tool call {idx + 1}/2 returned without "
            "content; FastMCP dispatch likely failed."
        )
    budget = baseline * _CONCURRENT_WALLCLOCK_FACTOR
    assert elapsed < budget, (
        f"query_knowledge + search_schema in concurrent gather took "
        f"{elapsed:.1f}s wall-clock; expected <{budget:.1f}s "
        f"(= {_CONCURRENT_WALLCLOCK_FACTOR}× a {baseline:.1f}s baseline "
        "captured in the same session)."
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
