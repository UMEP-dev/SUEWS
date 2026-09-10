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
from dataclasses import dataclass
from datetime import timedelta
from pathlib import Path
import shutil
import sys
import sysconfig
import time

import pytest

pytestmark = [pytest.mark.api, pytest.mark.smoke]


REPO_ROOT = Path(__file__).resolve().parents[2]
_HANDSHAKE_REQUEST_TIMEOUT = timedelta(seconds=30)
# Ceiling on any single request in the concurrency tests below. It sits well
# above the CLI cost on every CI host and well below pytest-timeout's 600 s,
# so a server that never answers fails here with an ``McpError`` naming the
# request instead of running the lane into its job timeout, which is how the
# scheduled Windows lane was lost for 20 nights (gh#1768).
_CONCURRENT_REQUEST_TIMEOUT = timedelta(seconds=120)


_mcp_session = pytest.importorskip(
    "mcp.client.session",
    reason=(
        "The MCP SDK is unavailable in the active Python environment. "
        "Install with `uv pip install --python .venv/bin/python -e mcp/`."
    ),
)
_mcp_stdio = pytest.importorskip(
    "mcp.client.stdio",
    reason=(
        "The MCP stdio client is unavailable in the active Python environment. "
        "Install with `uv pip install --python .venv/bin/python -e mcp/`."
    ),
)
ClientSession = _mcp_session.ClientSession
StdioServerParameters = _mcp_stdio.StdioServerParameters
stdio_client = _mcp_stdio.stdio_client


_ACTIVE_BIN_DIR = Path(sysconfig.get_path("scripts"))
_SUEWS_MCP_COMMAND = shutil.which("suews-mcp", path=str(_ACTIVE_BIN_DIR))


pytestmark_skipif = pytest.mark.skipif(
    _SUEWS_MCP_COMMAND is None,
    reason=(
        "`suews-mcp` is not installed in the active Python environment's "
        "scripts directory. "
        "Run `uv pip install --python .venv/bin/python -e mcp/` from the "
        "repo root before running this test."
    ),
)


# Expected surface — keep in sync with `mcp/src/suews_mcp/server.py`.
EXPECTED_TOOLS = frozenset({
    "validate_config",
    "inspect_config",
    "assess_readiness",
    "search_schema",
    "list_examples",
    "read_example",
    "list_docs",
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

# Prompts carry the procedural contract (honesty, energy-balance ladder,
# data sources) to every MCP client. Keep in sync with the
# `server.prompt(name=...)` registrations in `mcp/src/suews_mcp/server.py`.
EXPECTED_PROMPTS = frozenset({
    "fresh_site_setup",
    "parameter_importance",
    "evaluate_results",
})


async def _run_handshake() -> dict:
    """Spawn `suews-mcp`, do the JSON-RPC handshake, return what we discovered."""
    server_params = StdioServerParameters(
        command=_SUEWS_MCP_COMMAND,
        env={"SUEWS_MCP_PROJECT_ROOT": str(REPO_ROOT)},
    )

    async with stdio_client(server_params) as (read, write):
        async with ClientSession(
            read,
            write,
            read_timeout_seconds=_HANDSHAKE_REQUEST_TIMEOUT,
        ) as session:
            init_result = await session.initialize()

            tools_result = await session.list_tools()
            resource_templates_result = await session.list_resource_templates()
            static_resources_result = await session.list_resources()
            prompts_result = await session.list_prompts()

            manifest_result = await session.call_tool(
                "read_knowledge_manifest", arguments={}
            )

            return {
                "server_name": getattr(init_result.serverInfo, "name", None),
                "instructions": getattr(init_result, "instructions", None),
                "tool_names": frozenset(t.name for t in tools_result.tools),
                "resource_templates": frozenset(
                    str(r.uriTemplate)
                    for r in resource_templates_result.resourceTemplates
                ),
                "static_resources": frozenset(
                    str(r.uri) for r in static_resources_result.resources
                ),
                "prompt_names": frozenset(p.name for p in prompts_result.prompts),
                "manifest_envelope": manifest_result,
            }


@pytest.fixture(scope="module")
def handshake_result() -> dict:
    """Run protocol discovery once for the six immutable contract assertions."""
    return asyncio.run(_run_handshake())


@pytestmark_skipif
def test_initialize_advertises_suews_mcp(handshake_result: dict) -> None:
    """Server identifies itself as `suews-mcp` after the JSON-RPC handshake."""
    result = handshake_result
    assert result["server_name"] == "suews-mcp", (
        f"Expected serverInfo.name == 'suews-mcp', got {result['server_name']!r}. "
        "Check FastMCP server name in mcp/src/suews_mcp/server.py."
    )


@pytestmark_skipif
def test_tools_list_advertises_all_fourteen(handshake_result: dict) -> None:
    """All 14 tools registered in `server.py` are advertised through MCP."""
    result = handshake_result
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
def test_resources_advertise_all_six(handshake_result: dict) -> None:
    """All 6 resources registered in `server.py` surface through MCP.

    FastMCP routes URI patterns with `{var}` placeholders to
    `resources/templates/list` and patterns without placeholders to
    `resources/list`. The total advertised across both endpoints must
    equal the registered set.
    """
    result = handshake_result
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


@pytestmark_skipif
def test_initialize_advertises_instructions(handshake_result: dict) -> None:
    """The `initialize` result carries the server instructions so a client
    that reads only `serverInfo.instructions` still sees the contract.

    The instructions are the cross-client carrier of the honesty +
    energy-balance contract; if FastMCP stops forwarding `instructions=` the
    field comes back empty and Codex / Claude Desktop sessions lose it.
    """
    result = handshake_result
    instructions = result["instructions"]
    assert instructions, (
        "initialize result advertised no instructions. Check that "
        "`FastMCP(..., instructions=SERVER_INSTRUCTIONS)` is wired in "
        "server.py and that the SDK forwards it into InitializeResult."
    )
    assert "QN + QF = QS + QE + QH" in instructions, (
        "Server instructions lost the energy-balance contract. Check "
        "SERVER_INSTRUCTIONS in mcp/src/suews_mcp/prompts.py."
    )


@pytestmark_skipif
def test_prompts_advertise_all_three(handshake_result: dict) -> None:
    """All 3 prompts registered in `server.py` are advertised through MCP.

    The prompts carry the fresh-site / parameter-importance / evaluation
    procedures to non-Claude-Code clients; a dropped registration silently
    removes them from `prompts/list`.
    """
    result = handshake_result
    advertised = result["prompt_names"]

    missing = EXPECTED_PROMPTS - advertised
    extra = advertised - EXPECTED_PROMPTS

    assert not missing, (
        f"Prompts missing from MCP advertisement: {sorted(missing)}. "
        "Check `server.prompt(...)` registrations in server.py."
    )
    assert not extra, (
        f"Unexpected prompts advertised: {sorted(extra)}. Update "
        "EXPECTED_PROMPTS in this test if a new prompt was added intentionally."
    )


@dataclass(frozen=True)
class _ContrastResult:
    """Timings from one ``_run_serial_then_concurrent`` session.

    ``serial_each`` / ``concurrent_each`` hold one wall-clock figure per
    call in ``sequence`` order; the ``*_total`` fields are the wall time of
    the whole arm (the concurrent total is the ``asyncio.gather``).
    """

    serial_each: tuple[float, ...]
    serial_total: float
    concurrent_each: tuple[float, ...]
    concurrent_total: float
    envelopes: tuple


async def _timed_call(session, name: str, args: dict) -> tuple[object, float]:
    """Call one tool and return ``(envelope, seconds)`` for that call alone."""
    t0 = time.monotonic()
    envelope = await session.call_tool(name, arguments=args)
    return envelope, time.monotonic() - t0


async def _run_serial_then_concurrent(
    warm_up: list[tuple[str, dict]],
    sequence: list[tuple[str, dict]],
    per_task_timeout_factor: float,
) -> _ContrastResult:
    """Spawn ``suews-mcp`` once and, on that one session, (a) run the
    ``warm_up`` calls, (b) run ``sequence`` one call at a time (the serial
    arm), then (c) issue the same ``sequence`` concurrently through
    ``asyncio.gather`` (the concurrent arm).

    Why two arms in one session. The gh#1412 bug is that the synchronous
    ``subprocess.run`` inside a tool wrapper blocks the FastMCP asyncio event
    loop, so a second request on the same session is not read until the
    first subprocess has exited. Sequential awaits never show it (the loop is
    idle between calls); concurrent in-flight requests, the real plugin-host
    shape, either overlap on worker threads (fix in place) or serialise on
    the loop (bug present). What separates the two is the *contrast*
    between the serial and the concurrent arm, measured on the same host in
    the same session, read per call: a serialised dispatch answers the
    first concurrent call at about its serial cost and the next one a whole
    call later, an offloaded one answers them together. Both arms run under
    the same load, so contention that slows the machine slows them alike,
    where a fixed multiple of a single-call baseline (the previous shape)
    read pure CPU contention as a regression: on a 3-core runner shared
    with another test worker it measured 1.51x against a 1.5x budget with
    nothing regressed, and on an idle 96-core Linux machine the cross-tool
    variant failed its 1.8x per-task deadline on the healthy path.

    The warm-up primes one-shot costs (the knowledge-pack chunk load, the
    ``search_schema`` per-process cache) so both arms measure steady state.

    ``per_task_timeout_factor`` scales the serial-arm total into an
    ``asyncio.wait_for`` deadline on each concurrent task (floored, and
    capped at the request read-timeout). That deadline is a hang guard
    sitting inside the 120 s request read-timeout, not the detector: it
    sits well above both the bug shape (the slowest concurrent task costs
    about the serial total) and every healthy completion measured on a
    contended runner, so the contrast assertion, which names gh#1412 and
    prints the numbers, is what reports a regression.
    """
    server_params = StdioServerParameters(
        command=_SUEWS_MCP_COMMAND,
        env={"SUEWS_MCP_PROJECT_ROOT": str(REPO_ROOT)},
    )

    async with (
        stdio_client(server_params) as (read, write),
        ClientSession(
            read,
            write,
            read_timeout_seconds=_CONCURRENT_REQUEST_TIMEOUT,
        ) as session,
    ):
        await session.initialize()

        for name, args in warm_up:
            await session.call_tool(name, arguments=args)

        # Serial arm: the same calls, one at a time, on the same session.
        serial_each = []
        t_serial = time.monotonic()
        for name, args in sequence:
            _, seconds = await _timed_call(session, name, args)
            serial_each.append(seconds)
        serial_total = time.monotonic() - t_serial

        per_task_timeout = min(
            max(
                serial_total * per_task_timeout_factor,
                _PER_TASK_TIMEOUT_FLOOR_SECONDS,
            ),
            _CONCURRENT_REQUEST_TIMEOUT.total_seconds(),
        )

        # Concurrent arm: the same calls in flight together. gather()
        # schedules the tasks in ``sequence`` order, so the first call's
        # request is written to the server first; the tests rely on
        # that ordering when a fast probe follows a slow call.
        t_concurrent = time.monotonic()
        timed = await asyncio.gather(
            *(
                asyncio.wait_for(
                    _timed_call(session, name, args),
                    timeout=per_task_timeout,
                )
                for name, args in sequence
            )
        )
        concurrent_total = time.monotonic() - t_concurrent

    return _ContrastResult(
        serial_each=tuple(serial_each),
        serial_total=serial_total,
        concurrent_each=tuple(seconds for _, seconds in timed),
        concurrent_total=concurrent_total,
        envelopes=tuple(envelope for envelope, _ in timed),
    )


def _report_contrast(capsys, label: str, fields: dict[str, float]) -> None:
    """Write one greppable line of timings so a passing CI job records them.

    Goes to stderr with capture suspended: an xdist worker's stdout is the
    controller's transport and never reaches the log, its stderr does.
    """
    body = " ".join(
        f"{key}={value:.2f}" + ("" if key.startswith("ratio") else "s")
        for key, value in fields.items()
    )
    with capsys.disabled():
        print(f"\n[mcp-concurrency] {label}: {body}", file=sys.stderr, flush=True)


# Spread between the completion times of two concurrent calls of similar
# cost, as a fraction of the shorter serial call. A serialised dispatch
# answers the first call before it starts the second, so the second
# completes about one whole call after the first (measured about 1.0: with
# the offload disabled on a Linux machine, c1 = 10.79 s, c2 = 22.05 s
# against serial calls of 10.87 s and 10.73 s). An offloaded dispatch starts
# both at once and they complete together whether or not the machine has a
# spare core for the second (measured 0.00 to 0.39 s of spread, at most 0.14
# of a serial call, on sixteen CI lanes over two runs plus a Linux machine;
# the 0.39 s came from a lane where the two calls shared a contended core
# and each took three times its solo time). 0.5 sits between the two. Total
# wall time (C/S) is reported but not asserted: it separates the two
# dispatches only when a spare core exists, and read 1.04 and 1.60 on
# that lane in the two runs with the offload working.
_COMPLETION_SPREAD_RATIO = 0.5
# Delay a fast, primed probe suffers when issued behind a slow call, as a
# fraction of that slow call's concurrent-arm time. A blocked loop holds the
# probe's request for the whole slow call (about 1.0); a free loop answers it
# at once (about 0.0). 0.5 sits between the two.
_PROBE_DELAY_RATIO = 0.5
# Hang guard on each concurrent task, as a multiple of the serial-arm total
# with a floor, capped at the request read-timeout. Its only job is to beat
# the 120 s read-timeout on a server that stops answering, so it must sit
# clearly above every healthy completion measured, not near it: under a
# serialised dispatch the slowest task costs about the serial total, and on
# a contended CI lane with the offload working a healthy call completed at
# 9.02 s against a serial total of 5.63 s, which a 2 x S deadline (11.26 s)
# nearly caught. 4 x S clears that by a wide margin and the contrast
# assertion, not this deadline, is what reports gh#1412.
_PER_TASK_TIMEOUT_FACTOR = 4.0
_PER_TASK_TIMEOUT_FLOOR_SECONDS = 30.0


@pytest.mark.slow
@pytestmark_skipif
def test_concurrent_query_knowledge_does_not_block_event_loop(capsys) -> None:
    """Two ``query_knowledge`` calls issued concurrently via
    ``asyncio.gather`` on one MCP session complete together, rather than
    one whole call apart as they do when the dispatch serialises them.

    Regression guard for gh#1412. Sequential ``await`` x 2 masks the bug
    because the event loop is idle between calls; ``asyncio.gather``
    replays the real plugin-host shape where two requests are in-flight
    on the same stdio session. Under the bug the loop cannot read the
    second request from stdin while subprocess 1 is running, so the
    first call completes at about its serial cost and the second about
    one serial call later. With the fix (``_async_offload`` +
    ``anyio.to_thread.run_sync``) both subprocesses start at once and
    complete together. The assertion is on that completion spread,
    ``max(c) - min(c) < 0.5 x min(s1, s2)``; see
    ``_COMPLETION_SPREAD_RATIO`` for the two measured ends. The serial
    and concurrent totals (S, C) are reported for diagnosis but not
    asserted, because their ratio measures spare cores as much as
    dispatch: on a CI lane where the two calls shared one core it read
    1.04 with both calls overlapping.
    """
    result = asyncio.run(
        _run_serial_then_concurrent(
            warm_up=[
                (
                    "query_knowledge",
                    {"question": "warm-up baseline call", "limit": 1},
                ),
            ],
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
    spread = max(result.concurrent_each) - min(result.concurrent_each)
    _report_contrast(
        capsys,
        "query_knowledge x2",
        {
            "S": result.serial_total,
            "s1": result.serial_each[0],
            "s2": result.serial_each[1],
            "C": result.concurrent_total,
            "c1": result.concurrent_each[0],
            "c2": result.concurrent_each[1],
            "spread": spread,
            "ratio_C_over_S": result.concurrent_total / result.serial_total,
            "ratio_spread_over_min_s": spread / min(result.serial_each),
        },
    )
    assert len(result.envelopes) == 2, (
        "Expected two envelopes from the concurrent gather; got "
        f"{len(result.envelopes)}. If a task timed out, gh#1412 has regressed."
    )
    for idx, envelope in enumerate(result.envelopes):
        assert envelope.content, (
            f"Concurrent call {idx + 1}/2 returned without content; "
            "FastMCP dispatch likely failed."
        )
    budget = min(result.serial_each) * _COMPLETION_SPREAD_RATIO
    assert spread < budget, (
        f"Two concurrent query_knowledge calls completed {spread:.1f}s apart "
        f"(at {result.concurrent_each[0]:.1f}s and {result.concurrent_each[1]:.1f}s) "
        f"where the same two calls run one after the other in the same session "
        f"took {result.serial_each[0]:.1f}s and {result.serial_each[1]:.1f}s; "
        f"expected <{budget:.1f}s (= {_COMPLETION_SPREAD_RATIO} x the shorter "
        "serial call). The worker-thread offload has regressed: calls are "
        "serialising on the event loop instead of overlapping on threads "
        "(gh#1412)."
    )


@pytest.mark.slow
@pytestmark_skipif
def test_concurrent_query_knowledge_and_search_schema(capsys) -> None:
    """``query_knowledge`` concurrent with ``search_schema`` on one MCP
    session: the cross-tool variant the gh#1412 report listed explicitly
    (``query_knowledge`` then ``search_schema`` /
    ``read_knowledge_manifest`` / ``list_examples`` all showed the same
    second-call delay).

    The contrast here is that second-call delay, not total wall time:
    ``search_schema`` caches the ``suews schema`` envelope per server
    process, so once primed it costs milliseconds and the pair's total is
    the ``query_knowledge`` subprocess either way. Issued behind
    ``query_knowledge`` on a free loop, the primed probe is answered at
    once (its concurrent time is about its serial time); on a blocked loop
    its request waits for the whole subprocess (its concurrent time is
    about the ``query_knowledge`` time). The assertion is that the extra
    delay the probe suffers in the concurrent arm is below half the
    ``query_knowledge`` concurrent time; see ``_PROBE_DELAY_RATIO``.
    """
    result = asyncio.run(
        _run_serial_then_concurrent(
            warm_up=[
                (
                    "query_knowledge",
                    {"question": "warm-up baseline call", "limit": 1},
                ),
                # Prime the per-process schema cache so the probe measures
                # loop responsiveness, not a cold `suews schema` subprocess.
                ("search_schema", {"query": "sfr"}),
            ],
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
    probe_delay = result.concurrent_each[1] - result.serial_each[1]
    _report_contrast(
        capsys,
        "query_knowledge + search_schema",
        {
            "S": result.serial_total,
            "s_qk": result.serial_each[0],
            "s_ss": result.serial_each[1],
            "C": result.concurrent_total,
            "c_qk": result.concurrent_each[0],
            "c_ss": result.concurrent_each[1],
            "probe_delay": probe_delay,
            "ratio_delay_over_c_qk": probe_delay / result.concurrent_each[0],
        },
    )
    assert len(result.envelopes) == 2
    for idx, envelope in enumerate(result.envelopes):
        assert envelope.content, (
            f"Concurrent cross-tool call {idx + 1}/2 returned without "
            "content; FastMCP dispatch likely failed."
        )
    budget = result.concurrent_each[0] * _PROBE_DELAY_RATIO
    assert probe_delay < budget, (
        f"search_schema issued behind query_knowledge took "
        f"{result.concurrent_each[1]:.2f}s against {result.serial_each[1]:.2f}s "
        f"on its own in the same session, a delay of {probe_delay:.2f}s while "
        f"query_knowledge took {result.concurrent_each[0]:.1f}s; expected "
        f"<{budget:.1f}s (= {_PROBE_DELAY_RATIO} x the query_knowledge time). "
        "The event loop is blocked by the query_knowledge subprocess instead "
        "of reading the next request (gh#1412)."
    )


@pytestmark_skipif
def test_read_knowledge_manifest_returns_provenance(handshake_result: dict) -> None:
    """Calling `read_knowledge_manifest` over MCP returns pack provenance.

    The Layer-3 evidence contract requires `pack_version`, `schema_version`,
    and `git_sha` so that downstream answers can cite the exact revision.
    A manifest call that returns success but lacks any of these breaks
    every downstream cited-evidence assertion.
    """
    import json

    result = handshake_result
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
        assert manifest.get(required), (
            f"Manifest missing required provenance field {required!r}. "
            f"Got keys: {sorted(manifest.keys())}"
        )
