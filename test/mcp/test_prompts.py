"""Unit tests for the MCP server instructions and prompts.

These assert the *content* of the procedural guidance that ships inside the
MCP itself (`mcp/src/suews_mcp/prompts.py`) — the honesty contract, the
energy-balance parameter ladder, the authorised data sources, and the three
workflow prompts. The point of carrying this guidance in the MCP (rather than
only in the Claude Code `/suews` skill) is that it travels to every MCP client
via `serverInfo.instructions` and `prompts/get`; a regression here silently
strips that contract from Claude Desktop / Codex / Cursor sessions.

This module imports `suews_mcp.prompts` directly — no `mcp` SDK needed — so it
runs on every matrix cell. The protocol-level proof that the server actually
*advertises* the instructions and prompts lives in
`test_protocol_handshake.py` (gated on the `suews-mcp` console script).
"""

from __future__ import annotations

import pytest

from suews_mcp.prompts import (
    SERVER_INSTRUCTIONS,
    evaluate_results,
    fresh_site_setup,
    parameter_importance,
)

pytestmark = pytest.mark.api


# The three prompt functions, in registration order. Keep in sync with the
# `server.prompt(name=...)` calls in `mcp/src/suews_mcp/server.py`.
_PROMPTS = (fresh_site_setup, parameter_importance, evaluate_results)


def test_server_instructions_carry_the_full_contract() -> None:
    """`SERVER_INSTRUCTIONS` must state every limb of the contract so that a
    client which only reads `serverInfo.instructions` still sees it.
    """
    text = SERVER_INSTRUCTIONS
    assert len(text) > 400, "Instructions look truncated."
    # Honesty + provenance.
    assert "HONESTY" in text
    assert "provenance" in text.lower()
    # The energy-balance backbone, written as the actual identity.
    assert "QN + QF = QS + QE + QH" in text
    assert "residual" in text.lower()
    assert "albedo" in text.lower()
    # Closure is explicitly NOT a validation check (the recurring trap).
    assert "closure" in text.lower()
    assert "not a validation check" in text.lower()
    # Fresh-user readiness gate.
    assert "assess_readiness" in text
    # Authorised data sources — at least the anchors we committed to.
    for source in ("ERA5", "GLAMOUR", "SUEWS-database"):
        assert source in text, f"Instructions dropped data source {source!r}."
    # Plain-language rule.
    assert "PLAIN LANGUAGE" in text


def test_prompt_function_names_match_registered_names() -> None:
    """Each prompt is registered under its function name in server.py; a
    rename in one place without the other would silently break discovery.
    """
    assert [fn.__name__ for fn in _PROMPTS] == [
        "fresh_site_setup",
        "parameter_importance",
        "evaluate_results",
    ]


def test_prompts_return_substantial_text() -> None:
    """Every prompt returns non-trivial procedural text, not a stub."""
    for fn in _PROMPTS:
        body = fn()
        assert isinstance(body, str)
        assert len(body) > 300, f"Prompt {fn.__name__} looks like a stub."


def test_fresh_site_setup_walks_the_honest_workflow() -> None:
    """The fresh-site prompt must name the readiness gate, validation loop,
    the correct `run` invocation, and a stated readiness level.
    """
    text = fresh_site_setup()
    for anchor in ("init_case", "assess_readiness", "validate_config"):
        assert anchor in text, f"fresh_site_setup dropped {anchor!r}."
    # The `run` subcommand takes no `--format` flag — the prompt must warn
    # against it explicitly rather than suggesting it (recurring wrong flag).
    assert "suews run" in text
    assert "no `--format` flag" in text, (
        "fresh_site_setup must warn that `suews run` takes no `--format` flag."
    )
    # A single, explicit readiness level is the honesty pay-off.
    assert "readiness level" in text.lower()


def test_parameter_importance_derives_order_from_energy_balance() -> None:
    """The parameter ladder must lead with albedo and state QH/surface
    temperature are outputs, derived from the energy balance.
    """
    text = parameter_importance()
    assert "QN + QF = QS + QE + QH" in text
    assert "ALBEDO FIRST" in text
    assert "residual" in text.lower()
    # QH / surface temperature are outputs, never set by the user.
    assert "OUTPUT" in text
    # Parameter values come from the authorised database.
    assert "SUEWS-database" in text


def test_evaluate_results_blocks_the_circular_comparison() -> None:
    """The evaluation prompt must steer to the modelled T2 output (not the
    forcing Tair input) and reject closure-as-validation.
    """
    text = evaluate_results()
    assert "T2" in text
    assert "compare_runs" in text
    assert "circular" in text.lower()
    # Energy-balance closure is not an independent validation check.
    assert "closure" in text.lower()
