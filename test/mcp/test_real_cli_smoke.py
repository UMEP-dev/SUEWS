"""Layer-2 real-CLI smoke through the MCP backend.

The other tool tests in this directory monkey-patch ``subprocess.run``
to return a stub envelope; this file is the only path that actually
invokes the real ``suews`` CLI through ``run_suews_cli`` and asserts
the envelope round-trips.

Skipped (rather than failed) when ``suews`` is not on PATH — fresh
checkouts that have not run ``make dev`` legitimately do not have it.
"""

from __future__ import annotations

import shutil
from pathlib import Path

import pytest

pytestmark = pytest.mark.api


REPO_ROOT = Path(__file__).resolve().parents[2]


pytestmark_skipif = pytest.mark.skipif(
    shutil.which("suews") is None,
    reason=(
        "`suews` CLI is not on PATH. Run `make dev` from the repo root "
        "(or `uv pip install -e .`) before running this test."
    ),
)


@pytestmark_skipif
def test_validate_sample_config_succeeds() -> None:
    """Validating the bundled sample config returns a success envelope.

    Asserts the full provenance contract: the meta block must carry
    `suews_version`, `schema_version`, `git_commit`, and the exact
    command line — the same fields downstream knowledge-pack answers
    rely on for citation.
    """
    from suews_mcp.tools import validate_config

    config_path = REPO_ROOT / "src" / "supy" / "sample_data" / "sample_config.yml"
    assert config_path.exists(), (
        f"Sample config missing at {config_path}. "
        "Has src/supy/sample_data/ been moved?"
    )

    envelope = validate_config(
        str(config_path),
        project_root=str(REPO_ROOT),
    )

    assert envelope["status"] == "success", (
        f"Sample config rejected with errors: {envelope.get('errors')}"
    )

    data = envelope.get("data") or {}
    assert data.get("is_valid") is True, (
        f"Validator returned status=success but is_valid != True: {data}"
    )

    meta = envelope.get("meta") or {}
    for required in ("suews_version", "schema_version", "git_commit", "command"):
        assert required in meta and meta[required], (
            f"Meta block missing required field {required!r}. "
            f"Got keys: {sorted(meta.keys())}"
        )


@pytestmark_skipif
def test_validate_known_bad_config_returns_actionable_diagnostics() -> None:
    """Validating a known-bad fixture returns an error envelope with
    diagnostics agents can act on.

    The envelope must carry: ``status='error'``, a non-empty ``errors``
    list, each error with a ``code_name`` and a human-readable
    ``message``. A bare traceback or a misleading ``status='success'``
    fails this contract — agents need a structured failure they can
    surface back to the user.
    """
    from suews_mcp.tools import validate_config

    bad_fixture = REPO_ROOT / "test" / "mcp" / "fixtures" / "known_bad_config.yml"
    assert bad_fixture.exists(), f"Bad fixture missing at {bad_fixture}."

    envelope = validate_config(
        str(bad_fixture),
        project_root=str(REPO_ROOT),
    )

    assert envelope["status"] == "error", (
        f"Known-bad config returned status={envelope['status']!r}; "
        "envelope contract requires structured failure for invalid input. "
        f"Full envelope: {envelope}"
    )

    errors = envelope.get("errors") or []
    assert errors, "Error envelope returned empty errors list."

    err = errors[0]
    assert err.get("code_name"), (
        f"Error missing code_name: {err}. Agents rely on this for "
        "programmatic dispatch."
    )
    assert err.get("message"), (
        f"Error missing human-readable message: {err}."
    )

    # The fixture is invalid YAML; validator catches it as INVALID_YAML
    # at the structural layer. Asserting on the specific code locks in
    # the contract that early-stage failures surface clearly rather
    # than as opaque "validation failed".
    assert err["code_name"] == "INVALID_YAML", (
        f"Expected INVALID_YAML for the known-bad fixture, got "
        f"{err['code_name']!r}. If the fixture content changed, update "
        "this assertion to match the new failure class."
    )


@pytestmark_skipif
def test_query_knowledge_returns_provenance_metadata() -> None:
    """A real `query_knowledge` call returns matches with full provenance.

    Layer-3 evidence assertions later in the test suite depend on this:
    if the knowledge backend silently strips git_sha or github_url, the
    canonical Q&A runner can't verify cited evidence. This is the
    contract the Layer-3 runner builds on.
    """
    from suews_mcp.tools import query_knowledge

    envelope = query_knowledge("irrigation_fraction", limit=2)

    assert envelope["status"] == "success", (
        f"query_knowledge errored: {envelope.get('errors')}. Has the "
        "knowledge pack been built? Run `meson knowledge-pack build` "
        "or `suews knowledge build`."
    )

    data = envelope.get("data") or {}
    matches = data.get("matches") or []
    assert matches, (
        f"query_knowledge returned no matches for 'irrigation_fraction'. "
        f"Pack manifest: {data.get('manifest')}"
    )

    first = matches[0]
    for required in ("git_sha", "github_url", "repo_path", "line_start", "line_end"):
        assert required in first and first[required] not in (None, ""), (
            f"Match missing required provenance field {required!r}. "
            f"Got keys: {sorted(first.keys())}"
        )

    # Manifest itself carries the pack-version provenance the Layer-3
    # contract requires.
    manifest = data.get("manifest") or {}
    for required in ("git_sha", "schema_version"):
        assert required in manifest and manifest[required], (
            f"Pack manifest missing {required!r}. Got keys: "
            f"{sorted(manifest.keys())}"
        )
