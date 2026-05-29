"""Layer-0 packaging and manifest sanity checks for ``suews-mcp``.

Asserts that the bundled plugin manifests and ``.mcp.json`` files have the
shape both Claude Code and Codex accept. Pure file-IO + JSON parsing — no
subprocess, no network. See ``test_protocol_handshake.py`` for the layer
that actually spawns the server.

The Codex-vs-Claude shape concern was raised in gh#1384 (issue body,
"Current findings after MCP/Codex review", risk 1). After auditing the
Codex source, the conclusion is that **a single shape works for both**:
``{"mcpServers": {"<name>": {"command": "...", ...}}}``. Codex's plugin
loader uses an ``#[serde(untagged)]`` enum that accepts either this
wrapper or a bare ``{"<name>": {...}}`` server map; both manifests use
the wrapper form so the test asserts that. The snake-case
``mcp_servers`` only applies to the user-level ``~/.codex/config.toml``
table, not to plugin-bundled ``.mcp.json``. References:
``codex-rs/core-plugins/src/loader.rs`` (lines 82-96, 980-1002) and
``codex-rs/core-plugins/src/manifest.rs`` (lines 13-32).
"""

from __future__ import annotations

import json
from pathlib import Path

import pytest

pytestmark = pytest.mark.api


REPO_ROOT = Path(__file__).resolve().parents[2]


# Every plugin/manifest file we ship and the host that consumes it. The
# triplet (path, host, kind) lets a failing test name the offender clearly.
MANIFESTS: tuple[tuple[str, str, str], ...] = (
    (".mcp.json", "claude+codex", "mcp"),
    ("plugins/suews/.mcp.json", "claude+codex", "mcp"),
    (".codex-plugin/plugin.json", "codex", "plugin"),
    ("plugins/suews/.codex-plugin/plugin.json", "codex", "plugin"),
    (".claude-plugin/marketplace.json", "claude", "marketplace"),
)


def _load_json(rel_path: str) -> dict:
    return json.loads((REPO_ROOT / rel_path).read_text(encoding="utf-8"))


@pytest.mark.parametrize("rel_path,host,kind", MANIFESTS)
def test_manifest_is_valid_json(rel_path: str, host: str, kind: str) -> None:
    """Every shipped manifest parses as valid JSON."""
    data = _load_json(rel_path)
    assert isinstance(data, dict), f"{rel_path}: top level must be an object"


@pytest.mark.parametrize(
    "rel_path",
    [".mcp.json", "plugins/suews/.mcp.json"],
)
def test_mcp_json_has_accepted_shape(rel_path: str) -> None:
    """`.mcp.json` uses the wrapper shape both Claude Code and Codex accept.

    Per Codex's plugin loader (codex-rs/core-plugins/src/loader.rs), the
    accepted shapes are:
        - wrapped: `{"mcpServers": {"<name>": {...}}}`  <- what we use
        - bare:    `{"<name>": {...}}`
    Claude Code requires the wrapper. We use the wrapper so a single file
    serves both.
    """
    data = _load_json(rel_path)

    assert "mcpServers" in data, (
        f"{rel_path}: missing camelCase 'mcpServers' wrapper. "
        "Both Claude Code and Codex (untagged enum, wrapped variant) "
        "accept this; do not switch to snake_case 'mcp_servers' — that "
        "is the ~/.codex/config.toml table key, a different surface."
    )
    assert "mcp_servers" not in data, (
        f"{rel_path}: snake_case 'mcp_servers' is the user-level "
        "config.toml key, not a plugin-bundled .mcp.json key. Use "
        "camelCase 'mcpServers'."
    )

    servers = data["mcpServers"]
    assert isinstance(servers, dict) and servers, (
        f"{rel_path}: 'mcpServers' must be a non-empty object."
    )

    for name, cfg in servers.items():
        assert isinstance(cfg, dict), (
            f"{rel_path}: server '{name}' must be an object."
        )
        assert "command" in cfg, (
            f"{rel_path}: server '{name}' missing required 'command' field."
        )


@pytest.mark.parametrize(
    "rel_path",
    [".codex-plugin/plugin.json", "plugins/suews/.codex-plugin/plugin.json"],
)
def test_codex_plugin_references_mcp_servers_in_camelcase(rel_path: str) -> None:
    """Codex's plugin manifest references its MCP file via camelCase.

    Per codex-rs/core-plugins/src/manifest.rs, the field is declared
    `#[serde(rename_all = "camelCase")]` over `mcp_servers: Option<String>`,
    so JSON sees `mcpServers`. Confirmed against the official sample at
    `codex-rs/skills/.../plugin-creator/references/plugin-json-spec.md`.
    """
    data = _load_json(rel_path)

    assert "mcpServers" in data, (
        f"{rel_path}: Codex plugin.json must reference its MCP file via "
        "camelCase 'mcpServers' (the field is "
        "#[serde(rename_all = \"camelCase\")] over mcp_servers: Option<String>)."
    )

    ref = data["mcpServers"]
    assert isinstance(ref, str) and ref.startswith("./"), (
        f"{rel_path}: 'mcpServers' must be a relative path beginning with "
        f"'./', got {ref!r}."
    )

    # `./` in a Codex plugin.json resolves relative to the *plugin root*,
    # which is the parent of `.codex-plugin/`, not `.codex-plugin/` itself.
    plugin_root = Path(rel_path).parent.parent
    referenced = (REPO_ROOT / plugin_root / ref).resolve()
    assert referenced.exists(), (
        f"{rel_path}: 'mcpServers' references {ref!r} (relative to plugin "
        f"root {plugin_root}) which resolves to {referenced} but that file "
        "does not exist."
    )


def test_claude_marketplace_lists_suews_plugin() -> None:
    """The Claude Code marketplace entry for `suews` references the bundled
    plugin's MCP file."""
    data = _load_json(".claude-plugin/marketplace.json")
    plugins = data.get("plugins", [])

    suews = next((p for p in plugins if p.get("name") == "suews"), None)
    assert suews is not None, (
        "marketplace.json: missing 'suews' plugin entry."
    )
    assert "mcpServers" in suews, (
        "marketplace.json: 'suews' plugin must declare 'mcpServers'."
    )

    ref = suews["mcpServers"]
    assert isinstance(ref, str) and ref.startswith("./"), (
        f"marketplace.json: 'mcpServers' must be a relative path "
        f"beginning with './', got {ref!r}."
    )

    referenced = (REPO_ROOT / ref).resolve()
    assert referenced.exists(), (
        f"marketplace.json: 'mcpServers' references {ref!r} which "
        f"resolves to {referenced} but that file does not exist."
    )


def test_top_level_and_bundled_plugin_resolve_to_same_command() -> None:
    """The top-level plugin and the bundled `plugins/suews/` plugin must
    resolve to the same MCP server command. Otherwise installing one
    versus the other would produce divergent behaviour."""
    top_level_mcp = _load_json(".mcp.json")
    bundled_mcp = _load_json("plugins/suews/.mcp.json")

    top_servers = top_level_mcp["mcpServers"]
    bundled_servers = bundled_mcp["mcpServers"]

    assert set(top_servers.keys()) == set(bundled_servers.keys()), (
        "Top-level .mcp.json and plugins/suews/.mcp.json must declare the "
        "same set of MCP server names."
    )

    for name in top_servers:
        top_cmd = top_servers[name].get("command")
        bundled_cmd = bundled_servers[name].get("command")
        assert top_cmd == bundled_cmd, (
            f"server '{name}': command differs between top-level "
            f"({top_cmd!r}) and bundled ({bundled_cmd!r})."
        )
