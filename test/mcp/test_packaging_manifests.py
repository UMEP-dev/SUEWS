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

import hashlib
import json
from pathlib import Path
import subprocess
import sys

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


def test_source_codex_marketplace_disables_raw_repo_install() -> None:
    """Raw `UMEP-dev/SUEWS` is not the Codex install surface.

    Codex installs only the marketplace plugin source directory
    (`plugins/suews/`), so the self-contained installable bundle lives in the
    generated `UMEP-dev/suews-agent` distribution repo instead.
    """
    data = _load_json(".agents/plugins/marketplace.json")
    plugins = data.get("plugins", [])
    suews = next((p for p in plugins if p.get("name") == "suews"), None)

    assert suews is not None
    assert suews["source"]["path"] == "./plugins/suews"
    assert suews["policy"]["installation"] == "NOT_AVAILABLE"


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


def _skill_manifest(root: Path) -> dict[str, str]:
    """Map of relative path -> sha256 for every file under a skill dir."""
    out: dict[str, str] = {}
    for p in sorted(root.rglob("*")):
        if p.is_file() and "__pycache__" not in p.parts and p.suffix != ".pyc":
            out[str(p.relative_to(root))] = hashlib.sha256(p.read_bytes()).hexdigest()
    return out


def test_build_agent_plugin_generates_installable_distribution(tmp_path: Path) -> None:
    """The generated `suews-agent` repo is the self-contained install surface.

    It carries Claude Code's canonical `.claude/skills/suews/` path and Codex's
    required `plugins/suews/skills/suews/` path, both byte-identical to the
    source skill.
    """
    output = tmp_path / "suews-agent"
    subprocess.run(
        [
            sys.executable,
            str(REPO_ROOT / "scripts" / "build_agent_plugin.py"),
            "--output",
            str(output),
        ],
        check=True,
        capture_output=True,
    )

    codex_marketplace = json.loads(
        (output / ".agents" / "plugins" / "marketplace.json").read_text(
            encoding="utf-8"
        )
    )
    codex_plugin = codex_marketplace["plugins"][0]
    assert codex_plugin["name"] == "suews"
    assert codex_plugin["source"]["path"] == "./plugins/suews"
    assert codex_plugin["policy"]["installation"] == "AVAILABLE"

    claude_marketplace = json.loads(
        (output / ".claude-plugin" / "marketplace.json").read_text(
            encoding="utf-8"
        )
    )
    assert "version" not in claude_marketplace["metadata"]
    assert [plugin["name"] for plugin in claude_marketplace["plugins"]] == ["suews"]

    assert (output / ".mcp.json").exists()
    assert (output / "plugins" / "suews" / ".mcp.json").exists()
    assert (
        output / "plugins" / "suews" / ".codex-plugin" / "plugin.json"
    ).exists()
    assert (output / "plugins" / "suews" / "assets" / "icon.png").exists()

    source = _skill_manifest(REPO_ROOT / ".claude" / "skills" / "suews")
    claude_skill = _skill_manifest(output / ".claude" / "skills" / "suews")
    codex_skill = _skill_manifest(
        output / "plugins" / "suews" / "skills" / "suews"
    )
    assert claude_skill == source
    assert codex_skill == source


def test_build_plugin_generates_single_skill_bundle_from_source() -> None:
    """`scripts/build_plugin.py` (`make plugin`) generates the distributable
    bundle from the single source `.claude/skills/suews/`. The bundle
    (`plugins/suews/skills/`) is a gitignored build artifact — there is no
    committed copy to drift — so this runs the generator and verifies its
    output: exactly one skill (`suews`; fresh-site setup is a reference inside
    it, not a separate skill), byte-identical to the source.
    """
    subprocess.run(
        [sys.executable, str(REPO_ROOT / "scripts" / "build_plugin.py")],
        check=True,
        capture_output=True,
    )
    bundle_skills = REPO_ROOT / "plugins" / "suews" / "skills"
    dirs = sorted(p.name for p in bundle_skills.iterdir() if p.is_dir())
    assert dirs == ["suews"], f"bundle should ship only 'suews', found: {dirs}"

    src = _skill_manifest(REPO_ROOT / ".claude" / "skills" / "suews")
    bundle = _skill_manifest(bundle_skills / "suews")
    assert src == bundle, (
        "build_plugin.py output differs from .claude/skills/suews/ — generator bug. "
        f"source-only: {sorted(set(src) - set(bundle))}; "
        f"bundle-only: {sorted(set(bundle) - set(src))}; "
        f"differing: {sorted(k for k in src if k in bundle and src[k] != bundle[k])}"
    )
