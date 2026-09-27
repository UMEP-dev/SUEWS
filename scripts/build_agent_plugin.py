#!/usr/bin/env python3
"""Generate the SUEWS agent-plugin distribution repository.

The main SUEWS repository keeps `.claude/skills/suews/` as the single source of
truth. This script writes a self-contained marketplace repository for agent
hosts that need different packaging layouts:

- Claude Code reads `.claude-plugin/marketplace.json`, which points at
  `plugins/suews/`.
- Codex reads `.agents/plugins/marketplace.json` and installs `plugins/suews/`.
- Anthropic's plugin directory (claude.ai/directory) is submitted with
  `plugins/suews/` as the plugin path, so that folder must be self-contained:
  `.claude-plugin/plugin.json`, the skill, `.mcp.json`, a README and a licence.

The generated `.mcp.json` pins `suews-mcp` to the source commit, so the MCP
server an install launches matches the skill it was generated with, and the
directory's launcher check sees an exact version rather than a moving branch.

The output directory may already be a Git checkout; its `.git/` directory is
preserved while the generated payload is refreshed.
"""

from __future__ import annotations

import argparse
import json
from pathlib import Path
import shutil
import subprocess
from typing import Any

REPO = Path(__file__).resolve().parent.parent
PLUGIN_NAME = "suews"
MARKETPLACE_REPO = "UMEP-dev/suews-agent"
SOURCE_REPO = "UMEP-dev/SUEWS"
PLUGIN_DIR = Path("plugins") / PLUGIN_NAME
MCP_GIT_URL = f"git+https://github.com/{SOURCE_REPO}.git"

# Paths the generator owns in the output. `.claude/` and the root `.mcp.json`
# are no longer generated (the plugin folder carries its own), but stay listed
# so a refresh removes the copies earlier syncs left behind.
MANAGED_PATHS = (
    ".agents",
    ".claude",
    ".claude-plugin",
    "plugins",
    ".mcp.json",
    "LICENSE",
    "README.md",
)

IGNORE = shutil.ignore_patterns("__pycache__", "*.pyc", ".DS_Store")


def _read_json(path: Path) -> dict[str, Any]:
    return json.loads(path.read_text(encoding="utf-8"))


def _write_json(path: Path, payload: dict[str, Any]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(payload, indent=2) + "\n", encoding="utf-8")


def _copy_file(src: Path, dst: Path) -> None:
    dst.parent.mkdir(parents=True, exist_ok=True)
    shutil.copy2(src, dst)


def _copy_tree(src: Path, dst: Path) -> None:
    if dst.exists():
        shutil.rmtree(dst)
    shutil.copytree(src, dst, ignore=IGNORE)


def _clean_output(output: Path) -> None:
    output.mkdir(parents=True, exist_ok=True)
    for rel_path in MANAGED_PATHS:
        target = output / rel_path
        if target.is_dir():
            shutil.rmtree(target)
        elif target.exists():
            target.unlink()


def _git_commit() -> str:
    try:
        return subprocess.check_output(
            ["git", "-C", str(REPO), "rev-parse", "HEAD"],
            text=True,
            stderr=subprocess.DEVNULL,
        ).strip()
    except (subprocess.CalledProcessError, FileNotFoundError):
        return "unknown"


def _pinned_mcp(source_commit: str) -> dict[str, Any]:
    """Return the plugin `.mcp.json` with `suews-mcp` pinned to the source commit.

    The source `.mcp.json` follows the default branch, which suits a checkout.
    A published plugin should launch the server from the same commit as the
    skill it ships with.
    """
    payload = _read_json(REPO / PLUGIN_DIR / ".mcp.json")
    if source_commit == "unknown":
        return payload
    for server in payload["mcpServers"].values():
        server["args"] = [
            arg.replace(f"{MCP_GIT_URL}#", f"{MCP_GIT_URL}@{source_commit}#")
            for arg in server.get("args", [])
        ]
    return payload


def _codex_marketplace() -> dict[str, Any]:
    return {
        "name": "suews",
        "interface": {
            "displayName": "SUEWS",
            "shortDescription": "Urban climate modelling tools for AI assistants",
        },
        "plugins": [
            {
                "name": PLUGIN_NAME,
                "source": {
                    "source": "local",
                    "path": "./plugins/suews",
                },
                "policy": {
                    "installation": "AVAILABLE",
                    "authentication": "ON_INSTALL",
                },
                "category": "Productivity",
            }
        ],
    }


def _claude_marketplace() -> dict[str, Any]:
    source = _read_json(REPO / ".claude-plugin" / "marketplace.json")
    suews = next(
        plugin for plugin in source["plugins"] if plugin["name"] == PLUGIN_NAME
    )
    # The generated repository carries a self-contained plugin folder with its
    # own manifest, skill and `.mcp.json`, so the entry only needs to point at
    # it; the component paths in the source entry are relative to the SUEWS
    # repository root and do not apply here.
    suews = {
        "name": suews["name"],
        "description": suews["description"],
        "source": f"./{PLUGIN_DIR.as_posix()}",
    }
    metadata = {
        **source["metadata"],
        "repository": f"https://github.com/{MARKETPLACE_REPO}",
    }
    metadata.pop("version", None)
    return {
        "name": "suews",
        "owner": source["owner"],
        "metadata": metadata,
        "plugins": [suews],
    }


def _readme(source_commit: str) -> str:
    return f"""# SUEWS Agent Plugin

Self-contained SUEWS plugin marketplace for Claude Code and Codex.

This repository is generated from
[`{SOURCE_REPO}`](https://github.com/{SOURCE_REPO}). Do not edit generated
plugin contents by hand; update the canonical SUEWS skill in the source
repository and regenerate this distribution.

## Repository Governance

This is a generated distribution mirror, not a development repository. Treat it
as read-only for human edits: changes should be made in `{SOURCE_REPO}`, merged
to `master`, and then published here by the SUEWS agent-plugin sync workflow.

The `main` branch should be protected. In the current low-friction setup, the
sync workflow pushes with a fine-grained `SUEWS_AGENT_PUSH_TOKEN` stored only in
`{SOURCE_REPO}`, with the token owner kept as the temporary maintainer
exception. Do not push manual content edits here; they will be overwritten by
the next generated sync.

## Install

Claude Code:

```text
/plugin marketplace add {MARKETPLACE_REPO}
/plugin install suews@suews
```

Codex:

```bash
codex plugin marketplace add {MARKETPLACE_REPO}
codex plugin add suews@suews
```

## Contents

- `plugins/suews/`: the plugin itself, shared by every host. It holds
  `.claude-plugin/plugin.json` (Claude Code and Anthropic's plugin directory),
  `.codex-plugin/plugin.json` (Codex), the `suews` skill, and a `.mcp.json` that
  launches `suews-mcp` through `uvx`, pinned to the source commit below.
- `.claude-plugin/marketplace.json` for Claude Code (git commit identifies the
  installed plugin version).
- `.agents/plugins/marketplace.json` for Codex.

Generated from `{SOURCE_REPO}` commit `{source_commit}`.
"""


def _build(output: Path) -> None:
    _clean_output(output)

    source_commit = _git_commit()

    plugin_out = output / PLUGIN_DIR

    _copy_file(REPO / "LICENSE", output / "LICENSE")
    _copy_file(REPO / "LICENSE", plugin_out / "LICENSE")
    _copy_file(REPO / PLUGIN_DIR / "README.md", plugin_out / "README.md")
    _write_json(plugin_out / ".mcp.json", _pinned_mcp(source_commit))
    _copy_tree(
        REPO / ".claude" / "skills" / PLUGIN_NAME,
        plugin_out / "skills" / PLUGIN_NAME,
    )
    for subdir in ("assets", ".codex-plugin", ".claude-plugin"):
        _copy_tree(REPO / PLUGIN_DIR / subdir, plugin_out / subdir)

    _write_json(
        output / ".agents" / "plugins" / "marketplace.json", _codex_marketplace()
    )
    _write_json(
        output / ".claude-plugin" / "marketplace.json", _claude_marketplace()
    )
    (output / "README.md").write_text(_readme(source_commit), encoding="utf-8")

    print(f"agent plugin distribution written to {output}")


def _main() -> None:
    parser = argparse.ArgumentParser(
        description="Generate the SUEWS agent-plugin distribution repository."
    )
    parser.add_argument(
        "--output",
        required=True,
        type=Path,
        help="Output directory, usually a checkout of UMEP-dev/suews-agent.",
    )
    args = parser.parse_args()
    _build(args.output.resolve())


if __name__ == "__main__":
    _main()
