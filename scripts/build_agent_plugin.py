#!/usr/bin/env python3
"""Generate the SUEWS agent-plugin distribution repository.

The main SUEWS repository keeps `.claude/skills/suews/` as the single source of
truth. This script writes a self-contained marketplace repository for agent
hosts that need different packaging layouts:

- Claude Code reads `.claude-plugin/marketplace.json` and `.claude/skills/suews/`.
- Codex reads `.agents/plugins/marketplace.json` and installs `plugins/suews/`.

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

- `.claude-plugin/marketplace.json` and `.claude/skills/suews/` for Claude Code
  (git commit identifies the installed plugin version).
- `.agents/plugins/marketplace.json` and `plugins/suews/` for Codex.
- `.mcp.json` files that launch `suews-mcp` through `uvx`.

Generated from `{SOURCE_REPO}` commit `{source_commit}`.
"""


def _build(output: Path) -> None:
    _clean_output(output)

    source_commit = _git_commit()

    _copy_file(REPO / "LICENSE", output / "LICENSE")
    _copy_file(REPO / ".mcp.json", output / ".mcp.json")
    _copy_file(
        REPO / "plugins" / "suews" / ".mcp.json",
        output / "plugins" / "suews" / ".mcp.json",
    )
    _copy_tree(
        REPO / ".claude" / "skills" / "suews",
        output / ".claude" / "skills" / "suews",
    )
    _copy_tree(
        REPO / ".claude" / "skills" / "suews",
        output / "plugins" / "suews" / "skills" / "suews",
    )
    _copy_tree(
        REPO / "plugins" / "suews" / "assets",
        output / "plugins" / "suews" / "assets",
    )
    _copy_tree(
        REPO / "plugins" / "suews" / ".codex-plugin",
        output / "plugins" / "suews" / ".codex-plugin",
    )

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
