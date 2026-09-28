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

The plugin folder vendors the `suews-mcp` source as `server/`, with a
generated `uv.lock`, and its `.mcp.json` runs that copy through
`uv run --frozen` from `${CLAUDE_PLUGIN_ROOT}`. The directory holds plugins
that fetch a package with `npx`/`uvx` for review, and prefers vendored source
plus a lockfile; this also keeps the MCP server in step with the skill it
ships with. Codex does not expand `${CLAUDE_PLUGIN_ROOT}`, so its manifest
points at a separate `.codex-mcp.json` that keeps the `uvx` launcher pinned to
the source commit.

The output directory may already be a Git checkout; its `.git/` directory is
preserved while the generated payload is refreshed.
"""

from __future__ import annotations

import argparse
import json
from pathlib import Path
import os
import re
import shutil
import subprocess
import sys
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
SERVER_IGNORE = shutil.ignore_patterns(
    "__pycache__",
    "*.pyc",
    ".DS_Store",
    "*.egg-info",
    "build",
    "dist",
    ".venv",
    "_version_scm.py",
)

SERVER_DIR = "server"
CODEX_MCP_FILE = ".codex-mcp.json"

# The vendored server's lockfile must stay under the directory's 256 KiB
# per-file read limit. Locking every Python and platform `supy` could run on
# gives about 800 KiB, so the generated project is narrowed to the interpreters
# and platforms `supy` publishes wheels for. `uv run` fetches a matching
# managed Python when the user has none.
SERVER_REQUIRES_PYTHON = ">=3.12,<3.14"
SERVER_ENVIRONMENTS = (
    "sys_platform == 'darwin' and platform_machine == 'arm64'",
    "sys_platform == 'darwin' and platform_machine == 'x86_64'",
    "sys_platform == 'linux' and platform_machine == 'x86_64'",
    "sys_platform == 'win32' and platform_machine == 'AMD64'",
)
LOCKFILE_LIMIT_BYTES = 256 * 1024


def _read_json(path: Path) -> dict[str, Any]:
    return json.loads(path.read_text(encoding="utf-8"))


def _write_json(path: Path, payload: dict[str, Any]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(payload, indent=2) + "\n", encoding="utf-8")


def _copy_file(src: Path, dst: Path) -> None:
    dst.parent.mkdir(parents=True, exist_ok=True)
    shutil.copy2(src, dst)


def _copy_tree(src: Path, dst: Path, ignore: Any = IGNORE) -> None:
    if dst.exists():
        shutil.rmtree(dst)
    shutil.copytree(src, dst, ignore=ignore)


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
    """Return the source `.mcp.json` with `suews-mcp` pinned to the source commit.

    Codex cannot run the vendored server (see module docstring), so its MCP
    file keeps the `uvx` launcher, pinned so it matches the skill it ships
    with.
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


def _vendored_mcp() -> dict[str, Any]:
    """Return the Claude Code `.mcp.json` that runs the vendored server.

    `--project` rather than `--directory` keeps the working directory, which
    `suews-mcp` uses as its case root when no `--root` is given. `UV_PYTHON`
    overrides any interpreter the user pins globally that the lockfile does
    not cover.
    """
    return {
        "mcpServers": {
            "suews": {
                "command": "uv",
                "args": [
                    "run",
                    "--frozen",
                    "--project",
                    f"${{CLAUDE_PLUGIN_ROOT}}/{SERVER_DIR}",
                    "suews-mcp",
                ],
                "env": {"UV_PYTHON": SERVER_REQUIRES_PYTHON},
            }
        }
    }


SERVER_README = """# suews-mcp (vendored)

This folder is a copy of the `mcp/` package in the SUEWS source repository,
vendored into the plugin with a lockfile so the plugin runs the Model Context
Protocol server from its own files. The plugin's `.mcp.json` starts it with
`uv run --frozen`; there is nothing to install by hand. Development, tests and
documentation for the server live in the source repository.
"""


def _server_version() -> str:
    sys.path.insert(0, str(REPO))
    try:
        import get_ver_git

        version = get_ver_git.get_version_from_git()
        version_tuple = get_ver_git.parse_version_tuple(version)
    finally:
        sys.path.pop(0)
    return (
        "# file generated by `scripts/build_agent_plugin.py`\n"
        f"__version__ = version = {version!r}\n"
        f"__version_tuple__ = version_tuple = {version_tuple!r}\n"
    )


def _server_pyproject(source: str) -> str:
    """Narrow the vendored `pyproject.toml` so its lockfile stays small.

    Drops the `dev` extra (test tooling a plugin install never needs) and
    restricts the Python range and platforms, see `SERVER_ENVIRONMENTS`.
    """
    text, n_extra = re.subn(
        r"\[project\.optional-dependencies\]\ndev = \[.*?\]\n\n?",
        "",
        source,
        flags=re.DOTALL,
    )
    text, n_python = re.subn(
        r'^requires-python = ".*"$',
        f'requires-python = "{SERVER_REQUIRES_PYTHON}"',
        text,
        flags=re.MULTILINE,
    )
    if n_extra != 1 or n_python != 1 or "[tool.uv]" in text:
        raise RuntimeError(
            "mcp/pyproject.toml no longer has the shape build_agent_plugin.py "
            "rewrites; update _server_pyproject()."
        )
    environments = "".join(f'    "{env}",\n' for env in SERVER_ENVIRONMENTS)
    return f"{text.rstrip()}\n\n[tool.uv]\nenvironments = [\n{environments}]\n"


def _vendor_server(plugin_out: Path, *, lock: bool) -> None:
    server_out = plugin_out / SERVER_DIR
    _copy_tree(REPO / "mcp", server_out, ignore=SERVER_IGNORE)
    # `_version_scm.py` is gitignored in the source tree, so a fresh checkout
    # has none; without it the vendored package reports `0+unknown`.
    (server_out / "src" / "suews_mcp" / "_version_scm.py").write_text(
        _server_version(), encoding="utf-8"
    )
    # The source README documents installing and launching the server by hand,
    # which does not apply here; `pyproject.toml` still needs a readme.
    (server_out / "README.md").write_text(SERVER_README, encoding="utf-8")
    pyproject = server_out / "pyproject.toml"
    pyproject.write_text(
        _server_pyproject(pyproject.read_text(encoding="utf-8")), encoding="utf-8"
    )
    if not lock:
        return
    # A user-level constraint file or interpreter pin must not leak into the
    # published lockfile.
    env = {
        key: value
        for key, value in os.environ.items()
        if key not in {"UV_CONSTRAINT", "UV_PYTHON", "UV_OVERRIDE"}
    }
    subprocess.run(["uv", "lock"], cwd=server_out, env=env, check=True)
    size = (server_out / "uv.lock").stat().st_size
    if size > LOCKFILE_LIMIT_BYTES:
        raise RuntimeError(
            f"server/uv.lock is {size} bytes, over the plugin directory's "
            f"{LOCKFILE_LIMIT_BYTES}-byte read limit; narrow SERVER_ENVIRONMENTS."
        )


def _codex_plugin_manifest() -> dict[str, Any]:
    payload = _read_json(REPO / PLUGIN_DIR / ".codex-plugin" / "plugin.json")
    payload["mcpServers"] = f"./{CODEX_MCP_FILE}"
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
  `.codex-plugin/plugin.json` (Codex), the `suews` skill, and the `suews-mcp`
  source in `server/` with a `uv.lock`. Claude Code runs that copy through
  `uv run` (`.mcp.json`); Codex launches `suews-mcp` through `uvx`, pinned to
  the source commit below (`.codex-mcp.json`).
- `.claude-plugin/marketplace.json` for Claude Code (git commit identifies the
  installed plugin version).
- `.agents/plugins/marketplace.json` for Codex.

Generated from `{SOURCE_REPO}` commit `{source_commit}`.
"""


def _build(output: Path, *, lock: bool = True) -> None:
    _clean_output(output)

    source_commit = _git_commit()

    plugin_out = output / PLUGIN_DIR

    _copy_file(REPO / "LICENSE", output / "LICENSE")
    _copy_file(REPO / "LICENSE", plugin_out / "LICENSE")
    _copy_file(REPO / PLUGIN_DIR / "README.md", plugin_out / "README.md")
    _write_json(plugin_out / ".mcp.json", _vendored_mcp())
    _write_json(plugin_out / CODEX_MCP_FILE, _pinned_mcp(source_commit))
    _vendor_server(plugin_out, lock=lock)
    _copy_tree(
        REPO / ".claude" / "skills" / PLUGIN_NAME,
        plugin_out / "skills" / PLUGIN_NAME,
    )
    for subdir in ("assets", ".claude-plugin"):
        _copy_tree(REPO / PLUGIN_DIR / subdir, plugin_out / subdir)
    _write_json(
        plugin_out / ".codex-plugin" / "plugin.json", _codex_plugin_manifest()
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
    parser.add_argument(
        "--skip-lock",
        action="store_true",
        help="Do not run `uv lock` for the vendored server (offline tests).",
    )
    args = parser.parse_args()
    _build(args.output.resolve(), lock=not args.skip_lock)


if __name__ == "__main__":
    _main()
