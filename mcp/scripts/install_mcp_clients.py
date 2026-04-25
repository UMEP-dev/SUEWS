#!/usr/bin/env python3
"""Install SUEWS MCP into Codex and Claude Desktop configuration files."""

from __future__ import annotations

import argparse
from datetime import datetime
import json
from pathlib import Path
import re
import shutil
import sys

CODEX_CONFIG = Path.home() / ".codex" / "config.toml"
CLAUDE_CONFIG = Path.home() / "Library" / "Application Support" / "Claude" / "claude_desktop_config.json"
SERVER_NAME = "suews"


def _timestamp() -> str:
    return datetime.now().strftime("%Y%m%d-%H%M%S")


def _backup(path: Path, dry_run: bool) -> Path | None:
    if not path.exists():
        return None
    backup_path = path.with_name(f"{path.name}.bak-{_timestamp()}")
    if not dry_run:
        shutil.copy2(path, backup_path)
    return backup_path


def _toml_string(value: str) -> str:
    # JSON string escaping is compatible with TOML basic strings.
    return json.dumps(value, ensure_ascii=True)


def _command_spec(mode: str, server_dir: Path) -> tuple[str, list[str]]:
    if mode == "published":
        return "uvx", ["suews-mcp"]
    return "uv", ["run", "--directory", str(server_dir), "suews-mcp"]


def _codex_block(command: str, args: list[str]) -> str:
    args_text = ", ".join(_toml_string(item) for item in args)
    return (
        f"[mcp_servers.{SERVER_NAME}]\n"
        f"command = {_toml_string(command)}\n"
        f"args = [{args_text}]\n"
    )


def configure_codex(server_dir: Path, mode: str, dry_run: bool) -> None:
    backup_path = _backup(CODEX_CONFIG, dry_run=dry_run)
    original = CODEX_CONFIG.read_text(encoding="utf-8") if CODEX_CONFIG.exists() else ""
    command, args = _command_spec(mode=mode, server_dir=server_dir)
    block = _codex_block(command=command, args=args)

    pattern = re.compile(rf"(?ms)^\[mcp_servers\.{SERVER_NAME}\]\n.*?(?=^\[|\Z)")
    if pattern.search(original):
        updated = pattern.sub(block + "\n", original)
        action = "updated"
    else:
        sep = "" if not original or original.endswith("\n") else "\n"
        updated = f"{original}{sep}\n{block}" if original else block
        action = "added"

    print(f"[codex] {action} [mcp_servers.{SERVER_NAME}] in {CODEX_CONFIG}")
    if backup_path is not None:
        print(f"[codex] backup: {backup_path}")
    if dry_run:
        print("[codex] dry-run enabled; no file changes written.")
        return

    CODEX_CONFIG.parent.mkdir(parents=True, exist_ok=True)
    CODEX_CONFIG.write_text(updated, encoding="utf-8")


def configure_claude(server_dir: Path, mode: str, dry_run: bool) -> None:
    backup_path = _backup(CLAUDE_CONFIG, dry_run=dry_run)
    if CLAUDE_CONFIG.exists():
        payload = json.loads(CLAUDE_CONFIG.read_text(encoding="utf-8"))
    else:
        payload = {}

    if not isinstance(payload, dict):
        raise ValueError(f"{CLAUDE_CONFIG} is not a JSON object.")

    mcp_servers = payload.setdefault("mcpServers", {})
    if not isinstance(mcp_servers, dict):
        raise ValueError(f"{CLAUDE_CONFIG}: mcpServers is not an object.")

    command, args = _command_spec(mode=mode, server_dir=server_dir)
    mcp_servers[SERVER_NAME] = {
        "command": command,
        "args": args,
    }

    print(f"[claude] configured mcpServers.{SERVER_NAME} in {CLAUDE_CONFIG}")
    if backup_path is not None:
        print(f"[claude] backup: {backup_path}")
    if dry_run:
        print("[claude] dry-run enabled; no file changes written.")
        return

    CLAUDE_CONFIG.parent.mkdir(parents=True, exist_ok=True)
    CLAUDE_CONFIG.write_text(
        json.dumps(payload, ensure_ascii=False, indent=2) + "\n",
        encoding="utf-8",
    )


def parse_args() -> argparse.Namespace:
    script_dir = Path(__file__).resolve().parent
    default_server_dir = script_dir.parent.resolve()

    parser = argparse.ArgumentParser(
        description=(
            "Configure Codex and Claude Desktop to run the suews-mcp server."
        )
    )
    parser.add_argument(
        "--mode",
        choices=("published", "local"),
        default="published",
        help=(
            "How clients should start the server: "
            "'published' uses `uvx suews-mcp` (default), "
            "'local' uses `uv run --directory <path> suews-mcp`."
        ),
    )
    parser.add_argument(
        "--target",
        choices=("both", "codex", "claude"),
        default="both",
        help="Which client(s) to configure.",
    )
    parser.add_argument(
        "--server-directory",
        type=Path,
        default=default_server_dir,
        help=(
            "Directory containing the local suews-mcp package "
            "(used in --mode local)."
        ),
    )
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Show planned changes without writing files.",
    )
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    server_dir = args.server_directory.expanduser().resolve()
    mode = args.mode

    if mode == "local" and not server_dir.exists():
        print(f"Server directory does not exist: {server_dir}", file=sys.stderr)
        return 2

    if mode == "published" and shutil.which("uvx") is None:
        print("Warning: `uvx` not found on PATH. Install uv before using this MCP server.")
    if mode == "local" and shutil.which("uv") is None:
        print("Warning: `uv` not found on PATH. Install uv before using this MCP server.")

    if mode == "published":
        print("Using published mode: uvx suews-mcp")
    else:
        print(f"Using local mode with server directory: {server_dir}")

    try:
        if args.target in {"both", "codex"}:
            configure_codex(server_dir=server_dir, mode=mode, dry_run=args.dry_run)
        if args.target in {"both", "claude"}:
            configure_claude(server_dir=server_dir, mode=mode, dry_run=args.dry_run)
    except Exception as exc:
        print(f"Configuration failed: {exc}", file=sys.stderr)
        return 1

    print("Done.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
