"""Command-line entrypoint for the SUEWS MCP server."""

from __future__ import annotations

import argparse
from collections.abc import Sequence
import importlib.util
import logging
from pathlib import Path
import shutil
import sys

from .tools import ALLOWED_COMMANDS, resolve_root

EXIT_USAGE = 2


def _stderr(message: str) -> None:
    """Write one line to stderr."""
    print(f"suews-mcp: {message}", file=sys.stderr)


def _dependency_available(name: str) -> bool:
    """Return whether a Python dependency can be imported."""
    return importlib.util.find_spec(name) is not None


def _missing_executables() -> list[str]:
    """Return missing SUEWS CLI executables."""
    return [command for command in ALLOWED_COMMANDS if shutil.which(command) is None]


def build_parser() -> argparse.ArgumentParser:
    """Build the command-line parser."""
    parser = argparse.ArgumentParser(
        prog="suews-mcp",
        description="Run the SUEWS MCP server over stdio.",
    )
    parser.add_argument("--root", help="Absolute SUEWS project root")
    parser.add_argument(
        "--verbose",
        action="store_true",
        help="Enable informational server logs on stderr",
    )
    parser.add_argument(
        "--debug",
        action="store_true",
        help="Enable debug server logs on stderr",
    )
    return parser


def configure_logging(*, verbose: bool = False, debug: bool = False) -> logging.Logger:
    """Configure server-side logging to stderr."""
    level = logging.DEBUG if debug else logging.INFO if verbose else logging.WARNING
    logging.basicConfig(
        level=level,
        stream=sys.stderr,
        format="%(levelname)s:%(name)s:%(message)s",
    )
    return logging.getLogger("suews_mcp")


def validate_environment(root: str | None) -> Path | None:
    """Validate runtime requirements and return the resolved root."""
    error = None
    path_root = None

    if sys.version_info < (3, 10):
        error = "Python >=3.10 is required"
    elif not root:
        error = "--root is required"
    else:
        try:
            path_root = resolve_root(Path(root))
        except Exception as exc:
            error = f"invalid --root: {exc}"

    if error is None and not _dependency_available("mcp"):
        error = "missing dependency `mcp`; install with `pip install suews-mcp`"
    if error is None and not _dependency_available("supy"):
        error = "missing dependency `supy`; install a compatible SUEWS/SuPy release"

    if error is None:
        missing = _missing_executables()
        if missing:
            error = "missing SUEWS CLI executable(s): " + ", ".join(missing)

    if error is not None:
        _stderr(error)
        return None

    assert path_root is not None
    return path_root


def main(argv: Sequence[str] | None = None) -> int:
    """Run the SUEWS MCP server."""
    parser = build_parser()
    args = parser.parse_args(argv)
    logger = configure_logging(verbose=args.verbose, debug=args.debug)
    path_root = validate_environment(args.root)
    if path_root is None:
        return EXIT_USAGE

    from .server import create_server  # noqa: PLC0415

    server = create_server(path_root, logger=logger)
    server.run()
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
