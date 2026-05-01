"""CLI wrapper for the bundled Rust bridge binary."""

from __future__ import annotations

from collections.abc import Sequence
from importlib.resources import as_file, files
import os
from pathlib import Path
import subprocess
import sys


def _bridge_binary() -> Path:
    binary_name = "suews-engine.exe" if os.name == "nt" else "suews-engine"
    binary = files("supy").joinpath("bin").joinpath(binary_name)
    with as_file(binary) as path_obj:
        path = Path(path_obj)
        if not path.exists():
            raise FileNotFoundError(path)
        return path


def main(argv: Sequence[str] | None = None) -> None:
    """Run the bundled Rust CLI, forwarding command-line arguments."""
    try:
        bridge_path = _bridge_binary()
    except FileNotFoundError as exc:
        raise RuntimeError(
            "Bundled Rust bridge CLI not found. Rebuild/install SuPy with Meson Rust bridge enabled."
        ) from exc

    args = sys.argv[1:] if argv is None else list(argv)
    cmd = [str(bridge_path), *args]
    result = subprocess.run(cmd, check=False)
    raise SystemExit(result.returncode)
