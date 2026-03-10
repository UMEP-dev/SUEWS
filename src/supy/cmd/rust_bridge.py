"""CLI wrapper for the bundled Rust bridge binary."""

from __future__ import annotations

import os
import subprocess
import sys
from importlib.resources import as_file, files
from pathlib import Path


def _bridge_binary() -> Path:
    binary_name = "suews.exe" if os.name == "nt" else "suews"
    binary = files("supy").joinpath("bin").joinpath(binary_name)
    with as_file(binary) as path_obj:
        path = Path(path_obj)
        if not path.exists():
            raise FileNotFoundError(path)
        return path


def main() -> None:
    """Run the bundled Rust CLI, forwarding command-line arguments."""
    try:
        bridge_path = _bridge_binary()
    except FileNotFoundError as exc:
        raise RuntimeError(
            "Bundled Rust bridge CLI not found. Rebuild/install SuPy with Meson Rust bridge enabled."
        ) from exc

    cmd = [str(bridge_path), *sys.argv[1:]]
    result = subprocess.run(cmd, check=False)
    raise SystemExit(result.returncode)

