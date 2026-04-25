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


def main(argv: list[str] | None = None) -> None:
    """Run the bundled Rust CLI, forwarding command-line arguments.

    Parameters
    ----------
    argv : list[str] | None, optional
        Arguments to forward. When ``None`` (the default), ``sys.argv[1:]`` is
        used so the function still works as a stand-alone console script.
    """
    if argv is None:
        argv = sys.argv[1:]

    try:
        bridge_path = _bridge_binary()
    except FileNotFoundError as exc:
        raise RuntimeError(
            "Bundled Rust bridge CLI not found. Rebuild/install SuPy with Meson Rust bridge enabled."
        ) from exc

    cmd = [str(bridge_path), *argv]
    result = subprocess.run(cmd, check=False)
    raise SystemExit(result.returncode)
