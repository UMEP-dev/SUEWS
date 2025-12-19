#!/usr/bin/env python3
"""Utility script invoked from Meson to build SUEWS via make."""

from __future__ import annotations

import os
import platform
import subprocess
import sys
from pathlib import Path


def _pick_suews_fc() -> str | None:
    """Pick a Fortran compiler for building the legacy SUEWS Makefile targets.

    On Apple Silicon it's common to have an Intel Homebrew in /usr/local alongside
    an arm64 Homebrew in /opt/homebrew. If the Makefile uses the wrong gfortran,
    it can generate incompatible .mod files and break incremental builds.
    """
    fc = os.environ.get("SUEWS_FC") or os.environ.get("FC")
    if fc:
        return fc

    if sys.platform == "darwin" and platform.machine() == "arm64":
        fc_opt = Path("/opt/homebrew/bin/gfortran")
        if fc_opt.exists():
            return str(fc_opt)

    return None


def _run_make(
    args: list[str],
    suews_dir: Path,
    fc: str | None,
) -> subprocess.CompletedProcess[str]:
    make_cmd = ["make", "-C", str(suews_dir)]
    if fc:
        make_cmd.append(f"FC={fc}")
    make_cmd.extend(args)

    result = subprocess.run(
        make_cmd,
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
        check=False,
    )
    sys.stdout.write(result.stdout)
    return result


def main() -> None:
    if len(sys.argv) != 3:
        raise SystemExit("Usage: run_make.py <suews_dir> <stamp_file>")

    suews_dir = Path(sys.argv[1]).resolve()
    stamp_file = Path(sys.argv[2])

    fc = _pick_suews_fc()

    # Run make and propagate failures so Meson can fail fast.
    # On macOS it is common for Homebrew to upgrade gfortran, which leaves stale
    # Fortran module files (*.mod) that cannot be read by the new compiler.
    result = _run_make([], suews_dir, fc)
    if result.returncode != 0:
        output = result.stdout
        stale_mod_patterns = (
            "created by a different version of GNU Fortran",
            "Cannot read module file",
        )
        if any(pat in output for pat in stale_mod_patterns):
            sys.stdout.write(
                "\nDetected stale Fortran build artifacts; cleaning *.o and *.mod and retrying.\n"
            )
            clean = _run_make(["clean-obj", "clean-mods"], suews_dir, fc)
            if clean.returncode != 0:
                raise subprocess.CalledProcessError(clean.returncode, clean.args, output=clean.stdout)

            retry = _run_make([], suews_dir, fc)
            if retry.returncode != 0:
                raise subprocess.CalledProcessError(retry.returncode, retry.args, output=retry.stdout)
        else:
            raise subprocess.CalledProcessError(result.returncode, result.args, output=result.stdout)

    # Ensure an output file exists so Meson can track this custom target.
    stamp_file.parent.mkdir(parents=True, exist_ok=True)
    stamp_file.touch()


if __name__ == "__main__":
    main()
