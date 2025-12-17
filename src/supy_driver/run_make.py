#!/usr/bin/env python3
"""Utility script invoked from Meson to build SUEWS via make."""

from __future__ import annotations

import subprocess
import sys
from dataclasses import dataclass
from pathlib import Path


@dataclass(frozen=True)
class _MakeOverrides:
    fc: str | None = None
    cc: str | None = None


def _run_make(
    args: list[str], suews_dir: Path, overrides: _MakeOverrides
) -> subprocess.CompletedProcess[str]:
    make_vars: list[str] = []
    if overrides.fc:
        make_vars.append(f"FC={overrides.fc}")
    if overrides.cc:
        make_vars.append(f"CC={overrides.cc}")

    result = subprocess.run(
        ["make", "-C", str(suews_dir), *make_vars, *args],
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
        check=False,
    )
    sys.stdout.write(result.stdout)
    return result


def _archs(path: Path) -> set[str]:
    # `lipo -archs` works for Mach-O libs/archives on macOS; on other platforms it will fail.
    try:
        p = subprocess.run(
            ["lipo", "-archs", str(path)],
            text=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.DEVNULL,
            check=False,
        )
    except FileNotFoundError:
        return set()
    if p.returncode != 0:
        return set()
    return set(p.stdout.strip().split())


def _archs_from_file(path: Path) -> set[str]:
    try:
        p = subprocess.run(
            ["file", str(path)],
            text=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.DEVNULL,
            check=False,
        )
    except FileNotFoundError:
        return set()
    if p.returncode != 0:
        return set()
    out = p.stdout
    archs: set[str] = set()
    for arch in ("arm64", "arm64e", "x86_64"):
        if arch in out:
            archs.add(arch)
    return archs


def _expected_arch() -> str | None:
    try:
        p = subprocess.run(
            ["uname", "-m"],
            text=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.DEVNULL,
            check=False,
        )
    except FileNotFoundError:
        return None
    if p.returncode != 0:
        return None
    return p.stdout.strip() or None


def main() -> None:
    if len(sys.argv) not in (3, 4, 5):
        raise SystemExit("Usage: run_make.py <suews_dir> <stamp_file> [fc] [cc]")

    suews_dir = Path(sys.argv[1]).resolve()
    stamp_file = Path(sys.argv[2])
    overrides = _MakeOverrides(
        fc=sys.argv[3] if len(sys.argv) >= 4 else None,
        cc=sys.argv[4] if len(sys.argv) >= 5 else None,
    )

    # If we are overriding compilers, ensure we don't reuse stale artifacts built for a
    # different architecture (common on Apple Silicon when both x86_64 and arm64 GCC exist).
    expected = _expected_arch()
    if expected:
        lib_candidate = suews_dir / "lib" / "libsuewsdriver.a"
        archs: set[str] = set()
        if lib_candidate.exists():
            archs = _archs(lib_candidate)
        if not archs:
            # Some archives can become "mixed-arch" and fail `lipo`; fall back to a representative object file.
            obj_candidate = suews_dir / "src" / "suews_ctrl_const.o"
            if obj_candidate.exists():
                archs = _archs_from_file(obj_candidate)

        if archs and expected not in archs:
            sys.stdout.write(
                f"\nDetected mismatched SUEWS build artifacts architecture {sorted(archs)} "
                f"(expected {expected}); cleaning and rebuilding.\n"
            )
            clean = _run_make(["clean-obj", "clean-mods", "clean-lib"], suews_dir, overrides)
            if clean.returncode != 0:
                raise subprocess.CalledProcessError(
                    clean.returncode, clean.args, output=clean.stdout
                )

    # Run make and propagate failures so Meson can fail fast.
    # On macOS it is common for Homebrew to upgrade gfortran, which leaves stale
    # Fortran module files (*.mod) that cannot be read by the new compiler.
    result = _run_make([], suews_dir, overrides)
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
            clean = _run_make(["clean-obj", "clean-mods"], suews_dir, overrides)
            if clean.returncode != 0:
                raise subprocess.CalledProcessError(clean.returncode, clean.args, output=clean.stdout)

            retry = _run_make([], suews_dir, overrides)
            if retry.returncode != 0:
                raise subprocess.CalledProcessError(retry.returncode, retry.args, output=retry.stdout)
        else:
            raise subprocess.CalledProcessError(result.returncode, result.args, output=result.stdout)

    # Ensure an output file exists so Meson can track this custom target.
    stamp_file.parent.mkdir(parents=True, exist_ok=True)
    stamp_file.touch()


if __name__ == "__main__":
    main()
