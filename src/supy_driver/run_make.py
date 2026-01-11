#!/usr/bin/env python3
"""Utility script invoked from Meson to build SUEWS via make."""

from __future__ import annotations

from pathlib import Path
import platform
import subprocess
import sys


def _preferred_fortran_compiler():
    """Return an absolute Fortran compiler path when one is required.

    On Apple Silicon, it's common to have an x86_64 gfortran in `/usr/local/bin`
    (Rosetta/Homebrew) and an arm64 gfortran in `/opt/homebrew/bin`. Mixing those
    artefacts breaks the arm64 Python extension link step (missing symbols).
    """
    if sys.platform != "darwin" or platform.machine() != "arm64":
        return None

    fc = Path("/opt/homebrew/bin/gfortran")
    return str(fc) if fc.exists() else None


def _compiler_signature(fc: str) -> str:
    version = ""
    try:
        version = subprocess.check_output([fc, "--version"], text=True).splitlines()[0]
    except Exception:
        version = "unknown"
    return f"{platform.system()} {platform.machine()} | {fc} | {version}"


def _run_make(
    args: list[str],
    suews_dir: Path,
    *,
    fc_override,
) -> subprocess.CompletedProcess[str]:
    make_cmd = ["make", "-C", str(suews_dir)]
    if fc_override:
        make_cmd.append(f"FC={fc_override}")
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
    fc_override = _preferred_fortran_compiler()
    sig_file = stamp_file.parent / f"{stamp_file.name}.fortran.sig"

    # If the Fortran compiler signature changed since last build, clean once to
    # avoid mixing object files/archives compiled for different architectures.
    if fc_override:
        sig = _compiler_signature(fc_override)
        previous_sig = sig_file.read_text().strip() if sig_file.exists() else ""
        if previous_sig != sig:
            sys.stdout.write(
                "\nDetected Fortran compiler change; cleaning SUEWS objects/modules before rebuilding.\n"
            )
            clean = _run_make(["clean-obj", "clean-mods"], suews_dir, fc_override=fc_override)
            if clean.returncode != 0:
                raise subprocess.CalledProcessError(clean.returncode, clean.args, output=clean.stdout)

    # Run make and propagate failures so Meson can fail fast.
    # On macOS it is common for Homebrew to upgrade gfortran, which leaves stale
    # Fortran module files (*.mod) that cannot be read by the new compiler.
    result = _run_make([], suews_dir, fc_override=fc_override)
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
            clean = _run_make(["clean-obj", "clean-mods"], suews_dir, fc_override=fc_override)
            if clean.returncode != 0:
                raise subprocess.CalledProcessError(clean.returncode, clean.args, output=clean.stdout)

            retry = _run_make([], suews_dir, fc_override=fc_override)
            if retry.returncode != 0:
                raise subprocess.CalledProcessError(retry.returncode, retry.args, output=retry.stdout)
        else:
            raise subprocess.CalledProcessError(result.returncode, result.args, output=result.stdout)

    if fc_override:
        sig_file.parent.mkdir(parents=True, exist_ok=True)
        sig_file.write_text(_compiler_signature(fc_override) + "\n")

    # Ensure an output file exists so Meson can track this custom target.
    stamp_file.parent.mkdir(parents=True, exist_ok=True)
    stamp_file.touch()


if __name__ == "__main__":
    main()
