#!/usr/bin/env python3
"""Utility script invoked from Meson to build SUEWS via make."""

from __future__ import annotations

from collections.abc import Mapping
import os
from pathlib import Path
import platform
import subprocess
import sys

BUILD_PROFILES = ("release", "checked")
DEFAULT_BUILD_PROFILE = "release"
BUILD_PROFILE_ENV = "SUEWS_BUILD_PROFILE"
MAKE_JOBS_ENV = "SUEWS_MAKE_JOBS"
# Cap on the default job count. The module dependency graph is mostly a
# chain (types -> utilities -> physics -> driver), so jobs beyond this add
# memory pressure on CI runners without shortening the build.
MAX_DEFAULT_MAKE_JOBS = 8


def build_profile_from_env(environ: Mapping[str, str] | None = None) -> str:
    """Return the Fortran build profile selected by ``SUEWS_BUILD_PROFILE``.

    ``release`` (the default) compiles the physics library with ``-O3`` and
    no runtime checks; ``checked`` compiles with ``-O0 -fcheck=all`` and is
    what the nightly physics tier runs alongside the release wheels. Both keep ``-finit-real=zero`` and neither arms FPE traps; the
    flag sets live in ``src/suews/Makefile.gfortran``. The Rust bridge reads
    the same variable in ``src/suews_bridge/build.rs``.
    """
    source = os.environ if environ is None else environ
    raw = source.get(BUILD_PROFILE_ENV, "")
    profile = raw.strip().lower() or DEFAULT_BUILD_PROFILE
    if profile not in BUILD_PROFILES:
        raise SystemExit(
            f"{BUILD_PROFILE_ENV} must be one of {', '.join(BUILD_PROFILES)}; "
            f"got {raw!r}"
        )
    return profile


def make_args_for_profile(profile: str) -> list[str]:
    """Map a build profile to the ``DEBUG`` variable the SUEWS Makefile reads.

    A command-line ``DEBUG=`` (empty) reaches the Makefile's ``ifndef DEBUG``
    as undefined, because ``ifdef`` tests for a non-empty value, and a
    command-line variable overrides the ``DEBUG ?= 1`` default inside the
    Makefile. So the profile is decided here, never by the Makefile default.
    """
    if profile == "checked":
        return ["DEBUG=1"]
    if profile == "release":
        return ["DEBUG="]
    raise ValueError(f"unknown build profile {profile!r}")


def make_jobs_from_env(
    environ: Mapping[str, str] | None = None,
    cpu_count: int | None = None,
) -> int:
    """Return the number of parallel ``make`` jobs for the Fortran build.

    ``SUEWS_MAKE_JOBS`` sets it explicitly (``1`` restores a serial build);
    otherwise it is the CPU count capped at ``MAX_DEFAULT_MAKE_JOBS``. Parallel
    builds rely on the module dependency edges in ``src/suews/Makefile.deps``
    (gh#1790).
    """
    source = os.environ if environ is None else environ
    raw = source.get(MAKE_JOBS_ENV, "").strip()
    if raw:
        try:
            jobs = int(raw)
        except ValueError:
            jobs = 0
        if jobs < 1:
            raise SystemExit(f"{MAKE_JOBS_ENV} must be a positive integer; got {raw!r}")
        return jobs
    cpus = os.cpu_count() if cpu_count is None else cpu_count
    return max(1, min(cpus or 1, MAX_DEFAULT_MAKE_JOBS))


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
    jobs: int = 1,
) -> subprocess.CompletedProcess[str]:
    make_cmd = ["make", "-C", str(suews_dir), f"-j{jobs}"]
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
    profile = build_profile_from_env()
    profile_args = make_args_for_profile(profile)
    jobs = make_jobs_from_env()
    sys.stdout.write(
        f"SUEWS Fortran build profile: {profile} "
        f"({BUILD_PROFILE_ENV}={os.environ.get(BUILD_PROFILE_ENV, '') or 'unset'}), "
        f"make jobs: {jobs}\n"
    )

    # Run make and propagate failures so Meson can fail fast.
    # On macOS it is common for Homebrew to upgrade gfortran, which leaves stale
    # Fortran module files (*.mod) that cannot be read by the new compiler.
    result = _run_make(profile_args, suews_dir, fc, jobs)
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
                raise subprocess.CalledProcessError(
                    clean.returncode, clean.args, output=clean.stdout
                )

            retry = _run_make(profile_args, suews_dir, fc, jobs)
            if retry.returncode != 0:
                raise subprocess.CalledProcessError(
                    retry.returncode, retry.args, output=retry.stdout
                )
        else:
            raise subprocess.CalledProcessError(
                result.returncode, result.args, output=result.stdout
            )

    # Ensure an output file exists so Meson can track this custom target.
    stamp_file.parent.mkdir(parents=True, exist_ok=True)
    stamp_file.touch()


if __name__ == "__main__":
    main()
