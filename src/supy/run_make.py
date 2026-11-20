#!/usr/bin/env python3
"""Utility script invoked from Meson to build SUEWS via make."""

from __future__ import annotations

from collections.abc import Iterator, Mapping
import contextlib
import errno
import hashlib
import json
import os
from pathlib import Path
import platform
import subprocess
import sys

BUILD_PROFILES = ("release", "checked")
DEFAULT_BUILD_PROFILE = "release"
BUILD_PROFILE_ENV = "SUEWS_BUILD_PROFILE"
INIT_REAL_ENV = "SUEWS_INIT_REAL"
DEFAULT_INIT_REAL = "zero"
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


_EXT_LIB_SOURCE_SUFFIXES = {".f90", ".f95", ".f", ".c", ".h", ".inc", ".mk"}
# Compile-flag environment variables read by src/suews/Makefile* together with
# their effective default when unset (matching Makefile.gfortran's `?=` rules).
_MAKE_ENV_DEFAULTS: dict[str, str] = {
    "PROFILE": "",
    "SINGLE_PRECISION": "",
    "NETCDF4": "",
    INIT_REAL_ENV: DEFAULT_INIT_REAL,
}
_MAKE_ENV_KEYS = tuple(_MAKE_ENV_DEFAULTS)
_IGNORED_BUILD_CONFIG_NAMES = frozenset({"Makefile.deps"})
_IGNORED_EXT_LIB_DIRS = frozenset({"doc", "docs", "test", "tests"})
_BUILD_ARTIFACT_PATTERNS = ("*.o", "*.mod", "lib*.a")
BUILD_FINGERPRINT_FILENAME = ".suews_build_fingerprint"
BUILD_LOCK_FILENAME = ".suews_build.lock"


def _is_build_config_file(path: Path) -> bool:
    """Return True if ``path`` is a build Makefile or included ``.mk`` fragment.

    Excludes ``Makefile.deps`` (module dependency edges only; GNU Make rebuilds
    affected targets incrementally without wiping all compiled objects).
    """
    if not path.is_file() or path.name in _IGNORED_BUILD_CONFIG_NAMES:
        return False
    return path.name.startswith("Makefile") or path.suffix.lower() == ".mk"


def collect_build_config_files(suews_dir: Path) -> list[Path]:
    """Return sorted build-configuration Makefiles under ``suews_dir``.

    Excludes ``Makefile.deps`` (dependency edges handled incrementally by Make)
    and unused documentation/test Makefiles under ``ext_lib``.
    """
    config_files: list[Path] = [
        path for path in suews_dir.iterdir() if _is_build_config_file(path)
    ]

    ext_lib_dir = suews_dir / "ext_lib"
    if ext_lib_dir.is_dir():
        for path in ext_lib_dir.rglob("*"):
            if not _is_build_config_file(path):
                continue
            rel_parent_parts = path.relative_to(ext_lib_dir).parent.parts
            if any(part in _IGNORED_EXT_LIB_DIRS for part in rel_parent_parts):
                continue
            config_files.append(path)

    return sorted(
        config_files,
        key=lambda path: path.relative_to(suews_dir).as_posix(),
    )


def compute_build_fingerprint(
    suews_dir: Path,
    fc: str | None,
    profile: str,
    profile_args: list[str],
    environ: Mapping[str, str] | None = None,
) -> str:
    """Compute a deterministic SHA-256 fingerprint of effective build inputs."""
    source_env = os.environ if environ is None else environ
    file_hashes = [
        [
            path.relative_to(suews_dir).as_posix(),
            hashlib.sha256(path.read_bytes()).hexdigest(),
        ]
        for path in collect_build_config_files(suews_dir)
    ]
    make_env = {
        key: source_env.get(key, default) for key, default in _MAKE_ENV_DEFAULTS.items()
    }
    payload = {
        "fc": fc or "",
        "profile": profile,
        "profile_args": list(profile_args),
        "make_env": make_env,
        "files": file_hashes,
    }
    encoded = json.dumps(payload, sort_keys=True, separators=(",", ":")).encode("utf-8")
    return hashlib.sha256(encoded).hexdigest()


def fingerprint_path_for_suews(suews_dir: Path) -> Path:
    """Return the shared object-tree fingerprint path inside ``suews_dir``."""
    return suews_dir / BUILD_FINGERPRINT_FILENAME


def clean_build_artifacts(suews_dir: Path) -> list[Path]:
    """Remove compiled Fortran/C objects, modules, and archives under ``suews_dir``."""
    removed: list[Path] = []
    for pattern in _BUILD_ARTIFACT_PATTERNS:
        for path in sorted(suews_dir.rglob(pattern)):
            if path.is_file() and not path.is_symlink():
                path.unlink()
                removed.append(path)
    return sorted(removed)


def invalidate_stale_artifacts(
    suews_dir: Path,
    fingerprint: str,
) -> list[Path]:
    """Clean compiled artefacts when the shared ``suews_dir`` fingerprint changed."""
    fp_path = fingerprint_path_for_suews(suews_dir)
    previous = (
        fp_path.read_text(encoding="utf-8").strip() if fp_path.is_file() else None
    )
    if previous == fingerprint:
        return []

    removed = clean_build_artifacts(suews_dir)
    if fp_path.exists():
        fp_path.unlink()
    return removed


def write_build_fingerprint(suews_dir: Path, fingerprint: str) -> Path:
    """Write ``fingerprint`` beside the shared objects in ``suews_dir``.

    Leave the file untouched when its content already matches ``fingerprint`` so
    its modification time stays stable across incremental builds with the same
    configuration.
    """
    fp_path = fingerprint_path_for_suews(suews_dir)
    expected = f"{fingerprint}\n"
    if fp_path.is_file() and fp_path.read_text(encoding="utf-8") == expected:
        return fp_path
    fp_path.write_text(expected, encoding="utf-8")
    return fp_path


@contextlib.contextmanager
def _suews_build_lock(suews_dir: Path) -> Iterator[None]:
    """Serialise concurrent builds sharing the in-tree ``suews_dir`` objects."""
    lock_path = suews_dir / BUILD_LOCK_FILENAME
    with lock_path.open("a+b") as lock_file:
        if os.name == "nt":
            import msvcrt  # noqa: PLC0415

            lock_file.seek(0)
            if lock_path.stat().st_size == 0:
                lock_file.write(b"\0")
                lock_file.flush()
            lock_file.seek(0)
            while True:
                try:
                    msvcrt.locking(lock_file.fileno(), msvcrt.LK_LOCK, 1)
                    break
                except OSError as exc:
                    if exc.errno != errno.EDEADLK:
                        raise
            try:
                yield
            finally:
                lock_file.seek(0)
                msvcrt.locking(lock_file.fileno(), msvcrt.LK_UNLCK, 1)
        else:
            import fcntl  # noqa: PLC0415

            fcntl.flock(lock_file.fileno(), fcntl.LOCK_EX)
            try:
                yield
            finally:
                fcntl.flock(lock_file.fileno(), fcntl.LOCK_UN)


def _escape_depfile_token(token: str) -> str:
    """Escape a path for Ninja/Make depfile consumption."""
    return token.replace("$", "$$").replace(" ", "\\ ")


def collect_ext_lib_inputs(suews_dir: Path) -> list[Path]:
    """Return sorted vendored source/build inputs under ``suews_dir/ext_lib``."""
    ext_lib_dir = suews_dir / "ext_lib"
    if not ext_lib_dir.is_dir():
        return []
    inputs: list[Path] = []
    for path in sorted(ext_lib_dir.rglob("*")):
        if not path.is_file():
            continue
        if path.suffix.lower() in _EXT_LIB_SOURCE_SUFFIXES or path.name.startswith(
            "Makefile"
        ):
            inputs.append(path.resolve())
    return inputs


def collect_depfile_inputs(suews_dir: Path) -> list[Path]:
    """Return sorted depfile inputs including ``ext_lib`` and shared fingerprint."""
    deps = collect_ext_lib_inputs(suews_dir)
    fp_path = fingerprint_path_for_suews(suews_dir).resolve()
    if fp_path.is_file():
        deps.append(fp_path)
    return sorted(set(deps))


def write_depfile(depfile: Path, target: str, deps: list[Path]) -> None:
    """Write a single-target Ninja-compatible depfile."""
    depfile.parent.mkdir(parents=True, exist_ok=True)
    target_tok = _escape_depfile_token(target.replace("\\", "/"))
    dep_toks = [_escape_depfile_token(p.as_posix()) for p in deps]
    content = (
        f"{target_tok}: " + " \\\n  ".join(dep_toks) + "\n"
        if dep_toks
        else f"{target_tok}:\n"
    )
    depfile.write_text(content, encoding="utf-8")


def _run_make_with_retry(
    profile_args: list[str],
    suews_dir: Path,
    fc: str | None,
    jobs: int,
) -> None:
    """Run ``make`` and retry once after cleaning if stale ``.mod`` files fail."""
    # On macOS it is common for Homebrew to upgrade gfortran, which leaves stale
    # Fortran module files (*.mod) that cannot be read by the new compiler.
    result = _run_make(profile_args, suews_dir, fc, jobs)
    if result.returncode == 0:
        return

    stale_mod_patterns = (
        "created by a different version of GNU Fortran",
        "Cannot read module file",
    )
    if any(pat in result.stdout for pat in stale_mod_patterns):
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
        return

    raise subprocess.CalledProcessError(
        result.returncode, result.args, output=result.stdout
    )


def _record_build_outputs(
    suews_dir: Path,
    raw_stamp: str,
    stamp_file: Path,
    depfile: Path | None,
    fingerprint: str,
) -> None:
    """Persist shared fingerprint, touch Meson's stamp, and write depfile."""
    # Record the active shared object-tree fingerprint before updating
    # Meson's per-builddir stamp so mtime(stamp) >= mtime(fingerprint).
    fp_path = write_build_fingerprint(suews_dir, fingerprint)
    stamp_file.parent.mkdir(parents=True, exist_ok=True)
    stamp_file.touch()
    stamp_stat = stamp_file.stat()
    fp_stat = fp_path.stat()
    if stamp_stat.st_mtime_ns < fp_stat.st_mtime_ns:
        os.utime(stamp_file, ns=(fp_stat.st_atime_ns, fp_stat.st_mtime_ns))
    if depfile is not None:
        write_depfile(depfile, raw_stamp, collect_depfile_inputs(suews_dir))


def main() -> None:
    if len(sys.argv) not in {3, 4}:
        raise SystemExit("Usage: run_make.py <suews_dir> <stamp_file> [depfile]")

    suews_dir = Path(sys.argv[1]).resolve()
    raw_stamp = sys.argv[2]
    stamp_file = Path(raw_stamp)
    depfile = Path(sys.argv[3]) if len(sys.argv) == 4 else None

    fc = _pick_suews_fc()
    profile = build_profile_from_env()
    profile_args = make_args_for_profile(profile)
    jobs = make_jobs_from_env()
    sys.stdout.write(
        f"SUEWS Fortran build profile: {profile} "
        f"({BUILD_PROFILE_ENV}={os.environ.get(BUILD_PROFILE_ENV, '') or 'unset'}), "
        f"make jobs: {jobs}\n"
    )

    with _suews_build_lock(suews_dir):
        fingerprint = compute_build_fingerprint(suews_dir, fc, profile, profile_args)
        removed = invalidate_stale_artifacts(suews_dir, fingerprint)
        if removed:
            sys.stdout.write(
                f"Build configuration changed; cleaned {len(removed)} compiled "
                "Fortran/C build artefacts before rebuild.\n"
            )

        _run_make_with_retry(profile_args, suews_dir, fc, jobs)
        _record_build_outputs(suews_dir, raw_stamp, stamp_file, depfile, fingerprint)


if __name__ == "__main__":
    main()
