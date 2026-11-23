#!/usr/bin/env python3
"""Run curated offline Sphinx-Gallery smoke tutorials plus changed PR tutorials.

Used by the unprivileged ``.github/workflows/tutorial-smoke.yml`` pull-request
workflow (gh#2104 F-18) so broken tutorial API calls fail before merge without
re-enabling documentation builds inside the privileged ``pages-deploy.yml``
workflow (gh#1431).
"""

from __future__ import annotations

import argparse
from collections.abc import Mapping, Sequence
from concurrent.futures import ThreadPoolExecutor
from dataclasses import dataclass
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import time

REPO_ROOT = Path(__file__).resolve().parents[2]
DEFAULT_TUTORIALS_DIR = REPO_ROOT / "docs" / "source" / "tutorials"

# Fast, offline tutorials spanning core user-facing SuPy workflows:
# - tutorial_01_quick_start.py: sample-data OOP setup, run, and plots
# - tutorial_05_results_analysis.py: SUEWSOutput, resampling, energy/water budgets
# - tutorial_06_external_coupling.py: iterative coupling and state handoff
# - tutorial_07_attribution.py: attribution / sensitivity analysis workflow
#
# tutorial_02_setup_own_site.py is excluded from the default subset because its
# vendored US-AR1_2010_data_60.txt still carries a legacy xsmd column rejected
# by accept_legacy_soil_placeholder() (tracked separately); tutorials 03, 04,
# and 08 are slower multi-scenario/multi-grid examples executed in post-merge
# full docs builds unless modified by the pull request.
SMOKE_TUTORIALS: tuple[str, ...] = (
    "tutorial_01_quick_start.py",
    "tutorial_05_results_analysis.py",
    "tutorial_06_external_coupling.py",
    "tutorial_07_attribution.py",
)

# Documented network-only tutorials skipped even when changed on a PR.
# All eight current Sphinx-Gallery tutorials run offline, so this set is empty.
NETWORK_ONLY_TUTORIALS: frozenset[str] = frozenset()


@dataclass(frozen=True)
class TutorialResult:
    """Outcome of running a single tutorial script."""

    name: str
    returncode: int
    elapsed_seconds: float
    stdout: str
    stderr: str
    timed_out: bool = False

    @property
    def passed(self) -> bool:
        """Whether the tutorial exited cleanly without timeout."""
        return not self.timed_out and self.returncode == 0


def is_runnable_tutorial(
    path: Path,
    tutorials_dir: Path,
    *,
    network_only: frozenset[str] = NETWORK_ONLY_TUTORIALS,
) -> bool:
    """Return whether ``path`` is an existing runnable Sphinx-Gallery tutorial.

    Matches the ``filename_pattern`` (``tutorial_*.py``) and
    ``ignore_pattern`` (``__init__.py``) configured in ``docs/source/conf.py``
    and excludes any documented network-only tutorial filenames.

    Parameters
    ----------
    path : Path
        Candidate filesystem path.
    tutorials_dir : Path
        Root directory containing Sphinx-Gallery tutorials.
    network_only : frozenset[str], optional
        Tutorial filenames explicitly excluded because they require network.

    Returns
    -------
    bool
        ``True`` if ``path`` is a runnable tutorial file inside
        ``tutorials_dir``.
    """
    resolved_dir = tutorials_dir.resolve()
    resolved_path = path.resolve()
    if not resolved_path.is_file():
        return False
    if not resolved_path.is_relative_to(resolved_dir):
        return False
    name = resolved_path.name
    if name == "__init__.py" or name in network_only:
        return False
    return name.startswith("tutorial_") and resolved_path.suffix == ".py"


def discover_changed_files_from_git(
    base_ref: str,
    *,
    repo_root: Path = REPO_ROOT,
    tutorials_dir: Path = DEFAULT_TUTORIALS_DIR,
) -> list[Path]:
    """Return tutorial-directory paths changed since ``base_ref``.

    Runs ``git diff --name-status -z -M --diff-filter=ACDMR <base_ref>...HEAD``
    scoped to ``tutorials_dir``. Emits deleted paths and both old and new paths
    of renames/copies so deleting or renaming a data file under
    ``tutorials/data/`` still triggers every tutorial that references it.

    Parameters
    ----------
    base_ref : str
        Base commit SHA or git reference to diff against ``HEAD``.
    repo_root : Path, optional
        Git repository root directory.
    tutorials_dir : Path, optional
        Directory containing Sphinx-Gallery tutorials.

    Returns
    -------
    list[Path]
        Deduplicated absolute paths of added, copied, deleted, modified, or
        renamed files under ``tutorials_dir`` (including both sides of renames
        and copies).

    Raises
    ------
    RuntimeError
        If ``git diff`` exits with a non-zero status or emits malformed output.
    """
    resolved_root = repo_root.resolve()
    resolved_dir = tutorials_dir.resolve()
    try:
        pathspec = str(resolved_dir.relative_to(resolved_root))
    except ValueError:
        pathspec = str(resolved_dir)

    proc = subprocess.run(
        [
            "git",
            "-C",
            str(resolved_root),
            "diff",
            "--name-status",
            "-z",
            "-M",
            "--diff-filter=ACDMR",
            f"{base_ref}...HEAD",
            "--",
            pathspec,
        ],
        capture_output=True,
        text=True,
        check=False,
    )
    if proc.returncode != 0:
        raise RuntimeError(
            f"git diff failed for base {base_ref!r} (exit {proc.returncode}): "
            f"{proc.stderr.strip()}"
        )

    tokens = [tok for tok in proc.stdout.split("\0") if tok]
    changed: list[Path] = []
    seen: set[Path] = set()
    idx = 0
    while idx < len(tokens):
        status = tokens[idx]
        idx += 1
        if status.startswith(("R", "C")):
            if idx + 1 >= len(tokens):
                raise RuntimeError(
                    f"Malformed git diff --name-status -z output after {status!r}"
                )
            rel_paths = (tokens[idx], tokens[idx + 1])
            idx += 2
        else:
            if idx >= len(tokens):
                raise RuntimeError(
                    f"Malformed git diff --name-status -z output after {status!r}"
                )
            rel_paths = (tokens[idx],)
            idx += 1
        for rel_path in rel_paths:
            resolved = (resolved_root / rel_path).resolve(strict=False)
            if resolved not in seen:
                seen.add(resolved)
                changed.append(resolved)
    return changed


def _resolve_candidate_path(
    raw_path: Path,
    *,
    tutorials_dir: Path,
    repo_root: Path,
) -> Path | None:
    """Resolve a user- or git-supplied changed path for tutorial selection.

    Existing files resolve directly; non-existent paths (deleted files or the
    old side of renames) are preserved only when they lie under
    ``tutorials_dir / "data"`` so consuming tutorials are still executed, while
    deleted tutorial ``.py`` scripts are skipped cleanly.
    """
    data_dir = (tutorials_dir.resolve() / "data").resolve(strict=False)
    if raw_path.is_absolute():
        resolved = raw_path.resolve(strict=False)
        if resolved.is_file():
            return resolved
        if resolved != data_dir and resolved.is_relative_to(data_dir):
            return resolved
        return None
    for base in (repo_root, tutorials_dir, Path.cwd()):
        candidate = (base / raw_path).resolve()
        if candidate.is_file():
            return candidate
    for base in (repo_root, tutorials_dir, Path.cwd()):
        candidate = (base / raw_path).resolve(strict=False)
        if candidate != data_dir and candidate.is_relative_to(data_dir):
            return candidate
    return None


def find_tutorials_consuming_data_file(
    data_path: Path,
    tutorials_dir: Path,
    *,
    network_only: frozenset[str] = NETWORK_ONLY_TUTORIALS,
) -> list[Path]:
    """Return runnable tutorials that reference a changed file under ``data/``.

    Accepts deleted or renamed-away paths under ``tutorials_dir / "data"`` as
    well as existing files so removing or renaming a tutorial's data asset
    executes the consuming tutorial and surfaces the broken path.

    Parameters
    ----------
    data_path : Path
        Filesystem path to inspect (may no longer exist after deletion/rename).
    tutorials_dir : Path
        Root directory containing Sphinx-Gallery ``tutorial_*.py`` scripts and
        their ``data/`` subdirectory.
    network_only : frozenset[str], optional
        Tutorial filenames explicitly excluded because they require network.

    Returns
    -------
    list[Path]
        Sorted paths of runnable tutorials referencing ``data_path`` (empty if
        ``data_path`` does not reside under ``tutorials_dir / "data"``).
    """
    resolved_dir = tutorials_dir.resolve()
    data_dir = (resolved_dir / "data").resolve(strict=False)
    resolved_data = data_path.resolve(strict=False)
    if (
        resolved_data == data_dir
        or not resolved_data.is_relative_to(data_dir)
        or resolved_data.is_dir()
    ):
        return []

    rel_posix = resolved_data.relative_to(resolved_dir).as_posix()
    basename = resolved_data.name
    consumers: list[Path] = []
    for candidate in sorted(resolved_dir.rglob("tutorial_*.py")):
        if not is_runnable_tutorial(candidate, resolved_dir, network_only=network_only):
            continue
        source = candidate.read_text(encoding="utf-8", errors="replace")
        if basename in source or rel_posix in source:
            consumers.append(candidate.resolve())
    return consumers


def resolve_tutorial_paths(
    tutorials_dir: Path,
    names: Sequence[str] | None = None,
    *,
    changed_paths: Sequence[Path] | None = None,
    repo_root: Path = REPO_ROOT,
    network_only: frozenset[str] = NETWORK_ONLY_TUTORIALS,
) -> list[Path]:
    """Resolve the ordered union of base and changed runnable tutorial scripts.

    Parameters
    ----------
    tutorials_dir : Path
        Directory containing Sphinx-Gallery ``tutorial_*.py`` scripts.
    names : Sequence[str] | None, optional
        Specific tutorial filenames to run; defaults to ``SMOKE_TUTORIALS``.
    changed_paths : Sequence[Path] | None, optional
        Additional changed file paths (for example from a PR diff). Any
        existing runnable ``tutorial_*.py`` script under ``tutorials_dir``
        (excluding ``network_only``), as well as any runnable tutorial
        consuming an added, modified, deleted, or renamed file under
        ``tutorials_dir / "data"``, is unioned onto the run list; unrelated
        non-tutorial files and deleted tutorial scripts are ignored.
    repo_root : Path, optional
        Repository root used when resolving relative ``changed_paths``.
    network_only : frozenset[str], optional
        Tutorial filenames explicitly excluded because they require network.

    Returns
    -------
    list[Path]
        Deduplicated resolved filesystem paths (base tutorials first, followed
        by sorted extra changed runnable tutorials).

    Raises
    ------
    FileNotFoundError
        If ``tutorials_dir`` or any base tutorial file does not exist.
    """
    if not tutorials_dir.is_dir():
        raise FileNotFoundError(f"Tutorials directory not found: {tutorials_dir}")

    selected = list(names) if names else list(SMOKE_TUTORIALS)
    resolved: list[Path] = []
    seen: set[Path] = set()
    missing: list[str] = []
    for name in selected:
        candidate = (tutorials_dir / name).resolve()
        if not candidate.is_file():
            missing.append(name)
        elif candidate not in seen:
            seen.add(candidate)
            resolved.append(candidate)

    if missing:
        raise FileNotFoundError(
            f"Tutorial script(s) not found under {tutorials_dir}: {', '.join(missing)}"
        )

    resolved_dir = tutorials_dir.resolve()
    extras: list[Path] = []
    for raw_path in changed_paths or ():
        candidate = _resolve_candidate_path(
            raw_path,
            tutorials_dir=resolved_dir,
            repo_root=repo_root.resolve(),
        )
        if candidate is None:
            continue
        if is_runnable_tutorial(candidate, resolved_dir, network_only=network_only):
            if candidate not in seen:
                seen.add(candidate)
                extras.append(candidate)
            continue
        for consumer in find_tutorials_consuming_data_file(
            candidate,
            resolved_dir,
            network_only=network_only,
        ):
            if consumer not in seen:
                seen.add(consumer)
                extras.append(consumer)

    extras.sort(key=lambda p: p.relative_to(resolved_dir).as_posix())
    resolved.extend(extras)
    return resolved


def _coerce_text(value: str | bytes | None) -> str:
    """Normalise captured subprocess output into a ``str``."""
    if value is None:
        return ""
    if isinstance(value, bytes):
        return value.decode("utf-8", errors="replace")
    return value


def collect_package_provenance(
    python_executable: str = sys.executable,
    *,
    env: Mapping[str, str] | None = None,
) -> dict[str, str]:
    """Inspect imported ``supy`` and ``supy.suews_bridge`` provenance.

    Runs inside an isolated temporary working directory so repository source
    directories cannot shadow the installed environment under test.

    Parameters
    ----------
    python_executable : str, optional
        Python interpreter used to run the provenance probe.
    env : Mapping[str, str] | None, optional
        Optional environment override for the probe subprocess.

    Returns
    -------
    dict[str, str]
        Mapping with keys ``python``, ``supy_version``, ``supy_file``, and
        ``suews_bridge_file``.

    Raises
    ------
    RuntimeError
        If importing ``supy`` or ``supy.suews_bridge`` fails.
    """
    probe = (
        "import json, sys, supy\n"
        "import supy.suews_bridge as sb\n"
        "print(json.dumps({\n"
        "    'python': sys.executable,\n"
        "    'supy_version': str(getattr(supy, '__version__', '<unknown>')),\n"
        "    'supy_file': str(getattr(supy, '__file__', '<unknown>')),\n"
        "    'suews_bridge_file': str(getattr(sb, '__file__', '<unknown>')),\n"
        "}))\n"
    )
    run_env = dict(os.environ if env is None else env)
    run_env["MPLBACKEND"] = "Agg"
    run_env["PYTHONUTF8"] = "1"

    with tempfile.TemporaryDirectory(prefix="suews-tutorial-prov-") as tmp_dir:
        proc = subprocess.run(
            [python_executable, "-c", probe],
            cwd=tmp_dir,
            env=run_env,
            capture_output=True,
            text=True,
            check=False,
        )
    if proc.returncode != 0:
        raise RuntimeError(
            "Failed to inspect supy package provenance "
            f"(exit {proc.returncode}): {proc.stderr.strip()}"
        )
    lines = [line.strip() for line in proc.stdout.splitlines() if line.strip()]
    if not lines:
        raise RuntimeError("Package provenance probe emitted no output.")
    return json.loads(lines[-1])


def verify_provenance_prefix(
    provenance: Mapping[str, str],
    require_prefix: Path,
) -> None:
    """Ensure ``supy`` and ``supy.suews_bridge`` were loaded inside ``require_prefix``.

    Parameters
    ----------
    provenance : Mapping[str, str]
        Output from :func:`collect_package_provenance`.
    require_prefix : Path
        Expected virtual environment or installation prefix directory.

    Raises
    ------
    RuntimeError
        If either ``supy_file`` or ``suews_bridge_file`` resolves outside
        ``require_prefix``.
    """
    expected_root = require_prefix.resolve()
    for key in ("supy_file", "suews_bridge_file"):
        raw = provenance.get(key, "")
        if not raw or raw == "<unknown>":
            raise RuntimeError(f"Package provenance missing {key}: {provenance}")
        resolved = Path(raw).resolve()
        if not resolved.is_relative_to(expected_root):
            raise RuntimeError(
                f"Expected {key} inside {expected_root}, got stale/external path: {resolved}"
            )


def run_single_tutorial(
    script_path: Path,
    *,
    python_executable: str = sys.executable,
    timeout_seconds: float = 300.0,
) -> TutorialResult:
    """Execute one tutorial script in an isolated temporary working directory.

    Parameters
    ----------
    script_path : Path
        Absolute path to the tutorial Python script.
    python_executable : str, optional
        Python interpreter used to run the script (default: current interpreter).
    timeout_seconds : float, optional
        Per-tutorial wall-clock timeout in seconds (default: 300.0).

    Returns
    -------
    TutorialResult
        Captured exit status, elapsed wall seconds, and output streams.
    """
    env = dict(os.environ)
    py_bin_dir = str(Path(os.path.abspath(python_executable)).parent)
    existing_path = env.get("PATH", "")
    env["PATH"] = (
        f"{py_bin_dir}{os.pathsep}{existing_path}" if existing_path else py_bin_dir
    )
    env["MPLBACKEND"] = "Agg"
    env["CI"] = "true"
    env["PYTHONUTF8"] = "1"

    start = time.monotonic()
    with tempfile.TemporaryDirectory(prefix="suews-tutorial-smoke-") as tmp_dir:
        try:
            proc = subprocess.run(
                [python_executable, str(script_path)],
                cwd=tmp_dir,
                env=env,
                capture_output=True,
                text=True,
                timeout=timeout_seconds,
                check=False,
            )
            elapsed = time.monotonic() - start
            return TutorialResult(
                name=script_path.name,
                returncode=proc.returncode,
                elapsed_seconds=elapsed,
                stdout=proc.stdout,
                stderr=proc.stderr,
                timed_out=False,
            )
        except subprocess.TimeoutExpired as exc:
            elapsed = time.monotonic() - start
            return TutorialResult(
                name=script_path.name,
                returncode=124,
                elapsed_seconds=elapsed,
                stdout=_coerce_text(exc.stdout),
                stderr=_coerce_text(exc.stderr),
                timed_out=True,
            )


def run_tutorials(
    script_paths: Sequence[Path],
    *,
    workers: int = 2,
    timeout_seconds: float = 300.0,
    python_executable: str = sys.executable,
) -> list[TutorialResult]:
    """Execute a sequence of tutorial scripts, optionally in parallel.

    Parameters
    ----------
    script_paths : Sequence[Path]
        Tutorial script paths to execute.
    workers : int, optional
        Maximum concurrent subprocesses (default: 2).
    timeout_seconds : float, optional
        Per-tutorial timeout in seconds (default: 300.0).
    python_executable : str, optional
        Python interpreter used to run each tutorial.

    Returns
    -------
    list[TutorialResult]
        Results ordered to match ``script_paths``.
    """
    worker_count = max(1, min(workers, len(script_paths))) if script_paths else 1

    def _run(path: Path) -> TutorialResult:
        return run_single_tutorial(
            path,
            python_executable=python_executable,
            timeout_seconds=timeout_seconds,
        )

    if worker_count == 1:
        return [_run(path) for path in script_paths]

    with ThreadPoolExecutor(max_workers=worker_count) as pool:
        return list(pool.map(_run, script_paths))


def build_parser() -> argparse.ArgumentParser:
    """Create the CLI argument parser.

    Returns
    -------
    argparse.ArgumentParser
        Configured CLI parser.
    """
    parser = argparse.ArgumentParser(
        description=(
            "Run curated Sphinx-Gallery tutorial smoke scripts plus any "
            "changed runnable tutorial scripts from a pull request."
        )
    )
    parser.add_argument(
        "--tutorials-dir",
        type=Path,
        default=DEFAULT_TUTORIALS_DIR,
        help="Directory containing tutorial_*.py files (default: %(default)s).",
    )
    parser.add_argument(
        "--repo-root",
        type=Path,
        default=REPO_ROOT,
        help="Git repository root used with --changed-from-git-base (default: %(default)s).",
    )
    parser.add_argument(
        "--tutorial",
        action="append",
        dest="tutorials",
        default=[],
        help="Specific base tutorial filename to run (may be repeated; defaults to curated smoke subset).",
    )
    parser.add_argument(
        "--changed-file",
        action="append",
        dest="changed_files",
        type=Path,
        default=[],
        help="Changed file path to union if it is a runnable tutorial_*.py script (may be repeated).",
    )
    parser.add_argument(
        "--changed-from-git-base",
        default=None,
        help="Git base ref/SHA; union runnable tutorial_*.py scripts changed in <base>...HEAD.",
    )
    parser.add_argument(
        "--workers",
        type=int,
        default=2,
        help="Number of concurrent tutorial subprocesses (default: %(default)s).",
    )
    parser.add_argument(
        "--timeout",
        type=float,
        default=300.0,
        help="Per-tutorial timeout in seconds (default: %(default)s).",
    )
    parser.add_argument(
        "--require-prefix",
        type=Path,
        default=None,
        help="Require imported supy and supy.suews_bridge files to reside under this directory.",
    )
    parser.add_argument(
        "--skip-provenance",
        action="store_true",
        help="Skip supy/suews_bridge import provenance probe (for standalone script tests).",
    )
    parser.add_argument(
        "--list",
        action="store_true",
        help="Print resolved tutorial paths and exit without running.",
    )
    return parser


def main(argv: Sequence[str] | None = None) -> int:
    """Run the tutorial smoke check CLI.

    Parameters
    ----------
    argv : Sequence[str] | None, optional
        Explicit CLI arguments; defaults to ``sys.argv[1:]``.

    Returns
    -------
    int
        Exit status (``0`` if every tutorial passed, ``1`` otherwise).
    """
    args = build_parser().parse_args(argv)
    try:
        if args.workers < 1:
            raise ValueError("--workers must be >= 1")
        if args.timeout <= 0:
            raise ValueError("--timeout must be > 0")

        changed_paths: list[Path] = list(args.changed_files)
        if args.changed_from_git_base:
            changed_paths.extend(
                discover_changed_files_from_git(
                    args.changed_from_git_base,
                    repo_root=args.repo_root,
                    tutorials_dir=args.tutorials_dir,
                )
            )
        paths = resolve_tutorial_paths(
            args.tutorials_dir,
            args.tutorials or None,
            changed_paths=changed_paths,
            repo_root=args.repo_root,
        )
        if args.list:
            for path in paths:
                print(path)
            return 0

        if not args.skip_provenance:
            provenance = collect_package_provenance(sys.executable)
            if args.require_prefix is not None:
                verify_provenance_prefix(provenance, args.require_prefix)
            print("Package provenance:")
            print(f"  python:            {provenance['python']}")
            print(f"  supy.__version__:  {provenance['supy_version']}")
            print(f"  supy.__file__:     {provenance['supy_file']}")
            print(f"  suews_bridge:      {provenance['suews_bridge_file']}")
    except (FileNotFoundError, RuntimeError, ValueError) as exc:
        print(f"[X] {exc}", file=sys.stderr)
        return 1

    t0 = time.monotonic()
    results = run_tutorials(
        paths,
        workers=args.workers,
        timeout_seconds=args.timeout,
    )
    wall_total = time.monotonic() - t0

    failed: list[TutorialResult] = []
    for res in results:
        if res.passed:
            print(f"[OK] {res.name} ({res.elapsed_seconds:.2f}s)")
        else:
            failed.append(res)
            reason = "TIMEOUT" if res.timed_out else f"exit {res.returncode}"
            print(
                f"[X] {res.name} ({reason}, {res.elapsed_seconds:.2f}s)",
                file=sys.stderr,
            )
            if res.stderr.strip():
                tail = "\n".join(res.stderr.strip().splitlines()[-30:])
                print(f"--- stderr ({res.name}) ---\n{tail}", file=sys.stderr)
            elif res.stdout.strip():
                tail = "\n".join(res.stdout.strip().splitlines()[-30:])
                print(f"--- stdout ({res.name}) ---\n{tail}", file=sys.stderr)

    print(
        f"Tutorial smoke summary: {len(results) - len(failed)}/{len(results)} "
        f"passed in {wall_total:.2f}s wall time."
    )
    return 1 if failed else 0


if __name__ == "__main__":
    raise SystemExit(main())
