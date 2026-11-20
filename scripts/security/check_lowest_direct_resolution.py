#!/usr/bin/env python3
"""Verify declared lower bounds and ``uv pip compile --resolution=lowest-direct``.

Ensures every core runtime dependency in ``pyproject.toml`` declares an
explicit lower bound and resolves cleanly under ``uv``'s ``lowest-direct``
strategy across supported platforms on the minimum Python version (3.12),
without unpinned-dependency warnings.
"""

from __future__ import annotations

import argparse
from collections.abc import Sequence
from pathlib import Path
import re
import subprocess
import sys
import tomllib

DEFAULT_PLATFORMS = (
    "x86_64-unknown-linux-gnu",
    "aarch64-apple-darwin",
    "x86_64-pc-windows-msvc",
)

_NAME_RE = re.compile(r"^\s*([A-Za-z0-9](?:[A-Za-z0-9._-]*[A-Za-z0-9])?)")
_LOWER_BOUND_RE = re.compile(r"(?:>=|==|~=|(?<![!<>=~])>(?!=))")


def normalise_package_name(name: str) -> str:
    """Return the PEP 503 normalised distribution name.

    Parameters
    ----------
    name : str
        Raw distribution name from a PEP 508 requirement string.

    Returns
    -------
    str
        Lowercase distribution name with runs of ``.``, ``_``, and ``-``
        collapsed to ``-``.
    """
    return re.sub(r"[-_.]+", "-", name).lower()


def parse_requirement_floor(requirement: str) -> tuple[str, bool]:
    """Extract the normalised package name and whether a lower bound is present.

    Parameters
    ----------
    requirement : str
        PEP 508 requirement string (optionally with extras and markers).

    Returns
    -------
    tuple[str, bool]
        ``(normalised_name, has_lower_bound)`` where ``has_lower_bound`` is
        ``True`` when the version specifier contains ``>=``, ``>``, ``==``,
        or ``~=``.
    """
    head = requirement.split(";", maxsplit=1)[0].strip()
    match = _NAME_RE.match(head)
    if match is None:
        raise ValueError(f"Cannot parse package name from requirement: {requirement!r}")

    name = normalise_package_name(match.group(1))
    remainder = head[match.end() :].strip()
    if remainder.startswith("["):
        close_idx = remainder.find("]")
        if close_idx == -1:
            raise ValueError(f"Unclosed extras bracket in requirement: {requirement!r}")
        remainder = remainder[close_idx + 1 :].strip()

    has_lower = bool(_LOWER_BOUND_RE.search(remainder))
    return name, has_lower


def read_core_dependencies(path_pyproject: Path) -> list[str]:
    """Read ``[project].dependencies`` from ``pyproject.toml``.

    Parameters
    ----------
    path_pyproject : Path
        Path to ``pyproject.toml``.

    Returns
    -------
    list[str]
        Core runtime dependency strings declared in ``[project].dependencies``.
    """
    data = tomllib.loads(path_pyproject.read_text(encoding="utf-8"))
    project = data.get("project", {})
    dependencies = project.get("dependencies", [])
    if not isinstance(dependencies, list):
        raise ValueError("[project].dependencies must be a list of requirement strings")
    return [str(dep) for dep in dependencies]


def find_missing_lower_bounds(requirements: Sequence[str]) -> list[str]:
    """Return every requirement string that lacks an explicit lower bound.

    Parameters
    ----------
    requirements : Sequence[str]
        PEP 508 requirement strings to inspect.

    Returns
    -------
    list[str]
        Subset of ``requirements`` with no lower-bound operator.
    """
    missing: list[str] = []
    for req in requirements:
        _, has_lower = parse_requirement_floor(req)
        if not has_lower:
            missing.append(req)
    return missing


def parse_uv_compile_result(
    returncode: int,
    stdout: str,
    stderr: str,
) -> tuple[dict[str, str], list[str]]:
    """Validate ``uv pip compile --resolution=lowest-direct`` output.

    Parameters
    ----------
    returncode : int
        Process exit status returned by ``uv pip compile``.
    stdout : str
        Captured standard output containing pinned ``name==version`` lines.
    stderr : str
        Captured standard error containing resolver diagnostics and warnings.

    Returns
    -------
    tuple[dict[str, str], list[str]]
        ``(resolved_versions, errors)`` where ``resolved_versions`` maps
        normalised package names to resolved versions and ``errors`` is empty
        only when resolution succeeded with zero unpinned-dependency warnings.
    """
    errors: list[str] = []
    if returncode != 0:
        errors.append(
            f"uv pip compile exited with status {returncode}:\n{stderr.strip()}"
        )

    unpinned_warnings = [
        line.strip() for line in stderr.splitlines() if "is unpinned" in line.lower()
    ]
    for warning in unpinned_warnings:
        errors.append(f"unpinned direct dependency warning: {warning}")

    resolved: dict[str, str] = {}
    for line in stdout.splitlines():
        stripped = line.strip()
        if not stripped or stripped.startswith("#") or "==" not in stripped:
            continue
        pkg, version = stripped.split("==", maxsplit=1)
        resolved[normalise_package_name(pkg)] = version.strip()

    if returncode == 0 and not resolved:
        errors.append("uv pip compile succeeded but emitted zero pinned packages")

    return resolved, errors


def check_platform_resolution(
    requirements: Sequence[str],
    *,
    python_version: str,
    platform: str,
    only_binary_all: bool = False,
) -> tuple[dict[str, str], list[str]]:
    """Run ``uv pip compile --resolution=lowest-direct`` for one target platform.

    Parameters
    ----------
    requirements : Sequence[str]
        Core requirement strings to pass on standard input to ``uv``.
    python_version : str
        Target CPython version (for example ``"3.12"``).
    platform : str
        Target platform triple recognised by ``uv --python-platform``.
    only_binary_all : bool, optional
        When ``True``, pass ``--only-binary :all:`` so resolution succeeds only
        when prebuilt binary wheels exist for every resolved package.

    Returns
    -------
    tuple[dict[str, str], list[str]]
        ``(resolved_versions, errors)`` for the target platform.
    """
    cmd = [
        "uv",
        "pip",
        "compile",
        "--resolution=lowest-direct",
        "--python-version",
        python_version,
        "--python-platform",
        platform,
        "--no-python-downloads",
    ]
    if only_binary_all:
        cmd.extend(["--only-binary", ":all:"])
    cmd.append("-")
    proc = subprocess.run(
        cmd,
        input="\n".join(requirements) + "\n",
        text=True,
        capture_output=True,
        check=False,
    )
    return parse_uv_compile_result(proc.returncode, proc.stdout, proc.stderr)


def build_parser() -> argparse.ArgumentParser:
    """Create the CLI argument parser.

    Returns
    -------
    argparse.ArgumentParser
        Configured CLI parser.
    """
    parser = argparse.ArgumentParser(
        description=(
            "Check that every core runtime dependency declares a lower bound "
            "and resolves cleanly under uv --resolution=lowest-direct."
        )
    )
    parser.add_argument(
        "--pyproject",
        type=Path,
        default=Path("pyproject.toml"),
        help="Path to pyproject.toml (default: %(default)s).",
    )
    parser.add_argument(
        "--python-version",
        action="append",
        dest="python_versions",
        default=[],
        help=(
            "Target Python version for lowest-direct resolution (may be "
            "repeated; defaults to 3.12)."
        ),
    )
    parser.add_argument(
        "--platform",
        action="append",
        dest="platforms",
        default=[],
        help=(
            "Target platform for uv --python-platform (may be repeated; "
            f"defaults to {', '.join(DEFAULT_PLATFORMS)})."
        ),
    )
    parser.add_argument(
        "--only-binary-all",
        action="store_true",
        help=(
            "Pass --only-binary :all: to uv pip compile so resolution verifies "
            "wheel availability instead of falling back to sdists."
        ),
    )
    parser.add_argument(
        "--check-floors-only",
        action="store_true",
        help="Only check static lower-bound declarations without invoking uv.",
    )
    return parser


def main(argv: Sequence[str] | None = None) -> int:
    """Run the lower-bound and lowest-direct resolution audit.

    Parameters
    ----------
    argv : Sequence[str] | None, optional
        Explicit CLI arguments; defaults to ``sys.argv[1:]``.

    Returns
    -------
    int
        Exit status (``0`` on success, ``1`` on any violation).
    """
    args = build_parser().parse_args(argv)
    requirements = read_core_dependencies(args.pyproject)
    direct_names = sorted({parse_requirement_floor(req)[0] for req in requirements})

    missing = find_missing_lower_bounds(requirements)
    if missing:
        print(
            "[X] Core runtime dependencies missing an explicit lower bound:",
            file=sys.stderr,
        )
        for req in missing:
            print(f"  - {req}", file=sys.stderr)
        return 1

    print(
        f"[OK] All {len(requirements)} core dependency entries "
        f"({len(direct_names)} direct packages) declare explicit lower bounds."
    )
    if args.check_floors_only:
        return 0

    python_versions = args.python_versions or ["3.12"]
    platforms = args.platforms or list(DEFAULT_PLATFORMS)
    mode_label = "binary-only" if args.only_binary_all else "sdist-allowed"
    failed = False
    for python_version in python_versions:
        for platform in platforms:
            resolved, errors = check_platform_resolution(
                requirements,
                python_version=python_version,
                platform=platform,
                only_binary_all=args.only_binary_all,
            )
            if errors:
                failed = True
                print(
                    f"[X] lowest-direct [{mode_label}] resolution failed for "
                    f"Python {python_version} on {platform}:",
                    file=sys.stderr,
                )
                for err in errors:
                    print(f"  - {err}", file=sys.stderr)
                continue

            summary = ", ".join(
                f"{name}=={resolved.get(name, 'MISSING')}"
                for name in ("pandas", "scipy", "numpy", "pydantic", "pyarrow")
            )
            print(
                f"[OK] Python {python_version} ({platform}, {mode_label}): "
                f"resolved {len(resolved)} packages ({summary})"
            )

    return 1 if failed else 0


if __name__ == "__main__":
    raise SystemExit(main())
