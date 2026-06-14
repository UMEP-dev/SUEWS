#!/usr/bin/env python3
"""Export dependencies from pyproject.toml as requirements lines.

This keeps security tooling lightweight: scanners can resolve the declared
dependencies without needing to build the local package first.
"""

from __future__ import annotations

import argparse
from pathlib import Path

try:
    import tomllib
except ModuleNotFoundError:  # pragma: no cover - Python <3.11 fallback
    import tomli as tomllib  # type: ignore[no-redef]


def build_parser() -> argparse.ArgumentParser:
    """Create the CLI parser."""
    parser = argparse.ArgumentParser(
        description="Export pyproject dependencies as requirements.txt lines."
    )
    parser.add_argument(
        "--pyproject",
        default="pyproject.toml",
        help="Path to pyproject.toml (default: %(default)s).",
    )
    parser.add_argument(
        "--extra",
        action="append",
        default=[],
        metavar="NAME",
        help="Optional dependency group to include. May be repeated.",
    )
    return parser


def read_requirements(pyproject_path: Path, extras: list[str] | None = None) -> list[str]:
    """Read project dependencies and selected optional groups."""
    extras = extras or []
    data = tomllib.loads(pyproject_path.read_text(encoding="utf-8"))
    project = data.get("project", {})
    requirements = list(project.get("dependencies", []))
    optional_dependencies = project.get("optional-dependencies", {})

    for extra in extras:
        if extra not in optional_dependencies:
            known = ", ".join(sorted(optional_dependencies)) or "none"
            raise ValueError(f"Unknown optional dependency group '{extra}'. Available groups: {known}")
        requirements.extend(optional_dependencies[extra])

    deduped: list[str] = []
    seen: set[str] = set()
    for requirement in requirements:
        if requirement not in seen:
            deduped.append(requirement)
            seen.add(requirement)
    return deduped


def main() -> int:
    """Run the CLI."""
    args = build_parser().parse_args()
    requirements = read_requirements(Path(args.pyproject), extras=args.extra)
    print("\n".join(requirements))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
