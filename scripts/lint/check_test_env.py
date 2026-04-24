"""Preflight checks for local pytest entrypoints."""

from __future__ import annotations

import importlib.util
from pathlib import Path
import sys


def _spec_paths(module_name: str) -> list[Path]:
    spec = importlib.util.find_spec(module_name)
    if spec is None:
        return []

    raw_paths: list[str] = []
    if spec.origin and spec.origin not in {"built-in", "namespace"}:
        raw_paths.append(spec.origin)
    raw_paths.extend(spec.submodule_search_locations or [])
    return [Path(path).resolve() for path in raw_paths]


def _inside_repo(path: Path, repo_root: Path) -> bool:
    return path == repo_root or repo_root in path.parents


def main() -> int:
    repo_root = Path.cwd().resolve()
    missing: list[str] = []

    if importlib.util.find_spec("pytest") is None:
        missing.append("pytest")

    supy_paths = _spec_paths("supy")
    if not supy_paths:
        missing.append("editable supy package")
    elif not any(_inside_repo(path, repo_root) for path in supy_paths):
        found = ", ".join(str(path) for path in supy_paths)
        missing.append(f"editable supy package (found outside this repo: {found})")

    if not missing:
        return 0

    print(
        "\nERROR: Test environment is not ready.\n"
        f"Python: {sys.executable}\n"
        f"Missing: {', '.join(missing)}\n\n"
        "Run: make setup && source .venv/bin/activate && make dev",
        file=sys.stderr,
    )
    return 1


if __name__ == "__main__":
    raise SystemExit(main())
