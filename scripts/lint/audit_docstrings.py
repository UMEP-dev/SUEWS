#!/usr/bin/env python3
"""Emit the TOML per-file-ignores seed that freezes existing D-rule debt.

Runs ``ruff check --select D`` with the numpy pydocstyle convention against
``src/supy/``, groups the reported codes by file, and prints a commented TOML
fragment on stdout suitable for splicing into the ``[lint.per-file-ignores]``
table in ``.ruff.toml``. Use it to seed or refresh the legacy-debt block.

Usage
-----
    uv run python scripts/lint/audit_docstrings.py

Workflow for cleaning a file:
    1. Fix docstrings until ``ruff check --select D <file>`` is clean.
    2. Delete that file's line from the per-file-ignores block in
       ``.ruff.toml`` (or regenerate via this script and splice back in).
    3. Commit.
"""

from __future__ import annotations

import re
import subprocess
import sys
from collections import defaultdict
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[2]
TARGET = "src/supy/"
LINE_RE = re.compile(r"^(?P<file>[^:]+):\d+:\d+:\s+(?P<code>D\d+)\b")


def collect_failures() -> dict[str, list[str]]:
    """Run ruff and return ``{relative_file: sorted_unique_codes}``.

    Runs with ``--isolated`` so the existing per-file-ignores in ``.ruff.toml``
    do not suppress the very debt this helper is trying to measure. The
    ``_version.py`` generated file is excluded explicitly to match the
    project's ruff config.
    """
    result = subprocess.run(
        [
            "ruff",
            "check",
            "--isolated",
            "--select",
            "D",
            "--config",
            'lint.pydocstyle.convention = "numpy"',
            "--exclude",
            "src/supy/_version.py,src/supy/_suews_driver.py",
            "--output-format=concise",
            TARGET,
        ],
        cwd=REPO_ROOT,
        capture_output=True,
        text=True,
        check=False,
    )
    by_file: dict[str, set[str]] = defaultdict(set)
    for line in result.stdout.splitlines():
        m = LINE_RE.match(line)
        if m:
            by_file[m["file"]].add(m["code"])
    return {f: sorted(codes) for f, codes in sorted(by_file.items())}


def render(by_file: dict[str, list[str]]) -> str:
    """Render the commented TOML fragment ready to splice into .ruff.toml."""
    lines = [
        "# --- BEGIN legacy docstring debt (gh1294) ---",
        "# Regenerated via: uv run python scripts/lint/audit_docstrings.py",
        "# Remove a file's entry once its docstrings are cleaned.",
    ]
    for file, codes in by_file.items():
        rendered_codes = ", ".join(f'"{c}"' for c in codes)
        lines.append(f'"{file}" = [{rendered_codes}]')
    lines.append("# --- END legacy docstring debt (gh1294) ---")
    return "\n".join(lines) + "\n"


def main() -> int:
    by_file = collect_failures()
    sys.stdout.write(render(by_file))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
