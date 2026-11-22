#!/usr/bin/env python3
"""Summarise a coverage.py JSON report as totals and a per-package table.

The nightly `python_coverage` job in `build-wheels-reusable.yml` runs the
suite under coverage with the configuration in `pyproject.toml`
(`[tool.coverage.*]`) and writes `coverage.json`. This script reads that file
and prints the total line and branch percentages and one row per package: each
top-level subpackage of `supy` (`supy.data_model`, `supy.util`, ...), the
modules directly under `supy`, and `suews_mcp`. Files are grouped by their
reported path, which `[tool.coverage.paths]` maps to `src/supy/...` and
`mcp/src/suews_mcp/...`; a file outside those two trees is counted under
"other" and named in a note, because it means the path mapping did not apply.

Line percentage is covered statements over statements; branch percentage is
covered branches over branches. The combined figure coverage.py reports as
`percent_covered` is printed beside them.

Usage::

    python scripts/suews/coverage_summary.py coverage/coverage.json
    python scripts/suews/coverage_summary.py coverage/coverage.json --markdown "$GITHUB_STEP_SUMMARY"
"""

from __future__ import annotations

import argparse
from dataclasses import dataclass
import json
from pathlib import Path
import sys
from typing import Any

SUPY_ROOT = "src/supy/"
MCP_ROOT = "mcp/src/suews_mcp/"
SUPY_TOP_LEVEL = "supy (top-level modules)"
OTHER = "other"


@dataclass
class Tally:
    """Statement and branch counts for one group of files."""

    files: int = 0
    statements: int = 0
    covered_lines: int = 0
    branches: int = 0
    covered_branches: int = 0

    def add(self, summary: dict[str, Any]) -> None:
        """Add one file's coverage.py summary to the tally."""
        self.files += 1
        self.statements += int(summary.get("num_statements", 0))
        self.covered_lines += int(summary.get("covered_lines", 0))
        self.branches += int(summary.get("num_branches", 0))
        self.covered_branches += int(summary.get("covered_branches", 0))

    @property
    def line_percent(self) -> float | None:
        """Return covered statements over statements, or None without statements."""
        return 100.0 * self.covered_lines / self.statements if self.statements else None

    @property
    def branch_percent(self) -> float | None:
        """Return covered branches over branches, or None without branches."""
        return 100.0 * self.covered_branches / self.branches if self.branches else None


def package_of(path: str) -> str:
    """Return the package row a reported file path belongs to."""
    path = path.replace("\\", "/")
    if path.startswith(SUPY_ROOT):
        rest = path[len(SUPY_ROOT) :]
        return f"supy.{rest.split('/', 1)[0]}" if "/" in rest else SUPY_TOP_LEVEL
    if path.startswith(MCP_ROOT):
        return "suews_mcp"
    return OTHER


def tally_by_package(report: dict[str, Any]) -> tuple[Tally, dict[str, Tally], list[str]]:
    """Return the overall tally, one tally per package, and the unmapped files."""
    total = Tally()
    packages: dict[str, Tally] = {}
    unmapped: list[str] = []
    for path, record in sorted(report.get("files", {}).items()):
        summary = record.get("summary", {})
        package = package_of(path)
        if package == OTHER:
            unmapped.append(path)
        packages.setdefault(package, Tally()).add(summary)
        total.add(summary)
    return total, packages, unmapped


def _percent(value: float | None) -> str:
    return "-" if value is None else f"{value:.1f}%"


def _row_order(name: str) -> tuple[int, str]:
    # supy subpackages first, then the top-level modules, suews_mcp, other.
    rank = {SUPY_TOP_LEVEL: 1, "suews_mcp": 2, OTHER: 3}.get(name, 0)
    return rank, name


def markdown(report: dict[str, Any]) -> str:
    """Render the totals and the per-package table as Markdown."""
    total, packages, unmapped = tally_by_package(report)
    combined = report.get("totals", {}).get("percent_covered")
    lines = [
        "## Python coverage",
        "",
        f"Lines: **{_percent(total.line_percent)}** of {total.statements} statements; "
        f"branches: **{_percent(total.branch_percent)}** of {total.branches}; "
        f"combined (coverage.py `percent_covered`): {_percent(combined)}. "
        f"{total.files} files.",
        "",
        "| Package | Files | Statements | Lines | Branches | Branch coverage |",
        "|---|---:|---:|---:|---:|---:|",
    ]
    for name in sorted(packages, key=_row_order):
        tally = packages[name]
        lines.append(
            f"| `{name}` | {tally.files} | {tally.statements} | {_percent(tally.line_percent)} | "
            f"{tally.branches} | {_percent(tally.branch_percent)} |"
        )
    if unmapped:
        lines.extend([
            "",
            f"[X] {len(unmapped)} file(s) lie outside `{SUPY_ROOT}` and `{MCP_ROOT}`, so the "
            "`[tool.coverage.paths]` mapping in pyproject.toml did not apply to them:",
            *(f"- `{path}`" for path in unmapped[:20]),
        ])
    lines.append("")
    return "\n".join(lines)


def _parse_args(argv: list[str] | None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter
    )
    parser.add_argument("report", type=Path, help="coverage.py JSON report (coverage json)")
    parser.add_argument(
        "--markdown",
        type=Path,
        help="append the summary to this file (for example $GITHUB_STEP_SUMMARY)",
    )
    return parser.parse_args(argv)


def main(argv: list[str] | None = None) -> int:
    """Print the summary and append it to --markdown; return 2 on unreadable input."""
    args = _parse_args(argv)
    try:
        report = json.loads(args.report.read_text(encoding="utf-8"))
    except (OSError, ValueError) as exc:
        print(f"[X] cannot read {args.report}: {exc}")
        return 2
    text = markdown(report)
    print(text)
    if args.markdown:
        with args.markdown.open("a", encoding="utf-8") as handle:
            handle.write(text)
    return 0


if __name__ == "__main__":
    sys.exit(main())
