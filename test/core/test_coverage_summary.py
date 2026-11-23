"""Contract tests for the nightly Python coverage summary.

`scripts/suews/coverage_summary.py` turns the coverage.py JSON report of the
nightly `python_coverage` job into totals and a per-package table. These tests
drive it with a synthetic report.
"""

from __future__ import annotations

import json
from pathlib import Path
import sys
from typing import Any

import pytest

REPO_ROOT = Path(__file__).resolve().parents[2]
# scripts/ is not a package on sys.path under a bare `pytest` invocation
# (only `python -m pytest` puts the repository root there), so add it.
if str(REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(REPO_ROOT))

from scripts.suews import coverage_summary as summary  # noqa: E402

pytestmark = [pytest.mark.api, pytest.mark.contract("C-TOOLING")]


def _file(statements: int, covered: int, branches: int, covered_branches: int) -> dict[str, Any]:
    return {
        "summary": {
            "num_statements": statements,
            "covered_lines": covered,
            "num_branches": branches,
            "covered_branches": covered_branches,
        }
    }


REPORT = {
    "totals": {"percent_covered": 70.0},
    "files": {
        "src/supy/_run.py": _file(100, 80, 20, 10),
        "src/supy/data_model/core/model.py": _file(200, 150, 40, 30),
        "src/supy/data_model/validation.py": _file(100, 50, 0, 0),
        "src/supy/util/_era5.py": _file(50, 10, 10, 2),
        "mcp/src/suews_mcp/server.py": _file(50, 40, 10, 9),
    },
}


@pytest.mark.parametrize(
    ("path", "package"),
    [
        ("src/supy/_run.py", "supy (top-level modules)"),
        ("src/supy/data_model/core/model.py", "supy.data_model"),
        ("src/supy\\util\\_era5.py", "supy.util"),
        ("mcp/src/suews_mcp/tools/run.py", "suews_mcp"),
        ("/home/runner/.venv/lib/python3.12/site-packages/supy/_run.py", "other"),
    ],
)
def test_files_group_by_top_level_subpackage(path: str, package: str) -> None:
    assert summary.package_of(path) == package


def test_totals_and_rows_are_sums_of_their_files() -> None:
    total, packages, unmapped = summary.tally_by_package(REPORT)

    assert (total.files, total.statements, total.covered_lines) == (5, 500, 330)
    assert (total.branches, total.covered_branches) == (80, 51)
    assert total.line_percent == pytest.approx(66.0)
    assert total.branch_percent == pytest.approx(63.75)
    data_model = packages["supy.data_model"]
    assert (data_model.files, data_model.statements, data_model.covered_lines) == (2, 300, 200)
    assert data_model.branch_percent == pytest.approx(75.0)
    assert unmapped == []


def test_markdown_lists_totals_and_one_row_per_package() -> None:
    text = summary.markdown(REPORT)

    assert "Lines: **66.0%** of 500 statements; branches: **63.8%** of 80" in text
    assert "combined (coverage.py `percent_covered`): 70.0%" in text
    rows = [line for line in text.splitlines() if line.startswith("| `")]
    assert [row.split("|")[1].strip() for row in rows] == [
        "`supy.data_model`",
        "`supy.util`",
        "`supy (top-level modules)`",
        "`suews_mcp`",
    ]
    assert "| `supy.util` | 1 | 50 | 20.0% | 10 | 20.0% |" in text
    assert "[X]" not in text


def test_unmapped_files_are_named_not_hidden() -> None:
    report = {
        "files": {
            "src/supy/_run.py": _file(10, 5, 0, 0),
            "/opt/venv/lib/python3.12/site-packages/supy/_load.py": _file(10, 10, 2, 2),
        }
    }

    text = summary.markdown(report)

    assert "| `other` | 1 | 10 | 100.0% | 2 | 100.0% |" in text
    assert "[X] 1 file(s) lie outside" in text
    assert "site-packages/supy/_load.py" in text
    # A package without branches shows a dash rather than a fabricated figure.
    assert "| `supy (top-level modules)` | 1 | 10 | 50.0% | 0 | - |" in text


def test_cli_appends_markdown_and_reports_unreadable_input(
    tmp_path: Path, capsys: pytest.CaptureFixture[str]
) -> None:
    report = tmp_path / "coverage.json"
    report.write_text(json.dumps(REPORT), encoding="utf-8")
    target = tmp_path / "summary.md"
    target.write_text("earlier step\n", encoding="utf-8")

    assert summary.main([str(report), "--markdown", str(target)]) == 0
    written = target.read_text(encoding="utf-8")
    assert written.startswith("earlier step\n## Python coverage")
    assert "## Python coverage" in capsys.readouterr().out

    assert summary.main([str(tmp_path / "missing.json")]) == 2
    assert "[X] cannot read" in capsys.readouterr().out
