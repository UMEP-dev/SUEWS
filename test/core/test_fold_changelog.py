"""Tests for scripts/suews/fold_changelog.py.

The fold must only ever insert lines into CHANGELOG.md: every line that was
there before, including the table of contents and the Annual Statistics
table, has to survive in the same order. The other properties checked here
are where an entry lands (an existing date, a new date between two others, a
new year) and that malformed fragments are refused before anything is
written.
"""

from __future__ import annotations

import datetime as dt
import importlib.util
import os
from pathlib import Path
import shutil
import subprocess
import sys

import pytest

pytestmark = [pytest.mark.api, pytest.mark.contract("C-TOOLING")]


SCRIPT_PATH = (
    Path(__file__).resolve().parents[2] / "scripts" / "suews" / "fold_changelog.py"
)
SCRIPT_SPEC = importlib.util.spec_from_file_location("fold_changelog", SCRIPT_PATH)
assert SCRIPT_SPEC is not None
assert SCRIPT_SPEC.loader is not None
fold_changelog = importlib.util.module_from_spec(SCRIPT_SPEC)
sys.modules[SCRIPT_SPEC.name] = fold_changelog
SCRIPT_SPEC.loader.exec_module(fold_changelog)


CHANGELOG = """\
<!-- header comment -->

## Table of Contents

- [2026](#2026)
- [2025](#2025)

## Annual Statistics

| Year | Features | Bugfixes | Changes | Maintenance | Docs | Total |
|------|----------|----------|---------|-------------|------|-------|
| 2026 | 1        | 1        | 0 | 0 | 0 | 2    |

## 2026

### 20 Nov 2026

- [bugfix] Fixed the first thing (#10)

### 5 Nov 2026

- [feature][experimental] Added the second thing (#11)
  - with a sub-bullet

## 2025

### 1 Dec 2025

- [doc] Wrote the third thing (#9)
"""


def write_fragment(directory: Path, name: str, body: str) -> Path:
    directory.mkdir(parents=True, exist_ok=True)
    path = directory / name
    path.write_text(body, encoding="utf-8")
    return path


def fragment(tmp_path: Path, name: str, body: str):
    return fold_changelog.parse_fragment(write_fragment(tmp_path, name, body))


def assert_only_inserted(before: str, after: str) -> None:
    """Every original line survives, in order, as a subsequence of ``after``."""
    remaining = iter(after.split("\n"))
    for line in before.split("\n"):
        assert any(candidate == line for candidate in remaining), line


def section(text: str, heading: str) -> list[str]:
    lines = text.split("\n")
    start = lines.index(heading) + 1
    end = start
    while end < len(lines) and not lines[end].startswith(("## ", "### ")):
        end += 1
    return [line for line in lines[start:end] if line]


def test_appends_under_existing_date(tmp_path):
    frag = fragment(tmp_path, "12.maintenance.md", "- [maintenance] Tidied CI (#12)\n")
    after, notices = fold_changelog.fold(
        CHANGELOG, [frag], lambda _: dt.date(2026, 11, 20)
    )
    assert notices == []
    assert section(after, "### 20 Nov 2026") == [
        "- [bugfix] Fixed the first thing (#10)",
        "- [maintenance] Tidied CI (#12)",
    ]
    assert_only_inserted(CHANGELOG, after)


def test_new_date_lands_in_descending_order(tmp_path):
    newest = fragment(tmp_path, "13.bugfix.md", "- [bugfix] Newest (#13)\n")
    middle = fragment(tmp_path, "14.doc.md", "- [doc] Middle (#14)\n")
    days = {13: dt.date(2026, 11, 22), 14: dt.date(2026, 11, 9)}
    after, _ = fold_changelog.fold(CHANGELOG, [newest, middle], lambda f: days[f.ref])
    headings = [line for line in after.split("\n") if line.startswith("### ")]
    assert headings == [
        "### 22 Nov 2026",
        "### 20 Nov 2026",
        "### 9 Nov 2026",
        "### 5 Nov 2026",
        "### 1 Dec 2025",
    ]
    assert section(after, "### 9 Nov 2026") == ["- [doc] Middle (#14)"]
    assert "\n\n\n" not in after
    assert_only_inserted(CHANGELOG, after)


def test_oldest_date_in_year_goes_before_next_year(tmp_path):
    frag = fragment(tmp_path, "15.change.md", "- [change][stable] Early (#15)\n")
    after, _ = fold_changelog.fold(CHANGELOG, [frag], lambda _: dt.date(2026, 1, 2))
    lines = after.split("\n")
    assert lines.index("### 2 Jan 2026") < lines.index("## 2025")
    assert lines.index("### 5 Nov 2026") < lines.index("### 2 Jan 2026")
    assert "\n\n\n" not in after
    assert_only_inserted(CHANGELOG, after)


def test_new_year_adds_section_and_toc_but_not_statistics(tmp_path):
    frag = fragment(tmp_path, "16.feature.md", "- [feature][experimental] Next (#16)\n")
    after, _ = fold_changelog.fold(CHANGELOG, [frag], lambda _: dt.date(2027, 1, 4))
    lines = after.split("\n")
    assert lines.index("- [2027](#2027)") < lines.index("- [2026](#2026)")
    assert lines.index("## 2027") < lines.index("## 2026")
    assert section(after, "### 4 Jan 2027") == ["- [feature][experimental] Next (#16)"]
    stats = [line for line in lines if line.startswith("| 20")]
    assert stats == ["| 2026 | 1        | 1        | 0 | 0 | 0 | 2    |"]
    assert "\n\n\n" not in after
    assert_only_inserted(CHANGELOG, after)


def test_same_day_entries_follow_category_order(tmp_path):
    frags = [
        fragment(tmp_path, "30.doc.md", "- [doc] D (#30)\n"),
        fragment(tmp_path, "20.feature.md", "- [feature][experimental] F (#20)\n"),
        fragment(
            tmp_path, "21.bugfix.md", "- [bugfix] B (#21)\n  - detail (abc1234)\n"
        ),
    ]
    after, _ = fold_changelog.fold(CHANGELOG, frags, lambda _: dt.date(2026, 11, 21))
    assert section(after, "### 21 Nov 2026") == [
        "- [feature][experimental] F (#20)",
        "- [bugfix] B (#21)",
        "  - detail (abc1234)",
        "- [doc] D (#30)",
    ]


def test_entry_already_present_is_not_duplicated(tmp_path):
    frag = fragment(
        tmp_path, "10.bugfix.md", "- [bugfix] Fixed the first thing (#10)\n"
    )
    after, notices = fold_changelog.fold(
        CHANGELOG, [frag], lambda _: dt.date(2026, 11, 20)
    )
    assert after == CHANGELOG
    assert len(notices) == 1


def test_entry_already_present_with_sub_bullet_is_not_duplicated(tmp_path):
    frag = fragment(
        tmp_path,
        "11.feature.md",
        "- [feature][experimental] Added the second thing (#11)\n  - with a sub-bullet\n",
    )
    after, notices = fold_changelog.fold(
        CHANGELOG, [frag], lambda _: dt.date(2026, 11, 5)
    )
    assert after == CHANGELOG
    assert len(notices) == 1


def test_partial_overlap_is_refused_and_nothing_is_deleted(tmp_path):
    (tmp_path / "CHANGELOG.md").write_text(CHANGELOG, encoding="utf-8")
    frag = write_fragment(
        tmp_path / "changes",
        "10.bugfix.md",
        "- [bugfix] Fixed the first thing (#10)\n  - a new detail (abc1234)\n",
    )
    with pytest.raises(fold_changelog.FragmentError, match="rest differs"):
        fold_changelog.fold(
            CHANGELOG,
            [fold_changelog.parse_fragment(frag)],
            lambda _: dt.date(2026, 11, 20),
        )
    args = ["--root", str(tmp_path), "--date", "2026-11-20"]
    assert fold_changelog.main(args) == 1
    assert frag.exists()
    assert (tmp_path / "CHANGELOG.md").read_text(encoding="utf-8") == CHANGELOG


@pytest.mark.parametrize(
    ("name", "body", "message"),
    [
        ("12-bugfix.md", "- [bugfix] x (#12)\n", "name must be"),
        ("12.fix.md", "- [bugfix] x (#12)\n", "name must be"),
        ("12.bugfix.md", "- [feature] x (#12)\n", "does not match"),
        ("12.bugfix.md", "", "empty"),
        ("12.bugfix.md", "### 1 Nov 2026\n- [bugfix] x\n", "headings"),
        ("12.bugfix.md", "- [bugfix] x\n\n- [bugfix] y\n", "blank line"),
        ("12.bugfix.md", "bugfix: x\n", "must start with"),
        ("12.feature.md", "- [feature][beta] x\n", "unknown status"),
        ("12.bugfix.md", "  - orphan sub-bullet\n", "sub-bullets"),
    ],
)
def test_bad_fragments_are_refused(tmp_path, name, body, message):
    path = write_fragment(tmp_path, name, body)
    with pytest.raises(fold_changelog.FragmentError, match=message):
        fold_changelog.parse_fragment(path)


def test_check_mode_skips_readme_and_reports_errors(tmp_path, capsys):
    changes = tmp_path / "changes"
    write_fragment(changes, "README.md", "# How to write a fragment\n")
    write_fragment(changes, "12.bugfix.md", "- [bugfix] ok (#12)\n")
    assert fold_changelog.main(["--root", str(tmp_path), "--check"]) == 0
    write_fragment(changes, "13.bugfix.md", "- [doc] wrong tag (#13)\n")
    assert fold_changelog.main(["--root", str(tmp_path), "--check"]) == 1
    assert "13.bugfix.md" in capsys.readouterr().out


def test_cli_folds_and_deletes_fragments(tmp_path):
    (tmp_path / "CHANGELOG.md").write_text(CHANGELOG, encoding="utf-8")
    changes = tmp_path / "changes"
    readme = write_fragment(changes, "README.md", "# How to write a fragment\n")
    frag = write_fragment(changes, "12.bugfix.md", "- [bugfix] Folded (#12)\n")

    args = ["--root", str(tmp_path), "--date", "2026-11-20"]
    assert fold_changelog.main([*args, "--dry-run"]) == 0
    assert frag.exists()
    assert (tmp_path / "CHANGELOG.md").read_text(encoding="utf-8") == CHANGELOG

    assert fold_changelog.main(args) == 0
    after = (tmp_path / "CHANGELOG.md").read_text(encoding="utf-8")
    assert "- [bugfix] Folded (#12)" in section(after, "### 20 Nov 2026")
    assert not frag.exists()
    assert readme.exists()


@pytest.mark.skipif(shutil.which("git") is None, reason="git not available")
def test_date_comes_from_the_commit_that_added_the_fragment(tmp_path):
    def git(*args, date=None):
        env = {
            key: value
            for key, value in os.environ.items()
            if not key.startswith("GIT_")
        }
        env.update(
            GIT_AUTHOR_NAME="t",
            GIT_AUTHOR_EMAIL="t@example.invalid",
            GIT_COMMITTER_NAME="t",
            GIT_COMMITTER_EMAIL="t@example.invalid",
        )
        if date:
            env["GIT_COMMITTER_DATE"] = env["GIT_AUTHOR_DATE"] = date
        subprocess.run(["git", *args], cwd=tmp_path, check=True, env=env)

    git("init", "-q")
    (tmp_path / "CHANGELOG.md").write_text(CHANGELOG, encoding="utf-8")
    frag = write_fragment(tmp_path / "changes", "12.bugfix.md", "- [bugfix] x (#12)\n")
    git("add", ".")
    git("commit", "-q", "-m", "add", date="2026-11-21T10:00:00+00:00")
    frag.write_text("- [bugfix] x, reworded (#12)\n", encoding="utf-8")
    git("commit", "-q", "-am", "edit", date="2026-11-23T10:00:00+00:00")

    assert fold_changelog.git_added_date(frag, tmp_path) == dt.date(2026, 11, 21)

    frag.unlink()
    git("commit", "-q", "-am", "fold", date="2026-11-24T10:00:00+00:00")
    frag.write_text("- [bugfix] y, a later entry (#12)\n", encoding="utf-8")
    git("add", ".")
    git("commit", "-q", "-m", "re-add", date="2026-12-02T10:00:00+00:00")
    assert fold_changelog.git_added_date(frag, tmp_path) == dt.date(2026, 12, 2)

    untracked = write_fragment(tmp_path / "changes", "13.doc.md", "- [doc] y (#13)\n")
    assert fold_changelog.git_added_date(untracked, tmp_path) is None
    assert fold_changelog.main(["--root", str(tmp_path)]) == 1
    assert untracked.exists()
