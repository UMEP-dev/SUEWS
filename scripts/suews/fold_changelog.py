"""Fold per-PR changelog fragments from ``changes/`` into ``CHANGELOG.md``.

Each pull request records its CHANGELOG entry as a small file in ``changes/``
instead of editing ``CHANGELOG.md`` directly, so pull requests that touch
disjoint files can share a merge-queue batch.  This script moves those entries
into ``CHANGELOG.md`` under the right ``### D Mon YYYY`` heading and deletes the
fragment files.  It only inserts lines: existing entries, the table of contents
and the Annual Statistics table are never rewritten.

Fragment files are named ``<number>.<category>.md``, where ``<number>`` is the
PR or issue number and ``<category>`` is one of the CHANGELOG categories.  The
body is the exact bullet text that would otherwise go into ``CHANGELOG.md``.
The rules live in ``.claude/rules/changelog/format.md``.

The date of each fragment is the committer date of the commit that added it
(for a squash-merged PR, the day it landed on master), so entries keep the
"actual commit date" rule.  ``--date`` overrides this for every fragment.

Usage::

    python scripts/suews/fold_changelog.py --check     # validate fragments only
    python scripts/suews/fold_changelog.py --dry-run   # show the diff, change nothing
    python scripts/suews/fold_changelog.py             # fold and delete fragments

Standard library only.
"""

from __future__ import annotations

import argparse
from collections.abc import Callable
from dataclasses import dataclass
import datetime as dt
import difflib
from pathlib import Path
import re
import subprocess
import sys

CATEGORIES = ("feature", "bugfix", "change", "maintenance", "doc")
STATUS_TAGS = ("experimental", "stable", "internal")
MONTHS = (
    "Jan",
    "Feb",
    "Mar",
    "Apr",
    "May",
    "Jun",
    "Jul",
    "Aug",
    "Sep",
    "Oct",
    "Nov",
    "Dec",
)

FRAGMENT_DIR = "changes"
SKIPPED_NAMES = frozenset({"README.md", ".gitkeep"})
FRAGMENT_NAME_RE = re.compile(
    r"^(?P<ref>[1-9][0-9]*)\.(?P<category>" + "|".join(CATEGORIES) + r")\.md$"
)
ENTRY_RE = re.compile(r"^- \[(?P<category>[a-z]+)\](?:\[(?P<status>[a-z]+)\])?(?: |$)")
YEAR_HEADING_RE = re.compile(r"^## (?P<year>[0-9]{4})\s*$")
DATE_HEADING_RE = re.compile(
    r"^### (?P<day>[1-9]|[12][0-9]|3[01]) (?P<month>[A-Z][a-z]{2}) (?P<year>[0-9]{4})\s*$"
)
TOC_YEAR_RE = re.compile(r"^- \[(?P<year>[0-9]{4})\]\(#[0-9]{4}\)\s*$")


class FragmentError(ValueError):
    """A fragment file breaks the naming or content rules."""


@dataclass(frozen=True)
class Fragment:
    """One fragment file and the CHANGELOG lines it carries."""

    path: Path
    ref: int
    category: str
    lines: tuple[str, ...]


def format_date(day: dt.date) -> str:
    """Render a date as the CHANGELOG heading text, e.g. ``5 Nov 2026``."""
    return f"{day.day} {MONTHS[day.month - 1]} {day.year}"


def parse_heading_date(line: str) -> dt.date | None:
    """Return the date of a ``### D Mon YYYY`` heading, or None."""
    match = DATE_HEADING_RE.match(line)
    if not match or match.group("month") not in MONTHS:
        return None
    return dt.date(
        int(match.group("year")),
        MONTHS.index(match.group("month")) + 1,
        int(match.group("day")),
    )


def parse_fragment(path: Path) -> Fragment:
    """Read and validate one fragment file."""
    match = FRAGMENT_NAME_RE.match(path.name)
    if not match:
        raise FragmentError(
            f"{path}: name must be <number>.<category>.md with category one of "
            f"{', '.join(CATEGORIES)}"
        )
    category = match.group("category")
    text = path.read_text(encoding="utf-8")
    lines = [line.rstrip() for line in text.splitlines()]
    while lines and not lines[-1]:
        lines.pop()
    while lines and not lines[0]:
        lines.pop(0)
    if not lines:
        raise FragmentError(f"{path}: fragment is empty")
    for number, line in enumerate(lines, start=1):
        where = f"{path}:{number}"
        if not line:
            raise FragmentError(f"{where}: blank line inside a fragment")
        if line.startswith("#"):
            raise FragmentError(
                f"{where}: headings are added by the fold; write only bullet lines"
            )
        if line.startswith(" "):
            if number == 1 or not line.lstrip().startswith("- "):
                raise FragmentError(
                    f"{where}: indented lines must be sub-bullets of an entry"
                )
            continue
        entry = ENTRY_RE.match(line)
        if not entry:
            raise FragmentError(
                f"{where}: entry must start with '- [{category}]' "
                f"(see .claude/rules/changelog/format.md)"
            )
        if entry.group("category") != category:
            raise FragmentError(
                f"{where}: tag [{entry.group('category')}] does not match the "
                f"file name category [{category}]"
            )
        status = entry.group("status")
        if status is not None and status not in STATUS_TAGS:
            raise FragmentError(
                f"{where}: unknown status tag [{status}] (use {', '.join(STATUS_TAGS)})"
            )
    return Fragment(
        path=path, ref=int(match.group("ref")), category=category, lines=tuple(lines)
    )


def collect_fragments(fragment_dir: Path) -> tuple[list[Fragment], list[str]]:
    """Parse every fragment in ``fragment_dir``; return fragments and errors."""
    fragments: list[Fragment] = []
    errors: list[str] = []
    if not fragment_dir.is_dir():
        return fragments, errors
    for path in sorted(fragment_dir.iterdir()):
        if path.name in SKIPPED_NAMES:
            continue
        if path.is_dir():
            errors.append(f"{path}: subdirectories are not allowed in {FRAGMENT_DIR}/")
            continue
        try:
            fragments.append(parse_fragment(path))
        except FragmentError as exc:
            errors.append(str(exc))
    return fragments, errors


def git_added_date(path: Path, repo_root: Path) -> dt.date | None:
    """Committer date of the latest commit that added ``path`` on HEAD."""
    result = subprocess.run(
        [
            "git",
            "log",
            "--diff-filter=A",
            "--format=%cs",
            "--",
            path.relative_to(repo_root).as_posix(),
        ],
        cwd=repo_root,
        capture_output=True,
        text=True,
        encoding="utf-8",
        check=False,
    )
    stamps = [line for line in result.stdout.splitlines() if line.strip()]
    if result.returncode != 0 or not stamps:
        return None
    # git log lists newest first; a file name reused after an earlier fold
    # was deleted takes the date of its latest addition.
    return dt.date.fromisoformat(stamps[0].strip())


def _sort_key(fragment: Fragment) -> tuple[int, int]:
    return CATEGORIES.index(fragment.category), fragment.ref


def _section_end(lines: list[str], start: int) -> int:
    """Index after the last non-blank line of the block opened at ``start``."""
    end = start + 1
    while end < len(lines) and not lines[end].startswith(("## ", "### ")):
        end += 1
    while end > start + 1 and not lines[end - 1].strip():
        end -= 1
    return end


def _ensure_year(lines: list[str], year: int) -> int:
    """Return the index of ``## <year>``, creating the section if needed."""
    later_year_index = None
    for index, line in enumerate(lines):
        match = YEAR_HEADING_RE.match(line)
        if not match:
            continue
        found = int(match.group("year"))
        if found == year:
            return index
        if found < year and later_year_index is None:
            later_year_index = index
    insert_at = later_year_index if later_year_index is not None else len(lines)
    block = [f"## {year}", ""]
    if insert_at == len(lines) and lines and lines[-1].strip():
        block.insert(0, "")
    lines[insert_at:insert_at] = block
    _add_toc_year(lines, year)
    return _ensure_year(lines, year)


def _add_toc_year(lines: list[str], year: int) -> None:
    """Add ``- [YYYY](#YYYY)`` to the table of contents in descending order."""
    toc_rows = [i for i, line in enumerate(lines) if TOC_YEAR_RE.match(line)]
    if not toc_rows:
        return
    entry = f"- [{year}](#{year})"
    for index in toc_rows:
        if int(TOC_YEAR_RE.match(lines[index]).group("year")) < year:
            lines.insert(index, entry)
            return
    lines.insert(toc_rows[-1] + 1, entry)


def insert_entries(text: str, day: dt.date, entries: list[str]) -> str:
    """Insert ``entries`` under the ``### <day>`` heading of ``text``."""
    lines = text.split("\n")
    heading = f"### {format_date(day)}"
    for index, line in enumerate(lines):
        if parse_heading_date(line) == day:
            end = _section_end(lines, index)
            lines[end:end] = entries
            return "\n".join(lines)

    year_index = _ensure_year(lines, day.year)
    insert_at = None
    index = year_index + 1
    while index < len(lines) and not YEAR_HEADING_RE.match(lines[index]):
        existing = parse_heading_date(lines[index])
        if existing is not None and existing < day:
            insert_at = index
            break
        index += 1
    if insert_at is None:
        # No older heading in this year: append at the end of the year section.
        insert_at = index
        while insert_at > year_index + 1 and not lines[insert_at - 1].strip():
            insert_at -= 1
        block = ["", heading, "", *entries]
        if insert_at < len(lines) and lines[insert_at].strip():
            block.append("")
        lines[insert_at:insert_at] = block
    else:
        lines[insert_at:insert_at] = [heading, "", *entries, ""]
    return "\n".join(lines)


def _contains_block(lines: list[str], block: tuple[str, ...]) -> bool:
    """Report whether ``block`` occurs as consecutive lines of ``lines``."""
    size = len(block)
    return any(
        tuple(lines[start : start + size]) == block
        for start in range(len(lines) - size + 1)
    )


def fold(
    text: str,
    fragments: list[Fragment],
    date_for: Callable[[Fragment], dt.date],
) -> tuple[str, list[str]]:
    """Return ``text`` with every fragment folded in, plus skip notices.

    A fragment whose whole line sequence is already present verbatim in
    ``text`` is not inserted again (it was copied into ``CHANGELOG.md`` by
    hand), but is still reported so the caller can delete it.  A fragment
    whose first line is present but whose block differs raises
    ``FragmentError``: deleting it would lose the lines that differ.
    """
    notices: list[str] = []
    by_day: dict[dt.date, list[Fragment]] = {}
    for fragment in fragments:
        by_day.setdefault(date_for(fragment), []).append(fragment)
    original = text.split("\n")
    for day in sorted(by_day):
        entries: list[str] = []
        for fragment in sorted(by_day[day], key=_sort_key):
            if _contains_block(original, fragment.lines):
                notices.append(f"{fragment.path}: already in CHANGELOG.md, not added")
                continue
            if fragment.lines[0] in original:
                raise FragmentError(
                    f"{fragment.path}: its first line is already in CHANGELOG.md "
                    f"but the rest differs; reconcile by hand before folding"
                )
            entries.extend(fragment.lines)
        if entries:
            text = insert_entries(text, day, entries)
    return text, notices


def _fragment_date(fragment: Fragment, root: Path, override: dt.date | None) -> dt.date:
    day = override or git_added_date(fragment.path, root)
    if day is None:
        raise FragmentError(
            f"{fragment.path}: not committed, so it has no date; "
            f"commit it first or pass --date YYYY-MM-DD"
        )
    return day


def main(argv: list[str] | None = None) -> int:
    """Command-line entry point."""
    parser = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    parser.add_argument(
        "--root",
        type=Path,
        default=Path(__file__).resolve().parents[2],
        help="repository root (default: this checkout)",
    )
    mode = parser.add_mutually_exclusive_group()
    mode.add_argument(
        "--check", action="store_true", help="validate fragments and exit"
    )
    mode.add_argument(
        "--dry-run", action="store_true", help="print the diff; change nothing"
    )
    parser.add_argument(
        "--date",
        type=dt.date.fromisoformat,
        help="YYYY-MM-DD to use for every fragment instead of its git date",
    )
    parser.add_argument(
        "--keep", action="store_true", help="do not delete fragments after folding"
    )
    args = parser.parse_args(argv)

    root: Path = args.root.resolve()
    fragments, errors = collect_fragments(root / FRAGMENT_DIR)
    if errors:
        for error in errors:
            print(f"[X] {error}")
        return 1
    if args.check:
        print(f"[OK] {len(fragments)} changelog fragment(s) valid")
        return 0
    if not fragments:
        print("[OK] no changelog fragments to fold")
        return 0

    changelog = root / "CHANGELOG.md"
    before = changelog.read_text(encoding="utf-8")
    try:
        dates = {f.path: _fragment_date(f, root, args.date) for f in fragments}
        after, notices = fold(before, fragments, lambda f: dates[f.path])
    except FragmentError as exc:
        print(f"[X] {exc}")
        return 1
    for notice in notices:
        print(f"[--] {notice}")

    if args.dry_run:
        sys.stdout.writelines(
            difflib.unified_diff(
                before.splitlines(keepends=True),
                after.splitlines(keepends=True),
                "a/CHANGELOG.md",
                "b/CHANGELOG.md",
            )
        )
        return 0

    changelog.write_text(after, encoding="utf-8")
    if not args.keep:
        for fragment in fragments:
            fragment.path.unlink()
    print(f"[OK] folded {len(fragments)} fragment(s) into CHANGELOG.md")
    return 0


if __name__ == "__main__":
    sys.exit(main())
