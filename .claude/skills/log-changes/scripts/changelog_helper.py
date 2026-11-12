#!/usr/bin/env python3
"""
Comprehensive CHANGELOG restructuring script that:
1. Parses CHANGELOG.md to structured JSON (preserving optional governance status tags)
2. Cleans duplicates and merges entries
3. Sorts properly by date
4. Formats back to clean markdown while preserving existing Table of Contents
   and Annual Statistics sections (or generating fallbacks when absent)
"""

import argparse
from collections import defaultdict
import contextlib
from datetime import datetime
import hashlib
import json
from pathlib import Path
import re

CATEGORY_NORMALISE = {
    "changes": "change",
    "docs": "doc",
}

_LINK_TOKEN_RE = re.compile(
    r"(?P<protected>`[^`]*`|\[[^\]]+\]\([^)]+\)|https?://\S+)"
    r"|(?<!\w)PR\s*#(?P<pr>\d+)\b"
    r"|(?<!\w)gh#(?P<gh>\d+)\b"
    r"|(?<![\w/#\[])#(?P<issue>\d+)\b(?!\])"
)


class ChangelogRestructurer:
    def __init__(self, changelog_path: str = "CHANGELOG.md"):
        self.changelog_path = Path(changelog_path)
        self.date_pattern = re.compile(r"^(?:- |### )(\d{1,2} \w+ \d{4})(?::\s*)?$")
        self.year_heading_pattern = re.compile(r"^## (\d{4}|Unknown Year)\s*$")
        self.entry_pattern = re.compile(
            r"^(?:  )?- \[(feature|bugfix|change|changes|maintenance|doc|docs)\]"
            r"(?:\[(experimental|stable|internal)\])?:?\s+(.+)$"
        )

    @staticmethod
    def _record_date_entries(
        data: dict, current_date: str | None, current_entries: list[dict]
    ) -> None:
        if not current_date or not current_entries:
            return
        data["entries"].setdefault(current_date, []).extend(current_entries)
        data["raw_entries"].append({
            "date": current_date,
            "entries": current_entries.copy(),
        })

    @staticmethod
    def _consume_preamble_line(data: dict, section: str, line: str) -> str:
        if line.startswith("## Table of Contents"):
            data["toc_lines"].append(line)
            return "toc"
        if line.startswith("## Annual Statistics"):
            data["stats_lines"].append(line)
            return "stats"
        if section == "toc":
            data["toc_lines"].append(line)
        elif section == "stats":
            data["stats_lines"].append(line)
        else:
            data["header"].append(line)
        return section

    def _consume_entry_block(
        self, lines: list[str], start_idx: int, entry_match: re.Match[str]
    ) -> tuple[dict[str, object], int]:
        raw_cat = entry_match.group(1)
        category = CATEGORY_NORMALISE.get(raw_cat, raw_cat)
        details: list[str] = []
        j = start_idx + 1
        while j < len(lines) and (
            lines[j].startswith("    -") or lines[j].startswith("  -")
        ):
            if self.entry_pattern.match(lines[j]):
                break
            detail = lines[j].strip()
            if detail.startswith("- "):
                detail = detail[2:]
            details.append(detail)
            j += 1
        entry: dict[str, object] = {
            "category": category,
            "status": entry_match.group(2),
            "description": entry_match.group(3),
            "details": details,
        }
        return entry, j - 1

    def parse_to_json(self) -> dict:
        """Parse CHANGELOG.md into structured JSON format."""
        if not self.changelog_path.exists():
            raise FileNotFoundError(f"{self.changelog_path} not found")

        lines = self.changelog_path.read_text(encoding="utf-8").split("\n")
        data: dict = {
            "header": [],
            "toc_lines": [],
            "stats_lines": [],
            "entries": {},
            "raw_entries": [],
        }

        current_date: str | None = None
        current_entries: list[dict] = []
        preamble_section: str | None = "header"

        i = 0
        while i < len(lines):
            line = lines[i]
            date_match = self.date_pattern.match(line)
            if date_match:
                self._record_date_entries(data, current_date, current_entries)
                current_date = date_match.group(1)
                current_entries = []
                preamble_section = None
            elif self.year_heading_pattern.match(line):
                preamble_section = None
            elif preamble_section is not None:
                preamble_section = self._consume_preamble_line(
                    data, preamble_section, line
                )
            else:
                entry_match = self.entry_pattern.match(line)
                if entry_match:
                    entry, i = self._consume_entry_block(lines, i, entry_match)
                    current_entries.append(entry)
            i += 1

        self._record_date_entries(data, current_date, current_entries)
        return data

    @staticmethod
    def clean_entries(data: dict) -> dict:
        """Clean duplicates and merge similar entries."""
        cleaned_data = data.copy()
        cleaned_entries = {}
        seen_entries = set()

        for date_str, entries in data["entries"].items():
            cleaned_date_entries = []
            for entry in entries:
                entry_hash = hashlib.md5(
                    json.dumps(entry, sort_keys=True).encode()
                ).hexdigest()
                if entry_hash not in seen_entries:
                    seen_entries.add(entry_hash)
                    cleaned_date_entries.append(entry)
            if cleaned_date_entries:
                cleaned_entries[date_str] = cleaned_date_entries

        cleaned_data["entries"] = cleaned_entries
        return cleaned_data

    @staticmethod
    def sort_entries(data: dict) -> list[tuple[str, list]]:
        """Sort entries by date in reverse chronological order."""
        date_entries = []
        for date_str, entries in data["entries"].items():
            try:
                date_obj = datetime.strptime(date_str, "%d %b %Y")
                date_entries.append((date_obj, date_str, entries))
            except ValueError:
                print(f"Warning: Could not parse date '{date_str}'")

        date_entries.sort(key=lambda x: x[0], reverse=True)
        return [(date_str, entries) for _, date_str, entries in date_entries]

    @staticmethod
    def format_github_links(text: str) -> str:
        """Convert bare #NNN, gh#NNN, and PR #NNN references to GitHub links."""

        def _replace(match: re.Match[str]) -> str:
            if match.group("protected") is not None:
                return match.group("protected")
            if match.group("pr") is not None:
                num = match.group("pr")
                return f"[PR #{num}](https://github.com/UMEP-dev/SUEWS/pull/{num})"
            if match.group("gh") is not None:
                num = match.group("gh")
                return f"[gh#{num}](https://github.com/UMEP-dev/SUEWS/issues/{num})"
            num = match.group("issue")
            return f"[#{num}](https://github.com/UMEP-dev/SUEWS/issues/{num})"

        return _LINK_TOKEN_RE.sub(_replace, text)

    @staticmethod
    def generate_toc(sorted_entries: list[tuple[str, list]]) -> list[str]:
        """Generate fallback table of contents by year when none exists."""
        years = set()
        for date_str, _ in sorted_entries:
            with contextlib.suppress(ValueError):
                years.add(datetime.strptime(date_str, "%d %b %Y").year)

        toc_lines = ["## Table of Contents", ""]
        for year in sorted(years, reverse=True):
            toc_lines.append(f"- [{year}](#{year})")
        return toc_lines

    @staticmethod
    def generate_stats_table(sorted_entries: list[tuple[str, list]]) -> list[str]:
        """Generate fallback annual statistics table when none exists."""
        stats_by_year: dict[int, dict[str, int]] = defaultdict(lambda: defaultdict(int))
        for date_str, entries in sorted_entries:
            with contextlib.suppress(ValueError):
                year = datetime.strptime(date_str, "%d %b %Y").year
                for entry in entries:
                    stats_by_year[year][entry["category"]] += 1
                    stats_by_year[year]["total"] += 1

        table_lines = [
            "## Annual Statistics",
            "",
            "| Year | Features | Bugfixes | Changes | Maintenance | Docs | Total |",
            "|------|----------|----------|---------|-------------|------|-------|",
        ]
        categories = ["feature", "bugfix", "change", "maintenance", "doc"]
        for year in sorted(stats_by_year.keys(), reverse=True):
            stats = stats_by_year[year]
            counts = " ".join(f"{stats.get(cat, 0)} |" for cat in categories)
            table_lines.append(f"| {year} | {counts} {stats['total']} |")
        return table_lines

    @staticmethod
    def _trim_trailing_blanks(section_lines: list[str]) -> list[str]:
        trimmed = list(section_lines)
        while trimmed and not trimmed[-1]:
            trimmed.pop()
        return trimmed

    @staticmethod
    def _group_by_year(
        sorted_entries: list[tuple[str, list]],
    ) -> dict[int, list[tuple[str, list]]]:
        entries_by_year: dict[int, list[tuple[str, list]]] = defaultdict(list)
        for date_str, entries in sorted_entries:
            try:
                year = datetime.strptime(date_str, "%d %b %Y").year
                entries_by_year[year].append((date_str, entries))
            except ValueError:
                entries_by_year[0].append((date_str, entries))
        return entries_by_year

    def _render_date_block(self, date_str: str, entries: list[dict]) -> list[str]:
        preferred_order = ["feature", "bugfix", "change", "maintenance", "doc"]
        block = [f"### {date_str}", ""]
        by_category: dict[str, list[dict]] = defaultdict(list)
        for entry in entries:
            by_category[entry["category"]].append(entry)

        ordered_categories = preferred_order + [
            cat for cat in by_category if cat not in preferred_order
        ]
        for category in ordered_categories:
            for entry in by_category.get(category, []):
                description = self.format_github_links(entry["description"])
                status = entry.get("status")
                status_tag = f"[{status}]" if status else ""
                block.append(f"- [{entry['category']}]{status_tag} {description}")
                for detail in entry["details"]:
                    block.append(f"  - {self.format_github_links(detail)}")
        block.append("")
        return block

    def format_to_markdown(
        self, data: dict, sorted_entries: list[tuple[str, list]]
    ) -> str:
        """Format the cleaned and sorted data back to markdown."""
        lines: list[str] = []

        header_lines = self._trim_trailing_blanks(data.get("header", []))
        if header_lines:
            lines.extend(header_lines)
            lines.append("")

        preserved_toc = self._trim_trailing_blanks(data.get("toc_lines", []))
        toc = preserved_toc if preserved_toc else self.generate_toc(sorted_entries)
        if toc:
            lines.extend(toc)
            lines.append("")

        preserved_stats = self._trim_trailing_blanks(data.get("stats_lines", []))
        stats_table = (
            preserved_stats
            if preserved_stats
            else self.generate_stats_table(sorted_entries)
        )
        if stats_table:
            lines.extend(stats_table)

        entries_by_year = self._group_by_year(sorted_entries)
        for year in sorted(entries_by_year.keys(), reverse=True):
            lines.append("\n## Unknown Year" if year == 0 else f"\n## {year}")
            lines.append("")
            for date_str, entries in entries_by_year[year]:
                lines.extend(self._render_date_block(date_str, entries))

        lines = self._trim_trailing_blanks(lines)
        return "\n".join(lines) + "\n"

    def save_json_debug(self, data: dict, filename: str | None = None) -> Path:
        """Save parsed data to JSON for debugging."""
        target = (
            Path(filename)
            if filename is not None
            else self.changelog_path.with_name(f"{self.changelog_path.stem}_debug.json")
        )
        with open(target, "w", encoding="utf-8") as f:
            json.dump(data.copy(), f, indent=2, default=str)
        print(f"Debug data saved to {target}")
        return target

    def restructure(self, save_debug: bool = True) -> None:
        """Restructure CHANGELOG.md in place after writing a .backup copy."""
        print("Step 1: Parsing CHANGELOG.md to JSON...")
        data = self.parse_to_json()
        print(f"  Found {len(data['entries'])} unique dates")
        print(f"  Total raw entries: {len(data['raw_entries'])}")

        if save_debug:
            self.save_json_debug(data)

        print("\nStep 2: Cleaning duplicates...")
        cleaned_data = self.clean_entries(data)
        print(f"  Cleaned entries: {len(cleaned_data['entries'])} dates remain")

        print("\nStep 3: Sorting entries by date...")
        sorted_entries = self.sort_entries(cleaned_data)
        if sorted_entries:
            print(f"  Date range: {sorted_entries[0][0]} -> {sorted_entries[-1][0]}")

        print("\nStep 4: Formatting back to markdown...")
        markdown = self.format_to_markdown(cleaned_data, sorted_entries)

        backup_path = self.changelog_path.with_suffix(".md.backup")
        self.changelog_path.rename(backup_path)
        print(f"\nOriginal backed up to: {backup_path}")

        self.changelog_path.write_text(markdown, encoding="utf-8")
        print(f"[OK] Restructured CHANGELOG saved to: {self.changelog_path}")

        total_entries = sum(len(entries) for _, entries in sorted_entries)
        print("\nSummary:")
        print(f"  - Total dates: {len(sorted_entries)}")
        print(f"  - Total entries: {total_entries}")
        if sorted_entries:
            print(
                f"  - Average entries per date: {total_entries / len(sorted_entries):.1f}"
            )


def main() -> None:
    parser = argparse.ArgumentParser(description="Restructure SUEWS CHANGELOG.md")
    parser.add_argument(
        "path",
        nargs="?",
        default="CHANGELOG.md",
        help="Path to CHANGELOG markdown file (default: CHANGELOG.md)",
    )
    parser.add_argument(
        "--no-debug-json",
        action="store_true",
        help="Skip writing <stem>_debug.json",
    )
    args = parser.parse_args()
    restructurer = ChangelogRestructurer(args.path)
    restructurer.restructure(save_debug=not args.no_debug_json)


if __name__ == "__main__":
    main()
