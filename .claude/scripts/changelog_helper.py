#!/usr/bin/env python3
"""
Comprehensive CHANGELOG restructuring script that:
1. Parses CHANGELOG.md to structured JSON
2. Cleans duplicates and merges entries
3. Sorts properly by date
4. Formats back to clean markdown with:
   - Table of Contents by year
   - Annual statistics table
   - GitHub PR/issue links
   - Proper heading levels
"""

import re
import json
from datetime import datetime
from pathlib import Path
from typing import Dict, List, Tuple
import hashlib
from collections import defaultdict


class ChangelogRestructurer:
    def __init__(self, changelog_path: str = "CHANGELOG.md"):
        self.changelog_path = Path(changelog_path)
        # Support both old format (- DATE:) and new format (### DATE)
        self.date_pattern = re.compile(r"^(?:- |### )(\d{1,2} \w+ \d{4})(?::\s*)?$")
        self.entry_pattern = re.compile(r"^(?:  )?- \[(\w+)\] (.+)$")

    def parse_to_json(self) -> Dict:
        """Parse CHANGELOG.md into structured JSON format"""
        if not self.changelog_path.exists():
            raise FileNotFoundError(f"{self.changelog_path} not found")

        content = self.changelog_path.read_text()
        lines = content.split("\n")

        # Structure to hold parsed data
        data = {
            "header": [],
            "entries": {},  # date_str -> list of entries
            "raw_entries": [],  # For debugging
        }

        current_date = None
        current_entries = []
        in_header = True

        i = 0
        while i < len(lines):
            line = lines[i]

            # Check for date line
            date_match = self.date_pattern.match(line)
            if date_match:
                # Save previous date's entries
                if current_date and current_entries:
                    if current_date not in data["entries"]:
                        data["entries"][current_date] = []
                    data["entries"][current_date].extend(current_entries)
                    data["raw_entries"].append({
                        "date": current_date,
                        "entries": current_entries.copy(),
                    })

                # Start new date
                current_date = date_match.group(1)
                current_entries = []
                in_header = False

            elif in_header:
                data["header"].append(line)

            elif self.entry_pattern.match(line):
                # This is an entry line
                entry_match = self.entry_pattern.match(line)
                if entry_match:
                    category = entry_match.group(1)
                    description = entry_match.group(2)

                    # Start new entry
                    current_entry = {
                        "category": category,
                        "description": description,
                        "details": [],
                    }

                    # Look ahead for detail lines (starting with '  -' or '    -')
                    j = i + 1
                    while j < len(lines) and (
                        lines[j].startswith("    -") or lines[j].startswith("  -")
                    ):
                        # Skip if it's another category entry
                        if self.entry_pattern.match(lines[j]):
                            break
                        detail = lines[j].strip()
                        if detail.startswith("- "):
                            detail = detail[2:]  # Remove '- '
                        current_entry["details"].append(detail)
                        j += 1

                    current_entries.append(current_entry)
                    i = j - 1  # Skip the detail lines we just processed

            i += 1

        # Don't forget the last entry
        if current_date and current_entries:
            if current_date not in data["entries"]:
                data["entries"][current_date] = []
            data["entries"][current_date].extend(current_entries)
            data["raw_entries"].append({
                "date": current_date,
                "entries": current_entries,
            })

        return data

    def clean_entries(self, data: Dict) -> Dict:
        """Clean duplicates and merge similar entries"""
        cleaned_data = data.copy()
        cleaned_entries = {}

        # Track seen entries to remove duplicates
        seen_entries = set()

        for date_str, entries in data["entries"].items():
            cleaned_date_entries = []

            for entry in entries:
                # Create a hash of the entry to detect duplicates
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

    def sort_entries(self, data: Dict) -> List[Tuple[str, List]]:
        """Sort entries by date in reverse chronological order"""
        date_entries = []

        for date_str, entries in data["entries"].items():
            try:
                date_obj = datetime.strptime(date_str, "%d %b %Y")
                date_entries.append((date_obj, date_str, entries))
            except ValueError:
                print(f"Warning: Could not parse date '{date_str}'")

        # Sort by date object in reverse order
        date_entries.sort(key=lambda x: x[0], reverse=True)

        # Return sorted list of (date_str, entries) tuples
        return [(date_str, entries) for _, date_str, entries in date_entries]

    def format_github_links(self, text: str) -> str:
        """Convert #xxx and PR #xxx to GitHub links"""
        # Check if already formatted (contains markdown links)
        if "](https://github.com/" in text:
            return text

        # Pattern for PR mentions
        pr_pattern = r"PR #(\d+)"
        text = re.sub(
            pr_pattern, r"[PR #\1](https://github.com/UMEP-dev/SUEWS/pull/\1)", text
        )

        # Pattern for issue mentions (but not PR mentions and not already in brackets)
        issue_pattern = r"(?<!PR )(?<!\[)#(\d+)(?!\])"
        text = re.sub(
            issue_pattern, r"[#\1](https://github.com/UMEP-dev/SUEWS/issues/\1)", text
        )

        return text

    def generate_toc(self, sorted_entries: List[Tuple[str, List]]) -> List[str]:
        """Generate table of contents by year"""
        years = set()
        for date_str, _ in sorted_entries:
            try:
                date_obj = datetime.strptime(date_str, "%d %b %Y")
                years.add(date_obj.year)
            except ValueError:
                pass

        toc_lines = ["## Table of Contents", ""]
        for year in sorted(years, reverse=True):
            toc_lines.append(f"- [{year}](#{year})")

        return toc_lines

    def generate_stats_table(self, sorted_entries: List[Tuple[str, List]]) -> List[str]:
        """Generate annual statistics table"""
        # Collect stats by year
        stats_by_year = defaultdict(lambda: defaultdict(int))

        for date_str, entries in sorted_entries:
            try:
                date_obj = datetime.strptime(date_str, "%d %b %Y")
                year = date_obj.year

                for entry in entries:
                    category = entry["category"]
                    stats_by_year[year][category] += 1
                    stats_by_year[year]["total"] += 1

            except ValueError:
                pass

        # Generate table
        table_lines = [
            "",
            "## Annual Statistics",
            "",
            "| Year | Features | Bugfixes | Changes | Maintenance | Docs | Total |",
            "|------|----------|----------|---------|-------------|------|-------|",
        ]

        categories = ["feature", "bugfix", "change", "maintenance", "doc"]

        for year in sorted(stats_by_year.keys(), reverse=True):
            stats = stats_by_year[year]
            row = f"| {year} |"
            for cat in categories:
                count = stats.get(cat, 0)
                row += f" {count} |"
            row += f" {stats['total']} |"
            table_lines.append(row)

        return table_lines

    def format_to_markdown(
        self, data: Dict, sorted_entries: List[Tuple[str, List]]
    ) -> str:
        """Format the cleaned and sorted data back to markdown"""
        lines = []

        # Add header (but skip any existing TOC or stats tables)
        for line in data["header"]:
            # Skip lines that are part of TOC or stats table
            if line.startswith("## Table of Contents") or line.startswith(
                "## Annual Statistics"
            ):
                break
            if line.startswith("| Year |") or line.startswith("|------|"):
                continue
            if line.startswith("- [20") and "](#20" in line:  # TOC entry
                continue
            lines.append(line)

        # Remove trailing empty lines from header
        while lines and lines[-1] == "":
            lines.pop()

        lines.append("")  # Add single blank line after header

        # Add TOC
        toc = self.generate_toc(sorted_entries)
        lines.extend(toc)

        # Add stats table
        stats_table = self.generate_stats_table(sorted_entries)
        lines.extend(stats_table)
        lines.append("")  # Extra blank line

        # Group entries by year
        entries_by_year = defaultdict(list)
        for date_str, entries in sorted_entries:
            try:
                date_obj = datetime.strptime(date_str, "%d %b %Y")
                year = date_obj.year
                entries_by_year[year].append((date_str, entries))
            except ValueError:
                # If date parsing fails, put in "Unknown" year
                entries_by_year[0].append((date_str, entries))

        # Add entries grouped by year
        for year in sorted(entries_by_year.keys(), reverse=True):
            if year == 0:
                lines.append("\n## Unknown Year")
            else:
                lines.append(f"\n## {year}")

            lines.append("")

            for date_str, entries in entries_by_year[year]:
                lines.append(f"### {date_str}")

                # Group entries by category for better organization
                by_category = {}
                for entry in entries:
                    cat = entry["category"]
                    if cat not in by_category:
                        by_category[cat] = []
                    by_category[cat].append(entry)

                # Output in preferred category order
                category_order = ["feature", "bugfix", "change", "maintenance", "doc"]

                for category in category_order:
                    if category in by_category:
                        for entry in by_category[category]:
                            # Format description with GitHub links
                            description = self.format_github_links(entry["description"])
                            lines.append(f"- [{entry['category']}] {description}")

                            # Format details with GitHub links
                            for detail in entry["details"]:
                                formatted_detail = self.format_github_links(detail)
                                lines.append(f"  - {formatted_detail}")

                lines.append("")  # Blank line after each date

        # Remove trailing blank lines
        while lines and lines[-1] == "":
            lines.pop()

        return "\n".join(lines)

    def save_json_debug(self, data: Dict, filename: str = "changelog_debug.json"):
        """Save parsed data to JSON for debugging"""
        with open(filename, "w") as f:
            # Convert datetime objects to strings for JSON serialization
            json_data = data.copy()
            json.dump(json_data, f, indent=2, default=str)
        print(f"Debug data saved to {filename}")

    def restructure(self):
        """Main method to restructure the changelog"""
        print("Step 1: Parsing CHANGELOG.md to JSON...")
        data = self.parse_to_json()
        print(f"  Found {len(data['entries'])} unique dates")
        print(f"  Total raw entries: {len(data['raw_entries'])}")

        # Save debug JSON
        self.save_json_debug(data)

        print("\nStep 2: Cleaning duplicates...")
        cleaned_data = self.clean_entries(data)
        print(f"  Cleaned entries: {len(cleaned_data['entries'])} dates remain")

        print("\nStep 3: Sorting entries by date...")
        sorted_entries = self.sort_entries(cleaned_data)

        if sorted_entries:
            newest = sorted_entries[0][0]
            oldest = sorted_entries[-1][0]
            print(f"  Date range: {newest} → {oldest}")

        print("\nStep 4: Formatting back to markdown...")
        markdown = self.format_to_markdown(cleaned_data, sorted_entries)

        # Backup original
        backup_path = self.changelog_path.with_suffix(".md.backup")
        self.changelog_path.rename(backup_path)
        print(f"\nOriginal backed up to: {backup_path}")

        # Write new changelog
        self.changelog_path.write_text(markdown)
        print(f"✓ Restructured CHANGELOG saved to: {self.changelog_path}")

        # Summary statistics
        total_entries = sum(len(entries) for _, entries in sorted_entries)
        print(f"\nSummary:")
        print(f"  - Total dates: {len(sorted_entries)}")
        print(f"  - Total entries: {total_entries}")
        print(
            f"  - Average entries per date: {total_entries / len(sorted_entries):.1f}"
        )


def main():
    restructurer = ChangelogRestructurer()
    restructurer.restructure()


if __name__ == "__main__":
    main()
