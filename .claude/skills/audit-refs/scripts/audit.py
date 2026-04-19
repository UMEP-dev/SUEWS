#!/usr/bin/env python3
"""Audit SUEWS bib files for topic-tag convention compliance.

No network, no API key required. Every entry must carry a non-empty
`keywords` field whose values are drawn from the controlled vocabulary
defined below. Slugs must be lowercase, hyphen-separated, no spaces,
no uppercase.

Usage:
    uv run --no-project --with requests python audit.py <bib-file> [<bib-file>...]

Exit codes:
    0   all convention checks passed (abstract warnings may appear)
    1   one or more convention violations found
"""
from __future__ import annotations

import argparse
import re
import sys
from pathlib import Path

VOCAB: set[str] = {
    "energy-balance",
    "water-balance",
    "storage-heat",
    "radiation",
    "anthropogenic-heat",
    "carbon-flux",
    "building-energy",
    "model-infrastructure",
}

SLUG_RE = re.compile(r"^[a-z][a-z0-9]*(-[a-z0-9]+)*$")
ENTRY_START = re.compile(r"^@[A-Za-z]+\{([^,\s]+)\s*,", re.MULTILINE)

REQUIRED_FIELDS = ("title", "author", "year", "doi")


def _line_number(text: str, offset: int) -> int:
    return text.count("\n", 0, offset) + 1


def _match_field(body: str, name: str) -> tuple[int, int, str] | None:
    pattern = re.compile(rf"(^|\n)\s*{name}\s*=\s*\{{", re.IGNORECASE)
    m = pattern.search(body)
    if not m:
        return None
    open_brace = m.end() - 1
    depth = 1
    i = open_brace + 1
    while i < len(body) and depth > 0:
        c = body[i]
        if c == "{":
            depth += 1
        elif c == "}":
            depth -= 1
        i += 1
    if depth != 0:
        return None
    return m.start() + len(m.group(1)), i - 1, body[open_brace + 1:i - 1]


def extract_field(body: str, name: str) -> str | None:
    match = _match_field(body, name)
    return match[2] if match else None


def parse_slugs(raw: str) -> list[str]:
    return [s.strip() for s in raw.split(",") if s.strip()]


def audit_entry(entry: dict, file_path: str, all_keys: dict[str, str],
                violations: list[str], warnings: list[str]) -> None:
    key = entry["key"]
    body = entry["body"]
    line = entry["line"]
    prefix = f"{file_path}:{line} [{key}]"

    # Duplicate citation key check (across all files)
    if key in all_keys and all_keys[key] != f"{file_path}:{line}":
        violations.append(f"{prefix}: duplicate citation key (also at {all_keys[key]})")
    all_keys[key] = f"{file_path}:{line}"

    # keywords field
    keywords_raw = extract_field(body, "keywords")
    if keywords_raw is None:
        violations.append(f"{prefix}: missing `keywords` field")
    else:
        slugs = parse_slugs(keywords_raw)
        if not slugs:
            violations.append(f"{prefix}: `keywords` field is empty")
        for slug in slugs:
            if not SLUG_RE.match(slug):
                violations.append(
                    f"{prefix}: invalid slug format `{slug}` "
                    "(lowercase, hyphen-separated, no spaces)"
                )
            elif slug not in VOCAB:
                violations.append(
                    f"{prefix}: slug `{slug}` not in controlled vocabulary "
                    f"(allowed: {', '.join(sorted(VOCAB))})"
                )

    # Required fields
    for field in REQUIRED_FIELDS:
        val = extract_field(body, field)
        if val is None or not val.strip():
            violations.append(f"{prefix}: missing or empty `{field}`")

    # Abstract (warning only — collaborators without WoS access can still pass)
    abstract = extract_field(body, "abstract")
    if abstract is None or not abstract.strip():
        warnings.append(f"{prefix}: missing `abstract` (run `/audit-refs --enrich` if you have WoS/Crossref access)")


def find_entries(text: str) -> list[dict]:
    starts = list(ENTRY_START.finditer(text))
    entries = []
    for i, m in enumerate(starts):
        start = m.start()
        end = starts[i + 1].start() if i + 1 < len(starts) else len(text)
        entries.append({
            "key": m.group(1),
            "start": start,
            "end": end,
            "body": text[start:end],
            "line": _line_number(text, start),
        })
    return entries


def audit_file(path: Path, all_keys: dict[str, str]) -> tuple[int, list[str], list[str]]:
    text = path.read_text(encoding="utf-8")
    entries = find_entries(text)
    violations: list[str] = []
    warnings: list[str] = []
    for entry in entries:
        audit_entry(entry, str(path), all_keys, violations, warnings)
    return len(entries), violations, warnings


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0] if __doc__ else "")
    ap.add_argument("paths", nargs="+", help="Bib files to audit")
    ap.add_argument("--quiet", action="store_true",
                    help="Suppress per-file summary (only show violations/warnings/total)")
    args = ap.parse_args()

    all_keys: dict[str, str] = {}
    all_violations: list[str] = []
    all_warnings: list[str] = []
    total_entries = 0

    for p in args.paths:
        path = Path(p)
        if not path.exists():
            print(f"[error] {p} not found", file=sys.stderr)
            return 1
        n, violations, warnings = audit_file(path, all_keys)
        total_entries += n
        all_violations.extend(violations)
        all_warnings.extend(warnings)
        if not args.quiet:
            print(f"  {p}: {n} entries, {len(violations)} violations, {len(warnings)} warnings")

    if all_warnings:
        print("\n=== warnings ===")
        for w in all_warnings:
            print(f"  {w}")

    if all_violations:
        print("\n=== violations ===")
        for v in all_violations:
            print(f"  {v}")
        print(f"\n[FAIL] {len(all_violations)} violation(s) across {total_entries} entries")
        return 1

    print(f"\n[OK] {total_entries} entries pass convention audit"
          f" ({len(all_warnings)} warning(s))")
    return 0


if __name__ == "__main__":
    sys.exit(main())
