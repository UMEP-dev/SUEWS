#!/usr/bin/env python3
"""Enrich SUEWS bib files with abstracts via WoS/Crossref/OpenAlex.

Cascade: WoS Expanded -> WoS Starter -> Crossref -> OpenAlex.
WoS requires WOS_EXPANDED_API_KEY or WOS_API_KEY env var.
Crossref / OpenAlex are free, no key required.

In-place editing preserves original formatting. Only the `abstract` field is
inserted or overwritten (when it was empty); all other fields are untouched.
Entries already carrying a non-empty abstract are skipped (idempotent).

Usage:
    uv run --with requests python enrich.py \\
        docs/source/assets/refs/refs-SUEWS.bib \\
        docs/source/assets/refs/refs-community.bib

Options:
    --crossref-only     Skip WoS entirely (for collaborators without a key)
    --dry-run           Report what would change without writing
    --delay SECONDS     Pause between API calls (default 0.3)

Exit codes:
    0   enrichment completed (even if some entries unresolved)
    1   fatal error (no DOIs parsed, all APIs failing)
"""
from __future__ import annotations

import argparse
import json
import os
import re
import sys
import time
from pathlib import Path

import requests

USER_AGENT = "SUEWS-bib-enrich/1.0 (mailto:ting.sun@ucl.ac.uk)"
WOS_EXPANDED_URL = "https://api.clarivate.com/api/wos"
WOS_STARTER_URL = "https://api.clarivate.com/apis/wos-starter/v1/documents"
CROSSREF_URL = "https://api.crossref.org/works/{doi}"
OPENALEX_URL = "https://api.openalex.org/works/doi:{doi}"


# --- Abstract fetchers ---

def _clean_abstract(text: str) -> str:
    """Strip JATS/HTML tags and collapse whitespace."""
    if not text:
        return ""
    text = re.sub(r"<jats:[^>]+>", "", text)
    text = re.sub(r"</jats:[^>]+>", "", text)
    text = re.sub(r"<[^>]+>", "", text)
    text = re.sub(r"\s+", " ", text).strip()
    return text


def fetch_wos_expanded(doi: str, api_key: str) -> str | None:
    headers = {"X-ApiKey": api_key, "Accept": "application/json"}
    params = {
        "databaseId": "WOS",
        "usrQuery": f'DO=("{doi}")',
        "count": 1,
        "firstRecord": 1,
    }
    try:
        r = requests.get(WOS_EXPANDED_URL, headers=headers, params=params, timeout=20)
        if r.status_code != 200:
            return None
        data = r.json()
        records = data.get("Data", {}).get("Records", {}).get("records", {}).get("REC")
        if not records:
            return None
        if isinstance(records, dict):
            records = [records]
        rec = records[0]
        blocks = (
            rec.get("static_data", {})
            .get("fullrecord_metadata", {})
            .get("abstracts", {})
            .get("abstract")
        )
        if not blocks:
            return None
        if isinstance(blocks, dict):
            blocks = [blocks]
        paragraphs: list[str] = []
        for block in blocks:
            p = block.get("abstract_text", {}).get("p")
            if p is None:
                continue
            if isinstance(p, str):
                paragraphs.append(p)
            elif isinstance(p, list):
                paragraphs.extend(str(x) for x in p)
        return _clean_abstract(" ".join(paragraphs)) or None
    except Exception:
        return None


def fetch_wos_starter(doi: str, api_key: str) -> str | None:
    headers = {"X-ApiKey": api_key}
    params = {"q": f'DO="{doi}"', "limit": 1, "db": "WOS", "detail": "full"}
    try:
        r = requests.get(WOS_STARTER_URL, headers=headers, params=params, timeout=20)
        if r.status_code != 200:
            return None
        hits = r.json().get("hits", [])
        if not hits:
            return None
        abs_text = hits[0].get("abstract") or hits[0].get("citation", {}).get("abstract")
        return _clean_abstract(abs_text) if abs_text else None
    except Exception:
        return None


def fetch_crossref(doi: str) -> str | None:
    headers = {"User-Agent": USER_AGENT}
    try:
        r = requests.get(CROSSREF_URL.format(doi=doi), headers=headers, timeout=20)
        if r.status_code != 200:
            return None
        abs_text = r.json().get("message", {}).get("abstract")
        return _clean_abstract(abs_text) if abs_text else None
    except Exception:
        return None


def fetch_openalex(doi: str) -> str | None:
    headers = {"User-Agent": USER_AGENT}
    try:
        r = requests.get(OPENALEX_URL.format(doi=doi), headers=headers, timeout=20)
        if r.status_code != 200:
            return None
        idx = r.json().get("abstract_inverted_index")
        if not idx:
            return None
        positions: list[tuple[int, str]] = []
        for word, locs in idx.items():
            for loc in locs:
                positions.append((loc, word))
        positions.sort()
        return _clean_abstract(" ".join(w for _, w in positions)) or None
    except Exception:
        return None


def fetch_abstract(doi: str, *, crossref_only: bool, wos_expanded_key: str | None,
                   wos_starter_key: str | None) -> tuple[str | None, str | None]:
    """Return (abstract, source_name). source_name is None on failure."""
    if not crossref_only:
        if wos_expanded_key:
            result = fetch_wos_expanded(doi, wos_expanded_key)
            if result:
                return result, "wos-expanded"
        if wos_starter_key:
            result = fetch_wos_starter(doi, wos_starter_key)
            if result:
                return result, "wos-starter"
    result = fetch_crossref(doi)
    if result:
        return result, "crossref"
    result = fetch_openalex(doi)
    if result:
        return result, "openalex"
    return None, None


# --- Bib in-place editing ---

ENTRY_START = re.compile(r"^@[A-Za-z]+\{([^,\s]+)\s*,", re.MULTILINE)


def find_entries(text: str) -> list[dict]:
    """Return list of entries: {key, start, end, body}."""
    starts = list(ENTRY_START.finditer(text))
    entries = []
    for i, m in enumerate(starts):
        start = m.start()
        end = starts[i + 1].start() if i + 1 < len(starts) else len(text)
        body = text[start:end]
        entries.append({"key": m.group(1), "start": start, "end": end, "body": body})
    return entries


def _match_field(body: str, name: str) -> tuple[int, int, str] | None:
    """Find a top-level field `name = {...}`. Returns (start, end, value) or None.

    Handles nested braces in the value.
    """
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
    value_start = open_brace + 1
    value_end = i - 1
    # Extend end to include trailing comma and newline
    tail = i
    if tail < len(body) and body[tail] == ",":
        tail += 1
    if tail < len(body) and body[tail] == "\n":
        tail += 1
    return m.start() + len(m.group(1)), tail, body[value_start:value_end]


def extract_field(body: str, name: str) -> str | None:
    found = _match_field(body, name)
    return found[2] if found else None


def set_abstract(body: str, abstract: str) -> str:
    """Return body with abstract field inserted or updated.

    Preserves existing indentation style of other fields.
    """
    # Detect indentation by looking at any field line
    indent_m = re.search(r"\n(\s*)[A-Za-z_]+\s*=\s*\{", body)
    indent = indent_m.group(1) if indent_m else " "

    # Escape braces if present in abstract (unlikely for clean abstracts)
    safe_abstract = abstract.replace("\\", "\\\\")

    new_line = f"{indent}abstract = {{{safe_abstract}}},\n"

    existing = _match_field(body, "abstract")
    if existing:
        start, end, _ = existing
        return body[:start] + new_line + body[end:]

    # Insert before the closing brace of the entry. Ensure the preceding
    # field line ends with a comma so the new field parses cleanly.
    close_match = re.search(r"\}\s*$", body)
    if not close_match:
        return body  # malformed — skip
    close_pos = close_match.start()
    prefix = body[:close_pos]
    stripped = prefix.rstrip()
    if stripped and stripped[-1] not in ",{":
        prefix = stripped + ",\n"
    return prefix + new_line + body[close_pos:]


def extract_doi(body: str) -> str | None:
    val = extract_field(body, "doi")
    if not val:
        return None
    return val.strip()


def needs_abstract(body: str) -> bool:
    val = extract_field(body, "abstract")
    if val is None:
        return True
    return val.strip() == ""


# --- Driver ---

def process_file(path: Path, *, crossref_only: bool, dry_run: bool, delay: float,
                 wos_expanded_key: str | None, wos_starter_key: str | None) -> dict:
    text = path.read_text(encoding="utf-8")
    entries = find_entries(text)
    stats = {"total": len(entries), "already": 0, "enriched": 0,
             "no_doi": 0, "unresolved": 0, "by_source": {}}
    # Process last-to-first so offsets stay valid for naive string operations
    # (not strictly necessary here since we rebuild the text each iteration)
    new_text = text
    cursor_entries = find_entries(new_text)
    for entry in cursor_entries:
        body = entry["body"]
        key = entry["key"]
        if not needs_abstract(body):
            stats["already"] += 1
            continue
        doi = extract_doi(body)
        if not doi:
            stats["no_doi"] += 1
            print(f"  [{key}] skip: no DOI")
            continue
        abstract, source = fetch_abstract(
            doi,
            crossref_only=crossref_only,
            wos_expanded_key=wos_expanded_key,
            wos_starter_key=wos_starter_key,
        )
        if not abstract:
            stats["unresolved"] += 1
            print(f"  [{key}] unresolved: {doi}")
            time.sleep(delay)
            continue
        stats["enriched"] += 1
        stats["by_source"][source] = stats["by_source"].get(source, 0) + 1
        print(f"  [{key}] via {source}: {abstract[:80]}...")
        if not dry_run:
            new_body = set_abstract(body, abstract)
            new_text = new_text.replace(body, new_body, 1)
        time.sleep(delay)

    if not dry_run and new_text != text:
        path.write_text(new_text, encoding="utf-8")
    return stats


def main() -> int:
    ap = argparse.ArgumentParser(description="Enrich SUEWS bib files with abstracts")
    ap.add_argument("paths", nargs="+", help="Bib files to enrich (in place)")
    ap.add_argument("--crossref-only", action="store_true",
                    help="Skip WoS entirely; useful for collaborators without a WoS key")
    ap.add_argument("--dry-run", action="store_true",
                    help="Report without writing changes")
    ap.add_argument("--delay", type=float, default=0.3,
                    help="Seconds between API calls")
    args = ap.parse_args()

    wos_expanded_key = os.environ.get("WOS_EXPANDED_API_KEY")
    wos_starter_key = os.environ.get("WOS_API_KEY")
    if args.crossref_only:
        wos_expanded_key = None
        wos_starter_key = None

    if not args.crossref_only and not wos_expanded_key and not wos_starter_key:
        print("[warn] No WoS key found (WOS_EXPANDED_API_KEY / WOS_API_KEY).", file=sys.stderr)
        print("[warn] Falling back to Crossref + OpenAlex only. Pass --crossref-only to silence.",
              file=sys.stderr)

    totals = {"total": 0, "already": 0, "enriched": 0, "no_doi": 0, "unresolved": 0,
              "by_source": {}}

    for p in args.paths:
        path = Path(p)
        if not path.exists():
            print(f"[error] {p} not found", file=sys.stderr)
            return 1
        print(f"\n=== {p} ===")
        stats = process_file(
            path,
            crossref_only=args.crossref_only,
            dry_run=args.dry_run,
            delay=args.delay,
            wos_expanded_key=wos_expanded_key,
            wos_starter_key=wos_starter_key,
        )
        for key, val in stats.items():
            if key == "by_source":
                continue
            totals[key] += val
        for source, count in stats["by_source"].items():
            totals["by_source"][source] = totals["by_source"].get(source, 0) + count
        print(f"  -> {stats['enriched']} enriched, {stats['already']} already populated, "
              f"{stats['no_doi']} no DOI, {stats['unresolved']} unresolved")

    print("\n=== totals ===")
    print(json.dumps(totals, indent=2))
    return 0


if __name__ == "__main__":
    sys.exit(main())
