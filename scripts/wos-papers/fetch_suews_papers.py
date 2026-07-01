#!/usr/bin/env python3
"""
Fetch SUEWS-related papers from Web of Science and build the documentation data.

This script queries the Web of Science Expanded API for all papers related to
the SUEWS (Surface Urban Energy and Water Balance Scheme) model, enriches DOIs
and journal names against Crossref, analyses the geographic and thematic spread,
and writes the artefacts that drive the "Global Applications" documentation page:
a JSON export, a CSV table, the ``refs-wos.bib`` bibliography, and a generated
RST fragment.

Requirements:
    - WOS_EXPANDED_API_KEY environment variable (institutional subscription).
      Falls back to WOS_API_KEY, but the Expanded endpoint needs the Expanded
      key (the Starter key returns HTTP 403).
    - requests library
    - Network access to Crossref for the enrichment pass (skippable with
      --no-enrich).

Usage:
    # Refresh every documentation artefact in place (run from the repo root)
    export WOS_EXPANDED_API_KEY="your-api-key"
    python scripts/wos-papers/fetch_suews_papers.py

    # Write everything flat into a scratch directory instead
    python scripts/wos-papers/fetch_suews_papers.py --output-dir ./output

    # Skip Crossref enrichment (journal names still title-cased)
    python scripts/wos-papers/fetch_suews_papers.py --no-enrich

Author: Ting Sun (ting.sun@ucl.ac.uk)
Created: 2025-11-26
Repository: https://github.com/UMEP-dev/SUEWS
"""

from __future__ import annotations

import argparse
import csv
import html
import json
import os
import re
import sys
import time
from collections import Counter, defaultdict
from dataclasses import asdict, dataclass, field
from datetime import datetime
from difflib import SequenceMatcher
from io import StringIO
from pathlib import Path
from urllib.parse import quote

try:
    import requests
except ImportError:
    print("Error: 'requests' library required. Install with: pip install requests")
    sys.exit(1)


@dataclass
class Paper:
    """Represents a single academic paper."""

    uid: str
    title: str
    authors: list[str]
    author_count: int
    journal: str
    year: int
    volume: str
    issue: str
    pages: str
    doi: str
    abstract: str
    keywords: list[str] = field(default_factory=list)
    affiliations: list[str] = field(default_factory=list)
    cite_key: str = ""


def clean_wos_text(text: str) -> str:
    """Strip HTML/XML markup and decode entities from WoS-supplied text.

    WoS titles and abstracts arrive with markup (``<i>``, ``<sub>``) and SGML
    entities (``&amp;``, ``&lowast;``). Decode them and collapse whitespace so
    titles render cleanly in the BibTeX output.
    """
    if not text:
        return ""
    text = re.sub(r"<[^>]+>", "", text)  # strip HTML/XML tags
    text = html.unescape(text)  # decode &amp; &lt; &lowast; ...
    text = re.sub(r"\s+", " ", text).strip()  # collapse whitespace
    return text


# Small words kept lowercase when title-casing an all-caps journal name,
# except as the first word.
_JOURNAL_MINOR_WORDS = {
    "a",
    "an",
    "and",
    "as",
    "at",
    "but",
    "by",
    "for",
    "from",
    "in",
    "into",
    "nor",
    "of",
    "on",
    "or",
    "the",
    "to",
    "via",
    "with",
}


def titlecase_journal(name: str) -> str:
    """Convert a raw WoS ALL-CAPS journal name to Title Case.

    WoS returns journal names fully capitalised (``SUSTAINABLE CITIES AND
    SOCIETY``). Title-case them while keeping minor words lowercase. This is the
    deterministic fallback; Crossref enrichment overrides it with the canonical
    container title where a match is found.
    """
    if not name:
        return ""
    # Only reshape names that are essentially all upper case; leave already
    # mixed-case names (e.g. from Crossref) untouched.
    letters = [c for c in name if c.isalpha()]
    if letters and not all(c.isupper() for c in letters):
        return name
    words = name.split()
    out = []
    for i, word in enumerate(words):
        lower = word.lower()
        if i != 0 and lower in _JOURNAL_MINOR_WORDS:
            out.append(lower)
        else:
            out.append(lower.capitalize())
    return " ".join(out)


def _as_list(value) -> list:
    """Normalise a WoS field that may be a single dict, a list, or absent.

    The WoS JSON collapses single-element collections to a bare object, so a
    field that is a list for multi-author papers is a dict for single-author
    ones. Always return a list so callers can iterate uniformly.
    """
    if value is None:
        return []
    if isinstance(value, list):
        return value
    return [value]


# City list keyed by region, shared by the geographic analysis and the CSV
# region column. Matching is whole-word, case-insensitive, against title +
# abstract text.
CITIES_BY_REGION = {
    "Europe": [
        "London",
        "Helsinki",
        "Dublin",
        "Porto",
        "Swindon",
        "Hamburg",
        "Heraklion",
        "Zurich",
        "Freiburg",
        "Gothenburg",
        "Reading",
        "Paris",
        "Madrid",
        "Rome",
        "Berlin",
        "Munich",
        "Frankfurt",
        "Amsterdam",
        "Brussels",
        "Vienna",
        "Prague",
        "Warsaw",
        "Stockholm",
        "Oslo",
        "Copenhagen",
        "Lisbon",
        "Barcelona",
        "Milan",
        "Athens",
        "Budapest",
        "Edinburgh",
        "Glasgow",
        "Manchester",
        "Birmingham",
        "Marseille",
        "Lyon",
        "Rotterdam",
        "Cambridge",
        "Oxford",
    ],
    "Asia": [
        "Beijing",
        "Shanghai",
        "Singapore",
        "Xiong'an",
        "Xiongan",
        "Baoding",
        "Mumbai",
        "Tokyo",
        "Nanjing",
        "Guangzhou",
        "Shenzhen",
        "Hangzhou",
        "Wuhan",
        "Chengdu",
        "Tianjin",
        "Chongqing",
        "Suzhou",
        "Dalian",
        "Qingdao",
        "Taipei",
        "Hong Kong",
        "Kuala Lumpur",
        "Bangkok",
        "Jakarta",
        "Manila",
        "Seoul",
        "Osaka",
        "Nagoya",
        "Kyoto",
        "Delhi",
        "Kolkata",
        "Chennai",
        "Bangalore",
        "Hyderabad",
        "Colombo",
        "Hanoi",
        "Ho Chi Minh",
    ],
    "North America": [
        "Vancouver",
        "Phoenix",
        "Montreal",
        "Los Angeles",
        "Baltimore",
        "New York",
        "Chicago",
        "Boston",
        "Seattle",
        "Denver",
        "Miami",
        "Houston",
        "Dallas",
        "San Francisco",
        "San Diego",
        "Portland",
        "Austin",
        "Atlanta",
        "Philadelphia",
        "Washington",
        "Detroit",
        "Minneapolis",
        "Toronto",
        "Calgary",
        "Edmonton",
        "Ottawa",
        "Mexico City",
    ],
    "South America": [
        "Sao Paulo",
        "Rio de Janeiro",
        "Buenos Aires",
        "Bogota",
        "Lima",
        "Santiago",
        "Caracas",
        "Montevideo",
        "Bahia Blanca",
        "Brasilia",
    ],
    "Oceania": [
        "Melbourne",
        "Sydney",
        "Brisbane",
        "Perth",
        "Adelaide",
        "Auckland",
        "Wellington",
        "Christchurch",
        "Canberra",
    ],
    "Africa": [
        "Cape Town",
        "Cairo",
        "Lagos",
        "Nairobi",
        "Johannesburg",
        "Casablanca",
        "Accra",
        "Dakar",
        "Addis Ababa",
    ],
    "Middle East": [
        "Dubai",
        "Abu Dhabi",
        "Riyadh",
        "Doha",
        "Tel Aviv",
        "Istanbul",
        "Ankara",
    ],
}


class WoSClient:
    """Client for Web of Science Expanded API."""

    BASE_URL = "https://wos-api.clarivate.com/api/wos"

    def __init__(self, api_key: str, rate_limit: float = 0.5):
        """
        Initialize WoS client.

        Args:
            api_key: Web of Science API key
            rate_limit: Minimum seconds between requests (default: 0.5)
        """
        self.api_key = api_key
        self.rate_limit = rate_limit
        self.last_request = 0
        self.headers = {"X-ApiKey": api_key, "Accept": "application/json"}

    def _respect_rate_limit(self):
        """Implement rate limiting between API calls."""
        elapsed = time.time() - self.last_request
        if elapsed < self.rate_limit:
            time.sleep(self.rate_limit - elapsed)
        self.last_request = time.time()

    def search(
        self, query: str, database: str = "WOS", count: int = 50, first_record: int = 1
    ) -> dict:
        """
        Execute a search query against WoS.

        Args:
            query: WoS search query string
            database: Database to search (default: WOS)
            count: Number of records per request (max 50)
            first_record: Starting record number (1-indexed)

        Returns:
            API response as dictionary
        """
        self._respect_rate_limit()

        params = {
            "databaseId": database,
            "usrQuery": query,
            "count": min(count, 50),
            "firstRecord": first_record,
        }

        response = requests.get(
            self.BASE_URL, headers=self.headers, params=params, timeout=60
        )

        response.raise_for_status()
        return response.json()

    def get_total_records(self, query: str) -> int:
        """Get total number of records matching query."""
        result = self.search(query, count=1)
        return result.get("QueryResult", {}).get("RecordsFound", 0)

    def fetch_all(self, query: str, progress_callback=None) -> list[dict]:
        """
        Fetch all records matching a query.

        Args:
            query: WoS search query
            progress_callback: Optional callback(current, total) for progress

        Returns:
            List of raw record dictionaries
        """
        total = self.get_total_records(query)
        if progress_callback:
            progress_callback(0, total)

        all_records = []
        first_record = 1

        while first_record <= total:
            result = self.search(query, count=50, first_record=first_record)
            records = (
                result.get("Data", {})
                .get("Records", {})
                .get("records", {})
                .get("REC", [])
            )

            all_records.extend(records)
            first_record += 50

            if progress_callback:
                progress_callback(len(all_records), total)

        return all_records


def parse_record(record: dict) -> Paper:
    """
    Parse a WoS record into a Paper object.

    Args:
        record: Raw WoS API record

    Returns:
        Parsed Paper object
    """
    uid = record.get("UID", "")
    static_data = record.get("static_data", {})
    summary = static_data.get("summary", {})
    fullrecord = static_data.get("fullrecord_metadata", {})

    # Title and journal
    titles = _as_list(summary.get("titles", {}).get("title"))
    title = clean_wos_text(
        next((t.get("content", "") for t in titles if t.get("type") == "item"), "")
    )
    journal = next(
        (t.get("content", "") for t in titles if t.get("type") == "source"), ""
    )

    # Publication info
    pub_info = summary.get("pub_info", {})
    year = pub_info.get("pubyear", 0)
    volume = str(pub_info.get("vol", ""))
    issue = str(pub_info.get("issue", ""))
    page_data = pub_info.get("page", {})
    pages = page_data.get("content", "") if isinstance(page_data, dict) else ""

    # Authors (full list; WoS returns a dict when there is a single author)
    authors = []
    for n in _as_list(summary.get("names", {}).get("name")):
        if n.get("role") == "author":
            name = n.get("full_name", n.get("display_name", ""))
            if name:
                authors.append(name)

    # DOI: dynamic_data is a sibling of static_data on the record, not a child.
    doi = ""
    identifiers = _as_list(
        record.get("dynamic_data", {})
        .get("cluster_related", {})
        .get("identifiers", {})
        .get("identifier")
    )
    for ident in identifiers:
        if ident.get("type") == "doi":
            doi = ident.get("value", "")
            break

    # Abstract
    abstract_data = (
        fullrecord.get("abstracts", {})
        .get("abstract", {})
        .get("abstract_text", {})
        .get("p", "")
    )
    if isinstance(abstract_data, list):
        abstract = clean_wos_text(" ".join(str(p) for p in abstract_data))
    else:
        abstract = clean_wos_text(abstract_data or "")

    # Keywords
    keywords = []
    kw_data = fullrecord.get("keywords", {}).get("keyword")
    if kw_data is not None:
        keywords = [
            k if isinstance(k, str) else k.get("content", "") for k in _as_list(kw_data)
        ]

    # Affiliations: full address strings, best effort (not all records carry them)
    affiliations = []
    for addr in _as_list(fullrecord.get("addresses", {}).get("address_name")):
        spec = addr.get("address_spec", {}) if isinstance(addr, dict) else {}
        full = spec.get("full_address")
        if full:
            affiliations.append(clean_wos_text(full))

    return Paper(
        uid=uid,
        title=title,
        authors=authors,
        author_count=len(authors),
        journal=journal,
        year=year,
        volume=volume,
        issue=issue,
        pages=pages,
        doi=doi,
        abstract=abstract,
        keywords=keywords,
        affiliations=affiliations,
    )


def analyse_papers(papers: list[Paper]) -> dict:
    """
    Analyse papers for geographic and thematic patterns.

    Args:
        papers: List of Paper objects

    Returns:
        Analysis results dictionary
    """
    # City/location patterns (shared with detect_cities / CSV region column)
    locations = CITIES_BY_REGION

    # Application types
    applications = {
        "Surface Energy Balance": [
            "energy balance",
            "energy flux",
            "sensible heat",
            "latent heat",
            "heat flux",
            "SEB",
        ],
        "Urban Heat Island": [
            "urban heat island",
            "UHI",
            "heat island",
            "thermal environment",
            "overheating",
        ],
        "CO2/Carbon Flux": [
            "CO2",
            "carbon",
            "carbon dioxide",
            "carbon flux",
            "carbon neutral",
            "sequestration",
        ],
        "Building Energy": [
            "building energy",
            "air conditioning",
            "heat pump",
            "HVAC",
            "energy consumption",
            "cooling load",
        ],
        "Urban Vegetation": [
            "vegetation",
            "tree",
            "green",
            "LAI",
            "leaf area",
            "photosynthesis",
        ],
        "Water Balance": [
            "water balance",
            "runoff",
            "hydrological",
            "precipitation",
            "drainage",
            "flood",
        ],
        "Climate Scenarios": [
            "climate change",
            "climate scenario",
            "future climate",
            "RCP",
            "SSP",
            "2050",
            "2080",
        ],
        "Model Development": [
            "model development",
            "parameterization",
            "parameterisation",
            "scheme",
            "module",
            "coupled",
        ],
        "Model Evaluation": [
            "evaluation",
            "validation",
            "comparison",
            "intercomparison",
            "performance",
        ],
    }

    # Count locations
    location_counts = defaultdict(lambda: defaultdict(list))
    for paper in papers:
        text = f"{paper.title} {paper.abstract}".lower()
        for region, cities in locations.items():
            for city in cities:
                if re.search(rf"\b{re.escape(city.lower())}\b", text):
                    location_counts[region][city].append(paper.uid)

    # Count applications
    application_counts = defaultdict(list)
    for paper in papers:
        text = f"{paper.title} {paper.abstract}".lower()
        for app_type, keywords in applications.items():
            for kw in keywords:
                if kw.lower() in text:
                    application_counts[app_type].append(paper.uid)
                    break

    # Year distribution
    year_counts = Counter(p.year for p in papers if p.year)

    return {
        "total_papers": len(papers),
        "papers_with_abstracts": sum(1 for p in papers if p.abstract),
        "year_range": (min(year_counts.keys()), max(year_counts.keys())),
        "year_distribution": dict(sorted(year_counts.items())),
        "locations_by_region": {
            region: {city: len(uids) for city, uids in cities.items()}
            for region, cities in location_counts.items()
        },
        "applications": {
            app: len(uids)
            for app, uids in sorted(
                application_counts.items(), key=lambda x: -len(x[1])
            )
        },
        "unique_locations": sum(len(cities) for cities in location_counts.values()),
        # UID-level maps for the RST generator (per-city cite keys + dominant
        # application tags). Not rendered directly; consumed by generate_rst.
        "location_uids": {
            region: {city: list(uids) for city, uids in cities.items()}
            for region, cities in location_counts.items()
        },
        "application_uids": {
            app: list(uids) for app, uids in application_counts.items()
        },
    }


def load_existing_bib_keys(bib_paths: list[Path]) -> set[str]:
    """Return the set of cite keys already defined in other .bib files.

    Used to keep refs-wos.bib keys disjoint from the rest of the bibliography
    so sphinxcontrib-bibtex never sees a duplicate key across files.
    """
    keys: set[str] = set()
    for path in bib_paths:
        try:
            text = Path(path).read_text(encoding="utf-8", errors="ignore")
        except OSError:
            continue
        keys.update(re.findall(r"@\w+\s*\{\s*([^,\s]+)", text))
    return keys


def assign_cite_keys(
    papers: list[Paper], reserved_keys: set[str] | None = None
) -> None:
    """Assign a deterministic, unique BibTeX cite key to each paper in place.

    Key is ``<FirstAuthorSurname><Year>`` (ASCII only), with a lowercase-letter
    suffix on collision (``Smith2020``, ``Smith2020a``, ``Smith2020b``). Keys in
    ``reserved_keys`` (drawn from the other project .bib files) are treated as
    taken, so the generated keys never clash with an existing entry.
    """
    taken: set[str] = set(reserved_keys or set())
    for paper in papers:
        first_author = paper.authors[0].split(",")[0] if paper.authors else "Unknown"
        base = re.sub(r"[^A-Za-z0-9]", "", f"{first_author}{paper.year}") or "Unknown"
        candidate = base
        suffix = 0
        while candidate in taken:
            candidate = f"{base}{chr(ord('a') + suffix)}"
            suffix += 1
        taken.add(candidate)
        paper.cite_key = candidate


def _normalise_title(title: str) -> str:
    """Lowercase, alphanumeric-only form of a title for fuzzy comparison."""
    return re.sub(r"[^a-z0-9]+", " ", (title or "").lower()).strip()


def _title_similarity(a: str, b: str) -> float:
    """Similarity ratio in [0, 1] between two titles, markup-insensitive."""
    return SequenceMatcher(None, _normalise_title(a), _normalise_title(b)).ratio()


def enrich_with_crossref(
    papers: list[Paper],
    mailto: str = "ting.sun@ucl.ac.uk",
    rate_limit: float = 0.2,
    similarity_threshold: float = 0.9,
) -> dict:
    """Fill missing DOIs and canonicalise journal names via Crossref, in place.

    Always title-cases the raw WoS journal name as a deterministic baseline,
    then, where Crossref returns a confident title match, overrides the journal
    with Crossref's canonical container title and fills a missing DOI. Guarded
    by title similarity and a +/-1 year tolerance to avoid mismatches. Resilient:
    any network or parse failure leaves the (title-cased) WoS values intact.

    Returns a small summary dict for logging.
    """
    session = requests.Session()
    headers = {"User-Agent": f"SUEWS-wos-papers (mailto:{mailto})"}
    summary = {"doi_filled": 0, "journal_canonicalised": 0, "errors": 0}

    for paper in papers:
        paper.journal = titlecase_journal(paper.journal)
        match = None
        try:
            if paper.doi:
                resp = session.get(
                    f"https://api.crossref.org/works/{quote(paper.doi, safe='')}",
                    headers=headers,
                    timeout=30,
                )
                if resp.status_code == 200:
                    cand = resp.json().get("message", {})
                    titles = cand.get("title") or [""]
                    if (
                        _title_similarity(paper.title, titles[0])
                        >= similarity_threshold
                    ):
                        match = cand
            else:
                resp = session.get(
                    "https://api.crossref.org/works",
                    headers=headers,
                    params={"query.bibliographic": paper.title, "rows": 5},
                    timeout=30,
                )
                if resp.status_code == 200:
                    best, best_sim = None, 0.0
                    for item in resp.json().get("message", {}).get("items", []):
                        titles = item.get("title") or [""]
                        if not titles[0]:
                            continue
                        sim = _title_similarity(paper.title, titles[0])
                        parts = item.get("issued", {}).get("date-parts", [[None]])
                        cr_year = (parts[0] or [None])[0]
                        year_ok = (
                            not paper.year
                            or not cr_year
                            or abs(int(cr_year) - int(paper.year)) <= 1
                        )
                        if sim > best_sim and year_ok:
                            best, best_sim = item, sim
                    if best and best_sim >= similarity_threshold:
                        match = best
                        new_doi = match.get("DOI", "")
                        if new_doi:
                            paper.doi = new_doi
                            summary["doi_filled"] += 1
            if match:
                container = match.get("container-title") or []
                if container and container[0].strip():
                    canonical = container[0].strip()
                    if canonical != paper.journal:
                        summary["journal_canonicalised"] += 1
                    paper.journal = canonical
        except (requests.RequestException, ValueError, KeyError):
            summary["errors"] += 1
        time.sleep(rate_limit)

    return summary


def _bib_escape(text: str) -> str:
    """Escape BibTeX-structural characters in a braced field value."""
    return text.replace("&", r"\&").replace("%", r"\%").replace("#", r"\#")


def generate_bibtex(papers: list[Paper]) -> str:
    """
    Generate BibTeX entries for papers.

    Uses each paper's pre-assigned ``cite_key`` (see ``assign_cite_keys``) so the
    keys match those cited on the documentation page. Abstracts are intentionally
    omitted -- they are not rendered by the bibliography style and live in the
    JSON export instead.

    Args:
        papers: List of Paper objects (with cite_key assigned)

    Returns:
        BibTeX formatted string
    """
    entries = []

    for paper in papers:
        if not paper.title:
            continue

        entry_lines = [f"@article{{{paper.cite_key},"]
        entry_lines.append(f"  title = {{{_bib_escape(paper.title)}}},")

        if paper.authors:
            authors_str = " and ".join(paper.authors)
            entry_lines.append(f"  author = {{{authors_str}}},")

        entry_lines.append(f"  journal = {{{_bib_escape(paper.journal)}}},")
        entry_lines.append(f"  year = {{{paper.year}}},")

        if paper.volume:
            entry_lines.append(f"  volume = {{{paper.volume}}},")
        if paper.issue:
            entry_lines.append(f"  number = {{{paper.issue}}},")
        if paper.pages:
            entry_lines.append(f"  pages = {{{paper.pages}}},")
        if paper.doi:
            entry_lines.append(f"  doi = {{{paper.doi}}},")

        entry_lines.append("}")
        entries.append("\n".join(entry_lines))

    header = f"""% SUEWS-related publications from Web of Science
% Generated: {datetime.now().strftime("%Y-%m-%d %H:%M:%S")}
% Total papers: {len(papers)}
%
% This file is auto-generated by fetch_suews_papers.py
% Repository: https://github.com/UMEP-dev/SUEWS
"""

    return header + "\n\n" + "\n\n".join(entries)


def generate_markdown(papers: list[Paper], analysis: dict) -> str:
    """
    Generate Markdown summary of papers.

    Args:
        papers: List of Paper objects
        analysis: Analysis results

    Returns:
        Markdown formatted string
    """
    lines = [
        "# SUEWS Publications from Web of Science",
        "",
        f"**Generated**: {datetime.now().strftime('%Y-%m-%d')}",
        f"**Total papers**: {analysis['total_papers']}",
        f"**Year range**: {analysis['year_range'][0]} - {analysis['year_range'][1]}",
        f"**Unique locations**: {analysis['unique_locations']}",
        "",
        "## Summary Statistics",
        "",
        f"- Papers with abstracts: {analysis['papers_with_abstracts']}",
        "",
        "## Geographic Distribution",
        "",
    ]

    for region, cities in sorted(analysis["locations_by_region"].items()):
        if cities:
            city_list = ", ".join(
                f"{c} ({n})"
                for c, n in sorted(cities.items(), key=lambda x: -x[1])[:10]
            )
            lines.append(f"### {region}")
            lines.append(f"{city_list}")
            lines.append("")

    lines.extend([
        "## Application Areas",
        "",
    ])
    for app, count in analysis["applications"].items():
        pct = count / analysis["total_papers"] * 100
        lines.append(f"- **{app}**: {count} papers ({pct:.0f}%)")

    lines.extend([
        "",
        "## Publications by Year",
        "",
    ])
    for year, count in sorted(analysis["year_distribution"].items()):
        bar = "█" * count
        lines.append(f"- {year}: {bar} ({count})")

    lines.extend([
        "",
        "## All Papers",
        "",
    ])

    for i, paper in enumerate(sorted(papers, key=lambda p: -p.year), 1):
        authors = ", ".join(paper.authors[:3])
        if paper.author_count > 3:
            authors += f" et al."

        lines.append(f"### {i}. {paper.title}")
        lines.append(f"**Authors**: {authors}")
        lines.append(f"**Journal**: {paper.journal} ({paper.year})")
        if paper.doi:
            lines.append(f"**DOI**: [{paper.doi}](https://doi.org/{paper.doi})")
        if paper.abstract:
            abstract = paper.abstract[:500]
            if len(paper.abstract) > 500:
                abstract += "..."
            lines.append(f"\n> {abstract}")
        lines.append("")

    return "\n".join(lines)


def detect_cities(text: str) -> tuple[list[str], str]:
    """
    Detect cities mentioned in text and determine region.

    Args:
        text: Text to search (title + abstract)

    Returns:
        Tuple of (list of cities found, primary region)
    """
    cities_found = []
    regions_found = set()

    for region, cities in CITIES_BY_REGION.items():
        for city in cities:
            if re.search(rf"\b{re.escape(city)}\b", text, re.IGNORECASE):
                cities_found.append(city)
                regions_found.add(region)

    # Determine primary region
    if len(regions_found) == 1:
        primary_region = list(regions_found)[0]
    elif len(regions_found) > 1:
        primary_region = "Multiple"
    else:
        primary_region = "Global/Method"

    return cities_found, primary_region


def generate_csv(papers: list[Paper]) -> str:
    """
    Generate CSV with enriched paper data for interactive filtering.

    Args:
        papers: List of Paper objects

    Returns:
        CSV formatted string
    """
    output = StringIO()
    writer = csv.writer(output)

    # Header
    writer.writerow([
        "Citation Key",
        "Year",
        "First Author",
        "Title",
        "Journal",
        "Cities",
        "Region",
        "Type",
        "DOI",
    ])

    for paper in papers:
        first_author = paper.authors[0].split(",")[0] if paper.authors else "Unknown"

        # Detect cities and region
        text = f"{paper.title} {paper.abstract}"
        cities, region = detect_cities(text)

        # Determine paper type
        paper_type = "City Study" if cities else "Methodology"

        # Write row (cite key matches the BibTeX / page citations)
        writer.writerow([
            paper.cite_key,
            paper.year,
            first_author,
            paper.title[:100] + "..." if len(paper.title) > 100 else paper.title,
            paper.journal,
            "; ".join(cities) if cities else "-",
            region,
            paper_type,
            f"https://doi.org/{paper.doi}" if paper.doi else "-",
        ])

    return output.getvalue()


def _rst_section(title: str, char: str) -> list[str]:
    """Return an RST section heading (title + matching underline) as lines."""
    return [title, char * len(title), ""]


def generate_rst(papers: list[Paper], analysis: dict, data_date: str) -> str:
    """Generate the data section of the Global Applications page as RST.

    Emits summary statistics, a per-region city table (with auto-derived
    dominant application tags and ``:cite:`` keys), and an application-area
    table. Output is ASCII-only and reproducible from the analysis, so the page
    never drifts from the committed data. Included by ``global_applications.rst``.
    """
    uid_to_key = {p.uid: p.cite_key for p in papers}
    app_uid_sets = {
        app: set(uids) for app, uids in analysis.get("application_uids", {}).items()
    }
    location_uids = analysis.get("location_uids", {})
    total = analysis["total_papers"]
    year_range = analysis["year_range"]

    lines = [
        ".. Auto-generated by scripts/wos-papers/fetch_suews_papers.py"
        " -- do not edit by hand.",
        "",
        f"*Data as of {data_date}. Source:* "
        "`Web of Science <https://www.webofscience.com/>`_",
        "",
    ]

    # Summary statistics
    lines += _rst_section("Summary Statistics", "=")
    lines += [
        ".. list-table::",
        "   :widths: 30 70",
        "   :header-rows: 0",
        "",
        "   * - **Total Publications**",
        f"     - {total} papers indexed in Web of Science",
        "   * - **Year Range**",
        f"     - {year_range[0]} - {year_range[1]}",
        "   * - **Unique Locations**",
        f"     - {analysis['unique_locations']} cities/regions documented",
        "",
    ]

    # Geographic distribution
    lines += _rst_section("Geographic Distribution", "=")
    region_totals = {
        region: sum(cities.values())
        for region, cities in analysis["locations_by_region"].items()
    }
    for region in sorted(region_totals, key=lambda r: (-region_totals[r], r)):
        cities = analysis["locations_by_region"][region]
        if not cities:
            continue
        lines += _rst_section(region, "-")
        lines += [
            ".. list-table::",
            "   :widths: 25 10 40 25",
            "   :header-rows: 1",
            "",
            "   * - City",
            "     - Papers",
            "     - Key Applications",
            "     - References",
        ]
        for city in sorted(cities, key=lambda c: (-cities[c], c)):
            uids = location_uids.get(region, {}).get(city, [])
            app_hits = sorted(
                (
                    (app, sum(1 for u in uids if u in members))
                    for app, members in app_uid_sets.items()
                ),
                key=lambda x: (-x[1], x[0]),
            )
            # the three most common applications among this city's papers
            top_apps = "; ".join([app for app, n in app_hits if n > 0][:3]) or "-"
            keys = sorted({uid_to_key[u] for u in uids if u in uid_to_key})
            cites = f":cite:`{','.join(keys)}`" if keys else "-"
            lines += [
                f"   * - {city}",
                f"     - {len(uids)}",
                f"     - {top_apps}",
                f"     - {cites}",
            ]
        lines.append("")

    # Application areas
    lines += _rst_section("Application Areas", "=")
    lines += [
        "SUEWS publications span the following research themes; a single paper",
        "may contribute to several.",
        "",
        ".. list-table::",
        "   :widths: 50 20 30",
        "   :header-rows: 1",
        "",
        "   * - Application Type",
        "     - Papers",
        "     - Share",
    ]
    for app, count in analysis["applications"].items():
        pct = round(count / total * 100) if total else 0
        lines += [
            f"   * - **{app}**",
            f"     - {count}",
            f"     - {pct}%",
        ]
    lines.append("")

    return "\n".join(lines) + "\n"


# Default documentation locations, relative to the repository root.
# scripts/wos-papers/fetch_suews_papers.py -> repo root is parents[2].
_REPO_ROOT = Path(__file__).resolve().parents[2]
_DEFAULT_DOCS_DIR = _REPO_ROOT / "docs" / "source"
# Other project bibliographies whose keys refs-wos.bib must not clash with.
_OTHER_BIB_NAMES = ("refs-SUEWS.bib", "refs-others.bib", "refs-community.bib")
_DEFAULT_FORMATS = ["json", "bibtex", "csv", "rst"]


def _resolve_paths(args) -> dict:
    """Map each output artefact to its target path.

    By default artefacts go to their canonical documentation locations under
    ``--docs-dir``; ``--output-dir`` overrides this and writes everything flat
    into one directory (handy for ad-hoc runs and testing).
    """
    if args.output_dir is not None:
        base = args.output_dir
        return {
            "base": base,
            "wos_assets": base,
            "refs_dir": base,
            "json": base / "suews_wos_papers.json",
            "csv": base / "suews_wos_papers.csv",
            "bibtex": base / "refs-wos.bib",
            "markdown": base / "suews_wos_papers.md",
            "rst": base / "global_applications_generated.rst",
        }
    wos_assets = args.docs_dir / "assets" / "wos-papers"
    refs_dir = args.docs_dir / "assets" / "refs"
    return {
        "base": args.docs_dir,
        "wos_assets": wos_assets,
        "refs_dir": refs_dir,
        "json": wos_assets / "suews_wos_papers.json",
        "csv": wos_assets / "suews_wos_papers.csv",
        "bibtex": refs_dir / "refs-wos.bib",
        "markdown": wos_assets / "suews_wos_papers.md",
        "rst": wos_assets / "global_applications_generated.rst",
    }


def main():
    parser = argparse.ArgumentParser(
        description="Fetch SUEWS papers from Web of Science",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
    # Refresh all documentation artefacts in place (from repo root)
    python fetch_suews_papers.py

    # Write everything flat into a scratch directory
    python fetch_suews_papers.py --output-dir ./output

Environment:
    WOS_EXPANDED_API_KEY - Web of Science Expanded API key (required; the
        script falls back to WOS_API_KEY for backward compatibility, but the
        Expanded endpoint needs the Expanded key).
        """,
    )
    parser.add_argument(
        "--docs-dir",
        type=Path,
        default=_DEFAULT_DOCS_DIR,
        help="Documentation source root (default: <repo>/docs/source). Artefacts"
        " are written to their canonical locations beneath it.",
    )
    parser.add_argument(
        "--output-dir",
        "-o",
        type=Path,
        default=None,
        help="Write all artefacts flat into this directory instead of their"
        " canonical doc locations (for ad-hoc runs).",
    )
    parser.add_argument(
        "--formats",
        "-f",
        nargs="+",
        choices=["json", "bibtex", "markdown", "csv", "rst", "all"],
        default=_DEFAULT_FORMATS,
        help="Output formats (default: json bibtex csv rst)",
    )
    parser.add_argument(
        "--query",
        "-q",
        default='TS=SUEWS OR TS="Surface Urban Energy and Water Balance Scheme" OR TI=SUEWS',
        help="WoS search query",
    )
    parser.add_argument(
        "--no-enrich",
        action="store_true",
        help="Skip the Crossref enrichment pass (DOIs/journal names). Journal"
        " names are still title-cased; missing DOIs stay blank.",
    )

    args = parser.parse_args()

    # API key: the Expanded endpoint needs the Expanded key.
    api_key = os.environ.get("WOS_EXPANDED_API_KEY") or os.environ.get("WOS_API_KEY")
    if not api_key:
        print("Error: WOS_EXPANDED_API_KEY environment variable not set")
        print("Set it with: export WOS_EXPANDED_API_KEY='your-key'")
        sys.exit(1)

    # Determine formats and resolve output paths
    formats = set(args.formats)
    if "all" in formats:
        formats = {"json", "bibtex", "markdown", "csv", "rst"}
    paths = _resolve_paths(args)
    paths["wos_assets"].mkdir(parents=True, exist_ok=True)
    paths["refs_dir"].mkdir(parents=True, exist_ok=True)

    print("=" * 60)
    print("SUEWS Papers - Web of Science Fetcher")
    print("=" * 60)
    print(f"Query: {args.query}")
    print(f"Output base: {paths['base']}")
    print()

    # Fetch papers
    client = WoSClient(api_key)

    def progress(current, total):
        print(f"\rFetching: {current}/{total} papers", end="", flush=True)

    print("Connecting to Web of Science API...")
    raw_records = client.fetch_all(args.query, progress_callback=progress)
    print()

    # Parse and sort deterministically (year, then title) so cite keys and
    # output ordering are reproducible across runs.
    print("Parsing records...")
    papers = [parse_record(r) for r in raw_records]
    papers.sort(key=lambda p: (p.year, p.title.lower()))

    # Crossref enrichment (DOIs + canonical journal names). Always at least
    # title-cases the raw WoS journal names.
    if args.no_enrich:
        for paper in papers:
            paper.journal = titlecase_journal(paper.journal)
    else:
        print("Enriching via Crossref (DOIs, journal names)...")
        enrich_summary = enrich_with_crossref(papers)
        print(
            f"  DOIs filled: {enrich_summary['doi_filled']}, "
            f"journals canonicalised: {enrich_summary['journal_canonicalised']}, "
            f"errors: {enrich_summary['errors']}"
        )

    # Assign cite keys, avoiding clashes with the other project bibliographies.
    reserved = load_existing_bib_keys([
        paths["refs_dir"] / name for name in _OTHER_BIB_NAMES
    ])
    assign_cite_keys(papers, reserved_keys=reserved)

    # Analyse
    print("Analysing papers...")
    analysis = analyse_papers(papers)

    fetched = datetime.now()
    data_date = f"{fetched.day} {fetched:%b %Y}"

    print()
    print(f"Total papers found: {analysis['total_papers']}")
    print(f"Year range: {analysis['year_range'][0]} - {analysis['year_range'][1]}")
    print(f"Unique locations: {analysis['unique_locations']}")
    print(f"Papers with DOI: {sum(1 for p in papers if p.doi)}/{len(papers)}")
    print()

    # Write outputs
    if "json" in formats:
        with open(paths["json"], "w") as f:
            json.dump(
                {
                    "metadata": {
                        "query": args.query,
                        "fetched": fetched.isoformat(),
                        "total": len(papers),
                    },
                    "analysis": analysis,
                    "papers": [asdict(p) for p in papers],
                },
                f,
                indent=2,
            )
        print(f"JSON: {paths['json']}")

    if "bibtex" in formats:
        with open(paths["bibtex"], "w") as f:
            f.write(generate_bibtex(papers))
        print(f"BibTeX: {paths['bibtex']}")

    if "markdown" in formats:
        with open(paths["markdown"], "w") as f:
            f.write(generate_markdown(papers, analysis))
        print(f"Markdown: {paths['markdown']}")

    if "csv" in formats:
        with open(paths["csv"], "w", newline="") as f:
            f.write(generate_csv(papers))
        print(f"CSV: {paths['csv']}")

    if "rst" in formats:
        with open(paths["rst"], "w") as f:
            f.write(generate_rst(papers, analysis, data_date))
        print(f"RST: {paths['rst']}")

    print()
    print("Done!")


if __name__ == "__main__":
    main()
