#!/usr/bin/env python3
"""
Fetch SUEWS-related papers from Web of Science API.

This script queries the Web of Science Expanded API to retrieve all papers
related to the SUEWS (Surface Urban Energy and Water Balance Scheme) model,
extracts metadata and abstracts, and outputs results in multiple formats.

Requirements:
    - WOS_API_KEY environment variable (UCL institutional subscription)
    - requests library

Usage:
    # Set API key first
    export WOS_API_KEY="your-api-key"

    # Run with default options
    python fetch_suews_papers.py

    # Specify output directory
    python fetch_suews_papers.py --output-dir ./output

    # Generate specific formats only
    python fetch_suews_papers.py --formats json markdown

Author: Ting Sun (ting.sun@ucl.ac.uk)
Created: 2025-11-26
Repository: https://github.com/UMEP-dev/SUEWS
"""

import argparse
import json
import os
import re
import sys
import time
from collections import Counter, defaultdict
from dataclasses import asdict, dataclass, field
from datetime import datetime
from pathlib import Path
from typing import Optional

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

    # Title
    titles = summary.get("titles", {}).get("title", [])
    title = next((t.get("content", "") for t in titles if t.get("type") == "item"), "")
    # Clean HTML tags from title
    title = re.sub(r"<[^>]+>", "", title)

    # Journal
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

    # Authors
    names_data = summary.get("names", {}).get("name", [])
    authors = []
    for n in names_data:
        if n.get("role") == "author":
            name = n.get("full_name", n.get("display_name", ""))
            if name:
                authors.append(name)

    # DOI
    doi = ""
    identifiers = (
        static_data.get("dynamic_data", {})
        .get("cluster_related", {})
        .get("identifiers", {})
        .get("identifier", [])
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
        abstract = " ".join(abstract_data)
    else:
        abstract = abstract_data or ""

    # Keywords
    keywords = []
    kw_data = fullrecord.get("keywords", {}).get("keyword", [])
    if isinstance(kw_data, list):
        keywords = [k if isinstance(k, str) else k.get("content", "") for k in kw_data]

    return Paper(
        uid=uid,
        title=title,
        authors=authors[:10],  # First 10 authors
        author_count=len(authors),
        journal=journal,
        year=year,
        volume=volume,
        issue=issue,
        pages=pages,
        doi=doi,
        abstract=abstract,
        keywords=keywords,
    )


def analyse_papers(papers: list[Paper]) -> dict:
    """
    Analyse papers for geographic and thematic patterns.

    Args:
        papers: List of Paper objects

    Returns:
        Analysis results dictionary
    """
    # City/location patterns
    locations = {
        "Asia": [
            "Beijing",
            "Shanghai",
            "Singapore",
            "Hong Kong",
            "Mumbai",
            "Delhi",
            "Tokyo",
            "Seoul",
            "Taipei",
            "Bangkok",
            "Guangzhou",
            "Nanjing",
            "Xiong'an",
            "Xiongan",
            "Baoding",
            "Hangzhou",
            "Wuhan",
        ],
        "Europe": [
            "London",
            "Helsinki",
            "Swindon",
            "Dublin",
            "Berlin",
            "Hamburg",
            "Zurich",
            "Vienna",
            "Amsterdam",
            "Madrid",
            "Barcelona",
            "Rome",
            "Stockholm",
            "Copenhagen",
            "Bordeaux",
            "Heraklion",
            "Freiburg",
            "Porto",
            "Lisbon",
            "Marseille",
            "Rotterdam",
        ],
        "North America": [
            "Vancouver",
            "Los Angeles",
            "Montreal",
            "Phoenix",
            "New York",
            "Toronto",
            "Baltimore",
            "Chicago",
            "Boston",
        ],
        "Oceania": ["Melbourne", "Sydney", "Brisbane", "Auckland", "Perth"],
        "Other": ["Cairo", "Johannesburg", "Lagos", "São Paulo"],
    }

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
                if re.search(rf"\b{city.lower()}\b", text):
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
    }


def generate_bibtex(papers: list[Paper]) -> str:
    """
    Generate BibTeX entries for papers.

    Args:
        papers: List of Paper objects

    Returns:
        BibTeX formatted string
    """
    entries = []

    for paper in papers:
        if not paper.doi and not paper.title:
            continue

        # Generate citation key
        first_author = paper.authors[0].split(",")[0] if paper.authors else "Unknown"
        key = f"{first_author}{paper.year}"
        key = re.sub(r"[^a-zA-Z0-9]", "", key)

        # Build entry
        entry_lines = [f"@article{{{key},"]
        entry_lines.append(f"  title = {{{paper.title}}},")

        if paper.authors:
            authors_str = " and ".join(paper.authors[:10])
            if paper.author_count > 10:
                authors_str += " and others"
            entry_lines.append(f"  author = {{{authors_str}}},")

        entry_lines.append(f"  journal = {{{paper.journal}}},")
        entry_lines.append(f"  year = {{{paper.year}}},")

        if paper.volume:
            entry_lines.append(f"  volume = {{{paper.volume}}},")
        if paper.issue:
            entry_lines.append(f"  number = {{{paper.issue}}},")
        if paper.pages:
            entry_lines.append(f"  pages = {{{paper.pages}}},")
        if paper.doi:
            entry_lines.append(f"  doi = {{{paper.doi}}},")

        if paper.abstract:
            # Escape special characters in abstract
            abstract = paper.abstract.replace("{", "\\{").replace("}", "\\}")
            abstract = abstract.replace("%", "\\%").replace("&", "\\&")
            entry_lines.append(f"  abstract = {{{abstract}}},")

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


def main():
    parser = argparse.ArgumentParser(
        description="Fetch SUEWS papers from Web of Science",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
    python fetch_suews_papers.py
    python fetch_suews_papers.py --output-dir ./output
    python fetch_suews_papers.py --formats json bibtex

Environment:
    WOS_API_KEY - Web of Science API key (required)
        """,
    )
    parser.add_argument(
        "--output-dir",
        "-o",
        type=Path,
        default=Path("."),
        help="Output directory (default: current directory)",
    )
    parser.add_argument(
        "--formats",
        "-f",
        nargs="+",
        choices=["json", "bibtex", "markdown", "all"],
        default=["json"],
        help="Output formats (default: json)",
    )
    parser.add_argument(
        "--query",
        "-q",
        default='TS=SUEWS OR TS="Surface Urban Energy and Water Balance Scheme" OR TI=SUEWS',
        help="WoS search query",
    )

    args = parser.parse_args()

    # Check API key
    api_key = os.environ.get("WOS_API_KEY")
    if not api_key:
        print("Error: WOS_API_KEY environment variable not set")
        print("Set it with: export WOS_API_KEY='your-key'")
        sys.exit(1)

    # Create output directory
    args.output_dir.mkdir(parents=True, exist_ok=True)

    # Determine formats
    formats = set(args.formats)
    if "all" in formats:
        formats = {"json", "bibtex", "markdown"}

    print("=" * 60)
    print("SUEWS Papers - Web of Science Fetcher")
    print("=" * 60)
    print(f"Query: {args.query}")
    print(f"Output: {args.output_dir}")
    print()

    # Initialize client
    client = WoSClient(api_key)

    # Progress callback
    def progress(current, total):
        print(f"\rFetching: {current}/{total} papers", end="", flush=True)

    # Fetch papers
    print("Connecting to Web of Science API...")
    raw_records = client.fetch_all(args.query, progress_callback=progress)
    print()

    # Parse records
    print("Parsing records...")
    papers = [parse_record(r) for r in raw_records]

    # Analyse
    print("Analysing papers...")
    analysis = analyse_papers(papers)

    # Output results
    print()
    print(f"Total papers found: {analysis['total_papers']}")
    print(f"Year range: {analysis['year_range'][0]} - {analysis['year_range'][1]}")
    print(f"Unique locations: {analysis['unique_locations']}")
    print()

    # Write outputs
    if "json" in formats:
        json_path = args.output_dir / "suews_wos_papers.json"
        with open(json_path, "w") as f:
            json.dump(
                {
                    "metadata": {
                        "query": args.query,
                        "fetched": datetime.now().isoformat(),
                        "total": len(papers),
                    },
                    "analysis": analysis,
                    "papers": [asdict(p) for p in papers],
                },
                f,
                indent=2,
            )
        print(f"JSON: {json_path}")

    if "bibtex" in formats:
        bib_path = args.output_dir / "suews_wos_papers.bib"
        with open(bib_path, "w") as f:
            f.write(generate_bibtex(papers))
        print(f"BibTeX: {bib_path}")

    if "markdown" in formats:
        md_path = args.output_dir / "suews_wos_papers.md"
        with open(md_path, "w") as f:
            f.write(generate_markdown(papers, analysis))
        print(f"Markdown: {md_path}")

    print()
    print("Done!")


if __name__ == "__main__":
    main()
