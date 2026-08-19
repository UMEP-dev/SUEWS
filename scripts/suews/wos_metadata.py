#!/usr/bin/env python3
"""Fetch a minimal, provenance-rich list of SUEWS publications from WoS.

The pipeline deliberately persists only four bibliographic fields: Web of
Science UID, DOI, title, and publication year. Full records are processed in
memory but abstracts, affiliations, keywords, addresses, and citation metrics
are never written to the output manifest.
"""

from __future__ import annotations

import argparse
from collections.abc import Callable, Iterable, Mapping
from dataclasses import asdict, dataclass
from datetime import UTC, datetime
import hashlib
import html
import json
import os
from pathlib import Path
import platform
import re
import sys
import time
from typing import Any
from urllib.error import HTTPError, URLError
from urllib.parse import urlencode
from urllib.request import Request, urlopen

PIPELINE_VERSION = "1.0.0"
MANIFEST_SCHEMA_VERSION = 1
WOS_API_NAME = "Web of Science API Expanded"
WOS_API_VERSION = "expanded-unversioned-endpoint"
WOS_API_ENDPOINT = "https://wos-api.clarivate.com/api/wos"
DEFAULT_DATABASE_ID = "WOS"
DEFAULT_QUERY = (
    'TS=SUEWS OR TS="Surface Urban Energy and Water Balance Scheme" OR TI=SUEWS'
)
DEFAULT_PAGE_SIZE = 50
RETRIABLE_HTTP_STATUS = frozenset({429, 500, 502, 503, 504})

PROJECT_ROOT = Path(__file__).resolve().parents[2]
DEFAULT_CANONICAL_BIBS = (
    PROJECT_ROOT / "docs" / "source" / "assets" / "refs" / "refs-SUEWS.bib",
    PROJECT_ROOT / "docs" / "source" / "assets" / "refs" / "refs-community.bib",
)
DEFAULT_TOPIC_RULE = PROJECT_ROOT / ".claude" / "rules" / "docs" / "bib-topic-tags.md"

FIELD_PROVENANCE = {
    "doi": "REC.dynamic_data.cluster_related.identifiers.identifier[type=doi].value",
    "title": "REC.static_data.summary.titles.title[type=item].content",
    "uid": "REC.UID",
    "year": "REC.static_data.summary.pub_info.pubyear",
}
EXCLUDED_FIELDS = (
    "abstract",
    "affiliations",
    "addresses",
    "authors",
    "citation_metrics",
    "funding",
    "keywords",
)

ENTRY_START = re.compile(
    r"^@(?P<entry_type>[A-Za-z]+)\{(?P<key>[^,\s]+)\s*,", re.MULTILINE
)
TOPIC_LINE = re.compile(r"^- `(?P<slug>[a-z][a-z0-9-]*)`\s+", re.MULTILINE)


class PipelineError(RuntimeError):
    """Base exception for a failed metadata pipeline contract."""


class FetchError(PipelineError):
    """An API page could not be retrieved after bounded retries."""

    def __init__(self, message: str, events: list[dict[str, Any]]):
        super().__init__(message)
        self.events = events


class RecordError(PipelineError):
    """A WoS record lacks one of the four permitted required fields."""


@dataclass(frozen=True)
class PublicationRecord:
    """The complete public record contract for one candidate publication."""

    uid: str
    doi: str
    title: str
    year: int


@dataclass(frozen=True)
class FetchResult:
    """Raw records plus API evidence needed to build provenance."""

    raw_records: tuple[dict[str, Any], ...]
    records_found: int
    query_id: str | int | None
    pages_fetched: int
    errors: tuple[dict[str, Any], ...]


@dataclass(frozen=True)
class CanonicalContext:
    """Canonical bibliography and controlled-vocabulary evidence."""

    dois: frozenset[str]
    bibliography: tuple[dict[str, Any], ...]
    topic_vocabulary: tuple[str, ...]
    topic_vocabulary_source: dict[str, Any]


def _utc_now() -> str:
    """Return an explicit UTC timestamp with a stable ``Z`` suffix."""
    return datetime.now(UTC).isoformat().replace("+00:00", "Z")


def _sha256(path: Path) -> str:
    """Return the SHA-256 digest of a local provenance source."""
    return hashlib.sha256(path.read_bytes()).hexdigest()


def _relative_path(path: Path) -> str:
    """Render a repository-relative path without leaking external directories."""
    try:
        return path.resolve().relative_to(PROJECT_ROOT.resolve()).as_posix()
    except ValueError:
        return f"external/{path.name}"


def clean_doi(value: str) -> str:
    """Return a case-insensitive DOI identifier without URL prefixes."""
    doi = (value or "").strip()
    doi = re.sub(r"^https?://(?:dx\.)?doi\.org/", "", doi, flags=re.IGNORECASE)
    doi = re.sub(r"^doi:\s*", "", doi, flags=re.IGNORECASE)
    return doi.strip().lower()


def _clean_text(value: str) -> str:
    """Remove record markup and collapse whitespace."""
    text = re.sub(r"<[^>]+>", "", value or "")
    return re.sub(r"\s+", " ", html.unescape(text)).strip()


def _as_list(value: Any) -> list[Any]:
    """Treat WoS single-object and list-shaped collections uniformly."""
    if value is None:
        return []
    return value if isinstance(value, list) else [value]


def parse_record(raw_record: Mapping[str, Any]) -> PublicationRecord:
    """Reduce one WoS full record to the four-field public contract."""
    uid = str(raw_record.get("UID", "")).strip()
    static_data = raw_record.get("static_data", {})
    summary = static_data.get("summary", {}) if isinstance(static_data, Mapping) else {}

    title_items = _as_list(summary.get("titles", {}).get("title"))
    title = _clean_text(
        next(
            (
                str(item.get("content", ""))
                for item in title_items
                if isinstance(item, Mapping) and item.get("type") == "item"
            ),
            "",
        )
    )

    pub_info = summary.get("pub_info", {})
    try:
        year = int(pub_info.get("pubyear", 0))
    except (TypeError, ValueError) as exc:
        raise RecordError("publication year is not an integer") from exc

    dynamic_data = raw_record.get("dynamic_data", {})
    identifiers = _as_list(
        dynamic_data.get("cluster_related", {}).get("identifiers", {}).get("identifier")
        if isinstance(dynamic_data, Mapping)
        else None
    )
    doi = clean_doi(
        next(
            (
                str(identifier.get("value", ""))
                for identifier in identifiers
                if isinstance(identifier, Mapping)
                and str(identifier.get("type", "")).lower() == "doi"
            ),
            "",
        )
    )

    missing = [
        field
        for field, value in (
            ("uid", uid),
            ("doi", doi),
            ("title", title),
            ("year", year),
        )
        if not value
    ]
    if missing:
        raise RecordError(f"missing required field(s): {', '.join(missing)}")
    return PublicationRecord(uid=uid, doi=doi, title=title, year=year)


def _extract_braced_field(entry: str, name: str) -> str | None:
    """Extract a possibly nested braced BibTeX field."""
    match = re.search(rf"(?:^|\n)\s*{name}\s*=\s*\{{", entry, re.IGNORECASE)
    if match is None:
        quoted = re.search(
            rf'(?:^|\n)\s*{name}\s*=\s*"(?P<value>[^"]*)"',
            entry,
            re.IGNORECASE,
        )
        return quoted.group("value") if quoted else None

    opening = match.end() - 1
    depth = 1
    index = opening + 1
    while index < len(entry) and depth:
        if entry[index] == "{":
            depth += 1
        elif entry[index] == "}":
            depth -= 1
        index += 1
    if depth:
        raise PipelineError(f"unterminated {name} field in bibliography entry")
    return entry[opening + 1 : index - 1]


def _bibliography_entries(text: str) -> list[str]:
    """Split BibTeX text into entries without copying a second parser taxonomy."""
    starts = list(ENTRY_START.finditer(text))
    return [
        text[
            match.start() : starts[index + 1].start()
            if index + 1 < len(starts)
            else len(text)
        ]
        for index, match in enumerate(starts)
    ]


def load_topic_vocabulary(path: Path) -> tuple[str, ...]:
    """Read the controlled topic slugs from the repository rule itself."""
    slugs = tuple(sorted(set(TOPIC_LINE.findall(path.read_text(encoding="utf-8")))))
    if not slugs:
        raise PipelineError(
            f"no controlled topic vocabulary found in {_relative_path(path)}"
        )
    return slugs


def load_canonical_context(
    bibliography_paths: Iterable[Path], topic_rule_path: Path
) -> CanonicalContext:
    """Load DOI deduplication and topic evidence from canonical sources."""
    topic_vocabulary = load_topic_vocabulary(topic_rule_path)
    allowed_topics = set(topic_vocabulary)
    dois: set[str] = set()
    bibliography_evidence: list[dict[str, Any]] = []

    for path in bibliography_paths:
        text = path.read_text(encoding="utf-8")
        file_dois: set[str] = set()
        topics_seen: set[str] = set()
        for entry in _bibliography_entries(text):
            doi = clean_doi(_extract_braced_field(entry, "doi") or "")
            if doi:
                file_dois.add(doi)
                dois.add(doi)
            raw_topics = _extract_braced_field(entry, "keywords") or ""
            topics = {topic.strip() for topic in raw_topics.split(",") if topic.strip()}
            unknown = topics - allowed_topics
            if unknown:
                raise PipelineError(
                    f"{_relative_path(path)} contains topics outside the controlled "
                    f"vocabulary: {', '.join(sorted(unknown))}"
                )
            topics_seen.update(topics)
        bibliography_evidence.append({
            "doi_count": len(file_dois),
            "path": _relative_path(path),
            "sha256": _sha256(path),
            "topics_seen": sorted(topics_seen),
        })

    return CanonicalContext(
        dois=frozenset(dois),
        bibliography=tuple(bibliography_evidence),
        topic_vocabulary=topic_vocabulary,
        topic_vocabulary_source={
            "path": _relative_path(topic_rule_path),
            "sha256": _sha256(topic_rule_path),
        },
    )


class WoSClient:
    """Small WoS Expanded API client with bounded retries and pagination."""

    def __init__(
        self,
        api_key: str,
        *,
        endpoint: str = WOS_API_ENDPOINT,
        page_size: int = DEFAULT_PAGE_SIZE,
        max_attempts: int = 3,
        backoff_seconds: float = 0.5,
        max_backoff_seconds: float = 4.0,
        request_interval_seconds: float = 0.5,
        timeout_seconds: float = 60.0,
        open_fn: Callable[..., Any] = urlopen,
        sleep_fn: Callable[[float], None] = time.sleep,
    ) -> None:
        if max_attempts < 1:
            raise ValueError("max_attempts must be at least 1")
        if min(backoff_seconds, max_backoff_seconds, request_interval_seconds) < 0:
            raise ValueError("retry and request intervals cannot be negative")
        self.api_key = api_key
        self.endpoint = endpoint
        self.page_size = page_size
        self.max_attempts = max_attempts
        self.backoff_seconds = backoff_seconds
        self.max_backoff_seconds = max_backoff_seconds
        self.request_interval_seconds = request_interval_seconds
        self.timeout_seconds = timeout_seconds
        self.open_fn = open_fn
        self.sleep_fn = sleep_fn
        self.request_count = 0
        self.errors: list[dict[str, Any]] = []

    def _request_page(
        self, query: str, database_id: str, first_record: int
    ) -> dict[str, Any]:
        params = urlencode({
            "count": self.page_size,
            "databaseId": database_id,
            "firstRecord": first_record,
            "usrQuery": query,
        })
        request = Request(
            f"{self.endpoint}?{params}",
            headers={"Accept": "application/json", "X-ApiKey": self.api_key},
        )

        page_events: list[dict[str, Any]] = []
        for attempt in range(1, self.max_attempts + 1):
            if self.request_count and self.request_interval_seconds:
                self.sleep_fn(self.request_interval_seconds)
            self.request_count += 1
            error_cause: Exception | None = None
            try:
                with self.open_fn(request, timeout=self.timeout_seconds) as response:
                    return json.loads(response.read().decode("utf-8"))
            except HTTPError as exc:
                error_cause = exc
                retriable = exc.code in RETRIABLE_HTTP_STATUS
                event = self._error_event(
                    first_record,
                    attempt,
                    "http_error",
                    str(exc.reason),
                    status_code=exc.code,
                    retriable=retriable,
                )
            except (URLError, TimeoutError) as exc:
                error_cause = exc
                event = self._error_event(
                    first_record,
                    attempt,
                    "network_error",
                    str(getattr(exc, "reason", exc)),
                    status_code=None,
                    retriable=True,
                )
            except (UnicodeDecodeError, json.JSONDecodeError) as exc:
                error_cause = exc
                event = self._error_event(
                    first_record,
                    attempt,
                    "invalid_json",
                    str(exc),
                    status_code=None,
                    retriable=False,
                )

            page_events.append(event)
            self.errors.append(event)
            if not event["will_retry"]:
                raise FetchError(
                    "Web of Science request failed", page_events
                ) from error_cause
            self.sleep_fn(float(event["backoff_seconds"]))

        raise FetchError("Web of Science request exhausted retries", page_events)

    def _error_event(
        self,
        first_record: int,
        attempt: int,
        kind: str,
        message: str,
        *,
        status_code: int | None,
        retriable: bool,
    ) -> dict[str, Any]:
        will_retry = retriable and attempt < self.max_attempts
        backoff = (
            min(self.max_backoff_seconds, self.backoff_seconds * (2 ** (attempt - 1)))
            if will_retry
            else 0.0
        )
        return {
            "attempt": attempt,
            "backoff_seconds": backoff,
            "first_record": first_record,
            "kind": kind,
            "message": message,
            "stage": "request",
            "status_code": status_code,
            "will_retry": will_retry,
        }

    def fetch_all(self, query: str, database_id: str) -> FetchResult:
        """Fetch every result page, including dict-shaped single records."""
        self.errors.clear()
        self.request_count = 0
        raw_records: list[dict[str, Any]] = []
        first_record = 1
        records_found: int | None = None
        query_id: str | int | None = None
        pages_fetched = 0

        while records_found is None or len(raw_records) < records_found:
            payload = self._request_page(query, database_id, first_record)
            pages_fetched += 1
            query_result = payload.get("QueryResult", {})
            if records_found is None:
                records_found = int(query_result.get("RecordsFound", 0))
                query_id = query_result.get("QueryID")

            rec_value = (
                payload
                .get("Data", {})
                .get("Records", {})
                .get("records", {})
                .get("REC", [])
            )
            page_records = [
                record for record in _as_list(rec_value) if isinstance(record, dict)
            ]
            if records_found and not page_records:
                event = {
                    "first_record": first_record,
                    "kind": "empty_page",
                    "message": "API returned no records before RecordsFound was reached",
                    "stage": "pagination",
                }
                self.errors.append(event)
                raise FetchError("Web of Science pagination stopped early", [event])
            raw_records.extend(page_records)
            first_record += len(page_records)

            if records_found == 0:
                break

        return FetchResult(
            raw_records=tuple(raw_records[: records_found or 0]),
            records_found=records_found or 0,
            query_id=query_id,
            pages_fetched=pages_fetched,
            errors=tuple(self.errors),
        )


def deduplicate_records(
    records: Iterable[PublicationRecord], canonical_dois: frozenset[str]
) -> tuple[list[PublicationRecord], dict[str, int]]:
    """Remove canonical and repeated DOI records deterministically."""
    candidates: dict[str, PublicationRecord] = {}
    canonical_matches = 0
    repeated_dois = 0
    ordered = sorted(
        records, key=lambda item: (item.doi, item.year, item.title, item.uid)
    )
    for record in ordered:
        if record.doi in canonical_dois:
            canonical_matches += 1
        elif record.doi in candidates:
            repeated_dois += 1
        else:
            candidates[record.doi] = record
    output = sorted(
        candidates.values(),
        key=lambda item: (item.year, item.title.casefold(), item.doi, item.uid),
    )
    return output, {
        "canonical_doi_matches_removed": canonical_matches,
        "duplicate_dois_removed": repeated_dois,
        "records_emitted": len(output),
    }


def software_provenance() -> dict[str, str]:
    """Describe the exact pipeline and runtime used for the output."""
    return {
        "http_client": "urllib.request (Python standard library)",
        "pipeline_sha256": _sha256(Path(__file__)),
        "pipeline_version": PIPELINE_VERSION,
        "python": platform.python_version(),
    }


def build_manifest(
    fetch_result: FetchResult,
    canonical_context: CanonicalContext,
    *,
    query: str,
    database_id: str,
    retrieval_started_at: str,
    retrieval_completed_at: str,
    software: Mapping[str, str] | None = None,
) -> dict[str, Any]:
    """Build a deterministic manifest without restricted WoS fields."""
    parsed: list[PublicationRecord] = []
    errors = list(fetch_result.errors)
    for index, raw_record in enumerate(fetch_result.raw_records):
        try:
            parsed.append(parse_record(raw_record))
        except RecordError as exc:
            errors.append({
                "kind": "invalid_record",
                "message": str(exc),
                "record_index": index,
                "stage": "parse",
                "uid": str(raw_record.get("UID", "")),
            })

    candidates, deduplication = deduplicate_records(parsed, canonical_context.dois)
    if errors:
        status = "complete_with_warnings"
    else:
        status = "complete"

    return {
        "manifest_schema_version": MANIFEST_SCHEMA_VERSION,
        "provenance": {
            "api": {
                "database_id": database_id,
                "endpoint": WOS_API_ENDPOINT,
                "name": WOS_API_NAME,
                "query_id": fetch_result.query_id,
                "version": WOS_API_VERSION,
            },
            "canonical_bibliography": {
                "doi_count": len(canonical_context.dois),
                "files": list(canonical_context.bibliography),
            },
            "deduplication": deduplication,
            "dependencies": dict(software or software_provenance()),
            "errors": errors,
            "field_provenance": FIELD_PROVENANCE,
            "licensing": {
                "excluded_fields": list(EXCLUDED_FIELDS),
                "profile": "minimal-bibliographic-metadata",
                "redistributed_fields": ["uid", "doi", "title", "year"],
                "terms_review": "Institutional WoS API terms still govern retrieval and reuse.",
            },
            "query": query,
            "retrieval": {
                "completed_at": retrieval_completed_at,
                "data_version": retrieval_completed_at,
                "pages_fetched": fetch_result.pages_fetched,
                "records_found": fetch_result.records_found,
                "started_at": retrieval_started_at,
            },
            "status": status,
            "topic_vocabulary": {
                "source": canonical_context.topic_vocabulary_source,
                "values": list(canonical_context.topic_vocabulary),
            },
        },
        "records": [asdict(record) for record in candidates],
    }


def render_manifest(manifest: Mapping[str, Any]) -> str:
    """Render stable ASCII JSON terminated by exactly one LF."""
    return json.dumps(manifest, ensure_ascii=True, indent=2, sort_keys=True) + "\n"


def write_manifest(path: Path, manifest: Mapping[str, Any]) -> None:
    """Write a manifest as explicit UTF-8 with platform-independent LF endings."""
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", encoding="utf-8", newline="\n") as stream:
        stream.write(render_manifest(manifest))


def run_pipeline(
    client: WoSClient,
    canonical_context: CanonicalContext,
    *,
    query: str,
    database_id: str,
    clock: Callable[[], str] = _utc_now,
) -> tuple[dict[str, Any], bool]:
    """Run the fetch and return a success flag with its manifest."""
    started_at = clock()
    try:
        fetch_result = client.fetch_all(query, database_id)
    except FetchError as exc:
        failed_result = FetchResult(
            raw_records=(),
            records_found=0,
            query_id=None,
            pages_fetched=0,
            errors=tuple(client.errors or exc.events),
        )
        manifest = build_manifest(
            failed_result,
            canonical_context,
            query=query,
            database_id=database_id,
            retrieval_started_at=started_at,
            retrieval_completed_at=clock(),
        )
        manifest["provenance"]["status"] = "failed"
        return manifest, False

    manifest = build_manifest(
        fetch_result,
        canonical_context,
        query=query,
        database_id=database_id,
        retrieval_started_at=started_at,
        retrieval_completed_at=clock(),
    )
    return manifest, True


def _parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--query", default=DEFAULT_QUERY)
    parser.add_argument("--database-id", default=DEFAULT_DATABASE_ID)
    parser.add_argument("--max-attempts", type=int, default=3)
    parser.add_argument("--backoff-seconds", type=float, default=0.5)
    parser.add_argument("--max-backoff-seconds", type=float, default=4.0)
    parser.add_argument("--request-interval-seconds", type=float, default=0.5)
    parser.add_argument(
        "--canonical-bib",
        action="append",
        dest="canonical_bibs",
        type=Path,
        help="Canonical BibTeX source; repeat to override repository defaults.",
    )
    parser.add_argument("--topic-rule", type=Path, default=DEFAULT_TOPIC_RULE)
    return parser


def main() -> int:
    """Run the command-line metadata pipeline."""
    args = _parser().parse_args()
    api_key = os.environ.get("WOS_EXPANDED_API_KEY", "").strip()
    if not api_key:
        print("ERROR: WOS_EXPANDED_API_KEY is required", file=sys.stderr)
        return 2

    bibliography_paths = tuple(args.canonical_bibs or DEFAULT_CANONICAL_BIBS)
    try:
        canonical_context = load_canonical_context(bibliography_paths, args.topic_rule)
        client = WoSClient(
            api_key,
            max_attempts=args.max_attempts,
            backoff_seconds=args.backoff_seconds,
            max_backoff_seconds=args.max_backoff_seconds,
            request_interval_seconds=args.request_interval_seconds,
        )
        manifest, succeeded = run_pipeline(
            client,
            canonical_context,
            query=args.query,
            database_id=args.database_id,
        )
        write_manifest(args.output, manifest)
    except (OSError, PipelineError, ValueError) as exc:
        print(f"ERROR: {exc}", file=sys.stderr)
        return 1

    print(f"Wrote {len(manifest['records'])} candidate records to {args.output}")
    return 0 if succeeded else 1


if __name__ == "__main__":
    sys.exit(main())
