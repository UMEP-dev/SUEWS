"""Contract tests for the minimal Web of Science metadata pipeline."""

from __future__ import annotations

from dataclasses import asdict
from io import BytesIO
import json
from pathlib import Path
from urllib.error import HTTPError
from urllib.parse import parse_qs, urlparse

import pytest

from scripts.suews.wos_metadata import (
    CanonicalContext,
    FetchResult,
    PublicationRecord,
    WoSClient,
    build_manifest,
    deduplicate_records,
    load_canonical_context,
    parse_record,
    render_manifest,
    write_manifest,
)

pytestmark = pytest.mark.api

FIXED_SOFTWARE = {
    "http_client": "urllib.request (Python standard library)",
    "pipeline_sha256": "abc123",
    "pipeline_version": "1.0.0",
    "python": "3.13.0",
}


class JsonResponse(BytesIO):
    """Context-managed byte response for a mocked URL opener."""

    def __init__(self, payload: dict):
        super().__init__(json.dumps(payload).encode("utf-8"))


def _raw_record(
    *,
    uid: str = "WOS:1",
    doi: str = "10.1000/example",
    title: str = "A <i>SUEWS</i> study &amp; evaluation",
    year: int = 2026,
) -> dict:
    return {
        "UID": uid,
        "dynamic_data": {
            "cluster_related": {
                "identifiers": {"identifier": {"type": "doi", "value": doi}}
            }
        },
        "static_data": {
            "fullrecord_metadata": {
                "abstracts": {"abstract": {"abstract_text": {"p": "Restricted"}}},
                "addresses": {
                    "address_name": [{"address_spec": {"full_address": "Private"}}]
                },
            },
            "summary": {
                "pub_info": {"pubyear": year},
                "titles": {
                    "title": [
                        {"content": title, "type": "item"},
                        {"content": "JOURNAL", "type": "source"},
                    ]
                },
            },
        },
    }


def _context(*canonical_dois: str) -> CanonicalContext:
    return CanonicalContext(
        dois=frozenset(canonical_dois),
        bibliography=(
            {
                "doi_count": len(canonical_dois),
                "path": "refs.bib",
                "sha256": "bib-sha",
                "topics_seen": ["energy-balance"],
            },
        ),
        topic_vocabulary=("energy-balance", "water-balance"),
        topic_vocabulary_source={"path": "topics.md", "sha256": "topic-sha"},
    )


def test_parse_record_exports_only_permitted_fields() -> None:
    record = parse_record(_raw_record())

    assert record == PublicationRecord(
        uid="WOS:1",
        doi="10.1000/example",
        title="A SUEWS study & evaluation",
        year=2026,
    )
    assert set(asdict(record)) == {"uid", "doi", "title", "year"}


def test_single_record_pagination_accepts_mapping_shape() -> None:
    calls: list[dict[str, list[str]]] = []

    def open_single(request, *, timeout):
        assert timeout == pytest.approx(60.0)
        calls.append(parse_qs(urlparse(request.full_url).query))
        return JsonResponse({
            "Data": {"Records": {"records": {"REC": _raw_record()}}},
            "QueryResult": {"QueryID": 42, "RecordsFound": 1},
        })

    result = WoSClient(
        "secret",
        open_fn=open_single,
        request_interval_seconds=0,
        sleep_fn=lambda _: None,
    ).fetch_all("TS=SUEWS", "WOS")

    assert result.records_found == 1
    assert result.pages_fetched == 1
    assert result.query_id == 42
    assert [record["UID"] for record in result.raw_records] == ["WOS:1"]
    assert calls == [
        {
            "count": ["50"],
            "databaseId": ["WOS"],
            "firstRecord": ["1"],
            "usrQuery": ["TS=SUEWS"],
        }
    ]


def test_doi_deduplication_uses_canonical_bibliography_and_stable_winner(
    tmp_path: Path,
) -> None:
    topic_rule = tmp_path / "bib-topic-tags.md"
    topic_rule.write_text(
        "## Controlled vocabulary\n\n- `energy-balance` - energy\n",
        encoding="utf-8",
    )
    bibliography = tmp_path / "refs.bib"
    bibliography.write_text(
        """@article{Known,
  title = {Known},
  doi = {https://doi.org/10.1000/known},
  keywords = {energy-balance},
}
""",
        encoding="utf-8",
    )
    context = load_canonical_context((bibliography,), topic_rule)
    records = [
        PublicationRecord("WOS:3", "10.1000/new", "Zulu", 2026),
        PublicationRecord("WOS:1", "10.1000/known", "Known", 2024),
        PublicationRecord("WOS:2", "10.1000/new", "Alpha", 2025),
    ]

    candidates, summary = deduplicate_records(reversed(records), context.dois)

    assert candidates == [PublicationRecord("WOS:2", "10.1000/new", "Alpha", 2025)]
    assert summary == {
        "canonical_doi_matches_removed": 1,
        "duplicate_dois_removed": 1,
        "records_emitted": 1,
    }
    assert context.topic_vocabulary == ("energy-balance",)
    assert context.bibliography[0]["path"] == "external/refs.bib"
    assert context.topic_vocabulary_source["path"] == "external/bib-topic-tags.md"


def test_generated_manifest_is_deterministic_complete_and_lf_only(
    tmp_path: Path,
) -> None:
    raw_records = (_raw_record(uid="WOS:2", doi="10.1000/b", title="B"), _raw_record())
    fetch_result = FetchResult(
        raw_records=raw_records,
        records_found=2,
        query_id="query-1",
        pages_fetched=1,
        errors=(),
    )
    kwargs = {
        "query": "TS=SUEWS",
        "database_id": "WOS",
        "retrieval_started_at": "2026-08-19T10:00:00Z",
        "retrieval_completed_at": "2026-08-19T10:00:01Z",
        "software": FIXED_SOFTWARE,
    }
    manifest = build_manifest(fetch_result, _context(), **kwargs)
    reversed_manifest = build_manifest(
        FetchResult(
            raw_records=tuple(reversed(raw_records)),
            records_found=2,
            query_id="query-1",
            pages_fetched=1,
            errors=(),
        ),
        _context(),
        **kwargs,
    )

    assert render_manifest(manifest) == render_manifest(reversed_manifest)
    assert manifest["provenance"]["status"] == "complete"
    assert manifest["provenance"]["api"] == {
        "database_id": "WOS",
        "endpoint": "https://wos-api.clarivate.com/api/wos",
        "name": "Web of Science API Expanded",
        "query_id": "query-1",
        "version": "expanded-unversioned-endpoint",
    }
    assert manifest["provenance"]["dependencies"] == FIXED_SOFTWARE
    assert manifest["provenance"]["retrieval"]["data_version"] == (
        "2026-08-19T10:00:01Z"
    )
    assert set(manifest["provenance"]["field_provenance"]) == {
        "uid",
        "doi",
        "title",
        "year",
    }
    assert manifest["provenance"]["errors"] == []
    assert manifest["provenance"]["licensing"]["excluded_fields"] == [
        "abstract",
        "affiliations",
        "addresses",
        "authors",
        "citation_metrics",
        "funding",
        "keywords",
    ]

    output = tmp_path / "manifest.json"
    write_manifest(output, manifest)
    raw_output = output.read_bytes()
    assert raw_output.endswith(b"\n")
    assert b"\r\n" not in raw_output
    assert raw_output.count(b"Restricted") == 0
    assert raw_output.count(b"Private") == 0


def test_retry_backoff_is_bounded_and_recorded() -> None:
    attempts = 0
    sleeps: list[float] = []

    def open_after_retry(request, *, timeout):
        nonlocal attempts
        attempts += 1
        if attempts < 3:
            raise HTTPError(request.full_url, 429, "rate limited", {}, None)
        return JsonResponse({
            "Data": {"Records": {"records": {"REC": _raw_record()}}},
            "QueryResult": {"QueryID": 7, "RecordsFound": 1},
        })

    result = WoSClient(
        "secret",
        max_attempts=3,
        backoff_seconds=2.0,
        max_backoff_seconds=0.5,
        request_interval_seconds=0,
        open_fn=open_after_retry,
        sleep_fn=sleeps.append,
    ).fetch_all("TS=SUEWS", "WOS")

    assert result.records_found == 1
    assert attempts == 3
    assert sleeps == [0.5, 0.5]
    assert [event["will_retry"] for event in result.errors] == [True, True]
    assert all(event["backoff_seconds"] <= 0.5 for event in result.errors)
