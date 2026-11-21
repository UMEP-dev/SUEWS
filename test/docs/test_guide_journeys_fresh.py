"""Fresh manifests bind immutable page bytes without claiming human review."""

from copy import deepcopy
import os
from pathlib import Path
import subprocess
import sys

import pytest

pytest.importorskip("docutils", reason="Source binding requires optional docutils")
ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT))
import yaml  # noqa: E402

from scripts.suews.guide_journeys.extractor import Document  # noqa: E402
from scripts.suews.guide_journeys.fresh import extract_fresh  # noqa: E402
from scripts.suews.guide_journeys.manifest import check, load  # noqa: E402
from scripts.suews.guide_journeys.runner import resolve_steps, run_steps  # noqa: E402

pytestmark = pytest.mark.api


@pytest.fixture
def page_repo(tmp_path):
    """Commit the small fixture, then leave a contradictory working-tree edit."""
    page = tmp_path / "page.rst"
    text = (ROOT / "test/fixtures/guide_journeys/python_page.rst").read_text(
        encoding="utf-8"
    )
    page.write_text(text, encoding="utf-8")
    subprocess.run(["git", "init", "-q", str(tmp_path)], check=True)
    subprocess.run(["git", "add", "page.rst"], cwd=tmp_path, check=True)
    subprocess.run(
        [
            "git",
            "-c",
            "user.name=Test",
            "-c",
            "user.email=test@example.org",
            "commit",
            "-qm",
            "Fixture page",
        ],
        cwd=tmp_path,
        check=True,
    )
    page.write_text(text.replace("total = 0", "total = 99"), encoding="utf-8")
    return tmp_path, text


def fixture_manifest(root, expected):
    """Freeze a memory promise and keep installation outside statement execution."""
    manifest = extract_fresh(root, "HEAD", "page.rst", "Small reader journey")
    install = manifest["steps"].pop(0)
    manifest["installation"] = {
        "guide": "page.rst",
        "title": manifest["title"],
        **{k: v for k, v in install.items() if k not in {"id", "consumes"}},
    }
    manifest["exclusions"] = [
        dict(
            install,
            kind="installation",
            reason="Executed during environment preparation",
        )
    ]
    steps = [
        {
            "id": s["id"],
            "promises": [{"kind": "memory", "expression": "True", "expected": True}],
        }
        for s in manifest["steps"]
    ]
    steps[2]["promises"] = [
        {"kind": "memory", "expression": "total", "expected": expected}
    ]
    manifest["journeys"] = [{"id": "fixture", "inputs": {}, "steps": steps}]
    return manifest


@pytest.mark.parametrize("expected", [6, 7])
def test_fresh_fixture_memory_and_consumer_boundary(page_repo, expected):
    """Both outcomes execute revision bytes; failure leaves the consumer unrun."""
    root, text = page_repo
    manifest = fixture_manifest(root, expected)
    path = root / "fresh.yml"
    path.write_text(yaml.safe_dump(manifest), encoding="utf-8")
    manifest = load(path)
    document = Document(text, manifest["title"])
    revision = manifest["extraction_stamp"]["revision"]
    assert check(document, manifest, revision) == []
    assert "reviewed" not in yaml.safe_dump(manifest)
    blob = subprocess.check_output(
        ["git", "rev-parse", "HEAD:page.rst"], cwd=root, text=True
    ).strip()
    assert manifest["source_blob"] == blob
    specs = resolve_steps(document, manifest, manifest["journeys"][0])
    records, consumer = run_steps(specs, root, dict(os.environ), {})
    assert records[2]["promises"][0]["observed"]["value"] == 6
    assert records[-1]["status"] == ("passed" if expected == 6 else "unrun")
    if expected == 7:
        assert consumer == specs[3]["id"]


@pytest.mark.parametrize(
    "mutation", ["blob", "revision", "hash", "coordinate", "reviewed", "replacement"]
)
def test_fresh_binding_fails_closed(page_repo, mutation):
    """Neither stale coordinates nor invented review or executable text is accepted."""
    root, text = page_repo
    manifest = fixture_manifest(root, 6)
    if mutation == "blob":
        manifest["source_blob"] = "0" * 40
    elif mutation == "revision":
        manifest["extraction_stamp"]["revision"] = "0" * 40
    elif mutation == "hash":
        manifest["steps"][0]["sha256"] = "0" * 64
    elif mutation == "coordinate":
        manifest["steps"][0]["node"] = -1
    elif mutation == "reviewed":
        manifest["steps"][0]["reviewed_revision"] = "invented"
    else:
        manifest["steps"][0]["text"] = "total = 6"
    assert check(
        Document(text, manifest["title"]),
        manifest,
        subprocess.check_output(
            ["git", "rev-parse", "HEAD"], cwd=root, text=True
        ).strip(),
    )


def test_committed_and_fresh_span_objects_agree(page_repo):
    """Coordinates and anchors resolve to the identical typed source span."""
    root, text = page_repo
    manifest = fixture_manifest(root, 6)
    document = Document(text, manifest["title"])
    fresh = manifest["steps"][2]
    anchored = deepcopy(fresh)
    anchored.pop("node")
    anchored["anchor"] = "fixture/python"
    assert document.span(anchored) == document.span(fresh)


def test_install_exclusion_cannot_hide_an_unexecuted_statement(page_repo):
    """Excluding an instruction as setup requires the same actual install binding."""
    root, text = page_repo
    manifest = fixture_manifest(root, 6)
    manifest["installation"]["command"] = 99
    assert check(
        Document(text, manifest["title"]),
        manifest,
        manifest["extraction_stamp"]["revision"],
    )
