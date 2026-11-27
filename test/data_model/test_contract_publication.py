"""Tests for forcing/output contract site publication, preservation, and validation."""

from __future__ import annotations

import importlib.util
import json
from pathlib import Path
import subprocess
import sys

from jsonschema import Draft202012Validator
import pytest

from supy.data_model.contracts import (
    BUNDLE_FILENAMES,
    ContractPublicationError,
    canonical_json_bytes,
    publish_contracts_site,
    sha256_digest,
    validate_published_contracts,
)
from supy.data_model.forcing.version import CURRENT_FORCING_VERSION, FORCING_VERSIONS
from supy.data_model.output.version import CURRENT_OUTPUT_VERSION, OUTPUT_VERSIONS

pytestmark = [pytest.mark.api, pytest.mark.contract("C-SCHEMA", "C-TOOLING")]

PROJECT_ROOT = Path(__file__).resolve().parents[2]
SCRIPT_PATH = PROJECT_ROOT / "scripts/suews/publish_contracts.py"


def test_publish_materialises_versioned_and_latest_artefacts(tmp_path: Path) -> None:
    """Build every registered release and matching ``latest`` aliases with valid schemas."""
    out_dir = tmp_path / "contracts"
    report = publish_contracts_site(out_dir, base_url="https://suews.io", validate=True)
    assert report["validated"] is True

    assert CURRENT_FORCING_VERSION is not None
    assert CURRENT_OUTPUT_VERSION is not None

    # Every forcing release is present, digest-exact, and schema-valid.
    for ver, expected_digest in FORCING_VERSIONS.items():
        ver_dir = out_dir / "forcing" / ver
        assert {p.name for p in ver_dir.iterdir()} == set(BUNDLE_FILENAMES)
        cat_bytes = (ver_dir / "catalogue.json").read_bytes()
        assert sha256_digest(cat_bytes) == expected_digest
        schema = json.loads(
            (ver_dir / "catalogue.schema.json").read_text(encoding="utf-8")
        )
        catalogue = json.loads(cat_bytes.decode("utf-8"))
        Draft202012Validator.check_schema(schema)
        Draft202012Validator(schema).validate(catalogue)

    # Every output release is present, digest-exact, and schema-valid.
    for ver, expected_digest in OUTPUT_VERSIONS.items():
        ver_dir = out_dir / "output" / ver
        assert {p.name for p in ver_dir.iterdir()} == set(BUNDLE_FILENAMES)
        manifest_bytes = (ver_dir / "manifest.json").read_bytes()
        assert sha256_digest(manifest_bytes) == expected_digest
        schema = json.loads(
            (ver_dir / "catalogue.schema.json").read_text(encoding="utf-8")
        )
        catalogue = json.loads((ver_dir / "catalogue.json").read_text(encoding="utf-8"))
        Draft202012Validator.check_schema(schema)
        Draft202012Validator(schema).validate(catalogue)

    # Moving latest/ directories match current versions byte-for-byte.
    for iface, current in (
        ("forcing", CURRENT_FORCING_VERSION),
        ("output", CURRENT_OUTPUT_VERSION),
    ):
        for fname in BUNDLE_FILENAMES:
            assert (out_dir / iface / "latest" / fname).read_bytes() == (
                out_dir / iface / current / fname
            ).read_bytes()


def test_publish_preserves_historical_releases_and_rejects_mutation(
    tmp_path: Path,
) -> None:
    """Carry forward historical versions from prior deployments and refuse byte mutation."""
    previous = tmp_path / "previous"
    publish_contracts_site(previous, base_url="https://suews.io", validate=True)

    # Synthesise a previously published older forcing release (0.9.0) in the prior site.
    hist_ver = "0.9.0"
    hist_dir = previous / "forcing" / hist_ver
    hist_dir.mkdir(parents=True)
    cat_bytes = (previous / "forcing" / "1.0.0" / "catalogue.json").read_bytes()
    schema_bytes = (
        previous / "forcing" / "1.0.0" / "catalogue.schema.json"
    ).read_bytes()
    manifest_bytes = canonical_json_bytes({
        "files": {
            "catalogue.json": sha256_digest(cat_bytes),
            "catalogue.schema.json": sha256_digest(schema_bytes),
        },
        "interface": "forcing",
        "version": hist_ver,
    })
    (hist_dir / "catalogue.json").write_bytes(cat_bytes)
    (hist_dir / "catalogue.schema.json").write_bytes(schema_bytes)
    (hist_dir / "manifest.json").write_bytes(manifest_bytes)

    next_site = tmp_path / "next"
    report = publish_contracts_site(
        next_site,
        base_url="https://suews.io",
        preserve_from=previous,
        validate=True,
    )
    assert report["interfaces"]["forcing"]["preserved_versions"] == [hist_ver]
    assert hist_ver in report["interfaces"]["forcing"]["versions"]
    assert (
        next_site / "forcing" / hist_ver / "catalogue.json"
    ).read_bytes() == cat_bytes
    assert f"{hist_ver}/catalogue.json" in (
        next_site / "forcing" / "index.html"
    ).read_text(encoding="ascii")

    # Tampering with an already-published output release fails closed rather than overwriting.
    tampered_prev = tmp_path / "tampered_prev"
    publish_contracts_site(tampered_prev, base_url="https://suews.io", validate=True)
    bad_cat = tampered_prev / "output" / "1.0.0" / "catalogue.json"
    bad_cat.write_bytes(bad_cat.read_bytes() + b" ")
    with pytest.raises(ContractPublicationError, match=r"output/1\.0\.0"):
        publish_contracts_site(
            tmp_path / "rejected",
            base_url="https://suews.io",
            preserve_from=tampered_prev,
            validate=True,
        )


def test_validate_catches_latest_drift_broken_links_and_schema_failure(
    tmp_path: Path,
) -> None:
    """Validator fails closed on latest drift, broken/escaping HTML links, and bad JSON."""
    base = tmp_path / "contracts"
    publish_contracts_site(base, base_url="https://suews.io", validate=True)

    # 1. Drift in latest/
    latest_cat = base / "forcing" / "latest" / "catalogue.json"
    original_latest = latest_cat.read_bytes()
    latest_cat.write_bytes(original_latest + b"\n")
    with pytest.raises(
        ContractPublicationError, match=r"forcing/latest/catalogue\.json"
    ):
        validate_published_contracts(base, base_url="https://suews.io")
    latest_cat.write_bytes(original_latest)

    # 2. Broken relative link in HTML
    index_html = base / "index.html"
    original_html = index_html.read_text(encoding="ascii")
    index_html.write_text(
        original_html.replace(
            "</body>", '<a href="missing/file.json">broken</a></body>'
        ),
        encoding="ascii",
    )
    with pytest.raises(ContractPublicationError, match="target file does not exist"):
        validate_published_contracts(base, base_url="https://suews.io")

    # 3. Path-traversal link escaping contracts root
    index_html.write_text(
        original_html.replace("</body>", '<a href="../outside.json">escape</a></body>'),
        encoding="ascii",
    )
    with pytest.raises(ContractPublicationError, match="escapes contracts root"):
        validate_published_contracts(base, base_url="https://suews.io")
    index_html.write_text(original_html, encoding="ascii")

    # 4. Schema validation failure in a historical release (even with matching manifest)
    hist_ver = "0.8.0"
    hist_dir = base / "forcing" / hist_ver
    hist_dir.mkdir(parents=True)
    bad_catalogue = canonical_json_bytes({"unexpected": True})
    schema_bytes = (base / "forcing" / "1.0.0" / "catalogue.schema.json").read_bytes()
    manifest_bytes = canonical_json_bytes({
        "files": {
            "catalogue.json": sha256_digest(bad_catalogue),
            "catalogue.schema.json": sha256_digest(schema_bytes),
        },
        "interface": "forcing",
        "version": hist_ver,
    })
    (hist_dir / "catalogue.json").write_bytes(bad_catalogue)
    (hist_dir / "catalogue.schema.json").write_bytes(schema_bytes)
    (hist_dir / "manifest.json").write_bytes(manifest_bytes)
    with pytest.raises(ContractPublicationError, match="fails schema validation"):
        validate_published_contracts(base, base_url="https://suews.io")


def test_standalone_script_builds_and_validates_site(tmp_path: Path) -> None:
    """``scripts/suews/publish_contracts.py`` reproduces build and validation cleanly."""
    out_dir = tmp_path / "script-contracts"
    build = subprocess.run(
        [
            sys.executable,
            str(SCRIPT_PATH),
            "--output-dir",
            str(out_dir),
            "--base-url",
            "https://suews.io",
            "--json",
        ],
        capture_output=True,
        text=True,
        check=False,
    )
    assert build.returncode == 0, build.stdout + build.stderr
    build_payload = json.loads(build.stdout)
    assert build_payload["validated"] is True

    check = subprocess.run(
        [
            sys.executable,
            str(SCRIPT_PATH),
            "--validate-only",
            str(out_dir),
            "--base-url",
            "https://suews.io",
            "--json",
        ],
        capture_output=True,
        text=True,
        check=False,
    )
    assert check.returncode == 0, check.stdout + check.stderr
    check_payload = json.loads(check.stdout)
    assert check_payload["interfaces"]["forcing"] == list(FORCING_VERSIONS)
    assert check_payload["interfaces"]["output"] == list(OUTPUT_VERSIONS)


def test_standalone_loader_works_without_installed_supy(tmp_path: Path) -> None:
    """The standalone namespace loader resolves packaged artefacts directly from src/."""
    spec = importlib.util.spec_from_file_location("publish_contracts_test", SCRIPT_PATH)
    assert spec is not None and spec.loader is not None
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)

    standalone_contracts = mod._load_contracts_module(force_standalone=True)
    out_dir = tmp_path / "standalone-contracts"
    report = standalone_contracts.publish_contracts_site(
        out_dir, base_url="https://suews.io", validate=True
    )
    assert report["validated"] is True
    assert report["interfaces"]["forcing"]["current_version"] == CURRENT_FORCING_VERSION
    assert report["interfaces"]["output"]["current_version"] == CURRENT_OUTPUT_VERSION
