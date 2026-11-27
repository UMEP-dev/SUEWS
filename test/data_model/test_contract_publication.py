"""Tests for forcing/output contract site publication, preservation, and validation."""

from __future__ import annotations

import importlib.resources
import importlib.util
import json
from pathlib import Path
import shutil
import subprocess
import sys
from types import ModuleType

from click.testing import CliRunner
from jsonschema import Draft202012Validator
import pytest

from supy.cmd.suews_cli import cli
import supy.data_model.contracts as contracts_mod
from supy.data_model.contracts import (
    BUNDLE_FILENAMES,
    ContractError,
    ContractPublicationError,
    canonical_json_bytes,
    export_contract,
    forcing_contract_json_schema,
    load_contract_bundle_bytes,
    publish_contracts_site,
    sha256_digest,
    validate_published_contracts,
)
from supy.data_model.forcing.version import CURRENT_FORCING_VERSION, FORCING_VERSIONS
from supy.data_model.output.version import CURRENT_OUTPUT_VERSION, OUTPUT_VERSIONS

pytestmark = [pytest.mark.api, pytest.mark.contract("C-SCHEMA", "C-TOOLING")]

PROJECT_ROOT = Path(__file__).resolve().parents[2]
SCRIPT_PATH = PROJECT_ROOT / "scripts/suews/publish_contracts.py"


def _snapshot_tree(root: Path) -> dict[str, bytes]:
    return {
        p.relative_to(root).as_posix(): p.read_bytes()
        for p in sorted(root.rglob("*"))
        if p.is_file()
    }


def test_frozen_forcing_snapshots_are_packaged_immutable_and_current(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    """Forcing snapshots are frozen on disk and unaffected by live schema changes."""
    assert CURRENT_FORCING_VERSION is not None
    for ver in FORCING_VERSIONS:
        for fname in ("catalogue.schema.json", "manifest.json"):
            res = importlib.resources.files("supy.data_model").joinpath(
                f"contract_artefacts/forcing/{ver}/{fname}"
            )
            assert res.is_file(), f"missing packaged forcing snapshot {ver}/{fname}"

    # Current version snapshot schema matches live forcing_contract_json_schema().
    _, current_bundle = load_contract_bundle_bytes("forcing", CURRENT_FORCING_VERSION)
    assert current_bundle["catalogue.schema.json"] == canonical_json_bytes(
        forcing_contract_json_schema()
    )

    # Historical release bytes do not change even if forcing_contract_json_schema() changes.
    _, before = load_contract_bundle_bytes("forcing", "1.0.0")
    monkeypatch.setattr(
        contracts_mod,
        "forcing_contract_json_schema",
        lambda: {"type": "object", "required": ["future_breaking_field"]},
    )
    _, after = load_contract_bundle_bytes("forcing", "1.0.0")
    assert after == before


def test_publish_materialises_versioned_and_latest_artefacts(tmp_path: Path) -> None:
    """Build every registered release and matching ``latest`` aliases with valid schemas."""
    out_dir = tmp_path / "contracts"
    report = publish_contracts_site(out_dir, base_url="https://suews.io", validate=True)
    assert report["validated"] is True
    assert CURRENT_FORCING_VERSION is not None
    assert CURRENT_OUTPUT_VERSION is not None

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

    for iface, current in (
        ("forcing", CURRENT_FORCING_VERSION),
        ("output", CURRENT_OUTPUT_VERSION),
    ):
        for fname in BUNDLE_FILENAMES:
            assert (out_dir / iface / "latest" / fname).read_bytes() == (
                out_dir / iface / current / fname
            ).read_bytes()

    # Clean build and preservation build agree byte-for-byte.
    preserved_dir = tmp_path / "contracts-preserved"
    publish_contracts_site(
        preserved_dir,
        base_url="https://suews.io",
        preserve_from=out_dir,
        validate=True,
    )
    assert _snapshot_tree(preserved_dir) == _snapshot_tree(out_dir)


def test_publish_preserves_historical_releases_and_keeps_destination_atomic(
    tmp_path: Path,
) -> None:
    """Carry forward historical versions and leave existing destination untouched on conflict."""
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

    # Late output conflict leaves an existing destination tree completely byte-identical.
    existing_dest = tmp_path / "existing_dest"
    publish_contracts_site(
        existing_dest, base_url="https://suews.io/preview/pr-1", validate=True
    )
    before_dest = _snapshot_tree(existing_dest)

    tampered_prev = tmp_path / "tampered_prev"
    publish_contracts_site(tampered_prev, base_url="https://suews.io", validate=True)
    bad_cat = tampered_prev / "output" / "1.0.0" / "catalogue.json"
    bad_cat.write_bytes(bad_cat.read_bytes() + b" ")
    with pytest.raises(ContractPublicationError, match=r"output/1\.0\.0"):
        publish_contracts_site(
            existing_dest,
            base_url="https://suews.io",
            preserve_from=tampered_prev,
            validate=True,
        )
    assert _snapshot_tree(existing_dest) == before_dest

    # Wrong preservation root (parent directory instead of contracts/) fails closed.
    parent_wrong = tmp_path / "wrong_parent"
    (parent_wrong / "contracts").mkdir(parents=True)
    with pytest.raises(ContractPublicationError, match="must be a contracts root"):
        publish_contracts_site(
            tmp_path / "out_wrong_parent",
            preserve_from=parent_wrong,
            validate=True,
        )


def test_publish_rejects_symlinks_without_modifying_outside_targets(
    tmp_path: Path,
) -> None:
    """Symlinks in the destination tree are rejected before any file write or deletion."""
    outside_file = tmp_path / "outside.txt"
    outside_file.write_text("do-not-touch\n", encoding="ascii")
    outside_dir = tmp_path / "outside_dir"
    outside_dir.mkdir()
    outside_marker = outside_dir / "keep.txt"
    outside_marker.write_text("intact\n", encoding="ascii")

    # Case 1: symlink file inside output_dir
    dest_file_link = tmp_path / "dest_file_link"
    (dest_file_link / "forcing").mkdir(parents=True)
    (dest_file_link / "forcing" / "index.json").symlink_to(outside_file)
    with pytest.raises(ContractPublicationError, match="symlink"):
        publish_contracts_site(dest_file_link)
    assert outside_file.read_text(encoding="ascii") == "do-not-touch\n"

    # Case 2: symlink interface directory inside output_dir
    dest_dir_link = tmp_path / "dest_dir_link"
    dest_dir_link.mkdir()
    (dest_dir_link / "forcing").symlink_to(outside_dir)
    with pytest.raises(ContractPublicationError, match="symlink"):
        publish_contracts_site(dest_dir_link)
    assert outside_marker.read_text(encoding="ascii") == "intact\n"

    # Case 3: output_dir itself is a symlink
    root_link = tmp_path / "root_link"
    root_link.symlink_to(outside_dir)
    with pytest.raises(ContractPublicationError, match="symlink"):
        publish_contracts_site(root_link)
    assert outside_marker.read_text(encoding="ascii") == "intact\n"


def test_validator_forbids_external_schema_refs_offline(tmp_path: Path) -> None:
    """Preserved schemas with external ``$ref`` fail offline without network fetches."""
    base = tmp_path / "contracts"
    publish_contracts_site(base, base_url="https://suews.io", validate=True)

    hist_dir = base / "forcing" / "0.7.0"
    hist_dir.mkdir(parents=True)
    cat_bytes = canonical_json_bytes({"ok": True})
    schema_bytes = canonical_json_bytes({
        "$schema": "https://json-schema.org/draft/2020-12/schema",
        "$ref": "http://127.0.0.1:9/evil-external.json",
    })
    manifest_bytes = canonical_json_bytes({
        "files": {
            "catalogue.json": sha256_digest(cat_bytes),
            "catalogue.schema.json": sha256_digest(schema_bytes),
        },
        "interface": "forcing",
        "version": "0.7.0",
    })
    (hist_dir / "catalogue.json").write_bytes(cat_bytes)
    (hist_dir / "catalogue.schema.json").write_bytes(schema_bytes)
    (hist_dir / "manifest.json").write_bytes(manifest_bytes)

    with pytest.raises(ContractPublicationError, match="fails schema validation"):
        validate_published_contracts(base, base_url="https://suews.io")


def test_validate_checks_discovery_metadata_and_latest_drift(
    tmp_path: Path,
) -> None:
    """Validator catches metadata tampering, duplicate versions, and latest drift."""
    base = tmp_path / "contracts"
    publish_contracts_site(base, base_url="https://suews.io", validate=True)

    # 1. Drift in latest/
    latest_cat = base / "forcing" / "latest" / "catalogue.json"
    orig_latest = latest_cat.read_bytes()
    latest_cat.write_bytes(orig_latest + b"\n")
    with pytest.raises(
        ContractPublicationError, match=r"forcing/latest/catalogue\.json"
    ):
        validate_published_contracts(base, base_url="https://suews.io")
    latest_cat.write_bytes(orig_latest)

    # 2. Tampered latest.urls.catalogue in interface index.json
    iface_idx_path = base / "forcing" / "index.json"
    orig_iface_idx = iface_idx_path.read_bytes()
    mutated = json.loads(orig_iface_idx.decode("ascii"))
    mutated["latest"]["urls"]["catalogue"] = (
        "https://suews.io/contracts/forcing/1.0.0/catalogue.json"
    )
    iface_idx_path.write_bytes(canonical_json_bytes(mutated))
    with pytest.raises(ContractPublicationError, match=r"latest\.urls"):
        validate_published_contracts(base, base_url="https://suews.io")

    # 3. Tampered version digest in interface index.json
    mutated = json.loads(orig_iface_idx.decode("ascii"))
    mutated["versions"][0]["digests"]["catalogue.json"] = "sha256:" + "0" * 64
    iface_idx_path.write_bytes(canonical_json_bytes(mutated))
    with pytest.raises(ContractPublicationError, match=r"digests"):
        validate_published_contracts(base, base_url="https://suews.io")

    # 4. Duplicate version entry in interface index.json
    mutated = json.loads(orig_iface_idx.decode("ascii"))
    mutated["versions"].append(dict(mutated["versions"][0]))
    iface_idx_path.write_bytes(canonical_json_bytes(mutated))
    with pytest.raises(ContractPublicationError, match="duplicate version entry"):
        validate_published_contracts(base, base_url="https://suews.io")
    iface_idx_path.write_bytes(orig_iface_idx)

    # 5. Tampered root index.json current_version / version_count
    root_idx_path = base / "index.json"
    orig_root_idx = root_idx_path.read_bytes()
    mutated_root = json.loads(orig_root_idx.decode("ascii"))
    mutated_root["interfaces"]["output"]["current_version"] = "1.0.0"
    mutated_root["interfaces"]["output"]["version_count"] = 999
    root_idx_path.write_bytes(canonical_json_bytes(mutated_root))
    with pytest.raises(ContractPublicationError, match="current_version"):
        validate_published_contracts(base, base_url="https://suews.io")
    root_idx_path.write_bytes(orig_root_idx)


def test_validate_rejects_bad_semver_and_broken_html_links(tmp_path: Path) -> None:
    """Leading-zero SemVer, broken relative links, and path traversal fail closed."""
    base = tmp_path / "contracts"
    publish_contracts_site(base, base_url="https://suews.io", validate=True)

    # 1. Leading-zero SemVer directory (01.0.0) rejected by preservation and validation
    bad_semver_dir = base / "forcing" / "01.0.0"
    shutil.copytree(base / "forcing" / "1.0.0", bad_semver_dir)
    with pytest.raises(ContractPublicationError, match="non-SemVer"):
        validate_published_contracts(base, base_url="https://suews.io")
    with pytest.raises(
        ContractPublicationError, match="invalid release directory name"
    ):
        publish_contracts_site(tmp_path / "reject_semver", preserve_from=base)
    shutil.rmtree(bad_semver_dir)

    # 2. Broken and escaping relative links in HTML
    index_html = base / "index.html"
    orig_html = index_html.read_text(encoding="ascii")
    index_html.write_text(
        orig_html.replace("</body>", '<a href="missing/file.json">broken</a></body>'),
        encoding="ascii",
    )
    with pytest.raises(ContractPublicationError, match="target file does not exist"):
        validate_published_contracts(base, base_url="https://suews.io")

    index_html.write_text(
        orig_html.replace("</body>", '<a href="../outside.json">escape</a></body>'),
        encoding="ascii",
    )
    with pytest.raises(ContractPublicationError, match="escapes contracts root"):
        validate_published_contracts(base, base_url="https://suews.io")
    index_html.write_text(orig_html, encoding="ascii")


def test_corrupted_packaged_output_content_rejected_by_loader_and_cli(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """Corrupting output catalogue/schema while keeping manifest intact fails all exports."""
    fake_output_pkg = tmp_path / "fake_output"
    src_ver_dir = PROJECT_ROOT / "src/supy/data_model/output/artefacts/2.1.0"
    dst_ver_dir = fake_output_pkg / "artefacts/2.1.0"
    shutil.copytree(src_ver_dir, dst_ver_dir)
    # Tamper with catalogue.json while leaving manifest.json untouched.
    cat_file = dst_ver_dir / "catalogue.json"
    cat_file.write_bytes(cat_file.read_bytes() + b" ")

    monkeypatch.setattr(contracts_mod, "_resource_root", lambda iface: fake_output_pkg)

    with pytest.raises(ContractError, match="manifest"):
        load_contract_bundle_bytes("output", "2.1.0")
    with pytest.raises(ContractError, match="manifest"):
        export_contract("output", tmp_path / "cat.json", version="2.1.0")
    with pytest.raises(ContractError, match="manifest"):
        export_contract(
            "output", tmp_path / "bundle", version="2.1.0", artefact="bundle"
        )

    runner = CliRunner()
    res = runner.invoke(
        cli, ["contracts", "export", "output", "--version", "2.1.0", "--format", "json"]
    )
    assert res.exit_code == 1
    assert json.loads(res.output)["status"] == "error"


def test_standalone_script_builds_validates_and_ignores_stale_installed_supy(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """``scripts/suews/publish_contracts.py`` always uses checkout-local contracts."""
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
    assert json.loads(build.stdout)["validated"] is True

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

    # Even if an older/stale installed supy.data_model.contracts is in sys.modules,
    # the repository script still loads its own checkout's contracts module.
    stale = ModuleType("supy.data_model.contracts")
    monkeypatch.setitem(sys.modules, "supy.data_model.contracts", stale)

    spec = importlib.util.spec_from_file_location("publish_contracts_test", SCRIPT_PATH)
    assert spec is not None and spec.loader is not None
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)

    loaded = mod._load_contracts_module()
    assert loaded is not stale
    standalone_out = tmp_path / "standalone-contracts"
    report = loaded.publish_contracts_site(
        standalone_out, base_url="https://suews.io", validate=True
    )
    assert report["validated"] is True
    assert report["interfaces"]["forcing"]["current_version"] == CURRENT_FORCING_VERSION
    assert report["interfaces"]["output"]["current_version"] == CURRENT_OUTPUT_VERSION
