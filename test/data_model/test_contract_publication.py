"""Tests for forcing/output contract site publication, preservation, and validation."""

from __future__ import annotations

import gzip
import importlib.resources
import importlib.util
import json
import os
from pathlib import Path
import shutil
import stat
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
    FORCING_PUBLISHED_MANIFEST_DIGESTS,
    ContractError,
    ContractPublicationError,
    canonical_json_bytes,
    export_contract,
    forcing_contract_json_schema,
    load_contract_bundle_bytes,
    publish_contracts_site,
    sha256_digest,
    validate_published_contracts,
    write_forcing_published_snapshot,
)
from supy.data_model.forcing.version import CURRENT_FORCING_VERSION, FORCING_VERSIONS
from supy.data_model.output.version import CURRENT_OUTPUT_VERSION, OUTPUT_VERSIONS
from supy.knowledge.pack import EXCLUDED_GENERATED_ROOTS, build_pack

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
    """Forcing snapshots are pinned, historically accurate, and immune to live drift."""
    assert CURRENT_FORCING_VERSION is not None
    assert set(FORCING_PUBLISHED_MANIFEST_DIGESTS) == set(FORCING_VERSIONS)
    for ver in FORCING_VERSIONS:
        for fname in ("catalogue.schema.json", "manifest.json"):
            res = importlib.resources.files("supy.data_model").joinpath(
                f"contract_artefacts/forcing/{ver}/{fname}"
            )
            assert res.is_file(), f"missing packaged forcing snapshot {ver}/{fname}"

    # Historical schemas match historical capabilities (no backwards capability leakage).
    _, bundle_100 = load_contract_bundle_bytes("forcing", "1.0.0")
    schema_100 = json.loads(bundle_100["catalogue.schema.json"].decode("utf-8"))
    assert "supported_timestamp_references" not in schema_100["properties"]
    assert (
        "declared_rate_unit" not in schema_100["$defs"]["ForcingVariable"]["properties"]
    )

    _, bundle_110 = load_contract_bundle_bytes("forcing", "1.1.0")
    schema_110 = json.loads(bundle_110["catalogue.schema.json"].decode("utf-8"))
    assert "supported_timestamp_references" in schema_110["properties"]
    assert (
        "declared_rate_unit" not in schema_110["$defs"]["ForcingVariable"]["properties"]
    )

    # Current version snapshot schema matches live forcing_contract_json_schema().
    _, current_bundle = load_contract_bundle_bytes("forcing", CURRENT_FORCING_VERSION)
    assert current_bundle["catalogue.schema.json"] == canonical_json_bytes(
        forcing_contract_json_schema()
    )

    # Historical release bytes do not change even if forcing_contract_json_schema() changes.
    monkeypatch.setattr(
        contracts_mod,
        "forcing_contract_json_schema",
        lambda: {"type": "object", "required": ["future_breaking_field"]},
    )
    _, after = load_contract_bundle_bytes("forcing", "1.0.0")
    assert after == bundle_100


def test_forcing_manifest_pin_rejects_coordinated_schema_and_manifest_tamper(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """Changing forcing schema plus its manifest fails against the pinned manifest digest."""
    base = tmp_path / "contracts"
    publish_contracts_site(base, base_url="https://suews.io", validate=True)

    # Coordinated tamper in a published site: replace 1.0.0 schema with {} and recompute manifest.
    v100_dir = base / "forcing" / "1.0.0"
    cat_bytes = (v100_dir / "catalogue.json").read_bytes()
    fake_schema = canonical_json_bytes({})
    fake_manifest = canonical_json_bytes({
        "files": {
            "catalogue.json": sha256_digest(cat_bytes),
            "catalogue.schema.json": sha256_digest(fake_schema),
        },
        "interface": "forcing",
        "version": "1.0.0",
    })
    (v100_dir / "catalogue.schema.json").write_bytes(fake_schema)
    (v100_dir / "manifest.json").write_bytes(fake_manifest)
    with pytest.raises(ContractPublicationError, match="pinned digest"):
        validate_published_contracts(base, base_url="https://suews.io")

    # Coordinated tamper in packaged snapshots also fails in load_contract_bundle_bytes.
    fake_snap_root = tmp_path / "fake_contract_artefacts"
    fake_v100 = fake_snap_root / "forcing" / "1.0.0"
    fake_v100.mkdir(parents=True)
    (fake_v100 / "catalogue.schema.json").write_bytes(fake_schema)
    (fake_v100 / "manifest.json").write_bytes(fake_manifest)
    monkeypatch.setattr(
        contracts_mod, "_contract_artefacts_root", lambda: fake_snap_root
    )
    with pytest.raises(ContractError, match="pinned digest"):
        load_contract_bundle_bytes("forcing", "1.0.0")


def test_write_forcing_published_snapshot_workflow_and_overwrite_protection(
    tmp_path: Path,
) -> None:
    """New forcing release snapshot writer creates valid bundles and refuses byte overwrites."""
    fake_repo = tmp_path / "repo"
    art_dir = fake_repo / "src/supy/data_model/forcing/artefacts"
    art_dir.mkdir(parents=True)
    src_cat = (
        PROJECT_ROOT / "src/supy/data_model/forcing/artefacts/2.1.0.json"
    ).read_bytes()
    (art_dir / "2.2.0.json").write_bytes(src_cat)

    digest = write_forcing_published_snapshot("2.2.0", fake_repo)
    assert digest.startswith("sha256:")
    snap_dir = fake_repo / "src/supy/data_model/contract_artefacts/forcing/2.2.0"
    assert {p.name for p in snap_dir.iterdir()} == {
        "catalogue.schema.json",
        "manifest.json",
    }
    # Idempotent when bytes match.
    assert write_forcing_published_snapshot("2.2.0", fake_repo) == digest

    # Refuses to overwrite differing bytes.
    (snap_dir / "catalogue.schema.json").write_bytes(b"{}\n")
    with pytest.raises(ContractPublicationError, match="refusing to overwrite"):
        write_forcing_published_snapshot("2.2.0", fake_repo)


def test_publish_rejects_unrelated_destination_and_handles_permissions_and_cleanup(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """Reject unrelated directories, use readable POSIX mode, and report backup cleanup failure."""
    unrelated = tmp_path / "site"
    unrelated.mkdir()
    (unrelated / "CNAME").write_text("suews.io\n", encoding="ascii")
    (unrelated / "index.html").write_text("<html>landing</html>\n", encoding="ascii")
    before_unrelated = _snapshot_tree(unrelated)

    with pytest.raises(ContractPublicationError, match="unrecognised entries"):
        publish_contracts_site(unrelated)
    assert _snapshot_tree(unrelated) == before_unrelated

    # POSIX directory mode should not remain private 0700 from mkdtemp.
    fresh = tmp_path / "published_contracts"
    publish_contracts_site(fresh, base_url="https://suews.io", validate=True)
    if os.name == "posix":
        assert stat.S_IMODE(fresh.stat().st_mode) != 0o700
        fresh.chmod(0o755)
        publish_contracts_site(fresh, base_url="https://suews.io", validate=True)
        assert stat.S_IMODE(fresh.stat().st_mode) == 0o755

    # Post-commit backup cleanup failure still reports committed publication + warning.
    real_rmtree = shutil.rmtree
    retained_paths: list[Path] = []

    def fail_on_backup(path, *args, **kwargs):
        p = Path(path)
        if ".backup-" in p.name:
            retained_paths.append(p)
            raise PermissionError("simulated cleanup failure")
        return real_rmtree(path, *args, **kwargs)

    monkeypatch.setattr(contracts_mod.shutil, "rmtree", fail_on_backup)
    runner = CliRunner()
    res = runner.invoke(
        cli,
        [
            "contracts",
            "publish",
            "--output-dir",
            str(fresh),
            "--base-url",
            "https://suews.io/preview/pr-9",
            "--format",
            "json",
        ],
    )
    assert res.exit_code == 0, res.output
    env = json.loads(res.output)
    assert env["status"] == "warning"
    assert env["warnings"]
    assert env["data"]["retained_backup"]
    # Destination reflects the newly committed base URL.
    committed_idx = json.loads((fresh / "index.json").read_text(encoding="ascii"))
    assert committed_idx["base_url"] == "https://suews.io/preview/pr-9"
    for p in retained_paths:
        if p.exists():
            real_rmtree(p)


def test_validator_rejects_nan_json_blank_root_html_and_missing_hist_links(
    tmp_path: Path,
) -> None:
    """Reject NaN JSON constants, blank root HTML, and missing schema/manifest links."""
    base = tmp_path / "contracts"
    publish_contracts_site(base, base_url="https://suews.io", validate=True)

    # 1. Blank root index.html fails discovery-link check.
    root_html = base / "index.html"
    orig_root_html = root_html.read_text(encoding="ascii")
    root_html.write_text("", encoding="ascii")
    with pytest.raises(
        ContractPublicationError, match="missing required discovery link"
    ):
        validate_published_contracts(base, base_url="https://suews.io")
    root_html.write_text(orig_root_html, encoding="ascii")

    # 2. Removing a historical manifest link from forcing/index.html fails validation.
    forcing_html = base / "forcing" / "index.html"
    orig_forcing_html = forcing_html.read_text(encoding="ascii")
    forcing_html.write_text(
        orig_forcing_html.replace(
            'href="1.0.0/manifest.json"', 'href="1.0.0/catalogue.json"'
        ),
        encoding="ascii",
    )
    with pytest.raises(ContractPublicationError, match=r"1\.0\.0/manifest\.json"):
        validate_published_contracts(base, base_url="https://suews.io")
    forcing_html.write_text(orig_forcing_html, encoding="ascii")

    # 3. Non-standard JSON constant (NaN) in a preserved catalogue fails strictly.
    nan_dir = base / "forcing" / "0.6.0"
    nan_dir.mkdir(parents=True)
    nan_cat = b'{"value":NaN}\n'
    schema_bytes = canonical_json_bytes({
        "$schema": "https://json-schema.org/draft/2020-12/schema",
        "type": "object",
        "properties": {"value": {"type": "number"}},
    })
    manifest_bytes = canonical_json_bytes({
        "files": {
            "catalogue.json": sha256_digest(nan_cat),
            "catalogue.schema.json": sha256_digest(schema_bytes),
        },
        "interface": "forcing",
        "version": "0.6.0",
    })
    (nan_dir / "catalogue.json").write_bytes(nan_cat)
    (nan_dir / "catalogue.schema.json").write_bytes(schema_bytes)
    (nan_dir / "manifest.json").write_bytes(manifest_bytes)
    with pytest.raises(ContractPublicationError, match="non-standard JSON constant"):
        publish_contracts_site(tmp_path / "out_nan", preserve_from=base)


def test_knowledge_pack_excludes_contract_artefacts(tmp_path: Path) -> None:
    """Generated snapshots under ``contract_artefacts/`` are excluded from knowledge packs."""
    assert "src/supy/data_model/contract_artefacts/" in EXCLUDED_GENERATED_ROOTS
    fake_repo = tmp_path / "repo"
    (fake_repo / "src/suews/src").mkdir(parents=True)
    (fake_repo / "src/suews/src/mod.f95").write_text(
        "MODULE m\nEND MODULE\n", encoding="utf-8"
    )
    (fake_repo / "src/suews_bridge/src").mkdir(parents=True)
    (fake_repo / "src/suews_bridge/src/lib.rs").write_text(
        "pub fn f() {}\n", encoding="utf-8"
    )
    (fake_repo / "src/supy/data_model/configuration").mkdir(parents=True)
    (fake_repo / "src/supy/data_model/configuration/version.py").write_text(
        'CURRENT_SCHEMA_VERSION = "2026.6"\n', encoding="utf-8"
    )
    (fake_repo / "src/supy/_version_scm.py").write_text(
        '__version__ = version = "2026.6.0"\n', encoding="utf-8"
    )
    snap = (
        fake_repo / "src/supy/data_model/contract_artefacts/forcing/1.0.0/manifest.json"
    )
    snap.parent.mkdir(parents=True)
    snap.write_text('{"interface":"forcing"}\n', encoding="utf-8")

    pack_dir = tmp_path / "pack"
    manifest = build_pack(fake_repo, pack_dir, git_sha="abc123")
    assert "src/supy/data_model/contract_artefacts/" in manifest["excluded_roots"]
    with gzip.open(pack_dir / "chunks.jsonl.gz", "rt", encoding="utf-8") as fh:
        paths = {json.loads(line)["repo_path"] for line in fh}
    assert (
        "src/supy/data_model/contract_artefacts/forcing/1.0.0/manifest.json"
        not in paths
    )


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
