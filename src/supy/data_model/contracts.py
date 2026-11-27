"""Discovery, export, publication, and validation for SUEWS data contracts.

This module owns the user-facing and site-publication projection of the
independently versioned forcing and output contracts. Immutable release
artefacts are loaded from packaged resources under
``supy.data_model.forcing.artefacts``, ``supy.data_model.contract_artefacts``,
and ``supy.data_model.output.artefacts``.
"""

from __future__ import annotations

from hashlib import sha256
from html import escape as html_escape
from html.parser import HTMLParser
import importlib.resources
import json
import os
from pathlib import Path
import re
import shutil
import stat
import sys
import tempfile
from typing import Any, Literal
from urllib.parse import urlsplit

from jsonschema import Draft202012Validator
from jsonschema.exceptions import SchemaError, ValidationError
from referencing import Registry
from referencing.exceptions import NoSuchResource, Unresolvable

from .forcing.registry import ForcingRegistry
from .forcing.version import CURRENT_FORCING_VERSION, FORCING_VERSIONS
from .output.version import CURRENT_OUTPUT_VERSION, OUTPUT_VERSIONS

ContractInterface = Literal["forcing", "output"]
ContractArtefact = Literal["catalogue", "schema", "manifest"]
ContractExportArtefact = Literal["catalogue", "schema", "manifest", "bundle"]

CONTRACT_INTERFACES: tuple[ContractInterface, ...] = ("forcing", "output")
BUNDLE_FILENAMES: tuple[str, ...] = (
    "catalogue.json",
    "catalogue.schema.json",
    "manifest.json",
)
CONTENT_FILENAMES: tuple[str, ...] = (
    "catalogue.json",
    "catalogue.schema.json",
)
ARTEFACT_FILENAMES: dict[ContractArtefact, str] = {
    "catalogue": "catalogue.json",
    "schema": "catalogue.schema.json",
    "manifest": "manifest.json",
}

DEFAULT_BASE_URL = "https://suews.io"
CONTRACTS_URL_PATH = "contracts"
JSON_SCHEMA_DRAFT_2020_12 = "https://json-schema.org/draft/2020-12/schema"
# ASCII, no-leading-zero MAJOR.MINOR.PATCH grammar matching
# scripts/lint/check_data_interface_version_history.py.
_SEMVER_RE = re.compile(
    r"(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)\Z",
    re.ASCII,
)


# Append-only trust anchor pinning the frozen manifest.json digest for every
# published forcing release under src/supy/data_model/contract_artefacts/forcing/.
# FORCING_VERSIONS continues to pin the canonical catalogue.json digest; this
# map independently authenticates the paired schema and manifest bytes.
FORCING_PUBLISHED_MANIFEST_DIGESTS: dict[str, str] = {
    "1.0.0": "sha256:2166fced59163156a0ebdbe43d2dba9b455ee1d2219b0a66261b042999d30cc9",
    "1.1.0": "sha256:2fb67f418895b136ffe40c139da1af427a47441bced2da3749fc805ec34bfb96",
    "1.2.0": "sha256:5d789b097e716e8e74630dbc73aebfb5e4b0840bdcdcef16f5518871494f0625",
    "2.0.0": "sha256:e8c7ad65314e00c38a4d278ac2b99648d2cbcc3f7ad8c1d602e1317dbb827974",
    "2.1.0": "sha256:2c228de51b7d108f3a414f1f9c2faa9b626a40af5659da226c408c9ff2d37ba1",
}

_ROOT_ALLOWED_ENTRIES = frozenset({"index.json", "index.html", *CONTRACT_INTERFACES})
_IFACE_ALLOWED_FILES = frozenset({"index.json", "index.html"})


def _forbid_external_ref(uri: str) -> Any:
    raise NoSuchResource(ref=uri)


_OFFLINE_REGISTRY: Registry[Any] = Registry(retrieve=_forbid_external_ref)


class ContractError(ValueError):
    """Raised when a requested contract interface, version, or artefact is invalid."""


class ContractPublicationError(RuntimeError):
    """Raised when contract publication or site validation fails."""


def canonical_json_bytes(value: Any, *, ensure_ascii: bool = True) -> bytes:
    """Return deterministic canonical UTF-8 JSON bytes with a trailing newline."""
    return (
        json.dumps(
            value,
            sort_keys=True,
            separators=(",", ":"),
            ensure_ascii=ensure_ascii,
            allow_nan=False,
        )
        + "\n"
    ).encode("utf-8")


def sha256_digest(content: bytes) -> str:
    """Return the lowercase ``sha256:<hex>`` digest for raw bytes."""
    return f"sha256:{sha256(content).hexdigest()}"


def _is_valid_semver(version: str) -> bool:
    return _SEMVER_RE.fullmatch(version) is not None


def _semver_key(version: str) -> tuple[int, int, int]:
    match = _SEMVER_RE.fullmatch(version)
    if match is None:
        raise ContractError(f"invalid SemVer version: {version!r}")
    return (int(match.group(1)), int(match.group(2)), int(match.group(3)))


def _normalise_base_url(base_url: str) -> str:
    if not isinstance(base_url, str):
        raise ContractError("base_url must be a string")
    stripped = base_url.strip()
    if not stripped:
        raise ContractError("base_url must be a non-empty URL")
    if any(ord(ch) < 0x21 or ord(ch) > 0x7E for ch in stripped):
        raise ContractError(
            f"base_url must contain only printable ASCII characters without spaces: {base_url!r}"
        )
    try:
        parsed = urlsplit(stripped)
        port = parsed.port
    except ValueError as exc:
        raise ContractError(f"invalid base_url {base_url!r}: {exc}") from exc
    if parsed.scheme not in ("http", "https"):
        raise ContractError(f"base_url must use http or https scheme: {base_url!r}")
    if not parsed.netloc or parsed.hostname is None:
        raise ContractError(f"base_url must include a valid host: {base_url!r}")
    if parsed.netloc.endswith(":"):
        raise ContractError(f"base_url has an empty port: {base_url!r}")
    if port is not None and not (1 <= port <= 65535):
        raise ContractError(f"base_url port out of range: {base_url!r}")
    if parsed.username is not None or parsed.password is not None:
        raise ContractError(f"base_url must not include credentials: {base_url!r}")
    if parsed.query or parsed.fragment:
        raise ContractError(
            f"base_url must not include a query string or fragment: {base_url!r}"
        )
    if "?" in stripped or "#" in stripped:
        raise ContractError(f"base_url must not include '?' or '#': {base_url!r}")
    clean_path = parsed.path.rstrip("/")
    return f"{parsed.scheme}://{parsed.netloc}{clean_path}"


def _validate_interface(interface: str) -> ContractInterface:
    if interface not in CONTRACT_INTERFACES:
        raise ContractError(
            f"unknown contract interface {interface!r}; "
            f"expected one of: {', '.join(CONTRACT_INTERFACES)}"
        )
    return interface  # type: ignore[return-value]


def forcing_contract_json_schema() -> dict[str, Any]:
    """Return the Draft 2020-12 JSON Schema for the current forcing contract."""
    schema = ForcingRegistry.model_json_schema()
    schema["$schema"] = JSON_SCHEMA_DRAFT_2020_12
    schema["title"] = "SUEWS forcing contract catalogue"
    return schema


def get_contract_versions(interface: ContractInterface) -> dict[str, str]:
    """Return the ordered ``version -> digest`` registry for one interface."""
    iface = _validate_interface(interface)
    if iface == "forcing":
        return dict(FORCING_VERSIONS)
    return dict(OUTPUT_VERSIONS)


def get_current_contract_version(interface: ContractInterface) -> str:
    """Return the current published version for one interface."""
    iface = _validate_interface(interface)
    version = CURRENT_FORCING_VERSION if iface == "forcing" else CURRENT_OUTPUT_VERSION
    if version is None:
        raise ContractError(f"the {iface} contract has no published version")
    return version


def _package_resource_root(package_name: str):
    pkg = sys.modules.get(package_name)
    if pkg is not None and getattr(pkg, "__spec__", None) is None:
        paths = list(getattr(pkg, "__path__", ()))
        if paths:
            return Path(paths[0])
    return importlib.resources.files(package_name)


def _resource_root(interface: ContractInterface):
    return _package_resource_root(f"{__package__}.{interface}")


def _contract_artefacts_root():
    if not __package__:
        raise ContractError("contracts module must be loaded within a package")
    return _package_resource_root(__package__).joinpath("contract_artefacts")


def load_contract_bundle_bytes(
    interface: ContractInterface,
    version: str | None = None,
) -> tuple[str, dict[str, bytes]]:
    """Load and authenticate the canonical three-file bundle for one release.

    Every returned bundle is verified against its registered digest, its
    manifest file digests, and its bundled JSON Schema before bytes are
    returned.
    """
    iface = _validate_interface(interface)
    versions = get_contract_versions(iface)
    resolved = version if version is not None else get_current_contract_version(iface)
    if resolved not in versions:
        available = ", ".join(versions)
        raise ContractError(
            f"unknown {iface} contract version {resolved!r}; available: {available}"
        )

    expected_digest = versions[resolved]
    root = _resource_root(iface)
    bundle: dict[str, bytes] = {}

    if iface == "forcing":
        cat_resource = root.joinpath(f"artefacts/{resolved}.json")
        if not cat_resource.is_file():
            raise ContractError(
                f"packaged forcing artefact is missing for version {resolved}"
            )
        bundle["catalogue.json"] = cat_resource.read_bytes()

        published_root = _contract_artefacts_root().joinpath(f"forcing/{resolved}")
        for filename in ("catalogue.schema.json", "manifest.json"):
            resource = published_root.joinpath(filename)
            if not resource.is_file():
                raise ContractError(
                    f"packaged forcing contract snapshot is missing: {resolved}/{filename}"
                )
            bundle[filename] = resource.read_bytes()
    else:
        for filename in BUNDLE_FILENAMES:
            resource = root.joinpath(f"artefacts/{resolved}/{filename}")
            if not resource.is_file():
                raise ContractError(
                    f"packaged output artefact is missing: {resolved}/{filename}"
                )
            bundle[filename] = resource.read_bytes()

    expected_manifest_digest: str | None = None
    if iface == "forcing":
        expected_manifest_digest = FORCING_PUBLISHED_MANIFEST_DIGESTS.get(resolved)
        if expected_manifest_digest is None:
            raise ContractError(
                f"forcing/{resolved}: missing pinned manifest digest in "
                "FORCING_PUBLISHED_MANIFEST_DIGESTS"
            )
    try:
        _validate_bundle_content(
            iface,
            resolved,
            bundle,
            expected_registry_digest=expected_digest,
            expected_manifest_digest=expected_manifest_digest,
        )
    except ContractPublicationError as exc:
        raise ContractError(str(exc)) from exc
    return resolved, bundle


def write_forcing_published_snapshot(
    version: str,
    project_root: Path,
) -> str:
    """Write overwrite-protected schema and manifest snapshots for a forcing release.

    Reads the canonical catalogue at
    ``src/supy/data_model/forcing/artefacts/<version>.json`` and writes
    ``catalogue.schema.json`` and ``manifest.json`` under
    ``src/supy/data_model/contract_artefacts/forcing/<version>/``. Refuses to
    overwrite differing bytes in an existing snapshot directory. Returns the
    pinned ``manifest.json`` SHA-256 digest.
    """
    if not _is_valid_semver(version):
        raise ContractError(f"invalid SemVer version: {version!r}")
    cat_path = project_root / f"src/supy/data_model/forcing/artefacts/{version}.json"
    if not cat_path.is_file():
        raise ContractPublicationError(
            f"missing forcing catalogue artefact: {cat_path}"
        )
    cat_bytes = cat_path.read_bytes()
    schema_bytes = canonical_json_bytes(forcing_contract_json_schema())
    manifest_bytes = canonical_json_bytes({
        "files": {
            "catalogue.json": sha256_digest(cat_bytes),
            "catalogue.schema.json": sha256_digest(schema_bytes),
        },
        "interface": "forcing",
        "version": version,
    })
    bundle = {
        "catalogue.json": cat_bytes,
        "catalogue.schema.json": schema_bytes,
        "manifest.json": manifest_bytes,
    }
    _validate_bundle_content("forcing", version, bundle)

    target_dir = (
        project_root / f"src/supy/data_model/contract_artefacts/forcing/{version}"
    )
    snapshot_files = {
        "catalogue.schema.json": schema_bytes,
        "manifest.json": manifest_bytes,
    }
    if target_dir.exists() or target_dir.is_symlink():
        _reject_symlinks_in_tree(target_dir, f"forcing/{version} snapshot")
        if not target_dir.is_dir():
            raise ContractPublicationError(
                f"forcing/{version} snapshot path is not a directory: {target_dir}"
            )
        existing_names = {p.name for p in target_dir.iterdir()}
        if existing_names != set(snapshot_files) or any(
            not (target_dir / name).is_file()
            or (target_dir / name).read_bytes() != content
            for name, content in snapshot_files.items()
        ):
            raise ContractPublicationError(
                f"refusing to overwrite differing bytes in {target_dir}"
            )
    else:
        target_dir.parent.mkdir(parents=True, exist_ok=True)
        staging_dir = Path(
            tempfile.mkdtemp(prefix=f".{version}.staging-", dir=target_dir.parent)
        )
        try:
            for name, content in snapshot_files.items():
                (staging_dir / name).write_bytes(content)
            _apply_published_dir_mode(staging_dir, target_dir)
            staging_dir.rename(target_dir)
        finally:
            if staging_dir.exists():
                shutil.rmtree(staging_dir)
    return sha256_digest(manifest_bytes)


def load_contract_artefact_bytes(
    interface: ContractInterface,
    artefact: ContractArtefact = "catalogue",
    version: str | None = None,
) -> tuple[str, str, bytes]:
    """Load one canonical contract artefact as raw UTF-8 JSON bytes."""
    if artefact not in ARTEFACT_FILENAMES:
        raise ContractError(
            f"unknown contract artefact {artefact!r}; "
            f"expected one of: {', '.join(ARTEFACT_FILENAMES)}"
        )
    resolved, bundle = load_contract_bundle_bytes(interface, version=version)
    filename = ARTEFACT_FILENAMES[artefact]
    return resolved, filename, bundle[filename]


def get_contract_info(
    interface: ContractInterface | None = None,
    *,
    base_url: str = DEFAULT_BASE_URL,
) -> dict[str, Any]:
    """Return discovery metadata for one or both data contracts."""
    base = _normalise_base_url(base_url)
    selected: tuple[ContractInterface, ...] = (
        (_validate_interface(interface),)
        if interface is not None
        else CONTRACT_INTERFACES
    )
    interfaces_info: dict[str, Any] = {}
    for iface in selected:
        versions = get_contract_versions(iface)
        current = get_current_contract_version(iface)
        entries = [
            {
                "version": ver,
                "digest": digest,
                "is_current": ver == current,
                "urls": {
                    art: f"{base}/{CONTRACTS_URL_PATH}/{iface}/{ver}/{fname}"
                    for art, fname in ARTEFACT_FILENAMES.items()
                },
            }
            for ver, digest in versions.items()
        ]
        interfaces_info[iface] = {
            "interface": iface,
            "current_version": current,
            "digest_target": "catalogue.json"
            if iface == "forcing"
            else "manifest.json",
            "version_count": len(entries),
            "versions": entries,
            "index_url": f"{base}/{CONTRACTS_URL_PATH}/{iface}/index.json",
            "latest_urls": {
                art: f"{base}/{CONTRACTS_URL_PATH}/{iface}/latest/{fname}"
                for art, fname in ARTEFACT_FILENAMES.items()
            },
        }
    return {
        "base_url": base,
        "contracts_url": f"{base}/{CONTRACTS_URL_PATH}/",
        "index_url": f"{base}/{CONTRACTS_URL_PATH}/index.json",
        "interfaces": interfaces_info,
    }


def export_contract(
    interface: ContractInterface,
    output_path: Path,
    *,
    version: str | None = None,
    artefact: ContractExportArtefact = "catalogue",
) -> dict[str, Any]:
    """Export one contract artefact file or a complete three-file bundle to disk."""
    iface = _validate_interface(interface)
    if artefact == "bundle":
        resolved, bundle = load_contract_bundle_bytes(iface, version=version)
        output_path.mkdir(parents=True, exist_ok=True)
        written: list[dict[str, str]] = []
        for filename in BUNDLE_FILENAMES:
            target = output_path / filename
            data = bundle[filename]
            target.write_bytes(data)
            written.append({
                "file": filename,
                "path": str(target),
                "digest": sha256_digest(data),
            })
        return {
            "interface": iface,
            "version": resolved,
            "artefact": "bundle",
            "output_dir": str(output_path),
            "files": written,
        }

    resolved, filename, data = load_contract_artefact_bytes(
        iface,
        artefact=artefact,
        version=version,
    )
    output_path.parent.mkdir(parents=True, exist_ok=True)
    output_path.write_bytes(data)
    return {
        "interface": iface,
        "version": resolved,
        "artefact": artefact,
        "file": filename,
        "path": str(output_path),
        "digest": sha256_digest(data),
    }


def _reject_json_constant(token: str) -> Any:
    raise ValueError(f"non-standard JSON constant {token!r} is not allowed")


def _read_json_bytes(content: bytes, label: str) -> Any:
    try:
        return json.loads(
            content.decode("utf-8"),
            parse_constant=_reject_json_constant,
        )
    except (UnicodeDecodeError, json.JSONDecodeError, ValueError) as exc:
        raise ContractPublicationError(f"{label}: invalid JSON: {exc}") from exc


def _validate_bundle_content(
    interface: ContractInterface,
    version: str,
    bundle: dict[str, bytes],
    *,
    expected_registry_digest: str | None = None,
    expected_manifest_digest: str | None = None,
) -> dict[str, Any]:
    """Validate one release bundle's manifest, digests, and offline JSON Schema."""
    missing = [name for name in BUNDLE_FILENAMES if name not in bundle]
    if missing:
        raise ContractPublicationError(
            f"{interface}/{version}: missing bundle file(s): {missing}"
        )

    manifest = _read_json_bytes(
        bundle["manifest.json"], f"{interface}/{version}/manifest.json"
    )
    if not isinstance(manifest, dict):
        raise ContractPublicationError(
            f"{interface}/{version}/manifest.json: manifest must be a JSON object"
        )
    expected_manifest = {
        "files": {name: sha256_digest(bundle[name]) for name in CONTENT_FILENAMES},
        "interface": interface,
        "version": version,
    }
    if manifest != expected_manifest:
        raise ContractPublicationError(
            f"{interface}/{version}/manifest.json: metadata or file digests do not match"
        )

    if expected_registry_digest is not None:
        if interface == "forcing":
            actual_digest = sha256_digest(bundle["catalogue.json"])
            if actual_digest != expected_registry_digest:
                raise ContractPublicationError(
                    f"forcing/{version}/catalogue.json: digest {actual_digest} "
                    f"does not match FORCING_VERSIONS ({expected_registry_digest})"
                )
        else:
            actual_digest = sha256_digest(bundle["manifest.json"])
            if actual_digest != expected_registry_digest:
                raise ContractPublicationError(
                    f"output/{version}/manifest.json: digest {actual_digest} "
                    f"does not match OUTPUT_VERSIONS ({expected_registry_digest})"
                )

    if expected_manifest_digest is not None:
        actual_manifest_digest = sha256_digest(bundle["manifest.json"])
        if actual_manifest_digest != expected_manifest_digest:
            raise ContractPublicationError(
                f"{interface}/{version}/manifest.json: digest {actual_manifest_digest} "
                f"does not match pinned digest ({expected_manifest_digest})"
            )

    schema = _read_json_bytes(
        bundle["catalogue.schema.json"],
        f"{interface}/{version}/catalogue.schema.json",
    )
    catalogue = _read_json_bytes(
        bundle["catalogue.json"],
        f"{interface}/{version}/catalogue.json",
    )
    if not isinstance(schema, dict):
        raise ContractPublicationError(
            f"{interface}/{version}/catalogue.schema.json: schema must be a JSON object"
        )
    try:
        Draft202012Validator.check_schema(schema)
        Draft202012Validator(schema, registry=_OFFLINE_REGISTRY).validate(catalogue)
    except (SchemaError, ValidationError, Unresolvable, NoSuchResource) as exc:
        detail = getattr(exc, "message", str(exc))
        raise ContractPublicationError(
            f"{interface}/{version}: catalogue fails schema validation: {detail}"
        ) from exc

    return manifest


def _reject_symlinks_in_tree(directory: Path, label: str) -> None:
    """Reject a symlink root or any descendant symlink without following links."""
    if directory.is_symlink():
        raise ContractPublicationError(f"{label}: refuses symlink target: {directory}")
    if not directory.is_dir():
        return
    for root_str, dirs, files in os.walk(directory, followlinks=False):
        root_path = Path(root_str)
        for name in (*dirs, *files):
            candidate = root_path / name
            if candidate.is_symlink():
                raise ContractPublicationError(
                    f"{label}: refuses symlink inside tree: {candidate}"
                )


def _load_bundle_from_dir(
    release_dir: Path, interface: ContractInterface, version: str
) -> dict[str, bytes]:
    if release_dir.is_symlink():
        raise ContractPublicationError(
            f"{interface}/{version}: release directory must not be a symlink"
        )
    entries = list(release_dir.iterdir())
    symlinks = [p.name for p in entries if p.is_symlink()]
    if symlinks:
        raise ContractPublicationError(
            f"{interface}/{version}: release directory contains symlinks: {sorted(symlinks)}"
        )
    found = {p.name for p in entries if p.is_file()}
    expected = set(BUNDLE_FILENAMES)
    extra_dirs = [p.name for p in entries if not p.is_file()]
    if found != expected or extra_dirs:
        raise ContractPublicationError(
            f"{interface}/{version}: directory must contain exactly "
            f"{sorted(expected)}; found files={sorted(found)}, non-files={sorted(extra_dirs)}"
        )
    return {name: (release_dir / name).read_bytes() for name in BUNDLE_FILENAMES}


def _extract_preserved_index_inventory(
    source_root: Path,
    interface: ContractInterface,
) -> dict[str, dict[str, Any]]:
    """Parse and cross-check root and interface index inventories in a preserved source."""
    root_idx_path = source_root / "index.json"
    iface_idx_path = source_root / interface / "index.json"
    if not root_idx_path.is_file():
        raise ContractPublicationError(
            f"{interface}: preserved source missing root index.json: {source_root}"
        )
    if not iface_idx_path.is_file():
        raise ContractPublicationError(
            f"{interface}: preserved source missing {interface}/index.json: {source_root}"
        )

    root_idx = _read_json_bytes(
        root_idx_path.read_bytes(), f"preserved {source_root.name}/index.json"
    )
    if not isinstance(root_idx, dict) or not isinstance(
        root_idx.get("interfaces"), dict
    ):
        raise ContractPublicationError(
            f"{interface}: preserved root index.json has invalid structure in {source_root}"
        )
    root_iface = root_idx["interfaces"].get(interface)
    if not isinstance(root_iface, dict) or not isinstance(
        root_iface.get("versions"), list
    ):
        raise ContractPublicationError(
            f"{interface}: preserved root index.json missing versions list in {source_root}"
        )
    root_versions = root_iface["versions"]
    if any(not isinstance(v, str) or not _is_valid_semver(v) for v in root_versions):
        raise ContractPublicationError(
            f"{interface}: preserved root index.json contains invalid version name in {source_root}"
        )
    if len(set(root_versions)) != len(root_versions):
        raise ContractPublicationError(
            f"{interface}: preserved root index.json contains duplicate versions in {source_root}"
        )

    iface_idx = _read_json_bytes(
        iface_idx_path.read_bytes(),
        f"preserved {source_root.name}/{interface}/index.json",
    )
    if (
        not isinstance(iface_idx, dict)
        or iface_idx.get("interface") != interface
        or not isinstance(iface_idx.get("versions"), list)
    ):
        raise ContractPublicationError(
            f"{interface}: preserved {interface}/index.json has invalid structure in {source_root}"
        )

    indexed: dict[str, dict[str, Any]] = {}
    iface_versions: list[str] = []
    for entry in iface_idx["versions"]:
        if not isinstance(entry, dict) or not isinstance(entry.get("version"), str):
            raise ContractPublicationError(
                f"{interface}: preserved {interface}/index.json has invalid version entry"
            )
        ver = entry["version"]
        if not _is_valid_semver(ver) or ver in indexed:
            raise ContractPublicationError(
                f"{interface}: preserved {interface}/index.json has invalid or duplicate version {ver!r}"
            )
        digests = entry.get("digests")
        if (
            not isinstance(digests, dict)
            or set(digests) != set(BUNDLE_FILENAMES)
            or any(not isinstance(v, str) for v in digests.values())
            or not isinstance(entry.get("manifest_digest"), str)
            or not isinstance(entry.get("contract_digest"), str)
        ):
            raise ContractPublicationError(
                f"{interface}/{ver}: preserved {interface}/index.json has incomplete digest metadata"
            )
        indexed[ver] = entry
        iface_versions.append(ver)

    if iface_versions != root_versions:
        raise ContractPublicationError(
            f"{interface}: preserved root index.json versions {root_versions!r} "
            f"disagree with {interface}/index.json versions {iface_versions!r}"
        )
    return indexed


def _verify_bundle_against_preserved_index(
    interface: ContractInterface,
    version: str,
    bundle: dict[str, bytes],
    index_entry: dict[str, Any],
) -> None:
    """Ensure a preserved release bundle matches the digests recorded in its prior index."""
    actual_digests = {name: sha256_digest(bundle[name]) for name in BUNDLE_FILENAMES}
    actual_contract = (
        actual_digests["catalogue.json"]
        if interface == "forcing"
        else actual_digests["manifest.json"]
    )
    if (
        index_entry["digests"] != actual_digests
        or index_entry["manifest_digest"] != actual_digests["manifest.json"]
        or index_entry["contract_digest"] != actual_contract
    ):
        raise ContractPublicationError(
            f"{interface}/{version}: preserved bundle digests disagree with prior "
            f"{interface}/index.json"
        )


def _collect_preserved_bundles(
    interface: ContractInterface,
    sources: list[Path],
    packaged_versions: dict[str, str],
) -> dict[str, dict[str, bytes]]:
    """Collect and verify historical version bundles from existing deployments."""
    preserved: dict[str, dict[str, bytes]] = {}
    for source_root in sources:
        indexed_entries = _extract_preserved_index_inventory(source_root, interface)
        iface_dir = source_root / interface
        if not iface_dir.is_dir():
            raise ContractPublicationError(
                f"{interface}: preserved source missing {interface}/ directory: {source_root}"
            )
        disk_versions: set[str] = set()
        for child in sorted(iface_dir.iterdir()):
            if child.is_symlink():
                raise ContractPublicationError(
                    f"{interface}/{child.name}: refuses symlink in preserved source"
                )
            if not child.is_dir() or child.name == "latest":
                continue
            if not _is_valid_semver(child.name):
                raise ContractPublicationError(
                    f"{interface}/{child.name}: invalid release directory name in preserved source"
                )
            disk_versions.add(child.name)

        missing_on_disk = sorted(set(indexed_entries) - disk_versions)
        if missing_on_disk:
            raise ContractPublicationError(
                f"{interface}: preserved source {source_root} is missing indexed "
                f"release directory(ies): {missing_on_disk}"
            )
        unindexed_on_disk = sorted(disk_versions - set(indexed_entries))
        if unindexed_on_disk:
            raise ContractPublicationError(
                f"{interface}: preserved source {source_root} contains unindexed "
                f"release directory(ies): {unindexed_on_disk}"
            )

        for ver in sorted(disk_versions, key=_semver_key):
            bundle = _load_bundle_from_dir(iface_dir / ver, interface, ver)
            _validate_bundle_content(
                interface,
                ver,
                bundle,
                expected_registry_digest=packaged_versions.get(ver),
                expected_manifest_digest=(
                    FORCING_PUBLISHED_MANIFEST_DIGESTS.get(ver)
                    if interface == "forcing"
                    else None
                ),
            )
            _verify_bundle_against_preserved_index(
                interface, ver, bundle, indexed_entries[ver]
            )
            if ver in preserved and preserved[ver] != bundle:
                raise ContractPublicationError(
                    f"{interface}/{ver}: conflicting historical bytes across preserved sources"
                )
            preserved[ver] = bundle
    return preserved


def _render_html_page(title: str, subtitle: str, body_html: str) -> str:
    """Render a self-contained, accessible, ASCII-only HTML page."""
    return (
        "<!DOCTYPE html>\n"
        '<html lang="en">\n'
        "<head>\n"
        '  <meta charset="UTF-8">\n'
        '  <meta name="viewport" content="width=device-width, initial-scale=1.0">\n'
        f"  <title>{html_escape(title)}</title>\n"
        "  <style>\n"
        "    :root {\n"
        "      --bg: #f8fafc;\n"
        "      --surface: #ffffff;\n"
        "      --text: #0f172a;\n"
        "      --muted: #475569;\n"
        "      --border: #cbd5e1;\n"
        "      --accent: #0077b6;\n"
        "      --accent-soft: #e0f2fe;\n"
        "      --code-bg: #f1f5f9;\n"
        "    }\n"
        "    @media (prefers-color-scheme: dark) {\n"
        "      :root {\n"
        "        --bg: #0f172a;\n"
        "        --surface: #1e293b;\n"
        "        --text: #f1f5f9;\n"
        "        --muted: #94a3b8;\n"
        "        --border: #334155;\n"
        "        --accent: #38bdf8;\n"
        "        --accent-soft: #0c4a6e;\n"
        "        --code-bg: #0f172a;\n"
        "      }\n"
        "    }\n"
        "    @media (prefers-reduced-motion: reduce) {\n"
        "      * { transition: none !important; animation: none !important; }\n"
        "    }\n"
        "    body {\n"
        "      font-family: system-ui, -apple-system, sans-serif;\n"
        "      background: var(--bg);\n"
        "      color: var(--text);\n"
        "      margin: 0;\n"
        "      padding: 2rem 1.25rem;\n"
        "      line-height: 1.55;\n"
        "    }\n"
        "    main { max-width: 960px; margin: 0 auto; }\n"
        "    h1 { margin: 0 0 0.5rem; font-size: 1.75rem; }\n"
        "    h2 { margin: 2rem 0 0.75rem; font-size: 1.25rem; }\n"
        "    p.subtitle { color: var(--muted); margin: 0 0 1.5rem; }\n"
        "    a { color: var(--accent); }\n"
        "    .card {\n"
        "      background: var(--surface);\n"
        "      border: 1px solid var(--border);\n"
        "      border-radius: 8px;\n"
        "      padding: 1rem 1.25rem;\n"
        "      margin-bottom: 1rem;\n"
        "    }\n"
        "    table { width: 100%; border-collapse: collapse; background: var(--surface); }\n"
        "    th, td {\n"
        "      border: 1px solid var(--border);\n"
        "      padding: 0.55rem 0.75rem;\n"
        "      text-align: left;\n"
        "      vertical-align: top;\n"
        "      font-size: 0.92rem;\n"
        "    }\n"
        "    th { background: var(--code-bg); }\n"
        "    code, pre {\n"
        "      font-family: ui-monospace, SFMono-Regular, Menlo, monospace;\n"
        "      background: var(--code-bg);\n"
        "      border-radius: 4px;\n"
        "    }\n"
        "    code { padding: 0.1rem 0.3rem; font-size: 0.85em; }\n"
        "    pre { padding: 0.75rem 1rem; overflow-x: auto; }\n"
        "    .badge {\n"
        "      display: inline-block;\n"
        "      padding: 0.1rem 0.45rem;\n"
        "      border-radius: 999px;\n"
        "      background: var(--accent-soft);\n"
        "      font-size: 0.78rem;\n"
        "      font-weight: 600;\n"
        "    }\n"
        "    nav.crumb { margin-bottom: 1rem; font-size: 0.9rem; color: var(--muted); }\n"
        "  </style>\n"
        "</head>\n"
        "<body>\n"
        "  <main>\n"
        f"{body_html}\n"
        "  </main>\n"
        "</body>\n"
        "</html>\n"
    )


def _render_interface_index_html(
    interface: ContractInterface,
    index_data: dict[str, Any],
) -> str:
    label = "Forcing" if interface == "forcing" else "Output"
    current = index_data["current_version"]
    rows: list[str] = []
    for entry in reversed(index_data["versions"]):
        ver = entry["version"]
        badges = []
        if entry["is_current"]:
            badges.append('<span class="badge">latest</span>')
        if entry.get("preserved"):
            badges.append('<span class="badge">preserved</span>')
        badge_html = (" " + " ".join(badges)) if badges else ""
        digest_short = entry["contract_digest"].removeprefix("sha256:")[:12]
        rows.append(
            "      <tr>\n"
            f"        <td><code>{html_escape(ver)}</code>{badge_html}</td>\n"
            f'        <td><a href="{html_escape(ver)}/catalogue.json">catalogue.json</a></td>\n'
            f'        <td><a href="{html_escape(ver)}/catalogue.schema.json">catalogue.schema.json</a></td>\n'
            f'        <td><a href="{html_escape(ver)}/manifest.json">manifest.json</a></td>\n'
            f'        <td><code title="{html_escape(entry["contract_digest"])}">sha256:{html_escape(digest_short)}...</code></td>\n'
            "      </tr>"
        )
    table_rows = "\n".join(rows)
    body = (
        '    <nav class="crumb"><a href="../index.html">SUEWS Data Contracts</a> / '
        f"{label}</nav>\n"
        f"    <h1>SUEWS {label} Data Contract</h1>\n"
        f'    <p class="subtitle">Versioned, immutable machine-readable {interface} '
        "contract artefacts and moving <code>latest</code> aliases.</p>\n"
        '    <section class="card">\n'
        f"      <h2>Latest Release (<code>{html_escape(current)}</code>)</h2>\n"
        "      <ul>\n"
        '        <li><a href="latest/catalogue.json">latest/catalogue.json</a></li>\n'
        '        <li><a href="latest/catalogue.schema.json">latest/catalogue.schema.json</a></li>\n'
        '        <li><a href="latest/manifest.json">latest/manifest.json</a></li>\n'
        '        <li><a href="index.json">index.json</a> (machine-readable release index)</li>\n'
        "      </ul>\n"
        "      <pre><code>"
        f"suews contracts info {interface}\n"
        f"suews contracts export {interface} -o {interface}-catalogue.json\n"
        f"suews contracts export {interface} --artefact bundle -o ./{interface}-contract"
        "</code></pre>\n"
        "    </section>\n"
        "    <h2>Published Versions</h2>\n"
        "    <table>\n"
        "      <thead>\n"
        "        <tr>\n"
        "          <th>Version</th>\n"
        "          <th>Catalogue</th>\n"
        "          <th>JSON Schema</th>\n"
        "          <th>Manifest</th>\n"
        "          <th>Contract Digest</th>\n"
        "        </tr>\n"
        "      </thead>\n"
        "      <tbody>\n"
        f"{table_rows}\n"
        "      </tbody>\n"
        "    </table>"
    )
    return _render_html_page(
        f"SUEWS {label} Data Contract",
        f"Versioned SUEWS {interface} data contract artefacts.",
        body,
    )


def _render_root_index_html(root_index: dict[str, Any]) -> str:
    cards: list[str] = []
    for iface in CONTRACT_INTERFACES:
        info = root_index["interfaces"][iface]
        label = "Forcing" if iface == "forcing" else "Output"
        current = info["current_version"]
        count = info["version_count"]
        cards.append(
            '    <section class="card">\n'
            f'      <h2><a href="{iface}/index.html">{label} Contract</a> '
            f'<span class="badge">v{html_escape(current)}</span></h2>\n'
            f"      <p>{count} published version(s). "
            f'Machine-readable index: <a href="{iface}/index.json">{iface}/index.json</a>.</p>\n'
            "      <ul>\n"
            f'        <li><a href="{iface}/latest/catalogue.json">{iface}/latest/catalogue.json</a></li>\n'
            f'        <li><a href="{iface}/latest/catalogue.schema.json">{iface}/latest/catalogue.schema.json</a></li>\n'
            f'        <li><a href="{iface}/latest/manifest.json">{iface}/latest/manifest.json</a></li>\n'
            "      </ul>\n"
            "    </section>"
        )
    cards_html = "\n".join(cards)
    body = (
        '    <nav class="crumb"><a href="https://suews.io/">suews.io</a> / Data Contracts</nav>\n'
        "    <h1>SUEWS Data Contracts</h1>\n"
        '    <p class="subtitle">Stable versioned URLs and moving <code>latest</code> '
        "aliases for the SUEWS forcing and output data interfaces. "
        'Top-level index: <a href="index.json">index.json</a>.</p>\n'
        f"{cards_html}\n"
        '    <section class="card">\n'
        "      <h2>CLI Discovery and Export</h2>\n"
        "      <pre><code>suews contracts info --format json\n"
        "suews contracts show forcing --format json\n"
        "suews contracts export output --artefact bundle -o ./output-contract</code></pre>\n"
        "    </section>"
    )
    return _render_html_page(
        "SUEWS Data Contracts",
        "Versioned SUEWS forcing and output data contracts.",
        body,
    )


def _resolve_interface_bundles(
    iface: ContractInterface,
    preserve_sources: list[Path],
) -> tuple[dict[str, dict[str, bytes]], list[str]]:
    """Merge packaged and preserved release bundles while enforcing strict immutability."""
    packaged_versions = get_contract_versions(iface)
    preserved_bundles = _collect_preserved_bundles(
        iface, preserve_sources, packaged_versions
    )
    final_bundles: dict[str, dict[str, bytes]] = {}
    preserved_only: list[str] = []

    for ver in packaged_versions:
        _, packaged_bundle = load_contract_bundle_bytes(iface, version=ver)
        existing = preserved_bundles.get(ver)
        if existing is not None and existing != packaged_bundle:
            raise ContractPublicationError(
                f"{iface}/{ver}: refusing to overwrite differing published bundle"
            )
        final_bundles[ver] = packaged_bundle

    for ver, bundle in preserved_bundles.items():
        if ver not in final_bundles:
            final_bundles[ver] = bundle
            preserved_only.append(ver)

    return final_bundles, sorted(preserved_only, key=_semver_key)


def _write_interface_dir(
    iface: ContractInterface,
    staging_dir: Path,
    base: str,
    final_bundles: dict[str, dict[str, bytes]],
    preserved_only: list[str],
) -> dict[str, Any]:
    """Write one interface's versioned bundles, ``latest`` mirror, and indices."""
    current_version = get_current_contract_version(iface)
    preserved_set = set(preserved_only)
    ordered_versions = sorted(final_bundles, key=_semver_key)

    iface_dir = staging_dir / iface
    iface_dir.mkdir(parents=True, exist_ok=True)

    version_entries: list[dict[str, Any]] = []
    for ver in ordered_versions:
        bundle = final_bundles[ver]
        ver_dir = iface_dir / ver
        ver_dir.mkdir(parents=True, exist_ok=True)
        for filename in BUNDLE_FILENAMES:
            (ver_dir / filename).write_bytes(bundle[filename])

        digests = {name: sha256_digest(bundle[name]) for name in BUNDLE_FILENAMES}
        contract_digest = (
            digests["catalogue.json"]
            if iface == "forcing"
            else digests["manifest.json"]
        )
        version_entries.append({
            "version": ver,
            "is_current": ver == current_version,
            "preserved": ver in preserved_set,
            "contract_digest": contract_digest,
            "manifest_digest": digests["manifest.json"],
            "digests": digests,
            "paths": {
                art: f"{ver}/{fname}" for art, fname in ARTEFACT_FILENAMES.items()
            },
            "urls": {
                art: f"{base}/{CONTRACTS_URL_PATH}/{iface}/{ver}/{fname}"
                for art, fname in ARTEFACT_FILENAMES.items()
            },
        })

    latest_dir = iface_dir / "latest"
    latest_dir.mkdir(parents=True, exist_ok=True)
    current_bundle = final_bundles[current_version]
    for filename in BUNDLE_FILENAMES:
        (latest_dir / filename).write_bytes(current_bundle[filename])

    current_digests = {
        name: sha256_digest(current_bundle[name]) for name in BUNDLE_FILENAMES
    }
    iface_index = {
        "interface": iface,
        "base_url": base,
        "current_version": current_version,
        "version_count": len(version_entries),
        "latest": {
            "version": current_version,
            "digests": current_digests,
            "paths": {
                art: f"latest/{fname}" for art, fname in ARTEFACT_FILENAMES.items()
            },
            "urls": {
                art: f"{base}/{CONTRACTS_URL_PATH}/{iface}/latest/{fname}"
                for art, fname in ARTEFACT_FILENAMES.items()
            },
        },
        "versions": version_entries,
    }
    (iface_dir / "index.json").write_bytes(canonical_json_bytes(iface_index))
    (iface_dir / "index.html").write_text(
        _render_interface_index_html(iface, iface_index),
        encoding="ascii",
    )

    return {
        "interface": iface,
        "current_version": current_version,
        "version_count": len(version_entries),
        "index_path": f"{iface}/index.json",
        "html_path": f"{iface}/index.html",
        "index_url": f"{base}/{CONTRACTS_URL_PATH}/{iface}/index.json",
        "html_url": f"{base}/{CONTRACTS_URL_PATH}/{iface}/index.html",
        "latest_paths": {
            art: f"{iface}/latest/{fname}" for art, fname in ARTEFACT_FILENAMES.items()
        },
        "latest_urls": {
            art: f"{base}/{CONTRACTS_URL_PATH}/{iface}/latest/{fname}"
            for art, fname in ARTEFACT_FILENAMES.items()
        },
        "versions": [entry["version"] for entry in version_entries],
    }


def _assert_owned_bundle_dir(bundle_dir: Path, rel_label: str, label: str) -> None:
    """Verify a replaceable bundle directory contains only permitted contract files."""
    unexpected = [
        child.name
        for child in bundle_dir.iterdir()
        if child.is_symlink()
        or not child.is_file()
        or child.name not in BUNDLE_FILENAMES
    ]
    if unexpected:
        raise ContractPublicationError(
            f"{label}: unrecognised entries inside {rel_label}: {sorted(unexpected)}"
        )


def _assert_owned_interface_dir(
    iface_dir: Path, iface: ContractInterface, directory: Path, label: str
) -> None:
    """Verify one interface directory under ``directory`` contains only owned entries."""
    if iface_dir.is_symlink() or not iface_dir.is_dir():
        raise ContractPublicationError(f"{label}: expected directory at {iface_dir}")
    for child in iface_dir.iterdir():
        if child.is_symlink():
            raise ContractPublicationError(
                f"{label}: refuses symlink inside tree: {child}"
            )
        if child.name in _IFACE_ALLOWED_FILES:
            if not child.is_file():
                raise ContractPublicationError(
                    f"{label}: expected regular file at {child}"
                )
        elif child.name == "latest" or _is_valid_semver(child.name):
            if not child.is_dir():
                raise ContractPublicationError(
                    f"{label}: expected directory at {child}"
                )
            _assert_owned_bundle_dir(child, f"{iface}/{child.name}", label)
        else:
            raise ContractPublicationError(
                f"{label}: unrecognised entry {iface}/{child.name} in {directory}"
            )


def _assert_owned_contracts_root(directory: Path, label: str) -> None:
    """Verify a non-empty existing directory belongs exclusively to contract publication."""
    entries = list(directory.iterdir())
    if not entries:
        return
    names = {p.name for p in entries}
    unexpected = sorted(names - _ROOT_ALLOWED_ENTRIES)
    if unexpected:
        raise ContractPublicationError(
            f"{label}: refusing to replace directory containing unrecognised entries: "
            f"{unexpected} (in {directory})"
        )
    for root_file in _IFACE_ALLOWED_FILES:
        candidate = directory / root_file
        if candidate.exists() and (candidate.is_symlink() or not candidate.is_file()):
            raise ContractPublicationError(
                f"{label}: expected regular file at {candidate}"
            )
    root_idx_path = directory / "index.json"
    if not root_idx_path.is_file():
        raise ContractPublicationError(
            f"{label}: existing non-empty directory does not look like a contracts root "
            f"(missing index.json): {directory}"
        )
    root_idx = _read_json_bytes(root_idx_path.read_bytes(), f"{label}/index.json")
    if (
        not isinstance(root_idx, dict)
        or "contracts_url" not in root_idx
        or not isinstance(root_idx.get("interfaces"), dict)
    ):
        raise ContractPublicationError(
            f"{label}: existing index.json does not belong to SUEWS contracts"
        )
    for iface in CONTRACT_INTERFACES:
        iface_dir = directory / iface
        if iface_dir.exists():
            _assert_owned_interface_dir(iface_dir, iface, directory, label)


def _normalise_output_dir(output_dir: Path) -> Path:
    """Reject symlink roots and normalise destination to a stable absolute path."""
    raw = Path(output_dir)
    if raw.is_symlink():
        raise ContractPublicationError(f"output_dir: refuses symlink target: {raw}")
    resolved = raw.resolve(strict=False)
    if resolved.is_symlink():
        raise ContractPublicationError(f"output_dir: refuses symlink target: {raw}")
    if resolved == resolved.parent:
        raise ContractPublicationError(
            f"output_dir: refuses filesystem root as destination: {raw}"
        )
    return resolved


def _collect_preserve_sources(
    output_dir: Path, preserve_from: Path | None
) -> list[Path]:
    preserve_sources: list[Path] = []
    if preserve_from is not None:
        preserve_path = Path(preserve_from)
        _reject_symlinks_in_tree(preserve_path, "preserve_from")
        if not preserve_path.exists():
            raise ContractPublicationError(
                f"preserve_from directory does not exist: {preserve_path}"
            )
        if not preserve_path.is_dir():
            raise ContractPublicationError(
                f"preserve_from path is not a directory: {preserve_path}"
            )
        missing_ifaces = [
            iface
            for iface in CONTRACT_INTERFACES
            if not (preserve_path / iface).is_dir()
        ]
        if missing_ifaces:
            raise ContractPublicationError(
                "preserve_from must be a contracts root containing "
                f"{', '.join(CONTRACT_INTERFACES)} directories "
                f"(missing {missing_ifaces} in {preserve_path})"
            )
        preserve_sources.append(preserve_path)

    _reject_symlinks_in_tree(output_dir, "output_dir")
    if output_dir.exists() and not output_dir.is_dir():
        raise ContractPublicationError(
            f"output_dir exists and is not a directory: {output_dir}"
        )
    if output_dir.is_dir():
        _assert_owned_contracts_root(output_dir, "output_dir")
        if any(output_dir.iterdir()):
            resolved_out = output_dir.resolve()
            if all(src.resolve() != resolved_out for src in preserve_sources):
                preserve_sources.append(output_dir)
    return preserve_sources


def _apply_published_dir_mode(staging_dir: Path, output_dir: Path) -> None:
    """Replace ``mkdtemp``'s private ``0700`` mode with standard/existing directory mode."""
    if os.name != "posix":
        return
    if output_dir.is_dir():
        target_mode = stat.S_IMODE(output_dir.stat().st_mode)
    else:
        probe = staging_dir / ".mode-probe"
        probe.mkdir()
        target_mode = stat.S_IMODE(probe.stat().st_mode)
        probe.rmdir()
    staging_dir.chmod(target_mode)


def _commit_staged_dir(
    staging_dir: Path, output_dir: Path
) -> tuple[Path | None, str | None]:
    """Replace ``output_dir`` with ``staging_dir`` atomically with rollback.

    Returns ``(retained_backup_path, cleanup_warning)`` when the commit
    succeeded but post-commit deletion of the backup directory failed.
    """
    _apply_published_dir_mode(staging_dir, output_dir)
    if not output_dir.exists():
        staging_dir.rename(output_dir)
        return None, None
    backup_dir = Path(
        tempfile.mkdtemp(prefix=f".{output_dir.name}.backup-", dir=output_dir.parent)
    )
    backup_dir.rmdir()
    output_dir.rename(backup_dir)
    try:
        staging_dir.rename(output_dir)
    except Exception:
        if backup_dir.exists() and not output_dir.exists():
            backup_dir.rename(output_dir)
        raise
    try:
        shutil.rmtree(backup_dir)
    except OSError as exc:
        warning = (
            f"published successfully to {output_dir}, but failed to remove backup "
            f"directory {backup_dir}: {exc}"
        )
        return backup_dir, warning
    return None, None


def publish_contracts_site(
    output_dir: Path,
    *,
    base_url: str = DEFAULT_BASE_URL,
    preserve_from: Path | None = None,
    validate: bool = True,
) -> dict[str, Any]:
    """Materialise versioned and ``latest`` forcing and output contract artefacts.

    All preservation checks, bundle authentication, staging writes, and
    validation complete before ``output_dir`` is updated, so a failed run
    leaves any existing destination tree unchanged. If post-commit cleanup of
    the replaced backup directory fails, publication still succeeds and reports
    the retained backup path in ``warnings`` and ``retained_backup``.
    """
    base = _normalise_base_url(base_url)
    output_dir = _normalise_output_dir(output_dir)
    preserve_sources = _collect_preserve_sources(output_dir, preserve_from)

    # Resolve and authenticate BOTH interfaces before mutating anything on disk.
    resolved_bundles: dict[ContractInterface, dict[str, dict[str, bytes]]] = {}
    preserved_summary: dict[ContractInterface, list[str]] = {}
    for iface in CONTRACT_INTERFACES:
        resolved_bundles[iface], preserved_summary[iface] = _resolve_interface_bundles(
            iface, preserve_sources
        )

    output_dir.parent.mkdir(parents=True, exist_ok=True)
    staging_dir = Path(
        tempfile.mkdtemp(prefix=f".{output_dir.name}.staging-", dir=output_dir.parent)
    )
    retained_backup: Path | None = None
    warnings: list[str] = []
    try:
        root_interfaces: dict[str, Any] = {}
        for iface in CONTRACT_INTERFACES:
            root_interfaces[iface] = _write_interface_dir(
                iface,
                staging_dir,
                base,
                resolved_bundles[iface],
                preserved_summary[iface],
            )

        root_index = {
            "base_url": base,
            "contracts_url": f"{base}/{CONTRACTS_URL_PATH}/",
            "interfaces": root_interfaces,
        }
        (staging_dir / "index.json").write_bytes(canonical_json_bytes(root_index))
        (staging_dir / "index.html").write_text(
            _render_root_index_html(root_index),
            encoding="ascii",
        )

        validation_report: dict[str, Any] | None = None
        if validate:
            validation_report = validate_published_contracts(staging_dir, base_url=base)
            validation_report["contracts_dir"] = str(output_dir)

        retained_backup, cleanup_warning = _commit_staged_dir(staging_dir, output_dir)
        if cleanup_warning is not None:
            warnings.append(cleanup_warning)
    finally:
        if staging_dir.exists():
            shutil.rmtree(staging_dir)

    report: dict[str, Any] = {
        "output_dir": str(output_dir),
        "base_url": base,
        "interfaces": {
            iface: {
                "current_version": root_interfaces[iface]["current_version"],
                "versions": root_interfaces[iface]["versions"],
                "preserved_versions": preserved_summary[iface],
            }
            for iface in CONTRACT_INTERFACES
        },
        "validated": validate,
        "validation": validation_report,
    }
    if warnings:
        report["warnings"] = warnings
    if retained_backup is not None:
        report["retained_backup"] = str(retained_backup)
    return report


class _HrefCollector(HTMLParser):
    """Collect ``href`` attributes from HTML anchor and link tags."""

    def __init__(self) -> None:
        super().__init__()
        self.hrefs: list[str] = []

    def handle_starttag(self, tag: str, attrs: list[tuple[str, str | None]]) -> None:
        if tag in ("a", "link"):
            for key, value in attrs:
                if key == "href" and value is not None:
                    self.hrefs.append(value)


def _is_within_root(path: Path, root: Path) -> bool:
    try:
        path.resolve().relative_to(root.resolve())
    except (ValueError, OSError):
        return False
    return True


def _check_relative_path(
    base_dir: Path,
    rel_path: str,
    root_dir: Path,
    context: str,
    errors: list[str],
) -> None:
    if not rel_path or rel_path.startswith("/") or "\x00" in rel_path:
        errors.append(f"{context}: expected valid relative path, got {rel_path!r}")
        return
    try:
        target = (base_dir / rel_path).resolve()
        within = _is_within_root(target, root_dir)
        if not within:
            errors.append(f"{context}: path escapes contracts root: {rel_path!r}")
            return
        is_dir = target.is_dir()
        has_index = is_dir and (target / "index.html").is_file()
        is_file = not is_dir and target.is_file()
    except (ValueError, OSError) as exc:
        errors.append(f"{context}: invalid path {rel_path!r}: {exc}")
        return
    if is_dir:
        if not has_index:
            errors.append(
                f"{context}: linked directory missing index.html: {rel_path!r}"
            )
        return
    if not is_file:
        errors.append(f"{context}: target file does not exist: {rel_path!r}")


def _check_ascii_file(path: Path, label: str, errors: list[str]) -> None:
    try:
        path.read_bytes().decode("ascii")
    except UnicodeDecodeError as exc:
        errors.append(f"{label}: contains non-ASCII byte at offset {exc.start}")


def _load_json_object_file(path: Path, label: str, errors: list[str]) -> dict[str, Any]:
    if not path.is_file():
        errors.append(f"{label}: missing")
        return {}
    _check_ascii_file(path, label, errors)
    try:
        parsed = _read_json_bytes(path.read_bytes(), label)
    except ContractPublicationError as exc:
        errors.append(str(exc))
        return {}
    if not isinstance(parsed, dict):
        errors.append(f"{label}: root must be a JSON object")
        return {}
    return parsed


def _read_html_hrefs(path: Path, label: str, errors: list[str]) -> list[str] | None:
    try:
        text = path.read_text(encoding="utf-8")
    except (OSError, UnicodeDecodeError) as exc:
        errors.append(f"{label}: cannot decode HTML as UTF-8: {exc}")
        return None
    collector = _HrefCollector()
    collector.feed(text)
    return collector.hrefs


def _validate_mapping_exact(
    actual: Any,
    expected: dict[str, str],
    context: str,
    errors: list[str],
) -> None:
    if not isinstance(actual, dict):
        errors.append(f"{context}: expected object, got {type(actual).__name__}")
        return
    if actual != expected:
        errors.append(f"{context}: expected {expected!r}, got {actual!r}")


def _validate_root_interface_entry(
    root: Path,
    iface: ContractInterface,
    root_ifaces: dict[str, Any],
    effective_base: str | None,
    expected_versions: list[str],
    current_version: str,
    errors: list[str],
) -> None:
    entry = root_ifaces.get(iface)
    if not isinstance(entry, dict):
        errors.append(f"index.json: missing or invalid interface entry for {iface!r}")
        return

    if entry.get("interface") != iface:
        errors.append(
            f"index.json[{iface}]: interface expected {iface!r}, got {entry.get('interface')!r}"
        )
    if entry.get("current_version") != current_version:
        errors.append(
            f"index.json[{iface}]: current_version expected {current_version!r}, "
            f"got {entry.get('current_version')!r}"
        )
    if entry.get("version_count") != len(expected_versions):
        errors.append(
            f"index.json[{iface}]: version_count expected {len(expected_versions)}, "
            f"got {entry.get('version_count')!r}"
        )
    if entry.get("versions") != expected_versions:
        errors.append(
            f"index.json[{iface}]: versions expected {expected_versions!r}, "
            f"got {entry.get('versions')!r}"
        )

    expected_paths = {
        "index_path": f"{iface}/index.json",
        "html_path": f"{iface}/index.html",
    }
    for key, expected_rel in expected_paths.items():
        actual_rel = entry.get(key)
        if actual_rel != expected_rel:
            errors.append(
                f"index.json[{iface}].{key}: expected {expected_rel!r}, got {actual_rel!r}"
            )
        else:
            _check_relative_path(
                root, actual_rel, root, f"index.json[{iface}].{key}", errors
            )

    expected_latest_paths = {
        art: f"{iface}/latest/{fname}" for art, fname in ARTEFACT_FILENAMES.items()
    }
    _validate_mapping_exact(
        entry.get("latest_paths"),
        expected_latest_paths,
        f"index.json[{iface}].latest_paths",
        errors,
    )
    if isinstance(entry.get("latest_paths"), dict):
        for art, rel in entry["latest_paths"].items():
            if isinstance(rel, str):
                _check_relative_path(
                    root, rel, root, f"index.json[{iface}].latest_paths.{art}", errors
                )

    if effective_base is not None:
        if (
            entry.get("index_url")
            != f"{effective_base}/{CONTRACTS_URL_PATH}/{iface}/index.json"
        ):
            errors.append(
                f"index.json[{iface}].index_url mismatch: {entry.get('index_url')!r}"
            )
        if (
            entry.get("html_url")
            != f"{effective_base}/{CONTRACTS_URL_PATH}/{iface}/index.html"
        ):
            errors.append(
                f"index.json[{iface}].html_url mismatch: {entry.get('html_url')!r}"
            )
        expected_latest_urls = {
            art: f"{effective_base}/{CONTRACTS_URL_PATH}/{iface}/latest/{fname}"
            for art, fname in ARTEFACT_FILENAMES.items()
        }
        _validate_mapping_exact(
            entry.get("latest_urls"),
            expected_latest_urls,
            f"index.json[{iface}].latest_urls",
            errors,
        )


def _validate_version_entry(
    root: Path,
    iface_dir: Path,
    iface: ContractInterface,
    ver: str,
    current_version: str,
    bundle: dict[str, bytes],
    idx_entry: dict[str, Any],
    effective_base: str | None,
    errors: list[str],
) -> None:
    expected_digests = {name: sha256_digest(bundle[name]) for name in BUNDLE_FILENAMES}
    expected_contract_digest = (
        expected_digests["catalogue.json"]
        if iface == "forcing"
        else expected_digests["manifest.json"]
    )
    if idx_entry.get("is_current") is not (ver == current_version):
        errors.append(
            f"{iface}/index.json[{ver}]: is_current expected {ver == current_version!r}, "
            f"got {idx_entry.get('is_current')!r}"
        )
    if not isinstance(idx_entry.get("preserved"), bool):
        errors.append(f"{iface}/index.json[{ver}]: preserved must be a boolean")
    if idx_entry.get("contract_digest") != expected_contract_digest:
        errors.append(
            f"{iface}/index.json[{ver}]: contract_digest mismatch "
            f"(expected {expected_contract_digest!r}, got {idx_entry.get('contract_digest')!r})"
        )
    if idx_entry.get("manifest_digest") != expected_digests["manifest.json"]:
        errors.append(
            f"{iface}/index.json[{ver}]: manifest_digest mismatch "
            f"(expected {expected_digests['manifest.json']!r}, got {idx_entry.get('manifest_digest')!r})"
        )
    _validate_mapping_exact(
        idx_entry.get("digests"),
        expected_digests,
        f"{iface}/index.json[{ver}].digests",
        errors,
    )

    expected_paths = {
        art: f"{ver}/{fname}" for art, fname in ARTEFACT_FILENAMES.items()
    }
    _validate_mapping_exact(
        idx_entry.get("paths"),
        expected_paths,
        f"{iface}/index.json[{ver}].paths",
        errors,
    )
    if isinstance(idx_entry.get("paths"), dict):
        for art, rel in idx_entry["paths"].items():
            if isinstance(rel, str):
                _check_relative_path(
                    iface_dir, rel, root, f"{iface}/index.json[{ver}].{art}", errors
                )

    if effective_base is not None:
        expected_urls = {
            art: f"{effective_base}/{CONTRACTS_URL_PATH}/{iface}/{ver}/{fname}"
            for art, fname in ARTEFACT_FILENAMES.items()
        }
        _validate_mapping_exact(
            idx_entry.get("urls"),
            expected_urls,
            f"{iface}/index.json[{ver}].urls",
            errors,
        )


def _validate_latest_section(
    iface_dir: Path,
    iface: ContractInterface,
    current_version: str,
    iface_index: dict[str, Any],
    effective_base: str | None,
    errors: list[str],
) -> None:
    latest_dir = iface_dir / "latest"
    current_dir = iface_dir / current_version
    if not latest_dir.is_dir():
        errors.append(f"{iface}/latest/: missing directory")
        return
    if not current_dir.is_dir():
        return
    try:
        latest_bundle = _load_bundle_from_dir(latest_dir, iface, "latest")
        current_bundle = _load_bundle_from_dir(current_dir, iface, current_version)
    except ContractPublicationError as exc:
        errors.append(str(exc))
        return

    for filename in BUNDLE_FILENAMES:
        if latest_bundle[filename] != current_bundle[filename]:
            errors.append(
                f"{iface}/latest/{filename}: bytes differ from "
                f"{iface}/{current_version}/{filename}"
            )

    latest_meta = iface_index.get("latest")
    if not isinstance(latest_meta, dict):
        errors.append(f"{iface}/index.json: missing or invalid 'latest' object")
        return
    if latest_meta.get("version") != current_version:
        errors.append(
            f"{iface}/index.json: latest.version expected {current_version!r}, "
            f"got {latest_meta.get('version')!r}"
        )
    expected_digests = {
        name: sha256_digest(latest_bundle[name]) for name in BUNDLE_FILENAMES
    }
    _validate_mapping_exact(
        latest_meta.get("digests"),
        expected_digests,
        f"{iface}/index.json.latest.digests",
        errors,
    )
    expected_paths = {
        art: f"latest/{fname}" for art, fname in ARTEFACT_FILENAMES.items()
    }
    _validate_mapping_exact(
        latest_meta.get("paths"),
        expected_paths,
        f"{iface}/index.json.latest.paths",
        errors,
    )
    if effective_base is not None:
        expected_urls = {
            art: f"{effective_base}/{CONTRACTS_URL_PATH}/{iface}/latest/{fname}"
            for art, fname in ARTEFACT_FILENAMES.items()
        }
        _validate_mapping_exact(
            latest_meta.get("urls"),
            expected_urls,
            f"{iface}/index.json.latest.urls",
            errors,
        )


def _validate_root_html_coverage(root_html_path: Path, errors: list[str]) -> None:
    if not root_html_path.is_file():
        errors.append("missing top-level index.html")
        return
    hrefs = _read_html_hrefs(root_html_path, "index.html", errors)
    if hrefs is None:
        return
    href_set = set(hrefs)
    required = ["index.json"]
    for iface in CONTRACT_INTERFACES:
        required.extend([
            f"{iface}/index.html",
            f"{iface}/index.json",
            *(f"{iface}/latest/{fname}" for fname in BUNDLE_FILENAMES),
        ])
    for link in required:
        if link not in href_set:
            errors.append(f"index.html: missing required discovery link {link!r}")


def _validate_interface_html_coverage(
    iface_html_path: Path,
    iface: ContractInterface,
    ordered_versions: list[str],
    errors: list[str],
) -> None:
    if not iface_html_path.is_file():
        errors.append(f"{iface}/index.html: missing")
        return
    hrefs = _read_html_hrefs(iface_html_path, f"{iface}/index.html", errors)
    if hrefs is None:
        return
    href_set = set(hrefs)
    for required in (
        "../index.html",
        "index.json",
        *(f"latest/{fname}" for fname in BUNDLE_FILENAMES),
    ):
        if required not in href_set:
            errors.append(f"{iface}/index.html: missing link to {required}")
    for ver in ordered_versions:
        for fname in BUNDLE_FILENAMES:
            if f"{ver}/{fname}" not in href_set:
                errors.append(f"{iface}/index.html: missing link to {ver}/{fname}")


def _parse_indexed_versions(
    iface: ContractInterface,
    iface_index: dict[str, Any],
    ordered_versions: list[str],
    errors: list[str],
) -> dict[str, dict[str, Any]]:
    raw_versions = iface_index.get("versions")
    if not isinstance(raw_versions, list):
        errors.append(f"{iface}/index.json: 'versions' must be a list")
        return {}
    indexed: dict[str, dict[str, Any]] = {}
    seen_order: list[str] = []
    for pos, item in enumerate(raw_versions):
        if not isinstance(item, dict) or not isinstance(item.get("version"), str):
            errors.append(
                f"{iface}/index.json: versions[{pos}] must be an object with string 'version'"
            )
            continue
        ver = item["version"]
        if ver in indexed:
            errors.append(f"{iface}/index.json: duplicate version entry {ver!r}")
            continue
        indexed[ver] = item
        seen_order.append(ver)
    if seen_order != ordered_versions:
        errors.append(
            f"{iface}/index.json: versions list {seen_order!r} does not match "
            f"version directories {ordered_versions!r}"
        )
    return indexed


def _validate_interface_dir(
    root: Path,
    iface: ContractInterface,
    effective_base: str | None,
    errors: list[str],
) -> list[str]:
    """Validate one interface directory, its index, bundles, latest mirror, and links."""
    iface_dir = root / iface
    if not iface_dir.is_dir():
        errors.append(f"missing interface directory: {iface}/")
        return []

    packaged_versions = get_contract_versions(iface)
    current_version = get_current_contract_version(iface)
    iface_index = _load_json_object_file(
        iface_dir / "index.json", f"{iface}/index.json", errors
    )
    if iface_index:
        if iface_index.get("interface") != iface:
            errors.append(
                f"{iface}/index.json: interface expected {iface!r}, "
                f"got {iface_index.get('interface')!r}"
            )
        if effective_base is not None and iface_index.get("base_url") != effective_base:
            errors.append(
                f"{iface}/index.json: base_url expected {effective_base!r}, "
                f"got {iface_index.get('base_url')!r}"
            )
        if iface_index.get("current_version") != current_version:
            errors.append(
                f"{iface}/index.json: current_version {iface_index.get('current_version')!r} "
                f"does not match {current_version!r}"
            )

    dir_versions = {
        child.name
        for child in iface_dir.iterdir()
        if child.is_dir() and child.name != "latest"
    }
    invalid_names = sorted(n for n in dir_versions if not _is_valid_semver(n))
    if invalid_names:
        errors.append(
            f"{iface}/: unexpected non-SemVer subdirectories: {invalid_names}"
        )

    valid_versions = {n for n in dir_versions if _is_valid_semver(n)}
    ordered_versions = sorted(valid_versions, key=_semver_key)
    missing = sorted(set(packaged_versions) - valid_versions)
    if missing:
        errors.append(f"{iface}/: missing registered version directories: {missing}")

    if iface_index and iface_index.get("version_count") != len(ordered_versions):
        errors.append(
            f"{iface}/index.json: version_count expected {len(ordered_versions)}, "
            f"got {iface_index.get('version_count')!r}"
        )

    indexed = (
        _parse_indexed_versions(iface, iface_index, ordered_versions, errors)
        if iface_index
        else {}
    )

    checked: list[str] = []
    for ver in ordered_versions:
        try:
            bundle = _load_bundle_from_dir(iface_dir / ver, iface, ver)
            _validate_bundle_content(
                iface,
                ver,
                bundle,
                expected_registry_digest=packaged_versions.get(ver),
                expected_manifest_digest=(
                    FORCING_PUBLISHED_MANIFEST_DIGESTS.get(ver)
                    if iface == "forcing"
                    else None
                ),
            )
            checked.append(ver)
        except ContractPublicationError as exc:
            errors.append(str(exc))
            continue
        if ver in indexed:
            _validate_version_entry(
                root,
                iface_dir,
                iface,
                ver,
                current_version,
                bundle,
                indexed[ver],
                effective_base,
                errors,
            )

    _validate_latest_section(
        iface_dir, iface, current_version, iface_index, effective_base, errors
    )
    _validate_interface_html_coverage(
        iface_dir / "index.html", iface, ordered_versions, errors
    )
    return checked


def _validate_all_html_links(root: Path, errors: list[str]) -> tuple[int, int]:
    html_files = sorted(root.rglob("*.html"))
    checked_links = 0
    for html_path in html_files:
        rel_html = html_path.relative_to(root).as_posix()
        _check_ascii_file(html_path, rel_html, errors)
        hrefs = _read_html_hrefs(html_path, rel_html, errors)
        if hrefs is None:
            continue
        for href in hrefs:
            if href.startswith(("http://", "https://", "mailto:", "#")):
                continue
            target_rel = href.split("#", 1)[0].split("?", 1)[0]
            if not target_rel:
                continue
            checked_links += 1
            _check_relative_path(
                html_path.parent, target_rel, root, f"{rel_html} href={href!r}", errors
            )
    return len(html_files), checked_links


def _resolve_validation_base_url(
    root_index: dict[str, Any],
    base_url: str | None,
    errors: list[str],
) -> str | None:
    expected_base = _normalise_base_url(base_url) if base_url is not None else None
    recorded = root_index.get("base_url") if root_index else None
    effective_base = expected_base
    if root_index:
        if not isinstance(recorded, str):
            errors.append("index.json: missing or non-string base_url")
        else:
            try:
                normalised_recorded = _normalise_base_url(recorded)
            except ContractError as exc:
                errors.append(f"index.json: invalid base_url: {exc}")
                normalised_recorded = None
            if normalised_recorded is not None:
                if recorded != normalised_recorded:
                    errors.append(
                        f"index.json: base_url {recorded!r} is not normalised "
                        f"({normalised_recorded!r})"
                    )
                if expected_base is not None and normalised_recorded != expected_base:
                    errors.append(
                        f"index.json: base_url {recorded!r} does not match {expected_base!r}"
                    )
                if effective_base is None:
                    effective_base = normalised_recorded
    if effective_base is not None and root_index:
        expected_contracts_url = f"{effective_base}/{CONTRACTS_URL_PATH}/"
        if root_index.get("contracts_url") != expected_contracts_url:
            errors.append(
                f"index.json: contracts_url {root_index.get('contracts_url')!r} "
                f"does not match {expected_contracts_url!r}"
            )
    return effective_base


def validate_published_contracts(
    contracts_dir: Path,
    *,
    base_url: str | None = None,
) -> dict[str, Any]:
    """Validate a built contracts directory for schema, digest, metadata, and link integrity."""
    root = Path(contracts_dir)
    _reject_symlinks_in_tree(root, "contracts_dir")
    if not root.exists():
        raise ContractPublicationError(f"contracts directory does not exist: {root}")
    if not root.is_dir():
        raise ContractPublicationError(f"contracts path is not a directory: {root}")

    errors: list[str] = []
    _validate_root_html_coverage(root / "index.html", errors)
    root_index = _load_json_object_file(root / "index.json", "index.json", errors)
    effective_base = _resolve_validation_base_url(root_index, base_url, errors)

    raw_root_ifaces = root_index.get("interfaces") if root_index else None
    if isinstance(raw_root_ifaces, dict):
        root_ifaces = raw_root_ifaces
        extra_ifaces = sorted(set(root_ifaces) - set(CONTRACT_INTERFACES))
        if extra_ifaces:
            errors.append(f"index.json: unexpected interface entries: {extra_ifaces}")
    else:
        if root_index:
            errors.append("index.json: 'interfaces' must be a JSON object")
        root_ifaces = {}

    checked_versions: dict[str, list[str]] = {}
    for iface in CONTRACT_INTERFACES:
        checked_versions[iface] = _validate_interface_dir(
            root, iface, effective_base, errors
        )
        _validate_root_interface_entry(
            root,
            iface,
            root_ifaces,
            effective_base,
            checked_versions[iface],
            get_current_contract_version(iface),
            errors,
        )

    html_count, checked_links = _validate_all_html_links(root, errors)

    if errors:
        detail = "\n".join(f"- {err}" for err in errors)
        raise ContractPublicationError(
            f"Contract site validation failed with {len(errors)} error(s):\n{detail}"
        )

    return {
        "contracts_dir": str(root),
        "interfaces": checked_versions,
        "html_files_checked": html_count,
        "relative_links_checked": checked_links,
    }


__all__ = [
    "ARTEFACT_FILENAMES",
    "BUNDLE_FILENAMES",
    "CONTRACTS_URL_PATH",
    "CONTRACT_INTERFACES",
    "DEFAULT_BASE_URL",
    "FORCING_PUBLISHED_MANIFEST_DIGESTS",
    "ContractArtefact",
    "ContractError",
    "ContractExportArtefact",
    "ContractInterface",
    "ContractPublicationError",
    "canonical_json_bytes",
    "export_contract",
    "forcing_contract_json_schema",
    "get_contract_info",
    "get_contract_versions",
    "get_current_contract_version",
    "load_contract_artefact_bytes",
    "load_contract_bundle_bytes",
    "publish_contracts_site",
    "sha256_digest",
    "validate_published_contracts",
    "write_forcing_published_snapshot",
]
