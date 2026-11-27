"""Discovery, export, publication, and validation for SUEWS data contracts.

This module owns the user-facing and site-publication projection of the
independently versioned forcing and output contracts. Immutable release
artefacts are loaded from packaged resources under
``supy.data_model.forcing.artefacts`` and ``supy.data_model.output.artefacts``.
"""

from __future__ import annotations

from hashlib import sha256
from html import escape as html_escape
from html.parser import HTMLParser
import importlib.resources
import json
from pathlib import Path
import re
import shutil
import sys
from typing import Any, Literal

from jsonschema import Draft202012Validator
from jsonschema.exceptions import SchemaError, ValidationError

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
_SEMVER_RE = re.compile(r"^(\d+)\.(\d+)\.(\d+)$")


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


def _semver_key(version: str) -> tuple[int, int, int]:
    match = _SEMVER_RE.match(version)
    if match is None:
        raise ContractError(f"invalid SemVer version: {version!r}")
    return (int(match.group(1)), int(match.group(2)), int(match.group(3)))


def _normalise_base_url(base_url: str) -> str:
    stripped = base_url.strip().rstrip("/")
    if not stripped:
        raise ContractError("base_url must be a non-empty URL")
    return stripped


def _validate_interface(interface: str) -> ContractInterface:
    if interface not in CONTRACT_INTERFACES:
        raise ContractError(
            f"unknown contract interface {interface!r}; "
            f"expected one of: {', '.join(CONTRACT_INTERFACES)}"
        )
    return interface  # type: ignore[return-value]


def forcing_contract_json_schema() -> dict[str, Any]:
    """Return the Draft 2020-12 JSON Schema for the forcing contract catalogue."""
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


def _resource_root(interface: ContractInterface):
    package_name = f"{__package__}.{interface}"
    pkg = sys.modules.get(package_name)
    if pkg is not None and getattr(pkg, "__spec__", None) is None:
        paths = list(getattr(pkg, "__path__", ()))
        if paths:
            return Path(paths[0])
    return importlib.resources.files(package_name)


def load_contract_bundle_bytes(
    interface: ContractInterface,
    version: str | None = None,
) -> tuple[str, dict[str, bytes]]:
    """Load the canonical three-file bundle for one interface release.

    Returns
    -------
    tuple[str, dict[str, bytes]]
        ``(resolved_version, {"catalogue.json": ..., "catalogue.schema.json": ..., "manifest.json": ...})``
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

    if iface == "forcing":
        resource = root.joinpath(f"artefacts/{resolved}.json")
        if not resource.is_file():
            raise ContractError(
                f"packaged forcing artefact is missing for version {resolved}"
            )
        catalogue_bytes = resource.read_bytes()
        if sha256_digest(catalogue_bytes) != expected_digest:
            raise ContractError(
                f"packaged forcing artefact digest mismatch for version {resolved}"
            )
        schema_bytes = canonical_json_bytes(forcing_contract_json_schema())
        manifest_obj = {
            "files": {
                "catalogue.json": sha256_digest(catalogue_bytes),
                "catalogue.schema.json": sha256_digest(schema_bytes),
            },
            "interface": "forcing",
            "version": resolved,
        }
        manifest_bytes = canonical_json_bytes(manifest_obj)
        return resolved, {
            "catalogue.json": catalogue_bytes,
            "catalogue.schema.json": schema_bytes,
            "manifest.json": manifest_bytes,
        }

    bundle: dict[str, bytes] = {}
    for filename in BUNDLE_FILENAMES:
        resource = root.joinpath(f"artefacts/{resolved}/{filename}")
        if not resource.is_file():
            raise ContractError(
                f"packaged output artefact is missing: {resolved}/{filename}"
            )
        bundle[filename] = resource.read_bytes()

    if sha256_digest(bundle["manifest.json"]) != expected_digest:
        raise ContractError(
            f"packaged output manifest digest mismatch for version {resolved}"
        )
    return resolved, bundle


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
        entries = []
        for ver, digest in versions.items():
            entries.append({
                "version": ver,
                "digest": digest,
                "is_current": ver == current,
                "urls": {
                    art: f"{base}/{CONTRACTS_URL_PATH}/{iface}/{ver}/{fname}"
                    for art, fname in ARTEFACT_FILENAMES.items()
                },
            })
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


def _read_json_bytes(content: bytes, label: str) -> Any:
    try:
        return json.loads(content.decode("utf-8"))
    except (UnicodeDecodeError, json.JSONDecodeError) as exc:
        raise ContractPublicationError(f"{label}: invalid JSON: {exc}") from exc


def _validate_bundle_content(
    interface: ContractInterface,
    version: str,
    bundle: dict[str, bytes],
    *,
    expected_registry_digest: str | None = None,
) -> dict[str, Any]:
    """Validate one release bundle's manifest, digests, and JSON Schema conformance."""
    errors: list[str] = []
    for filename in BUNDLE_FILENAMES:
        if filename not in bundle:
            errors.append(f"{interface}/{version}: missing {filename}")
    if errors:
        raise ContractPublicationError("; ".join(errors))

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

    schema = _read_json_bytes(
        bundle["catalogue.schema.json"],
        f"{interface}/{version}/catalogue.schema.json",
    )
    catalogue = _read_json_bytes(
        bundle["catalogue.json"],
        f"{interface}/{version}/catalogue.json",
    )
    try:
        Draft202012Validator.check_schema(schema)
        Draft202012Validator(schema).validate(catalogue)
    except (SchemaError, ValidationError) as exc:
        raise ContractPublicationError(
            f"{interface}/{version}: catalogue fails schema validation: {exc.message}"
        ) from exc

    return manifest


def _load_bundle_from_dir(
    release_dir: Path, interface: ContractInterface, version: str
) -> dict[str, bytes]:
    found = {p.name for p in release_dir.iterdir() if p.is_file()}
    expected = set(BUNDLE_FILENAMES)
    extra_dirs = [p.name for p in release_dir.iterdir() if not p.is_file()]
    if found != expected or extra_dirs:
        raise ContractPublicationError(
            f"{interface}/{version}: directory must contain exactly "
            f"{sorted(expected)}; found files={sorted(found)}, non-files={sorted(extra_dirs)}"
        )
    return {name: (release_dir / name).read_bytes() for name in BUNDLE_FILENAMES}


def _collect_preserved_bundles(
    interface: ContractInterface,
    sources: list[Path],
    packaged_versions: dict[str, str],
) -> dict[str, dict[str, bytes]]:
    """Collect and verify historical version bundles from existing deployments."""
    preserved: dict[str, dict[str, bytes]] = {}
    for source_root in sources:
        iface_dir = source_root / interface
        if not iface_dir.is_dir():
            continue
        for child in sorted(iface_dir.iterdir()):
            if not child.is_dir() or child.name == "latest":
                continue
            if _SEMVER_RE.match(child.name) is None:
                continue
            ver = child.name
            bundle = _load_bundle_from_dir(child, interface, ver)
            _validate_bundle_content(
                interface,
                ver,
                bundle,
                expected_registry_digest=packaged_versions.get(ver),
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
    """Merge packaged and preserved release bundles while enforcing immutability."""
    packaged_versions = get_contract_versions(iface)
    preserved_bundles = _collect_preserved_bundles(
        iface, preserve_sources, packaged_versions
    )
    final_bundles: dict[str, dict[str, bytes]] = {}
    preserved_only: list[str] = []

    for ver in packaged_versions:
        _, packaged_bundle = load_contract_bundle_bytes(iface, version=ver)
        existing = preserved_bundles.get(ver)
        if existing is None:
            final_bundles[ver] = packaged_bundle
            continue
        if iface == "output":
            if existing != packaged_bundle:
                raise ContractPublicationError(
                    f"output/{ver}: refusing to overwrite differing published bundle"
                )
            final_bundles[ver] = packaged_bundle
        else:
            if existing["catalogue.json"] != packaged_bundle["catalogue.json"]:
                raise ContractPublicationError(
                    f"forcing/{ver}: refusing to overwrite differing catalogue.json"
                )
            # Keep already-published schema and manifest bytes intact so
            # previously deployed versioned URLs never drift.
            final_bundles[ver] = existing

    for ver, bundle in preserved_bundles.items():
        if ver not in final_bundles:
            final_bundles[ver] = bundle
            preserved_only.append(ver)

    return final_bundles, sorted(preserved_only, key=_semver_key)


def _publish_interface_dir(
    iface: ContractInterface,
    output_dir: Path,
    base: str,
    preserve_sources: list[Path],
) -> tuple[dict[str, Any], list[str]]:
    """Materialise one interface's versioned bundles, ``latest`` mirror, and indices."""
    current_version = get_current_contract_version(iface)
    final_bundles, preserved_only = _resolve_interface_bundles(iface, preserve_sources)
    preserved_set = set(preserved_only)
    ordered_versions = sorted(final_bundles, key=_semver_key)

    iface_dir = output_dir / iface
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
    if latest_dir.exists():
        shutil.rmtree(latest_dir)
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

    root_entry = {
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
    return root_entry, preserved_only


def publish_contracts_site(
    output_dir: Path,
    *,
    base_url: str = DEFAULT_BASE_URL,
    preserve_from: Path | None = None,
    validate: bool = True,
) -> dict[str, Any]:
    """Materialise versioned and ``latest`` forcing and output contract artefacts.

    Existing version-addressed bundles found in ``preserve_from`` or ``output_dir``
    are checked for byte-level immutability and carried forward so previously
    published versions remain retrievable after new releases.
    """
    base = _normalise_base_url(base_url)
    output_dir = Path(output_dir)

    preserve_sources: list[Path] = []
    if preserve_from is not None:
        preserve_path = Path(preserve_from)
        if not preserve_path.is_dir():
            raise ContractPublicationError(
                f"preserve_from directory does not exist: {preserve_path}"
            )
        preserve_sources.append(preserve_path)
    if output_dir.is_dir():
        resolved_out = output_dir.resolve()
        if all(src.resolve() != resolved_out for src in preserve_sources):
            preserve_sources.append(output_dir)

    output_dir.mkdir(parents=True, exist_ok=True)
    root_interfaces: dict[str, Any] = {}
    preserved_summary: dict[str, list[str]] = {}

    for iface in CONTRACT_INTERFACES:
        root_interfaces[iface], preserved_summary[iface] = _publish_interface_dir(
            iface, output_dir, base, preserve_sources
        )

    root_index = {
        "base_url": base,
        "contracts_url": f"{base}/{CONTRACTS_URL_PATH}/",
        "interfaces": root_interfaces,
    }
    (output_dir / "index.json").write_bytes(canonical_json_bytes(root_index))
    (output_dir / "index.html").write_text(
        _render_root_index_html(root_index),
        encoding="ascii",
    )

    validation_report: dict[str, Any] | None = None
    if validate:
        validation_report = validate_published_contracts(output_dir, base_url=base)

    return {
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
    except ValueError:
        return False
    return True


def _check_relative_path(
    base_dir: Path,
    rel_path: str,
    root_dir: Path,
    context: str,
    errors: list[str],
) -> None:
    if not rel_path or rel_path.startswith("/"):
        errors.append(f"{context}: expected relative path, got {rel_path!r}")
        return
    target = (base_dir / rel_path).resolve()
    if not _is_within_root(target, root_dir):
        errors.append(f"{context}: path escapes contracts root: {rel_path!r}")
        return
    if target.is_dir():
        if not (target / "index.html").is_file():
            errors.append(
                f"{context}: linked directory missing index.html: {rel_path!r}"
            )
        return
    if not target.is_file():
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


def _validate_root_interface_entry(
    root: Path,
    iface: ContractInterface,
    root_ifaces: dict[str, Any],
    errors: list[str],
) -> None:
    if iface not in root_ifaces:
        errors.append(f"index.json: missing interface entry for {iface!r}")
        return
    entry = root_ifaces[iface]
    for key in ("index_path", "html_path"):
        if isinstance(entry, dict) and isinstance(entry.get(key), str):
            _check_relative_path(
                root, entry[key], root, f"index.json[{iface}].{key}", errors
            )
        else:
            errors.append(f"index.json[{iface}]: missing {key}")
    latest_paths = entry.get("latest_paths", {}) if isinstance(entry, dict) else {}
    for art in ARTEFACT_FILENAMES:
        rel = latest_paths.get(art)
        if isinstance(rel, str):
            _check_relative_path(
                root, rel, root, f"index.json[{iface}].latest_paths.{art}", errors
            )
        else:
            errors.append(f"index.json[{iface}]: missing latest_paths.{art}")


def _validate_version_entry_links(
    root: Path,
    iface_dir: Path,
    iface: ContractInterface,
    ver: str,
    idx_entry: dict[str, Any],
    expected_base: str | None,
    errors: list[str],
) -> None:
    paths = idx_entry.get("paths", {})
    urls = idx_entry.get("urls", {})
    for art, fname in ARTEFACT_FILENAMES.items():
        rel = paths.get(art)
        if rel != f"{ver}/{fname}":
            errors.append(
                f"{iface}/index.json[{ver}]: paths.{art} expected "
                f"{ver}/{fname!r}, got {rel!r}"
            )
        else:
            _check_relative_path(
                iface_dir, rel, root, f"{iface}/index.json[{ver}].{art}", errors
            )
        if expected_base is not None:
            expected_url = f"{expected_base}/{CONTRACTS_URL_PATH}/{iface}/{ver}/{fname}"
            if urls.get(art) != expected_url:
                errors.append(
                    f"{iface}/index.json[{ver}]: urls.{art} expected "
                    f"{expected_url!r}, got {urls.get(art)!r}"
                )


def _validate_latest_parity(
    iface_dir: Path,
    iface: ContractInterface,
    current_version: str,
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


def _validate_interface_html_coverage(
    iface_html_path: Path,
    iface: ContractInterface,
    valid_dir_versions: set[str],
    errors: list[str],
) -> None:
    if not iface_html_path.is_file():
        errors.append(f"{iface}/index.html: missing")
        return
    collector = _HrefCollector()
    collector.feed(iface_html_path.read_text(encoding="utf-8"))
    href_set = set(collector.hrefs)
    for fname in BUNDLE_FILENAMES:
        if f"latest/{fname}" not in href_set:
            errors.append(f"{iface}/index.html: missing link to latest/{fname}")
    for ver in sorted(valid_dir_versions, key=_semver_key):
        if f"{ver}/catalogue.json" not in href_set:
            errors.append(f"{iface}/index.html: missing link to {ver}/catalogue.json")


def _validate_interface_dir(
    root: Path,
    iface: ContractInterface,
    expected_base: str | None,
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
    if iface_index and iface_index.get("current_version") != current_version:
        errors.append(
            f"{iface}/index.json: current_version {iface_index.get('current_version')!r} "
            f"does not match {current_version!r}"
        )

    dir_versions = {
        child.name
        for child in iface_dir.iterdir()
        if child.is_dir() and child.name != "latest"
    }
    invalid_names = sorted(n for n in dir_versions if _SEMVER_RE.match(n) is None)
    if invalid_names:
        errors.append(
            f"{iface}/: unexpected non-SemVer subdirectories: {invalid_names}"
        )

    valid_versions = {n for n in dir_versions if _SEMVER_RE.match(n) is not None}
    missing = sorted(set(packaged_versions) - valid_versions)
    if missing:
        errors.append(f"{iface}/: missing registered version directories: {missing}")

    indexed: dict[str, dict[str, Any]] = {}
    for item in (
        iface_index.get("versions", [])
        if isinstance(iface_index.get("versions"), list)
        else []
    ):
        if isinstance(item, dict) and isinstance(item.get("version"), str):
            indexed[item["version"]] = item
    if set(indexed) != valid_versions:
        errors.append(
            f"{iface}/index.json: indexed versions {sorted(indexed)} "
            f"do not match version directories {sorted(valid_versions)}"
        )

    checked: list[str] = []
    for ver in sorted(valid_versions, key=_semver_key):
        try:
            bundle = _load_bundle_from_dir(iface_dir / ver, iface, ver)
            _validate_bundle_content(
                iface, ver, bundle, expected_registry_digest=packaged_versions.get(ver)
            )
            checked.append(ver)
        except ContractPublicationError as exc:
            errors.append(str(exc))
        if ver in indexed:
            _validate_version_entry_links(
                root, iface_dir, iface, ver, indexed[ver], expected_base, errors
            )

    _validate_latest_parity(iface_dir, iface, current_version, errors)
    _validate_interface_html_coverage(
        iface_dir / "index.html", iface, valid_versions, errors
    )
    return checked


def _validate_all_html_links(root: Path, errors: list[str]) -> tuple[int, int]:
    html_files = sorted(root.rglob("*.html"))
    checked_links = 0
    for html_path in html_files:
        rel_html = html_path.relative_to(root).as_posix()
        _check_ascii_file(html_path, rel_html, errors)
        collector = _HrefCollector()
        collector.feed(html_path.read_text(encoding="utf-8"))
        for href in collector.hrefs:
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


def validate_published_contracts(
    contracts_dir: Path,
    *,
    base_url: str | None = None,
) -> dict[str, Any]:
    """Validate a built contracts directory for schema, digest, and link integrity."""
    root = Path(contracts_dir)
    if not root.is_dir():
        raise ContractPublicationError(f"contracts directory does not exist: {root}")

    expected_base = _normalise_base_url(base_url) if base_url is not None else None
    errors: list[str] = []

    if not (root / "index.html").is_file():
        errors.append("missing top-level index.html")
    root_index = _load_json_object_file(root / "index.json", "index.json", errors)
    if expected_base is not None and root_index:
        expected_contracts_url = f"{expected_base}/{CONTRACTS_URL_PATH}/"
        if root_index.get("contracts_url") != expected_contracts_url:
            errors.append(
                f"index.json: contracts_url {root_index.get('contracts_url')!r} "
                f"does not match {expected_contracts_url!r}"
            )

    root_ifaces = (
        root_index.get("interfaces", {}) if isinstance(root_index, dict) else {}
    )
    checked_versions: dict[str, list[str]] = {}
    for iface in CONTRACT_INTERFACES:
        _validate_root_interface_entry(root, iface, root_ifaces, errors)
        checked_versions[iface] = _validate_interface_dir(
            root, iface, expected_base, errors
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
]
