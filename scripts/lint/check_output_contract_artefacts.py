#!/usr/bin/env python3
"""Generate and audit immutable output-contract artefacts."""

from __future__ import annotations

import argparse
from hashlib import sha256
import importlib
import json
from pathlib import Path
import sys
from types import ModuleType
from typing import Any

from jsonschema import Draft202012Validator
from jsonschema.exceptions import SchemaError, ValidationError

PROJECT_ROOT = Path(__file__).resolve().parents[2]
SOURCE_ROOT = PROJECT_ROOT / "src"
_OUTPUT_SOURCE = SOURCE_ROOT / "supy/data_model/output"
_SOURCE_PACKAGE = "_suews_output_contract_source"
package = ModuleType(_SOURCE_PACKAGE)
package.__package__ = _SOURCE_PACKAGE
package.__path__ = [str(_OUTPUT_SOURCE)]
sys.modules[_SOURCE_PACKAGE] = package

contract = importlib.import_module(f"{_SOURCE_PACKAGE}.contract")
registry = importlib.import_module(f"{_SOURCE_PACKAGE}.registry")
version_registry = importlib.import_module(f"{_SOURCE_PACKAGE}.version")
output_contract_json_schema = contract.output_contract_json_schema
get_output_contract_catalogue = registry.get_output_contract_catalogue
CURRENT_OUTPUT_VERSION = version_registry.CURRENT_OUTPUT_VERSION
OUTPUT_VERSIONS = version_registry.OUTPUT_VERSIONS

ARTEFACT_ROOT = SOURCE_ROOT / "supy/data_model/output/artefacts"
_BUNDLE_FILES = frozenset({
    "catalogue.json",
    "catalogue.schema.json",
    "manifest.json",
})
_CONTENT_FILES = ("catalogue.json", "catalogue.schema.json")


class OutputContractAuditError(RuntimeError):
    """Raised when a published output-contract bundle is inconsistent."""


def canonical_json_bytes(value: Any) -> bytes:
    """Return the canonical UTF-8 JSON representation used by output releases."""
    return (
        json.dumps(
            value,
            sort_keys=True,
            separators=(",", ":"),
            ensure_ascii=True,
            allow_nan=False,
        )
        + "\n"
    ).encode("utf-8")


def _digest(content: bytes) -> str:
    return f"sha256:{sha256(content).hexdigest()}"


def build_output_contract_bundle(version: str) -> dict[str, bytes]:
    """Build the three canonical files for one output-contract release."""
    content = {
        "catalogue.json": canonical_json_bytes(
            get_output_contract_catalogue().model_dump(mode="json")
        ),
        "catalogue.schema.json": canonical_json_bytes(output_contract_json_schema()),
    }
    manifest = {
        "files": {name: _digest(content[name]) for name in _CONTENT_FILES},
        "interface": "output",
        "version": version,
    }
    return {**content, "manifest.json": canonical_json_bytes(manifest)}


def _read_json(path: Path) -> Any:
    try:
        return json.loads(path.read_text(encoding="utf-8"))
    except (OSError, UnicodeDecodeError, json.JSONDecodeError) as exc:
        raise OutputContractAuditError(
            f"cannot read canonical JSON {path}: {exc}"
        ) from exc


def _check_release_bundle(
    path_release: Path,
    version: str,
    expected_manifest_digest: str,
) -> dict[str, bytes]:
    if not path_release.is_dir():
        raise OutputContractAuditError(
            f"missing output release directory: {path_release}"
        )

    names = {path.name for path in path_release.iterdir()}
    if names != _BUNDLE_FILES:
        raise OutputContractAuditError(
            f"{version}: release directory must contain exactly "
            f"{sorted(_BUNDLE_FILES)}; found {sorted(names)}"
        )

    content = {
        name: (path_release / name).read_bytes() for name in sorted(_BUNDLE_FILES)
    }
    manifest = _read_json(path_release / "manifest.json")
    if content["manifest.json"] != canonical_json_bytes(manifest):
        raise OutputContractAuditError(f"{version}: manifest.json is not canonical")

    expected_manifest = {
        "files": {name: _digest(content[name]) for name in _CONTENT_FILES},
        "interface": "output",
        "version": version,
    }
    if manifest != expected_manifest:
        raise OutputContractAuditError(
            f"{version}: manifest file digest or release metadata does not match"
        )
    if _digest(content["manifest.json"]) != expected_manifest_digest:
        raise OutputContractAuditError(
            f"{version}: manifest digest does not match OUTPUT_VERSIONS"
        )

    schema = _read_json(path_release / "catalogue.schema.json")
    catalogue = _read_json(path_release / "catalogue.json")
    try:
        Draft202012Validator.check_schema(schema)
        Draft202012Validator(schema).validate(catalogue)
    except (SchemaError, ValidationError) as exc:
        raise OutputContractAuditError(
            f"{version}: catalogue does not validate against its schema: {exc}"
        ) from exc
    return content


def audit_output_contract(project_root: Path = PROJECT_ROOT) -> list[str]:
    """Audit all stored releases and freshness of the current output contract."""
    if CURRENT_OUTPUT_VERSION is None or not OUTPUT_VERSIONS:
        raise OutputContractAuditError("the output contract has no published version")
    latest_version = next(reversed(OUTPUT_VERSIONS))
    if latest_version != CURRENT_OUTPUT_VERSION:
        raise OutputContractAuditError(
            "current output version is not the latest release"
        )

    artefact_root = project_root / "src/supy/data_model/output/artefacts"
    stored_versions = (
        {path.name for path in artefact_root.iterdir() if path.is_dir()}
        if artefact_root.is_dir()
        else set()
    )
    registered_versions = set(OUTPUT_VERSIONS)
    if stored_versions != registered_versions:
        raise OutputContractAuditError(
            "stored output release directories must match OUTPUT_VERSIONS exactly "
            f"(stored={sorted(stored_versions)}, registered={sorted(registered_versions)})"
        )

    checked: list[str] = []
    for version, manifest_digest in OUTPUT_VERSIONS.items():
        stored = _check_release_bundle(
            artefact_root / version,
            version,
            manifest_digest,
        )
        if version == CURRENT_OUTPUT_VERSION:
            generated = build_output_contract_bundle(version)
            for name in sorted(_BUNDLE_FILES):
                if stored[name] != generated[name]:
                    raise OutputContractAuditError(
                        f"{version}: {name} is stale; regenerate the current bundle"
                    )
        checked.append(version)
    return checked


def write_output_contract_bundle(
    version: str,
    project_root: Path = PROJECT_ROOT,
) -> str:
    """Write a new release bundle without overwriting different bytes."""
    bundle = build_output_contract_bundle(version)
    path_release = project_root / f"src/supy/data_model/output/artefacts/{version}"
    if path_release.exists():
        existing_names = {path.name for path in path_release.iterdir()}
        if existing_names != _BUNDLE_FILES or any(
            (path_release / name).read_bytes() != content
            for name, content in bundle.items()
        ):
            raise OutputContractAuditError(
                f"refusing to overwrite different bytes in {path_release}"
            )
    else:
        path_release.mkdir(parents=True)
        for name, content in bundle.items():
            (path_release / name).write_bytes(content)
    return _digest(bundle["manifest.json"])


def _run(write_version: str | None) -> None:
    if write_version:
        digest = write_output_contract_bundle(write_version)
        print(f"[output-contract] wrote {write_version}: {digest}")
    else:
        versions = audit_output_contract()
        print("[output-contract] valid releases: " + ", ".join(versions))


def main(argv: list[str] | None = None) -> int:
    """Run the output-contract artefact audit or write a new release."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--write",
        metavar="VERSION",
        help="write a new immutable release bundle and print its manifest digest",
    )
    args = parser.parse_args(argv)
    try:
        _run(args.write)
    except (OSError, OutputContractAuditError) as exc:
        print(f"[output-contract] FAILED: {exc}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
