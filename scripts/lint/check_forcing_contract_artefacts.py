#!/usr/bin/env python3
"""Generate and audit immutable forcing-contract artefacts."""

from __future__ import annotations

import argparse
from hashlib import sha256
import importlib
import json
from pathlib import Path
import sys
from types import ModuleType
from typing import Any

PROJECT_ROOT = Path(__file__).resolve().parents[2]
SOURCE_ROOT = PROJECT_ROOT / "src"
_FORCING_SOURCE = SOURCE_ROOT / "supy/data_model/forcing"
_SOURCE_PACKAGE = "_suews_forcing_contract_source"
package = ModuleType(_SOURCE_PACKAGE)
package.__package__ = _SOURCE_PACKAGE
package.__path__ = [str(_FORCING_SOURCE)]
sys.modules[_SOURCE_PACKAGE] = package

variables = importlib.import_module(f"{_SOURCE_PACKAGE}.variables")
version_registry = importlib.import_module(f"{_SOURCE_PACKAGE}.version")
FORCING_REGISTRY = variables.FORCING_REGISTRY
CURRENT_FORCING_VERSION = version_registry.CURRENT_FORCING_VERSION
FORCING_VERSIONS = version_registry.FORCING_VERSIONS

ARTEFACT_ROOT = SOURCE_ROOT / "supy/data_model/forcing/artefacts"


class ForcingContractAuditError(RuntimeError):
    """Raised when a published forcing contract is inconsistent."""


def canonical_json_bytes(value: Any) -> bytes:
    """Return the canonical UTF-8 JSON representation for forcing releases."""
    return (
        json.dumps(
            value,
            sort_keys=True,
            separators=(",", ":"),
            ensure_ascii=False,
            allow_nan=False,
        )
        + "\n"
    ).encode("utf-8")


def _digest(content: bytes) -> str:
    return f"sha256:{sha256(content).hexdigest()}"


def build_forcing_contract_artefact() -> bytes:
    """Build canonical bytes directly from the authoritative registry."""
    return canonical_json_bytes(FORCING_REGISTRY.model_dump(mode="json"))


def audit_forcing_contract(project_root: Path = PROJECT_ROOT) -> list[str]:
    """Audit every stored release and freshness of the current contract."""
    if CURRENT_FORCING_VERSION is None or not FORCING_VERSIONS:
        raise ForcingContractAuditError("the forcing contract has no published version")
    if next(reversed(FORCING_VERSIONS)) != CURRENT_FORCING_VERSION:
        raise ForcingContractAuditError(
            "current forcing version is not the latest release"
        )

    artefact_root = project_root / "src/supy/data_model/forcing/artefacts"
    stored = (
        {path.name for path in artefact_root.iterdir()}
        if artefact_root.is_dir()
        else set()
    )
    registered = {f"{version}.json" for version in FORCING_VERSIONS}
    if stored != registered:
        raise ForcingContractAuditError(
            "stored forcing artefacts must match FORCING_VERSIONS exactly "
            f"(stored={sorted(stored)}, registered={sorted(registered)})"
        )

    checked: list[str] = []
    for version, expected_digest in FORCING_VERSIONS.items():
        path = artefact_root / f"{version}.json"
        content = path.read_bytes()
        if _digest(content) != expected_digest:
            raise ForcingContractAuditError(
                f"{version}: artefact digest does not match FORCING_VERSIONS"
            )
        try:
            parsed = json.loads(content)
        except (UnicodeDecodeError, json.JSONDecodeError) as exc:
            raise ForcingContractAuditError(
                f"{version}: cannot read canonical JSON: {exc}"
            ) from exc
        if content != canonical_json_bytes(parsed):
            raise ForcingContractAuditError(f"{version}: artefact is not canonical")
        if (
            version == CURRENT_FORCING_VERSION
            and content != build_forcing_contract_artefact()
        ):
            raise ForcingContractAuditError(
                f"{version}: artefact is stale; regenerate the current release"
            )
        checked.append(version)
    return checked


def write_forcing_contract_artefact(
    version: str,
    project_root: Path = PROJECT_ROOT,
) -> str:
    """Write a new release without overwriting different bytes."""
    content = build_forcing_contract_artefact()
    path = project_root / f"src/supy/data_model/forcing/artefacts/{version}.json"
    if path.exists() and path.read_bytes() != content:
        raise ForcingContractAuditError(
            f"refusing to overwrite different bytes in {path}"
        )
    if not path.exists():
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_bytes(content)
    return _digest(content)


def main(argv: list[str] | None = None) -> int:
    """Run the audit or write one new immutable release."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--write", metavar="VERSION")
    args = parser.parse_args(argv)
    try:
        _run(args.write)
    except (OSError, ForcingContractAuditError) as exc:
        print(f"[forcing-contract] FAILED: {exc}", file=sys.stderr)
        return 1
    return 0


def _run(write_version: str | None) -> None:
    if write_version:
        digest = write_forcing_contract_artefact(write_version)
        print(f"[forcing-contract] wrote {write_version}: {digest}")
    else:
        versions = audit_forcing_contract()
        print("[forcing-contract] valid releases: " + ", ".join(versions))


if __name__ == "__main__":
    raise SystemExit(main())
