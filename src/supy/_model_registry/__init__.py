"""SUEWS model-version registry (read-only).

Single source of truth for the release lineage: every SUEWS / supy release
characterised by its package tag, release date, YAML schema version, physics
era, install method, reproducibility status, and where its reference output
lives.

This package deliberately lives OUTSIDE ``supy.data_model`` so that edits do
not trip the schema-version-audit CI gate -- the registry is not part of the
YAML input surface.

The YAML data file (``registry.yaml``) is read and validated against
``ModelVersion`` lazily on first access (cached), so a malformed edit fails
loudly the first time the registry is queried rather than at import time.
"""

from __future__ import annotations

import re
from dataclasses import dataclass
from functools import lru_cache
from importlib.resources import files
from typing import Optional

import yaml

_ALLOWED_ERAS = {"fortran-binary", "f90wrap", "modern-fortran", "rust-bridge"}
_ALLOWED_INSTALL = {"pip", "compile", "unavailable"}
_ALLOWED_REPRO = {
    "benchmarked",
    "pip-installable",
    "legacy-external-ref",
    "documented-only",
}

# released accepts full ISO date or year-only (legacy releases where only the
# year is reliably known -- we record year-precision rather than fabricate a
# day).
_RELEASED_RE = re.compile(r"^\d{4}(-\d{2}-\d{2})?$")


@dataclass(frozen=True)
class ModelVersion:
    """One SUEWS release, fully characterised for the lineage registry."""

    tag: str
    released: str
    schema_version: Optional[str]
    physics_era: str
    install_method: str
    reproducibility: str
    reference_source: Optional[str]
    version_history_anchor: Optional[str]


def _validate(record: dict) -> ModelVersion:
    required = {
        "tag",
        "released",
        "schema_version",
        "physics_era",
        "install_method",
        "reproducibility",
        "reference_source",
        "version_history_anchor",
    }
    keys = set(record)
    missing = required - keys
    if missing:
        raise ValueError(f"registry record missing fields {sorted(missing)}: {record!r}")
    unknown = keys - required
    if unknown:
        raise ValueError(f"registry record has unknown fields {sorted(unknown)}: {record!r}")

    tag = record["tag"]
    if not isinstance(tag, str) or not tag:
        raise ValueError(f"registry tag must be a non-empty string: {record!r}")
    released = record["released"]
    if not isinstance(released, str) or not _RELEASED_RE.match(released):
        raise ValueError(f"registry released must be YYYY or YYYY-MM-DD: {record!r}")
    if record["physics_era"] not in _ALLOWED_ERAS:
        raise ValueError(f"bad physics_era for {tag}: {record['physics_era']!r}")
    if record["install_method"] not in _ALLOWED_INSTALL:
        raise ValueError(f"bad install_method for {tag}: {record['install_method']!r}")
    if record["reproducibility"] not in _ALLOWED_REPRO:
        raise ValueError(f"bad reproducibility for {tag}: {record['reproducibility']!r}")

    return ModelVersion(
        tag=tag,
        released=released,
        schema_version=record["schema_version"],
        physics_era=record["physics_era"],
        install_method=record["install_method"],
        reproducibility=record["reproducibility"],
        reference_source=record["reference_source"],
        version_history_anchor=record["version_history_anchor"],
    )


@lru_cache(maxsize=1)
def _load_raw() -> dict:
    text = (files("supy._model_registry") / "registry.yaml").read_text(encoding="utf-8")
    data = yaml.safe_load(text)
    if not isinstance(data, dict) or "versions" not in data:
        raise ValueError("registry.yaml must be a mapping with a 'versions' list")
    return data


@lru_cache(maxsize=1)
def _load_registry() -> tuple[ModelVersion, ...]:
    raw = _load_raw()
    versions = [_validate(r) for r in raw["versions"]]
    return tuple(sorted(versions, key=lambda v: (v.released, v.tag)))


def list_model_versions() -> list[ModelVersion]:
    """Return all known SUEWS releases, sorted ascending by (released, tag)."""
    return list(_load_registry())


def model_version_info(tag: str) -> ModelVersion:
    """Return the registry record for ``tag``; raise ``KeyError`` if unknown."""
    for version in _load_registry():
        if version.tag == tag:
            return version
    raise KeyError(f"unknown SUEWS model version tag: {tag!r}")


def schema_for(tag: str) -> Optional[str]:
    """Return the YAML schema version a release shipped (``None`` if pre-schema)."""
    return model_version_info(tag).schema_version


def drift_baseline_tag() -> str:
    """Return the pinned baseline release used by the drift harness."""
    raw = _load_raw()
    if "drift_baseline_tag" not in raw:
        raise ValueError("registry.yaml missing top-level 'drift_baseline_tag'")
    return raw["drift_baseline_tag"]
