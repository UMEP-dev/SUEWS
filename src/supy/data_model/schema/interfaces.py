"""Published schemas and catalogues for SUEWS data interfaces."""

from __future__ import annotations

import json
from pathlib import Path
from typing import Literal

from pydantic import BaseModel, ConfigDict, Field

from ..forcing import FORCING_REGISTRY, ForcingVariable
from ..output import OUTPUT_REGISTRY, OutputVariable
from .version import CURRENT_SCHEMA_VERSION

DATA_INTERFACE_VERSION = "1.0.0"
PUBLICATION_BASE_URL = "https://suews.io"

type InterfaceKind = Literal["forcing", "output"]


class ForcingVariableCatalogue(BaseModel):
    """Versioned catalogue containing every forcing-variable definition."""

    schema_uri: str = Field(alias="$schema")
    catalogue_uri: str = Field(alias="$id")
    kind: Literal["forcing"]
    catalogue_version: str
    config_schema_version: str
    variables: tuple[ForcingVariable, ...]

    model_config = ConfigDict(extra="forbid", populate_by_name=True)


class OutputVariableCatalogue(BaseModel):
    """Versioned catalogue containing every output-variable definition."""

    schema_uri: str = Field(alias="$schema")
    catalogue_uri: str = Field(alias="$id")
    kind: Literal["output"]
    catalogue_version: str
    config_schema_version: str
    variables: tuple[OutputVariable, ...]

    model_config = ConfigDict(extra="forbid", populate_by_name=True)


def _schema_url(kind: InterfaceKind, version: str, base_url: str) -> str:
    return f"{base_url.rstrip('/')}/schemas/{kind}-variables/{version}.json"


def _catalogue_url(kind: InterfaceKind, version: str, base_url: str) -> str:
    return f"{base_url.rstrip('/')}/catalogues/{kind}-variables/{version}.json"


def _check_version(version: str | None) -> str:
    target = version or DATA_INTERFACE_VERSION
    if target != DATA_INTERFACE_VERSION:
        raise ValueError(
            f"data-interface version {target!r} is not available; "
            f"current version is {DATA_INTERFACE_VERSION!r}"
        )
    return target


def _check_kind(kind: str) -> InterfaceKind:
    if kind not in {"forcing", "output"}:
        raise ValueError(
            f"data-interface kind {kind!r} is not available; "
            "choose 'forcing' or 'output'"
        )
    return kind


def generate_data_interface_schema(
    kind: InterfaceKind,
    version: str | None = None,
    base_url: str = PUBLICATION_BASE_URL,
) -> dict:
    """Generate the JSON Schema for a forcing or output catalogue."""
    kind = _check_kind(kind)
    target = _check_version(version)
    model = ForcingVariableCatalogue if kind == "forcing" else OutputVariableCatalogue
    schema = model.model_json_schema(by_alias=True)
    schema["$schema"] = "https://json-schema.org/draft/2020-12/schema"
    schema["$id"] = _schema_url(kind, target, base_url)
    schema["version"] = target
    schema["title"] = f"SUEWS {kind.title()} Variable Catalogue Schema v{target}"
    schema["description"] = (
        f"JSON Schema for the versioned SUEWS {kind} variable catalogue."
    )
    return schema


def generate_data_interface_catalogue(
    kind: InterfaceKind,
    version: str | None = None,
    base_url: str = PUBLICATION_BASE_URL,
) -> dict:
    """Generate a versioned forcing or output variable catalogue."""
    kind = _check_kind(kind)
    target = _check_version(version)
    if kind == "forcing":
        catalogue = ForcingVariableCatalogue(**{
            "$schema": _schema_url(kind, target, base_url),
            "$id": _catalogue_url(kind, target, base_url),
            "kind": kind,
            "catalogue_version": target,
            "config_schema_version": CURRENT_SCHEMA_VERSION,
            "variables": FORCING_REGISTRY.variables,
        })
    else:
        catalogue = OutputVariableCatalogue(**{
            "$schema": _schema_url(kind, target, base_url),
            "$id": _catalogue_url(kind, target, base_url),
            "kind": kind,
            "catalogue_version": target,
            "config_schema_version": CURRENT_SCHEMA_VERSION,
            "variables": tuple(OUTPUT_REGISTRY.variables),
        })

    return catalogue.model_dump(mode="json", by_alias=True)


def export_data_interface_artifacts(
    publication_root: Path,
    version: str | None = None,
    base_url: str = PUBLICATION_BASE_URL,
) -> dict[str, dict[str, Path]]:
    """Write versioned and latest schemas and catalogues for both interfaces."""
    target = _check_version(version)
    publication_root = Path(publication_root)
    exported: dict[str, dict[str, Path]] = {}

    for kind in ("forcing", "output"):
        schema = generate_data_interface_schema(kind, target, base_url)
        catalogue = generate_data_interface_catalogue(kind, target, base_url)
        schema_dir = publication_root / "schemas" / f"{kind}-variables"
        catalogue_dir = publication_root / "catalogues" / f"{kind}-variables"
        schema_dir.mkdir(parents=True, exist_ok=True)
        catalogue_dir.mkdir(parents=True, exist_ok=True)

        schema_path = schema_dir / f"{target}.json"
        schema_latest_path = schema_dir / "latest.json"
        catalogue_path = catalogue_dir / f"{target}.json"
        catalogue_latest_path = catalogue_dir / "latest.json"
        schema_content = json.dumps(schema, indent=2) + "\n"
        catalogue_content = json.dumps(catalogue, indent=2) + "\n"
        schema_path.write_text(schema_content, encoding="utf-8")
        schema_latest_path.write_text(schema_content, encoding="utf-8")
        catalogue_path.write_text(catalogue_content, encoding="utf-8")
        catalogue_latest_path.write_text(catalogue_content, encoding="utf-8")
        exported[kind] = {
            "schema": schema_path,
            "schema_latest": schema_latest_path,
            "catalogue": catalogue_path,
            "catalogue_latest": catalogue_latest_path,
        }

    return exported
