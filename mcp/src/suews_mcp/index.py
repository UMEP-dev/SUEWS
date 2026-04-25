"""Type index and lookup helpers for SUEWS MCP."""

from __future__ import annotations

import functools
from dataclasses import dataclass
from importlib.resources import files
import json
import re
from pathlib import Path
from typing import Any, Literal

Category = Literal["config", "state", "param", "output"]


@dataclass(frozen=True, slots=True)
class TypeDefinition:
    """Static metadata that maps a user-facing type to CLI schema commands."""

    type_name: str
    manifest_name: str
    category: Category
    cli_name: str
    description: str


@dataclass(frozen=True, slots=True)
class TypeEntry:
    """Resolved type metadata with manifest details."""

    type_name: str
    manifest_name: str
    category: Category
    cli_name: str
    description: str
    source: str | None
    status: str | None
    aliases: tuple[str, ...]

    def as_dict(self) -> dict[str, Any]:
        """Return serialisable dictionary for MCP responses."""
        return {
            "type_name": self.type_name,
            "manifest_name": self.manifest_name,
            "category": self.category,
            "cli_name": self.cli_name,
            "description": self.description,
            "source": self.source,
            "status": self.status,
            "aliases": list(self.aliases),
        }


TYPE_DEFINITIONS: tuple[TypeDefinition, ...] = (
    # Config
    TypeDefinition("suews-config", "SUEWS_CONFIG", "config", "suews-config", "Top-level model configuration."),
    TypeDefinition("suews-timer", "SUEWS_TIMER", "config", "suews-timer", "Simulation timing and timestep control."),
    TypeDefinition("suews-forcing", "SUEWS_FORCING", "config", "suews-forcing", "Meteorological forcing input metadata."),
    TypeDefinition("suews-site", "SUEWS_SITE", "config", "suews-site", "Site geometry, land cover, and parameter blocks."),
    # State
    TypeDefinition("ohm-state", "OHM_STATE", "state", "ohm", "Storage heat scheme runtime state."),
    TypeDefinition("hydro-state", "HYDRO_STATE", "state", "hydro", "Hydrological stores and related state variables."),
    TypeDefinition("heat-state", "HEAT_STATE", "state", "heat", "Heat-state arrays and thermal memory."),
    TypeDefinition("flag-state", "flag_STATE", "state", "flag", "Model control and diagnostic flags."),
    TypeDefinition("anthroemis-state", "anthroEmis_STATE", "state", "anthroemis", "Anthropogenic emission state terms."),
    TypeDefinition("atm-state", "atm_state", "state", "atm", "Atmospheric state variables."),
    TypeDefinition("phenology-state", "PHENOLOGY_STATE", "state", "phenology", "Vegetation phenology state."),
    TypeDefinition("snow-state", "SNOW_STATE", "state", "snow", "Snowpack state variables."),
    TypeDefinition("solar-state", "solar_State", "state", "solar", "Solar geometry and radiation state."),
    TypeDefinition("roughness-state", "ROUGHNESS_STATE", "state", "roughness", "Dynamic roughness and displacement state."),
    TypeDefinition("nhood-state", "NHOOD_STATE", "state", "nhood", "Neighbourhood canopy state for STEBBS."),
    # Param
    TypeDefinition("anthroheat-prm", "anthroHEAT_PRM", "param", "anthro-heat", "Anthropogenic heat parameter set."),
    TypeDefinition("anthroemis-prm", "anthroEMIS_PRM", "param", "anthro-emis", "Anthropogenic emission parameter set."),
    TypeDefinition(
        "building-archetype-prm",
        "BUILDING_ARCHETYPE_PRM",
        "param",
        "building-archetype",
        "Building archetype parameter definitions.",
    ),
    TypeDefinition("stebbs-prm", "STEBBS_PRM", "param", "stebbs", "STEBBS indoor/building parameterisation."),
    TypeDefinition("conductance-prm", "CONDUCTANCE_PRM", "param", "conductance", "Conductance and resistance parameters."),
    TypeDefinition("ehc-prm", "EHC_PRM", "param", "ehc", "EHC layer thermal parameter definitions."),
    TypeDefinition("spartacus-prm", "SPARTACUS_PRM", "param", "spartacus", "SPARTACUS urban radiation parameters."),
    TypeDefinition(
        "spartacus-layer-prm",
        "SPARTACUS_LAYER_PRM",
        "param",
        "spartacus-layer",
        "SPARTACUS layer-specific optical parameters.",
    ),
    TypeDefinition("bioco2-prm", "bioCO2_PRM", "param", "bioco2", "Biogenic CO2 parameterisation settings."),
    TypeDefinition("lai-prm", "LAI_PRM", "param", "lai", "Leaf area index parameters."),
    TypeDefinition("snow-prm", "SNOW_PRM", "param", "snow", "Snow process and melt parameters."),
    TypeDefinition("soil-prm", "SOIL_PRM", "param", "soil", "Soil hydraulic and thermal parameters."),
    TypeDefinition("lumps-prm", "LUMPS_PRM", "param", "lumps", "LUMPS storage heat scheme parameters."),
    TypeDefinition("ohm-coef-lc", "OHM_COEF_LC", "param", "ohm-coef-lc", "OHM coefficients by land-cover type."),
    TypeDefinition("ohm-prm", "OHM_PRM", "param", "ohm", "Core OHM parameter set."),
    TypeDefinition("lc-paved-prm", "LC_PAVED_PRM", "param", "lc-paved", "Paved-surface parameter set."),
    TypeDefinition("lc-bldg-prm", "LC_BLDG_PRM", "param", "lc-bldg", "Building-surface parameter set."),
    TypeDefinition("lc-bsoil-prm", "LC_BSOIL_PRM", "param", "lc-bsoil", "Bare-soil parameter set."),
    TypeDefinition("lc-water-prm", "LC_WATER_PRM", "param", "lc-water", "Open-water parameter set."),
    TypeDefinition("lc-dectr-prm", "LC_DECTR_PRM", "param", "lc-dectr", "Deciduous tree parameter set."),
    TypeDefinition("lc-evetr-prm", "LC_EVETR_PRM", "param", "lc-evetr", "Evergreen tree parameter set."),
    TypeDefinition("lc-grass-prm", "LC_GRASS_PRM", "param", "lc-grass", "Grass parameter set."),
    TypeDefinition("surf-store-prm", "SURF_STORE_PRM", "param", "surf-store", "Surface water storage parameters."),
    TypeDefinition("water-dist-prm", "WATER_DIST_PRM", "param", "water-dist", "Water distribution parameter set."),
    TypeDefinition("irrig-daywater", "IRRIG_daywater", "param", "irrig-daywater", "Daily irrigation water state/parameters."),
    TypeDefinition("irrigation-prm", "IRRIGATION_PRM", "param", "irrigation", "Irrigation control parameters."),
    # Output
    TypeDefinition("output-line", "output_line", "output", "line", "Single output line structure."),
    TypeDefinition("output-block", "output_block", "output", "block", "Output block data structure."),
    TypeDefinition("error-entry", "error_entry", "output", "error-entry", "Structured error record entry."),
    TypeDefinition("error-state", "error_state", "output", "error-state", "Error state collection."),
)


def _clean(value: str) -> str:
    return re.sub(r"[^a-z0-9]+", "", value.lower())


def _manifest_to_kebab(value: str) -> str:
    return value.replace("_", "-").lower()


@functools.lru_cache(maxsize=1)
def _load_default_manifest() -> dict[str, Any]:
    """Load and cache the bundled bridge manifest (read once per process)."""
    return json.loads(
        files("suews_mcp.data").joinpath("bridge-manifest.json").read_text(encoding="utf-8")
    )


def _load_manifest(manifest_path: Path | None = None) -> dict[str, Any]:
    if manifest_path is not None:
        return json.loads(manifest_path.read_text(encoding="utf-8"))
    return _load_default_manifest()


def load_entries(manifest_path: Path | None = None) -> list[TypeEntry]:
    """Load and resolve the searchable type index."""
    manifest = _load_manifest(manifest_path)
    manifest_lookup = {item["name"]: item for item in manifest.get("types", [])}

    entries: list[TypeEntry] = []
    for definition in TYPE_DEFINITIONS:
        item = manifest_lookup.get(definition.manifest_name, {})
        aliases = tuple(
            sorted(
                {
                    definition.type_name,
                    definition.cli_name,
                    definition.manifest_name,
                    _manifest_to_kebab(definition.manifest_name),
                    definition.manifest_name.lower(),
                    f"{definition.category}:{definition.cli_name}",
                    f"{definition.category}:{definition.type_name}",
                }
            )
        )
        entries.append(
            TypeEntry(
                type_name=definition.type_name,
                manifest_name=definition.manifest_name,
                category=definition.category,
                cli_name=definition.cli_name,
                description=definition.description,
                source=item.get("source"),
                status=item.get("status"),
                aliases=aliases,
            )
        )
    return entries


def build_catalogue(query: str = "", manifest_path: Path | None = None) -> dict[str, Any]:
    """Build catalogue payload grouped by category."""
    entries = load_entries(manifest_path)
    query = query.strip()
    # Treat common wildcard queries as "show everything"
    if query.lower() in ("all", "*", "everything", "list"):
        query = ""
    if query:
        terms = [term for term in re.split(r"\s+", query.lower()) if term]
        filtered: list[TypeEntry] = []
        for entry in entries:
            haystack = " ".join(
                [
                    entry.type_name,
                    entry.manifest_name,
                    entry.category,
                    entry.cli_name,
                    entry.description,
                    " ".join(entry.aliases),
                ]
            ).lower()
            if all(term in haystack for term in terms):
                filtered.append(entry)
        entries = filtered

    categories: dict[str, list[dict[str, Any]]] = {
        "config": [],
        "state": [],
        "param": [],
        "output": [],
    }
    for entry in entries:
        categories[entry.category].append(entry.as_dict())

    for value in categories.values():
        value.sort(key=lambda item: item["type_name"])

    return {
        "query": query,
        "total_types": sum(len(v) for v in categories.values()),
        "category_counts": {key: len(value) for key, value in categories.items()},
        "categories": categories,
        "tips": [
            "Use type_name to inspect one type in detail, e.g. type_name='ohm-state'.",
            "Use detail_level='schema' for machine-readable fields.",
            "Use detail_level='sample' to request example values.",
        ],
    }


def resolve_type_name(type_name: str, manifest_path: Path | None = None) -> TypeEntry:
    """Resolve user input to a concrete type entry."""
    candidate = type_name.strip()
    if not candidate:
        raise ValueError("type_name cannot be empty")

    entries = load_entries(manifest_path)
    exact_lookup = {entry.type_name.lower(): entry for entry in entries}
    exact = exact_lookup.get(candidate.lower())
    if exact is not None:
        return exact

    cleaned = _clean(candidate)
    matched: list[TypeEntry] = []
    for entry in entries:
        variants = {entry.type_name, entry.manifest_name, entry.cli_name, *entry.aliases}
        if any(_clean(value) == cleaned for value in variants):
            matched.append(entry)

    if not matched:
        known = ", ".join(sorted(entry.type_name for entry in entries))
        raise ValueError(f"Unknown type '{type_name}'. Known types: {known}")
    if len(matched) > 1:
        options = ", ".join(sorted(entry.type_name for entry in matched))
        raise ValueError(
            f"Ambiguous type '{type_name}'. Use a specific name, e.g. one of: {options}"
        )
    return matched[0]

