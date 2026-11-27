"""YAML-to-YAML schema upgrade tool for supy configurations.

Provides a supported upgrade path between formal supy releases. When the
validator on master tightens a structural rule (internal-only fields, union
dispatch, renamed sections), add a handler here keyed by
`(from_schema_version, to_schema_version)` so existing user YAMLs keep working.

See gh#1301, gh#1304.
"""

from __future__ import annotations

import calendar
from collections.abc import Iterable, Mapping
from contextlib import suppress
from pathlib import Path
from copy import deepcopy
from dataclasses import dataclass, field
from functools import wraps
import math
import numbers
import sys
from typing import Callable, Optional, Union

import yaml

from ...data_model.core.field_renames import (
    ALL_FIELD_RENAMES,
    ARCHETYPEPROPERTIES_RENAMES,
    ARCHETYPEPROPERTIES_PASCAL_RENAMES,
    DECTRPROPERTIES_RENAMES,
    EVETRPROPERTIES_RENAMES,
    LAIPARAMS_RENAMES,
    MODELPHYSICS_DEV12_RENAMES,
    MODELPHYSICS_SUFFIX_RENAMES,
    SNOWPARAMS_RENAMES,
    SNOWPARAMS_INTERMEDIATE_RENAMES,
    ARCHETYPEPROPERTIES_DEV3_RENAMES,
    ARCHETYPEPROPERTIES_DEV6_RENAMES,
    ARCHETYPEPROPERTIES_DEV7_RENAMES,
    ARCHETYPEPROPERTIES_DEV12_RENAMES,
    CO2PARAMS_RENAMES,
    STEBBSPROPERTIES_DEV3_RENAMES,
    STEBBSPROPERTIES_DEV8_RENAMES,
    STEBBSPROPERTIES_DEV12_RENAMES,
    STEBBSPROPERTIES_RENAMES,
    SURFACEPROPERTIES_RENAMES,
    VEGETATEDSURFACEPROPERTIES_RENAMES,
    collect_and_clear_vertical_layer_optical_nulls_in_raw_config,
    rename_keys_recursive,
    strip_vertical_layer_cross_facet_keys_in_raw_config,
)
from ...data_model.core.profile import (
    TEN_MINUTE_PROFILE_ALIAS_GROUPS as _TEN_MINUTE_PROFILE_ALIAS_GROUPS,
    normalise_ten_minute_profile_key,
    normalise_ten_minute_profile_mapping,
)
from ...data_model.core.state import validate_sdd_id
from ...data_model.configuration import CURRENT_SCHEMA_VERSION
from ...data_model.configuration.migration import SchemaMigrator
from ...data_model.validation.required_fields import (
    NON_WATER_SURFACES,
    WATER_DISTRIBUTION_REQUIRED,
)
from .._forcing import SOIL_CONVERT_HINT

# Historical rename steps must retain their outgoing dev5 spelling. The new
# delta owns the soil selector rename, including for explicit intermediate stops.
ALL_FIELD_RENAMES = {key: ("soil_moisture_deficit" if value == "soil_moisture" else value)
                     for key, value in ALL_FIELD_RENAMES.items()}
MODELPHYSICS_SUFFIX_RENAMES = {
    key: ("soil_moisture_deficit" if value == "soil_moisture" else value)
    for key, value in MODELPHYSICS_SUFFIX_RENAMES.items()
    if key != "soil_moisture_deficit"
}

# ---------------------------------------------------------------------------
# Package-version -> schema-version resolver
#
# The user typically knows the supy release tag (e.g. "2026.4.3") not the
# internal schema version. This table maps release tags to the schema version
# they shipped with. Extend when a release bumps `CURRENT_SCHEMA_VERSION`.
# ---------------------------------------------------------------------------

# Retrospectively-assigned schema versions per formal release tag. The
# `SCHEMA_VERSIONS` lineage in `src/supy/data_model/configuration/version.py`
# anchors each value here — see that file's docstring for the history of
# why the schema version was frozen at "2025.12" through several structural
# changes (gh#1304) and how the retrospective audit settled on these
# labels.
#
# * `2025.12` covers the 2025.10.15 / 2025.11.20 shape (pre-#879 STEBBS
#   layout). 2025.10.15 lacks a few fields present in 2025.11.20, but
#   those are additive so both tags share the schema.
# * `2026.1` covers the 2026.1.28 shape (post-#879 STEBBS clean-up; still
#   pre-#1261 setpoint split, still carries DeepSoilTemperature and the
#   DHW volume bounds).
# * `2026.4` is the current schema (post-#1240 / #1242 / #1261).
# Historical literal kept as a fallback (and regression anchor) in case the
# shipped registry is unavailable for any reason. The live mapping below is
# DERIVED from src/supy/_model_registry/registry.yaml so the lineage has a
# single source of truth; this fallback must stay equal to the derived map.
_PACKAGE_TO_SCHEMA_FALLBACK: dict[str, str] = {
    "2025.10.15": "2025.12",
    "2025.11.20": "2025.12",
    "2026.1.28": "2026.1",
    "2026.4.3": "2026.4",
    "2026.6.5": "2026.5",
}


def _build_package_to_schema() -> dict[str, str]:
    """Derive the release-tag -> schema map from the shipped registry.

    Falls back to the historical literal if the registry cannot be imported
    (e.g. a partial install), so the upgrader never loses this mapping.
    """
    try:
        from supy._model_registry import list_model_versions

        # Build inside the try: registry.yaml is read lazily on the first call
        # to list_model_versions(), so a missing/malformed registry raises here
        # (not at import) and must also degrade to the literal fallback.
        return {
            v.tag: v.schema_version
            for v in list_model_versions()
            if v.schema_version is not None
        }
    except Exception:
        return dict(_PACKAGE_TO_SCHEMA_FALLBACK)


_PACKAGE_TO_SCHEMA: dict[str, str] = _build_package_to_schema()


def _resolve_package_to_schema(version: str) -> str:
    """Map a release tag like '2026.4.3' to its schema version.

    If the supplied string is already a known schema version, it is returned
    unchanged. Unknown strings are returned as-is so downstream dispatch can
    surface a clear 'no migration path' error rather than silently remapping.
    """
    if version in _PACKAGE_TO_SCHEMA:
        return _PACKAGE_TO_SCHEMA[version]
    return version


# ---------------------------------------------------------------------------
# Logging helper (used by handlers; defined before handler module-level
# constants so any import-time wiring stays self-contained).
# ---------------------------------------------------------------------------


def _log(message: str) -> None:
    # Dereference sys.stderr at call time so pytest capsys / CliRunner can
    # intercept. Binding it in the signature would cache the original
    # stream and bypass capture fixtures.
    print(message, file=sys.stderr)


# ---------------------------------------------------------------------------
# Generic handler utilities
# ---------------------------------------------------------------------------

Handler = Callable[[dict], dict]


def _walk_site_container(cfg: dict, name: str) -> Iterable[dict]:
    """Yield each `sites[*].properties.<name>` mapping in `cfg`.

    Defensively skips sites or properties that are missing or non-dict so
    handlers can iterate without re-implementing the same guards. STEBBS
    parameters split across `building_archetype` (wall/roof, setpoints) and
    `stebbs` (DHW volumes, soil temperature, emissivity), so migration
    deltas are expressed per container.
    """
    for site in cfg.get("sites", []) or []:
        if not isinstance(site, dict):
            continue
        props = site.get("properties")
        if not isinstance(props, dict):
            continue
        container = props.get(name)
        if isinstance(container, dict):
            yield container


def _rename_field(arch: dict, old_name: str, new_name: str) -> bool:
    """Rename `old_name` to `new_name` in-place on `arch`.

    Logs the rename when it fires (so the user sees their value followed
    the rename instead of disappearing). Returns True iff a rename happened.
    No-op when `old_name` is absent. If `new_name` already exists, the
    user-supplied `new_name` wins (fresh value preserved over stale one).
    """
    if old_name not in arch:
        return False
    value = arch.pop(old_name)
    if new_name in arch:
        _log(
            f"[yaml-upgrade]   rename {old_name!r} -> {new_name!r} skipped "
            f"(target already present, dropping stale {old_name!r} value)"
        )
        return False
    arch[new_name] = value
    _log(f"[yaml-upgrade]   renamed {old_name!r} -> {new_name!r}")
    return True


def _apply_co2params_renames(cfg: dict) -> None:
    """Apply the gh#1688 CO2Params naming-convention completion."""
    for emissions in _walk_site_container(cfg, "anthropogenic_emissions"):
        co2 = emissions.get("co2")
        if not isinstance(co2, dict):
            continue
        for old_name, new_name in CO2PARAMS_RENAMES.items():
            _rename_field(co2, old_name, new_name)


def _drop_obsolete_field(arch: dict, name: str, reason: str) -> bool:
    """Drop `name` from `arch` and log the removal with `reason`.

    Use when the current schema no longer carries the field at all. Logging
    is the contract: the alternative (Pydantic `extra="ignore"`) silently
    swallows user data without trace, which is exactly the bug this helper
    exists to make impossible.
    """
    if name not in arch:
        return False
    arch.pop(name)
    _log(f"[yaml-upgrade]   dropped {name!r} ({reason})")
    return True


# ---------------------------------------------------------------------------
# Migration field tables
#
# Each table is a sequence of (source_field, ...) tuples consumed by the
# helpers above. Keeping them at module level makes the schema deltas
# auditable in one place and reusable across handlers that share a delta.
# ---------------------------------------------------------------------------

# 2025.12 -> 2026.1 (the #879 "clean-up of STEBBS parameters" delta, Nov 2025):
#
# `building_archetype`:
#   * Wallx1 -> WallOuterCapFrac, Roofx1 -> RoofOuterCapFrac (commits 770a7db9,
#     45b512b6).
#
# `stebbs`:
#   * IndoorAirStartTemperature -> InitialIndoorTemperature
#     OutdoorAirStartTemperature -> InitialOutdoorTemperature (semantic
#     renames in #879).
#   * DHWVesselEmissivity dropped (commit ae53cd31d, "clean up of dead code").
#   * Runtime-state / view-factor / start-temperature fields moved out of
#     user input (#879); kept here as explicit drops with a generic reason
#     so users see their values have not silently vanished.
_ARCH_RENAMES_2025_12_TO_2026_1: tuple[tuple[str, str], ...] = (
    ("Wallx1", "WallOuterCapFrac"),
    ("Roofx1", "RoofOuterCapFrac"),
)
_STEBBS_RENAMES_2025_12_TO_2026_1: tuple[tuple[str, str], ...] = (
    ("IndoorAirStartTemperature", "InitialIndoorTemperature"),
    ("OutdoorAirStartTemperature", "InitialOutdoorTemperature"),
)
_STEBBS_DROPS_2025_12_TO_2026_1: tuple[tuple[str, str], ...] = (
    (
        "DHWVesselEmissivity",
        "removed during STEBBS clean-up Nov 2025 (commit ae53cd31d)",
    ),
    # Physical constants previously exposed as user input, now internal.
    ("IndoorAirDensity", "moved to internal constant (#879)"),
    ("IndoorAirCp", "moved to internal constant (#879)"),
    # STEBBS view-factor fields absorbed into the runtime solver.
    ("WallBuildingViewFactor", "moved to runtime solver state (#879)"),
    ("WallGroundViewFactor", "moved to runtime solver state (#879)"),
    ("WallSkyViewFactor", "moved to runtime solver state (#879)"),
    # Legacy initial-mass temperature: superseded by InitialIndoorTemperature.
    ("IndoorMassStartTemperature", "merged into InitialIndoorTemperature (#879)"),
    # Runtime-state surface temperatures (now solver-driven, not user input).
    ("WallIndoorSurfaceTemperature", "moved to runtime solver state (#879)"),
    ("WallOutdoorSurfaceTemperature", "moved to runtime solver state (#879)"),
    ("RoofIndoorSurfaceTemperature", "moved to runtime solver state (#879)"),
    ("RoofOutdoorSurfaceTemperature", "moved to runtime solver state (#879)"),
    ("GroundFloorIndoorSurfaceTemperature", "moved to runtime solver state (#879)"),
    ("GroundFloorOutdoorSurfaceTemperature", "moved to runtime solver state (#879)"),
    ("WindowIndoorSurfaceTemperature", "moved to runtime solver state (#879)"),
    ("WindowOutdoorSurfaceTemperature", "moved to runtime solver state (#879)"),
    ("InternalWallDHWVesselTemperature", "moved to runtime solver state (#879)"),
    ("ExternalWallDHWVesselTemperature", "moved to runtime solver state (#879)"),
    ("InternalWallWaterTankTemperature", "moved to runtime solver state (#879)"),
    ("ExternalWallWaterTankTemperature", "moved to runtime solver state (#879)"),
    ("WaterTankTemperature", "moved to runtime solver state (#879)"),
    # Replaced by profile-based fields in #1038.
    ("ApplianceUsageFactor", "replaced by ApplianceProfile (#1038)"),
    ("TotalNumberofAppliances", "replaced by ApplianceProfile (#1038)"),
    ("DHWDrainFlowRate", "replaced by HotWaterFlowProfile (#1038)"),
    (
        "DomesticHotWaterTemperatureInUseInBuilding",
        "removed during STEBBS clean-up (#879)",
    ),
    (
        "OutdoorAirAnnualTemperature",
        "no longer a user input (#879)",
    ),
)

# 2026.1 -> 2026.4 (current):
# * #1240 renamed `stebbs.DeepSoilTemperature` to
#   `stebbs.AnnualMeanAirTemperature`,
# * #1242 removed `stebbs.MinimumVolumeOfDHWinUse` and
#   `stebbs.MaximumVolumeOfDHWinUse`,
# * #1261 split `building_archetype.HeatingSetpointTemperature` /
#   `CoolingSetpointTemperature` into scalar + `*Profile` (handled
#   separately via `_split_profile_fields` below).
# * Additional drops from the same window (#1245 / replaced by new
#   profile-based inputs).
_STEBBS_RENAMES_2026_1_TO_CURRENT: tuple[tuple[str, str], ...] = (
    ("DeepSoilTemperature", "AnnualMeanAirTemperature"),
)
_STEBBS_DROPS_2026_1_TO_CURRENT: tuple[tuple[str, str], ...] = (
    ("MinimumVolumeOfDHWinUse", "removed in #1242"),
    ("MaximumVolumeOfDHWinUse", "removed in #1242"),
    ("ApplianceRating", "replaced by lighting/metabolism profiles"),
    ("MetabolicRate", "replaced by MetabolismProfile"),
)
_ARCH_DROPS_2026_1_TO_CURRENT: tuple[tuple[str, str], ...] = (
    ("OccupantsProfile", "replaced by MetabolismProfile"),
)

_PROFILE_RENAME_PAIRS: tuple[tuple[str, float], ...] = (
    ("HeatingSetpointTemperature", 18.0),
    ("CoolingSetpointTemperature", 27.0),
)

# Category 5 of #1256 (gh#1327), shipping under the 2026.5.dev1 schema
# label — the first dev bump after 2026.5 Category 1, following the
# dev-label convention in `.claude/rules/python/schema-versioning.md`:
# eight STEBBS ArchetypeProperties fields with the fused `ext` fragment
# renamed to the spelt-out `External` form. Declared inline (not sourced
# from ARCHETYPEPROPERTIES_RENAMES after gh#1334) because ARCHETYPEPROPERTIES_RENAMES
# now maps directly to the 2026.5.dev3 snake_case final — the Cat 5 stop
# at the intermediate PascalCase shape has to live somewhere.
_ARCH_EXT_RENAMES_2026_5_TO_DEV1: tuple[tuple[str, str], ...] = (
    ("WallextThickness", "WallExternalThickness"),
    ("WallextEffectiveConductivity", "WallExternalEffectiveConductivity"),
    ("WallextDensity", "WallExternalDensity"),
    ("WallextCp", "WallExternalCp"),
    ("RoofextThickness", "RoofExternalThickness"),
    ("RoofextEffectiveConductivity", "RoofExternalEffectiveConductivity"),
    ("RoofextDensity", "RoofExternalDensity"),
    ("RoofextCp", "RoofExternalCp"),
)

# Category 1 pairs only — fused -> Cat-1 snake_case-with-suffix (e.g.
# `netradiationmethod` -> `net_radiation_method`). This mapping is
# declared inline (not imported from field_renames.py) because the
# Python registry's ``MODELPHYSICS_RENAMES`` now short-circuits straight
# to the Cat 2+3 final (``net_radiation`` etc.). The migrator still
# needs an explicit Cat 1 stop when a caller pins
# ``--target-version 2026.5``. ARCHETYPEPROPERTIES_RENAMES is excluded
# — those land separately via `_apply_arch_ext_renames` so each rename
# emits its own log line (TestNoSilentFieldDrops gate).
_MODELPHYSICS_CAT1_RENAMES: dict[str, str] = {
    "netradiationmethod": "net_radiation_method",
    "emissionsmethod": "emissions_method",
    "storageheatmethod": "storage_heat_method",
    "ohmincqf": "ohm_inc_qf",
    "roughlenmommethod": "roughness_length_momentum_method",
    "roughlenheatmethod": "roughness_length_heat_method",
    "stabilitymethod": "stability_method",
    "smdmethod": "smd_method",
    "waterusemethod": "water_use_method",
    "rslmethod": "rsl_method",
    "faimethod": "fai_method",
    "rsllevel": "rsl_level",
    "gsmodel": "gs_model",
    "snowuse": "snow_use",
    "stebbsmethod": "stebbs_method",
    "rcmethod": "rc_method",
}
# Snow Category 1 pairs only — fused -> Cat 1 snake_case (e.g.
# `preciplimit` -> `precip_limit`). Declared inline because
# SNOWPARAMS_RENAMES now short-circuits straight to the 2026.5.dev3 final
# (`preciplimit` -> `temperature_rain_snow_threshold` etc.); the migrator
# still needs an explicit Cat 1 stop when a caller pins
# ``--target-version 2026.5``.
_SNOWPARAMS_CAT1_RENAMES: dict[str, str] = {
    "crwmax": "water_holding_capacity_max",
    "crwmin": "water_holding_capacity_min",
    "preciplimit": "precip_limit",
    "preciplimitalb": "precip_limit_albedo",
    "snowalbmax": "snow_albedo_max",
    "snowalbmin": "snow_albedo_min",
    "snowdensmin": "snow_density_min",
    "snowdensmax": "snow_density_max",
    "snowlimbldg": "snow_limit_building",
    "snowlimpaved": "snow_limit_paved",
    "tempmeltfact": "temp_melt_factor",
    "radmeltfact": "rad_melt_factor",
}

_CATEGORY1_ONLY_RENAMES: dict[str, str] = {
    **_MODELPHYSICS_CAT1_RENAMES,
    **SURFACEPROPERTIES_RENAMES,
    **LAIPARAMS_RENAMES,
    **VEGETATEDSURFACEPROPERTIES_RENAMES,
    **EVETRPROPERTIES_RENAMES,
    **DECTRPROPERTIES_RENAMES,
    **_SNOWPARAMS_CAT1_RENAMES,
}

# Categories 2 + 3 of #1256 (gh#1321), shipping under the 2026.5.dev2
# schema label: 15 ModelPhysics fields with the redundant `_method` /
# `_model` suffix stripped and/or domain abbreviations expanded. Sourced
# from MODELPHYSICS_SUFFIX_RENAMES so field_renames.py stays the single
# registry for the Pydantic shim. The extra ``(setpointmethod, setpoint)``
# pair is appended because the Schema 2026.5 shape still carries the
# fused ``setpointmethod`` key (missed by Category 1) — only the 2026.5
# -> 2026.5.dev2 migrator needs to rewrite it since the Pydantic shim
# already handles the fused -> final case via MODELPHYSICS_RENAMES.
# Every rename emits its own log line (TestNoSilentFieldDrops gate).
_MODELPHYSICS_SUFFIX_RENAMES_TABLE: tuple[tuple[str, str], ...] = tuple(
    MODELPHYSICS_SUFFIX_RENAMES.items()
) + (("setpointmethod", "setpoint"),)

_STEBBS_MASTER_ALIAS_KEYS: tuple[str, ...] = (
    "stebbs",
    "stebbsmethod",
    "stebbs_method",
)


def _stebbs_master_aliases(physics: dict) -> list[str]:
    """Return present flat STEBBS master-toggle spellings."""
    return [key for key in _STEBBS_MASTER_ALIAS_KEYS if key in physics]


def _reject_duplicate_stebbs_master_aliases(physics: dict) -> list[str]:
    """Reject ambiguous STEBBS master-toggle aliases and return the winner."""
    aliases = _stebbs_master_aliases(physics)
    if len(aliases) > 1:
        raise YamlUpgradeError(
            "Multiple legacy STEBBS master aliases "
            f"({', '.join(aliases)}) are present. Use only one spelling."
        )
    return aliases


# gh#1334 (2026.5.dev2 -> 2026.5.dev3): convert STEBBS PascalCase to snake_case
# on the full user-facing YAML surface plus opaque-abbreviation clean-ups in
# SnowParams. Three tables — one per container affected under `site.properties`.
# Sourced from the class dicts in field_renames.py so that registry stays the
# single authoritative mapping.
_ARCH_RENAMES_DEV2_TO_DEV3: tuple[tuple[str, str], ...] = tuple(
    # Iterate only over the PascalCase entries (skip the pre-gh#1327 `Wallext`
    # legacy fused keys — a 2026.5.dev2 YAML has already been upgraded past
    # those through the Cat 5 intermediate handler, so they will not appear
    # here). The `WallExternal*` / `RoofExternal*` pairs come from
    # ARCHETYPEPROPERTIES_PASCAL_RENAMES; everything else is in
    # ARCHETYPEPROPERTIES_RENAMES keyed by its own PascalCase form.
    (old, new)
    for old, new in ARCHETYPEPROPERTIES_RENAMES.items()
    if not (old.startswith("Wallext") or old.startswith("Roofext"))
) + tuple(ARCHETYPEPROPERTIES_PASCAL_RENAMES.items())

_STEBBS_RENAMES_DEV2_TO_DEV3: tuple[tuple[str, str], ...] = tuple(
    STEBBSPROPERTIES_RENAMES.items()
)

# SnowParams: the six pre-existing intermediate renames (precip_limit ->
# temperature_rain_snow_threshold etc.) PLUS the five new snake_case
# renames for fields that had no fused predecessor (tau_a -> tau_cold_snow,
# etc.). A 2026.5.dev2 YAML carries the intermediate snake_case keys, not
# the fused legacy, so the migrator sources from
# SNOWPARAMS_INTERMEDIATE_RENAMES for the first six and picks out the
# "self-keyed" entries in SNOWPARAMS_RENAMES for the remaining five.
_SNOW_RENAMES_DEV2_TO_DEV3: tuple[tuple[str, str], ...] = tuple(
    SNOWPARAMS_INTERMEDIATE_RENAMES.items()
) + (
    ("tau_a", "tau_cold_snow"),
    ("tau_f", "tau_melting_snow"),
    ("tau_r", "tau_refreezing_snow"),
    ("snowprof_24hr", "snow_profile_24hr"),
    ("narp_emis_snow", "narp_emissivity_snow"),
)

# gh#1334 follow-through (2026.5.dev3 -> 2026.5.dev4): unify the STEBBS
# hot-water subsystem under a single `hot_water_*` prefix. Drops the
# opaque `dhw_` acronym and the split `water_tank_*` sibling so the
# YAML surface carries one prefix with `_tank_` / `_vessel_` component
# qualifiers. Sourced from the intermediate dicts in field_renames.py
# so that registry remains the single authoritative mapping.
_ARCH_RENAMES_DEV3_TO_DEV4: tuple[tuple[str, str], ...] = tuple(
    ARCHETYPEPROPERTIES_DEV3_RENAMES.items()
)

_STEBBS_RENAMES_DEV3_TO_DEV4: tuple[tuple[str, str], ...] = tuple(
    STEBBSPROPERTIES_DEV3_RENAMES.items()
)

# Schema 2026.5.dev6 -> 2026.5.dev7: apply Rule 2 of the SUEWS naming
# convention to ArchetypeProperties bulk-material and surface optical fields
# (44 renames). Pure key reorder; no data transformation. Sourced from the
# canonical dict in field_renames.py.
_ARCH_RENAMES_DEV6_TO_DEV7: tuple[tuple[str, str], ...] = tuple(
    ARCHETYPEPROPERTIES_DEV6_RENAMES.items()
)


# Schema 2026.5.dev10 -> 2026.5.dev11: naming-convention completion for the
# STEBBS building model, combining two independent rename groups in a single
# bump (gh#1392 + gh#1394). ArchetypeProperties Tier-1 completion (16 renames:
# archetype_* namespace, area_*/ratio_* geometry, power_*/temperature_air_*/
# volume_hot_water_tank/profile_* HVAC) on the `building_archetype` container, and
# StebbsProperties Rule-2 reorder (44 renames) on the `stebbs` container. Both
# are pure key reorders sourced from the canonical dicts in field_renames.py.
_ARCH_RENAMES_TIER1_TO_DEV11: tuple[tuple[str, str], ...] = tuple(
    ARCHETYPEPROPERTIES_DEV7_RENAMES.items()
)
_STEBBS_RENAMES_RULE2_TO_DEV11: tuple[tuple[str, str], ...] = tuple(
    STEBBSPROPERTIES_DEV8_RENAMES.items()
)

# Fields dropped at dev10 -> dev11 (D. Hertwig col D, 2026-05). Each drop carries
# a reason so the removal is logged rather than swallowed by Pydantic
# ``extra="ignore"`` (``TestNoSilentFieldDrops`` enforces this).
_ARCH_DROPS_TO_DEV11: tuple[tuple[str, str], ...] = (
    (
        "building_type",
        "unused by the STEBBS kernel (hardcoded to 'None'); removed per the "
        "Reading STEBBS team review (gh#1392)",
    ),
    (
        # Pre-gh#1334 PascalCase spelling: older YAMLs reach the dev10 -> dev11
        # step still carrying `BuildingType` (no longer snaked, since the rename
        # entry was removed with the field). Drop it under the same reason.
        "BuildingType",
        "unused by the STEBBS kernel (hardcoded to 'None'); removed per the "
        "Reading STEBBS team review (gh#1392)",
    ),
)

# StebbsProperties fields dropped at dev10 -> dev11 (D. Hertwig col D, 2026-05).
# The two hot-water-tank view factors were dead inputs: the STEBBS radiative
# transfer hardcodes BVF_tank=0.0 / MVF_tank=1.0 and never read them.
_STEBBS_DROPS_TO_DEV11: tuple[tuple[str, str], ...] = (
    (
        "hot_water_tank_building_wall_view_factor",
        "dead input - STEBBS radiative transfer hardcodes the tank-wall view "
        "factor (BVF_tank=0.0); never consumed (gh#1392)",
    ),
    (
        "hot_water_tank_internal_mass_view_factor",
        "dead input - STEBBS radiative transfer hardcodes the tank-internal-mass "
        "view factor (MVF_tank=1.0); never consumed (gh#1392)",
    ),
    (
        "HotWaterTankBuildingWallViewFactor",
        "dead input - STEBBS radiative transfer hardcodes the tank-wall view "
        "factor (BVF_tank=0.0); never consumed (gh#1392)",
    ),
    (
        "HotWaterTankInternalMassViewFactor",
        "dead input - STEBBS radiative transfer hardcodes the tank-internal-mass "
        "view factor (MVF_tank=1.0); never consumed (gh#1392)",
    ),
)

# Schema 2026.5.dev11 -> 2026.5.dev12: align STEBBS and Archetype field names
# with the Reading STEBBS team's "Column D" naming (gh#1392 follow-up;
# D. Hertwig / S. Rognone, 2026-05). Pure snake->snake key reorders. The ten
# StebbsProperties renames apply to the `stebbs` container; the six
# ArchetypeProperties renames (qualifier-first powers + setpoints/profiles)
# apply to the `building_archetype` container. Sourced directly from the two
# DEV12 rename dicts so the migrator, the Pydantic shim, and the raw-YAML
# precheck path all draw from a single source of truth.
_STEBBS_RENAMES_STRAGGLERS_TO_DEV12: tuple[tuple[str, str], ...] = tuple(
    STEBBSPROPERTIES_DEV12_RENAMES.items()
)
_ARCH_RENAMES_COLUMN_D_TO_DEV12: tuple[tuple[str, str], ...] = tuple(
    ARCHETYPEPROPERTIES_DEV12_RENAMES.items()
)


# ---------------------------------------------------------------------------
# Handlers
# ---------------------------------------------------------------------------


def _strip_internal_only_fields(cfg: dict) -> dict:
    """Drop the private `_yaml_path` / `_auto_generate_annotated` stragglers.

    These are session-only fields set by `SUEWSConfig.from_yaml`; any YAML
    produced by a pre-#1289 release that still carries them would be rejected
    by the current validator. Safe to run on every upgrade path as a no-op
    when absent.
    """
    cfg.pop("_yaml_path", None)
    cfg.pop("_auto_generate_annotated", None)
    return cfg


def _identity(cfg: dict) -> dict:
    """Return the config after the defensive strip (schema version matches)."""
    validate_active_soil_depth_raw(cfg)
    cfg = _strip_internal_only_fields(cfg)
    for arch in _walk_site_container(cfg, "building_archetype"):
        for name, reason in _ARCH_DROPS_TO_DEV11:
            _drop_obsolete_field(arch, name, reason)
    for stebbs in _walk_site_container(cfg, "stebbs"):
        for name, reason in _STEBBS_DROPS_TO_DEV11:
            _drop_obsolete_field(stebbs, name, reason)
    return cfg


def _split_profile_fields(building_archetype: dict) -> bool:
    """Split pre-#1261 profile-shaped setpoint fields into scalar + Profile.

    Before PR #1261, `HeatingSetpointTemperature` / `CoolingSetpointTemperature`
    held a `{working_day, holiday}` day-profile dict. After the split the
    scalar field carries a single temperature and the schedule moves to a new
    `*Profile` sibling. This function keeps the user's hourly values intact by
    relocating them and synthesising a scalar from the first working-day
    value (falling back to a schema-default if the profile is absent).

    Returns True iff at least one profile-shaped field was actually relocated
    into a `*Profile` sibling. Callers use this signal to decide whether the
    upgraded config should be flipped to `setpointmethod = 2` (SCHEDULED):
    pre-2026.1 YAMLs only carry scalar setpoints, and flipping their
    `setpointmethod` silently replaces the user's constant setpoints with the
    off-default profiles baked into `HeatingSetpointProfile` /
    `CoolingSetpointProfile` (heating 0 C, cooling 100 C).
    """
    if not isinstance(building_archetype, dict):
        return False
    split_any = False
    for name, default_scalar in _PROFILE_RENAME_PAIRS:
        old = building_archetype.get(name)
        if not isinstance(old, dict):
            continue
        if "working_day" not in old and "holiday" not in old:
            continue
        building_archetype[name + "Profile"] = old
        scalar = default_scalar
        working_day = old.get("working_day")
        if isinstance(working_day, dict) and working_day:
            first_value = next(iter(working_day.values()), None)
            if isinstance(first_value, (int, float)):
                scalar = float(first_value)
        building_archetype[name] = {"value": scalar}
        _log(
            f"[yaml-upgrade]   split {name!r} profile -> {name + 'Profile'!r} "
            f"(scalar seeded to {scalar})"
        )
        split_any = True
    return split_any


def _migrate_2025_12_to_2026_1(cfg: dict) -> dict:
    """Bring a 2025.12-shaped YAML to the 2026.1 shape (the #879 delta).

    Applies the `building_archetype.Wallx1`/`Roofx1` -> `*OuterCapFrac`
    renames and the STEBBS clean-up that ran on 2025-11-26 (semantic
    renames to `InitialIndoorTemperature` / `InitialOutdoorTemperature`,
    plus drops of DHWVesselEmissivity, the view-factor fields, the
    surface-temperature runtime state, the split-profile precursors, and
    the physical-constant slots moved internal).

    2025.10.15 YAMLs are a subset of 2025.11.20 YAMLs (only additive
    differences between them), so both route through this handler and
    pick up the same delta; rename/drop helpers no-op when the source
    field is absent.
    """
    cfg = _strip_internal_only_fields(cfg)
    for arch in _walk_site_container(cfg, "building_archetype"):
        for old, new in _ARCH_RENAMES_2025_12_TO_2026_1:
            _rename_field(arch, old, new)
    for stebbs in _walk_site_container(cfg, "stebbs"):
        for old, new in _STEBBS_RENAMES_2025_12_TO_2026_1:
            _rename_field(stebbs, old, new)
        for name, reason in _STEBBS_DROPS_2025_12_TO_2026_1:
            _drop_obsolete_field(stebbs, name, reason)
    for land_cover in _walk_site_container(cfg, "land_cover"):
        for surface_name in ("evetr", "dectr", "grass"):
            surface = land_cover.get(surface_name)
            if isinstance(surface, dict):
                _drop_obsolete_field(
                    surface,
                    "alb",
                    "replaced by alb_min/alb_max and initial-state alb_id",
                )
    return cfg


def _migrate_2026_1_to_current(cfg: dict) -> dict:
    """Upgrade 2026.1-shaped YAMLs to the current `2026.4` schema.

    Covers the structural deltas that landed between 2026.1.28 and 2026.4.3:

    * #1240 renamed `stebbs.DeepSoilTemperature` to
      `stebbs.AnnualMeanAirTemperature`,
    * #1242 removed `stebbs.MinimumVolumeOfDHWinUse` and
      `stebbs.MaximumVolumeOfDHWinUse`,
    * #1261 split the STEBBS heating/cooling setpoint fields under
      `building_archetype` into a scalar plus a `*Profile` sibling and
      gated the schedule on `model.physics.setpointmethod`.

    When the input carries profile-shaped setpoints (2026.1 source), the
    handler relocates the hourly values into the new `*Profile` keys (seeding
    the scalar from the first working-day entry) and sets
    `setpointmethod = 2` (SCHEDULED) so the migrated run honours the supplied
    schedule. When the input carries only scalar setpoints (the chained
    2025.12 path — see `_migrate_2025_12_to_current`), `setpointmethod` is
    left at its default of 0 (CONSTANT) so the user-supplied scalars continue
    to drive the run; flipping to SCHEDULED here would null the scalars in
    Phase B validation and replace them with the off-default profiles
    (heating 0 C, cooling 100 C), silently disabling heating/cooling.
    """
    cfg = _strip_internal_only_fields(cfg)
    any_profile_split = False
    for arch in _walk_site_container(cfg, "building_archetype"):
        if _split_profile_fields(arch):
            any_profile_split = True
    for stebbs in _walk_site_container(cfg, "stebbs"):
        for old, new in _STEBBS_RENAMES_2026_1_TO_CURRENT:
            _rename_field(stebbs, old, new)
        for name, reason in _STEBBS_DROPS_2026_1_TO_CURRENT:
            _drop_obsolete_field(stebbs, name, reason)
    if any_profile_split:
        physics = cfg.setdefault("model", {}).setdefault("physics", {})
        physics["setpointmethod"] = {"value": 2}
    return cfg


def _apply_arch_ext_renames(cfg: dict) -> None:
    """Rename the eight STEBBS ext fields in place with per-key log lines.

    Using ``_rename_field`` (rather than ``rename_keys_recursive``) on the
    ``building_archetype`` container is what makes each rename visible to
    the ``TestNoSilentFieldDrops`` audit — silent transforms via the
    generic recursive helper would fail that gate.
    """
    for arch in _walk_site_container(cfg, "building_archetype"):
        for old, new in _ARCH_EXT_RENAMES_2026_5_TO_DEV1:
            _rename_field(arch, old, new)


def _apply_modelphysics_suffix_renames(cfg: dict) -> None:
    """Apply the 15 Category 2+3 renames under ``model.physics`` in place.

    Per-field logging via ``_rename_field`` keeps each rename visible to
    the ``TestNoSilentFieldDrops`` audit (the same reason the ext rename
    uses this helper rather than ``rename_keys_recursive``).
    """
    model = cfg.get("model")
    if not isinstance(model, dict):
        return
    physics = model.get("physics")
    if not isinstance(physics, dict):
        return
    _reject_duplicate_stebbs_master_aliases(physics)
    leaf_conflicts = _stebbs_leaf_alias_conflicts(physics)
    if leaf_conflicts:
        raise YamlUpgradeError(
            "Multiple flat STEBBS physics switches map to the same nested leaf "
            f"({_format_stebbs_leaf_alias_conflicts(leaf_conflicts)}). Use only "
            "one spelling."
        )
    for old, new in _MODELPHYSICS_SUFFIX_RENAMES_TABLE:
        _rename_field(physics, old, new)


# gh#1456: dev12 -> dev13 STEBBS flat -> nested fold.
#
# The five non-master STEBBS switches move under model.physics.stebbs at the
# leaf names below; the master toggle (`stebbs`, a tri-state integer) is
# decomposed into stebbs.enabled + stebbs.parameters. Renames flow through
# _rename_field so each move is logged (TestNoSilentFieldDrops enforces this).
# When this fold runs after the dev11 -> dev12 Column D straggler renames the
# flat capacitance key is already spelled `capacitance`; the older
# `outer_cap_fraction` / fused `rcmethod` spellings are accepted too so a
# hand-written legacy YAML still folds. All three relocate to stebbs.capacitance.
_STEBBS_PHYSICS_LEAF_RENAMES_TO_DEV12: tuple[tuple[str, str], ...] = (
    ("capacitance", "capacitance"),
    ("outer_cap_fraction", "capacitance"),
    ("rcmethod", "capacitance"),
    ("rc_method", "capacitance"),
    ("setpoint", "setpoint"),
    ("setpointmethod", "setpoint"),
    ("same_albedo_wall", "same_albedo_wall"),
    ("same_albedo_roof", "same_albedo_roof"),
    ("same_emissivity_wall", "same_emissivity_wall"),
    ("same_emissivity_roof", "same_emissivity_roof"),
)


def _stebbs_flat_leaf_siblings(physics: dict) -> list[str]:
    """Return flat STEBBS leaf keys present beside a nested ``stebbs`` block."""
    return sorted(
        {
            old_key
            for old_key, _leaf in _STEBBS_PHYSICS_LEAF_RENAMES_TO_DEV12
            if old_key in physics
        }
    )


def _stebbs_leaf_alias_conflicts(physics: dict) -> list[tuple[str, list[str]]]:
    """Return flat STEBBS alias groups that would collide after folding."""
    present_by_leaf: dict[str, list[str]] = {}
    for old_key, nested_leaf in _STEBBS_PHYSICS_LEAF_RENAMES_TO_DEV12:
        if old_key in physics:
            present_by_leaf.setdefault(nested_leaf, []).append(old_key)
    return [
        (leaf, sorted(keys))
        for leaf, keys in sorted(present_by_leaf.items())
        if len(keys) > 1
    ]


def _format_stebbs_leaf_alias_conflicts(
    conflicts: list[tuple[str, list[str]]],
) -> str:
    """Format colliding flat STEBBS aliases for migration errors."""
    return "; ".join(
        f"{', '.join(keys)} -> stebbs.{leaf}" for leaf, keys in conflicts
    )


def _decompose_stebbs_master_value(entry):
    """Decompose a legacy flat `stebbs` master toggle into (enabled, params).

    Returns RefValue-wrapped scalars. 0 -> (False, 1); 1 -> (True, 1);
    2 -> (True, 2).
    """
    ref = entry.get("ref") if isinstance(entry, dict) else None

    def _wrapped(value, *, carry_ref=False):
        wrapped = {"value": value}
        if carry_ref and ref is not None:
            wrapped["ref"] = ref
        return wrapped

    if isinstance(entry, dict) and "value" not in entry:
        raise ValueError(
            "Legacy 'stebbs' mappings must use a 'value' key; use "
            "'stebbs.enabled' / 'stebbs.parameters' for the nested form."
        )

    raw = entry.get("value") if isinstance(entry, dict) and "value" in entry else entry
    if isinstance(raw, bool):
        code = int(raw)
    elif isinstance(raw, int):
        code = raw
    elif isinstance(raw, float) and raw.is_integer():
        code = int(raw)
    else:
        raise ValueError(
            "Legacy 'stebbs' master toggle expects integer 0, 1, or 2."
        )
    if code == 0:
        return _wrapped(False, carry_ref=True), _wrapped(1)
    if code == 1:
        return _wrapped(True, carry_ref=True), _wrapped(1)
    if code == 2:
        return _wrapped(True, carry_ref=True), _wrapped(2)
    raise ValueError("Legacy 'stebbs' master toggle expects integer 0, 1, or 2.")


def _apply_stebbs_physics_fold(cfg: dict) -> None:
    """Fold the flat STEBBS physics switches under model.physics.stebbs in place.

    gh#1456 dev12 -> dev13. Decomposes the legacy `stebbs` master toggle into
    `stebbs.enabled` + `stebbs.parameters`, and moves outer_cap_fraction /
    rcmethod -> stebbs.capacitance, setpoint / setpointmethod -> stebbs.setpoint,
    and the four same_* switches under the nested object. No-op when the YAML is
    already nested (the master toggle's keys are enabled/parameters/...). Each
    move is logged via _rename_field; no silent drops.
    """
    model = cfg.get("model")
    if not isinstance(model, dict):
        return
    physics = model.get("physics")
    if not isinstance(physics, dict):
        return

    master_aliases = _reject_duplicate_stebbs_master_aliases(physics)
    master_alias = master_aliases[0] if master_aliases else None
    existing = physics.get("stebbs")
    nested_keys = {
        "enabled",
        "parameters",
        *[old for old, _new in _STEBBS_PHYSICS_LEAF_RENAMES_TO_DEV12],
        *[new for _old, new in _STEBBS_PHYSICS_LEAF_RENAMES_TO_DEV12],
    }
    already_nested = isinstance(existing, dict) and any(
        k in existing for k in nested_keys
    )

    if already_nested:
        flat_conflicts = _stebbs_flat_leaf_siblings(physics)
        if flat_conflicts:
            raise YamlUpgradeError(
                "Both nested 'stebbs' and flat STEBBS physics switches "
                f"({', '.join(flat_conflicts)}) are present. Use only the "
                "nested 'stebbs' form."
            )
        stebbs_block = existing
    else:
        stebbs_block = {}
        if master_alias is not None:
            enabled, parameters = _decompose_stebbs_master_value(
                physics.pop(master_alias)
            )
            stebbs_block["enabled"] = enabled
            stebbs_block["parameters"] = parameters
            _log(
                f"[yaml-upgrade]   decomposed {master_alias!r} master toggle -> "
                "'stebbs.enabled' + 'stebbs.parameters'"
            )

    nested_conflicts = _stebbs_leaf_alias_conflicts(stebbs_block)
    if nested_conflicts:
        raise YamlUpgradeError(
            "Multiple nested STEBBS physics switches map to the same nested leaf "
            f"({_format_stebbs_leaf_alias_conflicts(nested_conflicts)}). Use only "
            "one spelling."
        )

    leaf_conflicts = _stebbs_leaf_alias_conflicts(physics)
    if leaf_conflicts:
        raise YamlUpgradeError(
            "Multiple flat STEBBS physics switches map to the same nested leaf "
            f"({_format_stebbs_leaf_alias_conflicts(leaf_conflicts)}). Use only "
            "one spelling."
        )

    for old_nested, nested_leaf in _STEBBS_PHYSICS_LEAF_RENAMES_TO_DEV12:
        if old_nested == nested_leaf or old_nested not in stebbs_block:
            continue
        if nested_leaf in stebbs_block:
            stebbs_block.pop(old_nested)
            _log(
                f"[yaml-upgrade]   rename 'stebbs.{old_nested}' -> "
                f"'stebbs.{nested_leaf}' skipped (target already present, "
                f"dropping stale 'stebbs.{old_nested}' value)"
            )
        else:
            stebbs_block[nested_leaf] = stebbs_block.pop(old_nested)
            _log(
                f"[yaml-upgrade]   renamed 'stebbs.{old_nested}' -> "
                f"'stebbs.{nested_leaf}'"
            )

    for old_flat, nested_leaf in _STEBBS_PHYSICS_LEAF_RENAMES_TO_DEV12:
        if old_flat in physics:
            if nested_leaf in stebbs_block:
                physics.pop(old_flat)
                _log(
                    f"[yaml-upgrade]   rename {old_flat!r} -> "
                    f"'stebbs.{nested_leaf}' skipped (target already present, "
                    f"dropping stale {old_flat!r} value)"
                )
            else:
                stebbs_block[nested_leaf] = physics.pop(old_flat)
                _log(
                    f"[yaml-upgrade]   moved {old_flat!r} -> 'stebbs.{nested_leaf}'"
                )

    if stebbs_block:
        physics["stebbs"] = stebbs_block


def _migrate_2026_4_to_2026_5(cfg: dict) -> dict:
    """Upgrade 2026.4-shaped YAMLs to the ``2026.5`` schema.

    Applies Category 1 of #1256: 59 fused compound field names in
    ``ModelPhysics`` / ``SurfaceProperties`` / ``LAIParams`` /
    ``VegetatedSurfaceProperties`` / ``EvetrProperties`` /
    ``DectrProperties`` / ``SnowParams`` rewritten to ``snake_case``
    (e.g. ``netradiationmethod`` -> ``net_radiation_method``,
    ``soildepth`` -> ``soil_depth``, ``baset`` -> ``base_temperature``,
    ``crwmax`` -> ``water_holding_capacity_max``). Full mapping lives
    in ``src/supy/data_model/core/field_renames.py``.
    """
    cfg = _strip_internal_only_fields(cfg)
    try:
        return rename_keys_recursive(cfg, _CATEGORY1_ONLY_RENAMES)
    except ValueError as exc:
        raise YamlUpgradeError(str(exc)) from exc


def _apply_stebbs_snake_renames(cfg: dict) -> None:
    """Apply gh#1334 STEBBS + Snow + Archetype PascalCase -> snake_case in place.

    Walks three containers under ``site.properties``:

    * ``building_archetype`` — 62 PascalCase ArchetypeProperties fields
      (plus the eight ``WallExternal``/``RoofExternal`` intermediate pairs
      from the Cat 5 gh#1327 sweep) rewritten to snake_case.
    * ``stebbs`` — 50 StebbsProperties fields rewritten to snake_case.
    * ``snow`` — 11 snake_case -> snake_case clean-ups (semantic fix for
      ``precip_limit`` -> ``temperature_rain_snow_threshold`` plus
      clarity renames for ``tau_a/f/r``, ``snowprof_24hr``,
      ``narp_emis_snow``, ``temp_melt_factor``, ``rad_melt_factor``).

    Each rename flows through ``_rename_field`` so a per-field log line
    is emitted (``TestNoSilentFieldDrops`` enforces this).
    """
    for arch in _walk_site_container(cfg, "building_archetype"):
        for old, new in _ARCH_RENAMES_DEV2_TO_DEV3:
            _rename_field(arch, old, new)
    for stebbs in _walk_site_container(cfg, "stebbs"):
        for old, new in _STEBBS_RENAMES_DEV2_TO_DEV3:
            _rename_field(stebbs, old, new)
    for snow in _walk_site_container(cfg, "snow"):
        for old, new in _SNOW_RENAMES_DEV2_TO_DEV3:
            _rename_field(snow, old, new)


def _apply_hot_water_unification_renames(cfg: dict) -> None:
    """Apply gh#1334 follow-through dev3 -> dev4 hot-water prefix unification.

    Rewrites the thirteen STEBBS ``dhw_*`` / ``water_tank_*`` fields
    and the one ArchetypeProperties ``water_tank_water_volume`` field
    to the unified ``hot_water_*`` prefix. Tank vs vessel separation is
    preserved through ``_tank_`` / ``_vessel_`` component qualifiers.

    Each rename flows through ``_rename_field`` so a per-field log line
    is emitted (``TestNoSilentFieldDrops`` enforces this).
    """
    for arch in _walk_site_container(cfg, "building_archetype"):
        for old, new in _ARCH_RENAMES_DEV3_TO_DEV4:
            _rename_field(arch, old, new)
    for stebbs in _walk_site_container(cfg, "stebbs"):
        for old, new in _STEBBS_RENAMES_DEV3_TO_DEV4:
            _rename_field(stebbs, old, new)


def _apply_forcing_subobject_restructure(cfg: dict) -> dict:
    """Move ``model.control.forcing_file`` under ``model.control.forcing.file``.

    gh#1372: introduces a ForcingControl sub-object so future forcing-related
    fields (gh#1373 disaggregation; resampling policy) have a stable home.
    The old value is preserved verbatim under ``forcing.file``: bare string,
    list of strings, or ``{value: ...}`` RefValue mapping all round-trip.
    """
    model = cfg.get("model")
    if not isinstance(model, dict):
        return cfg
    control = model.get("control")
    if not isinstance(control, dict):
        return cfg
    if "forcing_file" not in control:
        return cfg

    old_value = control.pop("forcing_file")
    forcing = control.setdefault("forcing", {})
    if not isinstance(forcing, dict):
        # User has manually set forcing to a non-dict; replace with a
        # dict so the legacy value has somewhere to land.
        forcing = {"file": old_value}
        control["forcing"] = forcing
        _log(
            "Migrated model.control.forcing_file -> model.control.forcing.file (gh#1372)"
        )
        return cfg
    if "file" in forcing:
        # Both keys present: the new shape wins; mirror the symmetric
        # _apply_output_subobject_restructure semantics.
        _log(
            "[yaml-upgrade]   dropped 'forcing_file' (already migrated; "
            "'forcing.file' wins)"
        )
        return cfg
    forcing["file"] = old_value
    _log("Migrated model.control.forcing_file -> model.control.forcing.file (gh#1372)")
    return cfg


def _apply_output_subobject_restructure(cfg: dict) -> dict:
    """Move ``model.control.output_file`` under ``model.control.output``.

    gh#1372 follow-up: introduces an OutputControl sub-object so the
    ``model.control`` surface is uniform with the new ``forcing:`` block.
    The dict form is preserved verbatim under ``output``, with the inner
    ``path`` field renamed to ``dir`` (clarifies it as a directory).
    The legacy string form (silently ignored since 2025.10.15) is
    dropped with a logged reason. If both ``output_file`` and ``output``
    are present, ``output`` wins and the legacy key is dropped.
    """
    model = cfg.get("model")
    if not isinstance(model, dict):
        return cfg
    control = model.get("control")
    if not isinstance(control, dict):
        return cfg
    if "output_file" not in control:
        return cfg

    legacy = control.pop("output_file")

    if "output" in control:
        _log(
            "[yaml-upgrade]   dropped 'output_file' (already migrated; "
            "'output' key wins)"
        )
        return cfg

    if isinstance(legacy, dict):
        migrated = {k: v for k, v in legacy.items() if k != "path"}
        if "path" in legacy and "dir" not in migrated:
            migrated["dir"] = legacy["path"]
            _log("[yaml-upgrade]   renamed 'output_file.path' -> 'output.dir'")
        control["output"] = migrated
        _log(
            "[yaml-upgrade]   migrated 'output_file' (dict) -> 'output' "
            "sub-object (gh#1372)"
        )
        return cfg

    # Legacy string form (e.g. output_file: "output.txt"): drop with reason.
    _log(
        "[yaml-upgrade]   dropped 'output_file' string value "
        f"({legacy!r}); use the 'output:' sub-object"
    )
    return cfg


def _apply_arch_rule2_renames(cfg: dict) -> None:
    """Apply Rule 2 reorder for ArchetypeProperties material/optical fields.

    The convention's Rule 2 (`.claude/rules/naming-convention.md`) puts the
    physical quantity first (``thickness_wall_outer`` not
    ``wall_external_thickness``); the layer-to-insulation qualifier moves
    from ``external`` to ``outer``; the ``effective_`` qualifier on the
    conductivity rows is dropped; and the heat-capacity distribution rows
    take the ``fraction_*`` non-physical category prefix.

    Each rename flows through ``_rename_field`` so a per-field log line
    is emitted (``TestNoSilentFieldDrops`` enforces this).
    """
    for arch in _walk_site_container(cfg, "building_archetype"):
        for old, new in _ARCH_RENAMES_DEV6_TO_DEV7:
            _rename_field(arch, old, new)


def _apply_naming_completion_renames(cfg: dict) -> None:
    """Apply the dev10 -> dev11 naming-convention completion.

    Two independent rename groups shipped together (gh#1392 + gh#1394):
    ArchetypeProperties Tier-1 completion on the ``building_archetype``
    container (plus the ``building_type`` drop and the wall/roof
    heat-capacity-fraction outer->external move from the Reading STEBBS
    team review), and StebbsProperties Rule-2 reorder on the ``stebbs``
    container. Renames are pure key reorders; ``building_type`` is dropped
    with a logged reason. Runs after every earlier rename layer so it sees
    the snake_case names those layers produce.

    Each rename flows through ``_rename_field`` so a per-field log line
    is emitted (``TestNoSilentFieldDrops`` enforces this).
    """
    for arch in _walk_site_container(cfg, "building_archetype"):
        for old, new in _ARCH_RENAMES_TIER1_TO_DEV11:
            _rename_field(arch, old, new)
        for name, reason in _ARCH_DROPS_TO_DEV11:
            _drop_obsolete_field(arch, name, reason)
    for stebbs in _walk_site_container(cfg, "stebbs"):
        for old, new in _STEBBS_RENAMES_RULE2_TO_DEV11:
            _rename_field(stebbs, old, new)
        for name, reason in _STEBBS_DROPS_TO_DEV11:
            _drop_obsolete_field(stebbs, name, reason)


def _apply_stebbs_straggler_renames(cfg: dict) -> None:
    """Apply the dev11 -> dev12 STEBBS / Archetype Column D alignment.

    The original four compound-noun stragglers kept at dev9 (gh#1392) plus the
    sixteen-field Column D alignment confirmed by the Reading STEBBS team review
    (D. Hertwig / S. Rognone, 2026-05). The ten StebbsProperties renames apply
    to the ``stebbs`` container; the six ArchetypeProperties renames
    (qualifier-first powers + setpoints/profiles) apply to the
    ``building_archetype`` container. The single ModelPhysics rename
    (``outer_cap_fraction`` -> ``capacitance``) applies to the top-level
    ``model.physics`` container. Pure key reorders. Runs after every earlier
    rename layer so it sees the snake_case names those layers produce.

    Each rename flows through ``_rename_field`` so a per-field log line is
    emitted (``TestNoSilentFieldDrops`` enforces this).
    """
    for stebbs in _walk_site_container(cfg, "stebbs"):
        for old, new in _STEBBS_RENAMES_STRAGGLERS_TO_DEV12:
            _rename_field(stebbs, old, new)
    for arch in _walk_site_container(cfg, "building_archetype"):
        for old, new in _ARCH_RENAMES_COLUMN_D_TO_DEV12:
            _rename_field(arch, old, new)
    physics = cfg.get("model", {}).get("physics")
    if isinstance(physics, dict):
        for old, new in MODELPHYSICS_DEV12_RENAMES.items():
            _rename_field(physics, old, new)


# gh#1495 dev13 -> dev14: the frontal_area_index readable selector dropped
# its synonym string aliases. Map any retired alias to the canonical
# observed/modelled value so an older YAML keeps loading.
_FAI_ALIAS_CANONICAL: dict[str, str] = {
    "provided": "observed",
    "use_provided": "observed",
    "simple_scheme": "modelled",
}


def _apply_fai_alias_canonicalisation(cfg: dict) -> None:
    """Rewrite retired frontal_area_index string aliases in place.

    gh#1495 dev13 -> dev14. The ``frontal_area_index`` readable selector was
    reduced to the canonical ``observed`` / ``modelled`` pair; the synonym
    aliases ``provided`` / ``use_provided`` / ``simple_scheme`` are no longer
    accepted. An older YAML that used a synonym is rewritten to its canonical
    replacement (``provided`` / ``use_provided`` -> ``observed``,
    ``simple_scheme`` -> ``modelled``) so it still loads. Integer values and
    already-canonical names are untouched.
    """
    model = cfg.get("model")
    if not isinstance(model, dict):
        return
    physics = model.get("physics")
    if not isinstance(physics, dict):
        return
    entry = physics.get("frontal_area_index")
    wrapped = isinstance(entry, dict) and "value" in entry
    raw = entry["value"] if wrapped else entry
    if not isinstance(raw, str):
        return
    canonical = _FAI_ALIAS_CANONICAL.get(raw.strip().lower())
    if canonical is None:
        return
    if wrapped:
        entry["value"] = canonical
    else:
        physics["frontal_area_index"] = canonical
    _log(
        f"[yaml-upgrade]   frontal_area_index {raw!r} -> {canonical!r} "
        "(retired synonym alias canonicalised)"
    )


def _migrate_2026_5_to_current(cfg: dict) -> dict:
    """Upgrade 2026.5-shaped YAMLs to the current schema.

    Chains the dev-label deltas:

    * 2026.5 -> 2026.5.dev1 (Category 5 of #1256, gh#1327): eight STEBBS
      ``ArchetypeProperties`` ``ext`` fields rewritten to ``External``.
    * 2026.5.dev1 -> 2026.5.dev2 (Categories 2 + 3 of #1256, gh#1321):
      15 ``ModelPhysics`` fields with the redundant ``_method`` /
      ``_model`` suffix dropped and/or domain abbreviations expanded.
    * 2026.5.dev2 -> 2026.5.dev3 (gh#1334): full STEBBS + Snow YAML
      surface converted to snake_case (124 renames).
    * 2026.5.dev3 -> 2026.5.dev4 (gh#1334 follow-through): hot-water
      prefix unification (14 renames).
    * 2026.5.dev4 -> 2026.5.dev5 (gh#972): nested physics acceptance
      widening, identity migration.
    * 2026.5.dev5 -> 2026.5.dev6 (gh#1333): site-level completeness
      validator tightening, identity migration.
    * 2026.5.dev6 -> 2026.5.dev7 (naming convention Rule 2):
      ArchetypeProperties bulk-material and surface optical fields
      reordered to ``<quantity>_<component>_<sub_class>`` (44 renames).
    * 2026.5.dev7 -> 2026.5.dev8 (PR#1395 registry refresh):
      identity migration (canonical rename registries refreshed,
      YAML surface unchanged).
    * 2026.5.dev8 -> 2026.5.dev9 (gh#1372): cumulative model.control
      restructure - forcing_file -> forcing.file, then output_file ->
      output (with inner path -> dir, legacy string form dropped).
    * 2026.6.dev2 -> 2026.6.dev3 (gh#1688): the omitted CO2Params fields
      renamed to convention-compliant identifiers while preserving values.
    * 2026.6.dev4 -> 2026.6.dev5 (#1832): the shortwave text selector
      renamed to ``perez`` without changing numeric option 3.
    * 2026.6.dev5 -> 2026.6.dev6 (#1974): default-backed
      ``output.include_provisional`` added; YAML content unchanged.
    * 2026.6.dev6 -> 2026.6.dev7 (#2001): the CO2 point source written
      as 0.0 wherever it was absent or null, the value the model received.
    * 2026.6.dev7 -> 2026.6.dev8 (#2001): a null ``initial_states.snowalb``
      dropped, so the default 0.5 applies; a null vegetation ``sdd_id`` or
      water ``flowchange`` written as 0.0, the value the model received.

    Each rename flows through ``_rename_field`` so a dedicated log line
    is emitted per field - ``TestNoSilentFieldDrops`` enforces that. The
    Pydantic backward-compat shim still accepts legacy names at load
    time, but YAMLs that round-trip through the migrator come out in the
    new spellings and no longer emit deprecation warnings.

    Under the dev-label convention
    (``.claude/rules/python/schema-versioning.md``) the release PR will
    collapse all dev-label steps into a single ``(<prev>, 2026.5)`` migration.
    """
    cfg = _strip_internal_only_fields(cfg)
    _apply_arch_ext_renames(cfg)
    _apply_modelphysics_suffix_renames(cfg)
    _apply_stebbs_snake_renames(cfg)
    _apply_hot_water_unification_renames(cfg)
    _apply_arch_rule2_renames(cfg)
    _apply_forcing_subobject_restructure(cfg)
    _apply_output_subobject_restructure(cfg)
    _apply_naming_completion_renames(cfg)
    _apply_stebbs_straggler_renames(cfg)
    _apply_stebbs_physics_fold(cfg)
    _apply_fai_alias_canonicalisation(cfg)
    _apply_co2params_renames(cfg)
    return _migrate_2026_6_dev3_to_current(cfg)


def _migrate_2026_5_to_2026_6_dev1(cfg: dict) -> dict:
    """Stamp the SPARTACUS direct/diffuse dev schema.

    The 2026.6.dev1 YAML delta adds only default-backed SPARTACUS controls
    (`kdown_split_method`, forest stream settings, and `vertical_layers.veg_ext`),
    so 2026.5 YAML content does not need reshaping.
    """
    return cfg


def _migrate_2026_6_dev1_to_2026_6_dev2(cfg: dict) -> dict:
    """Stamp the DyOHM-building storage-heat selector dev schema.

    The 2026.6.dev2 delta adds the opt-in ``storage_heat=dyohm_building``
    selector. Existing 2026.6.dev1 YAML content does not need reshaping.
    """
    return cfg


def _migrate_2026_6_dev2_to_2026_6_dev3(cfg: dict) -> dict:
    """Rename the fourteen CO2Params fields omitted from the #1256 sweep."""
    _apply_co2params_renames(cfg)
    return cfg


def _migrate_2026_6_dev3_to_2026_6_dev4(cfg: dict) -> dict:
    """Preserve YAML content when adding default-backed STEBBS controls.

    The new shading and heating inputs are optional. Model defaults retain
    disabled shading, indoor waste heat and entirely convective heating;
    explicitly supplied controls must also survive the upgrade unchanged.
    """
    return cfg


def _migrate_2026_6_dev4_to_2026_6_dev5(cfg: dict) -> dict:
    """Rename the retired shortwave selector, preserving its reference metadata."""
    model = cfg.get("model")
    if not isinstance(model, dict):
        return cfg
    physics = model.get("physics")
    if not isinstance(physics, dict):
        return cfg
    entry = physics.get("kdown_split_method")
    wrapped = isinstance(entry, dict) and "value" in entry
    raw = entry["value"] if wrapped else entry
    if not isinstance(raw, str) or raw.strip().lower() != "epw":
        return cfg
    if wrapped:
        entry["value"] = "perez"
    else:
        physics["kdown_split_method"] = "perez"
    _log(
        f"[yaml-upgrade]   kdown_split_method {raw!r} -> 'perez' "
        "(renamed selector; numeric option 3 unchanged)"
    )
    return cfg


def _migrate_2026_6_dev5_to_2026_6_dev6(cfg: dict) -> dict:
    """Preserve YAML content when adding ``output.include_provisional``.

    The new switch defaults to false. A YAML whose ``output.groups`` names a
    provisional group still loads (with a warning); saving that group needs
    the switch, which the migration deliberately does not turn on.
    """
    return cfg


_CO2_POINT_SOURCE = "emission_co2_point_source"
_CO2_POINT_SOURCE_LEGACY = "co2pointsource"


def _migrate_2026_6_dev6_to_2026_6_dev7(cfg: dict) -> dict:
    """Write the CO2 point source the model used to receive as 0.0.

    From 2026.6.dev7 the point source is required when the emissions method
    adds it to the CO2 flux (#2001). Before, an absent or null value reached
    the model as 0, so every site gets that 0.0
    written explicitly; a value the user gave is left alone.
    """
    for site in cfg.get("sites", []) or []:
        if not isinstance(site, dict):
            continue
        props = site.setdefault("properties", {})
        if not isinstance(props, dict):
            continue
        emissions = props.setdefault("anthropogenic_emissions", {})
        if not isinstance(emissions, dict):
            continue
        co2 = emissions.setdefault("co2", {})
        if not isinstance(co2, dict):
            continue
        if any(
            _unwrap_scalar(co2.get(name)) is not None
            for name in (_CO2_POINT_SOURCE, _CO2_POINT_SOURCE_LEGACY)
        ):
            continue
        co2.pop(_CO2_POINT_SOURCE_LEGACY, None)
        co2[_CO2_POINT_SOURCE] = {"value": 0.0}
        _log(
            f"[yaml-upgrade]   set {_CO2_POINT_SOURCE!r} to 0.0 "
            "(absent or null; the value the model used)"
        )
    return cfg


def _unwrap_scalar(value):
    """Return the scalar inside a ``{value: ...}`` wrapper, or the value itself."""
    if isinstance(value, dict):
        return value.get("value")
    return value


def _migrate_2026_6_dev7_to_2026_6_dev8(cfg: dict) -> dict:
    """Repair previously emitted nulls without changing the schema lineage.

    From 2026.6.dev8 ``snowalb`` rejects null (#2001). Earlier
    ``suews validate --science-fixes apply`` runs wrote ``snowalb: null`` for
    a snow-free start, so such files would no longer load. Removing the key
    lets the documented default (0.5) apply; a number the user gave is kept.
    Null vegetation ``sdd_id`` and water ``flowchange`` instead receive their
    former effective 0.0 explicitly, retaining any reference metadata.
    """
    def restore_zero(block, name):
        if not isinstance(block, dict) or name not in block:
            return
        entry = block[name]
        if _unwrap_scalar(entry) is not None:
            return
        if isinstance(entry, dict):
            entry["value"] = 0.0
        else:
            block[name] = 0.0
        _log(
            f"[yaml-upgrade]   set {name!r} to 0.0 "
            "(null is no longer accepted; preserve the former effective zero)"
        )

    for site in cfg.get("sites", []) or []:
        if not isinstance(site, dict):
            continue
        states = site.get("initial_states")
        if isinstance(states, dict):
            for surface in ("evetr", "dectr", "grass"):
                restore_zero(states.get(surface), "sdd_id")
            if "snowalb" in states and _unwrap_scalar(states["snowalb"]) is None:
                states.pop("snowalb")
                _log(
                    "[yaml-upgrade]   dropped 'snowalb' "
                    "(null is no longer accepted; the default 0.5 applies)"
                )
        props = site.get("properties")
        if isinstance(props, dict):
            land_cover = props.get("land_cover")
            if isinstance(land_cover, dict):
                restore_zero(land_cover.get("water"), "flowchange")
    return cfg


def _migrate_2026_6_dev6_to_current(cfg: dict) -> dict:
    """Chain the CO2 point source and the null snowalb removal."""
    cfg = _migrate_2026_6_dev6_to_2026_6_dev7(cfg)
    return _migrate_2026_6_dev7_to_2026_6_dev8(cfg)


def _migrate_2026_6_dev5_to_current(cfg: dict) -> dict:
    """Chain the provisional-output switch and all subsequent schema deltas."""
    cfg = _migrate_2026_6_dev5_to_2026_6_dev6(cfg)
    return _migrate_2026_6_dev6_to_current(cfg)


def _migrate_2026_6_dev4_to_current(cfg: dict) -> dict:
    """Chain the shortwave selector rename and all subsequent schema deltas."""
    cfg = _migrate_2026_6_dev4_to_2026_6_dev5(cfg)
    return _migrate_2026_6_dev5_to_current(cfg)


SOIL_MIGRATION_MESSAGE = (
    "This YAML uses observed soil moisture, so it must be converted together "
    "with its legacy xsmd forcing; no output configuration was written. "
    + SOIL_CONVERT_HINT
)
SOIL_EVIDENCE_MESSAGE = (
    "Soil-moisture migration evidence is incomplete or inconsistent; run the "
    "coordinated converter (suews convert --forcing ...) instead of calling the "
    "schema handler directly"
)


@dataclass(frozen=True)
class SoilMigrationContext:
    """Explicit evidence supplied by the coordinated forcing converter.

    Bounds are indexed by site position and expressed in metres. The completed
    manifest records original_method, source_unit, input_sha256 and output_sha256.
    This context is never serialised as runtime configuration.
    """

    layer_bounds: dict[int, tuple[float, float]]
    original_method: int
    forcing_manifest: dict
    legacy_depth_is_thickness: bool = False
    # Decisions taken by the handler, copied into the manifest ``changes``.
    notes: list = field(default_factory=list)


def _soil_raw(value):
    if isinstance(value, dict) and "value" in value:
        return _soil_raw(value["value"])
    if hasattr(value, "value"):
        return _soil_raw(value.value)
    return value


def _soil_method(value):
    raw = _soil_raw(value)
    if isinstance(raw, str):
        raw = raw.strip().lower().rsplit(".", 1)[-1]
        raw = {"modelled": 0, "observed_volumetric": 1,
               "observed_gravimetric": 2, "observed": "observed"}.get(raw, raw)
        if isinstance(raw, str) and raw in ("0", "1", "2"):
            raw = int(raw)
    if raw not in (0, 1, 2, "observed"):
        raise YamlUpgradeError(f"Unknown legacy soil-moisture method {raw!r}")
    return raw


def _scaled_soil_value(value, scale):
    if isinstance(value, dict):
        result = deepcopy(value)
        result["value"] = float(_soil_raw(value)) * scale
        return result
    return float(_soil_raw(value)) * scale


def _migrate_2026_6_dev8_to_2026_6_dev9(cfg: dict, *, migration_context=None) -> dict:
    """Apply the soil delta transactionally, retaining original method evidence."""
    result = deepcopy(cfg)
    physics = result.get("model", {}).get("physics", {})
    aliases = ("soil_moisture", "soil_moisture_deficit", "smdmethod", "smd_method")
    present = [key for key in aliases if key in physics]
    if len(present) > 1:
        raise YamlUpgradeError("Both legacy and canonical soil-moisture inputs were supplied; select one contract before conversion.")
    key = present[0] if present else None
    method = _soil_method(physics[key]) if key else 0
    active = method != 0
    context = migration_context
    sites = result.get("sites", [])
    if active:
        if not isinstance(context, SoilMigrationContext):
            raise YamlUpgradeError(SOIL_MIGRATION_MESSAGE)
        manifest = context.forcing_manifest
        expected_unit = {1: "m3 m-3", 2: "kg kg-1"}.get(context.original_method)
        if (expected_unit is None or (method != "observed" and method != context.original_method)
                or manifest.get("completed") is not True
                or manifest.get("original_method") != context.original_method
                or manifest.get("source_unit") != expected_unit
                or any(not isinstance(manifest.get(k), str) or len(manifest[k]) != 64
                       or any(c not in "0123456789abcdef" for c in manifest[k])
                       for k in ("input_sha256", "output_sha256"))
                or any(i not in context.layer_bounds for i in range(len(sites)))):
            raise YamlUpgradeError(SOIL_EVIDENCE_MESSAGE)
    logs = []
    for i, site in enumerate(sites):
        props = site.get("properties", {})
        old = props.get("soil_observation")
        if not active:
            if old is not None and any(k in old for k in ("depth", "smcap", "obs_sm_depth", "obs_sm_smcap")):
                if "smcap" in old:
                    logs.append("Sensor saturation capacity is retired; deficit now uses the configured model soil-store capacity")
                props.pop("soil_observation")
                logs.append(f"sites[{i}].soil_observation removed: inactive legacy sensor metadata cannot establish represented layer bounds")
            continue
        old = old or {}
        if any(k in old for k in ("layer_depth", "layer_top")):
            raise YamlUpgradeError("Both legacy and canonical soil-moisture inputs were supplied; select one contract before conversion.")
        top, depth = context.layer_bounds[i]
        if not math.isfinite(top) or not math.isfinite(depth) or abs(top) > 1e-6 or depth <= 0:
            raise YamlUpgradeError(
                "Observed soil moisture represents the whole soil store: pass "
                f"--layer-top 0 and a finite positive --layer-depth in m (got --layer-top {top:g}, "
                f"--layer-depth {depth:g})"
            )
        new = {"layer_top": top, "layer_depth": depth}
        if "depth" in old:
            new["layer_depth"] = _scaled_soil_value(old["depth"], 0.001)
            legacy_depth = float(_soil_raw(new["layer_depth"]))
            if context.legacy_depth_is_thickness and not math.isclose(legacy_depth, depth, rel_tol=0, abs_tol=1e-6):
                raise YamlUpgradeError(
                    f"sites[{i}]: legacy soil_observation depth {legacy_depth:g} m "
                    f"({float(_soil_raw(old['depth'])):g} mm) disagrees with --layer-depth {depth:g} m. "
                    "--legacy-depth-is-thickness requires them to match: pass "
                    f"--layer-depth {legacy_depth:g}, or drop --legacy-depth-is-thickness "
                    "to treat the legacy depth as the sensor installation depth"
                )
            if context.legacy_depth_is_thickness:
                note = f"sites[{i}].depth -> layer_depth: confirmed thickness, mm to m, scale 0.001"
            else:
                if isinstance(new["layer_depth"], dict):
                    new["layer_depth"]["value"] = depth
                else:
                    new["layer_depth"] = depth
                note = (
                    f"sites[{i}].depth {legacy_depth:g} m (legacy {float(_soil_raw(old['depth'])):g} mm) "
                    f"discarded as installation depth; layer_depth {depth:g} m from --layer-depth"
                )
            logs.append(note)
            context.notes.append(note)
        for name in ("ref", "soil_not_rocks"):
            if name in old:
                new[name] = old[name]
        if "bulk_density" in old:
            new["bulk_density"] = _scaled_soil_value(old["bulk_density"], 1000)
            logs.append(f"sites[{i}].bulk_density: g cm-3 to kg m-3, scale 1000")
        if "smcap" in old:
            logs.append("Sensor saturation capacity is retired; deficit now uses the configured model soil-store capacity")
        unknown = set(old) - {"depth", "smcap", "bulk_density", "soil_not_rocks", "ref"}
        if unknown:
            raise YamlUpgradeError(f"Unrecognised legacy soil observation fields: {sorted(unknown)}")
        props["soil_observation"] = new
    if key:
        entry = physics.pop(key)
        if isinstance(entry, dict):
            entry["value"] = "observed" if active else "modelled"
        else:
            entry = "observed" if active else "modelled"
        physics["soil_moisture"] = entry
        logs.append(f"{key} -> soil_moisture; original method {method}")
    if active:
        # Validate all sites against the current schema using a throwaway copy
        # stripped of legacy radiation, continuation-state placeholders,
        # pre-dev16 non-finite sdd_id values, pre-dev17 non-integer DOYs, and
        # pre-dev20 non-canonical 10-minute profile keys; the returned dev9
        # mapping stays intact for later deltas and single-pass logging.
        from ...data_model.core.config import SUEWSConfig
        candidate, _ = _drop_legacy_radiation_memory_zeros(result)
        candidate, _ = _drop_legacy_continuation_state_placeholders(candidate)
        _clear_nonfinite_sdd_for_dev9_candidate(candidate)
        _clear_noninteger_doy_for_dev9_candidate(candidate)
        _clear_noncanonical_ten_minute_keys_for_dev9_candidate(candidate)
        candidate = _clear_non_owning_vertical_layer_optical_keys_for_dev9_candidate(
            candidate
        )
        candidate["schema_version"] = CURRENT_SCHEMA_VERSION
        SUEWSConfig.model_validate(candidate)
        logs.append(f"legacy xsmd conversion confirmed; original method {context.original_method}; "
                    f"input_sha256={context.forcing_manifest['input_sha256']}; "
                    f"output_sha256={context.forcing_manifest['output_sha256']}")
    for message in logs:
        _log(f"[yaml-upgrade]   {message}")
    return result


def _migrate_2026_6_dev3_to_current(cfg: dict) -> dict:
    """Chain the optional STEBBS controls and all subsequent schema deltas."""
    cfg = _migrate_2026_6_dev3_to_2026_6_dev4(cfg)
    return _migrate_2026_6_dev4_to_current(cfg)


def _migrate_2026_6_dev2_to_current(cfg: dict) -> dict:
    """Apply the CO2Params renames and all subsequent schema deltas."""
    cfg = _migrate_2026_6_dev2_to_2026_6_dev3(cfg)
    return _migrate_2026_6_dev3_to_current(cfg)


def _migrate_2026_6_dev1_to_current(cfg: dict) -> dict:
    """Chain the dev2 identity delta and all subsequent schema deltas."""
    cfg = _migrate_2026_6_dev1_to_2026_6_dev2(cfg)
    return _migrate_2026_6_dev2_to_current(cfg)


def _migrate_2026_4_to_current(cfg: dict) -> dict:
    """Chain 2026.4 -> 2026.5 released schema -> current dev schema."""
    cfg = _migrate_2026_4_to_2026_5(cfg)
    return _migrate_2026_5_to_current(cfg)


def _migrate_2026_1_to_2026_4(cfg: dict) -> dict:
    """Pure #1240/#1242/#1261 delta, without the Category 1 renames."""
    cfg = _strip_internal_only_fields(cfg)
    any_profile_split = False
    for arch in _walk_site_container(cfg, "building_archetype"):
        if _split_profile_fields(arch):
            any_profile_split = True
        for name, reason in _ARCH_DROPS_2026_1_TO_CURRENT:
            _drop_obsolete_field(arch, name, reason)
    for stebbs in _walk_site_container(cfg, "stebbs"):
        for old, new in _STEBBS_RENAMES_2026_1_TO_CURRENT:
            _rename_field(stebbs, old, new)
        for name, reason in _STEBBS_DROPS_2026_1_TO_CURRENT:
            _drop_obsolete_field(stebbs, name, reason)
    if any_profile_split:
        physics = cfg.setdefault("model", {}).setdefault("physics", {})
        physics["setpointmethod"] = {"value": 2}
    return cfg


def _migrate_2025_12_to_2026_4(cfg: dict) -> dict:
    """Chain #879 clean-up -> 2026.1 delta, stopping at 2026.4."""
    cfg = _migrate_2025_12_to_2026_1(cfg)
    return _migrate_2026_1_to_2026_4(cfg)


def _migrate_2025_12_to_current(cfg: dict) -> dict:
    """Chain #879 clean-up -> 2026.1 delta -> 2026.5 -> current dev schema."""
    cfg = _migrate_2025_12_to_2026_4(cfg)
    return _migrate_2026_4_to_current(cfg)


def _migrate_2026_1_to_current(cfg: dict) -> dict:
    """Chain 2026.1 delta -> 2026.5 released schema -> current dev schema."""
    cfg = _migrate_2026_1_to_2026_4(cfg)
    return _migrate_2026_4_to_current(cfg)


def _migrate_2026_6_dev9_to_2026_6_dev10(cfg: dict) -> dict:
    """Stamp the corrected storage-capacity semantics without rewriting user inputs."""
    return cfg


_SOIL_PARAMS_REQUIRED_DEV11: tuple[tuple[str, str], ...] = (
    ("soil_depth", "soildepth"),
    ("soil_store_capacity", "soilstorecap"),
    ("saturated_hydraulic_conductivity", "sathydraulicconduct"),
)


def _format_site_label(site_idx: int, site: dict) -> str:
    """Return a concise site identifier naming index, name and gridiv."""
    parts: list[str] = []
    raw_name = _unwrap_scalar(site.get("name"))
    if raw_name is not None and str(raw_name).strip():
        parts.append(str(raw_name).strip())
    raw_gridiv = _unwrap_scalar(site.get("gridiv"))
    if raw_gridiv is not None and not isinstance(raw_gridiv, bool):
        parts.append(f"gridiv={raw_gridiv}")
    if parts:
        return f"sites[{site_idx}] ({', '.join(parts)})"
    return f"sites[{site_idx}]"


def _positive_soil_capacity(surface: dict) -> float | None:
    """Return a finite or infinite positive soil store capacity, or None."""
    for name in ("soil_store_capacity", "soilstorecap"):
        if name not in surface:
            continue
        raw = _unwrap_scalar(surface.get(name))
        if raw is None or isinstance(raw, bool):
            continue
        try:
            value = float(raw)
        except (TypeError, ValueError):
            return None
        if not math.isnan(value) and value > 0.0:
            return value
        return None
    return None


def _active_surface_sfr(surface: dict) -> float | None:
    """Return finite positive surface fraction for *surface*, or None when inactive."""
    raw_sfr = (
        _unwrap_scalar(surface.get("sfr")) if "sfr" in surface else (1.0 / 7.0)
    )
    if raw_sfr is None or isinstance(raw_sfr, bool):
        return None
    try:
        sfr_value = float(raw_sfr)
    except (TypeError, ValueError):
        return None
    if not (math.isfinite(sfr_value) and sfr_value > 0.0):
        return None
    return sfr_value


def _set_zero_soil_param(surface: dict, canonical: str, legacy: str) -> None:
    """Write explicit 0.0 for a missing/null soil field, preserving ref metadata."""
    if canonical in surface and isinstance(surface[canonical], dict):
        surface[canonical]["value"] = 0.0
        surface.pop(legacy, None)
    elif legacy in surface and isinstance(surface[legacy], dict):
        entry = surface.pop(legacy)
        entry["value"] = 0.0
        surface[canonical] = entry
    else:
        surface.pop(legacy, None)
        surface[canonical] = {"value": 0.0}


def _as_numeric_float(raw: object) -> float | None:
    """Convert non-bool numeric *raw* to float, returning None otherwise."""
    if raw is None or isinstance(raw, bool):
        return None
    try:
        return float(raw)
    except (TypeError, ValueError):
        return None


def _zero_soil_depth_reason(
    site_label: str,
    surface_name: str,
    surface: dict,
    sfr_value: float,
    positive_cap: float,
) -> str | None:
    """Return an error reason when *surface* supplies explicit zero ``soil_depth``."""
    for name in ("soil_depth", "soildepth"):
        if name not in surface:
            continue
        raw = _unwrap_scalar(surface.get(name))
        if raw is None:
            continue
        depth_value = _as_numeric_float(raw)
        if depth_value is not None and depth_value == 0:
            return (
                f"{site_label} properties.land_cover.{surface_name}.soil_depth "
                f"is 0 (sfr={sfr_value:g}, soil_store_capacity={positive_cap:g})"
            )
        return None
    return None


def validate_active_soil_depth_raw(cfg: dict) -> None:
    """Reject explicit zero ``soil_depth`` on active positive-capacity surfaces.

    Checks canonical (``soil_depth``, ``soil_store_capacity``) and legacy
    (``soildepth``, ``soilstorecap``) keys across all six non-water surfaces
    without mutating *cfg*. Raises :class:`YamlUpgradeError` before any output
    file is written.
    """
    zero_depths: list[str] = []
    for site_idx, site in enumerate(cfg.get("sites", []) or []):
        if not isinstance(site, dict):
            continue
        props = site.get("properties")
        land_cover = props.get("land_cover") if isinstance(props, dict) else None
        if not isinstance(land_cover, dict):
            continue
        site_label = _format_site_label(site_idx, site)
        for surface_name in NON_WATER_SURFACES:
            surface = land_cover.get(surface_name)
            if not isinstance(surface, dict):
                continue
            sfr_value = _active_surface_sfr(surface)
            if sfr_value is None:
                continue
            positive_cap = _positive_soil_capacity(surface)
            if positive_cap is None:
                continue
            reason = _zero_soil_depth_reason(
                site_label, surface_name, surface, sfr_value, positive_cap
            )
            if reason is not None:
                zero_depths.append(reason)
    if zero_depths:
        raise YamlUpgradeError(
            "Cannot migrate configuration: "
            + "; ".join(zero_depths)
            + ". Active non-water surfaces with positive soil_store_capacity "
            "require a positive soil_depth (in mm); zero soil_depth breaks "
            "wet-day soil water balance."
        )


def _migrate_active_surface_soil_params(
    site_label: str,
    surface_name: str,
    surface: dict,
    invalid_depths: list[str],
    pending_logs: list[str],
) -> None:
    """Apply dev11 soil parameter migration to one non-water surface mapping."""
    sfr_value = _active_surface_sfr(surface)
    if sfr_value is None:
        return
    positive_cap = _positive_soil_capacity(surface)
    for canonical, legacy in _SOIL_PARAMS_REQUIRED_DEV11:
        if any(
            _unwrap_scalar(surface.get(name)) is not None
            for name in (canonical, legacy)
        ):
            if canonical == "soil_depth" and positive_cap is not None:
                reason = _zero_soil_depth_reason(
                    site_label, surface_name, surface, sfr_value, positive_cap
                )
                if reason is not None:
                    invalid_depths.append(reason)
            continue
        if canonical == "soil_depth" and positive_cap is not None:
            invalid_depths.append(
                f"{site_label} properties.land_cover.{surface_name}.soil_depth "
                "is missing or null "
                f"(sfr={sfr_value:g}, soil_store_capacity={positive_cap:g})"
            )
            continue
        _set_zero_soil_param(surface, canonical, legacy)
        pending_logs.append(
            f"[yaml-upgrade]   set '{site_label} land_cover.{surface_name}.{canonical}' "
            "to 0.0 (absent or null on active surface; the value the model used)"
        )


def _migrate_2026_6_dev10_to_2026_6_dev11(cfg: dict) -> dict:
    """Migrate active non-water surface soil parameters to schema 2026.6.dev11.

    From 2026.6.dev11, ``soil_depth``, ``soil_store_capacity`` and
    ``saturated_hydraulic_conductivity`` are required on every active
    (``sfr > 0``) non-water surface (#2077, #2001). When ``soil_depth`` is
    missing, null, or explicitly ``0`` on an active surface with positive
    ``soil_store_capacity``, migration refuses to proceed (zero ``soil_depth``
    divides by zero in horizontal soil water redistribution and breaks wet-day
    water balance) and raises ``YamlUpgradeError`` listing every affected site
    and surface so the user can supply a positive ``soil_depth`` explicitly.
    Supplied positive values, inactive (``sfr == 0``) surfaces and ``water``
    remain unchanged; other missing or null soil parameters (including
    ``soil_depth`` when soil store capacity is zero or absent) receive explicit
    ``0.0`` while preserving any reference metadata.
    """
    result = deepcopy(cfg)
    invalid_depths: list[str] = []
    pending_logs: list[str] = []
    for site_idx, site in enumerate(result.get("sites", []) or []):
        if not isinstance(site, dict):
            continue
        props = site.get("properties")
        land_cover = props.get("land_cover") if isinstance(props, dict) else None
        if not isinstance(land_cover, dict):
            continue
        site_label = _format_site_label(site_idx, site)
        for surface_name in NON_WATER_SURFACES:
            surface = land_cover.get(surface_name)
            if isinstance(surface, dict):
                _migrate_active_surface_soil_params(
                    site_label, surface_name, surface, invalid_depths, pending_logs
                )
    if invalid_depths:
        raise YamlUpgradeError(
            "Cannot migrate configuration to schema 2026.6.dev11: "
            + "; ".join(invalid_depths)
            + ". Active non-water surfaces with positive soil_store_capacity "
            "require a positive user-supplied soil_depth (in mm) before "
            "migration; zero soil_depth breaks wet-day soil water balance."
        )
    for message in pending_logs:
        _log(message)
    return result


def _migrate_2026_6_dev11_to_2026_6_dev12(cfg: dict) -> dict:
    """Remove explicit numeric zeros from inapplicable water destinations.

    Validate only the fields this delta owns, through the public parser and
    destination validator. Partial configurations need not supply routing
    fractions, and applicable destinations retain their original representation.
    Nulls and accepted non-numeric representations remain unchanged.
    """
    from ...data_model.core.hydro import WaterDistribution
    from ...data_model.core.type import SurfaceType

    result = deepcopy(cfg)
    pending_logs: list[str] = []
    for site_idx, site in enumerate(result.get("sites", []) or []):
        if not isinstance(site, dict):
            continue
        props = site.get("properties")
        land_cover = props.get("land_cover") if isinstance(props, dict) else None
        if not isinstance(land_cover, dict):
            continue
        for surface_name in NON_WATER_SURFACES:
            surface = land_cover.get(surface_name)
            dist = surface.get("waterdist") if isinstance(surface, dict) else None
            if not isinstance(dist, dict):
                continue
            required = WATER_DISTRIBUTION_REQUIRED[surface_name]
            for name in WaterDistribution.model_fields:
                if not name.startswith("to_") or name in required or name not in dist:
                    continue
                path = (
                    f"{_format_site_label(site_idx, site)} "
                    f"properties.land_cover.{surface_name}.waterdist.{name}"
                )
                try:
                    parsed = WaterDistribution.model_validate({name: dist[name]})
                    parsed.validate_distribution(
                        SurfaceType(surface_name),
                        require_complete=False,
                        normalise_zero_disallowed=True,
                    )
                except ValueError as exc:
                    raise YamlUpgradeError(
                        f"Cannot migrate configuration to schema 2026.6.dev12: "
                        f"{path}: {exc}"
                    ) from exc
                raw = _unwrap_scalar(dist[name])
                if isinstance(raw, numbers.Real) and not isinstance(raw, bool) and raw == 0:
                    del dist[name]
                    pending_logs.append(
                        f"[yaml-upgrade]   drop '{path}' "
                        "(explicit numeric zero in an inapplicable destination; "
                        "absence preserves the load-time routing)"
                    )
    for message in pending_logs:
        _log(message)
    return result


_HANDLERS: dict[tuple[str, str], Handler] = {
    # Identity at the current schema is explicit so the dispatch completes
    # even when no real upgrade is needed.
    (CURRENT_SCHEMA_VERSION, CURRENT_SCHEMA_VERSION): _identity,
    # Intermediate stops at 2026.4 (used by callers pinning an older
    # target, e.g. migrate(..., to_version="2026.4")).
    ("2026.1", "2026.4"): _migrate_2026_1_to_2026_4,
    ("2025.12", "2026.4"): _migrate_2025_12_to_2026_4,
    # Upgrade each prior released schema straight to the current 2026.5
    # schema. 2026.5 is the collapse of the 2026.5.dev1..dev14 development
    # cycle into a single released label (see SCHEMA_VERSIONS["2026.5"] and
    # `.claude/rules/python/schema-versioning.md`). The (2026.4 -> 2026.5)
    # handler therefore applies the whole union of dev-cycle deltas in
    # order: #1256 Category 1 snake_case sweep, #1327 STEBBS ext->External,
    # #1321 ModelPhysics suffix drop, #1334 + #1337 STEBBS/Snow snake_case
    # and hot-water prefix unification, #972 accept-only nested physics,
    # #1333 completeness-validator tightening, the naming-convention Rule 2
    # reorders (#1395 / gh#1392 / gh#1394), #1372 model.control restructure
    # (+#1420 forcing-adapter follow-up), gh#1452 Column D alignment,
    # gh#1456 STEBBS physics fold, and gh#1495 frontal_area_index selector.
    # _migrate_2026_4_to_current chains _migrate_2026_4_to_2026_5 (Category 1)
    # then _migrate_2026_5_to_current (the remaining dev-cycle union).
    ("2026.6.dev7", CURRENT_SCHEMA_VERSION): _migrate_2026_6_dev7_to_2026_6_dev8,
    ("2026.6.dev6", CURRENT_SCHEMA_VERSION): _migrate_2026_6_dev6_to_current,
    ("2026.6.dev5", CURRENT_SCHEMA_VERSION): _migrate_2026_6_dev5_to_current,
    ("2026.6.dev4", CURRENT_SCHEMA_VERSION): _migrate_2026_6_dev4_to_current,
    ("2026.6.dev3", CURRENT_SCHEMA_VERSION): _migrate_2026_6_dev3_to_current,
    ("2026.6.dev2", CURRENT_SCHEMA_VERSION): _migrate_2026_6_dev2_to_current,
    ("2026.6.dev1", CURRENT_SCHEMA_VERSION): _migrate_2026_6_dev1_to_current,
    ("2026.5", CURRENT_SCHEMA_VERSION): _migrate_2026_5_to_current,
    ("2026.4", CURRENT_SCHEMA_VERSION): _migrate_2026_4_to_current,
    ("2026.1", CURRENT_SCHEMA_VERSION): _migrate_2026_1_to_current,
    ("2025.12", CURRENT_SCHEMA_VERSION): _migrate_2025_12_to_current,
}


# Snapshot the outgoing routes before wrapping; intermediate dev8 targets keep
# their original bodies. Every older-to-current edge composes the delta once.
def _with_soil_delta(handler):
    @wraps(handler)
    def migrate(cfg, *, migration_context=None):
        cfg = handler(deepcopy(cfg))
        return _migrate_2026_6_dev8_to_2026_6_dev9(cfg, migration_context=migration_context)
    return migrate


for (_source, _target), _handler in list(_HANDLERS.items()):
    if _target == CURRENT_SCHEMA_VERSION and _source != CURRENT_SCHEMA_VERSION:
        _HANDLERS[(_source, "2026.6.dev8")] = _handler
        _HANDLERS[(_source, _target)] = _with_soil_delta(_handler)
_HANDLERS[("2026.6.dev8", CURRENT_SCHEMA_VERSION)] = _migrate_2026_6_dev8_to_2026_6_dev9

# dev10 changes how the model reads the same inputs. The existing routes
# to current therefore also produce valid dev9 content for callers pinning dev9.
for (_source, _target), _handler in list(_HANDLERS.items()):
    if _target == CURRENT_SCHEMA_VERSION and _source != CURRENT_SCHEMA_VERSION:
        _HANDLERS[(_source, "2026.6.dev9")] = _handler
_HANDLERS[("2026.6.dev9", CURRENT_SCHEMA_VERSION)] = _migrate_2026_6_dev9_to_2026_6_dev10


def apply_migration_handler(handler, cfg, *, migration_context=None):
    """Pass private context only to handlers declaring that keyword."""
    from inspect import signature
    if "migration_context" in signature(handler, follow_wrapped=False).parameters:
        return handler(cfg, migration_context=migration_context)
    return handler(cfg)


# dev11 requires soil_depth, soil_store_capacity and
# saturated_hydraulic_conductivity on active non-water surfaces. Existing
# routes keep targeting dev10 for callers pinning dev10, and compose the dev11
# delta when upgrading to the current schema.
def _with_soil_params_delta(handler):
    @wraps(handler)
    def migrate(cfg, *, migration_context=None):
        cfg = apply_migration_handler(
            handler, deepcopy(cfg), migration_context=migration_context
        )
        return _migrate_2026_6_dev10_to_2026_6_dev11(cfg)
    return migrate


for (_source, _target), _handler in list(_HANDLERS.items()):
    if _target == CURRENT_SCHEMA_VERSION and _source != CURRENT_SCHEMA_VERSION:
        _HANDLERS[(_source, "2026.6.dev10")] = _handler
        _HANDLERS[(_source, _target)] = _with_soil_params_delta(_handler)
_HANDLERS[("2026.6.dev10", CURRENT_SCHEMA_VERSION)] = _migrate_2026_6_dev10_to_2026_6_dev11


# Preserve every dev11 target, including private soil migration context, before
# composing the water-distribution delta onto current-target routes.
def _with_water_distribution_delta(handler):
    @wraps(handler)
    def migrate(cfg, *, migration_context=None):
        cfg = apply_migration_handler(
            handler, deepcopy(cfg), migration_context=migration_context
        )
        return _migrate_2026_6_dev11_to_2026_6_dev12(cfg)
    return migrate


for (_source, _target), _handler in list(_HANDLERS.items()):
    if _target == CURRENT_SCHEMA_VERSION and _source != CURRENT_SCHEMA_VERSION:
        _HANDLERS[(_source, "2026.6.dev11")] = _handler
        _HANDLERS[(_source, _target)] = _with_water_distribution_delta(_handler)
_HANDLERS[("2026.6.dev11", CURRENT_SCHEMA_VERSION)] = _migrate_2026_6_dev11_to_2026_6_dev12


def _drop_legacy_radiation_memory_zeros(cfg: dict) -> tuple[dict, list[str]]:
    """Drop legacy seven-zero radiation placeholders without mutating ``cfg``.

    Returns the copied mapping and pending log messages; refuses non-zero
    values with ``YamlUpgradeError`` so restart memory is never discarded.
    """
    cfg = deepcopy(cfg)
    pending_logs: list[str] = []
    sites = cfg.get("sites")
    if not isinstance(sites, list):
        return cfg, pending_logs
    for index, site in enumerate(sites):
        states = site.get("initial_states") if isinstance(site, dict) else None
        if not isinstance(states, dict):
            continue
        for name in ("qn_surfs", "dqndt_surf"):
            if name not in states:
                continue
            values = states[name]
            path = f"sites[{index}].initial_states.{name}"
            if not (
                isinstance(values, list) and len(values) == 7
                and all(isinstance(v, numbers.Real) and not isinstance(v, bool)
                        and v == 0 for v in values)
            ):
                raise YamlUpgradeError(
                    f"{path} cannot initialise radiation memory for a fresh run. "
                    "Only legacy seven-zero placeholders can be migrated. "
                    "Remove the field to cold-start, or use a complete checkpoint "
                    "for continuation."
                )
            del states[name]
            pending_logs.append(
                f"[yaml-upgrade] dropped {path}: legacy zero placeholder; "
                "radiation memory is formed internally during cold start"
            )
    return cfg, pending_logs


def _migrate_2026_6_dev12_to_2026_6_dev13(cfg: dict) -> dict:
    """Remove old generated zeros without silently discarding restart values."""
    cfg, pending_logs = _drop_legacy_radiation_memory_zeros(cfg)
    for message in pending_logs:
        _log(message)
    return cfg


def _with_radiation_memory_delta(handler):
    @wraps(handler)
    def migrate(cfg, *, migration_context=None):
        cfg = apply_migration_handler(
            handler, deepcopy(cfg), migration_context=migration_context
        )
        return _migrate_2026_6_dev12_to_2026_6_dev13(cfg)
    return migrate


for (_source, _target), _handler in list(_HANDLERS.items()):
    if _target == CURRENT_SCHEMA_VERSION and _source != CURRENT_SCHEMA_VERSION:
        _HANDLERS[(_source, "2026.6.dev12")] = _handler
        _HANDLERS[(_source, _target)] = _with_radiation_memory_delta(_handler)
_HANDLERS[("2026.6.dev12", CURRENT_SCHEMA_VERSION)] = _migrate_2026_6_dev12_to_2026_6_dev13


def _is_bool_like(value: object) -> bool:
    """Return True for Python ``bool`` and NumPy ``bool_`` scalars."""
    return isinstance(value, bool) or type(value).__name__ == "bool_"


def _is_real_zero(value: object) -> bool:
    """Return True when ``value`` is a non-boolean real zero."""
    return isinstance(value, numbers.Real) and not _is_bool_like(value) and value == 0


def _is_real_equal(value: object, expected: float) -> bool:
    """Return True when ``value`` is a finite non-boolean real equal to ``expected``."""
    if not isinstance(value, numbers.Real) or _is_bool_like(value):
        return False
    try:
        numeric = float(value)
    except OverflowError:
        return False
    return math.isfinite(numeric) and numeric == expected


def _extract_control_tstep(cfg: dict) -> float | None:
    """Extract numeric ``model.control.tstep`` when present and finite."""
    model = cfg.get("model")
    control = model.get("control") if isinstance(model, dict) else None
    raw = control.get("tstep") if isinstance(control, dict) else None
    if isinstance(raw, dict):
        raw = raw.get("value")
    if isinstance(raw, numbers.Real) and not _is_bool_like(raw):
        try:
            numeric = float(raw)
        except OverflowError:
            return None
        if math.isfinite(numeric):
            return numeric
    return None


def _is_legacy_zero_wu(wu: object) -> bool:
    """Return True when ``wu`` is a legacy all-zero WaterUse placeholder mapping."""
    if not isinstance(wu, dict):
        return False
    allowed_keys = {"wu_total", "wu_auto", "wu_manual", "ref"}
    if not set(wu.keys()).issubset(allowed_keys):
        return False
    if wu.get("ref") is not None:
        return False
    for field in ("wu_total", "wu_auto", "wu_manual"):
        if field not in wu:
            continue
        entry = wu[field]
        if _is_real_zero(entry):
            continue
        if (
            isinstance(entry, dict)
            and set(entry.keys()).issubset({"value", "ref"})
            and "value" in entry
            and entry.get("ref") is None
            and _is_real_zero(entry["value"])
        ):
            continue
        return False
    return True


def _drop_legacy_continuation_state_placeholders(cfg: dict) -> tuple[dict, list[str]]:
    """Drop legacy continuation-state placeholders without mutating ``cfg``.

    Returns the copied mapping and pending log messages; refuses non-placeholder
    values with ``YamlUpgradeError`` so restart state is never silently dropped.
    """
    cfg = deepcopy(cfg)
    pending_logs: list[str] = []
    sites = cfg.get("sites")
    if not isinstance(sites, list):
        return cfg, pending_logs
    control_tstep = _extract_control_tstep(cfg)
    zero_only_fields = ("dt_since_start", "l_mod", "ustar", "ra_h", "rb", "rs")
    for index, site in enumerate(sites):
        states = site.get("initial_states") if isinstance(site, dict) else None
        if not isinstance(states, dict):
            continue
        for name in zero_only_fields:
            if name not in states:
                continue
            path = f"sites[{index}].initial_states.{name}"
            if not _is_real_zero(states[name]):
                raise YamlUpgradeError(
                    f"{path} cannot initialise continuation state for a fresh run. "
                    "Only legacy zero placeholders can be migrated. "
                    "Remove the field to cold-start, or use a complete checkpoint "
                    "for continuation."
                )
            del states[name]
            pending_logs.append(
                f"[yaml-upgrade] dropped {path}: legacy zero placeholder; "
                "continuation state is formed internally during cold start"
            )
        if "tair_av" in states:
            path = f"sites[{index}].initial_states.tair_av"
            val = states["tair_av"]
            if not (_is_real_zero(val) or _is_real_equal(val, 273.15)):
                raise YamlUpgradeError(
                    f"{path} cannot initialise continuation state for a fresh run. "
                    "Only legacy cold-start placeholders (0 or 273.15) can be migrated. "
                    "Remove the field to cold-start, or use a complete checkpoint "
                    "for continuation."
                )
            del states["tair_av"]
            pending_logs.append(
                f"[yaml-upgrade] dropped {path}: legacy cold-start placeholder; "
                "tair_av is formed internally during cold start"
            )
        if "tstep_prev" in states:
            path = f"sites[{index}].initial_states.tstep_prev"
            val = states["tstep_prev"]
            allowed_tstep = control_tstep is not None and _is_real_equal(val, control_tstep)
            if not (_is_real_zero(val) or allowed_tstep):
                raise YamlUpgradeError(
                    f"{path} cannot initialise continuation state for a fresh run. "
                    "Only legacy cold-start placeholders (0 or model.control.tstep) "
                    "can be migrated. Remove the field to cold-start, or use a "
                    "complete checkpoint for continuation."
                )
            del states["tstep_prev"]
            pending_logs.append(
                f"[yaml-upgrade] dropped {path}: legacy cold-start placeholder; "
                "tstep_prev is seeded from model.control.tstep during cold start"
            )
        for veg in ("evetr", "dectr", "grass"):
            veg_state = states.get(veg)
            if not isinstance(veg_state, dict) or "wu" not in veg_state:
                continue
            path = f"sites[{index}].initial_states.{veg}.wu"
            if not _is_legacy_zero_wu(veg_state["wu"]):
                raise YamlUpgradeError(
                    f"{path} cannot initialise irrigation memory for a fresh run. "
                    "Only legacy zero placeholders can be migrated. "
                    "Remove the field to cold-start, or use a complete checkpoint "
                    "for continuation."
                )
            del veg_state["wu"]
            pending_logs.append(
                f"[yaml-upgrade] dropped {path}: legacy zero placeholder; "
                "irrigation memory is formed internally during cold start"
            )
    return cfg, pending_logs


def _migrate_2026_6_dev13_to_2026_6_dev14(cfg: dict) -> dict:
    """Remove legacy continuation-state placeholders without discarding restart values."""
    cfg, pending_logs = _drop_legacy_continuation_state_placeholders(cfg)
    for message in pending_logs:
        _log(message)
    return cfg


def _with_continuation_state_delta(handler):
    @wraps(handler)
    def migrate(cfg, *, migration_context=None):
        cfg = apply_migration_handler(
            handler, deepcopy(cfg), migration_context=migration_context
        )
        return _migrate_2026_6_dev13_to_2026_6_dev14(cfg)
    return migrate


for (_source, _target), _handler in list(_HANDLERS.items()):
    if _target == CURRENT_SCHEMA_VERSION and _source != CURRENT_SCHEMA_VERSION:
        _HANDLERS[(_source, "2026.6.dev13")] = _handler
        _HANDLERS[(_source, _target)] = _with_continuation_state_delta(_handler)
_HANDLERS[("2026.6.dev13", CURRENT_SCHEMA_VERSION)] = _migrate_2026_6_dev13_to_2026_6_dev14


def _migrate_2026_6_dev14_to_2026_6_dev15(cfg: dict) -> dict:
    """Preserve supplied drainage coefficients; never infer them from omission."""
    result = deepcopy(cfg)
    sites = result.get("sites")
    if not isinstance(sites, list):
        return result
    missing = []
    for i, site in enumerate(sites):
        if not isinstance(site, dict):
            continue
        props = site.get("properties")
        land_cover = props.get("land_cover") if isinstance(props, dict) else None
        if not isinstance(land_cover, dict):
            continue
        for name in NON_WATER_SURFACES:
            surface = land_cover.get(name)
            if not isinstance(surface, dict) or _active_surface_sfr(surface) is None:
                continue
            params = surface.get("storage_drain_params", surface.get("storedrainprm"))
            if params is None:
                # Migration also serves partial configuration/annotation work.
                # Preserve omission; engine validation requires the block later.
                continue
            if (
                not isinstance(params, dict)
                or _unwrap_scalar(params.get("drain_eq")) not in (1, 2, 3)
            ):
                continue
            for coef in ("drain_coef_1", "drain_coef_2"):
                if _unwrap_scalar(params.get(coef)) is None:
                    missing.append(
                        f"sites[{i}].properties.land_cover.{name}.storage_drain_params.{coef}"
                    )
    if missing:
        raise YamlUpgradeError(
            "Cannot migrate drainage coefficients to schema 2026.6.dev15: "
            + "; ".join(missing)
            + ". Supply the missing drainage block or coefficients before migration. "
            "Explicit zero is allowed; omission does not establish a zero coefficient."
        )
    return result


def _with_drainage_delta(handler):
    @wraps(handler)
    def migrate(cfg, *, migration_context=None):
        cfg = apply_migration_handler(
            handler, deepcopy(cfg), migration_context=migration_context
        )
        return _migrate_2026_6_dev14_to_2026_6_dev15(cfg)
    return migrate


for (_source, _target), _handler in list(_HANDLERS.items()):
    if _target == CURRENT_SCHEMA_VERSION and _source != CURRENT_SCHEMA_VERSION:
        _HANDLERS[(_source, "2026.6.dev14")] = _handler
        _HANDLERS[(_source, _target)] = _with_drainage_delta(_handler)
_HANDLERS[("2026.6.dev14", CURRENT_SCHEMA_VERSION)] = _migrate_2026_6_dev14_to_2026_6_dev15


def _clear_nonfinite_sdd_for_dev9_candidate(candidate: dict) -> None:
    """Keep pre-dev16 SDD representations out of the throwaway dev9 validation copy."""
    sites = candidate.get("sites")
    if not isinstance(sites, list):
        return
    for site in sites:
        states = site.get("initial_states") if isinstance(site, dict) else None
        if not isinstance(states, dict):
            continue
        for veg in ("evetr", "dectr", "grass"):
            surface = states.get(veg)
            if not isinstance(surface, dict) or "sdd_id" not in surface:
                continue
            raw = _unwrap_scalar(surface["sdd_id"])
            if type(raw).__module__ == "numpy" and hasattr(raw, "item"):
                raw = raw.item()
            if isinstance(raw, bool):
                surface.pop("sdd_id")
                continue
            if isinstance(raw, numbers.Real) or type(raw).__module__ == "numpy":
                with suppress(TypeError, ValueError, OverflowError):
                    if not math.isfinite(float(raw)):
                        surface.pop("sdd_id")
                continue
            if isinstance(raw, str):
                with suppress(ValueError):
                    if not math.isfinite(float(raw)):
                        surface.pop("sdd_id")


def _max_dls_doy_for_config(cfg: dict) -> int:
    """Return 365 for non-leap-year start_time and 366 for leap or unknown years.

    Mirrors ``SUEWSConfig.validate_dls_parameters``.
    """
    model = cfg.get("model")
    control = model.get("control") if isinstance(model, dict) else None
    start_time = (
        _unwrap_scalar(control.get("start_time")) if isinstance(control, dict) else None
    )
    if start_time is not None:
        with suppress(ValueError, TypeError):
            year = int(str(start_time)[:4])
            if not calendar.isleap(year):
                return 365
    return 366


def _clear_noninteger_doy_for_dev9_candidate(candidate: dict) -> None:
    """Replace pre-dev17 valid DOY values on a throwaway dev9 validation copy.

    The dev8 -> dev9 observed-soil migration validates a throwaway copy with the
    current ``SUEWSConfig`` model, which enforces dev17 integer-only DOY rules.
    Historical targets ``dev9``..``dev16`` still accepted ``float`` DOYs (with
    ``ge=1, le=366`` already enforced on ``startdls``/``enddls`` and no bounds
    on ``ie_start``/``ie_end``). Replace only values that were historically
    valid before dev17 on that throwaway copy with ``1`` (preserving pair
    invariants) so historical targets keep pre-dev17 behaviour without relaxing
    pre-existing DLS bounds or mutating the returned config.
    """
    sites = candidate.get("sites")
    if not isinstance(sites, list):
        return
    max_dls_doy = float(_max_dls_doy_for_config(candidate))
    doy_specs = (
        ("irrigation", "ie_start", False),
        ("irrigation", "ie_end", False),
        ("anthropogenic_emissions", "startdls", True),
        ("anthropogenic_emissions", "enddls", True),
    )
    for site in sites:
        props = site.get("properties") if isinstance(site, dict) else None
        if not isinstance(props, dict):
            continue
        for section_name, field_name, has_pre_dev17_bounds in doy_specs:
            section = props.get(section_name)
            if not isinstance(section, dict) or field_name not in section:
                continue
            entry = section[field_name]
            if isinstance(entry, dict) and not (set(entry.keys()) <= {"value", "ref"}):
                continue
            raw = _unwrap_scalar(entry)
            if type(raw).__module__ == "numpy" and hasattr(raw, "item"):
                raw = raw.item()
            if isinstance(raw, bool):
                # Pre-dev17 float fields accepted True (1.0) on both irrigation
                # and DLS, and False (0.0) only on unbounded irrigation fields.
                if not has_pre_dev17_bounds or raw is True:
                    section[field_name] = 1
                continue
            if isinstance(raw, numbers.Real) or type(raw).__module__ == "numpy":
                with suppress(TypeError, ValueError, OverflowError):
                    fval = float(raw)
                    if has_pre_dev17_bounds:
                        if math.isfinite(fval) and 1.0 <= fval <= max_dls_doy and not fval.is_integer():
                            section[field_name] = 1
                    elif (
                        not math.isfinite(fval)
                        or not fval.is_integer()
                        or not (1 <= int(fval) <= 366)
                    ):
                        section[field_name] = 1
                continue
            if isinstance(raw, str):
                with suppress(ValueError):
                    fval = float(raw)
                    if not has_pre_dev17_bounds or (
                        math.isfinite(fval) and 1.0 <= fval <= max_dls_doy
                    ):
                        section[field_name] = 1


def validate_vegetation_sdd_raw(cfg: dict) -> None:
    """Report every explicitly invalid vegetation SDD without changing its source."""
    sites = cfg.get("sites")
    if not isinstance(sites, list):
        return
    problems = []
    for index, site in enumerate(sites):
        if not isinstance(site, dict) or not isinstance(site.get("initial_states"), dict):
            continue
        grid = site.get("gridiv")
        location = f"sites[{index}]" if grid is None else f"sites[{index}] (gridiv={grid})"
        for name in ("evetr", "dectr", "grass"):
            surface = site["initial_states"].get(name)
            if not isinstance(surface, dict) or "sdd_id" not in surface:
                continue
            value = _unwrap_scalar(surface["sdd_id"])
            field_path = f"{location}.initial_states.{name}.sdd_id"
            if isinstance(value, Mapping):
                problems.append(
                    f"{field_path} must be a finite number in K d. Supply the initial value "
                    "from this vegetation type's site or restart record. Omit the field "
                    "only when initialising it at zero is intended."
                )
                continue
            # The public model also accepts quoted numbers. Preserve the source form.
            if isinstance(value, str):
                with suppress(ValueError):
                    value = float(value)
            try:
                validate_sdd_id(value, field_path)
            except ValueError as error:
                problems.append(str(error))
    if problems:
        raise YamlUpgradeError("\n".join(problems))


def _with_senescence_check(handler):
    @wraps(handler)
    def migrate(cfg, *, migration_context=None):
        cfg = apply_migration_handler(handler, cfg, migration_context=migration_context)
        validate_vegetation_sdd_raw(cfg)
        return cfg
    return migrate


def _migrate_2026_6_dev15_to_2026_6_dev16(cfg: dict) -> dict:
    validate_vegetation_sdd_raw(cfg)
    return deepcopy(cfg)


for (_source, _target), _handler in list(_HANDLERS.items()):
    if _target == CURRENT_SCHEMA_VERSION and _source != CURRENT_SCHEMA_VERSION:
        _HANDLERS[(_source, "2026.6.dev15")] = _handler
        _HANDLERS[(_source, _target)] = _with_senescence_check(_handler)
_HANDLERS[("2026.6.dev15", CURRENT_SCHEMA_VERSION)] = _migrate_2026_6_dev15_to_2026_6_dev16
_HANDLERS[(CURRENT_SCHEMA_VERSION, CURRENT_SCHEMA_VERSION)] = _with_senescence_check(_identity)


def _check_and_normalise_integer_doys(cfg: dict) -> dict:
    """Normalise whole-number float DOYs to ``int`` and refuse fractional/invalid values.

    Day-of-year fields (``ie_start``, ``ie_end``, ``startdls``, ``enddls``) are
    integers in ``1..366`` (gh#2101). Legacy whole-number floats such as
    ``86.0`` are normalised to ``86`` without altering model semantics;
    fractional floats (such as ``20.5``), booleans, non-finite numbers,
    malformed strings, or out-of-range integers fail with ``YamlUpgradeError``
    so users replace them explicitly rather than silently truncating or zeroing.
    """
    from ...data_model.core.human_activity import coerce_integer_doy_scalar

    cfg = deepcopy(cfg)
    sites = cfg.get("sites")
    if not isinstance(sites, list):
        return cfg

    max_dls_doy = _max_dls_doy_for_config(cfg)
    doy_specs = (
        ("irrigation", "ie_start", 366),
        ("irrigation", "ie_end", 366),
        ("anthropogenic_emissions", "startdls", max_dls_doy),
        ("anthropogenic_emissions", "enddls", max_dls_doy),
    )
    pending_logs: list[str] = []

    for index, site in enumerate(sites):
        props = site.get("properties") if isinstance(site, dict) else None
        if not isinstance(props, dict):
            continue
        for section_name, field_name, max_doy in doy_specs:
            section = props.get(section_name)
            if not isinstance(section, dict) or field_name not in section:
                continue
            entry = section[field_name]
            path = f"sites[{index}].properties.{section_name}.{field_name}"
            if (
                isinstance(entry, Mapping)
                and "value" not in entry
                and all(key == "ref" for key in entry)
            ):
                # Empty or ref-only mapping (``{}`` / ``{ref: ...}``): disabled window.
                continue
            if isinstance(entry, Mapping) and "value" in entry:
                raw = entry["value"]
                wrapped = True
            else:
                raw = entry
                wrapped = False

            if raw is None:
                continue

            try:
                coerced = coerce_integer_doy_scalar(raw, path)
            except ValueError as exc:
                raise YamlUpgradeError(
                    f"{exc} Replace fractional or invalid day-of-year values "
                    f"manually with an integer day of year (1..{max_doy}) before upgrading."
                ) from exc

            if not (1 <= coerced <= max_doy):
                raise YamlUpgradeError(
                    f"{path} has out-of-range day-of-year value {coerced}; "
                    f"day-of-year field {field_name} requires an integer day of "
                    f"year between 1 and {max_doy}."
                )

            if type(raw) is not int:
                if wrapped:
                    entry["value"] = coerced
                else:
                    section[field_name] = coerced
                pending_logs.append(
                    f"[yaml-upgrade] normalised {path}: {raw!r} -> {coerced} "
                    "(integer day of year)"
                )

    for message in pending_logs:
        _log(message)
    return cfg


def _migrate_2026_6_dev16_to_2026_6_dev17(cfg: dict) -> dict:
    """Enforce integer day-of-year fields (1..366) for irrigation and DLS."""
    validate_vegetation_sdd_raw(cfg)
    return _check_and_normalise_integer_doys(cfg)


def _with_integer_doy_delta(handler):
    @wraps(handler)
    def migrate(cfg, *, migration_context=None):
        cfg = apply_migration_handler(handler, cfg, migration_context=migration_context)
        return _check_and_normalise_integer_doys(cfg)
    return migrate


for (_source, _target), _handler in list(_HANDLERS.items()):
    if _target == CURRENT_SCHEMA_VERSION and _source != CURRENT_SCHEMA_VERSION:
        _HANDLERS[(_source, "2026.6.dev16")] = _handler
        _HANDLERS[(_source, _target)] = _with_integer_doy_delta(_handler)
_HANDLERS[("2026.6.dev16", CURRENT_SCHEMA_VERSION)] = _migrate_2026_6_dev16_to_2026_6_dev17


def _migrate_2026_6_dev17_to_2026_6_dev18(cfg: dict) -> dict:
    """Record the default EHC surface-coupling change for storage_heat=5 YAMLs."""
    physics = (cfg.get("model") or {}).get("physics") or {}
    storage_heat = physics.get("storage_heat", physics.get("storageheatmethod"))
    if isinstance(storage_heat, dict):
        storage_heat = storage_heat.get("value", storage_heat)
    is_ehc = storage_heat in (5, "5", "ehc", "EHC") or (
        isinstance(storage_heat, dict) and "ehc" in storage_heat
    )
    if is_ehc and "ehc_surface_coupling" not in physics:
        _log(
            "[yaml-upgrade] model.physics.ehc_surface_coupling omitted with storage_heat=5: "
            "2026.6.dev18 defaults to implicit coupling; set ehc_surface_coupling: explicit "
            "to retain legacy explicit coupling"
        )
    return cfg


def _with_ehc_implicit_default_delta(handler):
    @wraps(handler)
    def migrate(cfg, *, migration_context=None):
        cfg = apply_migration_handler(
            handler, deepcopy(cfg), migration_context=migration_context
        )
        return _migrate_2026_6_dev17_to_2026_6_dev18(cfg)

    return migrate


for (_source, _target), _handler in list(_HANDLERS.items()):
    if _target == CURRENT_SCHEMA_VERSION and _source != CURRENT_SCHEMA_VERSION:
        _HANDLERS[(_source, "2026.6.dev17")] = _handler
        _HANDLERS[(_source, _target)] = _with_ehc_implicit_default_delta(_handler)
_HANDLERS[("2026.6.dev17", "2026.6.dev17")] = _with_integer_doy_delta(
    _with_senescence_check(_identity)
)
_HANDLERS[("2026.6.dev17", CURRENT_SCHEMA_VERSION)] = _with_ehc_implicit_default_delta(
    _with_integer_doy_delta(_with_senescence_check(_identity))
)
_HANDLERS[(CURRENT_SCHEMA_VERSION, CURRENT_SCHEMA_VERSION)] = _with_integer_doy_delta(
    _with_senescence_check(_identity)
)

_WEEKLY_PROFILE_DAYS: tuple[str, ...] = (
    "monday",
    "tuesday",
    "wednesday",
    "thursday",
    "friday",
    "saturday",
    "sunday",
)


def _weekly_profile_is_asymmetric(profile: object) -> bool:
    """Return True if a WeeklyProfile mapping assigns differing weekday values."""
    if not isinstance(profile, Mapping):
        return False
    if "value" in profile and isinstance(profile["value"], Mapping):
        profile = profile["value"]
    values: list[object] = []
    for day in _WEEKLY_PROFILE_DAYS:
        raw = profile.get(day, 0.0)
        if isinstance(raw, Mapping) and "value" in raw:
            raw = raw["value"]
        if isinstance(raw, (int, float)) and not isinstance(raw, bool):
            val = float(raw)
            if math.isnan(val):
                return True
            values.append(val)
        else:
            values.append(raw)
    first = values[0]
    return any(val != first for val in values[1:])


def _migrate_2026_6_dev18_to_2026_6_dev19(cfg: dict) -> dict:
    """Log an advisory for asymmetric irrigation weekday schedules without rotating keys."""
    sites = cfg.get("sites")
    if not isinstance(sites, list):
        return cfg

    for index, site in enumerate(sites):
        props = site.get("properties") if isinstance(site, Mapping) else None
        irrigation = props.get("irrigation") if isinstance(props, Mapping) else None
        if not isinstance(irrigation, Mapping):
            continue
        for field_name in ("daywat", "daywatper"):
            if field_name not in irrigation:
                continue
            if _weekly_profile_is_asymmetric(irrigation[field_name]):
                _log(
                    f"[yaml-upgrade] sites[{index}].properties.irrigation.{field_name} "
                    "has an asymmetric weekday schedule: 2026.6.dev19 aligns irrigation "
                    "weekday slots with calendar days (1=Sunday .. 7=Saturday in legacy "
                    "tables, 0=Sunday .. 6=Saturday in df_state). Keys were left "
                    "unchanged; if this YAML was converted from legacy "
                    "SUEWS_Irrigation.txt tables under an earlier schema, verify that "
                    "weekday entries match the intended calendar days."
                )
    return cfg


def _with_irrigation_weekday_advisory_delta(handler):
    @wraps(handler)
    def migrate(cfg, *, migration_context=None):
        cfg = apply_migration_handler(
            handler, deepcopy(cfg), migration_context=migration_context
        )
        return _migrate_2026_6_dev18_to_2026_6_dev19(cfg)

    return migrate


for (_source, _target), _handler in list(_HANDLERS.items()):
    if _target == CURRENT_SCHEMA_VERSION and _source != CURRENT_SCHEMA_VERSION:
        _HANDLERS[(_source, "2026.6.dev18")] = _handler
        _HANDLERS[(_source, _target)] = _with_irrigation_weekday_advisory_delta(
            _handler
        )
_HANDLERS[("2026.6.dev18", "2026.6.dev18")] = _with_integer_doy_delta(
    _with_senescence_check(_identity)
)
_HANDLERS[("2026.6.dev18", CURRENT_SCHEMA_VERSION)] = (
    _with_irrigation_weekday_advisory_delta(
        _with_integer_doy_delta(_with_senescence_check(_identity))
    )
)
_HANDLERS[(CURRENT_SCHEMA_VERSION, CURRENT_SCHEMA_VERSION)] = _with_integer_doy_delta(
    _with_senescence_check(_identity)
)


def _clear_noncanonical_ten_minute_keys_for_dev9_candidate(candidate: dict) -> None:
    """Sanitise pre-dev20 malformed/colliding 10-minute keys on the dev9 throwaway copy.

    The dev8 -> dev9 observed-soil migration validates a throwaway copy with the
    current ``SUEWSConfig`` model, which enforces dev20 10-minute profile key
    validation. Pre-dev20 ``TenMinuteProfile`` coerced arbitrary dict keys with
    ``str(k)`` while still validating float/None values. Rewrite only mappings
    whose keys fail dev20 validation (and whose values are all None or numeric)
    on that throwaway copy so historical targets ``dev9``..``dev19`` retain
    pre-dev20 behaviour and targeting ``dev20`` fails cleanly with
    ``YamlUpgradeError`` rather than Pydantic ``ValidationError``.
    """
    sites = candidate.get("sites")
    if not isinstance(sites, list):
        return
    for site in sites:
        props = site.get("properties") if isinstance(site, dict) else None
        if not isinstance(props, dict):
            continue
        for section_name, _canonical_field, candidate_names in (
            _TEN_MINUTE_PROFILE_ALIAS_GROUPS
        ):
            section = props.get(section_name)
            if not isinstance(section, dict):
                continue
            for field_name in candidate_names:
                profile = section.get(field_name)
                if not isinstance(profile, dict):
                    continue
                for day_type in ("working_day", "holiday"):
                    day_map = profile.get(day_type)
                    if not isinstance(day_map, dict):
                        continue
                    try:
                        normalise_ten_minute_profile_mapping(day_map)
                        continue
                    except ValueError:
                        pass
                    sanitised: dict[str, float | None] = {}
                    all_values_valid = True
                    for idx, val in enumerate(day_map.values()):
                        if val is None:
                            coerced_val: float | None = None
                        else:
                            try:
                                coerced_val = float(val)
                            except (TypeError, ValueError):
                                all_values_valid = False
                                break
                        sanitised[str((idx % 144) + 1)] = coerced_val
                    if all_values_valid:
                        profile[day_type] = sanitised


def _check_and_normalise_ten_minute_profile_keys(cfg: dict) -> dict:
    """Normalise valid 10-minute STEBBS profile keys and reject malformed/colliding keys."""
    cfg = deepcopy(cfg)
    sites = cfg.get("sites")
    if not isinstance(sites, list):
        return cfg

    problems: list[str] = []
    pending_logs: list[str] = []

    for index, site in enumerate(sites):
        props = site.get("properties") if isinstance(site, dict) else None
        if not isinstance(props, dict):
            continue
        for section_name, canonical_field, candidate_names in (
            _TEN_MINUTE_PROFILE_ALIAS_GROUPS
        ):
            section = props.get(section_name)
            if not isinstance(section, dict):
                continue
            present = [name for name in candidate_names if name in section]
            if len(present) > 1:
                problems.append(
                    f"sites[{index}].properties.{section_name}: conflicting aliases "
                    f"for '{canonical_field}': {', '.join(repr(n) for n in present)}"
                )
                continue
            for field_name in present:
                profile = section.get(field_name)
                if not isinstance(profile, dict):
                    continue
                for day_type in ("working_day", "holiday"):
                    day_map = profile.get(day_type)
                    if not isinstance(day_map, Mapping):
                        continue
                    path = (
                        f"sites[{index}].properties.{section_name}."
                        f"{field_name}.{day_type}"
                    )
                    try:
                        canonical_map = normalise_ten_minute_profile_mapping(
                            dict(day_map)
                        )
                    except ValueError as exc:
                        problems.append(f"{path}: {exc}")
                        continue
                    has_non_canonical = any(
                        not isinstance(raw_k, str)
                        or normalise_ten_minute_profile_key(raw_k) != raw_k
                        for raw_k in day_map
                    )
                    if has_non_canonical:
                        profile[day_type] = canonical_map
                        pending_logs.append(
                            f"[yaml-upgrade] normalised {path}: non-canonical 10-minute "
                            "profile keys -> canonical '1'..'144' strings"
                        )

    if problems:
        raise YamlUpgradeError("\n".join(problems))

    for message in pending_logs:
        _log(message)
    return cfg


def _with_ten_minute_profile_key_delta(handler):
    @wraps(handler)
    def migrate(cfg, *, migration_context=None):
        cfg = apply_migration_handler(
            handler, deepcopy(cfg), migration_context=migration_context
        )
        return _check_and_normalise_ten_minute_profile_keys(cfg)

    return migrate


for (_source, _target), _handler in list(_HANDLERS.items()):
    if _target == CURRENT_SCHEMA_VERSION and _source != CURRENT_SCHEMA_VERSION:
        _HANDLERS[(_source, "2026.6.dev19")] = _handler
        _HANDLERS[(_source, _target)] = _with_ten_minute_profile_key_delta(_handler)
_HANDLERS[("2026.6.dev19", "2026.6.dev19")] = _with_integer_doy_delta(
    _with_senescence_check(_identity)
)
_HANDLERS[("2026.6.dev19", CURRENT_SCHEMA_VERSION)] = (
    _with_ten_minute_profile_key_delta(
        _with_integer_doy_delta(_with_senescence_check(_identity))
    )
)
_HANDLERS[(CURRENT_SCHEMA_VERSION, CURRENT_SCHEMA_VERSION)] = (
    _with_ten_minute_profile_key_delta(
        _with_integer_doy_delta(_with_senescence_check(_identity))
    )
)


def _is_present_doy_entry(entry: object) -> bool:
    """Return True when a raw YAML DOY entry carries a non-null scalar value."""
    if entry is None:
        return False
    if isinstance(entry, Mapping):
        if "value" in entry:
            return entry["value"] is not None
        return False
    return True


def _advise_land_cover_and_dst_policy(cfg: dict) -> dict:
    """Log non-mutating advisories for dev21 land-cover sum and half-DST policies."""
    from ...data_model.validation.rules.hard_invariants import (  # noqa: PLC0415
        check_land_cover_fraction_sum,
    )

    sites = cfg.get("sites")
    if not isinstance(sites, list):
        return cfg

    for index, site in enumerate(sites):
        props = site.get("properties") if isinstance(site, Mapping) else None
        if not isinstance(props, Mapping):
            continue

        land_cover = props.get("land_cover")
        if isinstance(land_cover, Mapping) and land_cover:
            fractions: dict[str, object] = {}
            for surface_name, raw_surface in land_cover.items():
                surface_props = raw_surface
                if (
                    isinstance(surface_props, Mapping)
                    and "value" in surface_props
                    and isinstance(surface_props["value"], Mapping)
                ):
                    surface_props = surface_props["value"]
                if not isinstance(surface_props, Mapping) or "sfr" not in surface_props:
                    continue
                sfr_entry = surface_props["sfr"]
                if isinstance(sfr_entry, Mapping) and "value" in sfr_entry:
                    sfr_val = sfr_entry["value"]
                elif isinstance(sfr_entry, Mapping):
                    sfr_val = None
                else:
                    sfr_val = sfr_entry
                if sfr_val is not None:
                    fractions[str(surface_name)] = sfr_val
            if fractions:
                sum_check = check_land_cover_fraction_sum(fractions)
                if sum_check.band == "error" and sum_check.violation is not None:
                    _log(
                        f"[yaml-upgrade] sites[{index}].properties.land_cover: "
                        f"{sum_check.violation.message}. Schema 2026.6.dev21 rejects "
                        "land-cover surface fraction sum deviations exceeding 1e-4 "
                        "in Phase B, Phase C, SUEWSConfig.validate_for_engine(), "
                        "and SUEWSSimulation.run()."
                    )
                elif sum_check.band == "auto_fix" and sum_check.violation is not None:
                    _log(
                        f"[yaml-upgrade] sites[{index}].properties.land_cover: "
                        f"{sum_check.violation.message}"
                    )

        anthro = props.get("anthropogenic_emissions")
        if isinstance(anthro, Mapping):
            start_present = _is_present_doy_entry(anthro.get("startdls"))
            end_present = _is_present_doy_entry(anthro.get("enddls"))
            if start_present != end_present:
                _log(
                    f"[yaml-upgrade] sites[{index}].properties.anthropogenic_emissions: "
                    "only one of startdls/enddls is specified. Schema 2026.6.dev21 "
                    "treats half-specified daylight saving as a blocking Phase B error "
                    "unless both bounds are set, both are omitted/null, or the file "
                    "is validated with --science-fixes apply."
                )

    return cfg


def _with_land_cover_and_dst_advisory_delta(handler):
    @wraps(handler)
    def migrate(cfg, *, migration_context=None):
        cfg = apply_migration_handler(
            handler, deepcopy(cfg), migration_context=migration_context
        )
        return _advise_land_cover_and_dst_policy(cfg)

    return migrate


for (_source, _target), _handler in list(_HANDLERS.items()):
    if _target == CURRENT_SCHEMA_VERSION and _source != CURRENT_SCHEMA_VERSION:
        _HANDLERS[(_source, "2026.6.dev20")] = _handler
        _HANDLERS[(_source, _target)] = _with_land_cover_and_dst_advisory_delta(
            _handler
        )
_HANDLERS[("2026.6.dev20", "2026.6.dev20")] = _with_ten_minute_profile_key_delta(
    _with_integer_doy_delta(_with_senescence_check(_identity))
)
_HANDLERS[("2026.6.dev20", CURRENT_SCHEMA_VERSION)] = (
    _with_land_cover_and_dst_advisory_delta(
        _with_ten_minute_profile_key_delta(
            _with_integer_doy_delta(_with_senescence_check(_identity))
        )
    )
)


def _clear_non_owning_vertical_layer_optical_keys_for_dev9_candidate(
    candidate: dict,
) -> dict:
    """Sanitise pre-dev22 vertical-layer optical inputs on the dev9 throwaway copy.

    The dev8 -> dev9 observed-soil migration validates a throwaway copy with the
    current ``SUEWSConfig`` model, which enforces dev22 non-optional facet optical
    fields and warns on deprecated cross-facet keys. Clear both cross-facet
    keys and explicit owning nulls on that throwaway copy so historical targets
    ``dev9``..``dev21`` retain pre-dev22 behaviour while targeting ``dev22``
    rejects explicit owning nulls via ``YamlUpgradeError``.
    """
    cleaned, _ = strip_vertical_layer_cross_facet_keys_in_raw_config(candidate)
    cleaned, _ = collect_and_clear_vertical_layer_optical_nulls_in_raw_config(
        cleaned, clear_nulls=True
    )
    return cleaned if isinstance(cleaned, dict) else candidate


def _strip_non_owning_vertical_layer_optical_keys(cfg: dict) -> dict:
    """Enforce dev22 non-null owning optical fields and drop cross-facet keys."""
    raw_sites = cfg.get("sites") if isinstance(cfg, dict) else None
    _, null_hits = collect_and_clear_vertical_layer_optical_nulls_in_raw_config(
        cfg, clear_nulls=False
    )
    if null_hits:
        problems = []
        for site_idx, facet_attr, layer_idx, owning_key in null_hits:
            site_obj = (
                raw_sites[site_idx]
                if isinstance(raw_sites, list) and site_idx < len(raw_sites)
                else None
            )
            prefix = (
                f"{_format_site_label(site_idx, site_obj)} "
                f"properties.vertical_layers.{facet_attr}[{layer_idx}]"
            )
            problems.append(
                f"{prefix}.{owning_key}: explicit null is disallowed in schema "
                "2026.6.dev22; supply a finite number or omit the field to use the default."
            )
        raise YamlUpgradeError("\n".join(problems))

    result, removed = strip_vertical_layer_cross_facet_keys_in_raw_config(cfg)
    for site_idx, facet_attr, layer_idx, key, owner_path in removed:
        site_obj = (
            raw_sites[site_idx]
            if isinstance(raw_sites, list) and site_idx < len(raw_sites)
            else None
        )
        prefix = (
            f"{_format_site_label(site_idx, site_obj)} "
            f"properties.vertical_layers.{facet_attr}[{layer_idx}]"
        )
        _log(
            f"[yaml-upgrade] {prefix}: dropped {key!r} "
            f"(deprecated cross-facet key ignored on {facet_attr}; owned by {owner_path})"
        )
    return result


def _with_vertical_layer_split_delta(handler):
    @wraps(handler)
    def migrate(cfg, *, migration_context=None):
        cfg = apply_migration_handler(
            handler, deepcopy(cfg), migration_context=migration_context
        )
        return _strip_non_owning_vertical_layer_optical_keys(cfg)

    return migrate


for (_source, _target), _handler in list(_HANDLERS.items()):
    if _target == CURRENT_SCHEMA_VERSION and _source != CURRENT_SCHEMA_VERSION:
        _HANDLERS[_source, "2026.6.dev21"] = _handler
        _HANDLERS[_source, _target] = _with_vertical_layer_split_delta(_handler)
_HANDLERS["2026.6.dev21", "2026.6.dev21"] = _with_land_cover_and_dst_advisory_delta(
    _with_ten_minute_profile_key_delta(
        _with_integer_doy_delta(_with_senescence_check(_identity))
    )
)
_HANDLERS["2026.6.dev21", CURRENT_SCHEMA_VERSION] = _with_vertical_layer_split_delta(
    _with_land_cover_and_dst_advisory_delta(
        _with_ten_minute_profile_key_delta(
            _with_integer_doy_delta(_with_senescence_check(_identity))
        )
    )
)
_HANDLERS[CURRENT_SCHEMA_VERSION, CURRENT_SCHEMA_VERSION] = (
    _with_vertical_layer_split_delta(
        _with_land_cover_and_dst_advisory_delta(
            _with_ten_minute_profile_key_delta(
                _with_integer_doy_delta(_with_senescence_check(_identity))
            )
        )
    )
)


def _is_observed_lai_mode(cfg: Mapping) -> bool:
    """Return True when model.physics explicitly selects observed LAI (0/'observed')."""
    model = cfg.get("model")
    physics = model.get("physics") if isinstance(model, Mapping) else None
    if not isinstance(physics, Mapping):
        return False
    for key in ("leaf_area_index", "laimethod"):
        if key not in physics:
            continue
        raw = physics[key]
        if isinstance(raw, Mapping) and "value" in raw:
            raw = raw["value"]
        if isinstance(raw, bool):
            continue
        if raw == 0:
            return True
        if isinstance(raw, str) and raw.strip().lower() in ("0", "observed"):
            return True
    return False


def _advise_lai_daily_increment_phenology(cfg: dict) -> dict:
    """Log a non-mutating advisory for modelled-LAI configs upgraded to dev23."""
    if _is_observed_lai_mode(cfg):
        return cfg

    sites = cfg.get("sites")
    if not isinstance(sites, list):
        return cfg

    for index, site in enumerate(sites):
        if not isinstance(site, Mapping):
            continue
        props = site.get("properties")
        land_cover = props.get("land_cover") if isinstance(props, Mapping) else None
        init_states = site.get("initial_states")

        has_lai_cfg = False
        if isinstance(land_cover, Mapping):
            for surf in ("evetr", "dectr", "grass"):
                surf_cfg = land_cover.get(surf)
                if isinstance(surf_cfg, Mapping) and isinstance(
                    surf_cfg.get("lai"), Mapping
                ):
                    has_lai_cfg = True
                    break

        has_nonzero_sdd = False
        if isinstance(init_states, Mapping):
            for surf in ("evetr", "dectr", "grass"):
                surf_init = init_states.get(surf)
                if not isinstance(surf_init, Mapping) or "sdd_id" not in surf_init:
                    continue
                raw_sdd = surf_init["sdd_id"]
                if isinstance(raw_sdd, Mapping) and "value" in raw_sdd:
                    raw_sdd = raw_sdd["value"]
                if (
                    isinstance(raw_sdd, (int, float))
                    and not isinstance(raw_sdd, bool)
                    and raw_sdd != 0
                ):
                    has_nonzero_sdd = True
                    break

        if has_lai_cfg or has_nonzero_sdd:
            _log(
                f"[yaml-upgrade] {_format_site_label(index, site)}: schema "
                "2026.6.dev23 computes modelled LAI from daily GDD/SDD "
                "increments within fixed growth/senescence seasons (DOY 15 / "
                "196, swapped in the southern hemisphere). Values were left "
                "unchanged; existing lai_power coefficients (and any non-zero "
                "initial sdd_id) need retuning for the daily-increment "
                "formulation, and legacy gdd_full/sdd_full/lai_type fields "
                "are now ignored (use gdd_base/sdd_base for onset thresholds)."
            )

    return cfg


def _with_lai_daily_increment_advisory_delta(handler):
    @wraps(handler)
    def migrate(cfg, *, migration_context=None):
        cfg = apply_migration_handler(
            handler, deepcopy(cfg), migration_context=migration_context
        )
        return _advise_lai_daily_increment_phenology(cfg)

    return migrate


for (_source, _target), _handler in list(_HANDLERS.items()):
    if _target == CURRENT_SCHEMA_VERSION and _source != CURRENT_SCHEMA_VERSION:
        _HANDLERS[_source, "2026.6.dev22"] = _handler
        _HANDLERS[_source, _target] = _with_lai_daily_increment_advisory_delta(_handler)
_HANDLERS["2026.6.dev22", "2026.6.dev22"] = _with_vertical_layer_split_delta(
    _with_land_cover_and_dst_advisory_delta(
        _with_ten_minute_profile_key_delta(
            _with_integer_doy_delta(_with_senescence_check(_identity))
        )
    )
)
_HANDLERS["2026.6.dev22", CURRENT_SCHEMA_VERSION] = (
    _with_lai_daily_increment_advisory_delta(
        _with_vertical_layer_split_delta(
            _with_land_cover_and_dst_advisory_delta(
                _with_ten_minute_profile_key_delta(
                    _with_integer_doy_delta(_with_senescence_check(_identity))
                )
            )
        )
    )
)
_HANDLERS[CURRENT_SCHEMA_VERSION, CURRENT_SCHEMA_VERSION] = (
    _with_vertical_layer_split_delta(
        _with_land_cover_and_dst_advisory_delta(
            _with_ten_minute_profile_key_delta(
                _with_integer_doy_delta(_with_senescence_check(_identity))
            )
        )
    )
)

# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------


class YamlUpgradeError(RuntimeError):
    """Raised when a YAML cannot be upgraded (no handler, missing signature, ...)."""


def _detect_signature(cfg: dict) -> str | None:
    """Return the raw schema signature from the YAML dict, or None if missing.

    Distinct from `SchemaMigrator.auto_detect_version`, which falls back to
    the current schema when no signature is found. Here we want to know
    whether a signature is *actually present* so the CLI can nudge the user
    to supply `--from` when it isn't.
    """
    for key in ("schema_version", "version", "config_version"):
        if key in cfg:
            return str(cfg[key])
    return None


def _resolve_handler_chain(
    from_schema: str, to_schema: str
) -> Iterable[tuple[tuple[str, str], Handler]]:
    """Yield handler entries covering `from_schema -> to_schema`.

    The current registry carries only an identity handler; more handlers will
    land alongside structural schema changes. For now this raises when a
    non-trivial upgrade is asked for.
    """
    if (from_schema, to_schema) in _HANDLERS:
        yield (from_schema, to_schema), _HANDLERS[from_schema, to_schema]
        return
    raise YamlUpgradeError(
        f"No upgrade handler registered for schema {from_schema} -> {to_schema}. "
        "Add one to src/supy/util/converter/yaml_upgrade.py (see #1304)."
    )


def upgrade_yaml(
    input_path: str | Path,
    output_path: str | Path,
    from_ver: str | None = None,
    *,
    migration_context: SoilMigrationContext | None = None,
) -> None:
    """Upgrade a YAML configuration written for an earlier release.

    Parameters
    ----------
    input_path : str or Path
        Existing YAML to be upgraded.
    output_path : str or Path
        Destination for the upgraded YAML.
    from_ver : str, optional
        Source schema version or release tag. When omitted, the source version
        is auto-detected from the YAML's `schema_version` / `version` field.
    """
    input_path = Path(input_path)
    output_path = Path(output_path)

    with input_path.open("r", encoding="utf-8") as f:
        cfg = yaml.safe_load(f) or {}

    signature = _detect_signature(cfg)

    if from_ver is None:
        if signature is None:
            raise YamlUpgradeError(
                "No schema_version field found. This YAML predates the "
                f"v{CURRENT_SCHEMA_VERSION} schema. Re-run with -f/--from "
                "<tag> to specify the source version explicitly."
            )
        source_schema = _resolve_package_to_schema(signature)
        _log(f"[yaml-upgrade] Detected schema version from file: {signature}")
    else:
        source_schema = _resolve_package_to_schema(from_ver)
        if signature is not None and _resolve_package_to_schema(signature) != source_schema:
            _log(
                f"[yaml-upgrade] WARNING: user-supplied --from={from_ver} "
                f"(schema {source_schema}) disagrees with file signature "
                f"{signature}. Respecting user override."
            )
        else:
            _log(f"[yaml-upgrade] Using user-supplied --from={from_ver}")

    target_schema = CURRENT_SCHEMA_VERSION
    _log(
        f"[yaml-upgrade] Source schema: {source_schema}  "
        f"Target schema: {target_schema}"
    )

    if source_schema == target_schema:
        _log(
            f"[yaml-upgrade] No upgrade needed: YAML already at schema "
            f"{target_schema}. Writing defensively cleaned copy to "
            f"{output_path}."
        )

    for (from_s, to_s), handler in _resolve_handler_chain(
        source_schema, target_schema
    ):
        _log(f"[yaml-upgrade] Applying handler {from_s} -> {to_s}: {handler.__name__}")
        cfg = apply_migration_handler(handler, cfg, migration_context=migration_context)

    cfg["schema_version"] = target_schema

    output_path.parent.mkdir(parents=True, exist_ok=True)
    with output_path.open("w", encoding="utf-8") as f:
        yaml.safe_dump(cfg, f, default_flow_style=False, sort_keys=False)

    _log(f"[yaml-upgrade] Wrote upgraded YAML to {output_path}")


def upgrade_soil_observation(
    input_path: Union[str, Path], forcing_path: Union[str, Path],
    output_path: Union[str, Path], forcing_output: Union[str, Path],
    manifest_path: Union[str, Path], *, from_ver: Optional[str] = None,
    source_contract: Optional[str] = None, source_unit: Optional[str] = None,
    layer_top: Optional[float] = None, layer_depth: Optional[float] = None,
    legacy_depth_is_thickness: bool = False, drop_legacy: bool = False,
    dry_run: bool = False,
) -> dict:
    """Upgrade a coordinated YAML/forcing pair; see the file converter API."""
    from .soil_forcing import convert_soil_observation_files

    return convert_soil_observation_files(
        input_path, forcing_path, output_path, forcing_output, manifest_path,
        from_ver=from_ver, source_contract=source_contract, source_unit=source_unit,
        layer_top=layer_top, layer_depth=layer_depth,
        legacy_depth_is_thickness=legacy_depth_is_thickness,
        drop_legacy=drop_legacy, dry_run=dry_run,
    )


__all__ = [
    "SoilMigrationContext",
    "YamlUpgradeError",
    "upgrade_yaml",
    "upgrade_soil_observation",
]


# ---------------------------------------------------------------------------
# SchemaMigrator wiring
#
# Expose the handlers through `SchemaMigrator` so `SUEWSConfig.from_yaml`
# (with future `migrate=True`) can auto-migrate before validation.
# ---------------------------------------------------------------------------


def register_with_migrator(migrator: SchemaMigrator) -> None:
    """Register this module's handlers on a `SchemaMigrator` instance."""
    for key, handler in _HANDLERS.items():
        migrator.migration_handlers.setdefault(key, handler)
