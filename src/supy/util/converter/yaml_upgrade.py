"""YAML-to-YAML schema upgrade tool for supy configurations.

Provides a supported upgrade path between formal supy releases. When the
validator on master tightens a structural rule (internal-only fields, union
dispatch, renamed sections), add a handler here keyed by
`(from_schema_version, to_schema_version)` so existing user YAMLs keep working.

See gh#1301, gh#1304.
"""

from __future__ import annotations

from collections.abc import Iterable
from pathlib import Path
import sys
from typing import Callable

import yaml

from ...data_model.core.field_renames import (
    ALL_FIELD_RENAMES,
    ARCHETYPEPROPERTIES_RENAMES,
    ARCHETYPEPROPERTIES_PASCAL_RENAMES,
    DECTRPROPERTIES_RENAMES,
    EVETRPROPERTIES_RENAMES,
    LAIPARAMS_RENAMES,
    MODELPHYSICS_SUFFIX_RENAMES,
    SNOWPARAMS_RENAMES,
    SNOWPARAMS_INTERMEDIATE_RENAMES,
    ARCHETYPEPROPERTIES_DEV3_RENAMES,
    ARCHETYPEPROPERTIES_DEV6_RENAMES,
    STEBBSPROPERTIES_DEV3_RENAMES,
    STEBBSPROPERTIES_RENAMES,
    SURFACEPROPERTIES_RENAMES,
    VEGETATEDSURFACEPROPERTIES_RENAMES,
    rename_keys_recursive,
)
from ...data_model.schema import CURRENT_SCHEMA_VERSION
from ...data_model.schema.migration import SchemaMigrator

# ---------------------------------------------------------------------------
# Package-version -> schema-version resolver
#
# The user typically knows the supy release tag (e.g. "2026.4.3") not the
# internal schema version. This table maps release tags to the schema version
# they shipped with. Extend when a release bumps `CURRENT_SCHEMA_VERSION`.
# ---------------------------------------------------------------------------

# Retrospectively-assigned schema versions per formal release tag. The
# `SCHEMA_VERSIONS` lineage in `src/supy/data_model/schema/version.py`
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
_PACKAGE_TO_SCHEMA: dict[str, str] = {
    "2025.10.15": "2025.12",
    "2025.11.20": "2025.12",
    "2026.1.28": "2026.1",
    # 2026.4.3 pinned to the literal "2026.4" schema it shipped under; when
    # CURRENT_SCHEMA_VERSION bumps, the previous release tag keeps its
    # original schema label and upgrades to the new one via the registered
    # handler below, not by silent remapping.
    "2026.4.3": "2026.4",
    # Dev-cycle vendored fixtures: filename stem maps to its own schema label
    # so test_release_compat.py exercises the (label -> current) handler.
    "2026.5.dev6": "2026.5.dev6",
    "2026.5.dev7": "2026.5.dev7",
    "2026.5.dev8": "2026.5.dev8",
    "2026.5.dev9": "2026.5.dev9",
}


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

# Schema 2026.5.dev8 -> 2026.5.dev9: apply Rule 2 of the SUEWS naming
# convention to ArchetypeProperties bulk-material and surface optical fields
# (44 renames). Pure key reorder; no data transformation. Sourced from the
# canonical dict in field_renames.py. The source dict name is retained from
# the pre-merge master branch where this was originally the dev6 -> dev7 bump.
_ARCH_RENAMES_DEV6_TO_DEV7: tuple[tuple[str, str], ...] = tuple(
    ARCHETYPEPROPERTIES_DEV6_RENAMES.items()
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
    return _strip_internal_only_fields(cfg)


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
    for old, new in _MODELPHYSICS_SUFFIX_RENAMES_TABLE:
        _rename_field(physics, old, new)


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


def _migrate_2026_5_dev8_to_current(cfg: dict) -> dict:
    """Upgrade 2026.5.dev8-shaped YAMLs to the current schema.

    Applies the naming convention Rule 2 reorder for ArchetypeProperties
    bulk-material and surface optical fields (44 renames).
    """
    cfg = _strip_internal_only_fields(cfg)
    _apply_arch_rule2_renames(cfg)
    return cfg


def _migrate_2026_5_dev7_to_current(cfg: dict) -> dict:
    """Upgrade 2026.5.dev7-shaped YAMLs to the current schema.

    Applies the gh#1372 follow-up output-config restructure: output_file
    is moved under a new output sub-object and the inner ``path`` is
    renamed to ``dir``. Then applies the naming convention Rule 2 reorder.
    """
    cfg = _strip_internal_only_fields(cfg)
    _apply_output_subobject_restructure(cfg)
    _apply_arch_rule2_renames(cfg)
    return cfg


def _migrate_2026_5_dev6_to_current(cfg: dict) -> dict:
    """Upgrade 2026.5.dev6-shaped YAMLs to the current schema.

    Applies the gh#1372 forcing-config restructure (forcing_file moved
    under a new forcing sub-object) and the gh#1372 follow-up output
    restructure (output_file moved under a new output sub-object,
    path -> dir), then applies the naming convention Rule 2 reorder.
    """
    cfg = _strip_internal_only_fields(cfg)
    _apply_forcing_subobject_restructure(cfg)
    _apply_output_subobject_restructure(cfg)
    _apply_arch_rule2_renames(cfg)
    return cfg


def _migrate_2026_5_dev5_to_current(cfg: dict) -> dict:
    """Upgrade 2026.5.dev5 YAMLs to current."""
    cfg = _strip_internal_only_fields(cfg)
    _apply_forcing_subobject_restructure(cfg)
    _apply_output_subobject_restructure(cfg)
    _apply_arch_rule2_renames(cfg)
    return cfg


def _migrate_2026_5_dev4_to_current(cfg: dict) -> dict:
    """Upgrade 2026.5.dev4 YAMLs to current."""
    cfg = _strip_internal_only_fields(cfg)
    _apply_forcing_subobject_restructure(cfg)
    _apply_output_subobject_restructure(cfg)
    _apply_arch_rule2_renames(cfg)
    return cfg


def _migrate_2026_5_dev3_to_current(cfg: dict) -> dict:
    """Upgrade 2026.5.dev3-shaped YAMLs to the current schema.

    Applies the gh#1334 follow-through hot-water unification: 14 renames
    under ``site.properties.building_archetype`` and ``.stebbs``. Also
    applies the gh#1372 forcing-config restructure and the gh#1372
    follow-up output restructure, then the naming convention Rule 2 reorder.
    """
    cfg = _strip_internal_only_fields(cfg)
    _apply_hot_water_unification_renames(cfg)
    _apply_forcing_subobject_restructure(cfg)
    _apply_output_subobject_restructure(cfg)
    _apply_arch_rule2_renames(cfg)
    return cfg


def _migrate_2026_5_dev2_to_current(cfg: dict) -> dict:
    """Upgrade 2026.5.dev2-shaped YAMLs to the current schema.

    Chains gh#1334 (dev2 -> dev3: retires STEBBS PascalCase; 124 renames)
    and the gh#1334 follow-through (dev3 -> dev4: hot-water prefix
    unification; 14 renames). Also applies the gh#1372 forcing-config
    restructure and the gh#1372 follow-up output restructure on the way
    through, then the naming convention Rule 2 reorder.
    """
    cfg = _strip_internal_only_fields(cfg)
    _apply_stebbs_snake_renames(cfg)
    _apply_hot_water_unification_renames(cfg)
    _apply_forcing_subobject_restructure(cfg)
    _apply_output_subobject_restructure(cfg)
    _apply_arch_rule2_renames(cfg)
    return cfg


def _migrate_2026_5_to_current(cfg: dict) -> dict:
    """Upgrade 2026.5-shaped YAMLs to the current schema.

    Chains three dev-label deltas:

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
    * 2026.5.dev6 -> 2026.5.dev7 (gh#1372): forcing_file -> forcing.file.
    * 2026.5.dev7 -> 2026.5.dev8 (gh#1372 follow-up): output_file -> output.
    * 2026.5.dev8 -> 2026.5.dev9 (naming convention Rule 2):
      ArchetypeProperties bulk-material and surface optical fields
      reordered to ``<quantity>_<component>_<sub_class>`` (44 renames).

    Each rename flows through ``_rename_field`` so a dedicated log line
    is emitted per field - ``TestNoSilentFieldDrops`` enforces that. The
    Pydantic backward-compat shim still accepts legacy names at load
    time, but YAMLs that round-trip through the migrator come out in the
    new spellings and no longer emit deprecation warnings.

    Under the dev-label convention
    (``.claude/rules/python/schema-versioning.md``) the release PR will
    collapse all nine steps into a single ``(<prev>, 2026.5)`` migration.
    """
    cfg = _strip_internal_only_fields(cfg)
    _apply_arch_ext_renames(cfg)
    _apply_modelphysics_suffix_renames(cfg)
    _apply_stebbs_snake_renames(cfg)
    _apply_hot_water_unification_renames(cfg)
    _apply_forcing_subobject_restructure(cfg)
    _apply_output_subobject_restructure(cfg)
    _apply_arch_rule2_renames(cfg)
    return cfg


def _migrate_2026_5_dev1_to_current(cfg: dict) -> dict:
    """Upgrade 2026.5.dev1-shaped YAMLs to the current schema.

    Chains the Cat 2+3 ModelPhysics suffix/abbreviation rewrite (gh#1321),
    the gh#1334 STEBBS/Snow snake_case sweep, the gh#1334 follow-through
    hot-water prefix unification, the gh#1372 forcing-config restructure
    and the gh#1372 follow-up output restructure, then the naming convention
    Rule 2 reorder.
    """
    cfg = _strip_internal_only_fields(cfg)
    _apply_modelphysics_suffix_renames(cfg)
    _apply_stebbs_snake_renames(cfg)
    _apply_hot_water_unification_renames(cfg)
    _apply_forcing_subobject_restructure(cfg)
    _apply_output_subobject_restructure(cfg)
    _apply_arch_rule2_renames(cfg)
    return cfg


def _migrate_2026_4_to_current(cfg: dict) -> dict:
    """Chain 2026.4 -> 2026.5 Cat 1 -> dev1 ext -> dev2 suffix -> dev3 STEBBS snake."""
    cfg = _migrate_2026_4_to_2026_5(cfg)
    return _migrate_2026_5_to_current(cfg)


def _migrate_2026_1_to_2026_4(cfg: dict) -> dict:
    """Pure #1240/#1242/#1261 delta, without the Category 1 renames."""
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


def _migrate_2025_12_to_2026_4(cfg: dict) -> dict:
    """Chain #879 clean-up -> 2026.1 delta, stopping at 2026.4."""
    cfg = _migrate_2025_12_to_2026_1(cfg)
    return _migrate_2026_1_to_2026_4(cfg)


def _migrate_2025_12_to_current(cfg: dict) -> dict:
    """Chain #879 clean-up -> 2026.1 delta -> 2026.5 Cat. 1 -> dev1 ext -> dev2 suffix."""
    cfg = _migrate_2025_12_to_2026_4(cfg)
    return _migrate_2026_4_to_current(cfg)


def _migrate_2026_1_to_current(cfg: dict) -> dict:
    """Chain 2026.1 delta -> 2026.5 Cat. 1 -> dev1 STEBBS ext -> dev2 suffix."""
    cfg = _migrate_2026_1_to_2026_4(cfg)
    return _migrate_2026_4_to_current(cfg)


_HANDLERS: dict[tuple[str, str], Handler] = {
    # Identity at the current schema is explicit so the dispatch completes
    # even when no real upgrade is needed.
    (CURRENT_SCHEMA_VERSION, CURRENT_SCHEMA_VERSION): _identity,
    # Intermediate stops at 2026.4 (used by callers pinning an older
    # target, e.g. migrate(..., to_version="2026.4")).
    ("2026.1", "2026.4"): _migrate_2026_1_to_2026_4,
    ("2025.12", "2026.4"): _migrate_2025_12_to_2026_4,
    # Intermediate stops at 2026.5 (callers pinning Category 1 only).
    ("2026.4", "2026.5"): _migrate_2026_4_to_2026_5,
    # Chains to the current schema (2026.5.dev9: Cat 1 snake_case sweep
    # + Cat 5 STEBBS ext rename + Cat 2+3 ModelPhysics suffix drop
    # + gh#1334 STEBBS/Snow snake_case + gh#1334 follow-through hot-water
    # prefix unification + gh#972 accept-only nested physics sub-options
    # + gh#1333 site-level completeness validator tightening
    # + gh#1372 ForcingControl sub-object restructure
    # + gh#1372 follow-up output_file -> output sub-object restructure
    #   (path -> dir, drop legacy string form)
    # + naming convention Rule 2 ArchetypeProperties reorder.
    # The dev4 -> dev5 and dev5 -> dev6 deltas are accept-only / contract
    # tightening changes with no YAML rewrite, but the dev6 -> dev7 delta
    # rewrites forcing_file -> forcing.file and the dev7 -> dev8 delta
    # rewrites output_file -> output. The dev8 -> dev9 delta is a pure key
    # rename, so dev4/dev5 identity shortcuts have been replaced with proper
    # handlers that apply all later rewrites.
    ("2026.5.dev8", CURRENT_SCHEMA_VERSION): _migrate_2026_5_dev8_to_current,
    ("2026.5.dev7", CURRENT_SCHEMA_VERSION): _migrate_2026_5_dev7_to_current,
    ("2026.5.dev6", CURRENT_SCHEMA_VERSION): _migrate_2026_5_dev6_to_current,
    ("2026.5.dev5", CURRENT_SCHEMA_VERSION): _migrate_2026_5_dev5_to_current,
    ("2026.5.dev4", CURRENT_SCHEMA_VERSION): _migrate_2026_5_dev4_to_current,
    ("2026.5.dev3", CURRENT_SCHEMA_VERSION): _migrate_2026_5_dev3_to_current,
    ("2026.5.dev2", CURRENT_SCHEMA_VERSION): _migrate_2026_5_dev2_to_current,
    ("2026.5.dev1", CURRENT_SCHEMA_VERSION): _migrate_2026_5_dev1_to_current,
    ("2026.5", CURRENT_SCHEMA_VERSION): _migrate_2026_5_to_current,
    ("2026.4", CURRENT_SCHEMA_VERSION): _migrate_2026_4_to_current,
    ("2026.1", CURRENT_SCHEMA_VERSION): _migrate_2026_1_to_current,
    ("2025.12", CURRENT_SCHEMA_VERSION): _migrate_2025_12_to_current,
}


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
        cfg = handler(cfg)

    cfg["schema_version"] = target_schema

    output_path.parent.mkdir(parents=True, exist_ok=True)
    with output_path.open("w", encoding="utf-8") as f:
        yaml.safe_dump(cfg, f, default_flow_style=False, sort_keys=False)

    _log(f"[yaml-upgrade] Wrote upgraded YAML to {output_path}")


__all__ = [
    "YamlUpgradeError",
    "upgrade_yaml",
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
