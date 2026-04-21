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

from ...data_model.core.field_renames import ALL_FIELD_RENAMES
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


def _apply_field_renames_recursive(data, renames: dict) -> object:
    """Recursively rename dict keys using `renames` ({old: new}).

    Walks the YAML tree and replaces any key matching `renames` with its
    mapped value. Used by the 2026.4 -> 2026.5 handler to apply the
    Category 1 fused-identifier rename sweep (#1256) across every
    nested section without having to enumerate the YAML nesting.
    """
    if isinstance(data, dict):
        return {
            renames.get(k, k): _apply_field_renames_recursive(v, renames)
            for k, v in data.items()
        }
    if isinstance(data, list):
        return [_apply_field_renames_recursive(item, renames) for item in data]
    return data


def _migrate_2026_4_to_current(cfg: dict) -> dict:
    """Upgrade 2026.4-shaped YAMLs to the current `2026.5` schema.

    Applies the Category 1 fused-identifier rename sweep from #1256: 59
    compound field names (e.g. `netradiationmethod` ->
    `net_radiation_method`, `soildepth` -> `soil_depth`, `baset` ->
    `base_temperature`, `crwmax` -> `water_holding_capacity_max`) are
    rewritten to snake_case. Full mapping in
    ``src/supy/data_model/core/field_renames.py``. The Pydantic
    backward-compat shim still accepts the legacy names at load time,
    but YAMLs that round-trip through the migrator come out in the new
    spelling and no longer emit deprecation warnings.
    """
    cfg = _strip_internal_only_fields(cfg)
    return _apply_field_renames_recursive(cfg, ALL_FIELD_RENAMES)


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
    """Chain #879 clean-up -> 2026.1 delta -> Category 1 renames."""
    cfg = _migrate_2025_12_to_2026_4(cfg)
    return _migrate_2026_4_to_current(cfg)


def _migrate_2026_1_to_current(cfg: dict) -> dict:
    """Chain 2026.1 delta -> Category 1 renames."""
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
    # Chains to the current schema.
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
