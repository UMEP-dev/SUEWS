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

from ...data_model.schema import CURRENT_SCHEMA_VERSION
from ...data_model.schema.migration import SchemaMigrator

# ---------------------------------------------------------------------------
# Package-version -> schema-version resolver
#
# The user typically knows the supy release tag (e.g. "2026.4.3") not the
# internal schema version. This table maps release tags to the schema version
# they shipped with. Extend when a release bumps `CURRENT_SCHEMA_VERSION`.
# ---------------------------------------------------------------------------

# The `2026.1.28` release shipped the pre-#1261 setpoint shape under the
# same `2025.12` schema umbrella, so we retrofit a dedicated `2026.1` schema
# label for the dispatch to distinguish it from the post-#1261 shape. Older
# formal releases (`2025.10.15`, `2025.11.20`) and the post-split
# `2026.4.3` sample configs still parse directly under the current
# validator - no package-to-schema remap needed for them.
_SCHEMA_PRE_SETPOINT_SPLIT = "2026.1"

_PACKAGE_TO_SCHEMA: dict[str, str] = {
    "2026.1.28": _SCHEMA_PRE_SETPOINT_SPLIT,
    "2026.4.3": "2025.12",
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
# Handler registry
# ---------------------------------------------------------------------------

Handler = Callable[[dict], dict]


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


_PROFILE_RENAME_PAIRS: tuple[tuple[str, float], ...] = (
    ("HeatingSetpointTemperature", 18.0),
    ("CoolingSetpointTemperature", 27.0),
)


def _split_profile_fields(building_archetype: dict) -> dict:
    """Split pre-#1261 profile-shaped setpoint fields into scalar + Profile.

    Before PR #1261, `HeatingSetpointTemperature` / `CoolingSetpointTemperature`
    held a `{working_day, holiday}` day-profile dict. After the split the
    scalar field carries a single temperature and the schedule moves to a new
    `*Profile` sibling. This function keeps the user's hourly values intact by
    relocating them and synthesising a scalar from the first working-day
    value (falling back to a schema-default if the profile is absent).
    """
    if not isinstance(building_archetype, dict):
        return building_archetype
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
    return building_archetype


def _migrate_pre_setpoint_split_to_2025_12(cfg: dict) -> dict:
    """Upgrade pre-#1261 YAMLs to the current `2025.12` schema.

    The STEBBS setpoint split (gh#1261) renamed two building-archetype
    day-profile fields. This handler:

    * moves each profile-shaped `HeatingSetpointTemperature` /
      `CoolingSetpointTemperature` into a new `*Profile` sibling,
    * replaces the original key with the scalar form now required by the
      validator (seeded from the first working-day entry so downstream
      `setpointmethod=0` users get a sensible constant), and
    * records `model.physics.setpointmethod = 2` (SCHEDULED) so the migrated
      run honours the supplied hourly profile by default.
    """
    cfg = _strip_internal_only_fields(cfg)
    for site in cfg.get("sites", []) or []:
        if not isinstance(site, dict):
            continue
        props = site.get("properties")
        if not isinstance(props, dict):
            continue
        arch = props.get("building_archetype")
        if isinstance(arch, dict):
            _split_profile_fields(arch)
    physics = cfg.setdefault("model", {}).setdefault("physics", {})
    physics["setpointmethod"] = {"value": 2}
    return cfg


_HANDLERS: dict[tuple[str, str], Handler] = {
    # Identity at the current schema is explicit so the dispatch completes
    # even when no real upgrade is needed. Future (from, to) handlers go
    # alongside this entry keyed by their schema versions.
    (CURRENT_SCHEMA_VERSION, CURRENT_SCHEMA_VERSION): _identity,
    (_SCHEMA_PRE_SETPOINT_SPLIT, CURRENT_SCHEMA_VERSION): (
        _migrate_pre_setpoint_split_to_2025_12
    ),
}


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------


class YamlUpgradeError(RuntimeError):
    """Raised when a YAML cannot be upgraded (no handler, missing signature, ...)."""


def _log(message: str) -> None:
    # Dereference sys.stderr at call time so pytest capsys / CliRunner can
    # intercept. Binding it in the signature would cache the original
    # stream and bypass capture fixtures.
    print(message, file=sys.stderr)


def _detect_signature(cfg: dict) -> str | None:
    """Return the raw schema signature from the YAML dict, or None if missing.

    Distinct from `SchemaMigrator.auto_detect_version`, which falls back to
    the current schema when no signature is found. Here we want to know
    whether a signature is *actually present* so the CLI can nudge the user
    to supply `--from-ver` when it isn't.
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
