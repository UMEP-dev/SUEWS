"""Registry of legacy -> new DataFrame column names (gh#1325, Tier C).

Tier A (#1308) renamed the Pydantic/YAML field names to snake_case but
deliberately kept DataFrame state columns under the legacy fused
spellings so the Fortran bridge and its JSON registries would keep
working. Tier B (#1331) aligned the Rust-internal codec field names.

Tier C (gh#1325) renames the DataFrame column names in a phased rollout.
This module is the Phase 1 + Phase 2 deliverable:

* ``ALL_DF_COLUMN_RENAMES`` -- the closed set of legacy -> new DataFrame
  column names. Mirrors ``field_renames.ALL_FIELD_RENAMES`` because the
  design intent is that the DataFrame column name matches the Pydantic
  attribute name on each class.
* ``read_df_column`` -- the dual-read helper that every subsequent
  consumer migration will route through. Reads a column by its new name;
  falls back to the legacy name with a ``DeprecationWarning`` when the
  DataFrame still carries the old spelling.

PR 1 (issue #1325, phases 1 and 2) is additive only: no ``to_df_state`` /
``from_df_state`` changes, no consumer migration, no schema bump. The
registry and helper exist so PR 2 (dual-write in ``to_df_state``), PR 3
(consumer sweep), and PR 4 (drop-legacy + schema bump) have a single
seam to route through.
"""

from __future__ import annotations

import warnings
from typing import Any, Dict

import pandas as pd

from .field_renames import (
    ARCHETYPEPROPERTIES_DEV7_TO_PASCAL,
    DECTRPROPERTIES_RENAMES,
    EVETRPROPERTIES_RENAMES,
    LAIPARAMS_RENAMES,
    MODELPHYSICS_RENAMES,
    SNOWPARAMS_RENAMES,
    STEBBSPROPERTIES_DEV9_TO_PASCAL,
    SURFACEPROPERTIES_RENAMES,
    VEGETATEDSURFACEPROPERTIES_RENAMES,
)


# -- Per-domain DataFrame column rename maps ---------------------------------
#
# Each dict is keyed by the legacy DataFrame column name (as currently
# emitted by the class's ``to_df_state``) and maps to the target new name.
# For every class listed below, the legacy DataFrame column name coincides
# with the legacy Pydantic attribute name handled by ``field_renames.py``,
# so these dicts are re-exports of the corresponding Pydantic rename dicts.
# They are re-exported under DataFrame-specific names so the Tier C
# inventory is auditable on its own and so the cascade can diverge from
# the Pydantic-side set without breaking contract callers depend on.

MODELPHYSICS_DF_RENAMES: Dict[str, str] = dict(MODELPHYSICS_RENAMES)
SURFACEPROPERTIES_DF_RENAMES: Dict[str, str] = dict(SURFACEPROPERTIES_RENAMES)
LAIPARAMS_DF_RENAMES: Dict[str, str] = dict(LAIPARAMS_RENAMES)
VEGETATEDSURFACEPROPERTIES_DF_RENAMES: Dict[str, str] = dict(
    VEGETATEDSURFACEPROPERTIES_RENAMES
)
EVETRPROPERTIES_DF_RENAMES: Dict[str, str] = dict(EVETRPROPERTIES_RENAMES)
DECTRPROPERTIES_DF_RENAMES: Dict[str, str] = dict(DECTRPROPERTIES_RENAMES)
SNOWPARAMS_DF_RENAMES: Dict[str, str] = dict(SNOWPARAMS_RENAMES)

# ``ArchetypeProperties``: ``to_df_state`` lowercases the Pydantic
# attribute name before writing it into the DataFrame (see ``site.py``
# ``ArchetypeProperties.to_df_state`` and ``_ARCHETYPE_LEGACY_COL_NAMES``).
# We mirror that lowercasing here so the registry keys match the actual
# column names the DataFrame carries today and will carry after Tier C.
#
# The DataFrame column the Fortran bridge writes is the lowercased
# *PascalCase* legacy spelling (e.g. ``maxheatingpower``), but the current
# Pydantic attribute is the dev12 Column-D final (e.g.
# ``max_power_heating_system_air``). Deriving this map from the base
# ``ARCHETYPEPROPERTIES_RENAMES`` (whose values are dev7-level names) would
# leave ``read_df_column`` unable to resolve a current name back to the
# legacy column. Instead we key off ``ARCHETYPEPROPERTIES_DEV7_TO_PASCAL``,
# which maps each current dev12 final name to its PascalCase ancestry, so
# the legacy column resolves from today's field name (gh#1392 Column D /
# Codex P2 fix).
#
# Self-mappings (single-word identifiers where the legacy PascalCase and
# the new snake_case collapse to the same lowercased string — e.g.
# gh#1334's ``Occupants`` -> ``occupants``) are filtered out: the
# DataFrame column name is already identical on both sides of the rename
# so there is nothing to migrate.
ARCHETYPEPROPERTIES_DF_RENAMES: Dict[str, str] = {
    pascal.lower(): final.lower()
    for final, pascal in ARCHETYPEPROPERTIES_DEV7_TO_PASCAL.items()
    if pascal.lower() != final.lower()
}

# ``StebbsProperties`` uses the same lowercased-legacy pattern as
# ``ArchetypeProperties`` — ``to_df_state`` lowercases the PascalCase
# legacy attribute name before writing the DataFrame column (see
# ``_STEBBS_LEGACY_COL_NAMES`` in ``site.py``). The current Pydantic
# attribute is the dev12 Column-D final, so (as for Archetype above) the
# DataFrame map is keyed off ``STEBBSPROPERTIES_DEV9_TO_PASCAL`` — which
# chains each current dev12 final back to its PascalCase ancestry — rather
# than the base ``STEBBSPROPERTIES_RENAMES`` (whose values are dev8-level
# names). This lets ``read_df_column`` resolve a current name (e.g.
# ``max_power_cooling_system_air``) back to its legacy column
# (``maxcoolingpower``) (gh#1392 Column D / Codex P2 fix). Same
# self-mapping filter catches identifiers that collapse to the same string
# on both sides.
STEBBSPROPERTIES_DF_RENAMES: Dict[str, str] = {
    pascal.lower(): final.lower()
    for final, pascal in STEBBSPROPERTIES_DEV9_TO_PASCAL.items()
    if pascal.lower() != final.lower()
}


# -- Combined -----------------------------------------------------------------

ALL_DF_COLUMN_RENAMES: Dict[str, str] = {
    **MODELPHYSICS_DF_RENAMES,
    **SURFACEPROPERTIES_DF_RENAMES,
    **LAIPARAMS_DF_RENAMES,
    **VEGETATEDSURFACEPROPERTIES_DF_RENAMES,
    **EVETRPROPERTIES_DF_RENAMES,
    **DECTRPROPERTIES_DF_RENAMES,
    **ARCHETYPEPROPERTIES_DF_RENAMES,
    **STEBBSPROPERTIES_DF_RENAMES,
    **SNOWPARAMS_DF_RENAMES,
}

# Reverse map: new -> legacy. Required by ``read_df_column`` when the
# caller passes the new name but the DataFrame still carries the legacy
# spelling.
_REVERSE_DF_COLUMN_RENAMES: Dict[str, str] = {
    new: old for old, new in ALL_DF_COLUMN_RENAMES.items()
}


# -- Registry integrity (import-time assertions) -----------------------------
#
# Mirrors the invariants in ``field_renames.py`` so drift between the two
# registries fails at import time rather than at simulation time.

def _assert_registry_invariants() -> None:
    per_class_total = (
        len(MODELPHYSICS_DF_RENAMES)
        + len(SURFACEPROPERTIES_DF_RENAMES)
        + len(LAIPARAMS_DF_RENAMES)
        + len(VEGETATEDSURFACEPROPERTIES_DF_RENAMES)
        + len(EVETRPROPERTIES_DF_RENAMES)
        + len(DECTRPROPERTIES_DF_RENAMES)
        + len(ARCHETYPEPROPERTIES_DF_RENAMES)
        + len(STEBBSPROPERTIES_DF_RENAMES)
        + len(SNOWPARAMS_DF_RENAMES)
    )
    if len(ALL_DF_COLUMN_RENAMES) != per_class_total:
        raise RuntimeError(
            "ALL_DF_COLUMN_RENAMES size mismatch: "
            f"expected {per_class_total}, got {len(ALL_DF_COLUMN_RENAMES)}. "
            "A legacy DataFrame column appears in more than one per-class dict."
        )
    new_names = list(ALL_DF_COLUMN_RENAMES.values())
    if len(set(new_names)) != len(new_names):
        raise RuntimeError(
            "Duplicate new DataFrame column names detected in the registry."
        )
    legacy_names = set(ALL_DF_COLUMN_RENAMES.keys())
    shared = legacy_names & set(new_names)
    if shared:
        raise RuntimeError(
            "Legacy and new DataFrame column name sets overlap: "
            f"{sorted(shared)}"
        )


_assert_registry_invariants()


# -- Dual-read helper --------------------------------------------------------

_MISSING = object()


def _has_top_level_column(df: pd.DataFrame, name: str) -> bool:
    """Return True if ``name`` sits at level 0 of ``df.columns``.

    SUEWS DataFrame state uses a ``MultiIndex`` with the field name at
    level 0 (``("netradiationmethod", "0")`` etc.). Plain
    ``name in df.columns`` tests the full tuple on a ``MultiIndex`` and
    never matches a single-string ``name``, so the top level is walked
    explicitly.
    """
    if isinstance(df.columns, pd.MultiIndex):
        return name in df.columns.get_level_values(0)
    return name in df.columns


def read_df_column(
    df: pd.DataFrame,
    new_name: str,
    *,
    default: Any = _MISSING,
) -> Any:
    """Read a DataFrame column by its new snake_case name.

    Falls back to the legacy fused spelling with a ``DeprecationWarning``
    when the DataFrame still carries the old column. This is the single
    seam consumer code migrated in gh#1325 Phase 4 routes through, so the
    consumer can be written against the new spelling today while still
    reading DataFrames emitted by unmigrated code paths.

    Parameters
    ----------
    df : pandas.DataFrame
        State DataFrame whose columns are addressed by a top-level name
        (either a plain ``Index`` or a ``MultiIndex`` with the field name
        at level 0).
    new_name : str
        Target snake_case column name to read.
    default : Any, optional
        Returned when neither ``new_name`` nor its legacy alias is
        present. If omitted, a missing column raises ``KeyError``.

    Returns
    -------
    pandas.Series or pandas.DataFrame
        ``df[new_name]`` when present, otherwise ``df[legacy_name]`` with
        a deprecation warning, otherwise ``default``.

    Raises
    ------
    KeyError
        If neither spelling is present and no ``default`` was provided.
    """
    if _has_top_level_column(df, new_name):
        return df[new_name]

    legacy_name = _REVERSE_DF_COLUMN_RENAMES.get(new_name)
    if legacy_name is not None and _has_top_level_column(df, legacy_name):
        warnings.warn(
            f"DataFrame column '{legacy_name}' is deprecated; "
            f"use '{new_name}' instead (gh#1325 Tier C).",
            DeprecationWarning,
            stacklevel=2,
        )
        return df[legacy_name]

    if default is _MISSING:
        raise KeyError(
            f"DataFrame column '{new_name}' (legacy alias '{legacy_name}') "
            "not found."
        )
    return default
