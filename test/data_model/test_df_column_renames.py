"""Tests for the DataFrame column rename registry and helper (gh#1325 Tier C).

PR 1 lands the registry (``ALL_DF_COLUMN_RENAMES``) and the dual-read
helper (``read_df_column``). Neither changes what columns
``to_df_state`` currently emits -- those changes arrive in Phase 3
(gh#1325 PR 2) -- so these tests exercise the scaffolding in isolation:

* Registry integrity (no duplicates, disjoint legacy/new sets,
  snake_case for the non-STEBBS surface, size matches the per-class
  dicts).
* Helper behaviour (reads new name silently, falls back to legacy with
  a ``DeprecationWarning``, raises or returns ``default`` when neither
  is present).
* Cross-layer parity: every legacy DF column appears in the Rust
  ``FIELD_RENAMES`` table at ``src/suews_bridge/src/field_renames.rs``
  so drift between the Python and Rust name registries fails here,
  ahead of ``_validate_output_layout`` at simulation time.
"""

from __future__ import annotations

import re
import warnings
from pathlib import Path

import pandas as pd
import pytest

pytestmark = pytest.mark.api

from supy.data_model.core.df_column_renames import (
    ALL_DF_COLUMN_RENAMES,
    ARCHETYPEPROPERTIES_DF_RENAMES,
    DECTRPROPERTIES_DF_RENAMES,
    EVETRPROPERTIES_DF_RENAMES,
    LAIPARAMS_DF_RENAMES,
    MODELPHYSICS_DF_RENAMES,
    SNOWPARAMS_DF_RENAMES,
    STEBBSPROPERTIES_DF_RENAMES,
    SURFACEPROPERTIES_DF_RENAMES,
    VEGETATEDSURFACEPROPERTIES_DF_RENAMES,
    read_df_column,
)
from supy.data_model.core.field_renames import ALL_FIELD_RENAMES


_SNAKE_CASE_RE = re.compile(r"^[a-z][a-z0-9_]*$")
_RUST_FIELD_RENAMES_PATH = (
    Path(__file__).resolve().parents[2]
    / "src"
    / "suews_bridge"
    / "src"
    / "field_renames.rs"
)


def _rust_field_renames_text() -> str:
    """Return the full text of ``field_renames.rs`` for static alignment checks."""
    return _RUST_FIELD_RENAMES_PATH.read_text(encoding="utf-8")


class TestRegistryIntegrity:
    def test_all_renames_combines_per_class_dicts(self):
        expected = (
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
        assert len(ALL_DF_COLUMN_RENAMES) == expected

    def test_no_duplicate_new_names(self):
        values = list(ALL_DF_COLUMN_RENAMES.values())
        assert len(set(values)) == len(values), "Duplicate new DataFrame column names"

    def test_no_duplicate_legacy_names(self):
        keys = list(ALL_DF_COLUMN_RENAMES.keys())
        assert len(set(keys)) == len(keys), "Duplicate legacy DataFrame column names"

    def test_legacy_and_new_sets_disjoint(self):
        """No column can simultaneously be a legacy and a new name."""
        legacy = set(ALL_DF_COLUMN_RENAMES.keys())
        new = set(ALL_DF_COLUMN_RENAMES.values())
        assert legacy.isdisjoint(new), (
            f"Legacy and new DataFrame column name sets overlap: "
            f"{sorted(legacy & new)}"
        )

    def test_snake_case_targets(self):
        """All new DataFrame column names are lowercase snake_case.

        STEBBS ``ArchetypeProperties`` columns are lowercased during
        ``to_df_state`` emission, so the DF registry stores them in
        lowercase -- they satisfy snake_case checks even though the
        Pydantic attribute is PascalCase.
        """
        for legacy, new in ALL_DF_COLUMN_RENAMES.items():
            assert _SNAKE_CASE_RE.match(new), (
                f"New DataFrame column name must be lowercase snake_case: "
                f"{legacy!r} -> {new!r}"
            )


class TestArchetypePropertiesLowercasing:
    """STEBBS DF columns are lowercased during emission; the registry
    mirrors that casing so the helper works against real DataFrames."""

    def test_wall_external_thickness_lowercased(self):
        # gh#1334 -> gh#1390 target is the dev7 reorder
        # (thickness_wall_outer); lowercasing the Pydantic attribute
        # preserves the underscores.
        assert ARCHETYPEPROPERTIES_DF_RENAMES["wallextthickness"] == (
            "thickness_wall_outer"
        )

    def test_all_keys_and_values_lowercase(self):
        for legacy, new in ARCHETYPEPROPERTIES_DF_RENAMES.items():
            assert legacy == legacy.lower()
            assert new == new.lower()


class TestHelperNewName:
    def test_new_name_single_index_returns_series_silently(self):
        df = pd.DataFrame({"net_radiation": [3]})
        with warnings.catch_warnings():
            warnings.simplefilter("error", DeprecationWarning)
            series = read_df_column(df, "net_radiation")
        assert list(series) == [3]

    def test_new_name_multiindex_returns_sub_df_silently(self):
        df = pd.DataFrame({("net_radiation", "0"): [3]})
        df.columns = pd.MultiIndex.from_tuples(df.columns)
        with warnings.catch_warnings():
            warnings.simplefilter("error", DeprecationWarning)
            sub = read_df_column(df, "net_radiation")
        assert sub.iloc[0, 0] == 3


class TestHelperLegacyFallback:
    def test_legacy_name_emits_deprecation_warning(self):
        df = pd.DataFrame({("netradiationmethod", "0"): [3]})
        df.columns = pd.MultiIndex.from_tuples(df.columns)
        with warnings.catch_warnings(record=True) as captured:
            warnings.simplefilter("always", DeprecationWarning)
            sub = read_df_column(df, "net_radiation")
        messages = [
            str(w.message)
            for w in captured
            if issubclass(w.category, DeprecationWarning)
        ]
        assert any(
            "netradiationmethod" in m and "net_radiation" in m for m in messages
        ), f"Expected deprecation warning, got: {messages}"
        assert sub.iloc[0, 0] == 3

    def test_single_index_legacy_fallback(self):
        df = pd.DataFrame({"soildepth": [0.2]})
        with warnings.catch_warnings(record=True) as captured:
            warnings.simplefilter("always", DeprecationWarning)
            series = read_df_column(df, "soil_depth")
        messages = [
            str(w.message)
            for w in captured
            if issubclass(w.category, DeprecationWarning)
        ]
        assert any("soildepth" in m and "soil_depth" in m for m in messages)
        assert list(series) == [0.2]


class TestHelperMissing:
    def test_missing_with_default_returns_default(self):
        df = pd.DataFrame({"unrelated": [1]})
        with warnings.catch_warnings():
            warnings.simplefilter("error", DeprecationWarning)
            result = read_df_column(df, "net_radiation", default=-999)
        assert result == -999

    def test_missing_without_default_raises(self):
        df = pd.DataFrame({"unrelated": [1]})
        with pytest.raises(KeyError, match="net_radiation"):
            read_df_column(df, "net_radiation")


class TestRustBridgeAlignment:
    """Every legacy DataFrame column must appear in the Rust
    ``FIELD_RENAMES`` table so the two name registries stay aligned.

    STEBBS is excluded from the lowercase comparison because the Rust
    table stores the PascalCase Pydantic spelling (``WallextThickness``)
    while the DataFrame registry stores the lowercased DF-column form
    (``wallextthickness``).
    """

    def test_every_non_stebbs_legacy_column_listed_in_rust(self):
        rust_text = _rust_field_renames_text()
        # Both ArchetypeProperties and StebbsProperties DF columns use the
        # lowercased-PascalCase pattern; the Rust table keeps the
        # PascalCase Pydantic spelling, so both are covered by the
        # STEBBS-specific check below.
        stebbs_legacy = set(ARCHETYPEPROPERTIES_DF_RENAMES.keys()) | set(
            STEBBSPROPERTIES_DF_RENAMES.keys()
        )
        missing = []
        for legacy in ALL_DF_COLUMN_RENAMES.keys():
            if legacy in stebbs_legacy:
                continue
            # Rust FIELD_RENAMES entries are `(new_name, legacy_name)`; look
            # for the legacy name surrounded by quotes so substring collisions
            # (e.g. `baset` inside `basete`) don't give a false positive.
            if f'"{legacy}"' not in rust_text:
                missing.append(legacy)
        assert not missing, (
            f"Legacy DataFrame columns absent from Rust field_renames.rs: "
            f"{missing}"
        )

    def test_stebbs_legacy_columns_present_as_pascal_case_in_rust(self):
        rust_text = _rust_field_renames_text()
        missing = []
        for legacy_lower in ARCHETYPEPROPERTIES_DF_RENAMES.keys():
            # The Rust table keeps PascalCase; reconstruct the expected
            # spelling from the Pydantic side of the rename.
            from supy.data_model.core.field_renames import (
                ARCHETYPEPROPERTIES_RENAMES,
            )

            pascal_match = next(
                (
                    old
                    for old in ARCHETYPEPROPERTIES_RENAMES
                    if old.lower() == legacy_lower
                ),
                None,
            )
            assert pascal_match is not None, (
                f"STEBBS legacy column {legacy_lower!r} has no PascalCase "
                f"source in ARCHETYPEPROPERTIES_RENAMES"
            )
            if f'"{pascal_match}"' not in rust_text:
                missing.append(pascal_match)
        assert not missing, (
            f"STEBBS legacy columns absent from Rust field_renames.rs: "
            f"{missing}"
        )


class TestPydanticRegistryMirror:
    """The DF column rename registry is sourced from the Pydantic
    ``ALL_FIELD_RENAMES`` registry (plus STEBBS lowercasing). If Tier A
    adds a new Pydantic rename that should also rename the DataFrame
    column, this test forces an explicit decision rather than silent drift.
    """

    def test_non_stebbs_df_renames_match_field_renames(self):
        from supy.data_model.core.field_renames import (
            ARCHETYPEPROPERTIES_RENAMES,
            STEBBSPROPERTIES_RENAMES,
        )

        stebbs_legacy = set(ARCHETYPEPROPERTIES_RENAMES.keys()) | set(
            STEBBSPROPERTIES_RENAMES.keys()
        )
        # gh#1326 Tier D Fortran-internal renames live in
        # FORTRAN_INTERNAL_RENAMES, not ALL_FIELD_RENAMES, so no exclusion
        # block is needed here — iterating ALL_FIELD_RENAMES already skips
        # them.
        for legacy, new in ALL_FIELD_RENAMES.items():
            if legacy in stebbs_legacy:
                # STEBBS (Archetype + StebbsProperties) lowercases its
                # DF columns and is covered by a dedicated test.
                continue
            assert ALL_DF_COLUMN_RENAMES.get(legacy) == new, (
                f"Pydantic rename {legacy!r} -> {new!r} missing or divergent "
                f"in ALL_DF_COLUMN_RENAMES"
            )
