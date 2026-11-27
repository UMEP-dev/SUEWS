"""Regression tests for ProfileManager CSV column alignment.

Verifies that ``ProfileManager.ensure_required_profiles`` populates finite
24-hour values for newly created default and placeholder profiles regardless
of whether ``profile_data`` uses string column headers (loaded from CSV via
``pd.read_csv``), integer column headers, or starts empty, while preserving
existing profile rows untouched.
"""

from pathlib import Path

import numpy as np
import pandas as pd
import pytest

from supy._load import load_SUEWS_table
from supy.util.converter.table.profile_manager import ProfileManager

pytestmark = [pytest.mark.api, pytest.mark.contract("C-MIGRATE")]

_FIXTURE_PROFILES_2025A = (
    Path(__file__).resolve().parents[1]
    / "fixtures"
    / "legacy_format"
    / "2025a"
    / "Input"
    / "SUEWS_Profiles.txt"
)


def _write_profiles_csv(
    path: Path,
    rows: dict[int, list[float] | list[int]],
    *,
    legacy_two_line: bool,
    hour_labels: list[str] | None = None,
) -> None:
    """Write a whitespace-delimited SUEWS_Profiles.txt file."""
    labels = hour_labels if hour_labels is not None else [str(h) for h in range(24)]
    lines: list[str] = []
    if legacy_two_line:
        lines.append(" ".join(str(i) for i in range(1, len(labels) + 2)))
    lines.append("Code " + " ".join(labels))
    for code, values in rows.items():
        formatted = " ".join(
            str(v) if isinstance(v, int) else f"{v:.3f}" for v in values
        )
        lines.append(f"{code} {formatted}")
    path.write_text("\n".join(lines) + "\n", encoding="utf-8")


@pytest.mark.parametrize(
    "legacy_two_line",
    [
        pytest.param(True, id="legacy-two-line-header"),
        pytest.param(False, id="modern-single-line-header"),
    ],
)
def test_ensure_required_profiles_populates_finite_defaults_on_csv_load(
    tmp_path: Path, legacy_two_line: bool
) -> None:
    """CSV-loaded string-column tables align new defaults/placeholders without NaN."""
    # ARRANGE
    existing_values = [round(0.05 * (h + 1), 3) for h in range(24)]
    src_path = tmp_path / "source_profiles.txt"
    _write_profiles_csv(
        src_path,
        {10: existing_values},
        legacy_two_line=legacy_two_line,
    )

    pm = ProfileManager(src_path)
    assert pm.profile_data is not None
    assert list(pm.profile_data.columns) == [str(h) for h in range(24)]
    assert pm.validate_profile_code(321, "ActivityProfWD") == (False, 999)

    out_dir = tmp_path / "out"
    out_dir.mkdir()

    # ACT
    pm.ensure_required_profiles(out_dir)

    # ASSERT: in-memory DataFrame preserves code 10 and fills 1, 321, 999 with 1.0
    assert pm.available_profiles == {1, 10, 321, 999}
    assert 0 not in pm.available_profiles
    assert pm.profile_data is not None
    np.testing.assert_allclose(
        pm.profile_data.loc[10].to_numpy(dtype=float),
        existing_values,
        rtol=0.0,
        atol=1e-6,
    )
    for code in (1, 321, 999):
        row_vals = pm.profile_data.loc[code].to_numpy(dtype=float)
        assert np.all(np.isfinite(row_vals))
        np.testing.assert_allclose(row_vals, np.ones(24), rtol=0.0, atol=1e-6)

    # ASSERT: written file reloads cleanly via both ProfileManager and load_SUEWS_table
    written = out_dir / "SUEWS_Profiles.txt"
    reloaded_pm = ProfileManager(written)
    assert reloaded_pm.available_profiles == {1, 10, 321, 999}

    df_loaded = load_SUEWS_table(written)
    assert set(df_loaded.index) == {1, 10, 321, 999}
    np.testing.assert_allclose(
        df_loaded.loc[10].to_numpy(dtype=float),
        existing_values,
        rtol=0.0,
        atol=1e-3,
    )
    for code in (1, 321, 999):
        np.testing.assert_allclose(
            df_loaded.loc[code].to_numpy(dtype=float),
            np.ones(24),
            rtol=0.0,
            atol=1e-6,
        )


def test_ensure_required_profiles_handles_one_based_string_csv_headers(
    tmp_path: Path,
) -> None:
    """CSV tables with 1..24 string headers receive aligned finite defaults."""
    # ARRANGE
    existing_values = [round(0.1 + 0.02 * h, 3) for h in range(24)]
    src_path = tmp_path / "SUEWS_Profiles.txt"
    _write_profiles_csv(
        src_path,
        {20: existing_values},
        legacy_two_line=True,
        hour_labels=[str(h) for h in range(1, 25)],
    )

    pm = ProfileManager(src_path)
    assert pm.profile_data is not None
    assert list(pm.profile_data.columns) == [str(h) for h in range(1, 25)]
    pm.validate_profile_code(404, "TraffProfWD")

    out_dir = tmp_path / "out"
    out_dir.mkdir()

    # ACT
    pm.ensure_required_profiles(out_dir)

    # ASSERT
    assert pm.available_profiles == {1, 20, 404, 999}
    np.testing.assert_allclose(
        pm.profile_data.loc[20].to_numpy(dtype=float),
        existing_values,
        rtol=0.0,
        atol=1e-6,
    )
    for code in (1, 404, 999):
        vals = pm.profile_data.loc[code].to_numpy(dtype=float)
        assert np.all(np.isfinite(vals))
        np.testing.assert_allclose(vals, np.ones(24), rtol=0.0, atol=1e-6)

    df_loaded = load_SUEWS_table(out_dir / "SUEWS_Profiles.txt")
    assert set(df_loaded.index) == {1, 20, 404, 999}
    np.testing.assert_allclose(
        df_loaded.loc[20].to_numpy(dtype=float),
        existing_values,
        rtol=0.0,
        atol=1e-3,
    )


@pytest.mark.parametrize(
    "columns",
    [
        pytest.param(range(24), id="zero-based-int-columns"),
        pytest.param(range(1, 25), id="one-based-int-columns"),
    ],
)
def test_ensure_required_profiles_handles_integer_column_dataframes(
    tmp_path: Path, columns: range
) -> None:
    """In-memory integer-column DataFrames stay aligned alongside CSV string tables."""
    # ARRANGE
    existing_values = [round(0.25 + 0.03 * h, 3) for h in range(24)]
    pm = ProfileManager()
    pm.profile_data = pd.DataFrame(
        [existing_values],
        index=pd.Index([50], name="Code"),
        columns=columns,
    )
    pm.available_profiles = {50}
    pm.validate_profile_code(777, "PopProfWD")

    # ACT
    pm.ensure_required_profiles(tmp_path)

    # ASSERT
    assert pm.available_profiles == {1, 50, 777, 999}
    np.testing.assert_allclose(
        pm.profile_data.loc[50].to_numpy(dtype=float),
        existing_values,
        rtol=0.0,
        atol=1e-6,
    )
    for code in (1, 777, 999):
        vals = pm.profile_data.loc[code].to_numpy(dtype=float)
        assert np.all(np.isfinite(vals))
        np.testing.assert_allclose(vals, np.ones(24), rtol=0.0, atol=1e-6)

    df_loaded = load_SUEWS_table(tmp_path / "SUEWS_Profiles.txt")
    assert set(df_loaded.index) == {1, 50, 777, 999}
    np.testing.assert_allclose(
        df_loaded.loc[50].to_numpy(dtype=float),
        existing_values,
        rtol=0.0,
        atol=1e-3,
    )


@pytest.mark.parametrize(
    "initial_data",
    [
        pytest.param(None, id="no-loaded-profiles"),
        pytest.param(pd.DataFrame(), id="empty-zero-column-dataframe"),
    ],
)
def test_ensure_required_profiles_initialises_columns_when_empty(
    tmp_path: Path, initial_data: pd.DataFrame | None
) -> None:
    """Empty or missing profile tables initialise 24 hour columns cleanly."""
    # ARRANGE
    pm = ProfileManager()
    pm.profile_data = initial_data.copy() if initial_data is not None else None
    pm.validate_profile_code(555, "EnergyUseProfWD")

    # ACT
    pm.ensure_required_profiles(tmp_path)

    # ASSERT
    assert pm.profile_data is not None
    assert list(pm.profile_data.columns) == [str(h) for h in range(24)]
    assert pm.available_profiles == {1, 555, 999}

    df_loaded = load_SUEWS_table(tmp_path / "SUEWS_Profiles.txt")
    assert set(df_loaded.index) == {1, 555, 999}
    for code in (1, 555, 999):
        np.testing.assert_allclose(
            df_loaded.loc[code].to_numpy(dtype=float),
            np.ones(24),
            rtol=0.0,
            atol=1e-6,
        )


def test_ensure_required_profiles_preserves_real_fixture_and_int_valued_csv(
    tmp_path: Path,
) -> None:
    """Vendored 2025a fixture and integer-valued CSVs gain finite defaults intact."""
    # ARRANGE: real vendored SUEWS_Profiles.txt with -9 footer rows
    pm_fixture = ProfileManager(_FIXTURE_PROFILES_2025A)
    assert pm_fixture.profile_data is not None
    orig_row_10 = pm_fixture.profile_data.loc[10].to_numpy(dtype=float).copy()
    pm_fixture.validate_profile_code(612, "SnowClearingProfWD")

    fixture_out = tmp_path / "fixture_out"
    fixture_out.mkdir()

    # ACT
    pm_fixture.ensure_required_profiles(fixture_out)

    # ASSERT: existing profile 10 untouched; new placeholder 612 finite and reloadable
    np.testing.assert_allclose(
        pm_fixture.profile_data.loc[10].to_numpy(dtype=float),
        orig_row_10,
        rtol=0.0,
        atol=1e-6,
    )
    np.testing.assert_allclose(
        pm_fixture.profile_data.loc[612].to_numpy(dtype=float),
        np.ones(24),
        rtol=0.0,
        atol=1e-6,
    )
    df_fixture_loaded = load_SUEWS_table(fixture_out / "SUEWS_Profiles.txt")
    assert 612 in df_fixture_loaded.index
    np.testing.assert_allclose(
        df_fixture_loaded.loc[612].to_numpy(dtype=float),
        np.ones(24),
        rtol=0.0,
        atol=1e-6,
    )
    np.testing.assert_allclose(
        df_fixture_loaded.loc[10].to_numpy(dtype=float),
        orig_row_10,
        rtol=0.0,
        atol=1e-3,
    )

    # ARRANGE: integer-valued CSV (int64 columns) upcasts cleanly on float default row
    int_csv = tmp_path / "int_profiles.txt"
    _write_profiles_csv(int_csv, {10: [2] * 24}, legacy_two_line=False)
    pm_int = ProfileManager(int_csv)
    int_out = tmp_path / "int_out"
    int_out.mkdir()

    # ACT
    pm_int.ensure_required_profiles(int_out)

    # ASSERT
    assert pm_int.profile_data is not None
    np.testing.assert_allclose(
        pm_int.profile_data.loc[10].to_numpy(dtype=float),
        np.full(24, 2.0),
        rtol=0.0,
        atol=1e-6,
    )
    for code in (1, 999):
        np.testing.assert_allclose(
            pm_int.profile_data.loc[code].to_numpy(dtype=float),
            np.ones(24),
            rtol=0.0,
            atol=1e-6,
        )


def test_create_default_profile_columns_contract() -> None:
    """create_default_profile preserves range(24) default, aligns columns, and rejects <24."""
    pm = ProfileManager()

    # Default columns=None preserves backward-compatible range(24) integer index
    ser_default = pm.create_default_profile(42)
    assert ser_default.name == 42
    assert list(ser_default.index) == list(range(24))
    np.testing.assert_allclose(ser_default.to_numpy(dtype=float), np.ones(24))

    # Explicit >24 columns aligns to the first 24 hour labels
    extra_cols = [str(h) for h in range(24)] + ["extra"]
    ser_aligned = pm.create_default_profile(43, columns=extra_cols)
    assert ser_aligned.name == 43
    assert list(ser_aligned.index) == [str(h) for h in range(24)]
    np.testing.assert_allclose(ser_aligned.to_numpy(dtype=float), np.ones(24))

    # Fewer than 24 columns raises ValueError
    with pytest.raises(ValueError, match="Expected at least 24 profile columns"):
        pm.create_default_profile(44, columns=["0", "1", "2"])
