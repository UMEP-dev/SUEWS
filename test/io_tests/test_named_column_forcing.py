"""Tests for header-driven forcing column matching (gh#1372)."""

from pathlib import Path

import numpy as np
import pandas as pd
import pytest

pytestmark = pytest.mark.api

FIXTURE_DIR = Path(__file__).resolve().parent.parent / "fixtures" / "benchmark1" / "forcing"
CANONICAL_FIXTURE = FIXTURE_DIR / "Kc1_2011_data_5_tiny.txt"


def _read_canonical():
    from supy.util._io import read_forcing
    return read_forcing(str(CANONICAL_FIXTURE), tstep_mod=None)


def test_canonical_fixture_unchanged():
    """T1: existing canonical fixture must produce the same DataFrame as before
    (same numeric values; columns named to canonical set)."""
    df = _read_canonical()
    canonical = {"iy", "id", "it", "imin", "Tair", "RH", "U", "pres", "rain", "kdown"}
    assert canonical.issubset(set(df.columns))
    first = df.iloc[0]
    assert np.isfinite(first["Tair"])
    assert np.isfinite(first["pres"])  # in hPa after kPa->hPa conversion


def test_file_aliases_yield_identical_canonical_dataframe(tmp_path):
    """Aliases already accepted by Rust resolve to the same Python columns."""
    from supy.util._io import read_forcing

    aliases = {
        "qn": "qn1_obs",
        "qs": "qs_obs",
        "qf": "qf_obs",
        "Tair": "temp_c",
        "snow": "snowfrac",
        "wuh": "wu_mm",
    }
    lines = CANONICAL_FIXTURE.read_text(encoding="utf-8").splitlines()
    alias_header = " ".join(aliases.get(token, token) for token in lines[0].split())
    alias_path = tmp_path / "aliases.txt"
    alias_path.write_text("\n".join([alias_header, *lines[1:]]), encoding="utf-8")

    expected = _read_canonical()
    actual = read_forcing(str(alias_path), tstep_mod=None)

    pd.testing.assert_frame_equal(actual, expected)


def test_missing_baseline_column_raises(tmp_path):
    """T5: mis-named baseline column (`temperature` instead of `Tair`)
    raises ValueError whose message contains the canonical name."""
    from supy.util._io import read_forcing

    text = CANONICAL_FIXTURE.read_text(encoding="utf-8")
    lines = text.splitlines()
    header = lines[0].replace("Tair", "temperature")
    bad = "\n".join([header, *lines[1:]])
    bad_path = tmp_path / "bad.txt"
    bad_path.write_text(bad, encoding="utf-8")
    with pytest.raises(ValueError, match=r"\bTair\b"):
        read_forcing(str(bad_path), tstep_mod=None)


@pytest.mark.parametrize(
    "column",
    ("iy", "id", "it", "imin", "U", "RH", "Tair", "pres", "rain", "kdown"),
)
def test_each_missing_baseline_column_is_named_directly(tmp_path, column):
    """Every baseline header failure identifies the canonical column."""
    from supy.util._io import read_forcing

    lines = CANONICAL_FIXTURE.read_text(encoding="utf-8").splitlines()
    header_tokens = lines[0].split()
    drop_index = header_tokens.index(column)
    rows = [
        " ".join(
            token for index, token in enumerate(line.split()) if index != drop_index
        )
        for line in lines[1:]
    ]
    path = tmp_path / f"missing-{column}.txt"
    path.write_text(
        "\n".join([
            " ".join(
                token
                for index, token in enumerate(header_tokens)
                if index != drop_index
            ),
            *rows,
        ]),
        encoding="utf-8",
    )

    with pytest.raises(ValueError, match=rf"\b{column}\b"):
        read_forcing(str(path), tstep_mod=None)


def test_unknown_column_warns(tmp_path):
    """T6: an unknown column produces a UserWarning but the run continues."""
    from supy.util._io import read_forcing

    text = CANONICAL_FIXTURE.read_text(encoding="utf-8")
    lines = text.splitlines()
    header = lines[0] + " weird_var"
    rows = [line + " 0.0" for line in lines[1:]]
    augmented = "\n".join([header, *rows])
    path = tmp_path / "with_weird.txt"
    path.write_text(augmented, encoding="utf-8")
    with pytest.warns(UserWarning, match="weird_var"):
        df = read_forcing(str(path), tstep_mod=None)
    assert "weird_var" not in df.columns


def test_missing_optional_column_filled_with_sentinel(tmp_path):
    """Missing optional canonical columns are filled with -999."""
    from supy.util._io import read_forcing

    text = CANONICAL_FIXTURE.read_text(encoding="utf-8")
    lines = text.splitlines()
    header_tokens = lines[0].split()
    drop_idx = header_tokens.index("snow")
    new_header = " ".join(t for i, t in enumerate(header_tokens) if i != drop_idx)
    new_rows = []
    for row in lines[1:]:
        toks = row.split()
        new_rows.append(" ".join(t for i, t in enumerate(toks) if i != drop_idx))
    text_out = "\n".join([new_header, *new_rows])
    path = tmp_path / "no_snow.txt"
    path.write_text(text_out, encoding="utf-8")
    df = read_forcing(str(path), tstep_mod=None)
    assert "snow" in df.columns
    assert (df["snow"] == -999.0).all()


def test_per_landcover_columns_separated_into_extras(tmp_path):
    """T3/T4: lai_evetr/dectr/grass and wuh_paved end up in SUEWSForcing.extras,
    not in the kernel-facing DataFrame; main DataFrame shape unchanged."""
    from supy.suews_forcing import SUEWSForcing

    text = CANONICAL_FIXTURE.read_text(encoding="utf-8")
    lines = text.splitlines()
    header = lines[0] + " lai_evetr lai_dectr lai_grass wuh_paved"
    new_lines = [header]
    for row in lines[1:]:
        new_lines.append(row + " 1.5 2.5 3.5 0.25")
    p = tmp_path / "kc_per_landcover.txt"
    p.write_text("\n".join(new_lines), encoding="utf-8")

    forcing = SUEWSForcing.from_file(str(p))
    assert hasattr(forcing, "extras")
    assert set(forcing.extras.keys()) == {
        "lai_evetr", "lai_dectr", "lai_grass", "wuh_paved",
    }
    # Each extras series matches the appended constant value
    assert (forcing.extras["lai_evetr"] == 1.5).all()
    assert (forcing.extras["wuh_paved"] == 0.25).all()
    # Main DataFrame retains canonical columns; per-landcover ones are gone.
    canonical = {"iy", "id", "it", "imin", "Tair", "RH", "U", "pres", "rain",
                 "kdown", "snow", "ldown", "fcld", "Wuh", "xsmd", "lai",
                 "qn", "qh", "qe", "qs", "qf", "isec"}
    assert canonical.issubset(set(forcing.df.columns))
    assert "lai_evetr" not in forcing.df.columns


def test_surface_file_alias_is_stored_under_canonical_extra_name(tmp_path):
    """Rust-style ``wu_mm_*`` file aliases canonicalise to ``wuh_*`` extras."""
    from supy.suews_forcing import SUEWSForcing

    lines = CANONICAL_FIXTURE.read_text(encoding="utf-8").splitlines()
    alias_lines = [lines[0] + " wu_mm_paved"]
    alias_lines.extend(line + " 0.25" for line in lines[1:])
    path = tmp_path / "surface-alias.txt"
    path.write_text("\n".join(alias_lines), encoding="utf-8")

    forcing = SUEWSForcing.from_file(str(path))

    assert "wu_mm_paved" not in forcing.extras
    assert np.allclose(forcing.extras["wuh_paved"], 0.25)


def test_lai_per_landcover_rejected_for_non_vegetated_surface(tmp_path):
    """LAI is meaningful only for vegetated surfaces; lai_paved/lai_bldgs/
    lai_bsoil/lai_water must be treated as unknown (warn-and-drop), not
    plumbed through extras."""
    from supy.suews_forcing import SUEWSForcing

    text = CANONICAL_FIXTURE.read_text(encoding="utf-8")
    lines = text.splitlines()
    header = lines[0] + " lai_paved lai_water"
    new_lines = [header]
    for row in lines[1:]:
        new_lines.append(row + " 0.1 0.2")
    p = tmp_path / "kc_lai_nonveg.txt"
    p.write_text("\n".join(new_lines), encoding="utf-8")

    with pytest.warns(UserWarning):
        forcing = SUEWSForcing.from_file(str(p))
    assert "lai_paved" not in forcing.extras
    assert "lai_water" not in forcing.extras


def test_wuh_per_landcover_accepts_every_surface(tmp_path):
    """External water use is meaningful on every surface — irrigation
    and impervious-surface washing on the six land surfaces, fountains
    and ornamental water features on the open-water surface."""
    from supy.suews_forcing import SUEWSForcing

    text = CANONICAL_FIXTURE.read_text(encoding="utf-8")
    lines = text.splitlines()
    header = lines[0] + " wuh_paved wuh_grass wuh_water"
    new_lines = [header]
    for row in lines[1:]:
        new_lines.append(row + " 0.05 0.30 0.10")
    p = tmp_path / "kc_wuh_mixed.txt"
    p.write_text("\n".join(new_lines), encoding="utf-8")

    forcing = SUEWSForcing.from_file(str(p))
    assert "wuh_paved" in forcing.extras
    assert "wuh_grass" in forcing.extras
    assert "wuh_water" in forcing.extras
    assert (forcing.extras["wuh_water"] == 0.10).all()


def test_per_landcover_extras_survive_resampling(tmp_path):
    """Hourly per-landcover extras are resampled, not replaced by sentinels."""
    from importlib.resources import files

    from supy.suews_forcing import SUEWSForcing

    sample = files("supy") / "sample_data" / "Kc_2012_data_60.txt"
    lines = sample.read_text(encoding="utf-8").splitlines()
    path = tmp_path / "hourly_extra.txt"
    path.write_text(
        "\n".join([lines[0] + " lai_evetr", *[line + " 1.5" for line in lines[1:5]]]),
        encoding="utf-8",
    )

    forcing = SUEWSForcing.from_file(str(path))
    assert "lai_evetr" in forcing.extras
    assert np.isclose(forcing.extras["lai_evetr"], 1.5).all()


def test_wuh_per_landcover_extras_resample_as_timestep_sum(tmp_path):
    """Hourly wuh_<surface> depths are redistributed like rain."""
    from importlib.resources import files

    from supy.suews_forcing import SUEWSForcing

    sample = files("supy") / "sample_data" / "Kc_2012_data_60.txt"
    lines = sample.read_text(encoding="utf-8").splitlines()
    path = tmp_path / "hourly_wuh_extra.txt"
    path.write_text(
        "\n".join([lines[0] + " wuh_grass", *[line + " 12.0" for line in lines[1:4]]]),
        encoding="utf-8",
    )

    forcing = SUEWSForcing.from_file(str(path))
    assert "wuh_grass" in forcing.extras
    assert np.isclose(forcing.extras["wuh_grass"][:12], 1.0).all()
    assert np.isclose(forcing.extras["wuh_grass"][:12].sum(), 12.0)


def test_per_landcover_extras_survive_time_slicing(tmp_path):
    """Sliced SUEWSForcing objects keep time-aligned extras."""
    from supy.suews_forcing import SUEWSForcing

    text = CANONICAL_FIXTURE.read_text(encoding="utf-8")
    lines = text.splitlines()
    path = tmp_path / "kc_extra_slice.txt"
    data_rows = [
        line + f" {1.0 + i:.1f} {10.0 + i:.1f}"
        for i, line in enumerate(lines[1:])
    ]
    path.write_text(
        "\n".join([lines[0] + " lai_evetr wuh_grass", *data_rows]),
        encoding="utf-8",
    )

    forcing = SUEWSForcing.from_file(str(path), tstep_mod=None)
    sliced = forcing.iloc[:2]
    assert set(sliced.extras) == {"lai_evetr", "wuh_grass"}
    assert np.allclose(sliced.extras["lai_evetr"], [1.0, 2.0])
    assert np.allclose(sliced.extras["wuh_grass"], [10.0, 11.0])

    resampled = forcing.resample("15min")
    assert set(resampled.extras) == {"lai_evetr", "wuh_grass"}
    assert np.isclose(resampled.extras["lai_evetr"][0], 3.0)
    assert np.isclose(resampled.extras["wuh_grass"][0], 33.0)


def test_mixed_case_headers_across_files_coalesce(tmp_path):
    """Case-insensitive matching works across concatenated forcing files."""
    from supy.util._io import read_forcing

    text = CANONICAL_FIXTURE.read_text(encoding="utf-8")
    lines = text.splitlines()
    (tmp_path / "a.txt").write_text("\n".join(lines[:3]), encoding="utf-8")
    (tmp_path / "b.txt").write_text(
        "\n".join([lines[0].replace("Tair", "TAIR"), *lines[3:5]]),
        encoding="utf-8",
    )

    df = read_forcing(str(tmp_path / "*.txt"), tstep_mod=None)
    assert df["Tair"].notna().all()
    assert np.isfinite(df["Tair"]).all()


def test_shuffled_header_yields_same_dataframe_as_canonical():
    """T2: a fixture with shuffled column order produces the same DataFrame
    (canonical order, same values) as the canonical-ordered fixture."""
    from supy.util._io import read_forcing

    canonical_path = CANONICAL_FIXTURE
    shuffled_path = (
        Path(__file__).resolve().parent.parent
        / "fixtures" / "forcing" / "kc_shuffled.txt"
    )
    df_canonical = read_forcing(str(canonical_path), tstep_mod=None)
    df_shuffled = read_forcing(str(shuffled_path), tstep_mod=None)

    assert list(df_canonical.columns) == list(df_shuffled.columns)
    pd.testing.assert_frame_equal(df_canonical, df_shuffled)


def test_check_forcing_accepts_per_landcover_extras():
    """gh#1413 (PR#1378 follow-up): check_forcing must not flag per-landcover
    extras as missing/positional errors. Pre-fix, the validator zipped
    columns positionally against a 25-key reference list including the
    internal `isec` key, so any DataFrame with `lai_<surface>` or
    `wuh_<surface>` columns appended after the canonical 24 hit
    "Missing columns: {'isec'}" and "Column lai_evetr is not in the
    valid position for isec".
    """
    from supy._check import check_forcing
    from supy._load import CANONICAL_FORCING_COLUMNS

    idx = pd.date_range("2024-01-01", periods=24, freq="h")
    data = {
        "iy": idx.year,
        "id": idx.dayofyear,
        "it": idx.hour,
        "imin": idx.minute,
        "U": np.full(24, 3.0),
        "RH": np.full(24, 60.0),
        "Tair": np.full(24, 15.0),
        "pres": np.full(24, 1013.0),
        "rain": np.zeros(24),
        "kdown": np.full(24, 100.0),
    }
    # fill remaining canonical optionals with the sentinel the named-column
    # reader uses (-999.0)
    for col in CANONICAL_FORCING_COLUMNS:
        if col not in data:
            data[col] = np.full(24, -999.0)
    # per-landcover extras appended at the end
    data["lai_evetr"] = np.full(24, 1.5)
    data["lai_dectr"] = np.full(24, 1.2)
    data["wuh_paved"] = np.zeros(24)
    df_forcing = pd.DataFrame(data, index=idx)

    issues = check_forcing(df_forcing, fix=False)

    # validator should return None (passes) — no missing-column or
    # positional issues attributable to per-LC extras or to `isec`.
    assert issues is None, (
        f"check_forcing flagged per-landcover-aware DataFrame: {issues}"
    )


def test_check_forcing_enforces_wuh_depth_range_without_finite_cap():
    """Bulk and surface Wuh are non-negative depths with no finite maximum."""
    from supy._check import check_forcing

    df_forcing = _read_canonical()
    df_forcing["Wuh"] = 12.0
    df_forcing["wuh_paved"] = 12.0
    assert check_forcing(df_forcing, fix=False) is None

    df_forcing["wuh_paved"] = -0.1
    issues = check_forcing(df_forcing, fix=False)
    assert any("wuh_paved" in issue for issue in issues)

    df_forcing["wuh_paved"] = -999.0
    assert check_forcing(df_forcing, fix=False) is None
    assert np.isclose(df_forcing["wuh_paved"], -999.0).all()

    df_forcing["Wuh"] = -950.0
    df_forcing["wuh_paved"] = -950.0
    issues = check_forcing(df_forcing, fix=False)
    assert any("`wuh`" in issue for issue in issues)
    assert any("wuh_paved" in issue for issue in issues)

    df_forcing["Wuh"] = np.nan
    df_forcing["wuh_paved"] = np.nan
    issues = check_forcing(df_forcing, fix=False)
    assert any("`wuh`" in issue for issue in issues)
    assert any("wuh_paved" in issue for issue in issues)

    df_forcing["Wuh"] = np.inf
    df_forcing["wuh_paved"] = np.inf
    issues = check_forcing(df_forcing, fix=False)
    assert any("`wuh`" in issue for issue in issues)
    assert any("wuh_paved" in issue for issue in issues)


def test_check_forcing_fixes_invalid_wuh_without_clipping_missing_sentinel():
    """Unbounded Wuh rules support fix mode and preserve exact -999 values."""
    from supy._check import check_forcing

    df_forcing = _read_canonical()
    df_forcing["Wuh"] = 12.0
    df_forcing.iloc[0, df_forcing.columns.get_loc("Wuh")] = -999.0
    df_forcing.iloc[1, df_forcing.columns.get_loc("Wuh")] = -950.0
    df_forcing.iloc[2, df_forcing.columns.get_loc("Wuh")] = np.inf
    df_forcing.iloc[3, df_forcing.columns.get_loc("Wuh")] = np.nan

    fixed = check_forcing(df_forcing, fix=True)
    assert fixed["Wuh"].iloc[0] == -999.0
    assert fixed["Wuh"].iloc[1] == 0.0
    assert fixed["Wuh"].iloc[2] == -999.0
    assert fixed["Wuh"].iloc[3] == -999.0
    assert np.isclose(fixed["Wuh"].iloc[4:], 12.0).all()


@pytest.mark.parametrize("invalid", ("-950", "nan", "inf"))
@pytest.mark.parametrize("tstep_mod", (None, 300))
def test_file_resampling_rejects_invalid_wuh_before_missing_normalisation(
    tmp_path, invalid, tstep_mod
):
    """File loading must reject invalid Wuh before generic sentinel handling."""
    from supy.util._io import read_forcing

    path = tmp_path / f"invalid-wuh-{invalid}.txt"
    path.write_text(
        "\n".join([
            "iy id it imin Tair RH U pres rain kdown Wuh wuh_paved",
            "2012 1 1 0 10 50 2 101.3 0 0 2 2",
            f"2012 1 2 0 10 50 2 101.3 0 0 {invalid} {invalid}",
            "2012 1 3 0 10 50 2 101.3 0 0 2 2",
        ]),
        encoding="utf-8",
    )

    with pytest.raises(ValueError, match=r"Wuh|wuh_paved"):
        read_forcing(str(path), tstep_mod=tstep_mod)


def test_check_forcing_flags_truly_unknown_columns():
    """gh#1413: the name-based unknown-column check (replacing the legacy
    positional zip) must still reject columns that are neither canonical,
    `isec`, nor a whitelisted per-landcover extra.
    """
    from supy._check import check_forcing
    from supy._load import CANONICAL_FORCING_COLUMNS

    idx = pd.date_range("2024-01-01", periods=24, freq="h")
    data = {col: np.full(24, -999.0) for col in CANONICAL_FORCING_COLUMNS}
    # plausible but unsupported: soil moisture per land cover (see
    # @suegrimmond's #1378 comment — only LAI and water-use are
    # land-cover-resolved at the forcing layer).
    data["xsmd_evetr"] = np.full(24, 0.2)
    df_forcing = pd.DataFrame(data, index=idx)

    issues = check_forcing(df_forcing, fix=False)

    assert issues is not None
    assert any("Unknown forcing columns" in s and "xsmd_evetr" in s for s in issues), (
        f"expected 'Unknown forcing columns' issue mentioning xsmd_evetr; got: {issues}"
    )
