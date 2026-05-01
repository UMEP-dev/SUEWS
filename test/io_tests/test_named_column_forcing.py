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


def test_missing_baseline_column_raises(tmp_path):
    """T5: mis-named baseline column (`temperature` instead of `Tair`)
    raises ValueError whose message contains the canonical name."""
    from supy.util._io import read_forcing

    text = CANONICAL_FIXTURE.read_text()
    lines = text.splitlines()
    header = lines[0].replace("Tair", "temperature")
    bad = "\n".join([header, *lines[1:]])
    bad_path = tmp_path / "bad.txt"
    bad_path.write_text(bad)
    with pytest.raises(ValueError, match=r"\bTair\b"):
        read_forcing(str(bad_path), tstep_mod=None)


def test_unknown_column_warns(tmp_path):
    """T6: an unknown column produces a UserWarning but the run continues."""
    from supy.util._io import read_forcing

    text = CANONICAL_FIXTURE.read_text()
    lines = text.splitlines()
    header = lines[0] + " weird_var"
    rows = [line + " 0.0" for line in lines[1:]]
    augmented = "\n".join([header, *rows])
    path = tmp_path / "with_weird.txt"
    path.write_text(augmented)
    with pytest.warns(UserWarning, match="weird_var"):
        df = read_forcing(str(path), tstep_mod=None)
    assert "weird_var" not in df.columns


def test_missing_optional_column_filled_with_sentinel(tmp_path):
    """Missing optional canonical columns are filled with -999."""
    from supy.util._io import read_forcing

    text = CANONICAL_FIXTURE.read_text()
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
    path.write_text(text_out)
    df = read_forcing(str(path), tstep_mod=None)
    assert "snow" in df.columns
    assert (df["snow"] == -999.0).all()


def test_per_landcover_columns_separated_into_extras(tmp_path):
    """T3/T4: lai_evetr/dectr/grass and wuh_paved end up in SUEWSForcing.extras,
    not in the kernel-facing DataFrame; main DataFrame shape unchanged."""
    from supy.suews_forcing import SUEWSForcing

    text = CANONICAL_FIXTURE.read_text()
    lines = text.splitlines()
    header = lines[0] + " lai_evetr lai_dectr lai_grass wuh_paved"
    new_lines = [header]
    for row in lines[1:]:
        new_lines.append(row + " 1.5 2.5 3.5 0.25")
    p = tmp_path / "kc_per_landcover.txt"
    p.write_text("\n".join(new_lines))

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


def test_lai_per_landcover_rejected_for_non_vegetated_surface(tmp_path):
    """LAI is meaningful only for vegetated surfaces; lai_paved/lai_bldgs/
    lai_bsoil/lai_water must be treated as unknown (warn-and-drop), not
    plumbed through extras."""
    from supy.suews_forcing import SUEWSForcing

    text = CANONICAL_FIXTURE.read_text()
    lines = text.splitlines()
    header = lines[0] + " lai_paved lai_water"
    new_lines = [header]
    for row in lines[1:]:
        new_lines.append(row + " 0.1 0.2")
    p = tmp_path / "kc_lai_nonveg.txt"
    p.write_text("\n".join(new_lines))

    with pytest.warns(UserWarning):
        forcing = SUEWSForcing.from_file(str(p))
    assert "lai_paved" not in forcing.extras
    assert "lai_water" not in forcing.extras


def test_wuh_per_landcover_accepts_every_surface(tmp_path):
    """External water use is meaningful on every surface — irrigation
    and impervious-surface washing on the six land surfaces, fountains
    and ornamental water features on the open-water surface."""
    from supy.suews_forcing import SUEWSForcing

    text = CANONICAL_FIXTURE.read_text()
    lines = text.splitlines()
    header = lines[0] + " wuh_paved wuh_grass wuh_water"
    new_lines = [header]
    for row in lines[1:]:
        new_lines.append(row + " 0.05 0.30 0.10")
    p = tmp_path / "kc_wuh_mixed.txt"
    p.write_text("\n".join(new_lines))

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
    lines = sample.read_text().splitlines()
    path = tmp_path / "hourly_extra.txt"
    path.write_text(
        "\n".join([lines[0] + " lai_evetr", *[line + " 1.5" for line in lines[1:5]]])
    )

    forcing = SUEWSForcing.from_file(str(path))
    assert "lai_evetr" in forcing.extras
    assert np.isclose(forcing.extras["lai_evetr"], 1.5).all()


def test_mixed_case_headers_across_files_coalesce(tmp_path):
    """Case-insensitive matching works across concatenated forcing files."""
    from supy.util._io import read_forcing

    text = CANONICAL_FIXTURE.read_text()
    lines = text.splitlines()
    (tmp_path / "a.txt").write_text("\n".join(lines[:3]))
    (tmp_path / "b.txt").write_text(
        "\n".join([lines[0].replace("Tair", "TAIR"), *lines[3:5]])
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
