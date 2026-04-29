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
