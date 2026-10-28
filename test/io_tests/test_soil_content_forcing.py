"""Content survives public file and DataFrame paths before deficit derivation."""
# Tests deliberately exercise the private file/DataFrame preparation boundary.
# ruff: disable[import-private-name]
from pathlib import Path

import numpy as np
import pandas as pd
import pytest

from supy import SUEWSForcing
from supy.util._forcing import interpolate_soil_content
from supy.util._io import prepare_dataframe_forcing, read_forcing

# ruff: enable[import-private-name]

pytestmark = pytest.mark.api
FIXTURE = Path(__file__).parents[1] / "fixtures/soil_observation/modelled_content.txt"


def test_file_and_dataframe_interpolation_agree(tmp_path):
    frame = read_forcing(str(FIXTURE), tstep_mod=None).iloc[:3].copy()
    frame.index = pd.date_range("2024-01-01", periods=3, freq="h")
    frame["sm"] = [140, 60, 80]
    path = SUEWSForcing(frame).save(tmp_path / "content.txt")
    loaded = read_forcing(str(path), tstep_mod=300)
    direct = prepare_dataframe_forcing(frame, tstep_mod=300)
    pd.testing.assert_series_equal(loaded.sm, direct.sm)
    assert loaded.loc["2024-01-01 00:30", "sm"] == 100
    assert loaded.loc["2024-01-01 02:00", "sm"] == 80
    assert loaded.loc[:"2023-12-31 23:55", "sm"].eq(-999).all()


def test_positional_contract_is_explicit(tmp_path):
    content = FIXTURE.read_text(encoding="utf-8").splitlines()
    path = tmp_path / "positional.txt"
    path.write_text("\n".join(content[1:]) + "\n", encoding="utf-8")
    with pytest.raises(ValueError, match="source_contract"):
        read_forcing(str(path), tstep_mod=None)
    frame = read_forcing(str(path), tstep_mod=None, source_contract="2.0.0")
    expected = read_forcing(str(FIXTURE), tstep_mod=None)
    pd.testing.assert_frame_equal(frame, expected)
    with pytest.raises(ValueError, match="explicit migration"):
        read_forcing(str(path), source_contract="1.2.0")


def test_old_header_is_rejected_even_if_missing(tmp_path):
    path = tmp_path / "legacy.txt"
    path.write_text(FIXTURE.read_text(encoding="utf-8").replace(" sm ", " xsmd "), encoding="utf-8")
    with pytest.raises(ValueError, match="Legacy"):
        read_forcing(str(path))


def test_coarsening_samples_content_at_endpoint():
    frame = read_forcing(str(FIXTURE), tstep_mod=None).iloc[:12].copy()
    frame["sm"] = np.arange(len(frame)) + 40.0
    coarsened = SUEWSForcing(frame).resample("15min").df
    np.testing.assert_array_equal(coarsened.sm, frame.sm.iloc[2::3])


def test_interpolation_chunk_edges_match_with_shared_bounds():
    source = pd.Series([140, 60, 80], index=pd.date_range("2024-01-01", periods=3, freq="h"))
    target = pd.date_range(source.index[0], source.index[-1], freq="5min")
    complete = interpolate_soil_content(source, target)
    chunks = pd.concat([interpolate_soil_content(source.iloc[:2], target[:12]),
                        interpolate_soil_content(source.iloc[1:], target[12:])])
    pd.testing.assert_series_equal(chunks, complete)
