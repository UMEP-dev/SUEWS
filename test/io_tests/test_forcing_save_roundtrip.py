"""SUEWSForcing.save must produce a native file that loads back losslessly."""

import warnings

import numpy as np
import pandas as pd
import pytest

from supy.data_model.forcing import FORCING_REGISTRY
from supy.suews_forcing import SUEWSForcing

pytestmark = pytest.mark.api

_CANONICAL = list(FORCING_REGISTRY.canonical_file_columns)
_DATA_COLUMNS = [c for c in _CANONICAL if c not in {"iy", "id", "it", "imin"}]
_TIME_COLUMNS = ["iy", "id", "it", "imin"]


def _synthetic_forcing(index: pd.DatetimeIndex, seed: int = 0) -> SUEWSForcing:
    """Build a model-ready (hPa) forcing frame with every canonical column."""
    rng = np.random.default_rng(seed)
    n = len(index)
    df = pd.DataFrame(index=index)
    df["iy"] = index.year
    df["id"] = index.dayofyear
    df["it"] = index.hour
    df["imin"] = index.minute
    df["isec"] = 0
    for col in _DATA_COLUMNS:
        df[col] = -999.0
    df["U"] = rng.uniform(0.5, 8.0, n).round(3)
    df["RH"] = rng.uniform(30.0, 99.0, n).round(2)
    df["Tair"] = rng.uniform(-5.0, 30.0, n).round(2)
    df["pres"] = rng.uniform(980.0, 1035.0, n).round(1)  # hPa in memory
    df["rain"] = rng.uniform(0.0, 2.0, n).round(3)
    df["kdown"] = rng.uniform(0.0, 800.0, n).round(1)
    df["ldown"] = rng.uniform(250.0, 400.0, n).round(1)
    return SUEWSForcing(df, source="synthetic")


def _reload(path, tstep_mod):
    with warnings.catch_warnings():
        warnings.simplefilter("error", UserWarning)
        return SUEWSForcing.from_file(path, tstep_mod=tstep_mod)


def _assert_values_round_trip(original: SUEWSForcing, reloaded: SUEWSForcing):
    assert reloaded.index.equals(original.index)
    orig = original.df
    back = reloaded.df
    for col in _DATA_COLUMNS:
        np.testing.assert_allclose(
            back[col].to_numpy(),
            orig[col].to_numpy(),
            rtol=1e-12,
            atol=0,
            err_msg=f"column {col} changed across save/load",
        )


@pytest.fixture
def five_minute_forcing():
    index = pd.date_range("2012-01-01 00:05", periods=36, freq="5min")
    return _synthetic_forcing(index)


class TestNativeSaveLayout:
    def test_header_is_registry_canonical_without_index_or_isec(
        self, five_minute_forcing, tmp_path
    ):
        path = five_minute_forcing.save(tmp_path / "forcing.txt")
        header = path.read_text(encoding="utf-8").splitlines()[0].split("\t")
        assert header == _CANONICAL
        assert "isec" not in header
        assert header[0] == "iy"  # no leading datetime-index column

    def test_pressure_written_in_kpa(self, five_minute_forcing, tmp_path):
        path = five_minute_forcing.save(tmp_path / "forcing.txt")
        written = pd.read_csv(path, sep="\t")
        pres_var = FORCING_REGISTRY.by_file_name("pres")
        assert pres_var.unit == "kPa" and pres_var.runtime_unit == "hPa"
        np.testing.assert_allclose(
            written["pres"].to_numpy() * pres_var.runtime_scale,
            five_minute_forcing.df["pres"].to_numpy(),
            rtol=1e-12,
        )
        assert written["pres"].median() < 200  # kPa, not hPa

    def test_sentinel_in_scaled_column_is_not_scaled(self, tmp_path):
        index = pd.date_range("2012-06-01 00:05", periods=12, freq="5min")
        forcing = _synthetic_forcing(index)
        df = forcing.df
        df.loc[df.index[3], "pres"] = -999.0
        df.loc[df.index[4], "pres"] = np.nan
        forcing = SUEWSForcing(df)
        path = forcing.save(tmp_path / "forcing.txt")
        written = pd.read_csv(path, sep="\t")
        assert written["pres"].iloc[3] == pytest.approx(-999.0)
        assert written["pres"].iloc[4] == pytest.approx(-999.0)
        assert (written["pres"].drop(index=[3, 4]) < 200).all()


class TestNativeSaveRoundTrip:
    def test_values_and_index_round_trip_at_model_timestep(
        self, five_minute_forcing, tmp_path
    ):
        path = five_minute_forcing.save(tmp_path / "forcing.txt")
        reloaded = _reload(path, tstep_mod=300)
        _assert_values_round_trip(five_minute_forcing, reloaded)
        assert reloaded.df["pres"].median() > 900  # hPa restored in memory

    def test_midnight_and_year_boundary_timestamps_round_trip(self, tmp_path):
        # Interval-end convention: the last row of a year is 1 Jan 00:00 of
        # the next year, and each midnight row carries it=0, imin=0.
        index = pd.date_range("2012-12-31 21:00", "2013-01-01 03:00", freq="1h")
        forcing = _synthetic_forcing(index)
        path = forcing.save(tmp_path / "forcing.txt")
        rows = pd.read_csv(path, sep="\t")
        midnight = rows[(rows["it"] == 0) & (rows["imin"] == 0)]
        assert len(midnight) == 1
        assert midnight.iloc[0][["iy", "id"]].tolist() == [2013, 1]
        reloaded = _reload(path, tstep_mod=None)
        assert reloaded.index.equals(index)

    def test_non_default_resolution_round_trip(self, tmp_path):
        index = pd.date_range("2012-03-01 00:30", periods=48, freq="30min")
        forcing = _synthetic_forcing(index, seed=3)
        path = forcing.save(tmp_path / "forcing.txt")
        reloaded = _reload(path, tstep_mod=1800)
        _assert_values_round_trip(forcing, reloaded)
        assert reloaded.timestep == pd.Timedelta("30min")

    def test_extras_round_trip(self, five_minute_forcing, tmp_path):
        n = len(five_minute_forcing)
        five_minute_forcing._extras = {
            "wuh_grass": np.linspace(0.0, 0.5, n),
            "lai_grass": np.full(n, 2.5),
            "lai_evetr": np.full(n, 4.0),
        }
        path = five_minute_forcing.save(tmp_path / "forcing.txt")
        header = path.read_text(encoding="utf-8").splitlines()[0].split("\t")
        assert header[: len(_CANONICAL)] == _CANONICAL
        # Registry order: lai_* before wuh_*.
        assert header[len(_CANONICAL) :] == ["lai_evetr", "lai_grass", "wuh_grass"]

        reloaded = _reload(path, tstep_mod=300)
        assert set(reloaded.extras) == {"wuh_grass", "lai_grass", "lai_evetr"}
        for name, values in five_minute_forcing.extras.items():
            np.testing.assert_allclose(reloaded.extras[name], values, rtol=1e-12)
        _assert_values_round_trip(five_minute_forcing, reloaded)

    def test_missing_optional_columns_filled_with_sentinel(self, tmp_path):
        index = pd.date_range("2012-01-01 00:05", periods=12, freq="5min")
        forcing = _synthetic_forcing(index)
        df = forcing.df.drop(columns=["kdiff", "kdir", "wdir", "xsmd"])
        path = SUEWSForcing(df).save(tmp_path / "forcing.txt")
        written = pd.read_csv(path, sep="\t")
        assert list(written.columns) == _CANONICAL
        np.testing.assert_array_equal(
            written[["kdiff", "kdir", "wdir", "xsmd"]].to_numpy(), -999.0
        )
        _reload(path, tstep_mod=300)


class TestNativeSaveErrors:
    def test_missing_baseline_column_raises(self, five_minute_forcing, tmp_path):
        df = five_minute_forcing.df.drop(columns=["pres"])
        with pytest.raises(ValueError, match="baseline"):
            SUEWSForcing(df).save(tmp_path / "forcing.txt")

    def test_unknown_column_is_written_with_warning(
        self, five_minute_forcing, tmp_path
    ):
        df = five_minute_forcing.df
        df["not_a_forcing_variable"] = 1.0
        with pytest.warns(UserWarning, match="not_a_forcing_variable"):
            path = SUEWSForcing(df).save(tmp_path / "forcing.txt")
        header = path.read_text(encoding="utf-8").splitlines()[0].split("\t")
        assert header[-1] == "not_a_forcing_variable"


class TestCsvExport:
    def test_csv_keeps_internal_units_and_appends_extras(
        self, five_minute_forcing, tmp_path
    ):
        n = len(five_minute_forcing)
        five_minute_forcing._extras = {"lai_grass": np.full(n, 2.5)}
        path = five_minute_forcing.save(tmp_path / "forcing.csv", format="csv")
        written = pd.read_csv(path, index_col=0, parse_dates=True)
        assert written.index.equals(five_minute_forcing.index)
        assert "lai_grass" in written.columns
        np.testing.assert_allclose(
            written["pres"].to_numpy(), five_minute_forcing.df["pres"].to_numpy()
        )
