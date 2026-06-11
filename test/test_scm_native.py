"""Tests for the coupled single-column model (supy.scm, research preview).

The model physics lives in the compiled core (suews_phys_scm.f95 +
SUEWS_cal_multitsteps_scm); supy.scm only marshals. These tests cover:

- the parameter vector contract between Python and Fortran;
- a short coupled run staying physical (smoke);
- regression against a frozen six-hour baseline produced by the
  validated build (fixtures/scm/native_regression.json — see
  fixtures/scm/README.md for the validation campaign provenance);
- the experiment helpers (forcing tiling, rural-companion config).
"""

import json
from pathlib import Path

import numpy as np
import pytest

supy = pytest.importorskip("supy")

from supy.scm import (  # noqa: E402
    SCM_PARAM_DEFAULTS,
    make_rural_config,
    run_scm,
    scm_params_vector,
    tile_forcing,
)

FIXTURES = Path(__file__).parent / "fixtures" / "scm"


def _scm_available():
    try:
        from supy._run_rust import _load_rust_module

        return hasattr(_load_rust_module(), "run_suews_scm")
    except Exception:
        return False


pytestmark = pytest.mark.skipif(
    not _scm_available(), reason="suews_bridge.run_suews_scm not built"
)


@pytest.fixture(scope="module")
def sample():
    from supy.data_model import SUEWSConfig

    cfg_path = Path(supy.__file__).parent / "sample_data" / "sample_config.yml"
    config = SUEWSConfig.from_yaml(cfg_path)
    _, df_forcing = supy.load_sample_data()
    return config, df_forcing


def test_param_vector_layout():
    params = scm_params_vector()
    # must match SCM_PARAMS_LEN in suews_phys_scm.f95
    assert len(params) == 24
    assert params[0] == 20.0  # dz0
    assert params[10] == 15000.0  # city_length
    with pytest.raises(ValueError):
        scm_params_vector(no_such_parameter=1.0)


def test_six_hour_run_matches_frozen_baseline(sample):
    config, df_forcing = sample
    fix = json.loads((FIXTURES / "native_regression.json").read_text())
    window = df_forcing.loc[fix["start"] : fix["end"]]

    df_out, df_scm, state_json = run_scm(config, window)

    tair = df_scm["tair_mod"].iloc[::12].to_numpy()
    h_bl = df_scm["h_bl"].iloc[::12].to_numpy()
    qh = df_out["SUEWS"]["QH"].droplevel(0).iloc[::12].to_numpy()

    # tolerances absorb compiler/platform noise, not physics changes
    np.testing.assert_allclose(tair, fix["tair_mod_hourly"], atol=0.05)
    np.testing.assert_allclose(h_bl, fix["h_bl_hourly"], atol=20.0)
    np.testing.assert_allclose(qh, fix["qh_hourly"], atol=2.0)

    assert np.isfinite(df_scm[["tair_mod", "rh_mod", "u_mod", "h_bl"]]).all().all()
    assert df_scm["rh_mod"].between(2.0, 100.0).all()
    assert len(state_json) > 100


def test_snapshots_and_background_round_trip(sample):
    from supy.scm import make_background

    config, df_forcing = sample
    window = df_forcing.loc["2012-07-01 00:05":"2012-07-01 03:00"]
    df_out, df_scm, snaps, _ = run_scm(config, window, snapshot_every_h=1.0)
    assert len(snaps["times"]) == 3
    assert all(np.isfinite(row).all() for row in snaps["theta"])

    bg = make_background(snaps)
    # a ventilated run accepting its own background must execute
    df_out2, df_scm2, _ = run_scm(config, window, background=bg)
    assert np.isfinite(df_scm2["tau_adv"].iloc[-1])


def test_tile_forcing_calendar(sample):
    _, df_forcing = sample
    tiled = tile_forcing(df_forcing.iloc[: 288 * 60], [2012, 2013])
    assert tiled.index[0].year == 2012
    assert tiled.index.is_monotonic_increasing
    # 2013 block re-anchored to its own January; the 60-day slice spans
    # 29 Feb 2012, which must be dropped for the non-leap target year
    y2 = tiled[tiled.index.year == 2013]
    assert len(y2) == 288 * 59
    assert str(y2.index[0]) == "2013-01-01 00:05:00"
    assert not ((y2.index.month == 2) & (y2.index.day == 29)).any()


def test_rural_config_editor(sample):
    config, _ = sample
    rural = make_rural_config(config)
    lc = rural.sites[0].properties.land_cover

    def val(x):
        return x.value if hasattr(x, "value") else x

    assert val(lc.grass.sfr) == 0.70
    assert val(lc.bldgs.sfr) == 0.0
    total = sum(
        val(getattr(lc, n).sfr)
        for n in ("paved", "bldgs", "evetr", "dectr", "grass", "bsoil", "water")
    )
    assert total == pytest.approx(1.0)
    # the original config is untouched
    assert val(config.sites[0].properties.land_cover.bldgs.sfr) == pytest.approx(0.38)
