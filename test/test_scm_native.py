"""Tests for the coupled single-column model (supy.scm, research preview).

The model physics lives in the compiled core (suews_phys_scm.f95 +
SUEWS_cal_multitsteps_scm); supy.scm only marshals. Coverage:

- the parameter vector contract between Python and Fortran, including
  validation of invalid values (pure Python, runs everywhere);
- experiment helpers: calendar-safe forcing tiling, rural-companion
  config editing, background structural validation (pure Python);
- deterministic preview-page generation (build_site --check);
- native integration: a short coupled run staying physical, regression
  against a frozen six-hour baseline produced by the validated build
  (fixtures/scm/native_regression.json; provenance in
  fixtures/scm/README.md), snapshot/background round trip, and the
  stable ScmResult return contract.
"""

import json
import subprocess
import sys
from pathlib import Path

import numpy as np
import pytest

supy = pytest.importorskip("supy")

from supy.scm import (  # noqa: E402
    SCM_PARAM_DEFAULTS,
    ScmResult,
    background_arrays,
    make_background,
    make_rural_config,
    run_scm,
    scm_params_vector,
    tile_forcing,
)

# gh#1300 marker axis: this module exercises the public API surface
pytestmark = pytest.mark.api

FIXTURES = Path(__file__).parent / "fixtures" / "scm"
REPO = Path(__file__).resolve().parents[1]


def _scm_available():
    try:
        from supy._run_rust import _load_rust_module

        return hasattr(_load_rust_module(), "run_suews_scm")
    except Exception:
        return False


# applied per-test so the pure-Python helper coverage runs everywhere
needs_native = pytest.mark.skipif(
    not _scm_available(), reason="suews_bridge.run_suews_scm not built"
)


@pytest.fixture(scope="module")
def sample():
    from supy.data_model import SUEWSConfig

    cfg_path = Path(supy.__file__).parent / "sample_data" / "sample_config.yml"
    config = SUEWSConfig.from_yaml(cfg_path)
    _, df_forcing = supy.load_sample_data()
    return config, df_forcing


# ----------------------------------------------------------------------
# parameter contract (pure Python)
# ----------------------------------------------------------------------
def test_param_vector_layout():
    params = scm_params_vector()
    # must match SCM_PARAMS_LEN in suews_phys_scm.f95
    assert len(params) == 24
    assert params[0] == 20.0  # dz0
    assert params[10] == 15000.0  # city_length
    assert params[22] == 0.0  # use_background, internal, off by default


@pytest.mark.parametrize(
    "bad",
    [
        {"no_such_parameter": 1.0},
        {"use_background": 1},  # internal; derived from `background=`
        {"dz0": 0.0},
        {"dz0": float("nan")},
        {"ztop": 10.0},  # <= dz0
        {"stretch": 0.9},
        {"stretch": 2.0},
        {"tau_wind": 0.0},
        {"tau_ft": -1.0},
        {"substeps": 0},
        {"substeps": 2.5},
        {"stable_fn": 3},
        {"city_length": -5.0},
        {"obs_anchor_tau": float("inf")},
        {"dz0": 0.5, "ztop": 3000.0, "stretch": 1.0},  # > 500 levels
    ],
)
def test_invalid_parameters_rejected(bad):
    with pytest.raises(ValueError):
        scm_params_vector(**bad)


def test_background_structural_validation():
    t0 = "2012-07-01 00:05"
    good = dict(
        times=np.array(["2012-07-01T01:00", "2012-07-01T02:00"], dtype="datetime64[ns]"),
        z=np.array([10.0, 30.0, 60.0]),
        theta=np.full((2, 3), 290.0),
        q=np.full((2, 3), 0.005),
    )
    assert len(background_arrays(good, t0)) == 4

    bad_z = dict(good, z=np.array([10.0, 30.0, 20.0]))
    with pytest.raises(ValueError, match="heights"):
        background_arrays(bad_z, t0)
    bad_t = dict(good, times=good["times"][::-1])
    with pytest.raises(ValueError, match="times"):
        background_arrays(bad_t, t0)
    bad_v = dict(good, theta=np.full((2, 3), np.nan))
    with pytest.raises(ValueError, match="non-finite"):
        background_arrays(bad_v, t0)
    bad_shape = dict(good, q=np.full((3, 3), 0.005))
    with pytest.raises(ValueError, match="shape"):
        background_arrays(bad_shape, t0)


# ----------------------------------------------------------------------
# experiment helpers (pure Python)
# ----------------------------------------------------------------------
def test_tile_forcing_calendar(sample):
    _, df_forcing = sample
    tiled = tile_forcing(df_forcing, [2012, 2013])
    assert tiled.index[0] == df_forcing.index[0]
    assert tiled.index.is_monotonic_increasing
    # 2013 block (positional: the year-2012 block ends at the
    # end-of-interval stamp 2013-01-01 00:00) re-anchored, leap day dropped
    y2 = tiled.iloc[len(df_forcing):]
    assert len(y2) == 288 * 365
    assert str(y2.index[0]) == "2013-01-01 00:05:00"
    assert str(y2.index[-1]) == "2014-01-01 00:00:00"
    assert not ((y2.index.month == 2) & (y2.index.day == 29)).any()


def test_tile_forcing_rejects_unsafe_inputs(sample):
    _, df_forcing = sample
    # partial year
    with pytest.raises(ValueError, match="calendar year"):
        tile_forcing(df_forcing.iloc[: 288 * 60], [2012, 2013])
    # irregular step
    with pytest.raises(ValueError, match="regular"):
        tile_forcing(df_forcing.drop(df_forcing.index[1000]), [2013])
    # non-leap source onto leap target
    non_leap = tile_forcing(df_forcing, [2013])
    with pytest.raises(ValueError, match="leap"):
        tile_forcing(non_leap, [2016])


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


# ----------------------------------------------------------------------
# deterministic page generation
# ----------------------------------------------------------------------
def test_preview_page_is_fresh_and_deterministic():
    script = REPO / "site" / "preview" / "scm" / "build_site.py"
    result = subprocess.run(
        [sys.executable, str(script), "--check"],
        capture_output=True,
        text=True,
        check=False,
    )
    assert result.returncode == 0, (
        f"build_site --check failed (committed page stale or token "
        f"contract breached):\n{result.stdout}\n{result.stderr}"
    )


# ----------------------------------------------------------------------
# native integration
# ----------------------------------------------------------------------
@needs_native
def test_six_hour_run_matches_frozen_baseline(sample):
    config, df_forcing = sample
    fix = json.loads((FIXTURES / "native_regression.json").read_text(encoding="utf-8"))
    window = df_forcing.loc[fix["start"] : fix["end"]]

    res = run_scm(config, window)
    assert isinstance(res, ScmResult)
    assert res.snapshots is None  # not requested

    tair = res.diagnostics["tair_mod"].iloc[::12].to_numpy()
    h_bl = res.diagnostics["h_bl"].iloc[::12].to_numpy()
    qh = res.output["SUEWS"]["QH"].droplevel(0).iloc[::12].to_numpy()

    # tolerances absorb compiler/platform noise, not physics changes
    np.testing.assert_allclose(tair, fix["tair_mod_hourly"], atol=0.05)
    np.testing.assert_allclose(h_bl, fix["h_bl_hourly"], atol=20.0)
    np.testing.assert_allclose(qh, fix["qh_hourly"], atol=2.0)

    diag = res.diagnostics
    assert np.isfinite(diag[["tair_mod", "rh_mod", "u_mod", "h_bl"]]).all().all()
    assert diag["rh_mod"].between(2.0, 100.0).all()
    assert len(res.state_json) > 100


@needs_native
def test_snapshots_and_background_round_trip(sample):
    """Mirrors the documented public example: rural companion snapshots
    feeding the urban column's ventilation."""
    config, df_forcing = sample
    window = df_forcing.loc["2012-07-01 00:05":"2012-07-01 03:00"]

    rural = run_scm(make_rural_config(config), window, snapshot_every_h=1.0)
    assert rural.snapshots is not None
    # snapshot count contract: ceil(len/steps_per_hour), exactly
    assert len(rural.snapshots["times"]) == -(-len(window) // 12)
    assert all(np.isfinite(row).all() for row in rural.snapshots["theta"])

    res = run_scm(config, window, background=make_background(rural.snapshots))
    assert np.isfinite(res.diagnostics["tau_adv"].iloc[-1])
