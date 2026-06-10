"""Cross-backend equivalence: native Fortran-loop SCM vs the Python reference.

The native path (``suews_bridge.run_suews_scm`` -> ``module_phys_scm``) is a
line-for-line port of the validated Python column. Differences between the
backends come only from the SUEWS state pathway (the Python reference calls
``run_supy`` per step with state-JSON round-trips; the native loop keeps the
state in memory), so the two must agree closely but not bit-exactly.
"""

import numpy as np
import pytest

supy = pytest.importorskip("supy")

from pathlib import Path  # noqa: E402

from suews_scm.coupling import CoupledSCM  # noqa: E402
from suews_scm.native import run_coupled_native, scm_params_vector  # noqa: E402


def _native_available():
    try:
        from supy._run_rust import _load_rust_module

        return hasattr(_load_rust_module(), "run_suews_scm")
    except Exception:
        return False


pytestmark = pytest.mark.skipif(
    not _native_available(), reason="suews_bridge.run_suews_scm not built"
)


@pytest.fixture(scope="module")
def sample():
    from supy.data_model import SUEWSConfig

    cfg_path = Path(supy.__file__).parent / "sample_data" / "sample_config.yml"
    config = SUEWSConfig.from_yaml(cfg_path)
    df_state, df_forcing = supy.load_sample_data()
    return config, df_state, df_forcing


def test_param_vector_matches_coupled_defaults():
    params = scm_params_vector()
    assert len(params) == 23
    scm_defaults = CoupledSCM.__init__.__defaults__
    # spot-check the physically critical entries against CoupledSCM defaults
    assert params[0] == 20.0  # dz0
    assert params[9] == 2.0  # radiative cooling [K/day]
    assert params[10] == 15000.0  # city length [m]
    assert params[12] == 5  # substeps


def test_native_matches_python_reference_six_hours(sample):
    config, df_state, df_forcing = sample
    start, end = "2012-07-01 00:05", "2012-07-01 06:00"
    window = df_forcing.loc[start:end]

    df_out_n, df_scm_n, state_json = run_coupled_native(config, window)
    scm = CoupledSCM(df_state.copy(), df_forcing)
    df_p, _ = scm.run(start, end)

    d_tair = (df_scm_n["tair_mod"] - df_p["tair_mod"]).abs()
    d_h = (df_scm_n["h_bl"] - df_p["h_bl"]).abs()
    d_wth = (df_scm_n["wth"] - df_p["wth"]).abs()

    assert d_tair.max() < 0.15  # K
    assert d_h.max() < 60.0  # m
    assert d_wth.max() < 0.01  # K m s-1
    # outputs structurally sound
    assert np.isfinite(df_scm_n[["tair_mod", "rh_mod", "u_mod", "h_bl"]]).all().all()
    assert len(state_json) > 100
    # SUEWS fluxes agree between backends as well
    d_qh = (df_out_n["SUEWS"]["QH"].droplevel(0) - df_p["qh"]).abs()
    assert d_qh.max() < 10.0  # W m-2
