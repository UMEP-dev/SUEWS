"""Shared helpers for the demo cases.

All demos use the KCL (London) SuPy sample, spin SUEWS up offline to
21 July 2012, then run the two-way coupled SCM over 23-26 July (the
clearest summer window; 22 July is column spin-up).
"""

import sys
import warnings
from pathlib import Path

import numpy as np
import pandas as pd

ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT))

warnings.filterwarnings("ignore")

from suews_scm.coupling import CoupledSCM, make_background, spinup_state  # noqa: E402

CACHE = Path(__file__).resolve().parent / "cache"
SPINUP_END = "2012-07-21 23:55"
RUN_START = "2012-07-22 00:05"
RUN_END = "2012-07-27 00:00"
EVAL_START = "2012-07-23"  # exclude column spin-up day


def get_sample_and_state():
    """Sample data plus SUEWS state spun up to late July (cached)."""
    import supy as sp

    df_state0, df_forcing = sp.load_sample_data()
    # pickle is safe here: a local, gitignored cache written and read
    # only by these demo scripts on the same machine (no untrusted data)
    cache_file = CACHE / "state_spun_2012-07-21.pkl"
    if cache_file.exists():
        state = pd.read_pickle(cache_file)
    else:
        CACHE.mkdir(exist_ok=True)
        state = spinup_state(df_state0, df_forcing, SPINUP_END)
        state.to_pickle(cache_file)
    return state, df_forcing


def run_coupled(state, df_forcing, start=RUN_START, end=RUN_END, **scm_kwargs):
    """Run the coupled SCM over the demo episode."""
    scm = CoupledSCM(state.copy(), df_forcing, **scm_kwargs)
    return scm.run(start, end)


def make_rural_state(state):
    """Grass-dominated low-roughness configuration without QF.

    The vegetated soil stores are reset to 80 % of capacity
    ("well-watered grassland"): the spun-up urban state carries the
    depleted store of KCL's 3 % grass patch, which would make the
    rural reference unrealistically dry (daytime Bowen ratio > 3).
    """
    st = set_land_cover(
        state, dict(paved=0.10, grass=0.70, dectr=0.15, bsoil=0.05)
    )
    st = scale_anthropogenic_heat(st, 0.0)
    for c in [c for c in st.columns if c[0] == "z0m_in"]:
        st[c] = 0.1
    for c in [c for c in st.columns if c[0] == "zdm_in"]:
        st[c] = 0.5
    cap_cols = {c[1]: c for c in st.columns if c[0] == "soilstorecap_surf"}
    for c in [c for c in st.columns if c[0] == "soilstore_surf"]:
        if c[1] in cap_cols:
            st[c] = 0.8 * st[cap_cols[c[1]]]
    return st


def get_rural_run(state, df_forcing, start=RUN_START, end=RUN_END):
    """Closed rural coupled run + its background atmosphere (cached).

    The rural column is quasi-closed (clear-sky radiative cooling only);
    its hourly profiles serve as the upstream air that ventilates the
    urban columns.
    """
    df_file = CACHE / "rural_diag.parquet"
    bg_file = CACHE / "rural_background.npz"
    if df_file.exists() and bg_file.exists():
        df_r = pd.read_parquet(df_file)
        with np.load(bg_file) as dat:
            bg = dict(
                times=dat["times"].astype("datetime64[ns]"),
                z=dat["z"], theta=dat["theta"], q=dat["q"],
            )
        return df_r, bg
    df_r, snaps = run_coupled(
        make_rural_state(state), df_forcing, start=start, end=end, z0m_wind=0.1
    )
    bg = make_background(snaps)
    CACHE.mkdir(exist_ok=True)
    df_r.to_parquet(df_file)
    np.savez(
        bg_file,
        times=bg["times"].astype("int64"),
        z=bg["z"], theta=bg["theta"], q=bg["q"],
    )
    return df_r, bg


# ----------------------------------------------------------------------
# scenario editors (SuPy idiom: modify df_state columns directly)
# ----------------------------------------------------------------------
SURF_INDEX = dict(paved=0, bldgs=1, evetr=2, dectr=3, grass=4, bsoil=5, water=6)


def set_land_cover(state, fractions):
    """Set the seven land-cover fractions (must sum to 1)."""
    vals = np.zeros(7)
    for name, frac in fractions.items():
        vals[SURF_INDEX[name]] = frac
    assert abs(vals.sum() - 1.0) < 1.0e-9, "fractions must sum to 1"
    st = state.copy()
    for i, v in enumerate(vals):
        st[("sfr_surf", f"({i},)")] = v
    return st


def scale_anthropogenic_heat(state, factor):
    """Scale the Jarvi et al. (2011) QF coefficients by ``factor``."""
    st = state.copy()
    for var in ("qf_a", "qf_b", "qf_c", "qf0_beu"):
        cols = [c for c in st.columns if c[0] == var]
        for c in cols:
            st[c] = st[c] * factor
    return st


def set_building_albedo(state, albedo):
    """Set the bulk albedo of the building facet (cool-roof scenario)."""
    st = state.copy()
    cols = [c for c in st.columns if c[0] == "alb"]
    target = [c for c in cols if c[1] == f"({SURF_INDEX['bldgs']},)"]
    assert target, "building albedo column not found"
    for c in target:
        st[c] = albedo
    return st
