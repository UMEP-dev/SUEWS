"""Coupled single-column model (SCM) interface — research preview.

This module is the Python surface of the SUEWS coupled single-column
model: a one-dimensional atmospheric boundary-layer column running
*inside* the SUEWS Fortran timestep driver, so that air temperature,
humidity and wind become prognostic rather than prescribed. The model
physics lives entirely in the compiled core
(``src/suews/src/suews_phys_scm.f95``, loop in
``SUEWS_cal_multitsteps_scm``), reached through the Rust bridge in a
single call — six coupled column-years run in under three minutes on a
laptop.

Scientific summary
------------------
- Unstable conditions: first-order non-local K-profile closure within
  the diagnosed boundary layer (Troen & Mahrt 1986), counter-gradient
  term and turbulent Prandtl number after Holtslag & Boville (1993).
- Stable conditions and the free atmosphere: local Blackadar mixing
  length with a Richardson-number stability function (sharp cut-off or
  long tail; use the long tail for multi-season runs — see the
  documentation page for why).
- Boundary-layer height: bulk Richardson number (critical value 0.25)
  with the Vogelezang & Holtslag (1996) shear term and a Troen-Mahrt
  convective thermal excess.
- Ventilation: a fixed column over a city is not a closed system; the
  column may exchange heat and moisture with a background (rural
  companion) atmosphere on the advective time scale tau = L/U.
- Synoptic anchor (``obs_anchor_tau``): whole-column relaxation towards
  a profile anchored at the observed air state, the standard
  single-column representation of large-scale advection; required for
  the rural companion in multi-season runs.

Validation evidence
-------------------
The physics was developed and validated through a pure-Python reference
implementation (GABLS1 stable-boundary-layer case within the LES ranges
of Beare et al. 2006 on every diagnostic; convective growth within 4 %
of the Tennekes 1973 analytic law; five coupled July days over central
London with 3.4 K air-temperature RMSE against observations never shown
to the model). The reference implementation was retired once the native
port was pinned to it (air-temperature agreement <= 0.06 K over a
six-hour coupled window); it remains available in git history — see the
provenance section of :doc:`the documentation page
</integration/scm-coupled>` and the archived figures and metrics under
``docs/source/assets/img/scm/`` and ``test/fixtures/scm/``.

Usage
-----
>>> import supy as sp
>>> from supy.scm import run_scm
>>> from supy.data_model import SUEWSConfig
>>> config = SUEWSConfig.from_yaml("config.yml")
>>> df_state, df_forcing = sp.load_sample_data()
>>> df_output, df_scm, state_json = run_scm(config, df_forcing)

References
----------
Beare et al. (2006) Boundary-Layer Meteorol.; Cuxart et al. (2006)
Boundary-Layer Meteorol.; Holtslag & Boville (1993) J. Climate;
Onomura et al. (2015) Urban Climate 11, 1-23 (the BLUEWS lineage);
Tennekes (1973) J. Atmos. Sci.; Troen & Mahrt (1986) Boundary-Layer
Meteorol.; Vogelezang & Holtslag (1996) Boundary-Layer Meteorol.
"""

import numpy as np
import pandas as pd

# Flat parameter vector, ordered to match SCM_PARAMS_LEN in
# suews_phys_scm.f95 — keep the two in lock-step.
SCM_PARAM_DEFAULTS = {
    "dz0": 20.0,  # [m] first-layer thickness
    "ztop": 3000.0,  # [m] column top
    "stretch": 1.06,  # [-] grid stretching factor
    "h_init": 300.0,  # [m] initial well-mixed depth
    "gamma_theta": 0.006,  # [K m-1] free-atmosphere lapse rate
    "gamma_q": -5.0e-7,  # [kg kg-1 m-1] humidity lapse rate
    "z_ft_nudge": 1500.0,  # [m] free-troposphere nudging base
    "tau_ft": 86400.0,  # [s] free-troposphere nudging time scale
    "tau_wind": 1800.0,  # [s] wind nudging time scale
    "radiative_cooling": 2.0,  # [K day-1] clear-sky longwave divergence
    "city_length": 15000.0,  # [m] upstream fetch for ventilation
    "tau_adv_min": 900.0,  # [s] lower bound on the ventilation time scale
    "substeps": 5,  # [-] column substeps per SUEWS step
    "z0m_wind": 1.0,  # [m] roughness for the wind-nudge log profile
    "lambda_mix": 40.0,  # [m] Blackadar asymptotic mixing length
    "ric_stable": 0.2,  # [-] critical Ri of the stable closure
    "ric_h": 0.25,  # [-] critical bulk Ri for BL height
    "k_background": 1.0e-4,  # [m2 s-1] background diffusivity
    "cg_a": 7.2,  # [-] counter-gradient coefficient (HB93)
    "excess_b": 8.5,  # [-] convective thermal excess coefficient
    "wstar_fac": 0.6,  # [-] w* weighting in the velocity scale
    "stable_fn": 0,  # 0 = sharp cut-off, 1 = long tail (multi-season)
    "use_background": 0,  # set automatically when a background is supplied
    "obs_anchor_tau": 0.0,  # [s] synoptic anchor; 0 = off
}

SCM_DIAG_COLS = ["tair_mod", "rh_mod", "u_mod", "h_bl", "wth", "wq", "tau_adv"]


def scm_params_vector(**overrides):
    """Flat SCM parameter vector in the Fortran layout."""
    params = dict(SCM_PARAM_DEFAULTS)
    unknown = set(overrides) - set(params)
    if unknown:
        raise ValueError(f"unknown SCM parameters: {sorted(unknown)}")
    params.update(overrides)
    return [float(params[k]) for k in SCM_PARAM_DEFAULTS]


def make_background(snaps):
    """Package profile snapshots from one run as the ventilating
    background atmosphere for another (rural companion -> urban)."""
    return dict(
        times=np.array(snaps["times"], dtype="datetime64[ns]"),
        z=np.asarray(snaps["z"], dtype=float),
        theta=np.asarray(snaps["theta"], dtype=float),
        q=np.asarray(snaps["q"], dtype=float),
    )


def background_arrays(background, t0):
    """Flatten a background dict for the bridge (times as seconds since
    ``t0``, the first timestamp of the coupled run; the Fortran side does
    a floor lookup in time and linear interpolation in height)."""
    times = pd.DatetimeIndex(background["times"])
    t_sec = ((times - pd.Timestamp(t0)).total_seconds()).to_numpy(dtype=float)
    z = np.asarray(background["z"], dtype=float)
    theta = np.asarray(background["theta"], dtype=float)
    q = np.asarray(background["q"], dtype=float)
    if theta.shape != (t_sec.size, z.size) or q.shape != theta.shape:
        raise ValueError(
            f"background shape mismatch: times={t_sec.size}, z={z.size}, "
            f"theta={theta.shape}, q={q.shape}"
        )
    return (
        t_sec.tolist(),
        z.tolist(),
        theta.ravel(order="C").tolist(),
        q.ravel(order="C").tolist(),
    )


def _grid_levels(dz0, ztop, stretch):
    """Replicate the Fortran grid generation (same float arithmetic) to
    predict the column level count for snapshot buffers."""
    zi = [0.0]
    dz = dz0
    while zi[-1] < ztop:
        zi.append(zi[-1] + dz)
        dz *= stretch
    return [0.5 * (a + b) for a, b in zip(zi[:-1], zi[1:])]


def run_scm(
    config,
    df_forcing,
    state_json=None,
    background=None,
    snapshot_every_h=None,
    **scm_overrides,
):
    """Run SUEWS two-way coupled to the single-column boundary layer.

    Each SUEWS step, the column state at the forcing height replaces the
    air temperature, humidity and wind in the forcing (radiation,
    precipitation and pressure stay prescribed), and the SUEWS turbulent
    fluxes drive the column substeps in return. The whole loop executes
    in the compiled core; this function only marshals data.

    Parameters
    ----------
    config : supy.data_model.SUEWSConfig or str
        SUEWS configuration (or a ready YAML string).
    df_forcing : pandas.DataFrame
        Forcing for the coupled window (SuPy layout).
    state_json : str, optional
        Initial state from a previous bridge run (spin-up); when omitted
        the config-derived cold-start state is used.
    background : dict, optional
        Background atmosphere (``times``, ``z``, ``theta``, ``q``) from a
        companion run; enables the ventilation term (tau = city_length/U).
    snapshot_every_h : float, optional
        Record column theta/q profiles every this many hours and return
        them (the dict feeds :func:`make_background`).
    **scm_overrides
        Any :data:`SCM_PARAM_DEFAULTS` key.

    Returns
    -------
    (df_output, df_scm, state_json) or (df_output, df_scm, snaps, state_json)
        Standard SUEWS output, per-step SCM diagnostics
        (:data:`SCM_DIAG_COLS`), [profile snapshots,] final state JSON.
    """
    import yaml as _yaml

    from ._run_rust import (
        _check_rust_available,
        _parse_output_block,
        _prepare_forcing_block,
        _validate_output_layout,
        _yaml_Dumper,
    )

    rust = _check_rust_available()
    if not hasattr(rust, "run_suews_scm"):
        raise RuntimeError(
            "suews_bridge.run_suews_scm not available - rebuild with `make dev`"
        )
    _validate_output_layout(rust)

    if isinstance(config, str):
        config_yaml = config
    else:
        config_yaml = _yaml.dump(
            config.model_dump(exclude_none=True, mode="json"),
            default_flow_style=False,
            sort_keys=False,
            Dumper=_yaml_Dumper,
        )

    if background is not None:
        scm_overrides.setdefault("use_background", 1)
        bg_t, bg_z, bg_theta, bg_q = background_arrays(background, df_forcing.index[0])
    else:
        bg_t, bg_z, bg_theta, bg_q = [], [], [], []

    params = scm_params_vector(**scm_overrides)
    forcing_flat = _prepare_forcing_block(df_forcing).ravel(order="C").tolist()

    snap_every = 0
    nz_snap = 0
    if snapshot_every_h is not None:
        tstep = int((df_forcing.index[1] - df_forcing.index[0]).total_seconds())
        snap_every = max(int(snapshot_every_h * 3600 / tstep), 1)
        p = dict(SCM_PARAM_DEFAULTS)
        p.update({k: scm_overrides[k] for k in scm_overrides if k in p})
        nz_snap = len(_grid_levels(p["dz0"], p["ztop"], p["stretch"]))

    output_flat, scm_flat, snap_z, snap_theta, snap_q, state_json_out, len_sim = (
        rust.run_suews_scm(
            config_yaml,
            forcing_flat,
            len(df_forcing),
            params,
            bg_t,
            bg_z,
            bg_theta,
            bg_q,
            state_json=state_json,
            snap_every=snap_every,
            nz_snap=nz_snap,
        )
    )
    if len_sim != len(df_forcing):
        raise RuntimeError(
            f"SCM length mismatch: forcing={len(df_forcing)}, output={len_sim}"
        )

    df_output = _parse_output_block(output_flat, len_sim, grid_id=1)
    scm_arr = np.asarray(scm_flat, dtype=float).reshape((len_sim, len(SCM_DIAG_COLS)))
    df_scm = pd.DataFrame(scm_arr, columns=SCM_DIAG_COLS, index=df_forcing.index)
    df_scm = df_scm.mask(df_scm == -999.0)

    if snap_every > 0:
        n_snap = -(-len_sim // snap_every)
        snaps = dict(
            times=list(df_forcing.index[::snap_every][:n_snap]),
            z=np.asarray(snap_z, dtype=float),
            theta=list(np.asarray(snap_theta, dtype=float).reshape((n_snap, nz_snap))),
            q=list(np.asarray(snap_q, dtype=float).reshape((n_snap, nz_snap))),
        )
        return df_output, df_scm, snaps, state_json_out
    return df_output, df_scm, state_json_out


# ----------------------------------------------------------------------
# experiment helpers
# ----------------------------------------------------------------------
def tile_forcing(df_forcing, years):
    """Recycle a forcing year over several calendar years (multi-year
    experiments with a single observed year; leap day handled)."""
    blocks = []
    for year in years:
        df = df_forcing.copy()
        leap = (year % 4 == 0) and (year % 100 != 0 or year % 400 == 0)
        if not leap:
            df = df[~((df.index.month == 2) & (df.index.day == 29))]
        idx = pd.date_range(start=f"{year}-01-01 00:05", periods=len(df), freq="5min")
        df = df.set_axis(idx)
        blocks.append(df)
    return pd.concat(blocks)


def _set(obj, attr, value):
    cur = getattr(obj, attr)
    if hasattr(cur, "value"):
        cur.value = value
    else:
        setattr(obj, attr, value)


def _scale(obj, attr, factor):
    cur = getattr(obj, attr)
    if hasattr(cur, "working_day"):  # DayProfile
        for day in ("working_day", "holiday"):
            _scale(cur, day, factor)
        return
    tgt = cur.value if hasattr(cur, "value") else cur
    scaled = [v * factor for v in tgt] if isinstance(tgt, (list, tuple)) else tgt * factor
    if hasattr(cur, "value"):
        cur.value = scaled
    else:
        setattr(obj, attr, scaled)


def make_rural_config(config):
    """Derive a grass-dominated, low-roughness, no-QF, well-watered
    configuration from an urban one — the rural companion whose column
    ventilates the urban column. For multi-season runs give the rural
    run ``obs_anchor_tau=86400`` and both runs ``stable_fn=1``."""
    cfg = config.model_copy(deep=True)
    site = cfg.sites[0]
    lc = site.properties.land_cover
    fractions = dict(
        paved=0.10, bldgs=0.0, evetr=0.0, dectr=0.15, grass=0.70, bsoil=0.05, water=0.0
    )
    for name, frac in fractions.items():
        _set(getattr(lc, name), "sfr", frac)
    _set(site.properties, "z0m_in", 0.1)
    _set(site.properties, "zdm_in", 0.5)
    heat = site.properties.anthropogenic_emissions.heat
    for attr in ("qf_a", "qf_b", "qf_c", "qf0_beu"):
        _scale(heat, attr, 0.0)
    for name in ("grass", "dectr", "evetr", "bsoil"):
        cap = getattr(lc, name).soil_store_capacity
        cap_val = cap.value if hasattr(cap, "value") else cap
        _set(getattr(site.initial_states, name), "soilstore", 0.8 * float(cap_val))
    return cfg
