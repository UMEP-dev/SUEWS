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

What is prescribed and what is prognostic
-----------------------------------------
Air temperature and humidity are fully prognostic. Wind is relaxed
towards a log profile anchored at the observed speed (a 1-D column
cannot generate the synoptic pressure gradient). Radiation,
precipitation and pressure remain prescribed from the forcing file.

Validation evidence
-------------------
The physics was developed and validated through a pure-Python reference
implementation (GABLS1 stable-boundary-layer case within the LES ranges
of Beare et al. 2006 on every diagnostic; convective growth within 4 %
of the Tennekes 1973 analytic law; five coupled July days over central
London with 3.4 K air-temperature RMSE, the air-temperature
observations withheld after initialisation). The reference
implementation was retired once the native port was pinned to it
(air-temperature agreement <= 0.06 K over a six-hour coupled window;
over five free-running days the trajectories agree to 0.23 K mean with
transient divergence up to 2.5 K at morning transitions, while skill
against observations is statistically indistinguishable). It remains
available in git history — see the provenance section of :doc:`the
documentation page </integration/scm-coupled>` and the archived figures
and metrics under ``docs/source/assets/img/scm/`` and
``test/fixtures/scm/``.

Usage
-----
>>> from pathlib import Path
>>> import supy as sp
>>> from supy.scm import run_scm
>>> from supy.data_model import SUEWSConfig
>>> config = SUEWSConfig.from_yaml(
...     Path(sp.__file__).parent / "sample_data" / "sample_config.yml"
... )  # or your own configuration file
>>> df_state, df_forcing = sp.load_sample_data()  # or your own forcing
>>> res = run_scm(config, df_forcing.loc["2012-07-01":"2012-07-03"])
>>> res.output["SUEWS"]["QH"]  # standard SUEWS output
>>> res.diagnostics["tair_mod"]  # prognostic air temperature

References
----------
Beare et al. (2006) Boundary-Layer Meteorol.; Cuxart et al. (2006)
Boundary-Layer Meteorol.; Holtslag & Boville (1993) J. Climate;
Onomura et al. (2015) Urban Climate 11, 1-23 (the BLUEWS lineage);
Tennekes (1973) J. Atmos. Sci.; Troen & Mahrt (1986) Boundary-Layer
Meteorol.; Vogelezang & Holtslag (1996) Boundary-Layer Meteorol.
"""

import math
from dataclasses import dataclass
from typing import Optional

import numpy as np
import pandas as pd

# ----------------------------------------------------------------------
# parameter surface
# ----------------------------------------------------------------------
# Public, user-overridable parameters. ``use_background`` is part of the
# Fortran vector but is NOT public: it is derived from whether a
# validated background atmosphere is supplied (review finding: exposing
# it allowed inconsistent runs such as background data without the flag).
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
    "obs_anchor_tau": 0.0,  # [s] synoptic anchor; 0 = off
}

# Fortran flat-vector layout (keep in lock-step with SCM_PARAMS_LEN and
# the layout comment in suews_phys_scm.f95). ``use_background`` is
# internal and injected by :func:`_params_vector`.
_PARAM_ORDER = [
    "dz0", "ztop", "stretch", "h_init", "gamma_theta", "gamma_q",
    "z_ft_nudge", "tau_ft", "tau_wind", "radiative_cooling",
    "city_length", "tau_adv_min", "substeps", "z0m_wind", "lambda_mix",
    "ric_stable", "ric_h", "k_background", "cg_a", "excess_b",
    "wstar_fac", "stable_fn", "use_background", "obs_anchor_tau",
]

_MAX_GRID_LEVELS = 500

# YAML surface (data_model.core.scm.SCMConfig field) -> internal parameter
# name. The YAML names follow the SUEWS naming convention; the internal
# names match the Fortran parameter-vector layout.
_CONFIG_FIELD_MAP = {
    "height_mixed_layer_init": "h_init",
    "lapse_rate_theta": "gamma_theta",
    "lapse_rate_humidity": "gamma_q",
    "height_nudging_free_atmosphere": "z_ft_nudge",
    "timescale_nudging_free_atmosphere": "tau_ft",
    "timescale_wind_nudging": "tau_wind",
    "rate_radiative_cooling": "radiative_cooling",
    "length_city": "city_length",
    "timescale_ventilation_min": "tau_adv_min",
    "timescale_obs_anchor": "obs_anchor_tau",
    "count_substeps": "substeps",
    "z0m_wind_profile": "z0m_wind",
}

SCM_DIAG_COLS = ["tair_mod", "rh_mod", "u_mod", "h_bl", "wth", "wq", "tau_adv"]


def overrides_from_config(scm_config):
    """Translate a ``model.physics.scm`` configuration block (the user-facing YAML
    surface, :class:`supy.data_model.SCMConfig`) into the internal
    parameter names accepted by :func:`run_scm`."""
    out = {}
    stab = scm_config.stability
    stab = stab.value if hasattr(stab, "value") else str(stab)
    out["stable_fn"] = 1 if stab == "long_tail" else 0
    out["dz0"] = float(scm_config.grid.thickness_first_layer)
    out["ztop"] = float(scm_config.grid.height_top)
    out["stretch"] = float(scm_config.grid.ratio_stretch)
    for yaml_name, internal in _CONFIG_FIELD_MAP.items():
        out[internal] = getattr(scm_config, yaml_name)
    return out


@dataclass
class ScmResult:
    """Stable return contract of :func:`run_scm`.

    Attributes
    ----------
    output : pandas.DataFrame
        Standard SUEWS output (multi-group columns).
    diagnostics : pandas.DataFrame
        Per-step SCM diagnostics (:data:`SCM_DIAG_COLS`).
    snapshots : dict or None
        Hourly column profiles (``times``, ``z``, ``theta``, ``q``) when
        ``snapshot_every_h`` was set; ``None`` otherwise. Feeds
        :func:`make_background`.
    state_json : str
        Final SUEWS state, accepted by a subsequent run's
        ``state_json`` argument.
    """

    output: pd.DataFrame
    diagnostics: pd.DataFrame
    snapshots: Optional[dict]
    state_json: str


def _validate_params(params):
    """Range/finiteness validation mirrored by the Fortran-side validator."""

    def _finite(name, lo=None, hi=None, lo_open=False):
        v = float(params[name])
        if not math.isfinite(v):
            raise ValueError(f"SCM parameter {name} must be finite, got {v}")
        if lo is not None and (v <= lo if lo_open else v < lo):
            cmp = ">" if lo_open else ">="
            raise ValueError(f"SCM parameter {name} must be {cmp} {lo}, got {v}")
        if hi is not None and v > hi:
            raise ValueError(f"SCM parameter {name} must be <= {hi}, got {v}")
        return v

    dz0 = _finite("dz0", 0.0, lo_open=True)
    ztop = _finite("ztop", 0.0, lo_open=True)
    if ztop <= dz0:
        raise ValueError(f"SCM parameter ztop ({ztop}) must exceed dz0 ({dz0})")
    _finite("stretch", 1.0, 1.5)
    _finite("h_init", 0.0, lo_open=True)
    _finite("gamma_theta", 0.0, 0.1, lo_open=True)
    gq = float(params["gamma_q"])
    if not math.isfinite(gq) or abs(gq) > 1.0e-3:
        raise ValueError(f"SCM parameter gamma_q out of range: {gq}")
    _finite("z_ft_nudge", 0.0)
    _finite("tau_ft", 0.0, lo_open=True)
    _finite("tau_wind", 0.0, lo_open=True)
    _finite("radiative_cooling", 0.0)
    _finite("city_length", 0.0, lo_open=True)
    _finite("tau_adv_min", 0.0, lo_open=True)
    substeps = params["substeps"]
    if int(substeps) != substeps or not (1 <= int(substeps) <= 100):
        raise ValueError(f"SCM parameter substeps must be an integer in [1, 100], got {substeps}")
    _finite("z0m_wind", 0.0, lo_open=True)
    _finite("lambda_mix", 0.0, lo_open=True)
    _finite("ric_stable", 0.0, 10.0, lo_open=True)
    _finite("ric_h", 0.0, 10.0, lo_open=True)
    _finite("k_background", 0.0)
    _finite("cg_a", 0.0)
    _finite("excess_b", 0.0)
    _finite("wstar_fac", 0.0)
    if params["stable_fn"] not in (0, 1):
        raise ValueError(f"SCM parameter stable_fn must be 0 or 1, got {params['stable_fn']}")
    _finite("obs_anchor_tau", 0.0)

    n_levels = len(_grid_levels(dz0, ztop, float(params["stretch"])))
    if not (4 <= n_levels <= _MAX_GRID_LEVELS):
        raise ValueError(
            f"SCM grid (dz0={dz0}, ztop={ztop}, stretch={params['stretch']}) "
            f"gives {n_levels} levels; expected 4 to {_MAX_GRID_LEVELS}"
        )


def _params_vector(overrides, use_background):
    """Validated flat parameter vector in the Fortran layout."""
    if "use_background" in overrides:
        raise ValueError(
            "use_background is not user-settable; it is derived from the "
            "`background` argument of run_scm()"
        )
    unknown = set(overrides) - set(SCM_PARAM_DEFAULTS)
    if unknown:
        raise ValueError(f"unknown SCM parameters: {sorted(unknown)}")
    params = dict(SCM_PARAM_DEFAULTS)
    params.update(overrides)
    _validate_params(params)
    params["use_background"] = 1 if use_background else 0
    return [float(params[k]) for k in _PARAM_ORDER]


def scm_params_vector(**overrides):
    """Validated flat SCM parameter vector (no background; for inspection
    and for the cross-layer layout tests)."""
    return _params_vector(overrides, use_background=False)


def _grid_levels(dz0, ztop, stretch):
    """Replicate the Fortran grid generation (same float arithmetic) to
    predict the column level count for snapshot buffers."""
    zi = [0.0]
    dz = dz0
    while zi[-1] < ztop:
        zi.append(zi[-1] + dz)
        dz *= stretch
        if len(zi) > _MAX_GRID_LEVELS + 1:
            raise ValueError(
                f"SCM grid exceeds {_MAX_GRID_LEVELS} levels "
                f"(dz0={dz0}, ztop={ztop}, stretch={stretch})"
            )
    return [0.5 * (a + b) for a, b in zip(zi[:-1], zi[1:])]


# ----------------------------------------------------------------------
# background atmosphere
# ----------------------------------------------------------------------
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
    """Validate and flatten a background dict for the bridge.

    Times become seconds since ``t0`` (the first timestamp of the
    coupled run); the Fortran side does a floor lookup in time and
    linear interpolation in height. Raises ``ValueError`` on any
    structural defect rather than letting the kernel run ventilated by
    garbage.
    """
    times = pd.DatetimeIndex(background["times"])
    t_sec = ((times - pd.Timestamp(t0)).total_seconds()).to_numpy(dtype=float)
    z = np.asarray(background["z"], dtype=float)
    theta = np.asarray(background["theta"], dtype=float)
    q = np.asarray(background["q"], dtype=float)

    if t_sec.size < 1 or z.size < 2:
        raise ValueError(
            f"background needs >=1 time and >=2 levels, got {t_sec.size} x {z.size}"
        )
    if theta.shape != (t_sec.size, z.size) or q.shape != theta.shape:
        raise ValueError(
            f"background shape mismatch: times={t_sec.size}, z={z.size}, "
            f"theta={theta.shape}, q={q.shape}"
        )
    if not np.all(np.diff(t_sec) > 0):
        raise ValueError("background times must be strictly increasing")
    if not np.all(np.diff(z) > 0):
        raise ValueError("background heights must be strictly increasing")
    if not (np.isfinite(theta).all() and np.isfinite(q).all()):
        raise ValueError("background theta/q contain non-finite values")
    return (
        t_sec.tolist(),
        z.tolist(),
        theta.ravel(order="C").tolist(),
        q.ravel(order="C").tolist(),
    )


# ----------------------------------------------------------------------
# the coupled run
# ----------------------------------------------------------------------
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
        companion run; validated, then enables the ventilation term
        (tau = city_length / U).
    snapshot_every_h : float, optional
        Record column theta/q profiles every this many hours and return
        them in ``ScmResult.snapshots``.
    **scm_overrides
        Any :data:`SCM_PARAM_DEFAULTS` key; values are validated.

    Notes
    -----
    Parameter precedence: documented defaults < the ``model.physics.scm``
    block of the configuration (when ``config`` is a
    :class:`~supy.data_model.SUEWSConfig` carrying one) < keyword
    overrides. When ``config`` is given as a raw YAML string the
    ``model.physics.scm`` block is not interpreted; use keyword overrides.

    Returns
    -------
    ScmResult
        Stable named result: ``output``, ``diagnostics``, ``snapshots``
        (``None`` unless requested), ``state_json``.
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
        # the user-facing model.physics.scm block supplies parameter
        # defaults; explicit keyword overrides win
        model_block = getattr(config, "model", None)
        scm_block = getattr(getattr(model_block, "physics", None), "scm", None)
        if scm_block is not None:
            scm_overrides = {**overrides_from_config(scm_block), **scm_overrides}

    if background is not None:
        bg_t, bg_z, bg_theta, bg_q = background_arrays(background, df_forcing.index[0])
    else:
        bg_t, bg_z, bg_theta, bg_q = [], [], [], []

    params = _params_vector(scm_overrides, use_background=background is not None)
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

    snaps = None
    if snap_every > 0:
        n_snap = -(-len_sim // snap_every)
        snaps = dict(
            times=list(df_forcing.index[::snap_every][:n_snap]),
            z=np.asarray(snap_z, dtype=float),
            theta=list(np.asarray(snap_theta, dtype=float).reshape((n_snap, nz_snap))),
            q=list(np.asarray(snap_q, dtype=float).reshape((n_snap, nz_snap))),
        )
    return ScmResult(
        output=df_output,
        diagnostics=df_scm,
        snapshots=snaps,
        state_json=state_json_out,
    )


# ----------------------------------------------------------------------
# experiment helpers
# ----------------------------------------------------------------------
def tile_forcing(df_forcing, years):
    """Recycle one full forcing year over several calendar years.

    The input must be a regular, gap-free record covering exactly one
    calendar year with SUEWS end-of-interval timestamps (first record at
    ``YYYY-01-01 00:00 + step``, last at ``(YYYY+1)-01-01 00:00``).
    Anything else is rejected rather than silently re-stamped — partial
    years, irregular steps or multi-year inputs would otherwise produce
    plausible-looking but miscalendared experiments.

    The leap day is dropped when tiling onto a non-leap target year. A
    leap *target* year cannot be built from a non-leap source (the model
    would need a 29 February that does not exist) and is rejected.
    """
    idx = df_forcing.index
    if len(idx) < 2:
        raise ValueError("forcing must contain at least two records")
    steps = pd.unique(np.diff(idx.values))
    if len(steps) != 1:
        raise ValueError("forcing index must be regular (single time step, no gaps)")
    freq = pd.Timedelta(steps[0])
    if freq <= pd.Timedelta(0) or (pd.Timedelta(days=1) % freq) != pd.Timedelta(0):
        raise ValueError(f"forcing step {freq} must be positive and divide one day")

    y0 = idx[0].year
    src_leap = pd.Timestamp(f"{y0}-12-31").is_leap_year
    expected_start = pd.Timestamp(f"{y0}-01-01") + freq
    expected_end = pd.Timestamp(f"{y0 + 1}-01-01")
    if idx[0] != expected_start or idx[-1] != expected_end:
        raise ValueError(
            "forcing must cover exactly one calendar year with end-of-interval "
            f"timestamps ({expected_start} .. {expected_end}); "
            f"got {idx[0]} .. {idx[-1]}"
        )

    blocks = []
    for year in years:
        tgt_leap = pd.Timestamp(f"{year}-12-31").is_leap_year
        df = df_forcing.copy()
        if src_leap and not tgt_leap:
            df = df[~((df.index.month == 2) & (df.index.day == 29))]
        elif tgt_leap and not src_leap:
            raise ValueError(
                f"cannot tile a non-leap source year ({y0}) onto leap year {year}"
            )
        new_idx = pd.date_range(
            start=pd.Timestamp(f"{year}-01-01") + freq, periods=len(df), freq=freq
        )
        blocks.append(df.set_axis(new_idx))
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
