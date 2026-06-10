"""Native coupled SCM: the column runs inside the Fortran timestep loop.

This is the production backend. The Python :class:`~suews_scm.coupling.CoupledSCM`
remains the validated reference implementation (same physics, same defaults);
here the whole coupled loop -- SUEWS surface, column substeps, wind nudging,
ventilation -- executes in one call through the SUEWS Rust bridge
(``suews_bridge.run_suews_scm`` -> ``suews_scm_multitsteps_c`` ->
``SUEWS_cal_multitsteps_scm`` -> ``module_phys_scm``), eliminating the
per-step marshalling that dominates the Python loop (~0.1 s per 5-min step).

Parameter defaults are kept in lock-step with ``CoupledSCM``; the flat
vector layout matches ``SCM_PARAMS_LEN`` in ``suews_phys_scm.f95``.
"""

import numpy as np
import pandas as pd

# ordered to match the Fortran flat layout (suews_phys_scm.f95)
SCM_PARAM_DEFAULTS = {
    "dz0": 20.0,
    "ztop": 3000.0,
    "stretch": 1.06,
    "h_init": 300.0,
    "gamma_theta": 0.006,
    "gamma_q": -5.0e-7,
    "z_ft_nudge": 1500.0,
    "tau_ft": 86400.0,
    "tau_wind": 1800.0,
    "radiative_cooling": 2.0,
    "city_length": 15000.0,
    "tau_adv_min": 900.0,
    "substeps": 5,
    "z0m_wind": 1.0,
    "lambda_mix": 40.0,
    "ric_stable": 0.2,
    "ric_h": 0.25,
    "k_background": 1.0e-4,
    "cg_a": 7.2,
    "excess_b": 8.5,
    "wstar_fac": 0.6,
    "stable_fn": 0,  # 0 = sharp cut-off, 1 = long tail
    "use_background": 0,  # set automatically when a background is supplied
    # synoptic relaxation of the whole column towards a profile anchored
    # at the observed air state [s; 0 = off]. Used by the rural companion
    # in multi-season runs (a closed column drifts cold through winter);
    # native backend only.
    "obs_anchor_tau": 0.0,
}

SCM_DIAG_COLS = ["tair_mod", "rh_mod", "u_mod", "h_bl", "wth", "wq", "tau_adv"]


def scm_params_vector(**overrides):
    """Flat SCM parameter vector with CoupledSCM-compatible defaults."""
    params = dict(SCM_PARAM_DEFAULTS)
    unknown = set(overrides) - set(params)
    if unknown:
        raise ValueError(f"unknown SCM parameters: {sorted(unknown)}")
    params.update(overrides)
    return [float(params[k]) for k in SCM_PARAM_DEFAULTS]


def background_arrays(background, t0):
    """Flatten a background dict (as from ``make_background``) for the bridge.

    ``t0`` is the first timestamp of the coupled run; background snapshot
    times are converted to seconds since ``t0`` (the Fortran side does a
    floor lookup, matching the Python reference).
    """
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
    """Replicate the column grid generation to predict the level count
    and centre heights (same arithmetic as Grid / scm_column_init)."""
    zi = [0.0]
    dz = dz0
    while zi[-1] < ztop:
        zi.append(zi[-1] + dz)
        dz *= stretch
    z = [0.5 * (a + b) for a, b in zip(zi[:-1], zi[1:])]
    return z


def run_coupled_native(
    config,
    df_forcing,
    state_json=None,
    background=None,
    snapshot_every_h=None,
    **scm_overrides,
):
    """Run the coupled SUEWS-SCM natively in Fortran.

    Parameters
    ----------
    config : supy.data_model.SUEWSConfig or str
        SUEWS configuration (or a ready YAML string).
    df_forcing : pandas.DataFrame
        Forcing for the coupled window (SuPy layout). Radiation,
        precipitation and pressure are prescribed from it; air
        temperature, humidity and wind are prognosed by the column.
    state_json : str, optional
        Initial state from a previous bridge run (spin-up); when omitted
        the config-derived cold-start state is used.
    background : dict, optional
        Background atmosphere (``times``, ``z``, ``theta``, ``q``) from a
        companion rural run; enables the ventilation term.
    snapshot_every_h : float, optional
        Record column theta/q profiles every this many hours; the
        snapshots are returned in the same dict format that
        :meth:`CoupledSCM.run` produces (and that ``make_background``
        consumes), so a native rural run can ventilate a native urban one.
    **scm_overrides
        Any :data:`SCM_PARAM_DEFAULTS` key.

    Returns
    -------
    (pandas.DataFrame, pandas.DataFrame, str) or
    (pandas.DataFrame, pandas.DataFrame, dict, str) when snapshots are on
        Standard SUEWS output (multi-group columns), per-step SCM
        diagnostics, [profile snapshots,] and the final state JSON.
    """
    import yaml as _yaml
    from supy._run_rust import (
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
            f"native SCM length mismatch: forcing={len(df_forcing)}, output={len_sim}"
        )

    df_output = _parse_output_block(output_flat, len_sim, grid_id=1)
    scm_arr = np.asarray(scm_flat, dtype=float).reshape((len_sim, len(SCM_DIAG_COLS)))
    df_scm = pd.DataFrame(scm_arr, columns=SCM_DIAG_COLS, index=df_forcing.index)
    df_scm = df_scm.mask(df_scm == -999.0)

    if snap_every > 0:
        n_snap = -(-len_sim // snap_every)  # ceil division
        snaps = dict(
            times=list(df_forcing.index[::snap_every][:n_snap]),
            z=np.asarray(snap_z, dtype=float),
            theta=list(
                np.asarray(snap_theta, dtype=float).reshape((n_snap, nz_snap))
            ),
            q=list(np.asarray(snap_q, dtype=float).reshape((n_snap, nz_snap))),
        )
        return df_output, df_scm, snaps, state_json_out
    return df_output, df_scm, state_json_out
