"""Two-way coupling between SUEWS (via SuPy) and the atmospheric column.

Each SUEWS time step (typically 5 min):

1. the column state is interpolated to the SUEWS forcing height and
   written into the forcing record (air temperature, relative humidity,
   wind speed) -- radiation, precipitation and pressure remain
   prescribed from the observed forcing file;
2. SUEWS advances one step and returns the surface turbulent fluxes
   (QH, QE) and friction velocity;
3. the kinematic fluxes drive the column over several substeps.

Configurations
--------------
- ``wind_mode="nudge_obs"`` (default): the column wind profile is
  relaxed towards a log-profile anchored at the observed wind speed
  (time scale ``tau_wind``); heat and moisture remain fully prognostic
  (two-way). This is the honest configuration for comparison against
  observations, since a 1-D model cannot generate the synoptic
  pressure gradients that control the real wind.
- ``wind_mode="prognostic"``: fully prognostic momentum with a
  prescribed geostrophic wind -- for idealised experiments
  (urban-rural contrast, scenario studies).

Above ``z_ft_nudge`` the thermodynamic profiles are relaxed towards
their initial state with a long time scale (``tau_ft``), the standard
single-column representation of large-scale advection.

Ventilation
-----------
A fixed column over a city is not a closed system: advection
continuously replaces urban-heated air with upstream (rural) air on a
time scale ``tau_adv = city_length / U``. Without this term the urban
column accumulates the city's persistent (all-night positive) sensible
heat flux and drifts warm by several kelvin per day. The coupled model
therefore supports a ``background`` atmosphere -- typically generated
by a companion *rural* coupled run (closed, with clear-sky radiative
cooling) -- towards which temperature and humidity relax on the
advective time scale. Clear-sky radiative cooling (default 2 K per
day, e.g. Stull 1988) acts on both columns.
"""

import warnings

import numpy as np
import pandas as pd

from .column import ColumnModel
from .constants import CP_AIR, LV
from .grid import Grid
from . import thermo


def make_background(snaps):
    """Package profile snapshots from a coupled run as a background
    atmosphere for the ventilation term of another run."""
    return dict(
        times=np.array(snaps["times"], dtype="datetime64[ns]"),
        z=np.asarray(snaps["z"], dtype=float),
        theta=np.asarray(snaps["theta"], dtype=float),
        q=np.asarray(snaps["q"], dtype=float),
    )


def spinup_state(df_state_init, df_forcing, until):
    """Run SUEWS offline up to ``until`` and return the evolved state."""
    import supy as sp

    df_out, df_state = sp.run_supy(
        df_forcing.loc[:until], df_state_init, logging_level=50
    )
    return df_state


class CoupledSCM:
    """SUEWS land surface two-way coupled to the 1-D column."""

    def __init__(
        self,
        df_state_init,
        df_forcing,
        grid=None,
        wind_mode="nudge_obs",
        ug=8.0,
        vg=0.0,
        f_coriolis=1.15e-4,  # ~52 deg N
        h_init=300.0,
        gamma_theta=0.006,
        gamma_q=-5.0e-7,
        z_ft_nudge=1500.0,
        tau_ft=24.0 * 3600.0,
        tau_wind=1800.0,
        substeps=5,
        z0m_wind=1.0,
        radiative_cooling=2.0,  # K per day, clear-sky longwave divergence
        background=None,  # dict(times, z, theta, q) from a companion run
        city_length=15000.0,  # m, upstream fetch for the advective time scale
        tau_adv_min=900.0,  # s, lower bound on the ventilation time scale
        column_kwargs=None,
    ):
        self.df_state = df_state_init.copy()
        self.df_forcing = df_forcing
        self.wind_mode = wind_mode
        self.tau_wind = tau_wind
        self.substeps = substeps
        self.z0m_wind = z0m_wind
        self.h_init = h_init
        self.gamma_theta = gamma_theta
        self.gamma_q = gamma_q
        self.z_ft_nudge = z_ft_nudge
        self.tau_ft = tau_ft

        self.z_meas = float(np.asarray(df_state_init["z"]).ravel()[0])
        self.tstep = int(
            (df_forcing.index[1] - df_forcing.index[0]).total_seconds()
        )

        self.background = background
        self.city_length = city_length
        self.tau_adv_min = tau_adv_min

        if grid is None:
            grid = Grid(dz0=20.0, ztop=3000.0, stretch=1.06)
        kwargs = dict(column_kwargs or {})
        kwargs.setdefault("z0m", z0m_wind)
        kwargs.setdefault("theta_tendency", -radiative_cooling / 86400.0)
        if wind_mode == "nudge_obs":
            kwargs.setdefault("f_coriolis", 0.0)
        else:
            kwargs.setdefault("f_coriolis", f_coriolis)
            kwargs.setdefault("ug", ug)
            kwargs.setdefault("vg", vg)
        self.column_kwargs = kwargs
        self.grid = grid
        self.col = None

    # ------------------------------------------------------------------
    def _wind_shape(self, z):
        """Log-profile shape function anchored at the forcing height."""
        z0 = self.z0m_wind
        f = np.log((np.minimum(z, 300.0) + z0) / z0) / np.log((self.z_meas + z0) / z0)
        return f

    def _init_column(self, row):
        p_sfc = float(row["pres"]) * 100.0
        t_air = float(row["Tair"]) + 273.15
        rh = float(row["RH"])
        u_obs = max(float(row["U"]), 0.5)

        grid = self.grid
        col = ColumnModel(grid, p_sfc=p_sfc, t_ref=t_air, **self.column_kwargs)

        p_zm = thermo.pressure_profile(p_sfc, t_air, np.array([self.z_meas]))[0]
        theta_m = thermo.theta_from_t(t_air, p_zm)
        q_m = float(thermo.q_from_rh(rh, t_air, p_zm))

        theta = theta_m + self.gamma_theta * np.maximum(grid.z - self.h_init, 0.0)
        q = np.clip(q_m + self.gamma_q * np.maximum(grid.z - self.h_init, 0.0), 1.0e-5, None)
        if self.wind_mode == "nudge_obs":
            u = u_obs * self._wind_shape(grid.z)
            v = np.zeros(grid.n)
        else:
            u = np.full(grid.n, self.column_kwargs.get("ug", 8.0))
            v = np.full(grid.n, self.column_kwargs.get("vg", 0.0))
        col.set_initial_profiles(theta=theta, q=q, u=u, v=v)
        col.set_nudging(self.tau_ft, self.z_ft_nudge, theta_ref=theta.copy(), q_ref=q.copy())
        self.col = col

    def _column_at_zmeas(self):
        col, g = self.col, self.grid
        theta = float(np.interp(self.z_meas, g.z, col.theta))
        q = float(np.interp(self.z_meas, g.z, col.q))
        u = float(np.interp(self.z_meas, g.z, col.u))
        v = float(np.interp(self.z_meas, g.z, col.v))
        p = float(np.interp(self.z_meas, g.z, col.p_levels))
        t_air = thermo.t_from_theta(theta, p)
        rh = float(np.clip(thermo.rh_from_q(q, t_air, p), 2.0, 100.0))
        wind = float(np.clip(np.hypot(u, v), 0.5, None))
        return t_air, rh, wind, q, p

    def _background_profiles(self, when):
        """Background theta/q on the column grid at time ``when``."""
        bg = self.background
        times = bg["times"]
        # latest snapshot at or before `when` (clamp to the first one)
        idx = int(np.searchsorted(times, np.datetime64(when), side="right")) - 1
        idx = max(idx, 0)
        theta_bg = np.interp(self.grid.z, bg["z"], bg["theta"][idx])
        q_bg = np.interp(self.grid.z, bg["z"], bg["q"][idx])
        return theta_bg, q_bg

    def _ventilate(self, dt, theta_bg, q_bg, h_bl):
        """Advective exchange with the background air, tau = L / U."""
        below = self.grid.z <= max(1.2 * h_bl, 800.0)
        u_mean = float(np.mean(np.hypot(self.col.u, self.col.v)[below]))
        tau = max(self.city_length / max(u_mean, 0.5), self.tau_adv_min)
        fac = dt / tau
        self.col.theta += fac * (theta_bg - self.col.theta)
        self.col.q += fac * (q_bg - self.col.q)
        return tau

    # ------------------------------------------------------------------
    def run(self, start, end, snapshot_every_h=1.0):
        """Run the coupled model over ``df_forcing[start:end]``.

        Returns
        -------
        (pandas.DataFrame, dict)
            Per-step diagnostics, and hourly profile snapshots
            (``z``, ``theta``, ``q``, ``times``).
        """
        import supy as sp

        forcing = self.df_forcing.loc[start:end]
        if self.col is None:
            self._init_column(forcing.iloc[0])

        dt_col = self.tstep / self.substeps
        n_snap = max(1, int(snapshot_every_h * 3600 / self.tstep))

        records = []
        snaps = {"times": [], "theta": [], "q": [], "z": self.grid.z.copy()}
        state = self.df_state

        with warnings.catch_warnings():
            warnings.simplefilter("ignore")
            for k in range(len(forcing)):
                row = forcing.iloc[[k]].copy()
                t_air, rh, wind, q_zm, p_zm = self._column_at_zmeas()
                row.loc[:, "Tair"] = t_air - 273.15
                row.loc[:, "RH"] = rh
                row.loc[:, "U"] = wind

                df_out, state = sp.run_supy(row, state, logging_level=50)
                out = df_out["SUEWS"].iloc[0]
                qh, qe = float(out["QH"]), float(out["QE"])
                ustar = max(float(out["UStar"]), 0.05)
                t2 = float(out["T2"])  # SUEWS screen-level diagnostic

                rho = thermo.rho_air(t_air, p_zm, q_zm)
                wth = qh / (rho * CP_AIR)
                wq = qe / (rho * LV)

                u_obs = max(float(forcing["U"].iloc[k]), 0.5)
                u_target = u_obs * self._wind_shape(self.grid.z)
                if self.background is not None:
                    theta_bg, q_bg = self._background_profiles(forcing.index[k])
                tau_adv = np.nan
                for _ in range(self.substeps):
                    diag = self.col.step(dt_col, surface=dict(wth=wth, wq=wq, ustar=ustar))
                    if self.wind_mode == "nudge_obs":
                        fac = dt_col / self.tau_wind
                        self.col.u += fac * (u_target - self.col.u)
                        self.col.v += fac * (0.0 - self.col.v)
                    if self.background is not None:
                        tau_adv = self._ventilate(dt_col, theta_bg, q_bg, diag["h"])

                records.append(
                    dict(
                        time=forcing.index[k],
                        tair_mod=t_air - 273.15,
                        t2_mod=t2,
                        rh_mod=rh,
                        u_mod=wind,
                        tair_obs=float(forcing["Tair"].iloc[k]),
                        rh_obs=float(forcing["RH"].iloc[k]),
                        u_obs=float(forcing["U"].iloc[k]),
                        qh=qh,
                        qe=qe,
                        qn=float(out["QN"]),
                        qs=float(out["QS"]),
                        qf=float(out["QF"]),
                        ustar=ustar,
                        h_bl=diag["h"],
                        wth=wth,
                        wq=wq,
                        tau_adv=tau_adv,
                    )
                )
                if k % n_snap == 0:
                    snaps["times"].append(forcing.index[k])
                    snaps["theta"].append(self.col.theta.copy())
                    snaps["q"].append(self.col.q.copy())

        df = pd.DataFrame.from_records(records).set_index("time")
        self.df_state = state
        return df, snaps
