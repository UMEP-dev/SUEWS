"""Multi-level single-column boundary-layer model.

Prognostic variables: potential temperature ``theta``, specific humidity
``q``, horizontal wind ``u``, ``v`` on a stretched grid (cell centres);
turbulent fluxes and eddy diffusivities live on cell interfaces.

Closure
-------
- Unstable conditions: first-order non-local K-profile within the
  diagnosed boundary layer (Troen & Mahrt 1986), with the
  counter-gradient correction and turbulent Prandtl number of
  Holtslag & Boville (1993).
- Stable / neutral conditions and the free atmosphere: local mixing
  length closure (Blackadar 1962) with a Richardson-number stability
  function; the default "sharp" cut-off form
  ``f = (1 - Ri/Ric)^2``, Ric = 0.2 follows the recommendation of the
  GABLS1 intercomparison that long-tailed operational functions
  over-deepen the stable boundary layer (Cuxart et al. 2006).
- Boundary-layer height: bulk Richardson number method with the
  Vogelezang & Holtslag (1996) shear term (100 u*^2) and a convective
  thermal excess (Troen & Mahrt 1986).

Numerics
--------
Operator-split per time step: (1) surface fluxes (prescribed, or from
Monin-Obukhov similarity given a surface temperature); (2) eddy
diffusivity profiles; (3) backward-Euler implicit vertical diffusion in
conservative flux form (exactly conserves column integrals up to the
prescribed surface flux); (4) exact rotation for the Coriolis term
about the geostrophic wind; (5) optional subsidence (first-order
upwind), prescribed clear-air radiative tendency, and relaxation
(nudging) towards reference profiles.
"""

import numpy as np

from .constants import GRAV, VONK
from .grid import Grid
from .surface import most_fluxes
from .thermo import pressure_profile, exner
from .tridiag import solve_tridiag


class ColumnModel:
    """1-D atmospheric column with first-order K closure."""

    def __init__(
        self,
        grid: Grid,
        f_coriolis=1.0e-4,
        ug=0.0,
        vg=0.0,
        z0m=0.1,
        z0h=None,
        p_sfc=1.0e5,
        t_ref=285.0,
        lambda_mix=40.0,
        ric_stable=0.2,
        ric_h=0.25,
        stable_fn="sharp",
        k_background=1.0e-4,
        counter_gradient_a=7.2,
        thermal_excess_b=8.5,
        wstar_fac=0.6,
        subsidence_divergence=0.0,
        theta_tendency=0.0,
    ):
        self.grid = grid
        self.f = f_coriolis
        self.ug = ug
        self.vg = vg
        self.z0m = z0m
        self.z0h = z0h if z0h is not None else z0m
        self.p_sfc = p_sfc
        self.lambda_mix = lambda_mix
        self.ric_stable = ric_stable
        self.ric_h = ric_h
        self.stable_fn = stable_fn
        self.k_background = k_background
        self.cg_a = counter_gradient_a
        self.excess_b = thermal_excess_b
        self.wstar_fac = wstar_fac
        self.div = subsidence_divergence
        self.theta_tendency = theta_tendency

        self.p_levels = pressure_profile(p_sfc, t_ref, grid.z)
        self.exner_levels = exner(self.p_levels)

        n = grid.n
        self.theta = np.full(n, 288.0)
        self.q = np.zeros(n)
        self.u = np.zeros(n)
        self.v = np.zeros(n)

        # nudging configuration (off by default)
        self.nudge_tau = None
        self.nudge_above = None
        self.theta_ref = None
        self.q_ref = None
        self.uv_ref = None

        # last-step diagnostics for inspection / plotting
        self.k_m_last = np.zeros(n + 1)
        self.k_h_last = np.zeros(n + 1)
        self.h_last = grid.z[0]

    # ------------------------------------------------------------------
    def set_initial_profiles(self, theta, q=None, u=None, v=None):
        """Set initial cell-centre profiles."""
        self.theta = np.array(theta, dtype=float)
        if q is not None:
            self.q = np.array(q, dtype=float)
        if u is not None:
            self.u = np.array(u, dtype=float)
        if v is not None:
            self.v = np.array(v, dtype=float)

    def set_nudging(self, tau, above, theta_ref=None, q_ref=None, uv_ref=None):
        """Relax profiles towards references above a given height.

        Standard single-column practice to represent large-scale
        advection that a 1-D model cannot generate itself.
        """
        self.nudge_tau = tau
        self.nudge_above = above
        self.theta_ref = None if theta_ref is None else np.array(theta_ref, dtype=float)
        self.q_ref = None if q_ref is None else np.array(q_ref, dtype=float)
        self.uv_ref = uv_ref

    # ------------------------------------------------------------------
    def _theta_v(self):
        return self.theta * (1.0 + 0.61 * self.q)

    def _diagnose_h(self, ustar, wthv_s):
        """Boundary-layer height by the bulk Richardson number method."""
        z = self.grid.z
        thv = self._theta_v()
        du2 = (self.u - self.u[0]) ** 2 + (self.v - self.v[0]) ** 2 + 100.0 * ustar**2

        def _scan(thv_ref):
            rib = (GRAV / thv_ref) * (thv - thv_ref) * z / np.maximum(du2, 0.01)
            above = np.nonzero(rib >= self.ric_h)[0]
            if above.size == 0:
                return z[-1]
            k = above[0]
            if k == 0:
                return z[0]
            # linear interpolation in Ri between k-1 and k
            r0, r1 = rib[k - 1], rib[k]
            w = (self.ric_h - r0) / max(r1 - r0, 1.0e-12)
            return z[k - 1] + w * (z[k] - z[k - 1])

        h = _scan(thv[0])
        if wthv_s > 1.0e-6:
            wstar = (GRAV / thv[0] * wthv_s * max(h, 10.0)) ** (1.0 / 3.0)
            w_m = (ustar**3 + self.wstar_fac * wstar**3) ** (1.0 / 3.0)
            thv_ref = thv[0] + self.excess_b * wthv_s / max(w_m, 0.01)
            h = _scan(thv_ref)
        return max(h, self.grid.z[0])

    def _local_k(self):
        """Local mixing-length diffusivities at internal interfaces."""
        g = self.grid
        zi = g.zi[1:-1]
        thv = self._theta_v()
        dthv = np.diff(thv) / g.dzc
        du = np.diff(self.u) / g.dzc
        dv = np.diff(self.v) / g.dzc
        shear2 = du**2 + dv**2 + 1.0e-8
        thv_i = 0.5 * (thv[:-1] + thv[1:])
        n2 = GRAV / thv_i * dthv
        ri = n2 / shear2

        lmix = 1.0 / (1.0 / (VONK * zi) + 1.0 / self.lambda_mix)
        if self.stable_fn == "sharp":
            f_stab = np.clip(1.0 - ri / self.ric_stable, 0.0, None) ** 2
        elif self.stable_fn == "long_tail":
            f_stab = 1.0 / (1.0 + 10.0 * ri)
        else:
            raise ValueError(f"unknown stable_fn: {self.stable_fn}")
        f_unst = np.sqrt(np.clip(1.0 - 16.0 * ri, 1.0, None))
        f = np.where(ri >= 0.0, f_stab, f_unst)
        k = lmix**2 * np.sqrt(shear2) * f
        return k, k.copy()  # momentum, heat (Pr = 1 in local regime)

    def _nonlocal_k(self, h, ustar, wthv_s):
        """Troen-Mahrt K-profile at internal interfaces, below h."""
        zi = self.grid.zi[1:-1]
        thv1 = self._theta_v()[0]
        wstar = (GRAV / thv1 * wthv_s * h) ** (1.0 / 3.0)
        w_m = (ustar**3 + self.wstar_fac * wstar**3) ** (1.0 / 3.0)

        # turbulent Prandtl number at 0.1 h (Holtslag & Boville 1993)
        obukhov = -(ustar**3) * thv1 / (VONK * GRAV * max(wthv_s, 1.0e-9))
        zeta = 0.1 * h / obukhov
        phi_m = (1.0 - 16.0 * zeta) ** (-0.25)
        phi_h = (1.0 - 16.0 * zeta) ** (-0.5)
        pr = phi_h / phi_m + self.cg_a * VONK * 0.1 * wstar / max(w_m, 1.0e-6)

        shape = np.clip(zi / h, 0.0, 1.0)
        k_m = VONK * w_m * zi * (1.0 - shape) ** 2
        k_m[zi >= h] = 0.0
        k_h = k_m / pr
        return k_m, k_h, wstar, w_m, pr

    # ------------------------------------------------------------------
    def _assemble_diffusion(self, phi, k_iface, dt, flux_sfc=0.0, drag=0.0, src=None):
        """Backward-Euler implicit diffusion step in conservative flux form.

        ``k_iface`` has n+1 entries; entries 0 and n are ignored (the
        surface flux is prescribed, the top is a zero-flux lid).
        ``drag`` adds an implicit linear sink in the lowest cell
        (momentum surface stress). ``src`` is an explicit tendency.
        """
        g = self.grid
        n = g.n
        k_in = k_iface[1:-1]  # internal interfaces

        lower = np.zeros(n)
        upper = np.zeros(n)
        diag = np.ones(n)
        rhs = phi.copy()

        cu = dt * k_in / (g.dz[:-1] * g.dzc)  # flux into cell below
        cl = dt * k_in / (g.dz[1:] * g.dzc)  # flux into cell above
        upper[:-1] = -cu
        lower[1:] = -cl
        diag[:-1] += cu
        diag[1:] += cl

        rhs[0] += dt * flux_sfc / g.dz[0]
        diag[0] += dt * drag / g.dz[0]
        if src is not None:
            rhs += dt * src
        return solve_tridiag(lower, diag, upper, rhs)

    def _counter_gradient_src(self, k_h_iface, h, gamma):
        """Explicit flux-form tendency from the counter-gradient term."""
        g = self.grid
        flux = np.zeros(g.n + 1)
        zi = g.zi[1:-1]
        inside = (zi > 0.0) & (zi < h)
        flux[1:-1][inside] = k_h_iface[1:-1][inside] * gamma
        return -np.diff(flux) / g.dz

    # ------------------------------------------------------------------
    def step(self, dt, surface):
        """Advance the column by one time step.

        Parameters
        ----------
        dt : float
            Time step [s].
        surface : dict
            Either prescribed kinematic fluxes
            ``dict(wth=..., wq=..., ustar=...)`` (coupled mode, fluxes
            from SUEWS), or ``dict(theta_s=...)`` [K, absolute surface
            temperature] for idealised runs (fluxes from
            Monin-Obukhov similarity; dry surface).

        Returns
        -------
        dict
            Diagnostics: ``h``, ``ustar``, ``wth``, ``wq``, ``wthv``,
            ``obukhov``, ``wstar``.
        """
        g = self.grid
        thv = self._theta_v()
        wind1 = max(np.hypot(self.u[0], self.v[0]), 0.1)

        if "wth" in surface:
            wth = float(surface["wth"])
            wq = float(surface.get("wq", 0.0))
            ustar = max(float(surface["ustar"]), 0.01)
            wthv = wth * (1.0 + 0.61 * self.q[0]) + 0.61 * self.theta[0] * wq
            wthv_safe = wthv if abs(wthv) > 1.0e-9 else 1.0e-9
            obukhov = -(ustar**3) * thv[0] / (VONK * GRAV * wthv_safe)
        elif "theta_s" in surface:
            # surface assumed to share the lowest-level humidity (dry idealised mode)
            thv_s = float(surface["theta_s"]) / exner(self.p_sfc) * (1.0 + 0.61 * self.q[0])
            sl = most_fluxes(wind1, thv[0], thv_s, g.z[0], self.z0m, self.z0h)
            ustar = sl["ustar"]
            wthv = sl["wthv"]
            obukhov = sl["obukhov"]
            wth = wthv / (1.0 + 0.61 * self.q[0])  # dry idealised surface
            wq = 0.0
        else:
            raise ValueError("surface must provide either fluxes or theta_s")

        h = self._diagnose_h(ustar, wthv)

        k_m = np.zeros(g.n + 1)
        k_h = np.zeros(g.n + 1)
        k_m_loc, k_h_loc = self._local_k()
        wstar = 0.0
        gamma_th = 0.0
        gamma_q = 0.0
        if wthv > 1.0e-6:
            k_m_nl, k_h_nl, wstar, w_m, _ = self._nonlocal_k(h, ustar, wthv)
            k_m[1:-1] = np.maximum(k_m_nl, k_m_loc)
            k_h[1:-1] = np.maximum(k_h_nl, k_h_loc)
            gamma_th = self.cg_a * wstar * wth / (w_m**2 * h)
            gamma_q = self.cg_a * wstar * wq / (w_m**2 * h)
        else:
            k_m[1:-1] = k_m_loc
            k_h[1:-1] = k_h_loc
        k_m[1:-1] += self.k_background
        k_h[1:-1] += self.k_background

        src_th = self._counter_gradient_src(k_h, h, gamma_th)
        src_q = self._counter_gradient_src(k_h, h, gamma_q)
        if self.theta_tendency:
            src_th = src_th + self.theta_tendency
        if self.nudge_tau is not None:
            mask = g.z >= self.nudge_above
            if self.theta_ref is not None:
                src_th = src_th + mask * (self.theta_ref - self.theta) / self.nudge_tau
            if self.q_ref is not None:
                src_q = src_q + mask * (self.q_ref - self.q) / self.nudge_tau

        self.theta = self._assemble_diffusion(self.theta, k_h, dt, flux_sfc=wth, src=src_th)
        self.q = self._assemble_diffusion(self.q, k_h, dt, flux_sfc=wq, src=src_q)
        self.q = np.clip(self.q, 0.0, None)

        drag = ustar**2 / wind1
        self.u = self._assemble_diffusion(self.u, k_m, dt, drag=drag)
        self.v = self._assemble_diffusion(self.v, k_m, dt, drag=drag)

        # Coriolis: exact rotation about the geostrophic wind
        if self.f:
            w = (self.u - self.ug) + 1j * (self.v - self.vg)
            w *= np.exp(-1j * self.f * dt)
            self.u = self.ug + w.real
            self.v = self.vg + w.imag

        # subsidence: w(z) = -div * z, first-order upwind (downward motion)
        if self.div:
            w_sub = -self.div * g.z
            for arr in (self.theta, self.q):
                grad_up = np.zeros(g.n)
                grad_up[:-1] = (arr[1:] - arr[:-1]) / g.dzc
                arr -= dt * w_sub * grad_up

        self.k_m_last = k_m
        self.k_h_last = k_h
        self.h_last = h
        return dict(h=h, ustar=ustar, wth=wth, wq=wq, wthv=wthv, obukhov=obukhov, wstar=wstar)
