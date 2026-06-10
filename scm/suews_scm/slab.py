"""CLASS-style slab (mixed-layer) convective boundary-layer model.

Bulk model of a well-mixed convective boundary layer capped by a sharp
inversion (zeroth-order jump), following the classical formulation of
Tennekes (1973) as consolidated in the CLASS framework
(Vila-Guerau de Arellano et al. 2015). This is the modern descendant of
the legacy SUEWS-CBL / BLUEWS slab scheme (Onomura et al. 2015) that
ships with the SUEWS Fortran code (``suews_phys_bluews.f95``).

Prognostic variables:

- ``h``        boundary-layer depth [m]
- ``theta_m``  mixed-layer potential temperature [K]
- ``dtheta``   inversion jump of theta [K]
- ``q_m``      mixed-layer specific humidity [kg kg-1]
- ``dq``       inversion jump of q [kg kg-1]

Closure: entrainment flux proportional to the surface buoyancy flux,
``w_e = beta * wthv_s / dthv`` with beta = 0.2 (LES consensus, e.g.
Sullivan et al. 1998). When the inversion jump collapses the model
switches to the encroachment limit ``w_e = wthv_s / (gamma h)``.

Integration: classical fourth-order Runge-Kutta.
"""

import numpy as np


class SlabCBL:
    """Bulk convective boundary-layer model with zeroth-order jump."""

    def __init__(
        self,
        h0,
        theta0,
        dtheta0,
        gamma_theta,
        q0=0.0,
        dq0=0.0,
        gamma_q=0.0,
        beta=0.2,
        divergence=0.0,
        dtheta_min=0.05,
    ):
        self.h = float(h0)
        self.theta_m = float(theta0)
        self.dtheta = float(dtheta0)
        self.q_m = float(q0)
        self.dq = float(dq0)
        self.gamma_theta = float(gamma_theta)
        self.gamma_q = float(gamma_q)
        self.beta = float(beta)
        self.div = float(divergence)
        self.dtheta_min = float(dtheta_min)

    # ------------------------------------------------------------------
    def _rhs(self, state, wth, wq):
        h, th, dth, q, dq = state
        h = max(h, 10.0)

        wthv = wth * (1.0 + 0.61 * q) + 0.61 * th * wq
        dthv = dth * (1.0 + 0.61 * q) + 0.61 * th * dq

        w_sub = -self.div * h  # subsidence velocity at the BL top (<= 0)

        if dthv > self.dtheta_min:
            w_e = self.beta * max(wthv, 0.0) / dthv
        else:
            # encroachment limit: jump has collapsed
            w_e = max(wthv, 0.0) / (self.gamma_theta * h)

        dh_dt = w_e + w_sub
        dth_dt = (wth + w_e * dth) / h
        ddth_dt = self.gamma_theta * w_e - dth_dt
        dq_dt = (wq + w_e * dq) / h
        ddq_dt = self.gamma_q * w_e - dq_dt
        return np.array([dh_dt, dth_dt, ddth_dt, dq_dt, ddq_dt])

    def step(self, dt, wth, wq=0.0):
        """Advance the slab by one time step (RK4).

        Parameters
        ----------
        dt : float
            Time step [s].
        wth : float
            Kinematic surface heat flux [K m s-1].
        wq : float
            Kinematic surface moisture flux [kg kg-1 m s-1].

        Returns
        -------
        dict
            ``h``, ``theta_m``, ``dtheta``, ``q_m``, ``dq``, ``we``.
        """
        y = np.array([self.h, self.theta_m, self.dtheta, self.q_m, self.dq])
        k1 = self._rhs(y, wth, wq)
        k2 = self._rhs(y + 0.5 * dt * k1, wth, wq)
        k3 = self._rhs(y + 0.5 * dt * k2, wth, wq)
        k4 = self._rhs(y + dt * k3, wth, wq)
        y = y + dt / 6.0 * (k1 + 2.0 * k2 + 2.0 * k3 + k4)

        self.h, self.theta_m, self.dtheta, self.q_m, self.dq = y
        self.dtheta = max(self.dtheta, 0.0)

        wthv = wth * (1.0 + 0.61 * self.q_m) + 0.61 * self.theta_m * wq
        dthv = self.dtheta * (1.0 + 0.61 * self.q_m) + 0.61 * self.theta_m * self.dq
        if dthv > self.dtheta_min:
            we = self.beta * max(wthv, 0.0) / dthv
        else:
            we = max(wthv, 0.0) / (self.gamma_theta * self.h)
        return dict(
            h=self.h, theta_m=self.theta_m, dtheta=self.dtheta,
            q_m=self.q_m, dq=self.dq, we=we,
        )
