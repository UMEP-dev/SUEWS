"""Stretched vertical grid for the column model."""

import numpy as np


class Grid:
    """Geometrically stretched vertical grid.

    Cell interfaces start at the surface (z = 0); layer thicknesses grow
    by ``stretch`` per layer from ``dz0`` until the top interface reaches
    ``ztop``. Prognostic variables live at cell centres ``z``; turbulent
    fluxes and eddy diffusivities live at interfaces ``zi``.

    Attributes
    ----------
    z : numpy.ndarray, shape (n,)
        Cell-centre heights [m].
    zi : numpy.ndarray, shape (n + 1,)
        Interface heights [m], ``zi[0] = 0``.
    dz : numpy.ndarray, shape (n,)
        Layer thicknesses [m].
    dzc : numpy.ndarray, shape (n - 1,)
        Distances between adjacent cell centres [m].
    n : int
        Number of layers.
    """

    def __init__(self, dz0=10.0, ztop=3000.0, stretch=1.08):
        if dz0 <= 0 or ztop <= dz0 or stretch < 1.0:
            raise ValueError("require dz0 > 0, ztop > dz0, stretch >= 1")
        interfaces = [0.0]
        dz = dz0
        while interfaces[-1] < ztop:
            interfaces.append(interfaces[-1] + dz)
            dz *= stretch
        self.zi = np.array(interfaces)
        self.dz = np.diff(self.zi)
        self.z = 0.5 * (self.zi[:-1] + self.zi[1:])
        self.dzc = np.diff(self.z)
        self.n = self.z.size

    def __repr__(self):
        return (
            f"Grid(n={self.n}, dz0={self.dz[0]:.1f} m, "
            f"top={self.zi[-1]:.0f} m)"
        )
