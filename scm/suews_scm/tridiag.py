"""Tridiagonal linear solver (Thomas algorithm)."""

import numpy as np


def solve_tridiag(a, b, c, d):
    """Solve a tridiagonal system A x = d.

    Parameters
    ----------
    a : array_like, shape (n,)
        Sub-diagonal coefficients; ``a[0]`` is unused.
    b : array_like, shape (n,)
        Main diagonal coefficients.
    c : array_like, shape (n,)
        Super-diagonal coefficients; ``c[-1]`` is unused.
    d : array_like, shape (n,)
        Right-hand side.

    Returns
    -------
    numpy.ndarray
        Solution vector x. Inputs are not modified.

    Notes
    -----
    Standard Thomas algorithm, O(n). Stable for diagonally dominant
    systems, which is always the case for the implicit diffusion
    matrices assembled in :mod:`suews_scm.column`.
    """
    b = np.asarray(b, dtype=float).copy()
    d = np.asarray(d, dtype=float).copy()
    a = np.asarray(a, dtype=float)
    c = np.asarray(c, dtype=float)
    n = b.size

    for i in range(1, n):
        w = a[i] / b[i - 1]
        b[i] -= w * c[i - 1]
        d[i] -= w * d[i - 1]

    x = np.empty(n)
    x[-1] = d[-1] / b[-1]
    for i in range(n - 2, -1, -1):
        x[i] = (d[i] - c[i] * x[i + 1]) / b[i]
    return x
