"""Tests for the tridiagonal (Thomas) solver."""

import numpy as np
import pytest

from suews_scm.tridiag import solve_tridiag


def _dense_from_bands(a, b, c):
    n = len(b)
    m = np.zeros((n, n))
    for i in range(n):
        m[i, i] = b[i]
        if i > 0:
            m[i, i - 1] = a[i]
        if i < n - 1:
            m[i, i + 1] = c[i]
    return m


def test_matches_dense_solver_on_random_diagonally_dominant_system():
    rng = np.random.default_rng(42)
    n = 50
    a = rng.uniform(-1.0, 0.0, n)
    c = rng.uniform(-1.0, 0.0, n)
    a[0] = 0.0
    c[-1] = 0.0
    # diagonally dominant -> well conditioned
    b = 2.0 + np.abs(a) + np.abs(c) + rng.uniform(0.0, 1.0, n)
    d = rng.normal(size=n)

    x = solve_tridiag(a, b, c, d)
    x_ref = np.linalg.solve(_dense_from_bands(a, b, c), d)

    np.testing.assert_allclose(x, x_ref, rtol=1e-12, atol=1e-12)


def test_identity_system_returns_rhs():
    n = 7
    a = np.zeros(n)
    c = np.zeros(n)
    b = np.ones(n)
    d = np.arange(n, dtype=float)
    np.testing.assert_allclose(solve_tridiag(a, b, c, d), d)


def test_does_not_mutate_inputs():
    n = 5
    a = np.full(n, -1.0)
    a[0] = 0.0
    b = np.full(n, 3.0)
    c = np.full(n, -1.0)
    c[-1] = 0.0
    d = np.ones(n)
    copies = [arr.copy() for arr in (a, b, c, d)]
    solve_tridiag(a, b, c, d)
    for arr, ref in zip((a, b, c, d), copies):
        np.testing.assert_array_equal(arr, ref)
