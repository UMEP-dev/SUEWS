"""Tests for the stretched vertical grid."""

import numpy as np
import pytest

from suews_scm.grid import Grid


def test_grid_basic_geometry():
    grid = Grid(dz0=10.0, ztop=3000.0, stretch=1.08)
    # interfaces start at the surface
    assert grid.zi[0] == 0.0
    # top interface reaches at least ztop
    assert grid.zi[-1] >= 3000.0
    # centres lie midway between interfaces
    np.testing.assert_allclose(grid.z, 0.5 * (grid.zi[:-1] + grid.zi[1:]))
    # strictly increasing
    assert np.all(np.diff(grid.z) > 0)
    assert np.all(np.diff(grid.zi) > 0)
    # first layer thickness as requested
    assert grid.dz[0] == pytest.approx(10.0)
    # consistency: dz = diff(zi), n levels
    np.testing.assert_allclose(grid.dz, np.diff(grid.zi))
    assert grid.n == len(grid.z)


def test_uniform_grid_when_stretch_is_one():
    grid = Grid(dz0=50.0, ztop=1000.0, stretch=1.0)
    np.testing.assert_allclose(grid.dz, 50.0)
    assert grid.n == 20
