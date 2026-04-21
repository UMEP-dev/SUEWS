"""Tier A data-model tests for A-gs coupling scaffolding (``ags_method``).

These tests guard the Python-side plumbing for the forthcoming Farquhar-von
Caemmerer-Berry + Medlyn A-gs path. They assert:

* The new :class:`~supy.data_model.core.model.AgsMethod` enum is well-formed.
* :attr:`ModelPhysics.ags_method` defaults to ``JARVIS`` so existing runs are
  unaffected.
* ``ags_method`` round-trips through ``to_df_state``/``from_df_state`` and
  legacy DataFrames predating the field still load (defaulting to ``JARVIS``).
* The new FvCB/Medlyn per-surface parameters on
  :class:`~supy.data_model.core.site.VegetatedSurfaceProperties` default to
  ``None`` and fall back to published PFT-mean values (Kattge et al. 2009,
  Lin et al. 2015) when packed into ``df_state``; explicit user values
  override the fallbacks.

The Fortran-side wiring is still to come; these tests pin the Python contract
so it does not drift before the coupled solver lands.
"""

from __future__ import annotations

import pandas as pd
import pytest

from supy.data_model.core.model import AgsMethod, ModelPhysics
from supy.data_model.core.site import (
    DectrProperties,
    EvetrProperties,
    GrassProperties,
    VegetatedSurfaceProperties,
)
from supy.data_model.core.type import RefValue

FVCB_FIELDS = ("v_cmax25", "j_max25", "g_1", "g_0", "c3c4_flag")
FVCB_FALLBACKS = {
    "v_cmax25": 60.0,
    "j_max25": 100.0,
    "g_1": 3.8,
    "g_0": 0.01,
    "c3c4_flag": 1,
}


def _scalar(df: pd.DataFrame, col: tuple[str, str]) -> float | int:
    """Extract a single value from a 2-level MultiIndex DataFrame column."""
    return df[col].values.item()


def _unwrap(value):
    return value.value if isinstance(value, RefValue) else value


def test_enum_values_and_members():
    assert int(AgsMethod.JARVIS) == 1
    assert int(AgsMethod.MEDLYN_FVCB) == 2
    assert {m.name for m in AgsMethod} == {"JARVIS", "MEDLYN_FVCB"}


def test_enum_internal_marker():
    # JARVIS is the default public path; MEDLYN_FVCB stays internal until the
    # solver, c_a forcing column, and Tier B validation have landed.
    assert AgsMethod.JARVIS._internal is False
    assert AgsMethod.MEDLYN_FVCB._internal is True


def test_model_physics_ags_method_defaults_to_jarvis():
    mp = ModelPhysics()
    assert _unwrap(mp.ags_method) == AgsMethod.JARVIS


def test_model_physics_ags_method_roundtrips_through_df_state():
    mp = ModelPhysics()
    df = mp.to_df_state(grid_id=1)
    assert ("ags_method", "0") in df.columns
    assert _scalar(df, ("ags_method", "0")) == 1

    mp_rt = ModelPhysics.from_df_state(df, grid_id=1)
    assert _unwrap(mp_rt.ags_method) == AgsMethod.JARVIS


def test_legacy_df_without_ags_method_loads_as_jarvis():
    mp = ModelPhysics()
    df = mp.to_df_state(grid_id=1).drop(columns=[("ags_method", "0")])
    mp_rt = ModelPhysics.from_df_state(df, grid_id=1)
    assert _unwrap(mp_rt.ags_method) == AgsMethod.JARVIS


@pytest.mark.parametrize(
    "veg_cls",
    [EvetrProperties, DectrProperties, GrassProperties],
)
def test_fvcb_fields_default_to_none(veg_cls):
    veg = veg_cls()
    for field in FVCB_FIELDS:
        assert getattr(veg, field) is None, (
            f"{veg_cls.__name__}.{field} should default to None"
        )


@pytest.mark.parametrize(
    ("veg_cls", "surf_idx"),
    [(EvetrProperties, 2), (DectrProperties, 3), (GrassProperties, 4)],
)
def test_fvcb_fallback_values_land_in_df_state(veg_cls, surf_idx):
    veg = veg_cls()
    df = veg.to_df_state(grid_id=1)
    col_suffix = f"({surf_idx - 2},)"
    for field, expected in FVCB_FALLBACKS.items():
        value = _scalar(df, (field, col_suffix))
        assert value == pytest.approx(expected), (
            f"{veg_cls.__name__}.{field} fallback expected {expected} got {value}"
        )


def test_explicit_fvcb_values_override_fallbacks():
    veg = EvetrProperties(
        v_cmax25=RefValue(55.0),
        j_max25=RefValue(95.0),
        g_1=RefValue(2.3),
        g_0=RefValue(0.02),
        c3c4_flag=RefValue(1),
    )
    df = veg.to_df_state(grid_id=1)
    assert _scalar(df, ("v_cmax25", "(0,)")) == pytest.approx(55.0)
    assert _scalar(df, ("j_max25", "(0,)")) == pytest.approx(95.0)
    assert _scalar(df, ("g_1", "(0,)")) == pytest.approx(2.3)
    assert _scalar(df, ("g_0", "(0,)")) == pytest.approx(0.02)
    assert _scalar(df, ("c3c4_flag", "(0,)")) == 1


def test_schema_exposes_units_and_display_names():
    schema = VegetatedSurfaceProperties.model_json_schema()
    props = schema.get("properties", {})
    expected_units = {
        "v_cmax25": "umol m^-2 s^-1",
        "j_max25": "umol m^-2 s^-1",
        "g_1": "kPa^0.5",
        "g_0": "mol m^-2 s^-1",
        "c3c4_flag": "dimensionless",
    }
    for field, unit in expected_units.items():
        assert field in props, (
            f"{field} missing from VegetatedSurfaceProperties schema"
        )
        assert props[field].get("unit") == unit, (
            f"{field} unit mismatch: expected {unit} got {props[field].get('unit')}"
        )
        assert props[field].get("display_name"), (
            f"{field} missing display_name in json_schema_extra"
        )
