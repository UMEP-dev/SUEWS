"""Degree-day onset thresholds ``gdd_base`` and ``sdd_base`` on ``LAIParams``.

``gdd_base`` is the growing degree days that must accumulate before leaf
growth starts, and ``sdd_base`` the (negative) senescence degree days that
must accumulate before leaf off starts. This module checks that the data
model gives them documented defaults, enforces their signs, keeps them
through the DataFrame state, and hands them to the engine YAML.
"""

from __future__ import annotations

from importlib import import_module
from importlib.resources import files

from pydantic import ValidationError
import pytest
import yaml

from supy.data_model.core import SUEWSConfig
from supy.data_model.core.site import LAIParams
from supy.data_model.core.type import RefValue

config_to_engine_yaml = import_module("supy._run_rust").config_to_engine_yaml

pytestmark = [
    pytest.mark.unit,
    pytest.mark.cfg,
    pytest.mark.contract("C-VALID", "C-ROUNDTRIP", "C-EMIT"),
]

VEG_SURFACES = ("evetr", "dectr", "grass")


def _value(x):
    """Unwrap a RefValue object or its serialised ``{value: ...}`` form."""
    if isinstance(x, RefValue):
        return x.value
    if isinstance(x, dict):
        return x["value"]
    return x


def test_defaults_are_zero_thresholds():
    lai = LAIParams()
    assert _value(lai.gdd_base) == 0.0
    assert _value(lai.sdd_base) == 0.0


@pytest.mark.parametrize(
    ("field", "bad_value"),
    [("gdd_base", -1.0), ("sdd_base", 1.0)],
)
def test_wrong_sign_is_rejected(field, bad_value):
    with pytest.raises(ValidationError, match=field):
        LAIParams(**{field: bad_value})


def test_df_state_roundtrip_keeps_thresholds():
    lai = LAIParams(gdd_base=120.0, sdd_base=-240.0)
    df = lai.to_df_state(grid_id=1, surf_idx=3)
    restored = LAIParams.from_df_state(df, grid_id=1, surf_idx=3)
    assert _value(restored.gdd_base) == pytest.approx(120.0)
    assert _value(restored.sdd_base) == pytest.approx(-240.0)


def test_df_state_without_threshold_columns_uses_defaults():
    df = LAIParams().to_df_state(grid_id=1, surf_idx=3)
    df = df.drop(columns=["gdd_base", "sdd_base"], level=0)
    restored = LAIParams.from_df_state(df, grid_id=1, surf_idx=3)
    assert _value(restored.gdd_base) == 0.0
    assert _value(restored.sdd_base) == 0.0


def test_engine_yaml_carries_thresholds():
    sample_path = files("supy").joinpath("sample_data", "sample_config.yml")
    raw = yaml.safe_load(sample_path.read_text(encoding="utf-8"))
    land_cover = raw["sites"][0]["properties"]["land_cover"]
    for i, surface in enumerate(VEG_SURFACES):
        land_cover[surface]["lai"]["gdd_base"] = {"value": 100.0 + i}
        land_cover[surface]["lai"]["sdd_base"] = {"value": -200.0 - i}

    resolved = yaml.safe_load(config_to_engine_yaml(SUEWSConfig.from_dict(raw)))
    resolved_lc = resolved["sites"][0]["properties"]["land_cover"]
    for i, surface in enumerate(VEG_SURFACES):
        lai = resolved_lc[surface]["lai"]
        assert _value(lai["gdd_base"]) == pytest.approx(100.0 + i)
        assert _value(lai["sdd_base"]) == pytest.approx(-200.0 - i)
