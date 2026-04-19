"""GH-1292 PR1 data-model tests for the moisture-aware LAI parameters.

These tests lock the PR1 contract: (a) the six new optional fields
(`w_wilt`, `w_opt`, `f_shape`, `w_on`, `w_off`, `tau_w`) are serialised
through `to_df_state`/`from_df_state` with tolerant defaults; (b) the
threshold consistency validator fires only for `laitype == 2`; and (c)
`checker_rules_indiv.json` covers every new df_state column so that
`check_state()` does not reject a run.
"""

from __future__ import annotations

import json
from pathlib import Path

import pytest
from pydantic import ValidationError

from supy import SUEWSSimulation
from supy._check import check_state
from supy.data_model.core.site import LAIParams
from supy.data_model.core.type import RefValue

VEG_IDX_EVETR = 0  # LAIParams.to_df_state uses surf_idx - 2, so veg_idx=0 maps to EVETR.


def _roundtrip(lai: LAIParams) -> LAIParams:
    df = lai.to_df_state(grid_id=1, surf_idx=2)
    return LAIParams.from_df_state(df, grid_id=1, surf_idx=2)


def test_defaults_loaded_for_legacy_config() -> None:
    """LAIParams(laitype=0) without moisture fields round-trips to defaults."""

    lai = LAIParams(laitype=0)
    df = lai.to_df_state(grid_id=1, surf_idx=2)
    for field in ("w_wilt", "w_opt", "f_shape", "w_on", "w_off", "tau_w"):
        col = (field, f"({VEG_IDX_EVETR},)")
        assert col in df.columns, f"missing df_state column {col}"
        assert df.loc[1, col] == LAIParams.MOISTURE_DEFAULTS[field]

    recovered = LAIParams.from_df_state(df, grid_id=1, surf_idx=2)
    assert recovered.laitype.value == 0
    for field in LAIParams.MOISTURE_DEFAULTS:
        value = getattr(recovered, field)
        assert value is not None
        actual = value.value if isinstance(value, RefValue) else value
        assert actual == LAIParams.MOISTURE_DEFAULTS[field]


def test_round_trip_with_explicit_fields() -> None:
    """Explicit laitype=2 with non-default moisture fields survives round-trip."""

    lai = LAIParams(
        laitype=2,
        w_wilt=0.2,
        w_opt=0.45,
        f_shape=1.5,
        w_on=0.4,
        w_off=0.15,
        tau_w=20.0,
    )
    recovered = _roundtrip(lai)
    assert recovered.laitype.value == 2
    assert pytest.approx(0.2) == recovered.w_wilt.value
    assert pytest.approx(0.45) == recovered.w_opt.value
    assert pytest.approx(1.5) == recovered.f_shape.value
    assert pytest.approx(0.4) == recovered.w_on.value
    assert pytest.approx(0.15) == recovered.w_off.value
    assert pytest.approx(20.0) == recovered.tau_w.value


def test_validator_fires_only_for_laitype_2() -> None:
    """w_opt <= w_wilt raises only under laitype=2; legacy laitype=0 ignores it."""

    with pytest.raises(ValidationError, match="w_opt"):
        LAIParams(laitype=2, w_wilt=0.4, w_opt=0.3)

    with pytest.raises(ValidationError, match="w_off"):
        LAIParams(laitype=2, w_on=0.3, w_off=0.5)

    # Under laitype=0 the validator must stay silent.
    LAIParams(laitype=0, w_wilt=0.4, w_opt=0.3, w_on=0.3, w_off=0.5)


def test_checker_rules_cover_new_columns() -> None:
    """src/supy/checker_rules_indiv.json must list every new moisture column."""

    rules_path = (
        Path(__file__).resolve().parents[2]
        / "src"
        / "supy"
        / "checker_rules_indiv.json"
    )
    rules = json.loads(rules_path.read_text(encoding="utf-8"))
    for field in ("w_wilt", "w_opt", "f_shape", "w_on", "w_off", "tau_w"):
        assert field in rules, f"checker_rules_indiv.json missing entry for {field}"


def test_check_state_accepts_legacy_df_without_moisture_columns() -> None:
    """Legacy df_state inputs may omit the new moisture columns without failing validation."""

    sim = SUEWSSimulation.from_sample_data()
    df_state = sim._config.to_df_state()
    moisture_cols = [
        (field, f"({veg_idx},)")
        for field in ("w_wilt", "w_opt", "f_shape", "w_on", "w_off", "tau_w")
        for veg_idx in range(3)
    ]
    df_legacy = df_state.drop(columns=moisture_cols)

    issues = check_state(df_legacy, fix=False)
    issues_text = "\n".join(issues)
    assert "Mandatory columns missing from df_state" not in issues_text
    for field in ("w_wilt", "w_opt", "f_shape", "w_on", "w_off", "tau_w"):
        assert field not in issues_text, issues_text
