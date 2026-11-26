"""Contract tests for 10-minute STEBBS profile key normalisation and strict rejection."""

from __future__ import annotations

from importlib.resources import as_file, files
from pathlib import Path

from pydantic import ValidationError
import pytest
import yaml

from supy._run_rust import config_to_engine_yaml  # ruff: ignore[import-private-name]
from supy.data_model import SUEWSConfig
from supy.data_model.core.profile import (
    CANONICAL_TEN_MINUTE_KEYS,
    CoolingSetpointProfile,
    HeatingSetpointProfile,
    TenMinuteProfile,
    normalise_ten_minute_profile_key,
    normalise_ten_minute_profile_mapping,
)
from supy.data_model.validation.pipeline.orchestrator import (
    run_phase_a,
    run_phase_b,
    run_phase_c,
)

pytestmark = [
    pytest.mark.api,
    pytest.mark.contract("C-VALID", "C-ROUNDTRIP", "C-STRICT"),
]

MALFORMED_TEN_MINUTE_KEYS: tuple[object, ...] = (
    "+1",
    "-1",
    " 1",
    "1 ",
    "\t1",
    "1\n",
    "1.0",
    "0_1",
    "1_0",
    "\u0661",
    "\uff10",
    "\uff11",
    "0",
    "00",
    "000",
    "145",
    "0145",
    "",
    "abc",
    0,
    -1,
    145,
    1.0,
    2.5,
    True,
    False,
    None,
)

COLLISION_KEY_PAIRS: tuple[tuple[object, object], ...] = (
    ("1", "01"),
    ("01", "001"),
    (1, "1"),
    (1, "01"),
    ("144", "0144"),
    (144, "0144"),
)


def _make_mixed_valid_144(base: float = 0.0) -> dict[object, float]:
    """Build a 144-step mapping mixing zero-padded strings, integers, and canonical keys."""
    out: dict[object, float] = {}
    for step in range(1, 145):
        if step == 1:
            key: object = "001"
        elif step <= 9:
            key = f"{step:02d}"
        elif step <= 20:
            key = step
        elif step == 144:
            key = "0144"
        else:
            key = str(step)
        out[key] = round(base + step * 0.01, 4)
    return out


@pytest.mark.parametrize(
    ("raw_key", "expected"),
    [
        ("1", "1"),
        ("01", "1"),
        ("001", "1"),
        ("09", "9"),
        ("009", "9"),
        ("10", "10"),
        ("010", "10"),
        ("144", "144"),
        ("0144", "144"),
        (1, "1"),
        (72, "72"),
        (144, "144"),
    ],
)
def test_normalise_ten_minute_profile_key_accepts_valid_forms(
    raw_key: object, expected: str
) -> None:
    """Zero-padded ASCII digits and plain integers 1..144 map to canonical strings."""
    assert normalise_ten_minute_profile_key(raw_key) == expected


@pytest.mark.parametrize("bad_key", MALFORMED_TEN_MINUTE_KEYS)
def test_normalise_ten_minute_profile_key_rejects_malformed(bad_key: object) -> None:
    """Signed, padded, float, bool, non-ASCII, and out-of-range keys raise ValueError."""
    with pytest.raises(ValueError, match="Invalid 10-minute profile key"):
        normalise_ten_minute_profile_key(bad_key)


@pytest.mark.parametrize(("key_a", "key_b"), COLLISION_KEY_PAIRS)
def test_normalise_ten_minute_profile_mapping_rejects_collisions(
    key_a: object, key_b: object
) -> None:
    """Two keys normalising to the same canonical 10-minute step raise ValueError."""
    raw: dict[object, float] = {str(s): 0.5 for s in range(1, 145)}
    canon = normalise_ten_minute_profile_key(key_a)
    raw.pop(canon, None)
    raw[key_a] = 0.1
    raw[key_b] = 0.2
    with pytest.raises(ValueError, match=f"Duplicate 10-minute profile key '{canon}'"):
        normalise_ten_minute_profile_mapping(raw)


@pytest.mark.parametrize(
    "profile_cls",
    [TenMinuteProfile, HeatingSetpointProfile, CoolingSetpointProfile],
)
def test_ten_minute_profile_models_normalise_valid_keys_and_preserve_values(
    profile_cls: type[TenMinuteProfile],
) -> None:
    """TenMinuteProfile and setpoint subclasses normalise zero-padded and integer keys."""
    wd = _make_mixed_valid_144(base=10.0)
    hol = _make_mixed_valid_144(base=20.0)

    prof = profile_cls(working_day=wd, holiday=hol)
    assert tuple(prof.working_day.keys()) == CANONICAL_TEN_MINUTE_KEYS
    assert tuple(prof.holiday.keys()) == CANONICAL_TEN_MINUTE_KEYS
    assert prof.working_day["1"] == pytest.approx(10.01)
    assert prof.working_day["144"] == pytest.approx(11.44)
    assert prof.holiday["1"] == pytest.approx(20.01)
    assert prof.holiday["144"] == pytest.approx(21.44)


@pytest.mark.parametrize(
    "profile_cls",
    [TenMinuteProfile, HeatingSetpointProfile, CoolingSetpointProfile],
)
@pytest.mark.parametrize("bad_key", ["+1", "-1", " 1", "1.0", "0", "145", True, 1.0])
def test_ten_minute_profile_models_reject_malformed_keys(
    profile_cls: type[TenMinuteProfile], bad_key: object
) -> None:
    """Malformed step keys fail Pydantic validation across TenMinuteProfile subclasses."""
    valid = {str(s): 0.5 for s in range(1, 145)}
    bad = dict(valid)
    bad.pop("1")
    bad[bad_key] = 0.5
    with pytest.raises(ValidationError, match="Invalid 10-minute profile key"):
        profile_cls(working_day=bad, holiday=valid)


@pytest.mark.parametrize(
    "profile_cls",
    [TenMinuteProfile, HeatingSetpointProfile, CoolingSetpointProfile],
)
@pytest.mark.parametrize(("key_a", "key_b"), [("1", "01"), (1, "01"), ("144", "0144")])
def test_ten_minute_profile_models_reject_colliding_keys(
    profile_cls: type[TenMinuteProfile], key_a: object, key_b: object
) -> None:
    """Colliding step keys fail Pydantic validation instead of silently overwriting."""
    valid = {str(s): 0.5 for s in range(1, 145)}
    colliding: dict[object, float] = dict(valid)
    canon = normalise_ten_minute_profile_key(key_a)
    colliding.pop(canon)
    colliding[key_a] = 0.1
    colliding[key_b] = 0.2
    with pytest.raises(ValidationError, match="Duplicate 10-minute profile key"):
        profile_cls(working_day=colliding, holiday=valid)


FIVE_TEN_MINUTE_PROFILE_PATHS: tuple[tuple[str, str], ...] = (
    ("building_archetype", "profile_metabolism"),
    ("building_archetype", "profile_setpoint_temperature_heating_air"),
    ("building_archetype", "profile_setpoint_temperature_cooling_air"),
    ("stebbs", "profile_flow_hot_water"),
    ("stebbs", "profile_appliance"),
)


def _load_sample_config_dict() -> dict:
    """Load the packaged sample YAML configuration as a fresh mutable dict."""
    with as_file(files("supy") / "sample_data" / "sample_config.yml") as path_cfg:
        return yaml.safe_load(path_cfg.read_text(encoding="utf-8"))


@pytest.mark.parametrize(("section", "field_name"), FIVE_TEN_MINUTE_PROFILE_PATHS)
def test_suews_config_from_yaml_normalises_ten_minute_profile_keys(
    tmp_path: Path, section: str, field_name: str
) -> None:
    """SUEWSConfig.from_yaml normalises valid integer/zero-padded 10-minute profile keys."""
    cfg_dict = _load_sample_config_dict()
    wd = _make_mixed_valid_144(base=10.0)
    hol = _make_mixed_valid_144(base=20.0)
    cfg_dict["sites"][0]["properties"][section][field_name] = {
        "working_day": wd,
        "holiday": hol,
    }
    yaml_path = tmp_path / f"{field_name}.yml"
    yaml_path.write_text(yaml.safe_dump(cfg_dict, sort_keys=False), encoding="utf-8")

    loaded = SUEWSConfig.from_yaml(str(yaml_path))
    profile = getattr(getattr(loaded.sites[0].properties, section), field_name)
    assert tuple(profile.working_day.keys()) == CANONICAL_TEN_MINUTE_KEYS
    assert tuple(profile.holiday.keys()) == CANONICAL_TEN_MINUTE_KEYS
    assert profile.working_day["1"] == pytest.approx(10.01)
    assert profile.working_day["144"] == pytest.approx(11.44)
    assert profile.holiday["1"] == pytest.approx(20.01)
    assert profile.holiday["144"] == pytest.approx(21.44)


@pytest.mark.parametrize(
    ("section", "field_name", "day_type", "bad_entries", "expected_msg"),
    [
        (
            "building_archetype",
            "profile_metabolism",
            "working_day",
            {"+1": 0.5},
            "Invalid 10-minute profile key",
        ),
        (
            "building_archetype",
            "profile_setpoint_temperature_heating_air",
            "holiday",
            {"145": 20.0},
            "Invalid 10-minute profile key",
        ),
        (
            "stebbs",
            "profile_flow_hot_water",
            "working_day",
            {"1": 0.1, "01": 0.2},
            "Duplicate 10-minute profile key",
        ),
        (
            "stebbs",
            "profile_appliance",
            "holiday",
            {1: 0.1, "01": 0.2},
            "Duplicate 10-minute profile key",
        ),
    ],
)
def test_suews_config_from_yaml_rejects_malformed_and_colliding_ten_minute_keys(
    tmp_path: Path,
    section: str,
    field_name: str,
    day_type: str,
    bad_entries: dict[object, float],
    expected_msg: str,
) -> None:
    """SUEWSConfig.from_yaml rejects malformed/colliding 10-minute keys with field path."""
    cfg_dict = _load_sample_config_dict()
    valid = {str(s): 0.5 for s in range(1, 145)}
    bad = dict(valid)
    bad.pop("1")
    bad.update(bad_entries)
    other_day = "holiday" if day_type == "working_day" else "working_day"
    cfg_dict["sites"][0]["properties"][section][field_name] = {
        day_type: bad,
        other_day: valid,
    }
    yaml_path = tmp_path / f"{field_name}_{day_type}_bad.yml"
    yaml_path.write_text(yaml.safe_dump(cfg_dict, sort_keys=False), encoding="utf-8")

    with pytest.raises(ValueError, match=expected_msg) as exc_info:
        SUEWSConfig.from_yaml(str(yaml_path))
    err_text = str(exc_info.value)
    assert f"sites[gridiv=1].properties.{section}.{field_name}.{day_type}" in err_text


@pytest.mark.parametrize(
    "profile_cls",
    [TenMinuteProfile, HeatingSetpointProfile, CoolingSetpointProfile],
)
def test_ten_minute_profile_df_state_roundtrip_and_live_mutation_guards(
    profile_cls: type[TenMinuteProfile],
) -> None:
    """Direct ``to_df_state`` round-trips canonical slots and rejects post-init collisions."""
    wd = _make_mixed_valid_144(base=5.0)
    hol = _make_mixed_valid_144(base=15.0)
    prof = profile_cls(working_day=wd, holiday=hol)

    df_state = prof.to_df_state(1, "profile_metabolism")
    reconstructed = profile_cls.from_df_state(df_state, 1, "profile_metabolism")
    assert tuple(reconstructed.working_day.keys()) == CANONICAL_TEN_MINUTE_KEYS
    assert tuple(reconstructed.holiday.keys()) == CANONICAL_TEN_MINUTE_KEYS
    assert reconstructed.working_day["1"] == pytest.approx(5.01)
    assert reconstructed.working_day["144"] == pytest.approx(6.44)
    assert reconstructed.holiday["1"] == pytest.approx(15.01)
    assert reconstructed.holiday["144"] == pytest.approx(16.44)

    # Valid zero-padded post-construction replacement normalises cleanly on to_df_state().
    prof.working_day = _make_mixed_valid_144(base=7.0)
    df_norm = prof.to_df_state(1, "profile_metabolism")
    assert df_norm.loc[1, ("profile_metabolism", "(0, 0)")] == pytest.approx(7.01)
    assert df_norm.loc[1, ("profile_metabolism", "(143, 0)")] == pytest.approx(8.44)

    # Post-construction collision must fail instead of silently overwriting slot 0.
    prof_dup = profile_cls()
    prof_dup.working_day["01"] = 99.0
    with pytest.raises(ValueError, match="Duplicate 10-minute profile key '1'"):
        prof_dup.to_df_state(1, "profile_metabolism")

    # Post-construction malformed key must fail in holiday as well.
    prof_bad = profile_cls()
    prof_bad.holiday.pop("1")
    prof_bad.holiday["+1"] = 99.0
    with pytest.raises(ValueError, match="Invalid 10-minute profile key"):
        prof_bad.to_df_state(1, "profile_metabolism")


def test_suews_config_live_ten_minute_mutation_validated_across_export_boundaries(
    tmp_path: Path,
) -> None:
    """Post-construction 10-min mutations are normalised/rejected across to_df_state, to_yaml, and engine export."""
    cfg = SUEWSConfig.from_dict(_load_sample_config_dict())

    # Valid zero-padded post-construction mutation normalises cleanly on to_yaml, engine export, and to_df_state.
    cfg.sites[
        0
    ].properties.building_archetype.profile_metabolism.working_day = (
        _make_mixed_valid_144(base=2.0)
    )
    yaml_out = tmp_path / "mutated_valid.yml"
    cfg.to_yaml(str(yaml_out))
    dumped = yaml.safe_load(yaml_out.read_text(encoding="utf-8"))
    dumped_wd = dumped["sites"][0]["properties"]["building_archetype"][
        "profile_metabolism"
    ]["working_day"]
    assert tuple(dumped_wd.keys()) == CANONICAL_TEN_MINUTE_KEYS
    assert dumped_wd["1"] == pytest.approx(2.01)
    assert dumped_wd["144"] == pytest.approx(3.44)

    cfg.sites[0].properties.stebbs.profile_appliance.holiday = _make_mixed_valid_144(
        base=4.0
    )
    engine_data = yaml.safe_load(config_to_engine_yaml(cfg, for_file_output=False))
    engine_hol = engine_data["sites"][0]["properties"]["stebbs"]["profile_appliance"][
        "holiday"
    ]
    assert tuple(engine_hol.keys()) == CANONICAL_TEN_MINUTE_KEYS
    assert engine_hol["1"] == pytest.approx(4.01)
    assert engine_hol["144"] == pytest.approx(5.44)

    cfg.sites[
        0
    ].properties.stebbs.profile_flow_hot_water.working_day = _make_mixed_valid_144(
        base=6.0
    )
    df_state = cfg.to_df_state()
    assert df_state.loc[1, ("hotwaterflowprofile", "(0, 0)")] == pytest.approx(6.01)

    # Live collision mutation ("1" + "01") must be rejected before to_df_state, to_yaml, and engine export.
    cfg.sites[0].properties.building_archetype.profile_metabolism.working_day["01"] = (
        99.0
    )
    with pytest.raises(ValueError, match="Duplicate 10-minute profile key '1'"):
        cfg.to_df_state()
    dup_out = tmp_path / "mutated_dup.yml"
    with pytest.raises(ValueError, match="Duplicate 10-minute profile key '1'"):
        cfg.to_yaml(str(dup_out))
    assert not dup_out.exists()
    with pytest.raises(ValueError, match="Duplicate 10-minute profile key '1'"):
        config_to_engine_yaml(cfg, for_file_output=False)

    # Restore valid metabolism profile and inject malformed key into stebbs profile.
    cfg.sites[0].properties.building_archetype.profile_metabolism.working_day.pop("01")
    cfg.sites[0].properties.stebbs.profile_appliance.holiday.pop("1")
    cfg.sites[0].properties.stebbs.profile_appliance.holiday["+1"] = 0.5
    with pytest.raises(ValueError, match="Invalid 10-minute profile key"):
        cfg.to_df_state()
    bad_out = tmp_path / "mutated_bad.yml"
    with pytest.raises(ValueError, match="Invalid 10-minute profile key"):
        cfg.to_yaml(str(bad_out))
    assert not bad_out.exists()
    with pytest.raises(ValueError, match="Invalid 10-minute profile key"):
        config_to_engine_yaml(cfg, for_file_output=False)


def test_validation_pipeline_phases_a_b_c_normalise_zero_padded_ten_minute_keys(
    tmp_path: Path,
) -> None:
    """Phases A, B, and C accept complete zero-padded 10-minute profiles without null-padding collisions."""
    raw = _load_sample_config_dict()
    props = raw["sites"][0]["properties"]
    # Replace canonical "1" with "01" (exact scenario that previously triggered 1: null in Phase A)
    met_wd = dict(props["building_archetype"]["profile_metabolism"]["working_day"])
    val_1 = met_wd.pop("1")
    met_wd["01"] = val_1
    props["building_archetype"]["profile_metabolism"]["working_day"] = met_wd
    props["building_archetype"]["profile_setpoint_temperature_heating_air"][
        "holiday"
    ] = _make_mixed_valid_144(base=18.0)
    props["stebbs"]["profile_appliance"]["working_day"] = _make_mixed_valid_144(
        base=0.1
    )

    cfg_path = tmp_path / "zero_padded_10min.yml"
    cfg_path.write_text(yaml.safe_dump(raw, sort_keys=False), encoding="utf-8")
    out_a_yaml = tmp_path / "out_a.yml"
    out_a_report = tmp_path / "report_a.txt"

    with as_file(files("supy") / "sample_data" / "sample_config.yml") as std_path:
        report_a = run_phase_a(
            str(cfg_path),
            str(std_path),
            str(out_a_yaml),
            str(out_a_report),
            mode="public",
            silent=True,
            forcing="off",
        )
        assert not report_a.has_errors, report_a.issues
        a_props = yaml.safe_load(out_a_yaml.read_text(encoding="utf-8"))["sites"][0][
            "properties"
        ]
        assert (
            tuple(a_props["building_archetype"]["profile_metabolism"]["working_day"])
            == CANONICAL_TEN_MINUTE_KEYS
        )
        assert a_props["building_archetype"]["profile_metabolism"]["working_day"][
            "1"
        ] == pytest.approx(val_1)
        assert (
            tuple(
                a_props["building_archetype"][
                    "profile_setpoint_temperature_heating_air"
                ]["holiday"]
            )
            == CANONICAL_TEN_MINUTE_KEYS
        )
        assert (
            tuple(a_props["stebbs"]["profile_appliance"]["working_day"])
            == CANONICAL_TEN_MINUTE_KEYS
        )

        report_b = run_phase_b(
            str(out_a_yaml),
            str(cfg_path),
            str(std_path),
            str(tmp_path / "out_b.yml"),
            str(tmp_path / "report_b.txt"),
            str(out_a_report),
            phase_a_performed=True,
            mode="public",
            silent=True,
            science_fixes="off",
        )
        assert not report_b.has_errors, report_b.issues

    # Run Phase C on both Phase A output (chained A -> C) and raw zero-padded input (standalone C).
    report_c_chained = run_phase_c(
        str(out_a_yaml),
        str(tmp_path / "out_c.yml"),
        str(tmp_path / "report_c.txt"),
        mode="public",
        silent=True,
    )
    assert not report_c_chained.has_errors, report_c_chained.issues

    out_c_direct = tmp_path / "out_c_direct.yml"
    report_c_direct = run_phase_c(
        str(cfg_path),
        str(out_c_direct),
        str(tmp_path / "report_c_direct.txt"),
        mode="public",
        silent=True,
    )
    assert not report_c_direct.has_errors, report_c_direct.issues
    data_c = yaml.safe_load(out_c_direct.read_text(encoding="utf-8"))
    assert (
        tuple(
            data_c["sites"][0]["properties"]["building_archetype"][
                "profile_metabolism"
            ]["working_day"]
        )
        == CANONICAL_TEN_MINUTE_KEYS
    )


def test_phase_a_to_c_does_not_pad_null_on_malformed_ten_minute_key(
    tmp_path: Path,
) -> None:
    """Phase A leaves malformed 10-min mappings unpadded so Phase C rejects the real bad key."""
    raw = _load_sample_config_dict()
    wd = raw["sites"][0]["properties"]["building_archetype"]["profile_metabolism"][
        "working_day"
    ]
    val_1 = wd.pop("1")
    wd["+1"] = val_1
    in_yaml = tmp_path / "malformed_10min.yml"
    in_yaml.write_text(yaml.safe_dump(raw, sort_keys=False), encoding="utf-8")

    out_a_yaml = tmp_path / "updatedA_malformed_10min.yml"
    with as_file(files("supy") / "sample_data" / "sample_config.yml") as std_path:
        run_phase_a(
            str(in_yaml),
            str(std_path),
            str(out_a_yaml),
            str(tmp_path / "reportA_malformed_10min.txt"),
            mode="public",
            silent=True,
            forcing="off",
        )
    data_a = yaml.safe_load(out_a_yaml.read_text(encoding="utf-8"))
    a_wd = data_a["sites"][0]["properties"]["building_archetype"]["profile_metabolism"][
        "working_day"
    ]
    assert "1" not in a_wd
    assert "+1" in a_wd

    report_c = run_phase_c(
        str(out_a_yaml),
        str(tmp_path / "updatedC_malformed_10min.yml"),
        str(tmp_path / "reportC_malformed_10min.txt"),
        mode="public",
        silent=True,
    )
    assert report_c.has_errors
    assert any(
        "Invalid 10-minute profile key" in issue.message for issue in report_c.issues
    )
