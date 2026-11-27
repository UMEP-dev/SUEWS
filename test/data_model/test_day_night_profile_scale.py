"""Validation, default-scale, and schema-migration tests for #2043.

Population (``popprof_24hr``) and human-activity
(``profile_human_activity_24hr``) 24-hour profiles are read raw by the
anthropogenic emissions kernel on a fixed ``[1.0, 2.0]`` scale
(``1.0`` = night, ``2.0`` = day, values in between = transition).
"""

from __future__ import annotations

from copy import deepcopy
from importlib.resources import files
from pathlib import Path

from pydantic import ValidationError
import pytest
import yaml

from supy._run_rust import config_to_engine_yaml  # ruff: ignore[import-private-name]
from supy.data_model import SUEWSConfig
from supy.data_model.configuration import CURRENT_SCHEMA_VERSION, SchemaMigrator
from supy.data_model.core.human_activity import AnthropogenicHeat, CO2Params
from supy.util.converter.yaml_upgrade import YamlUpgradeError, upgrade_yaml

pytestmark = [
    pytest.mark.api,
    pytest.mark.cfg,
    pytest.mark.core,
    pytest.mark.contract("C-VALID", "C-MIGRATE", "C-EMIT"),
]

_EXPECTED_POP_801 = [1.0] * 5 + [1.5] + [2.0] * 15 + [1.5] + [1.0] * 2
_EXPECTED_POP_802 = [1.0] * 8 + [1.5] + [2.0] * 13 + [1.5] + [1.0]
_EXPECTED_ACT_55663 = [1.0] * 7 + [1.5] + [2.0] * 14 + [1.5] + [1.0]
_EXPECTED_ACT_55664 = [1.0] * 8 + [1.5] + [2.0] * 13 + [1.5] + [1.0]

_LEGACY_661 = {
    str(h): v
    for h, v in enumerate(
        (
            0.57, 0.45, 0.43, 0.4, 0.4, 0.45, 0.71, 1.2,
            1.44, 1.29, 1.28, 1.31, 1.3, 1.32, 1.35, 1.44,
            1.51, 1.41, 1.14, 0.99, 0.86, 0.85, 0.8, 0.7,
        ),
        start=1,
    )
}
_LEGACY_662 = {
    str(h): v
    for h, v in enumerate(
        (
            0.65, 0.49, 0.46, 0.47, 0.47, 0.53, 0.7, 1.13,
            1.37, 1.37, 1.3, 1.37, 1.33, 1.3, 1.27, 1.36,
            1.44, 1.3, 1.1, 0.98, 0.84, 0.9, 0.87, 0.74,
        ),
        start=1,
    )
}


def _uniform_profile(value: float) -> dict[str, dict[str, float]]:
    hours = {str(h): value for h in range(1, 25)}
    return {"working_day": hours.copy(), "holiday": hours.copy()}


def _load_sample_raw() -> dict:
    text = (
        files("supy")
        .joinpath("sample_data", "sample_config.yml")
        .read_text(encoding="utf-8")
    )
    return yaml.safe_load(text)


def test_omitted_day_night_profiles_default_to_uniform_one() -> None:
    """Omitted popprof_24hr and profile_human_activity_24hr default to uniform 1.0."""
    heat = AnthropogenicHeat()
    co2 = CO2Params()

    for profile in (heat.popprof_24hr, co2.profile_human_activity_24hr):
        assert profile is not None
        for day_type in ("working_day", "holiday"):
            day_map = getattr(profile, day_type)
            assert len(day_map) == 24
            assert all(day_map[str(h)] == pytest.approx(1.0) for h in range(1, 25))

    # Non-day-night hourly profiles retain the generic 1/24 uniform default.
    for day_type in ("working_day", "holiday"):
        ah_map = getattr(heat.ahprof_24hr, day_type)
        tr_map = getattr(co2.profile_traffic_24hr, day_type)
        assert all(ah_map[str(h)] == pytest.approx(1.0 / 24.0) for h in range(1, 25))
        assert all(tr_map[str(h)] == pytest.approx(1.0 / 24.0) for h in range(1, 25))

    # Engine YAML emission preserves uniform 1.0 when omitted from raw YAML.
    raw = _load_sample_raw()
    raw["sites"][0]["properties"]["anthropogenic_emissions"]["heat"].pop(
        "popprof_24hr", None
    )
    raw["sites"][0]["properties"]["anthropogenic_emissions"]["co2"].pop(
        "profile_human_activity_24hr", None
    )
    cfg = SUEWSConfig.from_dict(raw)
    resolved = yaml.safe_load(config_to_engine_yaml(cfg, for_file_output=False))
    anthro = resolved["sites"][0]["properties"]["anthropogenic_emissions"]
    for prof in (
        anthro["heat"]["popprof_24hr"],
        anthro["co2"]["profile_human_activity_24hr"],
    ):
        for day_type in ("working_day", "holiday"):
            assert all(
                prof[day_type][str(h)] == pytest.approx(1.0) for h in range(1, 25)
            )


def test_shipped_sample_config_carries_canonical_day_night_profiles() -> None:
    """Shipped sample_config.yml uses legacy 801/802 and 55663/55664 on [1.0, 2.0]."""
    raw = _load_sample_raw()
    assert raw["schema_version"] == CURRENT_SCHEMA_VERSION
    anthro = raw["sites"][0]["properties"]["anthropogenic_emissions"]
    pop = anthro["heat"]["popprof_24hr"]
    act = anthro["co2"]["profile_human_activity_24hr"]

    assert [pop["working_day"][str(h)] for h in range(1, 25)] == _EXPECTED_POP_801
    assert [pop["holiday"][str(h)] for h in range(1, 25)] == _EXPECTED_POP_802
    assert [act["working_day"][str(h)] for h in range(1, 25)] == _EXPECTED_ACT_55663
    assert [act["holiday"][str(h)] for h in range(1, 25)] == _EXPECTED_ACT_55664


@pytest.mark.parametrize("valid_value", [1.0, 1.5, 2.0])
def test_day_night_profiles_accept_boundary_and_transition_values(
    valid_value: float,
) -> None:
    """Values across [1.0, 2.0] inclusive are accepted on both profile fields."""
    heat = AnthropogenicHeat(popprof_24hr=_uniform_profile(valid_value))
    co2 = CO2Params(profile_human_activity_24hr=_uniform_profile(valid_value))
    assert heat.popprof_24hr.working_day["1"] == pytest.approx(valid_value)
    assert co2.profile_human_activity_24hr.holiday["24"] == pytest.approx(valid_value)


@pytest.mark.parametrize(
    "bad_value",
    [1.0 / 24.0, 0.4, 0.9999, 0.0, -1.0, 2.0001, 2.5, float("nan"), float("inf")],
)
def test_day_night_profiles_reject_out_of_range_values(bad_value: float) -> None:
    """Values outside [1.0, 2.0] or non-finite raise ValidationError."""
    bad = _uniform_profile(1.5)
    bad["holiday"]["12"] = bad_value

    with pytest.raises(ValidationError, match=r"popprof_24hr.*\[1\.0, 2\.0\]"):
        AnthropogenicHeat(popprof_24hr=bad)

    with pytest.raises(
        ValidationError, match=r"profile_human_activity_24hr.*\[1\.0, 2\.0\]"
    ):
        CO2Params(profile_human_activity_24hr=bad)


@pytest.mark.parametrize(
    ("section", "field", "bad_value"),
    [
        ("heat", "popprof_24hr", 0.4),
        ("heat", "popprof_24hr", 2.2),
        ("co2", "profile_human_activity_24hr", 1.0 / 24.0),
        ("co2", "profile_human_activity_24hr", True),
    ],
)
def test_live_mutation_caught_by_config_validation(
    section: str, field: str, bad_value: object
) -> None:
    """Post-construction mutations outside [1.0, 2.0] fail validate_hourly_profile_hours."""
    cfg = SUEWSConfig.from_dict(_load_sample_raw())
    anthro = cfg.sites[0].properties.anthropogenic_emissions
    profile = getattr(getattr(anthro, section), field)
    profile.working_day["8"] = bad_value

    with pytest.raises(ValueError, match=r"\[1\.0, 2\.0\]"):
        cfg.validate_hourly_profile_hours()


def test_dev23_to_dev24_migration_handles_valid_default_shipped_and_custom(
    tmp_path: Path, capsys
) -> None:
    """Schema migration 2026.6.dev23 -> 2026.6.dev24 covers all four cases cleanly."""
    migrator = SchemaMigrator()
    base_dev23 = _load_sample_raw()
    base_dev23["schema_version"] = "2026.6.dev23"
    capsys.readouterr()

    # 1. Valid [1.0, 2.0] profiles remain untouched and emit no profile rewrite logs.
    migrated_valid = migrator.migrate(deepcopy(base_dev23), to_version="2026.6.dev24")
    err_valid = capsys.readouterr().err
    assert "popprof_24hr" not in err_valid
    assert "profile_human_activity_24hr" not in err_valid
    assert migrated_valid["schema_version"] == "2026.6.dev24"
    assert (
        migrated_valid["sites"][0]["properties"]["anthropogenic_emissions"]["heat"][
            "popprof_24hr"
        ]
        == base_dev23["sites"][0]["properties"]["anthropogenic_emissions"]["heat"][
            "popprof_24hr"
        ]
    )

    # Re-migrating an already-dev24 config is idempotent and silent.
    remigrated = migrator.migrate(deepcopy(migrated_valid), to_version="2026.6.dev24")
    assert remigrated == migrated_valid
    assert capsys.readouterr().err == ""

    # 2. Legacy uniform 1/24 default upgrades to uniform 1.0 with an explicit log.
    cfg_uniform = deepcopy(base_dev23)
    anthro_u = cfg_uniform["sites"][0]["properties"]["anthropogenic_emissions"]
    anthro_u["heat"]["popprof_24hr"] = _uniform_profile(1.0 / 24.0)
    anthro_u["co2"]["profile_human_activity_24hr"] = _uniform_profile(1.0 / 24.0)

    # Historical target 2026.6.dev23 leaves values unchanged.
    stayed_dev23 = migrator.migrate(deepcopy(cfg_uniform), to_version="2026.6.dev23")
    assert stayed_dev23["schema_version"] == "2026.6.dev23"
    assert stayed_dev23["sites"][0]["properties"]["anthropogenic_emissions"]["heat"][
        "popprof_24hr"
    ]["working_day"]["1"] == pytest.approx(1.0 / 24.0)
    capsys.readouterr()

    upgraded_u = migrator.migrate(deepcopy(cfg_uniform), to_version="2026.6.dev24")
    err_u = capsys.readouterr().err
    assert "legacy uniform 1/24 default to uniform 1.0" in err_u
    for prof in (
        upgraded_u["sites"][0]["properties"]["anthropogenic_emissions"]["heat"][
            "popprof_24hr"
        ],
        upgraded_u["sites"][0]["properties"]["anthropogenic_emissions"]["co2"][
            "profile_human_activity_24hr"
        ],
    ):
        for day_type in ("working_day", "holiday"):
            assert all(prof[day_type][str(h)] == 1.0 for h in range(1, 25))

    # 3. Known shipped London QF profile (661/662) upgrades to 801/802 and 55663/55664.
    cfg_shipped = deepcopy(base_dev23)
    anthro_s = cfg_shipped["sites"][0]["properties"]["anthropogenic_emissions"]
    anthro_s["heat"]["popprof_24hr"] = {
        "working_day": _LEGACY_661.copy(),
        "holiday": _LEGACY_662.copy(),
    }
    anthro_s["co2"]["profile_human_activity_24hr"] = {
        "working_day": _LEGACY_661.copy(),
        "holiday": _LEGACY_661.copy(),
    }
    in_path = tmp_path / "shipped_dev23.yml"
    out_path = tmp_path / "shipped_dev24.yml"
    in_path.write_text(yaml.safe_dump(cfg_shipped, sort_keys=False), encoding="utf-8")
    upgrade_yaml(in_path, out_path)
    err_s = capsys.readouterr().err
    assert "legacy profile 801" in err_s
    assert "legacy profile 802" in err_s
    assert "legacy profile 55663" in err_s
    assert "legacy profile 55664" in err_s
    loaded = SUEWSConfig.from_yaml(str(out_path))
    assert loaded.schema_version == CURRENT_SCHEMA_VERSION
    pop_loaded = loaded.sites[0].properties.anthropogenic_emissions.heat.popprof_24hr
    act_loaded = (
        loaded.sites[0].properties.anthropogenic_emissions.co2.profile_human_activity_24hr
    )
    assert [pop_loaded.working_day[str(h)] for h in range(1, 25)] == _EXPECTED_POP_801
    assert [pop_loaded.holiday[str(h)] for h in range(1, 25)] == _EXPECTED_POP_802
    assert [act_loaded.working_day[str(h)] for h in range(1, 25)] == _EXPECTED_ACT_55663
    assert [act_loaded.holiday[str(h)] for h in range(1, 25)] == _EXPECTED_ACT_55664

    # 4. Arbitrary custom out-of-range user profile raises YamlUpgradeError.
    cfg_custom = deepcopy(base_dev23)
    cfg_custom["sites"][0]["properties"]["anthropogenic_emissions"]["heat"][
        "popprof_24hr"
    ]["working_day"]["4"] = 0.5
    with pytest.raises(YamlUpgradeError, match=r"outside the kernel's \[1\.0, 2\.0\]"):
        migrator.migrate(cfg_custom, to_version="2026.6.dev24")


@pytest.mark.parametrize("bad_bool", [True, False])
def test_ordinary_construction_rejects_booleans(bad_bool: bool) -> None:
    """Booleans are rejected at HourlyProfile construction before float coercion."""
    bad = _uniform_profile(1.0)
    bad["working_day"]["1"] = bad_bool

    with pytest.raises(ValidationError, match="not bool"):
        AnthropogenicHeat(popprof_24hr=bad)

    with pytest.raises(ValidationError, match="not bool"):
        CO2Params(profile_human_activity_24hr=bad)


def test_migration_accepts_quoted_numeric_strings_and_rejects_colliding_keys() -> None:
    """Migration accepts quoted in-range numeric strings and rejects colliding hour keys."""
    migrator = SchemaMigrator()
    base_dev23 = _load_sample_raw()
    base_dev23["schema_version"] = "2026.6.dev23"

    # Quoted numeric strings in [1.0, 2.0] match HourlyProfile's coercion contract.
    cfg_quoted = deepcopy(base_dev23)
    quoted_profile = {
        "working_day": {str(h): "1.5" for h in range(1, 25)},
        "holiday": {str(h): "1.25" for h in range(1, 25)},
    }
    anthro_q = cfg_quoted["sites"][0]["properties"]["anthropogenic_emissions"]
    anthro_q["heat"]["popprof_24hr"] = deepcopy(quoted_profile)
    anthro_q["co2"]["profile_human_activity_24hr"] = deepcopy(quoted_profile)
    migrated_q = migrator.migrate(cfg_quoted, to_version="2026.6.dev24")
    loaded_q = SUEWSConfig.from_dict(migrated_q)
    assert loaded_q.sites[0].properties.anthropogenic_emissions.heat.popprof_24hr.working_day[
        "1"
    ] == pytest.approx(1.5)

    # Colliding hour keys ("1" and "01") must fail loudly rather than overwriting.
    cfg_collide = deepcopy(base_dev23)
    colliding_day = {str(h): 1.0 / 24.0 for h in range(1, 25)}
    colliding_day["1"] = 0.5
    colliding_day["01"] = 1.0 / 24.0
    cfg_collide["sites"][0]["properties"]["anthropogenic_emissions"]["heat"][
        "popprof_24hr"
    ]["working_day"] = colliding_day
    with pytest.raises(YamlUpgradeError, match="invalid or colliding hour keys"):
        migrator.migrate(cfg_collide, to_version="2026.6.dev24")


def test_schema_migrate_cli_validates_already_dev24_config(tmp_path: Path) -> None:
    """Public ``suews schema migrate`` checks day/night bounds on already-dev24 files."""
    from click.testing import CliRunner  # noqa: PLC0415

    from supy.cmd.schema_cli import cli as schema_cli  # noqa: PLC0415

    bad_dev24 = _load_sample_raw()
    bad_dev24["schema_version"] = CURRENT_SCHEMA_VERSION
    bad_dev24["sites"][0]["properties"]["anthropogenic_emissions"]["heat"][
        "popprof_24hr"
    ]["working_day"]["1"] = 0.5
    bad_path = tmp_path / "bad_dev24.yml"
    bad_path.write_text(yaml.safe_dump(bad_dev24, sort_keys=False), encoding="utf-8")

    runner = CliRunner()
    result = runner.invoke(
        schema_cli,
        [
            "--quiet",
            "migrate",
            str(bad_path),
            "--target-version",
            CURRENT_SCHEMA_VERSION,
            "--dry-run",
        ],
    )
    assert result.exit_code != 0
    assert "outside the kernel's [1.0, 2.0]" in result.output


@pytest.mark.parametrize("var_name", ["popprof_24hr", "humactivity_24hr"])
@pytest.mark.parametrize("bad_val", [-999.0, float("nan"), float("-inf"), float("inf"), 0.5, 2.5])
def test_check_state_enforces_finite_day_night_bounds(
    var_name: str, bad_val: float
) -> None:
    """check_state rejects missing/sentinel/non-finite/out-of-range values when active."""
    import numpy as np  # noqa: PLC0415

    from supy._check import check_state  # noqa: PLC0415

    cfg = SUEWSConfig.from_dict(_load_sample_raw())
    df_state = cfg.to_df_state()
    baseline_issues = check_state(df_state, fix=False) or []
    assert not any(var_name in msg for msg in baseline_issues)

    df_bad = df_state.copy()
    df_bad[(var_name, "(0, 0)")] = bad_val
    issues = check_state(df_bad, fix=False) or []
    assert any(var_name in msg for msg in issues)

    # When emissionsmethod == 0 (inactive), exact -999 and NaN are permitted,
    # whereas +/-inf and finite out-of-range values remain rejected.
    df_inactive = df_bad.copy()
    df_inactive[("emissionsmethod", "0")] = 0
    inactive_issues = check_state(df_inactive, fix=False) or []
    if bad_val == -999.0 or np.isnan(bad_val):
        assert not any(var_name in msg for msg in inactive_issues)
    else:
        assert any(var_name in msg for msg in inactive_issues)


@pytest.mark.parametrize("validate_profiles", [False, True])
def test_legacy_2016a_conversion_materialises_day_night_default_profile(
    tmp_path: Path, validate_profiles: bool
) -> None:
    """Legacy 2016a->2025a conversion materialises finite 1.0 profile 99901 under both flags."""
    import numpy as np  # noqa: PLC0415

    from supy._load import load_InitialCond_grid_df  # noqa: PLC0415
    from supy.util.converter.table import convert_table  # noqa: PLC0415

    fixture_2016a = (
        Path(__file__).resolve().parents[1] / "fixtures" / "legacy_format" / "2016a"
    )
    if not (fixture_2016a / "RunControl.nml").exists():
        pytest.skip("legacy 2016a fixture not available")

    dest = tmp_path / f"converted_vp_{validate_profiles}"
    convert_table(
        str(fixture_2016a),
        str(dest),
        "2016a",
        "2025a",
        validate_profiles=validate_profiles,
    )

    profiles_path = dest / "Input" / "SUEWS_Profiles.txt"
    if not profiles_path.exists():
        profiles_path = dest / "SUEWS_Profiles.txt"
    profiles_text = profiles_path.read_text(encoding="utf-8")
    assert "99901" in profiles_text
    assert "nan" not in profiles_text.lower()

    df_state = load_InitialCond_grid_df(dest / "RunControl.nml")
    for var_name in ("popprof_24hr", "humactivity_24hr"):
        vals = df_state[var_name].to_numpy(dtype=float)
        assert np.all(np.isfinite(vals))
        assert np.all((vals >= 1.0) & (vals <= 2.0))
