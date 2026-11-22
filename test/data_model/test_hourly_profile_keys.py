"""Contract tests for 24-hour profile key normalisation and strict rejection (#2098)."""

from __future__ import annotations

from copy import deepcopy
from importlib.resources import as_file, files
from pathlib import Path

from click.testing import CliRunner
from pydantic import ValidationError
import pytest
import yaml

from supy import SUEWSSimulation
from supy._run_rust import config_to_engine_yaml  # ruff: ignore[import-private-name]
from supy.cmd.validate_config import cli as validate_cli
from supy.data_model import HourlyProfile, SUEWSConfig
from supy.data_model.core.profile import (
    CANONICAL_HOUR_KEYS,
    normalise_hourly_profile_key,
    normalise_hourly_profile_mapping,
)
from supy.data_model.validation.pipeline.orchestrator import (
    run_phase_a,
    run_phase_b,
    run_phase_c,
)

pytestmark = pytest.mark.api

SEVEN_PROFILE_PATHS: tuple[tuple[str, ...], ...] = (
    ("snow", "snow_profile_24hr"),
    ("irrigation", "wuprofa_24hr"),
    ("irrigation", "wuprofm_24hr"),
    ("anthropogenic_emissions", "heat", "ahprof_24hr"),
    ("anthropogenic_emissions", "heat", "popprof_24hr"),
    ("anthropogenic_emissions", "co2", "profile_traffic_24hr"),
    ("anthropogenic_emissions", "co2", "profile_human_activity_24hr"),
)

MALFORMED_HOUR_KEYS: tuple[object, ...] = (
    "+1",
    "-1",
    " 1",
    "1 ",
    "\t1",
    "1\n",
    "1.0",
    "0_1",
    "1_0",
    "\u0661",  # Arabic-Indic digit 1
    "\uff10",  # Fullwidth digit 0
    "\uff11",  # Fullwidth digit 1
    "0",
    "00",
    "000",
    "25",
    "025",
    "",
    "abc",
    0,
    -1,
    25,
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
    ("24", "024"),
)


def _load_sample_dict() -> dict:
    """Load the packaged sample configuration as a fresh mutable mapping."""
    with as_file(files("supy") / "sample_data" / "sample_config.yml") as path_cfg:
        return yaml.safe_load(path_cfg.read_text(encoding="utf-8"))


def _make_zero_padded_hours(base: float = 0.0) -> dict[object, float]:
    """Build a 24-hour mapping mixing zero-padded strings, integers, and canonical keys."""
    out: dict[object, float] = {}
    for hour in range(1, 25):
        if hour == 1:
            key: object = "001"
        elif hour <= 9:
            key = f"{hour:02d}"
        elif hour <= 15:
            key = hour
        elif hour == 24:
            key = "024"
        else:
            key = str(hour)
        out[key] = round(base + hour * 0.01, 4)
    return out


def _get_nested(mapping: dict, path: tuple[str, ...]) -> dict:
    """Return a nested dict along *path*."""
    node = mapping
    for part in path:
        node = node[part]
    return node


def _set_nested(mapping: dict, path: tuple[str, ...], value: object) -> None:
    """Assign *value* at *path* inside *mapping*."""
    node = mapping
    for part in path[:-1]:
        node = node[part]
    node[path[-1]] = value


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
        ("24", "24"),
        ("024", "24"),
        (1, "1"),
        (9, "9"),
        (24, "24"),
    ],
)
def test_normalise_hourly_profile_key_accepts_canonical_zero_padded_and_int(
    raw_key: object, expected: str
) -> None:
    """Zero-padded ASCII digits and plain integers 1..24 map to canonical strings."""
    assert normalise_hourly_profile_key(raw_key) == expected


@pytest.mark.parametrize("bad_key", MALFORMED_HOUR_KEYS)
def test_normalise_hourly_profile_key_rejects_malformed(bad_key: object) -> None:
    """Signed, padded, float, bool, non-ASCII, and out-of-range keys raise ValueError."""
    with pytest.raises(ValueError, match="Invalid 24-hour profile key"):
        normalise_hourly_profile_key(bad_key)


@pytest.mark.parametrize(("key_a", "key_b"), COLLISION_KEY_PAIRS)
def test_normalise_hourly_profile_mapping_rejects_collisions(
    key_a: object, key_b: object
) -> None:
    """Two keys that normalise to the same canonical hour raise ValueError."""
    raw = {str(h): 0.04 for h in range(1, 25)}
    canon = normalise_hourly_profile_key(key_a)
    raw.pop(canon, None)
    raw[key_a] = 0.1
    raw[key_b] = 0.2
    with pytest.raises(ValueError, match="Duplicate 24-hour profile key"):
        normalise_hourly_profile_mapping(raw)


def test_hourly_profile_direct_normalises_and_orders_keys() -> None:
    """HourlyProfile normalises zero-padded and integer keys to canonical '1'..'24'."""
    wd = _make_zero_padded_hours(base=1.0)
    hol = _make_zero_padded_hours(base=2.0)
    profile = HourlyProfile(working_day=wd, holiday=hol)

    assert tuple(profile.working_day.keys()) == CANONICAL_HOUR_KEYS
    assert tuple(profile.holiday.keys()) == CANONICAL_HOUR_KEYS
    assert profile.working_day["1"] == pytest.approx(1.01)
    assert profile.working_day["9"] == pytest.approx(1.09)
    assert profile.working_day["24"] == pytest.approx(1.24)
    assert profile.holiday["1"] == pytest.approx(2.01)


@pytest.mark.parametrize("bad_key", MALFORMED_HOUR_KEYS)
def test_hourly_profile_direct_rejects_malformed_keys(bad_key: object) -> None:
    """Direct HourlyProfile validation rejects every malformed key form."""
    day = {str(h): 1.0 / 24.0 for h in range(2, 25)}
    day[bad_key] = 1.0 / 24.0
    with pytest.raises(ValidationError, match="Invalid 24-hour profile key"):
        HourlyProfile(working_day=day, holiday={str(h): 1.0 / 24.0 for h in range(1, 25)})


@pytest.mark.parametrize(("key_a", "key_b"), COLLISION_KEY_PAIRS)
def test_hourly_profile_direct_rejects_collisions(
    key_a: object, key_b: object
) -> None:
    """Direct HourlyProfile validation rejects canonical key collisions."""
    day: dict[object, float] = {str(h): 1.0 / 24.0 for h in range(1, 25)}
    day.pop(normalise_hourly_profile_key(key_a), None)
    day[key_a] = 0.1
    day[key_b] = 0.2
    with pytest.raises(ValidationError, match="Duplicate 24-hour profile key"):
        HourlyProfile(working_day=day, holiday={str(h): 1.0 / 24.0 for h in range(1, 25)})


def test_all_seven_profiles_normalise_across_config_export_and_simulation(
    tmp_path: Path,
) -> None:
    """Zero-padded keys across all 7 profiles normalise on load, YAML export, and engine export."""
    raw = _load_sample_dict()
    props = raw["sites"][0]["properties"]
    for idx, rel_path in enumerate(SEVEN_PROFILE_PATHS, start=1):
        _set_nested(
            props,
            rel_path,
            {
                "working_day": _make_zero_padded_hours(base=float(idx)),
                "holiday": _make_zero_padded_hours(base=float(idx) + 0.5),
            },
        )

    # 1. SUEWSConfig.from_dict
    cfg_from_dict = SUEWSConfig.from_dict(deepcopy(raw))

    # 2. SUEWSConfig.from_yaml
    yaml_in = tmp_path / "zero_padded.yml"
    yaml_in.write_text(yaml.safe_dump(raw, sort_keys=False), encoding="utf-8")
    cfg_from_yaml = SUEWSConfig.from_yaml(str(yaml_in))

    # 3. SUEWSSimulation(yaml_path) & update_config(dict)
    sim = SUEWSSimulation().update_config(str(yaml_in), auto_load_forcing=False)
    sim_direct_raw = deepcopy(raw)
    sim_direct_raw["model"]["control"]["forcing"]["file"] = None
    sim_direct = SUEWSSimulation(sim_direct_raw)
    assert tuple(
        sim_direct.config.sites[0].properties.snow.snow_profile_24hr.working_day.keys()
    ) == CANONICAL_HOUR_KEYS
    updated_sites = deepcopy(raw["sites"])
    updated_sites[0]["properties"]["snow"]["snow_profile_24hr"] = {
        "working_day": _make_zero_padded_hours(base=9.0),
        "holiday": _make_zero_padded_hours(base=9.5),
    }
    sim.update_config({"sites": updated_sites}, auto_load_forcing=False)
    assert tuple(
        sim.config.sites[0].properties.snow.snow_profile_24hr.working_day.keys()
    ) == CANONICAL_HOUR_KEYS
    assert sim.config.sites[0].properties.snow.snow_profile_24hr.working_day[
        "1"
    ] == pytest.approx(9.01)

    # 4. SUEWSConfig.to_yaml and config_to_engine_yaml emit canonical "1".."24"
    yaml_out = tmp_path / "exported.yml"
    cfg_from_yaml.to_yaml(str(yaml_out))
    exported_data = yaml.safe_load(yaml_out.read_text(encoding="utf-8"))
    engine_data = yaml.safe_load(config_to_engine_yaml(cfg_from_dict, for_file_output=False))

    for idx, rel_path in enumerate(SEVEN_PROFILE_PATHS, start=1):
        for dumped in (exported_data, engine_data):
            prof = _get_nested(dumped["sites"][0]["properties"], rel_path)
            assert tuple(prof["working_day"].keys()) == CANONICAL_HOUR_KEYS
            assert tuple(prof["holiday"].keys()) == CANONICAL_HOUR_KEYS
            assert prof["working_day"]["1"] == pytest.approx(idx + 0.01)
            assert prof["working_day"]["9"] == pytest.approx(idx + 0.09)
            assert prof["working_day"]["24"] == pytest.approx(idx + 0.24)
            assert prof["holiday"]["1"] == pytest.approx(idx + 0.51)

    # 5. to_df_state preserves canonical hourly values cleanly
    df_state = cfg_from_yaml.to_df_state()
    assert df_state.loc[1, ("snowprof_24hr", "(0, 0)")] == pytest.approx(1.01)
    assert df_state.loc[1, ("snowprof_24hr", "(23, 1)")] == pytest.approx(1.74)


@pytest.mark.parametrize("rel_path", SEVEN_PROFILE_PATHS)
def test_all_seven_profiles_reject_malformed_colliding_and_incomplete_keys(
    rel_path: tuple[str, ...],
) -> None:
    """Every 24-hour profile field rejects malformed keys, collisions, and missing hours."""
    # Malformed "+1"
    raw_bad = _load_sample_dict()
    wd_bad = _get_nested(raw_bad["sites"][0]["properties"], rel_path)["working_day"]
    wd_bad.pop("1")
    wd_bad["+1"] = 0.04
    with pytest.raises(ValueError, match="Invalid 24-hour profile key"):
        SUEWSConfig.from_dict(raw_bad)

    # Collision "1" + "01"
    raw_dup = _load_sample_dict()
    wd_dup = _get_nested(raw_dup["sites"][0]["properties"], rel_path)["working_day"]
    wd_dup["01"] = 0.05
    with pytest.raises(ValueError, match="Duplicate 24-hour profile key"):
        SUEWSConfig.from_dict(raw_dup)

    # Completeness (missing hour 24)
    raw_missing = _load_sample_dict()
    wd_missing = _get_nested(raw_missing["sites"][0]["properties"], rel_path)["working_day"]
    wd_missing.pop("24")
    with pytest.raises(ValueError, match=r"missing hours:\s*\[24\]"):
        SUEWSConfig.from_dict(raw_missing)


def test_post_construction_mutation_validated_before_engine_export() -> None:
    """Post-construction mutations are normalised or rejected by validate_for_engine()."""
    cfg = SUEWSConfig.from_dict(_load_sample_dict())

    # Valid zero-padded mutation is normalised before engine export
    cfg.sites[0].properties.snow.snow_profile_24hr.working_day = _make_zero_padded_hours(
        base=3.0
    )
    engine_data = yaml.safe_load(config_to_engine_yaml(cfg, for_file_output=False))
    snow_wd = engine_data["sites"][0]["properties"]["snow"]["snow_profile_24hr"][
        "working_day"
    ]
    assert tuple(snow_wd.keys()) == CANONICAL_HOUR_KEYS
    assert snow_wd["1"] == pytest.approx(3.01)

    # Malformed mutation is rejected before engine export
    cfg.sites[0].properties.snow.snow_profile_24hr.working_day = {
        "+1" if h == 1 else str(h): 0.04 for h in range(1, 25)
    }
    with pytest.raises(ValueError, match="Invalid 24-hour profile key"):
        config_to_engine_yaml(cfg, for_file_output=False)

    # Collision mutation is rejected before engine export
    colliding = {str(h): 0.04 for h in range(1, 25)}
    colliding["01"] = 0.09
    cfg.sites[0].properties.snow.snow_profile_24hr.working_day = colliding
    with pytest.raises(ValueError, match="Duplicate 24-hour profile key"):
        config_to_engine_yaml(cfg, for_file_output=False)


def test_post_construction_mutation_validated_before_to_yaml(tmp_path: Path) -> None:
    """Post-construction mutations are normalised or rejected before ``to_yaml()``."""
    cfg = SUEWSConfig.from_dict(_load_sample_dict())

    # Valid zero-padded mutation is normalised to canonical "1".."24" on YAML export
    cfg.sites[0].properties.snow.snow_profile_24hr.working_day = _make_zero_padded_hours(
        base=4.0
    )
    valid_out = tmp_path / "valid_mutated.yml"
    cfg.to_yaml(str(valid_out))
    dumped = yaml.safe_load(valid_out.read_text(encoding="utf-8"))
    snow_wd = dumped["sites"][0]["properties"]["snow"]["snow_profile_24hr"][
        "working_day"
    ]
    assert tuple(snow_wd.keys()) == CANONICAL_HOUR_KEYS
    assert snow_wd["1"] == pytest.approx(4.01)
    assert snow_wd["24"] == pytest.approx(4.24)

    # Malformed mutation is rejected and no output file is written
    for idx, bad_key in enumerate(("+1", " 1", "0", "25")):
        cfg.sites[0].properties.snow.snow_profile_24hr.working_day = {
            bad_key if h == 1 else str(h): 0.04 for h in range(1, 25)
        }
        bad_out = tmp_path / f"bad_mutated_{idx}.yml"
        with pytest.raises(ValueError, match="Invalid 24-hour profile key"):
            cfg.to_yaml(str(bad_out))
        assert not bad_out.exists()

    # Collision mutation is rejected and no output file is written
    colliding = {str(h): 0.04 for h in range(1, 25)}
    colliding["01"] = 0.09
    cfg.sites[0].properties.snow.snow_profile_24hr.working_day = colliding
    dup_out = tmp_path / "dup_mutated.yml"
    with pytest.raises(ValueError, match="Duplicate 24-hour profile key"):
        cfg.to_yaml(str(dup_out))
    assert not dup_out.exists()

    # Incomplete mutation (missing hour 24) is rejected and no output file is written
    cfg.sites[0].properties.snow.snow_profile_24hr.working_day = {
        str(h): 0.04 for h in range(1, 24)
    }
    incomplete_out = tmp_path / "incomplete_mutated.yml"
    with pytest.raises(ValueError, match=r"missing hours:\s*\[24\]"):
        cfg.to_yaml(str(incomplete_out))
    assert not incomplete_out.exists()


def _prepare_zero_padded_sample_dir(tmp_path: Path) -> Path:
    """Copy packaged sample data and inject zero-padded 24-hour profile keys."""
    sample_dir = tmp_path / "sample_run"
    sample_dir.mkdir()
    trv_sample = files("supy") / "sample_data"
    for entry in trv_sample.iterdir():
        if entry.is_file():
            (sample_dir / entry.name).write_bytes(entry.read_bytes())

    cfg_path = sample_dir / "sample_config.yml"
    raw = yaml.safe_load(cfg_path.read_text(encoding="utf-8"))
    raw["sites"][0]["properties"]["anthropogenic_emissions"]["heat"]["ahprof_24hr"][
        "working_day"
    ] = _make_zero_padded_hours(base=0.5)
    raw["sites"][0]["properties"]["snow"]["snow_profile_24hr"][
        "holiday"
    ] = _make_zero_padded_hours(base=0.2)
    cfg_path.write_text(yaml.safe_dump(raw, sort_keys=False), encoding="utf-8")
    return cfg_path


def test_validation_pipeline_phases_normalise_zero_padded_keys(tmp_path: Path) -> None:
    """Validation phases A, B, and C export canonical '1'..'24' with no false alerts."""
    cfg_path = _prepare_zero_padded_sample_dir(tmp_path)
    out_a_yaml = tmp_path / "out_a.yml"
    out_a_report = tmp_path / "report_a.txt"
    out_b_yaml = tmp_path / "out_b.yml"
    out_b_report = tmp_path / "report_b.txt"

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
        data_a = yaml.safe_load(out_a_yaml.read_text(encoding="utf-8"))
        assert tuple(
            data_a["sites"][0]["properties"]["anthropogenic_emissions"]["heat"][
                "ahprof_24hr"
            ]["working_day"].keys()
        ) == CANONICAL_HOUR_KEYS
        assert tuple(
            data_a["sites"][0]["properties"]["snow"]["snow_profile_24hr"][
                "holiday"
            ].keys()
        ) == CANONICAL_HOUR_KEYS

        report_b = run_phase_b(
            str(cfg_path),
            str(cfg_path),
            str(std_path),
            str(out_b_yaml),
            str(out_b_report),
            str(out_a_report),
            phase_a_performed=False,
            mode="public",
            silent=True,
            science_fixes="off",
        )
        assert not report_b.has_errors, report_b.issues
        data_b = yaml.safe_load(out_b_yaml.read_text(encoding="utf-8"))
        assert tuple(
            data_b["sites"][0]["properties"]["anthropogenic_emissions"]["heat"][
                "ahprof_24hr"
            ]["working_day"].keys()
        ) == CANONICAL_HOUR_KEYS

    out_c_yaml = tmp_path / "out_c.yml"
    out_c_report = tmp_path / "report_c.txt"
    report_c = run_phase_c(
        str(cfg_path), str(out_c_yaml), str(out_c_report), mode="public", silent=True
    )
    assert not report_c.has_errors, report_c.issues
    data_c = yaml.safe_load(out_c_yaml.read_text(encoding="utf-8"))
    assert tuple(
        data_c["sites"][0]["properties"]["anthropogenic_emissions"]["heat"][
            "ahprof_24hr"
        ]["working_day"].keys()
    ) == CANONICAL_HOUR_KEYS
    assert (
        "found missing in user YAML at level sites.1.properties.anthropogenic_emissions.heat.ahprof_24hr"
        not in out_c_report.read_text(encoding="utf-8")
    )


@pytest.mark.parametrize(
    ("label", "mutate"),
    [
        ("malformed", lambda wd: (wd.pop("1"), wd.__setitem__("+1", 0.5))),
        ("collision", lambda wd: wd.__setitem__("01", 0.9)),
    ],
)
def test_cli_validate_rejects_malformed_and_colliding_hour_keys(
    tmp_path: Path, label: str, mutate
) -> None:
    """CLI ``suews validate`` fails on malformed and colliding 24-hour profile keys."""
    bad_dir = tmp_path / f"cli_{label}"
    bad_dir.mkdir()
    bad_raw = _load_sample_dict()
    mutate(
        bad_raw["sites"][0]["properties"]["anthropogenic_emissions"]["heat"][
            "ahprof_24hr"
        ]["working_day"]
    )
    bad_cfg = bad_dir / "bad.yml"
    bad_cfg.write_text(yaml.safe_dump(bad_raw, sort_keys=False), encoding="utf-8")
    result = CliRunner().invoke(
        validate_cli,
        [str(bad_cfg), "--pipeline", "C", "--dry-run", "--format", "json"],
    )
    assert result.exit_code != 0, f"CLI must fail on {label} hour keys"


def test_phase_a_to_c_rejects_missing_hour_without_null_padding(
    tmp_path: Path,
) -> None:
    """Phase A must not pad a missing 24-hour slot with null across Phase A -> C."""
    raw = _load_sample_dict()
    raw["sites"][0]["properties"]["anthropogenic_emissions"]["heat"]["ahprof_24hr"][
        "working_day"
    ].pop("24")
    in_yaml = tmp_path / "missing_hour.yml"
    in_yaml.write_text(yaml.safe_dump(raw, sort_keys=False), encoding="utf-8")

    out_a_yaml = tmp_path / "updatedA_missing_hour.yml"
    with as_file(files("supy") / "sample_data" / "sample_config.yml") as std_path:
        run_phase_a(
            str(in_yaml),
            str(std_path),
            str(out_a_yaml),
            str(tmp_path / "reportA_missing_hour.txt"),
            mode="public",
            silent=True,
            forcing="off",
        )
    data_a = yaml.safe_load(out_a_yaml.read_text(encoding="utf-8"))
    ahprof_wd = data_a["sites"][0]["properties"]["anthropogenic_emissions"]["heat"][
        "ahprof_24hr"
    ]["working_day"]
    assert "24" not in ahprof_wd

    report_c = run_phase_c(
        str(out_a_yaml),
        str(tmp_path / "updatedC_missing_hour.yml"),
        str(tmp_path / "reportC_missing_hour.txt"),
        mode="public",
        silent=True,
    )
    assert report_c.has_errors
    assert any("missing hours: [24]" in issue.message for issue in report_c.issues)


def test_phase_a_to_c_rejects_missing_day_type_without_null_padding(
    tmp_path: Path,
) -> None:
    """Phase A must not pad a missing day-type block with 24 null slots across A -> C."""
    raw = _load_sample_dict()
    raw["sites"][0]["properties"]["anthropogenic_emissions"]["heat"]["ahprof_24hr"].pop(
        "holiday"
    )
    in_yaml = tmp_path / "missing_day.yml"
    in_yaml.write_text(yaml.safe_dump(raw, sort_keys=False), encoding="utf-8")

    out_a_yaml = tmp_path / "updatedA_missing_day.yml"
    with as_file(files("supy") / "sample_data" / "sample_config.yml") as std_path:
        run_phase_a(
            str(in_yaml),
            str(std_path),
            str(out_a_yaml),
            str(tmp_path / "reportA_missing_day.txt"),
            mode="public",
            silent=True,
            forcing="off",
        )
    data_a = yaml.safe_load(out_a_yaml.read_text(encoding="utf-8"))
    ahprof = data_a["sites"][0]["properties"]["anthropogenic_emissions"]["heat"][
        "ahprof_24hr"
    ]
    assert "holiday" not in ahprof

    report_c = run_phase_c(
        str(out_a_yaml),
        str(tmp_path / "updatedC_missing_day.yml"),
        str(tmp_path / "reportC_missing_day.txt"),
        mode="public",
        silent=True,
    )
    assert report_c.has_errors
