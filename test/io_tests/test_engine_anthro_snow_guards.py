"""End-to-end engine and data-model guards for snow and anthropogenic emissions (#2002)."""

from importlib import import_module
from importlib.resources import as_file, files
import os
from pathlib import Path
import subprocess

import pandas as pd
from pyarrow import ipc
import pytest
import yaml

from supy.data_model import SUEWSConfig
from supy.data_model.core.human_activity import AnthropogenicHeat
from supy.data_model.core.site import SnowParams

_run_rust = import_module("supy._run_rust")

_SNOW_DEFAULT_KEYS = (
    "water_holding_capacity_max",
    "water_holding_capacity_min",
    "narp_emissivity_snow",
    "precipitation_threshold_albedo_reset",
    "snow_albedo_max",
    "snow_albedo_min",
    "snow_depth_limit_building",
    "snow_depth_limit_paved",
    "tau_cold_snow",
    "tau_melting_snow",
    "tau_refreezing_snow",
    "temperature_melt_factor",
    "radiation_melt_factor",
)
_ANTHRO_HEAT_DAY_PROFILES = (
    "qf0_beu",
    "qf_a",
    "qf_b",
    "qf_c",
    "baset_cooling",
    "baset_heating",
    "ah_min",
    "ah_slope_cooling",
    "ah_slope_heating",
    "popdensdaytime",
)

pytestmark = [
    pytest.mark.api,
    pytest.mark.core,
    pytest.mark.contract("C-STRICT", "C-EMIT", "C-NATIVE-ERROR"),
]


def _locate_suews_engine() -> Path:
    """Return the bundled or locally built ``suews-engine`` binary."""
    binary_name = "suews-engine.exe" if os.name == "nt" else "suews-engine"
    bundled = files("supy").joinpath("bin").joinpath(binary_name)
    with as_file(bundled) as bundled_path:
        if bundled_path.exists():
            return Path(bundled_path)

    repo_root = Path(__file__).resolve().parents[2]
    dev_binary = repo_root / "src" / "suews_bridge" / "target" / "release" / binary_name
    if dev_binary.exists():
        return dev_binary
    pytest.skip("Rust CLI binary not found; build with: make bridge")


def _unwrap(value):
    return value["value"] if isinstance(value, dict) else value


def _prepare_resolved_run(
    run_dir: Path,
    *,
    emissions: int = 2,
    snow_use: int = 0,
    n_forcing_rows: int = 2,
    forcing_qf: float | None = None,
    mutator=None,
) -> Path:
    """Write a resolved YAML config and forcing slice into ``run_dir``."""
    sample_dir = files("supy").joinpath("sample_data")
    with sample_dir.joinpath("sample_config.yml").open(encoding="utf-8") as handle:
        cfg = yaml.safe_load(handle)

    control = cfg["model"]["control"]
    control["output"]["dir"] = str(run_dir)
    control["start_time"] = None
    control["end_time"] = None
    forcing_name = _unwrap(control["forcing"]["file"])
    assert isinstance(forcing_name, str) and forcing_name

    physics = cfg["model"]["physics"]
    physics.pop("snow", None)
    physics["emissions"] = {"value": emissions}
    physics["snow_use"] = {"value": snow_use}
    resolved_doc = yaml.safe_load(
        _run_rust.config_to_engine_yaml(SUEWSConfig.from_dict(cfg))
    )
    if mutator is not None:
        mutator(resolved_doc)

    forcing_raw = sample_dir.joinpath(forcing_name).read_text(encoding="utf-8")
    forcing_lines = [line for line in forcing_raw.splitlines() if line.strip()]
    qf_idx = forcing_lines[0].split().index("qf")
    out_lines = [forcing_lines[0]]
    for line in forcing_lines[1 : 1 + n_forcing_rows]:
        parts = line.split()
        if forcing_qf is not None:
            parts[qf_idx] = f"{forcing_qf:.3f}"
        out_lines.append(" ".join(parts))
    (run_dir / forcing_name).write_text("\n".join(out_lines) + "\n", encoding="utf-8")

    config_path = run_dir / "resolved_config.yml"
    config_path.write_text(
        yaml.safe_dump(resolved_doc, sort_keys=False), encoding="utf-8"
    )
    return config_path


def test_data_model_emits_required_snow_and_anthro_heat_defaults(tmp_path: Path):
    """Data model emits authoritative Snow and AnthropogenicHeat defaults when omitted (#2001)."""
    sample_dir = files("supy").joinpath("sample_data")
    sample_raw = yaml.safe_load(
        sample_dir.joinpath("sample_config.yml").read_text(encoding="utf-8")
    )
    sample_raw["model"]["control"]["output"]["dir"] = str(tmp_path)
    sample_raw["model"]["control"]["start_time"] = None
    sample_raw["model"]["control"]["end_time"] = None
    sample_raw["model"]["physics"].pop("snow", None)
    sample_raw["model"]["physics"]["snow_use"] = {"value": 1}
    sample_raw["model"]["physics"]["emissions"] = {"value": 3}

    props = sample_raw["sites"][0]["properties"]
    for key in _SNOW_DEFAULT_KEYS:
        props["snow"].pop(key, None)
    props["snow"].pop("snow_profile_24hr", None)
    for key in _ANTHRO_HEAT_DAY_PROFILES:
        props["anthropogenic_emissions"]["heat"].pop(key, None)
    props["anthropogenic_emissions"]["heat"].pop("popdensnighttime", None)
    props["anthropogenic_emissions"]["heat"].pop("ahprof_24hr", None)
    props["anthropogenic_emissions"]["heat"].pop("popprof_24hr", None)
    props["anthropogenic_emissions"]["co2"].pop("profile_traffic_24hr", None)
    props["anthropogenic_emissions"]["co2"].pop("profile_human_activity_24hr", None)

    resolved_text = _run_rust.config_to_engine_yaml(SUEWSConfig.from_dict(sample_raw))
    resolved_props = yaml.safe_load(resolved_text)["sites"][0]["properties"]

    snow_defaults = SnowParams()
    for key in _SNOW_DEFAULT_KEYS:
        assert key in resolved_props["snow"], f"snow.{key} missing from engine YAML"
        assert _unwrap(resolved_props["snow"][key]) == pytest.approx(
            _unwrap(getattr(snow_defaults, key))
        )

    heat_defaults = AnthropogenicHeat()
    resolved_heat = resolved_props["anthropogenic_emissions"]["heat"]
    for key in _ANTHRO_HEAT_DAY_PROFILES:
        dp = getattr(heat_defaults, key)
        assert _unwrap(resolved_heat[key]["working_day"]) == pytest.approx(
            _unwrap(dp.working_day)
        )
        assert _unwrap(resolved_heat[key]["holiday"]) == pytest.approx(
            _unwrap(dp.holiday)
        )
    assert _unwrap(resolved_heat["popdensnighttime"]) == pytest.approx(
        heat_defaults.popdensnighttime
    )

    resolved_co2 = resolved_props["anthropogenic_emissions"]["co2"]
    for profile_node, label in (
        (resolved_props["snow"]["snow_profile_24hr"], "snow.snow_profile_24hr"),
        (resolved_heat["ahprof_24hr"], "heat.ahprof_24hr"),
        (resolved_heat["popprof_24hr"], "heat.popprof_24hr"),
        (resolved_co2["profile_traffic_24hr"], "co2.profile_traffic_24hr"),
        (
            resolved_co2["profile_human_activity_24hr"],
            "co2.profile_human_activity_24hr",
        ),
    ):
        for day_key in ("working_day", "holiday"):
            assert day_key in profile_node, (
                f"{label}.{day_key} missing from engine YAML"
            )
            assert len(profile_node[day_key]) == 24
            for hour in range(1, 25):
                assert profile_node[day_key][str(hour)] == pytest.approx(1.0 / 24.0)

    forcing_name = _unwrap(sample_raw["model"]["control"]["forcing"]["file"])
    forcing_lines = [
        line
        for line in sample_dir.joinpath(forcing_name)
        .read_text(encoding="utf-8")
        .splitlines()
        if line.strip()
    ]
    (tmp_path / forcing_name).write_text(
        "\n".join(forcing_lines[:3]) + "\n", encoding="utf-8"
    )
    cfg_path = tmp_path / "default_emitted.yml"
    cfg_path.write_text(resolved_text, encoding="utf-8")
    result = subprocess.run(
        [str(_locate_suews_engine()), "run", str(cfg_path)],
        capture_output=True,
        text=True,
        timeout=60,
        check=False,
    )
    assert result.returncode == 0, (
        f"default-emitted snow/anthro config failed ({result.returncode}): {result.stderr}"
    )
    assert (tmp_path / "suews_output.arrow").is_file()


@pytest.mark.parametrize(
    ("emissions", "snow_use", "path_to_drop", "expected_path"),
    [
        (
            2,
            1,
            ("snow", "water_holding_capacity_max"),
            "properties.snow.water_holding_capacity_max",
        ),
        (
            2,
            1,
            ("snow", "temperature_rain_snow_threshold"),
            "properties.snow.temperature_rain_snow_threshold",
        ),
        (
            2,
            1,
            ("snow", "snow_density_min"),
            "properties.snow.snow_density_min",
        ),
        (
            2,
            1,
            ("snow", "snow_depth_limit_paved"),
            "properties.snow.snow_depth_limit_paved",
        ),
        (
            2,
            1,
            ("land_cover", "grass", "snowpack_limit"),
            "properties.land_cover.grass.snowpack_limit",
        ),
        (
            0,
            0,
            ("anthropogenic_emissions", "heat", "baset_heating", "working_day"),
            "properties.anthropogenic_emissions.heat.baset_heating.working_day",
        ),
        (
            2,
            0,
            ("anthropogenic_emissions", "heat", "qf_a", "working_day"),
            "properties.anthropogenic_emissions.heat.qf_a.working_day",
        ),
        (
            1,
            0,
            ("anthropogenic_emissions", "heat", "ah_slope_heating", "holiday"),
            "properties.anthropogenic_emissions.heat.ah_slope_heating.holiday",
        ),
        (
            3,
            0,
            ("anthropogenic_emissions", "heat", "ah_slope_cooling", "holiday"),
            "properties.anthropogenic_emissions.heat.ah_slope_cooling.holiday",
        ),
        (
            4,
            0,
            ("anthropogenic_emissions", "co2", "type_traffic_rate"),
            "properties.anthropogenic_emissions.co2.type_traffic_rate",
        ),
        (
            2,
            0,
            ("anthropogenic_emissions", "co2", "emission_factor_energy_vehicle"),
            "properties.anthropogenic_emissions.co2.emission_factor_energy_vehicle",
        ),
        (
            2,
            0,
            ("anthropogenic_emissions", "co2", "emission_heat_metabolism_min"),
            "properties.anthropogenic_emissions.co2.emission_heat_metabolism_min",
        ),
        (
            5,
            0,
            ("anthropogenic_emissions", "co2", "emission_factor_co2_fuel"),
            "properties.anthropogenic_emissions.co2.emission_factor_co2_fuel",
        ),
        (
            12,
            0,
            ("anthropogenic_emissions", "co2", "emission_factor_co2_fuel"),
            "properties.anthropogenic_emissions.co2.emission_factor_co2_fuel",
        ),
        (
            2,
            1,
            ("snow", "snow_profile_24hr"),
            "properties.snow.snow_profile_24hr",
        ),
        (
            2,
            0,
            ("anthropogenic_emissions", "heat", "ahprof_24hr", "working_day"),
            "properties.anthropogenic_emissions.heat.ahprof_24hr.working_day",
        ),
        (
            2,
            0,
            ("anthropogenic_emissions", "co2", "profile_traffic_24hr"),
            "properties.anthropogenic_emissions.co2.profile_traffic_24hr",
        ),
        (
            2,
            0,
            ("anthropogenic_emissions", "enddls"),
            "properties.anthropogenic_emissions.enddls",
        ),
    ],
)
def test_engine_rejects_missing_active_snow_and_anthro_fields(
    tmp_path: Path,
    emissions: int,
    snow_use: int,
    path_to_drop: tuple[str, ...],
    expected_path: str,
):
    """Standalone ``suews-engine`` rejects missing active snow or anthropogenic keys."""
    engine_bin = _locate_suews_engine()

    def mutate(doc: dict):
        node = doc["sites"][0]["properties"]
        for key in path_to_drop[:-1]:
            node = node[key]
        node.pop(path_to_drop[-1], None)

    config_path = _prepare_resolved_run(
        tmp_path,
        emissions=emissions,
        snow_use=snow_use,
        forcing_qf=10.0 if emissions == 0 else None,
        mutator=mutate,
    )
    result = subprocess.run(
        [str(engine_bin), "run", str(config_path)],
        capture_output=True,
        text=True,
        timeout=60,
        check=False,
    )
    assert result.returncode != 0, (
        f"missing {expected_path} must not exit 0 (stdout={result.stdout})"
    )
    assert expected_path in result.stderr, result.stderr
    assert not (tmp_path / "suews_output.arrow").exists()


def test_engine_accepts_omitted_inactive_snow_and_observed_anthro(tmp_path: Path):
    """Standalone ``suews-engine`` runs cleanly with ``snow_use=0`` and ``emissions=0``."""
    engine_bin = _locate_suews_engine()

    def mutate(doc: dict):
        props = doc["sites"][0]["properties"]
        for key in (
            *_SNOW_DEFAULT_KEYS,
            "temperature_rain_snow_threshold",
            "snow_density_min",
            "snow_density_max",
            "snow_profile_24hr",
        ):
            props["snow"].pop(key, None)
        for surface in ("paved", "bldgs", "evetr", "dectr", "grass", "bsoil", "water"):
            props["land_cover"][surface].pop("snowpack_limit", None)
        ae = props["anthropogenic_emissions"]
        ae.pop("startdls", None)
        ae.pop("enddls", None)
        for key in (
            "qf0_beu",
            "qf_a",
            "qf_b",
            "qf_c",
            "ah_min",
            "ah_slope_cooling",
            "ah_slope_heating",
            "popdensdaytime",
            "popdensnighttime",
            "ahprof_24hr",
            "popprof_24hr",
        ):
            ae["heat"].pop(key, None)
        for key in (
            "emission_factor_co2_fuel",
            "emission_factor_energy_vehicle",
            "emission_factor_co2_vehicle",
            "fraction_fossil_fuel_heating",
            "fraction_fossil_fuel_non_heating",
            "emission_co2_metabolism_max",
            "emission_heat_metabolism_max",
            "emission_co2_metabolism_min",
            "emission_heat_metabolism_min",
            "traffic_rate",
            "type_traffic_rate",
            "profile_traffic_24hr",
            "profile_human_activity_24hr",
        ):
            ae["co2"].pop(key, None)

    config_path = _prepare_resolved_run(
        tmp_path,
        emissions=0,
        snow_use=0,
        n_forcing_rows=24,
        forcing_qf=12.0,
        mutator=mutate,
    )
    result = subprocess.run(
        [str(engine_bin), "run", str(config_path)],
        capture_output=True,
        text=True,
        timeout=60,
        check=False,
    )
    assert result.returncode == 0, (
        f"omitted inactive snow/anthro fields failed ({result.returncode}): {result.stderr}"
    )
    assert (tmp_path / "suews_output.arrow").is_file()


def _read_arrow_df(arrow_path: Path) -> pd.DataFrame:
    """Read an Arrow IPC output file into a DataFrame and release the mmap."""
    with ipc.open_file(str(arrow_path)) as reader:
        return reader.read_all().to_pandas()


def test_engine_snow_disabled_positive_initial_snowpack_matches_zero_control(
    tmp_path: Path,
):
    """Snow-disabled run clears positive initial snowpack and omits snow scalars (#2002)."""
    engine_bin = _locate_suews_engine()

    ctrl_dir = tmp_path / "control"
    ctrl_dir.mkdir()
    ctrl_cfg = _prepare_resolved_run(
        ctrl_dir,
        emissions=2,
        snow_use=0,
        n_forcing_rows=12,
    )

    test_dir = tmp_path / "positive_snowpack_omitted_snow"
    test_dir.mkdir()

    def mutate(doc: dict):
        site = doc["sites"][0]
        site["initial_states"]["paved"]["snowpack"] = {"value": 5.0}
        props = site["properties"]
        for key in (
            *_SNOW_DEFAULT_KEYS,
            "temperature_rain_snow_threshold",
            "snow_density_min",
            "snow_density_max",
            "snow_profile_24hr",
        ):
            props["snow"].pop(key, None)
        for surface in ("paved", "bldgs", "evetr", "dectr", "grass", "bsoil", "water"):
            props["land_cover"][surface].pop("snowpack_limit", None)

    test_cfg = _prepare_resolved_run(
        test_dir,
        emissions=2,
        snow_use=0,
        n_forcing_rows=12,
        mutator=mutate,
    )

    for label, cfg_path in (("control", ctrl_cfg), ("test", test_cfg)):
        result = subprocess.run(
            [str(engine_bin), "run", str(cfg_path)],
            capture_output=True,
            text=True,
            timeout=60,
            check=False,
        )
        assert result.returncode == 0, (
            f"{label} snow_use=0 run failed ({result.returncode}): {result.stderr}"
        )

    pd.testing.assert_frame_equal(
        _read_arrow_df(test_dir / "suews_output.arrow"),
        _read_arrow_df(ctrl_dir / "suews_output.arrow"),
        check_exact=True,
    )


@pytest.mark.parametrize(
    ("emissions", "heat_drops", "co2_drops"),
    [
        (
            1,
            ("qf_a", "qf_b", "qf_c", "ah_slope_cooling"),
            ("traffic_rate", "type_traffic_rate"),
        ),
        (
            2,
            ("ah_min", "ah_slope_heating", "ah_slope_cooling"),
            ("traffic_rate", "type_traffic_rate"),
        ),
        (
            5,
            ("ah_min", "ah_slope_heating", "ah_slope_cooling"),
            (),
        ),
    ],
)
def test_engine_inactive_anthro_omissions_preserve_output(
    tmp_path: Path,
    emissions: int,
    heat_drops: tuple[str, ...],
    co2_drops: tuple[str, ...],
):
    """Omitting unused heat/traffic branches under emissions 1/2/5 preserves exact output."""
    engine_bin = _locate_suews_engine()

    ctrl_dir = tmp_path / f"ctrl_em{emissions}"
    ctrl_dir.mkdir()
    ctrl_cfg = _prepare_resolved_run(
        ctrl_dir,
        emissions=emissions,
        snow_use=0,
        n_forcing_rows=12,
    )

    test_dir = tmp_path / f"omitted_em{emissions}"
    test_dir.mkdir()

    def mutate(doc: dict):
        ae = doc["sites"][0]["properties"]["anthropogenic_emissions"]
        for key in heat_drops:
            ae["heat"].pop(key, None)
        for key in co2_drops:
            ae["co2"].pop(key, None)

    test_cfg = _prepare_resolved_run(
        test_dir,
        emissions=emissions,
        snow_use=0,
        n_forcing_rows=12,
        mutator=mutate,
    )

    for label, cfg_path in (("control", ctrl_cfg), ("test", test_cfg)):
        result = subprocess.run(
            [str(engine_bin), "run", str(cfg_path)],
            capture_output=True,
            text=True,
            timeout=60,
            check=False,
        )
        assert result.returncode == 0, (
            f"{label} emissions={emissions} run failed ({result.returncode}): {result.stderr}"
        )

    pd.testing.assert_frame_equal(
        _read_arrow_df(test_dir / "suews_output.arrow"),
        _read_arrow_df(ctrl_dir / "suews_output.arrow"),
        check_exact=True,
    )


def test_engine_rejects_null_hour_slot_in_active_anthro_profile(tmp_path: Path):
    """Standalone ``suews-engine`` rejects a null hour slot in an active 24-hour profile."""
    engine_bin = _locate_suews_engine()

    def mutate(doc: dict):
        co2 = doc["sites"][0]["properties"]["anthropogenic_emissions"]["co2"]
        co2["profile_human_activity_24hr"]["holiday"]["12"] = None

    config_path = _prepare_resolved_run(
        tmp_path,
        emissions=2,
        snow_use=0,
        mutator=mutate,
    )
    result = subprocess.run(
        [str(engine_bin), "run", str(config_path)],
        capture_output=True,
        text=True,
        timeout=60,
        check=False,
    )
    assert result.returncode != 0, (
        f"null active hourly profile slot must not exit 0 (stdout={result.stdout})"
    )
    assert (
        "properties.anthropogenic_emissions.co2.profile_human_activity_24hr.holiday.12"
        in result.stderr
    ), result.stderr
    assert not (tmp_path / "suews_output.arrow").exists()


def test_engine_vegetated_snow_without_paved_or_bldgs_still_requires_snow_profile(
    tmp_path: Path,
):
    """With ``snow_use=1`` and zero paved/building fractions, omitting ``snow_profile_24hr`` fails."""
    engine_bin = _locate_suews_engine()

    def mutate(doc: dict):
        site = doc["sites"][0]
        lc = site["properties"]["land_cover"]
        paved_sfr = _unwrap(lc["paved"]["sfr"])
        bldgs_sfr = _unwrap(lc["bldgs"]["sfr"])
        grass_sfr = _unwrap(lc["grass"]["sfr"])
        lc["paved"]["sfr"] = {"value": 0.0}
        lc["bldgs"]["sfr"] = {"value": 0.0}
        lc["grass"]["sfr"] = {"value": grass_sfr + paved_sfr + bldgs_sfr}
        site["initial_states"]["grass"]["snowpack"] = {"value": 5.0}
        site["properties"]["snow"].pop("snow_depth_limit_paved", None)
        site["properties"]["snow"].pop("snow_depth_limit_building", None)
        lc["paved"].pop("snowpack_limit", None)
        lc["bldgs"].pop("snowpack_limit", None)
        site["properties"]["snow"].pop("snow_profile_24hr", None)

    config_path = _prepare_resolved_run(
        tmp_path,
        emissions=2,
        snow_use=1,
        mutator=mutate,
    )
    result = subprocess.run(
        [str(engine_bin), "run", str(config_path)],
        capture_output=True,
        text=True,
        timeout=60,
        check=False,
    )
    assert result.returncode != 0, (
        f"omitted snow_profile_24hr under snow_use=1 must not exit 0 (stdout={result.stdout})"
    )
    assert "properties.snow.snow_profile_24hr" in result.stderr, result.stderr
    assert not (tmp_path / "suews_output.arrow").exists()


def test_engine_vegetated_snow_without_paved_or_bldgs_runs_without_inactive_limits(
    tmp_path: Path,
):
    """With ``snow_use=1`` and zero paved/building fractions, omitting their limits preserves output."""
    engine_bin = _locate_suews_engine()

    def set_veg_snow(doc: dict):
        site = doc["sites"][0]
        lc = site["properties"]["land_cover"]
        paved_sfr = _unwrap(lc["paved"]["sfr"])
        bldgs_sfr = _unwrap(lc["bldgs"]["sfr"])
        grass_sfr = _unwrap(lc["grass"]["sfr"])
        lc["paved"]["sfr"] = {"value": 0.0}
        lc["bldgs"]["sfr"] = {"value": 0.0}
        lc["grass"]["sfr"] = {"value": grass_sfr + paved_sfr + bldgs_sfr}
        site["initial_states"]["grass"]["snowpack"] = {"value": 5.0}

    def set_veg_snow_and_omit_inactive_limits(doc: dict):
        set_veg_snow(doc)
        site = doc["sites"][0]
        lc = site["properties"]["land_cover"]
        site["properties"]["snow"].pop("snow_depth_limit_paved", None)
        site["properties"]["snow"].pop("snow_depth_limit_building", None)
        lc["paved"].pop("snowpack_limit", None)
        lc["bldgs"].pop("snowpack_limit", None)
        lc["water"].pop("snowpack_limit", None)

    ctrl_dir = tmp_path / "ctrl_veg_snow"
    ctrl_dir.mkdir()
    ctrl_cfg = _prepare_resolved_run(
        ctrl_dir,
        emissions=2,
        snow_use=1,
        n_forcing_rows=12,
        mutator=set_veg_snow,
    )

    test_dir = tmp_path / "omitted_veg_snow"
    test_dir.mkdir()
    test_cfg = _prepare_resolved_run(
        test_dir,
        emissions=2,
        snow_use=1,
        n_forcing_rows=12,
        mutator=set_veg_snow_and_omit_inactive_limits,
    )

    for label, cfg_path in (("control", ctrl_cfg), ("test", test_cfg)):
        result = subprocess.run(
            [str(engine_bin), "run", str(cfg_path)],
            capture_output=True,
            text=True,
            timeout=60,
            check=False,
        )
        assert result.returncode == 0, (
            f"{label} vegetated snow run failed ({result.returncode}): {result.stderr}"
        )

    pd.testing.assert_frame_equal(
        _read_arrow_df(test_dir / "suews_output.arrow"),
        _read_arrow_df(ctrl_dir / "suews_output.arrow"),
        check_exact=True,
    )


def test_engine_water_only_snow_omits_snow_profile_and_preserves_output(
    tmp_path: Path,
):
    """Water-only ``snow_use=1`` site skips non-water ``SnowCalc`` and permits omitting ``snow_profile_24hr``."""
    engine_bin = _locate_suews_engine()

    def set_water_only(doc: dict):
        site = doc["sites"][0]
        lc = site["properties"]["land_cover"]
        for surface in ("paved", "bldgs", "evetr", "dectr", "grass", "bsoil"):
            lc[surface]["sfr"] = {"value": 0.0}
        lc["water"]["sfr"] = {"value": 1.0}
        site["initial_states"]["water"]["snowpack"] = {"value": 5.0}

    def set_water_only_and_omit_inactive(doc: dict):
        set_water_only(doc)
        site = doc["sites"][0]
        lc = site["properties"]["land_cover"]
        for surface in ("paved", "bldgs", "evetr", "dectr", "grass", "bsoil", "water"):
            lc[surface].pop("snowpack_limit", None)
        site["properties"]["snow"].pop("snow_depth_limit_paved", None)
        site["properties"]["snow"].pop("snow_depth_limit_building", None)
        site["properties"]["snow"].pop("snow_profile_24hr", None)

    ctrl_dir = tmp_path / "ctrl_water_snow"
    ctrl_dir.mkdir()
    ctrl_cfg = _prepare_resolved_run(
        ctrl_dir,
        emissions=2,
        snow_use=1,
        n_forcing_rows=12,
        mutator=set_water_only,
    )

    test_dir = tmp_path / "omitted_water_snow"
    test_dir.mkdir()
    test_cfg = _prepare_resolved_run(
        test_dir,
        emissions=2,
        snow_use=1,
        n_forcing_rows=12,
        mutator=set_water_only_and_omit_inactive,
    )

    for label, cfg_path in (("control", ctrl_cfg), ("test", test_cfg)):
        result = subprocess.run(
            [str(engine_bin), "run", str(cfg_path)],
            capture_output=True,
            text=True,
            timeout=60,
            check=False,
        )
        assert result.returncode == 0, (
            f"{label} water-only snow run failed ({result.returncode}): {result.stderr}"
        )

    pd.testing.assert_frame_equal(
        _read_arrow_df(test_dir / "suews_output.arrow"),
        _read_arrow_df(ctrl_dir / "suews_output.arrow"),
        check_exact=True,
    )
