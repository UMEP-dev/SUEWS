"""End-to-end engine and data-model guards for snow and anthropogenic emissions (#2002, #2349)."""

from copy import deepcopy
from importlib import import_module
from importlib.resources import files
from pathlib import Path
import subprocess

import pandas as pd
from pyarrow import ipc
import pytest
import yaml

from supy import SUEWSSimulation
from supy.data_model import SUEWSConfig
from supy.data_model.core.human_activity import AnthropogenicHeat
from supy.data_model.core.site import SnowParams

_run_rust = import_module("supy._run_rust")
_YAML_LOADER = getattr(yaml, "CSafeLoader", yaml.SafeLoader)
_YAML_DUMPER = getattr(yaml, "CSafeDumper", yaml.SafeDumper)

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


def _unwrap(value):
    return value["value"] if isinstance(value, dict) else value


def _dump_yaml(doc: dict) -> str:
    return yaml.dump(doc, Dumper=_YAML_DUMPER, sort_keys=False)


def _load_yaml(text: str) -> dict:
    return yaml.load(text, Loader=_YAML_LOADER)


@pytest.fixture(scope="module")
def _anthro_snow_base(sample_config_loaded, sample_forcing_parsed):
    """Cache base resolved YAML, sample raw dict, forcing lines, and PyO3 payload."""
    sample_dir = files("supy") / "sample_data"
    raw_cfg = _load_yaml((sample_dir / "sample_config.yml").read_text(encoding="utf-8"))
    forcing_name = _unwrap(raw_cfg["model"]["control"]["forcing"]["file"])
    assert isinstance(forcing_name, str) and forcing_name
    forcing_raw = (sample_dir / forcing_name).read_text(encoding="utf-8")
    forcing_lines = [line for line in forcing_raw.splitlines() if line.strip()]

    base_cfg = sample_config_loaded.model_copy(deep=True)
    base_cfg.model.control.start_time = None
    base_cfg.model.control.end_time = None
    resolved_base = _load_yaml(_run_rust.config_to_engine_yaml(base_cfg))
    # Establish positive baseline that the unmutated resolved dict passes SUEWSConfig.
    SUEWSConfig.from_dict(deepcopy(resolved_base))

    _, df_forcing = sample_forcing_parsed(12)
    forcing_flat = (
        _run_rust._prepare_forcing_block(df_forcing).ravel(order="C").tolist()
    )
    return {
        "raw_cfg": raw_cfg,
        "resolved_base": resolved_base,
        "forcing_name": forcing_name,
        "forcing_lines": forcing_lines,
        "df_forcing": df_forcing,
        "forcing_flat": forcing_flat,
        "forcing_count": len(df_forcing),
    }


def _prepare_resolved_run(
    base_cache: dict,
    run_dir: Path,
    *,
    emissions: int = 2,
    snow_use: int = 0,
    n_forcing_rows: int = 2,
    forcing_qf: float | None = None,
    mutator=None,
) -> tuple[Path, dict]:
    """Write a resolved YAML config and forcing slice into ``run_dir``."""
    resolved_doc = deepcopy(base_cache["resolved_base"])
    control = resolved_doc["model"]["control"]
    control["output"]["dir"] = str(run_dir)
    control["start_time"] = None
    control["end_time"] = None
    physics = resolved_doc["model"]["physics"]
    physics.pop("snow", None)
    physics["emissions"] = {"value": emissions}
    physics["snow_use"] = {"value": snow_use}
    if mutator is not None:
        mutator(resolved_doc)

    forcing_name = base_cache["forcing_name"]
    forcing_lines = base_cache["forcing_lines"]
    qf_idx = forcing_lines[0].split().index("qf")
    out_lines = [forcing_lines[0]]
    for line in forcing_lines[1 : 1 + n_forcing_rows]:
        parts = line.split()
        if forcing_qf is not None:
            parts[qf_idx] = f"{forcing_qf:.3f}"
        out_lines.append(" ".join(parts))
    (run_dir / forcing_name).write_text("\n".join(out_lines) + "\n", encoding="utf-8")

    config_path = run_dir / "resolved_config.yml"
    config_path.write_text(_dump_yaml(resolved_doc), encoding="utf-8")
    return config_path, resolved_doc


def _read_arrow_df(arrow_path: Path) -> pd.DataFrame:
    """Read an Arrow IPC output file into a DataFrame and release the mmap."""
    with ipc.open_file(str(arrow_path)) as reader:
        return reader.read_all().to_pandas()


def test_data_model_emits_required_snow_and_anthro_heat_defaults(
    tmp_path: Path, suews_engine_bin: Path, _anthro_snow_base: dict
):
    """Data model emits authoritative Snow and AnthropogenicHeat defaults when omitted (#2001)."""
    sample_raw = deepcopy(_anthro_snow_base["raw_cfg"])
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
    resolved_props = _load_yaml(resolved_text)["sites"][0]["properties"]

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

    forcing_name = _anthro_snow_base["forcing_name"]
    (tmp_path / forcing_name).write_text(
        "\n".join(_anthro_snow_base["forcing_lines"][:3]) + "\n", encoding="utf-8"
    )
    cfg_path = tmp_path / "default_emitted.yml"
    cfg_path.write_text(resolved_text, encoding="utf-8")
    result = subprocess.run(
        [str(suews_engine_bin), "run", str(cfg_path)],
        capture_output=True,
        text=True,
        timeout=60,
        check=False,
    )
    assert result.returncode == 0, (
        f"default-emitted snow/anthro config failed ({result.returncode}): {result.stderr}"
    )
    assert (tmp_path / "suews_output.arrow").is_file()


def _drop_hour_24(profile_day: dict) -> None:
    profile_day.pop("24", None)
    profile_day.pop(24, None)


@pytest.mark.parametrize(
    ("emissions", "snow_use", "mutator", "expected_path", "public_needles"),
    [
        # Snow subsystem: missing, null, malformed, incorrectly sized (24-hour profile)
        pytest.param(
            2,
            1,
            lambda p: p["snow"].pop("water_holding_capacity_max", None),
            "properties.snow.water_holding_capacity_max",
            None,
            id="snow-missing-scalar",
        ),
        pytest.param(
            2,
            1,
            lambda p: p["snow"].__setitem__(
                "water_holding_capacity_max", {"value": None}
            ),
            "properties.snow.water_holding_capacity_max",
            ("water_holding_capacity_max", "null"),
            id="snow-null-scalar",
        ),
        pytest.param(
            2,
            1,
            lambda p: p["snow"].__setitem__("tau_cold_snow", "bad"),
            "properties.snow.tau_cold_snow",
            ("tau_cold_snow",),
            id="snow-malformed-scalar",
        ),
        pytest.param(
            2,
            1,
            lambda p: _drop_hour_24(p["snow"]["snow_profile_24hr"]["working_day"]),
            "properties.snow.snow_profile_24hr.working_day",
            ("snow_profile_24hr.working_day", "24"),
            id="snow-wrong-size-profile",
        ),
        # Anthropogenic emissions subsystem: missing, null, malformed, incorrectly sized
        pytest.param(
            2,
            0,
            lambda p: p["anthropogenic_emissions"]["heat"]["qf_a"].pop(
                "working_day", None
            ),
            "properties.anthropogenic_emissions.heat.qf_a.working_day",
            None,
            id="anthro-missing-scalar",
        ),
        pytest.param(
            2,
            0,
            lambda p: p["anthropogenic_emissions"]["co2"][
                "profile_human_activity_24hr"
            ]["holiday"].__setitem__("12", None),
            "properties.anthropogenic_emissions.co2.profile_human_activity_24hr.holiday.12",
            (
                "properties.anthropogenic_emissions.co2.profile_human_activity_24hr.holiday.12",
            ),
            id="anthro-null-profile-slot",
        ),
        pytest.param(
            2,
            0,
            lambda p: p["anthropogenic_emissions"].__setitem__(
                "startdls", {"value": 86.5}
            ),
            "properties.anthropogenic_emissions.startdls",
            ("startdls", "integer"),
            id="anthro-malformed-dls",
        ),
        pytest.param(
            2,
            0,
            lambda p: _drop_hour_24(
                p["anthropogenic_emissions"]["heat"]["ahprof_24hr"]["working_day"]
            ),
            "properties.anthropogenic_emissions.heat.ahprof_24hr.working_day",
            ("ahprof_24hr.working_day", "24"),
            id="anthro-wrong-size-profile",
        ),
    ],
)
def test_snow_and_anthro_br02_failure_witnesses(
    tmp_path: Path,
    suews_engine_bin: Path,
    _anthro_snow_base: dict,
    emissions: int,
    snow_use: int,
    mutator,
    expected_path: str,
    public_needles: tuple[str, ...] | None,
):
    """BR-02 witnesses for snow and anthropogenic emissions across Python and suews-engine."""

    def mutate_doc(doc: dict):
        mutator(doc["sites"][0]["properties"])

    config_path, mutated_doc = _prepare_resolved_run(
        _anthro_snow_base,
        tmp_path,
        emissions=emissions,
        snow_use=snow_use,
        mutator=mutate_doc,
    )

    # 1. Standalone suews-engine witness
    result = subprocess.run(
        [str(suews_engine_bin), "run", str(config_path)],
        capture_output=True,
        text=True,
        timeout=60,
        check=False,
    )
    assert result.returncode != 0, (
        f"invalid {expected_path} must not exit 0 (stdout={result.stdout})"
    )
    assert expected_path in result.stderr, result.stderr
    assert not (tmp_path / "suews_output.arrow").exists()

    # 2. PyO3 Python-bridge witness
    rust = _run_rust._check_rust_available()
    with pytest.raises(RuntimeError) as exc_info:
        rust.run_suews(
            _dump_yaml(mutated_doc),
            _anthro_snow_base["forcing_flat"],
            _anthro_snow_base["forcing_count"],
        )
    assert expected_path in str(exc_info.value), str(exc_info.value)

    # 3. Public Python entry point witness (SUEWSConfig parse or SUEWSSimulation.run)
    if public_needles is not None:
        try:
            cfg = SUEWSConfig.from_dict(mutated_doc)
        except (ValueError, TypeError) as exc:
            msg = str(exc)
            for needle in public_needles:
                assert needle in msg, f"expected {needle!r} in {msg!r}"
            return
        df_forcing = _anthro_snow_base["df_forcing"]
        sim = SUEWSSimulation()
        sim.update_config(cfg, auto_load_forcing=False)
        sim.update_forcing(df_forcing)
        with pytest.raises(RuntimeError) as sim_exc:
            sim.run(
                start_date=df_forcing.index[0],
                end_date=df_forcing.index[-1],
                n_jobs=1,
                _validate_forcing=False,
            )
        msg = str(sim_exc.value)
        for needle in public_needles:
            assert needle in msg, f"expected {needle!r} in {msg!r}"


def test_engine_accepts_omitted_inactive_snow_and_observed_anthro(
    tmp_path: Path, suews_engine_bin: Path, _anthro_snow_base: dict
):
    """Standalone ``suews-engine`` runs cleanly with ``snow_use=0`` and ``emissions=0``."""

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

    config_path, _ = _prepare_resolved_run(
        _anthro_snow_base,
        tmp_path,
        emissions=0,
        snow_use=0,
        n_forcing_rows=24,
        forcing_qf=12.0,
        mutator=mutate,
    )
    result = subprocess.run(
        [str(suews_engine_bin), "run", str(config_path)],
        capture_output=True,
        text=True,
        timeout=60,
        check=False,
    )
    assert result.returncode == 0, (
        f"omitted inactive snow/anthro fields failed ({result.returncode}): {result.stderr}"
    )
    assert (tmp_path / "suews_output.arrow").is_file()


def test_engine_snow_disabled_positive_initial_snowpack_matches_zero_control(
    tmp_path: Path, suews_engine_bin: Path, _anthro_snow_base: dict
):
    """Snow-disabled run clears positive initial snowpack and omits snow scalars (#2002)."""
    ctrl_dir = tmp_path / "control"
    ctrl_dir.mkdir()
    ctrl_cfg, _ = _prepare_resolved_run(
        _anthro_snow_base,
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

    test_cfg, _ = _prepare_resolved_run(
        _anthro_snow_base,
        test_dir,
        emissions=2,
        snow_use=0,
        n_forcing_rows=12,
        mutator=mutate,
    )

    for label, cfg_path in (("control", ctrl_cfg), ("test", test_cfg)):
        result = subprocess.run(
            [str(suews_engine_bin), "run", str(cfg_path)],
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
    suews_engine_bin: Path,
    _anthro_snow_base: dict,
    emissions: int,
    heat_drops: tuple[str, ...],
    co2_drops: tuple[str, ...],
):
    """Omitting unused heat/traffic branches under emissions 1/2/5 preserves exact output."""
    ctrl_dir = tmp_path / f"ctrl_em{emissions}"
    ctrl_dir.mkdir()
    ctrl_cfg, _ = _prepare_resolved_run(
        _anthro_snow_base,
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

    test_cfg, _ = _prepare_resolved_run(
        _anthro_snow_base,
        test_dir,
        emissions=emissions,
        snow_use=0,
        n_forcing_rows=12,
        mutator=mutate,
    )

    for label, cfg_path in (("control", ctrl_cfg), ("test", test_cfg)):
        result = subprocess.run(
            [str(suews_engine_bin), "run", str(cfg_path)],
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


def test_engine_vegetated_snow_without_paved_or_bldgs_runs_without_inactive_limits(
    tmp_path: Path, suews_engine_bin: Path, _anthro_snow_base: dict
):
    """With ``snow_use=1`` and zero paved/building fractions, omitting their limits preserves output."""

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
    ctrl_cfg, _ = _prepare_resolved_run(
        _anthro_snow_base,
        ctrl_dir,
        emissions=2,
        snow_use=1,
        n_forcing_rows=12,
        mutator=set_veg_snow,
    )

    test_dir = tmp_path / "omitted_veg_snow"
    test_dir.mkdir()
    test_cfg, _ = _prepare_resolved_run(
        _anthro_snow_base,
        test_dir,
        emissions=2,
        snow_use=1,
        n_forcing_rows=12,
        mutator=set_veg_snow_and_omit_inactive_limits,
    )

    for label, cfg_path in (("control", ctrl_cfg), ("test", test_cfg)):
        result = subprocess.run(
            [str(suews_engine_bin), "run", str(cfg_path)],
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
    tmp_path: Path, suews_engine_bin: Path, _anthro_snow_base: dict
):
    """Water-only ``snow_use=1`` site skips non-water ``SnowCalc`` and permits omitting ``snow_profile_24hr``."""

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
    ctrl_cfg, _ = _prepare_resolved_run(
        _anthro_snow_base,
        ctrl_dir,
        emissions=2,
        snow_use=1,
        n_forcing_rows=12,
        mutator=set_water_only,
    )

    test_dir = tmp_path / "omitted_water_snow"
    test_dir.mkdir()
    test_cfg, _ = _prepare_resolved_run(
        _anthro_snow_base,
        test_dir,
        emissions=2,
        snow_use=1,
        n_forcing_rows=12,
        mutator=set_water_only_and_omit_inactive,
    )

    for label, cfg_path in (("control", ctrl_cfg), ("test", test_cfg)):
        result = subprocess.run(
            [str(suews_engine_bin), "run", str(cfg_path)],
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
