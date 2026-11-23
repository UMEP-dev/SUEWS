"""Behavioural and parity coverage for the four native aerodynamic selectors."""

from __future__ import annotations

from copy import deepcopy
from importlib import import_module
from importlib.resources import as_file, files
import json
import os
from pathlib import Path
import subprocess

from click.testing import CliRunner
import numpy as np
from pyarrow import ipc
import pytest
import yaml

from supy import SUEWSSimulation
from supy.cmd.suews_cli import cli
from supy.data_model import ModelPhysics, SUEWSConfig

_run_rust = import_module("supy._run_rust")

pytestmark = [
    pytest.mark.physics,
    pytest.mark.api,
    pytest.mark.contract("C-STRICT", "C-BIND", "C-EMIT", "C-PARITY", "C-RESPONSE"),
]

SELECTORS = (
    "roughness_length_momentum",
    "roughness_length_heat",
    "roughness_sublayer",
    "roughness_sublayer_level",
)

DOCUMENTED_DEFAULTS = {
    "roughness_length_momentum": 2,
    "roughness_length_heat": 2,
    "roughness_sublayer": 2,
    "roughness_sublayer_level": 0,
}


def _unwrap(value):
    while isinstance(value, dict) and "value" in value:
        value = value["value"]
    while hasattr(value, "value"):
        value = value.value
    return value


def _extract_four(physics) -> dict[str, int]:
    if isinstance(physics, dict):
        return {k: int(_unwrap(physics[k])) for k in SELECTORS}
    return {k: int(_unwrap(getattr(physics, k))) for k in SELECTORS}


def _locate_suews_engine() -> Path:
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


def _sample_raw() -> dict:
    sample_dir = files("supy").joinpath("sample_data")
    return yaml.safe_load(
        sample_dir.joinpath("sample_config.yml").read_text(encoding="utf-8")
    )


def _sample_raw_without_selectors() -> dict:
    raw = _sample_raw()
    for key in SELECTORS:
        raw["model"]["physics"].pop(key, None)
    return raw


def _make_zero_canopy(props: dict) -> None:
    lc = props["land_cover"]
    moved = (
        float(lc["bldgs"]["sfr"]["value"])
        + float(lc["evetr"]["sfr"]["value"])
        + float(lc["dectr"]["sfr"]["value"])
    )
    lc["bldgs"]["sfr"]["value"] = 0.0
    lc["evetr"]["sfr"]["value"] = 0.0
    lc["dectr"]["sfr"]["value"] = 0.0
    lc["paved"]["sfr"]["value"] = round(float(lc["paved"]["sfr"]["value"]) + moved, 6)


def _make_fully_pervious(props: dict) -> None:
    lc = props["land_cover"]
    moved = (
        float(lc["paved"]["sfr"]["value"])
        + float(lc["bldgs"]["sfr"]["value"])
        + float(lc["water"]["sfr"]["value"])
    )
    lc["paved"]["sfr"]["value"] = 0.0
    lc["bldgs"]["sfr"]["value"] = 0.0
    lc["water"]["sfr"]["value"] = 0.0
    lc["grass"]["sfr"]["value"] = round(float(lc["grass"]["sfr"]["value"]) + moved, 6)


def _make_low_pai(props: dict) -> None:
    lc = props["land_cover"]
    moved = (
        float(lc["bldgs"]["sfr"]["value"])
        - 0.05
        + float(lc["evetr"]["sfr"]["value"])
        + float(lc["dectr"]["sfr"]["value"])
    )
    lc["bldgs"]["sfr"]["value"] = 0.05
    lc["evetr"]["sfr"]["value"] = 0.0
    lc["dectr"]["sfr"]["value"] = 0.0
    lc["paved"]["sfr"]["value"] = round(float(lc["paved"]["sfr"]["value"]) + moved, 6)


def _sample_forcing_slice(doy: int, *, hours: tuple[int, int] = (0, 23)) -> str:
    """Return hourly forcing lines for one day (e.g. wet/windy DOY 176 or DOY 5)."""
    sample_dir = files("supy").joinpath("sample_data")
    lines = [
        ln
        for ln in sample_dir.joinpath("Kc_2012_data_60.txt")
        .read_text(encoding="utf-8")
        .splitlines()
        if ln.strip()
    ]
    header = lines[0].split()
    doy_idx = header.index("id")
    hour_idx = header.index("it")
    kept = [lines[0]]
    for ln in lines[1:]:
        cols = ln.split()
        if int(cols[doy_idx]) == doy and hours[0] <= int(cols[hour_idx]) <= hours[1]:
            kept.append(ln)
    assert len(kept) > 1, f"no forcing rows found for DOY {doy}"
    return "\n".join(kept) + "\n"


def _make_forcing_df(tmp_path: Path, doy: int, *, hours: tuple[int, int] = (9, 18)):
    forcing_file = tmp_path / f"forcing_doy{doy}.txt"
    forcing_file.write_text(_sample_forcing_slice(doy, hours=hours), encoding="utf-8")
    raw = _sample_raw()
    raw["model"]["control"]["forcing"]["file"] = str(forcing_file)
    sim = SUEWSSimulation()
    sim.update_config(raw)
    return sim.forcing.df.copy()


def _run_config_frame(config_dict: dict, forcing_df):
    rust_module = _run_rust._load_rust_module()
    forcing_block = _run_rust._prepare_forcing_block(forcing_df)
    output_flat, state_json, len_sim = rust_module.run_suews(
        json.dumps(config_dict),
        forcing_block.ravel(order="C").tolist(),
        len(forcing_df),
    )[:3]
    assert len_sim == len(forcing_df)
    return _run_rust._parse_output_block(output_flat, len_sim, grid_id=1), state_json


def _run_variant(physics_overrides: dict, forcing_df, *, site_mutator=None):
    raw = _sample_raw()
    for key, val in physics_overrides.items():
        raw["model"]["physics"][key] = {"value": val}
    if site_mutator is not None:
        site_mutator(raw["sites"][0]["properties"])
    cfg = SUEWSConfig.from_dict(raw)
    frame, _ = _run_config_frame(
        cfg.model_dump(exclude_none=True, mode="json"), forcing_df
    )
    return frame


def _read_arrow_columns(arrow_path: Path, columns: list[str]) -> dict[str, np.ndarray]:
    with ipc.open_file(str(arrow_path)) as reader:
        table = reader.read_all().select(columns)
        return {
            name: table.column(name).to_numpy(zero_copy_only=False).copy()
            for name in columns
        }


@pytest.fixture(scope="module")
def resolved_sample_dict() -> dict:
    """Resolved sample configuration dict, validated once per module."""
    cfg = SUEWSConfig.from_dict(_sample_raw())
    return yaml.safe_load(_run_rust.config_to_engine_yaml(cfg))


@pytest.fixture(scope="module")
def short_forcing_df(tmp_path_factory):
    """Two 5-minute forcing steps from wet/windy DOY 176 for native guard checks."""
    work = tmp_path_factory.mktemp("aero_short")
    return _make_forcing_df(work, doy=176, hours=(11, 12)).iloc[:2]


@pytest.fixture(scope="module")
def wet_windy_forcing_df(tmp_path_factory):
    """Eight-hour summer wet+windy slice (DOY 176, U up to 6.73 m/s, rain 1.4 mm)."""
    work = tmp_path_factory.mktemp("aero_doy176")
    return _make_forcing_df(work, doy=176, hours=(10, 17))


def test_python_declarations_and_exports_resolve_documented_defaults(tmp_path: Path):
    """All supported Python declarations and exports resolve omitted selectors."""
    assert _extract_four(ModelPhysics()) == DOCUMENTED_DEFAULTS
    assert _extract_four(SUEWSConfig().model.physics) == DOCUMENTED_DEFAULTS

    raw = _sample_raw_without_selectors()
    cfg = SUEWSConfig.from_dict(raw)
    assert _extract_four(cfg.model.physics) == DOCUMENTED_DEFAULTS

    src_yml = tmp_path / "source_omitted.yml"
    src_yml.write_text(yaml.safe_dump(raw, sort_keys=False), encoding="utf-8")
    assert _extract_four(SUEWSConfig.from_yaml(str(src_yml)).model.physics) == (
        DOCUMENTED_DEFAULTS
    )

    for include_internal in (True, False):
        exported_path = tmp_path / f"exported_{include_internal}.yml"
        cfg.to_yaml(str(exported_path), include_internal=include_internal)
        exported_doc = yaml.safe_load(exported_path.read_text(encoding="utf-8"))
        assert _extract_four(exported_doc["model"]["physics"]) == DOCUMENTED_DEFAULTS

    for for_file_output in (True, False):
        eng_doc = yaml.safe_load(
            _run_rust.config_to_engine_yaml(cfg, for_file_output=for_file_output)
        )
        assert _extract_four(eng_doc["model"]["physics"]) == DOCUMENTED_DEFAULTS

    _, payloads = _run_rust._grid_config_jsons(cfg)
    assert _extract_four(json.loads(payloads[0])["model"]["physics"]) == (
        DOCUMENTED_DEFAULTS
    )

    df_state = cfg.to_df_state()
    assert {
        "roughness_length_momentum": int(df_state.loc[1, ("roughlenmommethod", "0")]),
        "roughness_length_heat": int(df_state.loc[1, ("roughlenheatmethod", "0")]),
        "roughness_sublayer": int(df_state.loc[1, ("rslmethod", "0")]),
        "roughness_sublayer_level": int(df_state.loc[1, ("rsllevel", "0")]),
    } == DOCUMENTED_DEFAULTS
    assert _extract_four(SUEWSConfig.from_df_state(df_state).model.physics) == (
        DOCUMENTED_DEFAULTS
    )


def test_python_exports_preserve_defaults_on_inactive_combinations():
    """Omitted selectors remain explicitly resolved on MOST, flat and minimal configs."""
    raw = _sample_raw_without_selectors()

    d_most = deepcopy(raw)
    d_most["model"]["physics"]["roughness_sublayer"] = "most"
    c_most = SUEWSConfig.from_dict(d_most)
    assert _extract_four(
        yaml.safe_load(_run_rust.config_to_engine_yaml(c_most))["model"]["physics"]
    ) == {**DOCUMENTED_DEFAULTS, "roughness_sublayer": 0}

    d_flat = deepcopy(raw)
    _make_zero_canopy(d_flat["sites"][0]["properties"])
    c_flat = SUEWSConfig.from_dict(d_flat)
    assert _extract_four(
        yaml.safe_load(_run_rust.config_to_engine_yaml(c_flat))["model"]["physics"]
    ) == DOCUMENTED_DEFAULTS

    d_min = deepcopy(raw)
    d_min["model"]["physics"]["emissions"] = 0
    d_min["model"]["physics"]["storage_heat"] = 0
    d_min["model"]["physics"].pop("snow_use", None)
    d_min["model"]["physics"]["snow"] = 0
    c_min = SUEWSConfig.from_dict(d_min)
    assert _extract_four(
        yaml.safe_load(_run_rust.config_to_engine_yaml(c_min))["model"]["physics"]
    ) == DOCUMENTED_DEFAULTS


def test_omitted_source_selectors_match_explicit_defaults_in_oop_and_cli(
    tmp_path: Path, monkeypatch, wet_windy_forcing_df
):
    """Omitted source selectors match explicit defaults across OOP and CLI routes."""
    raw = _sample_raw_without_selectors()
    forcing_df = wet_windy_forcing_df.iloc[:12]
    explicit_raw = deepcopy(raw)
    for key, val in DOCUMENTED_DEFAULTS.items():
        explicit_raw["model"]["physics"][key] = {"value": val}

    sim_omitted = SUEWSSimulation()
    sim_omitted.update_config(raw, auto_load_forcing=False)
    sim_explicit = SUEWSSimulation()
    sim_explicit.update_config(explicit_raw, auto_load_forcing=False)

    df_omitted, state_omitted = _run_config_frame(
        sim_omitted.config.model_dump(exclude_none=True, mode="json"), forcing_df
    )
    df_explicit, state_explicit = _run_config_frame(
        sim_explicit.config.model_dump(exclude_none=True, mode="json"), forcing_df
    )
    np.testing.assert_array_equal(df_omitted.to_numpy(), df_explicit.to_numpy())
    assert json.loads(state_omitted) == json.loads(state_explicit)

    forcing_file = tmp_path / "cli_forcing.txt"
    forcing_file.write_text(
        _sample_forcing_slice(176, hours=(11, 13)), encoding="utf-8"
    )
    cli_raw = deepcopy(raw)
    cli_control = cli_raw["model"]["control"]
    cli_control["forcing"]["file"] = str(forcing_file)
    cli_control["start_time"] = "2012-06-24 10:05:00"
    cli_control["end_time"] = "2012-06-24 13:00:00"
    cli_source = tmp_path / "cli_omitted.yml"
    cli_source.write_text(yaml.safe_dump(cli_raw, sort_keys=False), encoding="utf-8")
    completed = []

    def capture_save(simulation, **kwargs):
        completed.append(simulation)
        return []

    monkeypatch.setattr(SUEWSSimulation, "save", capture_save)
    res = CliRunner().invoke(cli, ["run", str(cli_source)])
    assert res.exit_code == 0, res.output
    assert len(completed) == 1
    assert _extract_four(completed[0].config.model.physics) == DOCUMENTED_DEFAULTS


@pytest.mark.parametrize("selector", SELECTORS)
@pytest.mark.parametrize(
    "bad_value",
    [
        "__OMIT__",
        None,
        {"value": None},
        {},
        {"ref": {"desc": "citation"}},
        {"other": 1},
        True,
        1.5,
        float("nan"),
        {"value": float("nan")},
        "",
        [],
        {"value": {"value": 1}},
    ],
)
def test_native_python_callers_reject_invalid_selectors_before_output(
    resolved_sample_dict: dict, short_forcing_df, selector: str, bad_value
):
    """Direct PyO3 callers fail with canonical paths and do not touch output buffers."""
    invalid = deepcopy(resolved_sample_dict)
    if bad_value == "__OMIT__":
        invalid["model"]["physics"].pop(selector)
    else:
        invalid["model"]["physics"][selector] = bad_value

    # Use YAML dump so float("nan") serialises as `.nan` rather than JSON NaN.
    valid_payload = yaml.safe_dump(resolved_sample_dict, sort_keys=False)
    invalid_payload = yaml.safe_dump(invalid, sort_keys=False)

    forcing_block = _run_rust._prepare_forcing_block(short_forcing_df)
    forcing_flat = forcing_block.ravel(order="C").tolist()
    n_steps = len(short_forcing_df)
    rust_module = _run_rust._load_rust_module()
    pattern = rf"`model\.physics\.{selector}`"

    with pytest.raises(RuntimeError, match=pattern):
        rust_module.run_suews(invalid_payload, forcing_flat, n_steps)

    with pytest.raises(RuntimeError, match=pattern):
        rust_module.run_suews_with_state(invalid_payload, forcing_flat, n_steps, "{}")

    ar_data, ar_datetime = _run_rust._alloc_output_buffers(n_steps)
    with pytest.raises(RuntimeError, match=pattern):
        rust_module.run_suews_into(
            invalid_payload, forcing_block, n_steps, ar_data, ar_datetime, None
        )
    assert np.all(np.equal(ar_data, 0.0))
    assert np.all(np.equal(ar_datetime, 0.0))

    with pytest.raises(RuntimeError, match="grid 1:.*" + pattern):
        rust_module.run_suews_multi(
            [valid_payload, invalid_payload], forcing_flat, n_steps, 2
        )

    ar_data_m, ar_dt_m = _run_rust._alloc_output_store(2, n_steps)
    with pytest.raises(RuntimeError, match="grid 1:.*" + pattern):
        rust_module.run_suews_multi_into(
            [valid_payload, invalid_payload],
            forcing_block,
            n_steps,
            [ar_data_m[:n_steps], ar_data_m[n_steps:]],
            [ar_dt_m[:n_steps], ar_dt_m[n_steps:]],
            None,
            2,
        )


@pytest.mark.parametrize("selector", SELECTORS)
@pytest.mark.parametrize(
    "raw_snippet",
    [
        None,
        "null",
        "{value: null}",
        "{}",
        "{ref: {desc: citation}}",
        ".nan",
        "{value: .nan}",
        ".inf",
        "1.5",
        "true",
        "''",
        "[]",
        "{value: {value: 1}}",
    ],
)
def test_standalone_engine_rejects_invalid_selectors_without_writing_output(
    tmp_path: Path, resolved_sample_dict: dict, selector: str, raw_snippet: str | None
):
    """Standalone `suews-engine run` stops with canonical path and writes no files."""
    engine_bin = _locate_suews_engine()
    doc = deepcopy(resolved_sample_dict)
    doc["model"]["control"]["output"]["dir"] = str(tmp_path)
    doc["model"]["control"]["forcing"]["file"] = "forcing.txt"

    if raw_snippet is None:
        doc["model"]["physics"].pop(selector, None)
        cfg_text = yaml.safe_dump(doc, sort_keys=False)
    else:
        token = f"__RAW_{selector}__"
        doc["model"]["physics"][selector] = token
        dumped = yaml.safe_dump(doc, sort_keys=False)
        for target in (f"'{token}'", f'"{token}"', token):
            if target in dumped:
                cfg_text = dumped.replace(target, raw_snippet, 1)
                break
        else:
            raise AssertionError("failed to inject raw YAML token")

    (tmp_path / "forcing.txt").write_text(
        _sample_forcing_slice(176, hours=(11, 13)), encoding="utf-8"
    )
    cfg_path = tmp_path / "resolved.yml"
    cfg_path.write_text(cfg_text, encoding="utf-8")
    before = {p: p.read_bytes() for p in tmp_path.rglob("*") if p.is_file()}

    result = subprocess.run(
        [str(engine_bin), "run", str(cfg_path)],
        capture_output=True,
        text=True,
        timeout=60,
        check=False,
    )

    assert result.returncode != 0
    assert f"`model.physics.{selector}`" in result.stderr
    assert "suews run" in result.stderr
    assert not (tmp_path / "suews_output.arrow").exists()
    assert {p: p.read_bytes() for p in tmp_path.rglob("*") if p.is_file()} == before


def test_asymmetric_valid_roughness_alternatives_reach_proper_receivers(
    wet_windy_forcing_df,
):
    """Momentum and heat roughness alternatives reach distinct Fortran receivers."""
    forcing_df = wet_windy_forcing_df

    mom_z0m = {
        code: float(
            _run_variant({"roughness_length_momentum": code}, forcing_df)
            .loc[:, ("SUEWS", "z0m")]
            .mean()
        )
        for code in (1, 2, 3, 4)
    }
    assert len({round(v, 5) for v in mom_z0m.values()}) == 4, mom_z0m

    heat_ra = {}
    heat_qh = {}
    for code in (1, 2, 3, 4):
        df = _run_variant({"roughness_length_heat": code}, forcing_df)
        assert np.isclose(df.loc[:, ("SUEWS", "z0m")].mean(), mom_z0m[1], atol=1e-12)
        heat_ra[code] = float(df.loc[:, ("SUEWS", "RA")].mean())
        heat_qh[code] = float(df.loc[:, ("SUEWS", "QH")].mean())
    assert len({round(v, 3) for v in heat_ra.values()}) == 4, heat_ra
    assert len({round(v, 3) for v in heat_qh.values()}) == 4, heat_qh

    # Adaptive heat (5) matches Brutsaert (1) on a fully pervious surface,
    # differing from Kawai (2).
    df_perv_1 = _run_variant(
        {"roughness_length_heat": 1}, forcing_df, site_mutator=_make_fully_pervious
    )
    df_perv_2 = _run_variant(
        {"roughness_length_heat": 2}, forcing_df, site_mutator=_make_fully_pervious
    )
    df_perv_5 = _run_variant(
        {"roughness_length_heat": 5}, forcing_df, site_mutator=_make_fully_pervious
    )
    np.testing.assert_allclose(
        df_perv_5.loc[:, ("SUEWS", "QH")].to_numpy(),
        df_perv_1.loc[:, ("SUEWS", "QH")].to_numpy(),
        rtol=0.0,
        atol=1e-12,
    )
    assert not np.allclose(
        df_perv_5.loc[:, ("SUEWS", "QH")].to_numpy(),
        df_perv_2.loc[:, ("SUEWS", "QH")].to_numpy(),
    )

    # Zero-canopy site (Zh=0, PAI=0): roughness_length_momentum=1 vs 2 still differs.
    df_zero_1 = _run_variant(
        {"roughness_length_momentum": 1}, forcing_df, site_mutator=_make_zero_canopy
    )
    df_zero_2 = _run_variant(
        {"roughness_length_momentum": 2}, forcing_df, site_mutator=_make_zero_canopy
    )
    assert not np.allclose(
        df_zero_1.loc[:, ("SUEWS", "z0m")].to_numpy(),
        df_zero_2.loc[:, ("SUEWS", "z0m")].to_numpy(),
    )
    assert not np.allclose(
        df_zero_1.loc[:, ("SUEWS", "QH")].to_numpy(),
        df_zero_2.loc[:, ("SUEWS", "QH")].to_numpy(),
    )


def test_roughness_sublayer_and_level_remain_unconditionally_active(
    wet_windy_forcing_df,
):
    """RSL method (0/1/2) and RSL level (0/1/2 under MOST + emissions=0) stay active."""
    forcing_df = wet_windy_forcing_df

    df_most = _run_variant({"roughness_sublayer": 0}, forcing_df)
    df_rst = _run_variant({"roughness_sublayer": 1}, forcing_df)
    assert np.isclose(df_most.loc[:, ("RSL", "flag_RSL")].mean(), 0.0, atol=1e-12)
    assert np.isclose(df_rst.loc[:, ("RSL", "flag_RSL")].mean(), 1.0, atol=1e-12)
    assert not np.allclose(
        df_most.loc[:, ("SUEWS", "T2")].to_numpy(),
        df_rst.loc[:, ("SUEWS", "T2")].to_numpy(),
    )

    df_low_rst = _run_variant(
        {"roughness_sublayer": 1}, forcing_df, site_mutator=_make_low_pai
    )
    df_low_var = _run_variant(
        {"roughness_sublayer": 2}, forcing_df, site_mutator=_make_low_pai
    )
    assert np.isclose(df_low_rst.loc[:, ("RSL", "flag_RSL")].mean(), 1.0, atol=1e-12)
    assert np.isclose(df_low_var.loc[:, ("RSL", "flag_RSL")].mean(), 0.0, atol=1e-12)

    obs_qf_forcing = forcing_df.copy()
    obs_qf_forcing["qf"] = 25.0
    most_levels = {
        lvl: _run_variant(
            {
                "emissions": 0,
                "roughness_sublayer": 0,
                "roughness_sublayer_level": lvl,
            },
            obs_qf_forcing,
        )
        for lvl in (0, 1, 2)
    }
    assert not np.allclose(
        most_levels[1].loc[:, ("SUEWS", "RS")].to_numpy(),
        most_levels[0].loc[:, ("SUEWS", "RS")].to_numpy(),
    )
    assert not np.allclose(
        most_levels[2].loc[:, ("SUEWS", "RS")].to_numpy(),
        most_levels[0].loc[:, ("SUEWS", "RS")].to_numpy(),
    )
    assert not np.allclose(
        most_levels[2].loc[:, ("SUEWS", "RS")].to_numpy(),
        most_levels[1].loc[:, ("SUEWS", "RS")].to_numpy(),
    )


@pytest.mark.parametrize(
    "case",
    [
        # Summer wet + windy day (U up to 6.73 m/s, rain 1.4 mm, sunlit)
        {
            "doy": 176,
            "hours": (10, 17),
            "start_time": "2012-06-24 09:05:00",
            "end_time": "2012-06-24 17:00:00",
            "physics": {
                "roughness_length_momentum": 3,
                "roughness_length_heat": 4,
                "roughness_sublayer": 0,
                "roughness_sublayer_level": 2,
            },
        },
        # Winter windy + wet day (U up to 9.34 m/s, rain 0.8 mm)
        {
            "doy": 5,
            "hours": (10, 17),
            "start_time": "2012-01-05 09:05:00",
            "end_time": "2012-01-05 17:00:00",
            "physics": {
                "roughness_length_momentum": 2,
                "roughness_length_heat": 1,
                "roughness_sublayer": 2,
                "roughness_sublayer_level": 1,
            },
        },
    ],
    ids=["summer_wet_windy", "winter_windy_wet"],
)
def test_numerical_parity_between_python_and_standalone_engine_on_wet_windy_cases(
    tmp_path: Path, case: dict
):
    """Valid wet/windy runs agree across SUEWSSimulation and standalone suews-engine."""
    engine_bin = _locate_suews_engine()
    doy = case["doy"]
    forcing_file = tmp_path / f"forcing_{doy}.txt"
    forcing_file.write_text(
        _sample_forcing_slice(doy, hours=case["hours"]), encoding="utf-8"
    )

    raw = _sample_raw()
    control = raw["model"]["control"]
    control["output"]["dir"] = str(tmp_path)
    control["forcing"]["file"] = str(forcing_file)
    control["start_time"] = case["start_time"]
    control["end_time"] = case["end_time"]
    for key, val in case["physics"].items():
        raw["model"]["physics"][key] = {"value": val}

    sim = SUEWSSimulation()
    sim.update_config(raw)
    py_out = sim.run().df.droplevel("grid")

    resolved_path = tmp_path / "resolved.yml"
    resolved_path.write_text(
        _run_rust.config_to_engine_yaml(sim.config), encoding="utf-8"
    )
    proc = subprocess.run(
        [str(engine_bin), "run", str(resolved_path)],
        capture_output=True,
        text=True,
        timeout=60,
        check=False,
    )
    assert proc.returncode == 0, proc.stderr

    arrow_path = tmp_path / "suews_output.arrow"
    assert arrow_path.is_file()
    check_cols = ["QH", "QE", "QF", "z0m", "zdm", "RA", "RS", "UStar", "T2", "U10"]
    arrow_cols = _read_arrow_columns(arrow_path, check_cols)
    for col in check_cols:
        py_vals = py_out.loc[:, ("SUEWS", col)].to_numpy(dtype=float)
        eng_vals = np.where(
            np.equal(arrow_cols[col], -999.0), np.nan, arrow_cols[col]
        )
        assert np.isfinite(py_vals).all(), f"non-finite Python {col}"
        np.testing.assert_allclose(
            eng_vals, py_vals, rtol=1e-12, atol=1e-12, equal_nan=True
        )
