"""End-to-end engine and data-model guards for site scalars and land cover (#2002)."""

from copy import deepcopy
from importlib import import_module
from importlib.resources import as_file, files
import os
from pathlib import Path
import subprocess

import numpy as np
import pandas as pd
from pyarrow import ipc
import pytest
import yaml

from supy import SUEWSSimulation
from supy.data_model import SUEWSConfig

_run_rust = import_module("supy._run_rust")

_SURFACES = ("paved", "bldgs", "evetr", "dectr", "grass", "bsoil", "water")
_VEG_SURFACES = ("evetr", "dectr", "grass")

_DEFAULTED_SITE_SCALARS = (
    "z0m_in",
    "zdm_in",
    "pipecapacity",
    "runofftowater",
    "narp_trans_site",
    "n_buildings",
    "h_std",
    "lambda_c",
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
    pytest.skip("suews-engine binary not found")


def _stage_engine_case(tmp_path: Path, config_dict: dict) -> tuple[Path, Path]:
    """Write a short forcing slice and YAML config for ``suews-engine``."""
    repo_root = Path(__file__).resolve().parents[2]
    forcing_src = repo_root / "src" / "supy" / "sample_data" / "Kc_2012_data_60.txt"
    lines = forcing_src.read_text(encoding="utf-8").splitlines()
    short_forcing = tmp_path / "forcing.txt"
    short_forcing.write_text("\n".join(lines[:14]) + "\n", encoding="utf-8")

    cfg = deepcopy(config_dict)
    out_dir = tmp_path / "out"
    out_dir.mkdir(exist_ok=True)
    cfg["model"]["control"]["forcing"] = {
        "file": str(short_forcing),
        "timestamp_reference": "local_standard_time",
    }
    cfg["model"]["control"]["output"] = {"dir": str(out_dir)}
    cfg["model"]["control"]["start_time"] = None
    cfg["model"]["control"]["end_time"] = None

    cfg_path = tmp_path / "config.yml"
    cfg_path.write_text(yaml.safe_dump(cfg, sort_keys=False), encoding="utf-8")
    return cfg_path, out_dir


def _run_engine(cfg_path: Path) -> subprocess.CompletedProcess[str]:
    """Run ``suews-engine`` against ``cfg_path`` and return the completed process."""
    engine = _locate_suews_engine()
    return subprocess.run(
        [str(engine), "run", str(cfg_path)],
        capture_output=True,
        text=True,
        check=False,
    )


def _read_ipc_table(path: Path) -> pd.DataFrame:
    """Load an Arrow IPC output file produced by ``suews-engine``."""
    with path.open("rb") as fh:
        return ipc.open_file(fh).read_all().to_pandas()


@pytest.fixture(scope="module")
def sample_bridge_inputs():
    """Return a clean sample config dict and 12-step forcing payload."""
    sim = SUEWSSimulation.from_sample_data()
    df_forcing = sim.forcing.df.iloc[:12].copy()
    raw_dict = sim.config.model_dump(exclude_none=True, mode="json")
    forcing_flat = (
        _run_rust._prepare_forcing_block(df_forcing).ravel(order="C").tolist()
    )
    return raw_dict, forcing_flat, len(df_forcing)


def _run_native(
    cfg_dict: dict,
    forcing_flat: list[float],
    count: int,
) -> pd.DataFrame:
    """Execute the native Rust/Fortran kernel in memory and parse output."""
    result = _run_rust._check_rust_available().run_suews(
        yaml.safe_dump(cfg_dict, sort_keys=False),
        forcing_flat,
        count,
    )
    assert result[2] == count
    return _run_rust._parse_output_block(result[0], count, 1)


def _set_sfrs(cfg_dict: dict, sfrs: dict[str, float]) -> None:
    """Assign explicit surface area fractions for all seven land-cover blocks."""
    lc = cfg_dict["sites"][0]["properties"]["land_cover"]
    for surf in _SURFACES:
        lc[surf]["sfr"] = {"value": sfrs[surf]}


def test_default_emission_parity_via_suews_config_and_engine(tmp_path):
    """Stripping defaulted site scalars and land-cover fields emits valid engine YAML (#2001)."""
    sim = SUEWSSimulation.from_sample_data()
    raw = sim.config.model_dump(exclude_none=True, mode="json")
    site = raw["sites"][0]
    site.pop("gridiv", None)
    props = site["properties"]
    for key in _DEFAULTED_SITE_SCALARS:
        props.pop(key, None)

    lc = props["land_cover"]
    for surf in _SURFACES:
        for key in ("emis", "state_limit", "wet_threshold", "irrigation_fraction"):
            lc[surf].pop(key, None)
    for surf in _VEG_SURFACES:
        for key in ("alb_min", "alb_max", "max_conductance"):
            lc[surf].pop(key, None)
    for key in (
        "porosity_min_deciduous",
        "porosity_max_deciduous",
        "capacity_max_deciduous",
        "capacity_min_deciduous",
    ):
        lc["dectr"].pop(key, None)

    # Active urban site with zero tree fractions: Python allows omitting tree heights/FAIs.
    _set_sfrs(
        raw,
        {
            "paved": 0.4,
            "bldgs": 0.4,
            "evetr": 0.0,
            "dectr": 0.0,
            "grass": 0.1,
            "bsoil": 0.05,
            "water": 0.05,
        },
    )
    lc["evetr"].pop("height_evergreen_tree", None)
    lc["evetr"].pop("fai_evergreen_tree", None)
    lc["dectr"].pop("height_deciduous_tree", None)
    lc["dectr"].pop("fai_deciduous_tree", None)

    rebuilt = SUEWSConfig.from_dict(raw)
    dumped = rebuilt.model_dump(exclude_none=True, mode="json")
    cfg_path, out_dir = _stage_engine_case(tmp_path, dumped)
    proc = _run_engine(cfg_path)
    assert proc.returncode == 0, f"stdout:\n{proc.stdout}\nstderr:\n{proc.stderr}"

    suews_files = list(out_dir.glob("*SUEWS*.arrow"))
    assert suews_files, f"no SUEWS output written to {out_dir}"
    df_out = _read_ipc_table(suews_files[0])
    assert len(df_out) > 0
    assert np.isfinite(df_out["QN"].to_numpy()).all()


_REJECTION_CASES = [
    pytest.param({}, {}, ("gridiv",), None, "sites[0].gridiv", id="missing_gridiv"),
    pytest.param(
        {},
        {},
        ("properties", "land_cover", "paved", "sfr"),
        None,
        "sites[gridiv=1].properties.land_cover.paved.sfr",
        id="missing_sfr",
    ),
    pytest.param(
        {},
        {},
        ("properties", "land_cover", "paved", "sfr"),
        {"value": -0.05},
        "sites[gridiv=1].properties.land_cover.paved.sfr",
        id="negative_sfr",
    ),
    pytest.param(
        {},
        {},
        ("properties", "z0m_in"),
        None,
        "sites[gridiv=1].properties.z0m_in",
        id="missing_z0m_in",
    ),
    pytest.param(
        {},
        {},
        ("properties", "zdm_in"),
        None,
        "sites[gridiv=1].properties.zdm_in",
        id="missing_zdm_in",
    ),
    pytest.param(
        {},
        {},
        ("properties", "pipecapacity"),
        None,
        "sites[gridiv=1].properties.pipecapacity",
        id="missing_pipecapacity",
    ),
    pytest.param(
        {},
        {},
        ("properties", "pipecapacity"),
        {"value": None},
        "sites[gridiv=1].properties.pipecapacity",
        id="null_pipecapacity",
    ),
    pytest.param(
        {},
        {},
        ("properties", "pipecapacity"),
        {"value": float("nan")},
        "sites[gridiv=1].properties.pipecapacity",
        id="nan_pipecapacity",
    ),
    pytest.param(
        {},
        {"water": 0.05},
        ("properties", "runofftowater"),
        None,
        "sites[gridiv=1].properties.runofftowater",
        id="missing_runofftowater",
    ),
    pytest.param(
        {},
        {"water": 0.05},
        ("properties", "runofftowater"),
        {"value": None},
        "sites[gridiv=1].properties.runofftowater",
        id="null_runofftowater",
    ),
    pytest.param(
        {},
        {"water": 0.05},
        ("properties", "runofftowater"),
        {"value": float("inf")},
        "sites[gridiv=1].properties.runofftowater",
        id="inf_runofftowater",
    ),
    pytest.param(
        {},
        {},
        ("properties", "n_buildings"),
        None,
        "sites[gridiv=1].properties.n_buildings",
        id="missing_n_buildings",
    ),
    pytest.param(
        {"roughness_sublayer": 2},
        {
            "paved": 0.32,
            "bldgs": 0.68,
            "evetr": 0.0,
            "dectr": 0.0,
            "grass": 0.0,
            "bsoil": 0.0,
            "water": 0.0,
        },
        ("properties", "n_buildings"),
        None,
        "sites[gridiv=1].properties.n_buildings",
        id="missing_n_buildings_rsl2_pai_068_f32_boundary",
    ),
    pytest.param(
        {"roughness_sublayer": 2},
        {
            "paved": 0.32,
            "bldgs": 0.68,
            "evetr": 0.0,
            "dectr": 0.0,
            "grass": 0.0,
            "bsoil": 0.0,
            "water": 0.0,
        },
        ("properties", "h_std"),
        None,
        "sites[gridiv=1].properties.h_std",
        id="missing_h_std_rsl2_pai_068_f32_boundary",
    ),
    pytest.param(
        {},
        {},
        ("properties", "h_std"),
        None,
        "sites[gridiv=1].properties.h_std",
        id="missing_h_std",
    ),
    pytest.param(
        {"storage_heat": 6},
        {},
        ("properties", "lambda_c"),
        None,
        "sites[gridiv=1].properties.lambda_c",
        id="missing_lambda_c",
    ),
    pytest.param(
        {"net_radiation": 0, "snow_use": 0, "storage_heat": 1},
        {},
        ("properties", "land_cover", "paved", "emis"),
        None,
        "sites[gridiv=1].properties.land_cover.paved.emis",
        id="missing_paved_emis_beers",
    ),
    pytest.param(
        {"net_radiation": 0, "snow_use": 0, "storage_heat": 1},
        {},
        ("properties", "land_cover", "bldgs", "emis"),
        None,
        "sites[gridiv=1].properties.land_cover.bldgs.emis",
        id="missing_bldgs_emis_beers",
    ),
    pytest.param(
        {},
        {},
        ("properties", "land_cover", "bldgs", "bldgh"),
        None,
        "sites[gridiv=1].properties.land_cover.bldgs.bldgh",
        id="missing_bldgh",
    ),
    pytest.param(
        {"frontal_area_index": 0},
        {},
        ("properties", "land_cover", "bldgs", "faibldg"),
        None,
        "sites[gridiv=1].properties.land_cover.bldgs.faibldg",
        id="missing_faibldg",
    ),
    pytest.param(
        {},
        {"evetr": 0.05},
        ("properties", "land_cover", "evetr", "height_evergreen_tree"),
        None,
        "sites[gridiv=1].properties.land_cover.evetr.height_evergreen_tree",
        id="missing_evetreeh",
    ),
    pytest.param(
        {"frontal_area_index": 0},
        {"evetr": 0.05},
        ("properties", "land_cover", "evetr", "fai_evergreen_tree"),
        None,
        "sites[gridiv=1].properties.land_cover.evetr.fai_evergreen_tree",
        id="missing_faievetree",
    ),
    pytest.param(
        {},
        {"dectr": 0.05},
        ("properties", "land_cover", "dectr", "height_deciduous_tree"),
        None,
        "sites[gridiv=1].properties.land_cover.dectr.height_deciduous_tree",
        id="missing_dectreeh",
    ),
    pytest.param(
        {"frontal_area_index": 0},
        {"dectr": 0.05},
        ("properties", "land_cover", "dectr", "fai_deciduous_tree"),
        None,
        "sites[gridiv=1].properties.land_cover.dectr.fai_deciduous_tree",
        id="missing_faidectree",
    ),
    pytest.param(
        {},
        {},
        ("properties", "land_cover", "dectr", "porosity_min_deciduous"),
        None,
        "sites[gridiv=1].properties.land_cover.dectr.porosity_min_deciduous",
        id="missing_dectr_pormin",
    ),
    pytest.param(
        {},
        {},
        ("properties", "land_cover", "evetr", "alb_min"),
        None,
        "sites[gridiv=1].properties.land_cover.evetr.alb_min",
        id="missing_evetr_alb_min",
    ),
    pytest.param(
        {},
        {},
        ("properties", "land_cover", "grass", "max_conductance"),
        None,
        "sites[gridiv=1].properties.land_cover.grass.max_conductance",
        id="missing_grass_max_cond",
    ),
    pytest.param(
        {},
        {},
        ("properties", "land_cover", "grass", "wet_threshold"),
        None,
        "sites[gridiv=1].properties.land_cover.grass.wet_threshold",
        id="missing_grass_wet_thresh",
    ),
    pytest.param(
        {"water_use": 0},
        {},
        ("properties", "land_cover", "bsoil", "irrigation_fraction"),
        None,
        "sites[gridiv=1].properties.land_cover.bsoil.irrigation_fraction",
        id="missing_bsoil_irrfrac",
    ),
    pytest.param(
        {},
        {"water": 0.05},
        ("properties", "land_cover", "water", "state_limit"),
        None,
        "sites[gridiv=1].properties.land_cover.water.state_limit",
        id="missing_water_state_limit",
    ),
    pytest.param(
        {},
        {},
        ("properties", "narp_trans_site"),
        {"value": "invalid"},
        "sites[gridiv=1].properties.narp_trans_site",
        id="malformed_inactive_narp_trans",
    ),
    pytest.param(
        {},
        {},
        ("properties", "land_cover", "paved", "state_limit"),
        {"value": "invalid"},
        "sites[gridiv=1].properties.land_cover.paved.state_limit",
        id="malformed_inactive_state_limit",
    ),
]


@pytest.mark.parametrize(
    (
        "physics_overrides",
        "sfr_overrides",
        "target_path",
        "replacement",
        "expected_path",
    ),
    _REJECTION_CASES,
)
def test_active_site_scalar_and_landcover_rejections(
    sample_bridge_inputs,
    physics_overrides,
    sfr_overrides,
    target_path,
    replacement,
    expected_path,
):
    """Native kernel rejects missing active or malformed inactive scalars with canonical paths."""
    base_cfg, forcing_flat, count = sample_bridge_inputs
    cfg = deepcopy(base_cfg)
    phys = cfg["model"]["physics"]
    for key, val in physics_overrides.items():
        phys[key] = {"value": val}
    site = cfg["sites"][0]
    lc = site["properties"]["land_cover"]
    for surf, val in sfr_overrides.items():
        lc[surf]["sfr"] = {"value": val}

    parent = site
    for key in target_path[:-1]:
        parent = parent[key]
    leaf = target_path[-1]
    if replacement is None:
        parent.pop(leaf, None)
    else:
        parent[leaf] = replacement

    with pytest.raises(
        RuntimeError, match=expected_path.replace("[", r"\[").replace("]", r"\]")
    ):
        _run_native(cfg, forcing_flat, count)


def test_inactive_paved_only_omission_is_bit_identical(sample_bridge_inputs):
    """Omitting all inactive scalars on a paved-only observed-radiation run matches exactly."""
    base_cfg, forcing_flat, count = sample_bridge_inputs
    cfg_kept = deepcopy(base_cfg)
    phys = cfg_kept["model"]["physics"]
    phys["net_radiation"] = {"value": 0}
    phys["snow_use"] = {"value": 0}
    phys["storage_heat"] = {"value": 1}
    phys["water_use"] = {"value": 1}
    phys["frontal_area_index"] = {"value": 1}
    phys["roughness_sublayer"] = {"value": 2}
    _set_sfrs(
        cfg_kept,
        {
            "paved": 1.0,
            "bldgs": 0.0,
            "evetr": 0.0,
            "dectr": 0.0,
            "grass": 0.0,
            "bsoil": 0.0,
            "water": 0.0,
        },
    )
    props = cfg_kept["sites"][0]["properties"]
    props["runofftowater"] = {"value": 0.45}
    props["narp_trans_site"] = {"value": 0.37}
    props["n_buildings"] = {"value": 42}
    props["h_std"] = {"value": 9.5}
    props["lambda_c"] = {"value": 2.4}
    lc = props["land_cover"]
    for surf in _SURFACES:
        lc[surf]["emis"] = {"value": 0.91}
        lc[surf]["state_limit"] = {"value": 12.5}
        lc[surf]["irrigation_fraction"] = {"value": 0.75}
    lc["bldgs"]["bldgh"] = {"value": 18.0}
    lc["bldgs"]["faibldg"] = {"value": 0.4}
    lc["evetr"]["height_evergreen_tree"] = {"value": 12.0}
    lc["evetr"]["fai_evergreen_tree"] = {"value": 0.3}
    lc["dectr"]["height_deciduous_tree"] = {"value": 14.0}
    lc["dectr"]["fai_deciduous_tree"] = {"value": 0.35}
    for surf in _VEG_SURFACES:
        lc[surf]["max_conductance"] = {"value": 15.0}

    cfg_omitted = deepcopy(cfg_kept)
    o_props = cfg_omitted["sites"][0]["properties"]
    for key in ("runofftowater", "narp_trans_site", "n_buildings", "h_std", "lambda_c"):
        o_props.pop(key, None)
    o_lc = o_props["land_cover"]
    for surf in _SURFACES:
        for key in ("emis", "state_limit", "irrigation_fraction"):
            o_lc[surf].pop(key, None)
    o_lc["bldgs"].pop("bldgh", None)
    o_lc["bldgs"].pop("faibldg", None)
    o_lc["evetr"].pop("height_evergreen_tree", None)
    o_lc["evetr"].pop("fai_evergreen_tree", None)
    o_lc["dectr"].pop("height_deciduous_tree", None)
    o_lc["dectr"].pop("fai_deciduous_tree", None)
    for surf in _VEG_SURFACES:
        o_lc[surf].pop("max_conductance", None)

    df_kept = _run_native(cfg_kept, forcing_flat, count)
    df_omitted = _run_native(cfg_omitted, forcing_flat, count)
    pd.testing.assert_frame_equal(df_omitted, df_kept, check_exact=True)


def test_inactive_roughness_in_and_tree_fields_are_bit_identical(sample_bridge_inputs):
    """Omitting z0m_in/zdm_in under forced RSL and absent-tree heights/FAIs matches exactly."""
    base_cfg, forcing_flat, count = sample_bridge_inputs
    cfg_kept = deepcopy(base_cfg)
    phys = cfg_kept["model"]["physics"]
    phys["roughness_length_momentum"] = {"value": 2}
    phys["roughness_sublayer"] = {"value": 1}
    phys["frontal_area_index"] = {"value": 0}
    _set_sfrs(
        cfg_kept,
        {
            "paved": 0.4,
            "bldgs": 0.4,
            "evetr": 0.0,
            "dectr": 0.0,
            "grass": 0.1,
            "bsoil": 0.05,
            "water": 0.05,
        },
    )
    props = cfg_kept["sites"][0]["properties"]
    props["z0m_in"] = {"value": 2.3}
    props["zdm_in"] = {"value": 8.7}
    lc = props["land_cover"]
    lc["evetr"]["height_evergreen_tree"] = {"value": 16.0}
    lc["evetr"]["fai_evergreen_tree"] = {"value": 0.42}
    lc["dectr"]["height_deciduous_tree"] = {"value": 19.0}
    lc["dectr"]["fai_deciduous_tree"] = {"value": 0.38}

    cfg_omitted = deepcopy(cfg_kept)
    o_props = cfg_omitted["sites"][0]["properties"]
    o_props.pop("z0m_in", None)
    o_props.pop("zdm_in", None)
    o_lc = o_props["land_cover"]
    o_lc["evetr"].pop("height_evergreen_tree", None)
    o_lc["evetr"].pop("fai_evergreen_tree", None)
    o_lc["dectr"].pop("height_deciduous_tree", None)
    o_lc["dectr"].pop("fai_deciduous_tree", None)

    df_kept = _run_native(cfg_kept, forcing_flat, count)
    df_omitted = _run_native(cfg_omitted, forcing_flat, count)
    for group in ("SUEWS", "DailyState", "RSL"):
        if group in df_kept.columns.get_level_values(0):
            pd.testing.assert_frame_equal(
                df_omitted[group], df_kept[group], check_exact=True
            )


def test_inactive_water_only_snow_omission_is_bit_identical(sample_bridge_inputs):
    """Water-only snow run omitting pipecapacity, runofftowater, and non-water wet_threshold matches."""
    base_cfg, forcing_flat, count = sample_bridge_inputs
    cfg_kept = deepcopy(base_cfg)
    phys = cfg_kept["model"]["physics"]
    phys["snow_use"] = {"value": 1}
    phys["roughness_sublayer"] = {"value": 0}
    _set_sfrs(
        cfg_kept,
        {
            "paved": 0.0,
            "bldgs": 0.0,
            "evetr": 0.0,
            "dectr": 0.0,
            "grass": 0.0,
            "bsoil": 0.0,
            "water": 1.0,
        },
    )
    props = cfg_kept["sites"][0]["properties"]
    props["pipecapacity"] = {"value": 250.0}
    props["runofftowater"] = {"value": 0.6}
    lc = props["land_cover"]
    for surf in ("paved", "bldgs", "evetr", "dectr", "grass", "bsoil"):
        lc[surf]["wet_threshold"] = {"value": 3.5}

    cfg_omitted = deepcopy(cfg_kept)
    o_props = cfg_omitted["sites"][0]["properties"]
    o_props.pop("pipecapacity", None)
    o_props.pop("runofftowater", None)
    o_lc = o_props["land_cover"]
    for surf in ("paved", "bldgs", "evetr", "dectr", "grass", "bsoil"):
        o_lc[surf].pop("wet_threshold", None)

    df_kept = _run_native(cfg_kept, forcing_flat, count)
    df_omitted = _run_native(cfg_omitted, forcing_flat, count)
    pd.testing.assert_frame_equal(df_omitted, df_kept, check_exact=True)


@pytest.mark.parametrize(
    "null_value", [None, {"value": None}], ids=["bare_null", "wrapped_null"]
)
def test_explicit_null_irrigation_fraction_rejected_when_modelled_and_allowed_when_observed(
    sample_bridge_inputs, null_value
):
    """Explicit null irrigation_fraction (#2101 gap) fails loudly when modelled, passes when observed."""
    base_cfg, forcing_flat, count = sample_bridge_inputs

    # Active modelled irrigation (water_use=0): explicit null dropped by exclude_none
    # is caught by the strict Rust bridge instead of silently substituting 0.0.
    raw_modelled = deepcopy(base_cfg)
    raw_modelled["model"]["physics"]["water_use"] = {"value": 0}
    raw_modelled["sites"][0]["properties"]["land_cover"]["grass"][
        "irrigation_fraction"
    ] = null_value
    dumped_modelled = SUEWSConfig.from_dict(raw_modelled).model_dump(
        exclude_none=True, mode="json"
    )
    with pytest.raises(
        RuntimeError,
        match=r"sites\[gridiv=1\]\.properties\.land_cover\.grass\.irrigation_fraction",
    ):
        _run_native(dumped_modelled, forcing_flat, count)

    # Inactive observed irrigation (water_use=1): explicit null passes with bit-identical output.
    raw_observed = deepcopy(base_cfg)
    raw_observed["model"]["physics"]["water_use"] = {"value": 1}
    df_kept = _run_native(raw_observed, forcing_flat, count)
    raw_observed["sites"][0]["properties"]["land_cover"]["grass"][
        "irrigation_fraction"
    ] = null_value
    dumped_observed = SUEWSConfig.from_dict(raw_observed).model_dump(
        exclude_none=True, mode="json"
    )
    df_null = _run_native(dumped_observed, forcing_flat, count)
    pd.testing.assert_frame_equal(df_null, df_kept, check_exact=True)
