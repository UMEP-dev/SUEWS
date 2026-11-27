"""End-to-end engine and data-model guards for site scalars and land cover (#2002, #2349)."""

from copy import deepcopy
from importlib import import_module
from importlib.resources import files
from pathlib import Path
import subprocess

import numpy as np
import pandas as pd
from pyarrow import ipc
import pytest
import yaml

from supy.data_model import SUEWSConfig

_run_rust = import_module("supy._run_rust")
_YAML_DUMPER = getattr(yaml, "CSafeDumper", yaml.SafeDumper)

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
    pytest.mark.physics,
    pytest.mark.core,
    pytest.mark.contract("C-STRICT", "C-EMIT", "C-NATIVE-ERROR"),
]


def _dump_yaml(doc: dict) -> str:
    return yaml.dump(doc, Dumper=_YAML_DUMPER, sort_keys=False)


@pytest.fixture(scope="module")
def _short_forcing_text() -> str:
    """Cache 13-line hourly forcing prefix from packaged sample data."""
    raw = (files("supy") / "sample_data" / "Kc_2012_data_60.txt").read_text(
        encoding="utf-8"
    )
    lines = raw.splitlines()
    return "\n".join(lines[:14]) + "\n"


def _stage_engine_case(
    tmp_path: Path, config_dict: dict, short_forcing_text: str
) -> tuple[Path, Path]:
    """Write a short forcing slice and YAML config for ``suews-engine``."""
    short_forcing = tmp_path / "forcing.txt"
    short_forcing.write_text(short_forcing_text, encoding="utf-8")

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
    cfg_path.write_text(_dump_yaml(cfg), encoding="utf-8")
    return cfg_path, out_dir


def _run_engine(engine: Path, cfg_path: Path) -> subprocess.CompletedProcess[str]:
    """Run ``suews-engine`` against ``cfg_path`` and return the completed process."""
    return subprocess.run(
        [str(engine), "run", str(cfg_path)],
        capture_output=True,
        text=True,
        timeout=60,
        check=False,
    )


def _read_ipc_table(path: Path) -> pd.DataFrame:
    """Load an Arrow IPC output file produced by ``suews-engine``."""
    with path.open("rb") as fh:
        return ipc.open_file(fh).read_all().to_pandas()


@pytest.fixture(scope="module")
def sample_bridge_inputs(sample_config_loaded, sample_forcing_parsed):
    """Return a clean sample config dict and 12-step forcing payloads (default and observed QN)."""
    _, df_forcing = sample_forcing_parsed(12)
    df_forcing_obs_qn = df_forcing.copy()
    df_forcing_obs_qn["qn"] = 50.0
    raw_dict = sample_config_loaded.model_dump(exclude_none=True, mode="json")
    # Establish positive baseline that the unmutated dict passes SUEWSConfig.
    SUEWSConfig.from_dict(deepcopy(raw_dict))
    forcing_flat = (
        _run_rust._prepare_forcing_block(df_forcing).ravel(order="C").tolist()
    )
    forcing_obs_qn_flat = (
        _run_rust._prepare_forcing_block(df_forcing_obs_qn).ravel(order="C").tolist()
    )
    return raw_dict, forcing_flat, forcing_obs_qn_flat, len(df_forcing)


def _run_native(
    cfg_dict: dict,
    forcing_flat: list[float],
    count: int,
) -> pd.DataFrame:
    """Execute the native Rust/Fortran kernel in memory and parse output."""
    result = _run_rust._check_rust_available().run_suews(
        _dump_yaml(cfg_dict),
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


@pytest.mark.e2e
def test_default_emission_parity_via_suews_config_and_engine(
    tmp_path: Path,
    suews_engine_bin: Path,
    sample_bridge_inputs,
    _short_forcing_text: str,
):
    """Stripping defaulted site scalars and land-cover fields emits valid engine YAML (#2001)."""
    base_cfg, _, _, _ = sample_bridge_inputs
    raw = deepcopy(base_cfg)
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
        site["initial_states"][surf]["alb_id"] = {"value": 0.25}
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
    cfg_path, out_dir = _stage_engine_case(tmp_path, dumped, _short_forcing_text)
    proc = _run_engine(suews_engine_bin, cfg_path)
    assert proc.returncode == 0, f"stdout:\n{proc.stdout}\nstderr:\n{proc.stderr}"

    arrow_path = out_dir / "suews_output.arrow"
    assert arrow_path.is_file(), f"no suews_output.arrow written to {out_dir}"
    df_out = _read_ipc_table(arrow_path)
    assert len(df_out) > 0
    qn_col = "QN" if "QN" in df_out.columns else "qn"
    assert qn_col in df_out.columns
    assert np.isfinite(df_out[qn_col].to_numpy()).all()


_WITNESS_CASES = [
    # Site scalars subsystem: missing, null, malformed
    pytest.param(
        ("properties", "pipecapacity"),
        None,
        "sites[gridiv=1].properties.pipecapacity",
        None,
        id="site-scalar-missing-pipecapacity",
    ),
    pytest.param(
        ("properties", "pipecapacity"),
        {"value": None},
        "sites[gridiv=1].properties.pipecapacity",
        ("pipecapacity", "null"),
        id="site-scalar-null-pipecapacity",
    ),
    pytest.param(
        ("properties", "narp_trans_site"),
        {"value": "invalid"},
        "sites[gridiv=1].properties.narp_trans_site",
        ("narp_trans_site",),
        id="site-scalar-malformed-narp_trans_site",
    ),
    # Land-cover scalars subsystem: missing, null, malformed
    pytest.param(
        ("properties", "land_cover", "paved", "sfr"),
        None,
        "sites[gridiv=1].properties.land_cover.paved.sfr",
        None,
        id="landcover-missing-sfr",
    ),
    pytest.param(
        ("properties", "land_cover", "paved", "sfr"),
        {"value": None},
        "sites[gridiv=1].properties.land_cover.paved.sfr",
        ("land_cover.paved.sfr", "null"),
        id="landcover-null-sfr",
    ),
    pytest.param(
        ("properties", "land_cover", "paved", "sfr"),
        {"value": -0.05},
        "sites[gridiv=1].properties.land_cover.paved.sfr",
        ("land_cover.paved.sfr",),
        id="landcover-malformed-negative-sfr",
    ),
]


@pytest.mark.e2e
@pytest.mark.parametrize(
    ("target_path", "replacement", "expected_path", "public_needles"),
    _WITNESS_CASES,
)
def test_site_scalar_and_landcover_br02_failure_witnesses(
    tmp_path: Path,
    suews_engine_bin: Path,
    sample_bridge_inputs,
    _short_forcing_text: str,
    target_path: tuple[str, ...],
    replacement,
    expected_path: str,
    public_needles: tuple[str, ...] | None,
):
    """BR-02 witnesses for site scalars and land cover across Python and suews-engine."""
    base_cfg, forcing_flat, _, count = sample_bridge_inputs
    cfg = deepcopy(base_cfg)
    site = cfg["sites"][0]

    parent = site
    for key in target_path[:-1]:
        parent = parent[key]
    leaf = target_path[-1]
    if replacement is None:
        parent.pop(leaf, None)
    else:
        parent[leaf] = replacement

    # 1. PyO3 Python-bridge witness
    with pytest.raises(
        RuntimeError, match=expected_path.replace("[", r"\[").replace("]", r"\]")
    ):
        _run_native(cfg, forcing_flat, count)

    # 2. Standalone suews-engine witness
    cfg_path, out_dir = _stage_engine_case(tmp_path, cfg, _short_forcing_text)
    proc = _run_engine(suews_engine_bin, cfg_path)
    assert proc.returncode != 0, (
        f"invalid {expected_path} must not exit 0 (stdout={proc.stdout})"
    )
    assert expected_path in f"{proc.stdout}\n{proc.stderr}", proc.stderr
    assert not (out_dir / "suews_output.arrow").exists()

    # 3. Public Python SUEWSConfig entry point witness
    if public_needles is not None:
        with pytest.raises((ValueError, TypeError)) as cfg_exc:
            SUEWSConfig.from_dict(cfg)
        msg = str(cfg_exc.value)
        for needle in public_needles:
            assert needle in msg, f"expected {needle!r} in {msg!r}"


def test_inactive_paved_only_omission_is_bit_identical(sample_bridge_inputs):
    """Omitting all inactive scalars on a paved-only observed-radiation run matches exactly."""
    base_cfg, _, forcing_obs_qn_flat, count = sample_bridge_inputs
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

    df_kept = _run_native(cfg_kept, forcing_obs_qn_flat, count)
    df_omitted = _run_native(cfg_omitted, forcing_obs_qn_flat, count)
    pd.testing.assert_frame_equal(df_omitted, df_kept, check_exact=True)


def test_inactive_roughness_in_and_tree_fields_are_bit_identical(sample_bridge_inputs):
    """Omitting z0m_in/zdm_in under forced RSL and absent-tree heights/FAIs matches exactly."""
    base_cfg, forcing_flat, _, count = sample_bridge_inputs
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
    base_cfg, forcing_flat, _, count = sample_bridge_inputs
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
    base_cfg, forcing_flat, _, count = sample_bridge_inputs

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
