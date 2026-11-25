"""Layer 3 parity suite: 5-way entry-point equivalence and df_state/YAML ratchet.

gh#2099 (part of gh#2083; Refs #1991, #2001, #2002, #2077):

1. Verifies that all five public configuration entry points agree on both
   valid positive controls and single-field negative controls:
   - ``SUEWSConfig.from_yaml(path)``
   - ``SUEWSConfig.from_dict(raw_dict)``
   - Programmatic ``SUEWSConfig(**kwargs)`` at the execution/export handoff
     (``SUEWSSimulation``, ``config_to_engine_yaml``, ``_grid_config_jsons``)
   - ``SUEWSSimulation.update_config(...)``
   - ``suews validate`` (``validate_single_file`` and Phase A round-trip)
2. Exercises the shipped ``suews validate`` CLI and feeds the
   validated YAML artefact into ``SUEWSSimulation`` for a real short run,
   paired with a single-field negative control that must fail.
3. Enforces a strict shrinking parity ratchet comparing ``cfg.to_df_state()``
   against ``config_to_engine_yaml(cfg)`` pinned in
   ``test/data_model/known_df_yaml_parity_gaps.json``, failing on both newly
   introduced shadow-default divergences and stale fixed entries.
"""

from __future__ import annotations

import copy
from importlib.resources import as_file, files
import json
import math
from pathlib import Path
from typing import Any

from click.testing import CliRunner
import numpy as np
import pandas as pd
import pytest
import yaml

from supy._env import trv_supy_module
from supy._run_rust import (
    _grid_config_jsons,
    config_to_engine_yaml,
)
from supy.cmd.suews_cli import cli as suews_cli
from supy.cmd.validate_config import validate_single_file
from supy.data_model.core import SUEWSConfig
from supy.data_model.core.model import Model, ModelPhysics
from supy.data_model.core.site import LAIParams
from supy.data_model.core.type import RefValue
from supy.data_model.schema import generate_json_schema
from supy.data_model.validation.pipeline.phase_a import annotate_missing_parameters
from supy.suews_sim import SUEWSSimulation

pytestmark = [
    pytest.mark.api,
    pytest.mark.cfg,
    pytest.mark.contract("C-PARITY", "C-VALID", "C-ROUNDTRIP", "C-EMIT"),
]

GAPS_BASELINE_PATH = Path(__file__).parent / "known_df_yaml_parity_gaps.json"

SURFACES: tuple[str, ...] = (
    "paved",
    "bldgs",
    "evetr",
    "dectr",
    "grass",
    "bsoil",
    "water",
)
VEG_SURFACES: tuple[str, ...] = ("evetr", "dectr", "grass")
OHM_SEASONS: tuple[str, ...] = ("summer_dry", "summer_wet", "winter_dry", "winter_wet")
OHM_COEFS: tuple[str, ...] = ("a1", "a2", "a3")
LAI_POWER_FIELDS: tuple[str, ...] = (
    "growth_lai",
    "growth_gdd",
    "senescence_lai",
    "senescence_sdd",
)
SHORT_RUN_STEPS = 12  # 1 hour at 300 s timestep
# Only test-owned serialisation and assertion parsing use libyaml. Public
# from_yaml/Phase A/CLI loaders still execute their normal implementation.
_TEST_YAML_LOADER = getattr(yaml, "CSafeLoader", yaml.SafeLoader)
_TEST_YAML_DUMPER = getattr(yaml, "CSafeDumper", yaml.SafeDumper)


# ---------------------------------------------------------------------------
# Shared fixtures and helpers
# ---------------------------------------------------------------------------


@pytest.fixture(scope="module")
def sample_dict() -> dict[str, Any]:
    """Load the packaged sample configuration as a plain dict."""
    resource = trv_supy_module / "sample_data" / "sample_config.yml"
    return yaml.load(resource.read_text(encoding="utf-8"), Loader=_TEST_YAML_LOADER)


def _strip_ref_metadata(obj: Any) -> Any:
    """Recursively omit optional ``ref`` citation blocks."""
    if isinstance(obj, dict):
        return {k: _strip_ref_metadata(v) for k, v in obj.items() if k != "ref"}
    if isinstance(obj, list):
        return [_strip_ref_metadata(v) for v in obj]
    return obj


@pytest.fixture(scope="module")
def compact_valid_dict(sample_dict: dict[str, Any]) -> dict[str, Any]:
    """Smallest valid public SUEWS configuration preserving all core surfaces and physics.

    Omits optional defaulted blocks not exercised by the test (disabled STEBBS
    and building-archetype 10-minute schedules, optional irrigation/LUMPS/SPARTACUS
    blocks, 24-hour sub-profiles, and ``ref`` citation metadata) and uses a valid
    2-layer vertical profile so real public YAML parsing, schema checks, and
    Pydantic validation run without repeatedly parsing 1,456 lines of unused
    10-minute schedules.
    """
    compact = _strip_ref_metadata(copy.deepcopy(sample_dict))
    props = compact["sites"][0]["properties"]
    for opt_block in ("stebbs", "building_archetype", "irrigation", "lumps", "spartacus"):
        props.pop(opt_block, None)
    if isinstance(props.get("snow"), dict):
        props["snow"].pop("snow_profile_24hr", None)
    ae = props.get("anthropogenic_emissions")
    if isinstance(ae, dict):
        ae.pop("heat", None)
        if isinstance(ae.get("co2"), dict):
            for key in (
                "profile_traffic_24hr",
                "profile_human_activity_24hr",
                "emission_factor_co2_vehicle",
                "traffic_rate",
            ):
                ae["co2"].pop(key, None)
    vl = props["vertical_layers"]
    vl["nlayer"] = {"value": 2}
    vl["height"] = {"value": [0.0, 10.0, 20.0]}
    for key in ("veg_frac", "veg_scale", "veg_ext", "building_frac", "building_scale"):
        val = vl[key]
        if isinstance(val, dict) and "value" in val:
            vl[key] = {"value": val["value"][:2]}
        elif isinstance(val, list):
            vl[key] = val[:2]
    vl["roofs"] = vl["roofs"][:2]
    vl["walls"] = vl["walls"][:2]
    init_st = compact["sites"][0]["initial_states"]
    init_st["roofs"] = init_st["roofs"][:2]
    init_st["walls"] = init_st["walls"][:2]
    return compact


@pytest.fixture(scope="module")
def short_sample_forcing(tmp_path_factory: pytest.TempPathFactory):
    """Load the first 288 five-minute forcing rows (1 day) via the real file loader.

    Produces rows identical to ``sample_forcing_parsed(n)`` for ``n <= 288``
    without parsing and disaggregating all 8,784 hourly rows of the full year
    on every pytest worker.
    """
    sample_dir = files("supy").joinpath("sample_data")
    forcing_src = sample_dir / "Kc_2012_data_60.txt"
    lines = forcing_src.read_text(encoding="utf-8").splitlines()
    tmp_dir = tmp_path_factory.mktemp("short_forcing")
    short_file = tmp_dir / "Kc_2012_data_60.txt"
    short_file.write_text("\n".join(lines[:26]) + "\n", encoding="utf-8")
    df_day = SUEWSSimulation._load_forcing_file(short_file, tstep_mod=300).iloc[:288]

    def _slice(n_steps: int) -> pd.DataFrame:
        if n_steps > len(df_day):
            raise ValueError(f"Requested {n_steps} steps > cached {len(df_day)}")
        return df_day.iloc[:n_steps].copy()

    return _slice


@pytest.fixture(scope="module")
def current_json_schema() -> dict[str, Any]:
    """Generate the current JSON schema once per module for validate_single_file."""
    return generate_json_schema()


def _unwrap(val: Any) -> Any:
    if isinstance(val, dict) and set(val.keys()) <= {"value", "ref"} and "value" in val:
        return val["value"]
    return val


def _write_yaml(path: Path, payload: dict[str, Any]) -> Path:
    path.write_text(
        yaml.dump(payload, sort_keys=False, Dumper=_TEST_YAML_DUMPER), encoding="utf-8"
    )
    return path


def _copy_sample_data(dst: Path, *, max_forcing_rows: int = 24) -> Path:
    """Copy packaged sample_data files into ``dst`` via the Traversable protocol.

    Truncates the hourly forcing file to its header plus ``max_forcing_rows``
    data rows so ``SUEWSSimulation`` exercises real sibling forcing auto-loading
    without spending seconds parsing a full year of forcing rows.
    """
    sample_dir = files("supy").joinpath("sample_data")
    for resource in sample_dir.iterdir():
        if not resource.is_file():
            continue
        target = dst / resource.name
        if resource.name.endswith(".txt") and max_forcing_rows > 0:
            lines = resource.read_text(encoding="utf-8").splitlines()
            target.write_text(
                "\n".join(lines[: max_forcing_rows + 1]) + "\n", encoding="utf-8"
            )
        else:
            target.write_bytes(resource.read_bytes())
    return dst / "sample_config.yml"


_LOADER_BOOKKEEPING_KEYS = frozenset({
    "_yaml_path",
    "_auto_generate_annotated",
    "_yaml_raw",
})


def _strip_loader_bookkeeping(doc: dict[str, Any]) -> dict[str, Any]:
    """Strip root loader bookkeeping keys added by ``SUEWSConfig._prepare_raw_input``.

    ``from_yaml`` attaches ``_yaml_path``, ``_auto_generate_annotated``, and
    ``_yaml_raw`` via root ``extra="allow"``; ``from_dict`` attaches the latter
    two; programmatic ``SUEWSConfig(**kwargs)`` attaches none. The Rust engine
    ignores these root bookkeeping keys and reads only ``model`` and ``sites``.
    """
    return {k: v for k, v in doc.items() if k not in _LOADER_BOOKKEEPING_KEYS}


def _normalise_engine_dict(raw_yaml: str) -> dict[str, Any]:
    """Parse engine YAML into a plain dict for structural/numeric comparison."""
    return _strip_loader_bookkeeping(yaml.load(raw_yaml, Loader=_TEST_YAML_LOADER))


def _get_yaml_path(doc: dict[str, Any], path: str) -> Any:
    """Resolve a dotted/indexed path such as ``sites[0].properties.lat``."""
    cur: Any = doc
    for part in path.split("."):
        if cur is None:
            return None
        if "[" in part and part.endswith("]"):
            key, idx_str = part[:-1].split("[", 1)
            if key:
                if not isinstance(cur, dict) or key not in cur:
                    return None
                cur = cur[key]
            idx = int(idx_str)
            if not isinstance(cur, list) or idx >= len(cur):
                return None
            cur = cur[idx]
        else:
            if not isinstance(cur, dict) or part not in cur:
                return None
            cur = cur[part]
    return _unwrap(cur)


def _get_df_val(df: pd.DataFrame, col: tuple[str, str]) -> Any:
    if col not in df.columns:
        return None
    val = df.iat[0, df.columns.get_loc(col)]
    if pd.isna(val):
        return None
    if hasattr(val, "item"):
        return val.item()
    return val


def _scalar_entry(
    df_col: tuple[str, str], yaml_path: str, owner: str = "#2309"
) -> dict[str, Any]:
    return {
        "kind": "scalar",
        "df_col": list(df_col),
        "yaml_path": yaml_path,
        "owner_issue": owner,
    }


def _vector_entry(
    df_var: str,
    df_indices: list[str],
    df_col_label: str,
    yaml_path: str,
    owner: str = "#2309",
) -> dict[str, Any]:
    return {
        "kind": "vector",
        "df_var": df_var,
        "df_indices": df_indices,
        "df_col": [df_var, df_col_label],
        "yaml_path": yaml_path,
        "owner_issue": owner,
    }


def _model_and_site_scalar_mappings() -> list[dict[str, Any]]:
    """Map model control/physics and site-level scalar blocks."""
    entries: list[dict[str, Any]] = [
        _scalar_entry(("tstep", "0"), "model.control.tstep"),
        _scalar_entry(("diagnose", "0"), "model.control.diagnose"),
    ]
    for field, col in ModelPhysics._FIELD_COL_PAIRS.default:
        entries.append(_scalar_entry((col, "0"), f"model.physics.{field}"))
    entries.append(
        _scalar_entry(("sw_dn_direct_frac", "0"), "model.physics.sw_dn_direct_frac")
    )
    for field, col in ModelPhysics._STEBBS_FIELD_COL_PAIRS.default:
        entries.append(_scalar_entry((col, "0"), f"model.physics.stebbs.{field}"))

    site_groups: tuple[tuple[str, tuple[str, ...]], ...] = (
        (
            "sites[0].properties",
            (
                "lat",
                "lng",
                "alt",
                "timezone",
                "surfacearea",
                "z",
                "z0m_in",
                "zdm_in",
                "pipecapacity",
                "runofftowater",
                "narp_trans_site",
                "n_buildings",
                "h_std",
                "lambda_c",
            ),
        ),
        (
            "sites[0].properties.lumps",
            ("raincover", "rainmaxres", "drainrt", "veg_type"),
        ),
        (
            "sites[0].properties.spartacus",
            (
                "air_ext_lw",
                "air_ext_sw",
                "air_ssa_lw",
                "air_ssa_sw",
                "ground_albedo_dir_mult_fact",
                "n_stream_lw_urban",
                "n_stream_sw_urban",
                "n_stream_lw_forest",
                "n_stream_sw_forest",
                "n_vegetation_region_urban",
                "n_vegetation_region_forest",
                "sw_dn_direct_frac",
                "use_sw_direct_albedo",
                "veg_contact_fraction_const",
                "veg_fsd_const",
                "veg_ssa_lw",
                "veg_ssa_sw",
            ),
        ),
        (
            "sites[0].properties.conductance",
            (
                "g_max",
                "g_k",
                "g_q_base",
                "g_q_shape",
                "g_t",
                "g_sm",
                "kmax",
                "s1",
                "s2",
                "tl",
                "th",
            ),
        ),
        (
            "sites[0].properties.irrigation",
            ("h_maintain", "faut", "ie_start", "ie_end", "internalwateruse_h"),
        ),
        (
            "sites[0].properties.anthropogenic_emissions",
            ("startdls", "enddls"),
        ),
    )
    for prefix, fields in site_groups:
        for field in fields:
            entries.append(_scalar_entry((field, "0"), f"{prefix}.{field}"))

    for col, field in (
        ("co2pointsource", "emission_co2_point_source"),
        ("ef_umolco2perj", "emission_factor_co2_fuel"),
        ("enef_v_jkm", "emission_factor_energy_vehicle"),
        ("frfossilfuel_heat", "fraction_fossil_fuel_heating"),
        ("frfossilfuel_nonheat", "fraction_fossil_fuel_non_heating"),
        ("maxfcmetab", "emission_co2_metabolism_max"),
        ("maxqfmetab", "emission_heat_metabolism_max"),
        ("minfcmetab", "emission_co2_metabolism_min"),
        ("minqfmetab", "emission_heat_metabolism_min"),
        ("trafficunits", "type_traffic_rate"),
    ):
        entries.append(
            _scalar_entry(
                (col, "0"),
                f"sites[0].properties.anthropogenic_emissions.co2.{field}",
            )
        )
    return entries


def _snow_mappings() -> list[dict[str, Any]]:
    """Map snow scalars, thresholds, and OHM coefficients."""
    entries: list[dict[str, Any]] = []
    for col, field in (
        ("crwmax", "water_holding_capacity_max"),
        ("crwmin", "water_holding_capacity_min"),
        ("narp_emis_snow", "narp_emissivity_snow"),
        ("preciplimit", "temperature_rain_snow_threshold"),
        ("preciplimitalb", "precipitation_threshold_albedo_reset"),
        ("snowalbmax", "snow_albedo_max"),
        ("snowalbmin", "snow_albedo_min"),
        ("snowdensmin", "snow_density_min"),
        ("snowdensmax", "snow_density_max"),
        ("snowlimbldg", "snow_depth_limit_building"),
        ("snowlimpaved", "snow_depth_limit_paved"),
        ("tau_a", "tau_cold_snow"),
        ("tau_f", "tau_melting_snow"),
        ("tau_r", "tau_refreezing_snow"),
        ("tempmeltfact", "temperature_melt_factor"),
        ("radmeltfact", "radiation_melt_factor"),
    ):
        entries.append(
            _scalar_entry((col, "0"), f"sites[0].properties.snow.{field}", "#2101")
        )
    entries.append(
        _scalar_entry(
            ("ohm_threshsw", "(7,)"),
            "sites[0].properties.snow.ohm_threshold_summer_winter",
        )
    )
    entries.append(
        _scalar_entry(
            ("ohm_threshwd", "(7,)"),
            "sites[0].properties.snow.ohm_threshold_wet_dry",
        )
    )
    for s_idx, season in enumerate(OHM_SEASONS):
        for c_idx, coef in enumerate(OHM_COEFS):
            entries.append(
                _scalar_entry(
                    ("ohm_coef", f"(7, {s_idx}, {c_idx})"),
                    f"sites[0].properties.snow.ohm_coef.{season}.{coef}",
                )
            )
    return entries


def _single_surface_mappings(surf_idx: int, surf_name: str) -> list[dict[str, Any]]:
    """Map common per-surface land-cover, OHM, soil, drainage, and thermal fields."""
    base = f"sites[0].properties.land_cover.{surf_name}"
    entries: list[dict[str, Any]] = [
        _scalar_entry(("sfr_surf", f"({surf_idx},)"), f"{base}.sfr"),
        _scalar_entry(("emis", f"({surf_idx},)"), f"{base}.emis"),
        _scalar_entry(("chanohm", f"({surf_idx},)"), f"{base}.ch_anohm"),
        _scalar_entry(("cpanohm", f"({surf_idx},)"), f"{base}.rho_cp_anohm"),
        _scalar_entry(("kkanohm", f"({surf_idx},)"), f"{base}.k_anohm"),
        _scalar_entry(
            ("ohm_threshsw", f"({surf_idx},)"), f"{base}.ohm_threshold_summer_winter"
        ),
        _scalar_entry(
            ("ohm_threshwd", f"({surf_idx},)"), f"{base}.ohm_threshold_wet_dry"
        ),
        _scalar_entry(("soildepth", f"({surf_idx},)"), f"{base}.soil_depth", "#2077"),
        _scalar_entry(
            ("soilstorecap_surf", f"({surf_idx},)"),
            f"{base}.soil_store_capacity",
            "#2077",
        ),
        _scalar_entry(("statelimit_surf", f"({surf_idx},)"), f"{base}.state_limit"),
        _scalar_entry(("wetthresh_surf", f"({surf_idx},)"), f"{base}.wet_threshold"),
        _scalar_entry(
            ("sathydraulicconduct", f"({surf_idx},)"),
            f"{base}.saturated_hydraulic_conductivity",
            "#2077",
        ),
        _scalar_entry(
            ("soildensity", f"({surf_idx},)"), f"{base}.soil_density", "#2309"
        ),
        _scalar_entry(("snowpacklimit", f"({surf_idx},)"), f"{base}.snowpack_limit"),
        _scalar_entry((f"irrfrac{surf_name}", "0"), f"{base}.irrigation_fraction"),
    ]
    for s_idx, season in enumerate(OHM_SEASONS):
        for c_idx, coef in enumerate(OHM_COEFS):
            entries.append(
                _scalar_entry(
                    ("ohm_coef", f"({surf_idx}, {s_idx}, {c_idx})"),
                    f"{base}.ohm_coef.{season}.{coef}",
                )
            )
    for i, field in enumerate((
        "store_min",
        "drain_eq",
        "drain_coef_1",
        "drain_coef_2",
        "store_max",
        "store_cap",
    )):
        entries.append(
            _scalar_entry(
                ("storedrainprm", f"({i}, {surf_idx})"),
                f"{base}.storage_drain_params.{field}",
                "#2101",
            )
        )
    for df_var, field in (
        ("dz_surf", "dz"),
        ("k_surf", "k"),
        ("cp_surf", "rho_cp"),
    ):
        entries.append(
            _vector_entry(
                df_var,
                [f"({surf_idx}, {i})" for i in range(5)],
                f"({surf_idx}, 0..4)",
                f"{base}.thermal_layers.{field}",
                "#2309",
            )
        )
    if surf_name in ("paved", "bldgs", "bsoil", "water"):
        entries.append(_scalar_entry(("alb", f"({surf_idx},)"), f"{base}.alb"))
    if surf_name != "water":
        for dst_idx, dst_field in enumerate((
            "to_paved",
            "to_bldgs",
            "to_evetr",
            "to_dectr",
            "to_grass",
            "to_bsoil",
            "to_water",
        )):
            entries.append(
                _scalar_entry(
                    ("waterdist", f"({dst_idx}, {surf_idx})"),
                    f"{base}.waterdist.{dst_field}",
                )
            )
        terminal = "to_runoff" if surf_name in ("paved", "bldgs") else "to_soilstore"
        entries.append(
            _scalar_entry(
                ("waterdist", f"(7, {surf_idx})"), f"{base}.waterdist.{terminal}"
            )
        )
    return entries


def _vegetation_and_morphology_mappings() -> list[dict[str, Any]]:
    """Map building/tree morphology, water flowchange, and vegetated bio-CO2/LAI fields."""
    entries: list[dict[str, Any]] = [
        _scalar_entry(
            ("flowchange", "0"), "sites[0].properties.land_cover.water.flowchange"
        ),
        _scalar_entry(
            ("faibldg", "0"), "sites[0].properties.land_cover.bldgs.faibldg", "#2001"
        ),
        _scalar_entry(
            ("bldgh", "0"), "sites[0].properties.land_cover.bldgs.bldgh", "#2001"
        ),
        _scalar_entry(
            ("faievetree", "0"),
            "sites[0].properties.land_cover.evetr.fai_evergreen_tree",
            "#2001",
        ),
        _scalar_entry(
            ("evetreeh", "0"),
            "sites[0].properties.land_cover.evetr.height_evergreen_tree",
            "#2001",
        ),
        _scalar_entry(
            ("faidectree", "0"),
            "sites[0].properties.land_cover.dectr.fai_deciduous_tree",
            "#2001",
        ),
        _scalar_entry(
            ("dectreeh", "0"),
            "sites[0].properties.land_cover.dectr.height_deciduous_tree",
            "#2001",
        ),
        _scalar_entry(
            ("pormin_dec", "0"),
            "sites[0].properties.land_cover.dectr.porosity_min_deciduous",
        ),
        _scalar_entry(
            ("pormax_dec", "0"),
            "sites[0].properties.land_cover.dectr.porosity_max_deciduous",
        ),
        _scalar_entry(
            ("capmax_dec", "0"),
            "sites[0].properties.land_cover.dectr.capacity_max_deciduous",
        ),
        _scalar_entry(
            ("capmin_dec", "0"),
            "sites[0].properties.land_cover.dectr.capacity_min_deciduous",
        ),
    ]
    for veg_idx, surf_name in enumerate(VEG_SURFACES):
        base = f"sites[0].properties.land_cover.{surf_name}"
        entries.append(_scalar_entry((f"albmin_{surf_name}", "0"), f"{base}.alb_min"))
        entries.append(_scalar_entry((f"albmax_{surf_name}", "0"), f"{base}.alb_max"))
        for col, field, owner in (
            ("beta_bioco2", "beta_bio_co2", "#2101"),
            ("beta_enh_bioco2", "beta_enh_bioco2", "#2101"),
            ("alpha_bioco2", "alpha_bio_co2", "#2101"),
            ("alpha_enh_bioco2", "alpha_enh_bioco2", "#2101"),
            ("resp_a", "resp_a", "#2101"),
            ("resp_b", "resp_b", "#2101"),
            ("theta_bioco2", "theta_bio_co2", "#2101"),
            ("maxconductance", "max_conductance", "#2309"),
            ("min_res_bioco2", "min_res_bioco2", "#2101"),
            ("ie_a", "ie_a", "#2309"),
            ("ie_m", "ie_m", "#2309"),
        ):
            entries.append(
                _scalar_entry((col, f"({veg_idx},)"), f"{base}.{field}", owner)
            )
        for col, field, owner in (
            ("baset", "base_temperature", "#2001"),
            ("gddfull", "gdd_full", "#2001"),
            ("basete", "base_temperature_senescence", "#2001"),
            ("sddfull", "sdd_full", "#2001"),
            ("laimin", "lai_min", "#2309"),
            ("laimax", "lai_max", "#2101"),
            ("laitype", "lai_type", "#2309"),
        ):
            entries.append(
                _scalar_entry((col, f"({veg_idx},)"), f"{base}.lai.{field}", owner)
            )
        for p_idx, p_field in enumerate(LAI_POWER_FIELDS):
            entries.append(
                _scalar_entry(
                    ("laipower", f"({p_idx}, {veg_idx})"),
                    f"{base}.lai.lai_power.{p_field}",
                    "#2101",
                )
            )
    return entries


def _vertical_layers_and_initial_state_mappings(nlayer: int) -> list[dict[str, Any]]:
    """Map vertical layers, roof/wall facets, and initial state variables."""
    entries: list[dict[str, Any]] = [
        _scalar_entry(("nlayer", "0"), "sites[0].properties.vertical_layers.nlayer"),
        _vector_entry(
            "height",
            [f"({i},)" for i in range(nlayer + 1)],
            f"(0..{nlayer})",
            "sites[0].properties.vertical_layers.height",
        ),
    ]
    for arr_field in (
        "veg_frac",
        "veg_scale",
        "veg_ext",
        "building_frac",
        "building_scale",
    ):
        entries.append(
            _vector_entry(
                arr_field,
                [f"({i},)" for i in range(nlayer)],
                f"(0..{nlayer - 1})",
                f"sites[0].properties.vertical_layers.{arr_field}",
            )
        )
    for layer_idx in range(nlayer):
        for facet in ("roof", "wall"):
            base = f"sites[0].properties.vertical_layers.{facet}s[{layer_idx}]"
            entries.extend([
                _scalar_entry((f"alb_{facet}", f"({layer_idx},)"), f"{base}.alb"),
                _scalar_entry((f"emis_{facet}", f"({layer_idx},)"), f"{base}.emis"),
                _scalar_entry(
                    (f"statelimit_{facet}", f"({layer_idx},)"),
                    f"{base}.state_limit",
                ),
                _scalar_entry(
                    (f"soilstorecap_{facet}", f"({layer_idx},)"),
                    f"{base}.soil_store_capacity",
                    "#2309",
                ),
                _scalar_entry(
                    (f"wetthresh_{facet}", f"({layer_idx},)"),
                    f"{base}.wet_threshold",
                ),
            ])
            for df_var, field in (
                (f"dz_{facet}", "dz"),
                (f"k_{facet}", "k"),
                (f"cp_{facet}", "rho_cp"),
            ):
                entries.append(
                    _vector_entry(
                        df_var,
                        [f"({layer_idx}, {i})" for i in range(5)],
                        f"({layer_idx}, 0..4)",
                        f"{base}.thermal_layers.{field}",
                        "#2309",
                    )
                )

    entries.append(_scalar_entry(("snowalb", "0"), "sites[0].initial_states.snowalb"))
    for surf_idx, surf_name in enumerate(SURFACES):
        base = f"sites[0].initial_states.{surf_name}"
        entries.extend([
            _scalar_entry(("state_surf", f"({surf_idx},)"), f"{base}.state"),
            _scalar_entry(("soilstore_surf", f"({surf_idx},)"), f"{base}.soilstore"),
            _scalar_entry(("tsfc_surf", f"({surf_idx},)"), f"{base}.tsfc"),
            _scalar_entry(("tin_surf", f"({surf_idx},)"), f"{base}.tin"),
        ])
        for col in ("snowfrac", "snowpack", "icefrac", "snowwater", "snowdens"):
            entries.append(_scalar_entry((col, f"({surf_idx},)"), f"{base}.{col}"))
    for veg_idx, surf_name in enumerate(VEG_SURFACES):
        base = f"sites[0].initial_states.{surf_name}"
        entries.extend([
            _scalar_entry((f"alb{surf_name}_id", "0"), f"{base}.alb_id"),
            _scalar_entry(("lai_id", f"({veg_idx},)"), f"{base}.lai_id"),
            _scalar_entry(("gdd_id", f"({veg_idx},)"), f"{base}.gdd_id"),
            _scalar_entry(("sdd_id", f"({veg_idx},)"), f"{base}.sdd_id"),
        ])
    entries.append(
        _scalar_entry(("porosity_id", "0"), "sites[0].initial_states.dectr.porosity_id")
    )
    entries.append(
        _scalar_entry(("decidcap_id", "0"), "sites[0].initial_states.dectr.decidcap_id")
    )
    return entries


def _build_df_yaml_mapping(nlayer: int) -> list[dict[str, Any]]:
    """Return systematic mappings between ``to_df_state()`` columns and engine YAML paths."""
    entries = _model_and_site_scalar_mappings()
    entries.extend(_snow_mappings())
    for surf_idx, surf_name in enumerate(SURFACES):
        entries.extend(_single_surface_mappings(surf_idx, surf_name))
    entries.extend(_vegetation_and_morphology_mappings())
    entries.extend(_vertical_layers_and_initial_state_mappings(nlayer))
    return entries


def _values_close(a: Any, b: Any) -> bool:
    if a is None or b is None:
        return a is None and b is None
    if isinstance(a, list) and isinstance(b, list):
        return len(a) == len(b) and all(_values_close(x, y) for x, y in zip(a, b))
    if isinstance(a, (int, float)) and isinstance(b, (int, float)):
        return math.isclose(float(a), float(b), rel_tol=1e-9, abs_tol=1e-12)
    return a == b


def _collect_df_yaml_divergences(
    df: pd.DataFrame,
    engine_doc: dict[str, Any],
    nlayer: int,
) -> list[dict[str, Any]]:
    """Compare all mapped ``to_df_state()`` columns against engine YAML values.

    A divergence occurs when:
    - Engine YAML carries a value and ``df_state`` differs (or is missing), OR
    - Engine YAML omits the key (``None``, which the Rust bridge zero-initialises
      as ``0.0``) while ``to_df_state()`` supplies a non-zero shadow default.
    """
    divergences: list[dict[str, Any]] = []
    for item in _build_df_yaml_mapping(nlayer):
        if item["kind"] == "scalar":
            df_val = _get_df_val(df, tuple(item["df_col"]))
            yaml_val = _get_yaml_path(engine_doc, item["yaml_path"])
            if yaml_val is not None:
                diverges = not _values_close(df_val, yaml_val)
            else:
                diverges = df_val is not None and not _values_close(df_val, 0.0)
            if diverges:
                divergences.append({
                    "df_col": item["df_col"],
                    "yaml_path": item["yaml_path"],
                    "df_value": df_val,
                    "yaml_value": yaml_val,
                    "owner_issue": item["owner_issue"],
                })
        else:
            df_vals = [
                _get_df_val(df, (item["df_var"], idx)) for idx in item["df_indices"]
            ]
            yaml_vals = _get_yaml_path(engine_doc, item["yaml_path"])
            if yaml_vals is not None:
                diverges = not _values_close(df_vals, yaml_vals)
            else:
                diverges = any(
                    v is not None and not _values_close(v, 0.0) for v in df_vals
                )
            if diverges:
                divergences.append({
                    "df_col": item["df_col"],
                    "yaml_path": item["yaml_path"],
                    "df_value": df_vals,
                    "yaml_value": yaml_vals,
                    "owner_issue": item["owner_issue"],
                })
    return divergences


def _assert_matches_shrinking_ratchet(
    observed: list[dict[str, Any]],
    baseline_gaps: list[dict[str, Any]],
) -> None:
    """Enforce exact match between observed divergences and pinned baseline gaps.

    Fails on both newly introduced divergences (regression) and stale baseline
    entries whose shadow defaults have been fixed (shrinking ratchet).
    """
    obs_by_key = {(tuple(g["df_col"]), g["yaml_path"]): g for g in observed}
    base_by_key = {(tuple(g["df_col"]), g["yaml_path"]): g for g in baseline_gaps}

    new_keys = sorted(set(obs_by_key) - set(base_by_key))
    stale_keys = sorted(set(base_by_key) - set(obs_by_key))
    value_drift = []
    for key in sorted(set(obs_by_key) & set(base_by_key)):
        obs = obs_by_key[key]
        base = base_by_key[key]
        if not _values_close(obs["df_value"], base["df_value"]) or not _values_close(
            obs["yaml_value"], base["yaml_value"]
        ):
            value_drift.append((key, base, obs))

    problems = []
    if new_keys:
        details = ", ".join(
            f"{k[1]} (df={obs_by_key[k]['df_value']!r}, yaml={obs_by_key[k]['yaml_value']!r})"
            for k in new_keys[:5]
        )
        problems.append(
            f"{len(new_keys)} unpinned new df_state/YAML divergence(s): {details}"
        )
    if stale_keys:
        details = ", ".join(k[1] for k in stale_keys[:5])
        problems.append(
            f"{len(stale_keys)} stale resolved entry/entries still in baseline "
            f"(remove them to shrink the ratchet): {details}"
        )
    if value_drift:
        details = ", ".join(
            f"{k[1]} (baseline df={b['df_value']!r}/yaml={b['yaml_value']!r}, "
            f"observed df={o['df_value']!r}/yaml={o['yaml_value']!r})"
            for k, b, o in value_drift[:5]
        )
        problems.append(f"{len(value_drift)} value drift(s): {details}")

    assert not problems, (
        "df_state vs config_to_engine_yaml parity ratchet failed:\n- "
        + "\n- ".join(problems)
    )


def _build_sparse_executable_dict(sample_dict: dict[str, Any]) -> dict[str, Any]:
    """Build a valid executable sparse config that exposes all remaining shadow defaults.

    Sets ``paved.sfr = 1.0`` and all other six surface fractions to ``0.0``
    (with ``nlayer = 1``), keeping ``paved`` soil parameters populated so the
    config remains valid both before and after soil-parameter requirements land
    (#2077), while stripping all omitted optional fields whose ``to_df_state()``
    still injects non-zero shadow defaults.
    """
    sparse = copy.deepcopy(sample_dict)
    props = sparse["sites"][0]["properties"]
    lc = props["land_cover"]
    for surf in SURFACES:
        lc[surf]["sfr"] = {"value": 1.0 if surf == "paved" else 0.0}
        lc[surf].pop("soil_density", None)
        lc[surf].pop("thermal_layers", None)
        if surf != "paved":
            for key in (
                "soil_depth",
                "soil_store_capacity",
                "saturated_hydraulic_conductivity",
                "faibldg",
                "bldgh",
                "fai_evergreen_tree",
                "height_evergreen_tree",
                "fai_deciduous_tree",
                "height_deciduous_tree",
                "beta_bio_co2",
                "alpha_bio_co2",
                "resp_a",
                "resp_b",
                "theta_bio_co2",
                "lai",
            ):
                lc[surf].pop(key, None)
    props.pop("conductance", None)
    for key in (
        "temperature_rain_snow_threshold",
        "snow_density_min",
        "snow_density_max",
    ):
        props["snow"].pop(key, None)
    vl = props["vertical_layers"]
    vl["nlayer"] = {"value": 1}
    vl["height"] = {"value": [0.0, 10.0]}
    vl["veg_frac"] = {"value": [0.0]}
    vl["veg_scale"] = {"value": [1.0]}
    vl["veg_ext"] = {"value": [0.5]}
    vl["building_frac"] = {"value": [0.0]}
    vl["building_scale"] = {"value": [1.0]}
    vl["roofs"] = [{}]
    vl["walls"] = [{}]
    init_st = sparse["sites"][0]["initial_states"]
    init_st["roofs"] = init_st["roofs"][:1]
    init_st["walls"] = init_st["walls"][:1]
    return sparse


def _run_phase_a_roundtrip(
    payload: dict[str, Any], tmp_path: Path, stem: str
) -> SUEWSConfig:
    """Run Phase A annotation on ``payload`` and reload the emitted YAML via ``from_yaml``."""
    user_yaml = _write_yaml(tmp_path / f"{stem}_in.yml", payload)
    uptodate_yaml = tmp_path / f"{stem}_updated.yml"
    report_txt = tmp_path / f"{stem}_report.txt"
    with as_file(trv_supy_module / "sample_data" / "sample_config.yml") as sample_path:
        annotate_missing_parameters(
            user_file=str(user_yaml),
            standard_file=str(sample_path),
            uptodate_file=str(uptodate_yaml),
            report_file=str(report_txt),
            forcing="off",
        )
    return SUEWSConfig.from_yaml(str(uptodate_yaml))


# ---------------------------------------------------------------------------
# 1. Five-way entry-point positive controls
# ---------------------------------------------------------------------------


class TestFiveWayEntryPointPositiveParity:
    """All five public entry points resolve valid configurations equivalently."""

    @pytest.mark.medium
    @pytest.mark.parametrize("variant", ["sample", "custom_non_default"])
    def test_five_entrypoints_produce_equivalent_engine_yaml_and_df_state(
        self, sample_dict, current_json_schema, tmp_path, variant
    ):
        """All 5 entry points yield identical engine YAML, grid JSON, and df_state."""
        # ARRANGE
        payload = copy.deepcopy(sample_dict)
        if variant == "custom_non_default":
            payload["model"]["control"]["tstep"] = {"value": 600}
            payload["model"]["physics"]["emissions"] = {"value": 12}
            payload["sites"][0]["properties"]["anthropogenic_emissions"]["co2"][
                "emission_co2_point_source"
            ] = {"value": 2.5}
            payload["sites"][0]["properties"]["land_cover"]["paved"]["emis"] = {
                "value": 0.88
            }
            payload["sites"][0]["properties"]["land_cover"]["bldgs"]["bldgh"] = {
                "value": 18.5
            }
        yaml_path = _write_yaml(tmp_path / f"{variant}.yml", payload)

        # ACT: load via all 5 public entry points
        cfg_yaml = SUEWSConfig.from_yaml(str(yaml_path))
        cfg_dict = SUEWSConfig.from_dict(copy.deepcopy(payload))
        cfg_prog = SUEWSConfig(**copy.deepcopy(payload))
        sim_update = SUEWSSimulation().update_config(
            copy.deepcopy(payload), auto_load_forcing=False
        )
        cfg_update = sim_update.config
        is_valid, errors = validate_single_file(yaml_path, current_json_schema)
        cfg_phase_a = _run_phase_a_roundtrip(payload, tmp_path, f"pos_{variant}")

        # ASSERT
        assert is_valid, f"validate_single_file rejected valid {variant}: {errors}"
        ref_engine = _normalise_engine_dict(config_to_engine_yaml(cfg_yaml))
        ref_ids, ref_jsons = _grid_config_jsons(cfg_yaml)
        ref_df = cfg_yaml.to_df_state()

        for label, cfg, precomputed_df in (
            ("from_dict", cfg_dict, None),
            ("programmatic", cfg_prog, None),
            ("update_config", cfg_update, sim_update.state_init),
            ("phase_a_roundtrip", cfg_phase_a, None),
        ):
            assert _normalise_engine_dict(config_to_engine_yaml(cfg)) == ref_engine, (
                f"{label} engine YAML diverged from from_yaml on {variant}"
            )
            ids, jsons = _grid_config_jsons(cfg)
            assert ids == ref_ids
            assert [_strip_loader_bookkeeping(json.loads(j)) for j in jsons] == [
                _strip_loader_bookkeeping(json.loads(j)) for j in ref_jsons
            ]
            cfg_df = precomputed_df if precomputed_df is not None else cfg.to_df_state()
            pd.testing.assert_frame_equal(cfg_df, ref_df)

    def test_partial_update_config_patch_matches_full_dict_resolution(
        self, compact_valid_dict
    ):
        """Partial ``update_config`` dict patch resolves identically to a full dict."""
        # ARRANGE
        full_payload = copy.deepcopy(compact_valid_dict)
        full_payload["model"]["control"]["tstep"] = {"value": 600}
        full_payload["model"]["physics"]["emissions"] = {"value": 12}
        full_payload["sites"][0]["properties"]["anthropogenic_emissions"]["co2"][
            "emission_co2_point_source"
        ] = {"value": 2.5}
        full_payload["sites"][0]["properties"]["land_cover"]["paved"]["emis"] = {
            "value": 0.88
        }
        full_payload["sites"][0]["properties"]["land_cover"]["bldgs"]["bldgh"] = {
            "value": 18.5
        }
        cfg_full = SUEWSConfig.from_dict(full_payload)
        ref_engine = _normalise_engine_dict(config_to_engine_yaml(cfg_full))
        ref_df = cfg_full.to_df_state()

        # ACT
        sim_patched = SUEWSSimulation().update_config(
            copy.deepcopy(compact_valid_dict), auto_load_forcing=False
        )
        sim_patched.update_config(
            {
                "model": {
                    "control": {"tstep": {"value": 600}},
                    "physics": {"emissions": {"value": 12}},
                },
                "sites": {
                    0: {
                        "properties": {
                            "anthropogenic_emissions": {
                                "co2": {"emission_co2_point_source": {"value": 2.5}}
                            },
                            "land_cover": {
                                "paved": {"emis": {"value": 0.88}},
                                "bldgs": {"bldgh": {"value": 18.5}},
                            },
                        }
                    }
                },
            },
            auto_load_forcing=False,
        )

        # ASSERT
        assert (
            _normalise_engine_dict(config_to_engine_yaml(sim_patched.config))
            == ref_engine
        )
        pd.testing.assert_frame_equal(sim_patched.state_init, ref_df)

    def test_five_entrypoints_agree_on_omitted_defaulted_fields(
        self, sample_dict, current_json_schema, tmp_path
    ):
        """Omitting defaulted non-optional fields resolves identically across all 5 paths."""
        # ARRANGE: omit defaulted non-optional fields plus soil_field_capacity
        # (lawfully optional under modelled soil moisture soil_moisture=0)
        sparse = copy.deepcopy(sample_dict)
        paved = sparse["sites"][0]["properties"]["land_cover"]["paved"]
        del paved["emis"]
        del paved["alb"]
        paved.pop("soil_field_capacity", None)
        del sparse["sites"][0]["properties"]["lumps"]
        yaml_path = _write_yaml(tmp_path / "sparse_defaults.yml", sparse)

        # ACT
        cfg_yaml = SUEWSConfig.from_yaml(str(yaml_path))
        cfg_dict = SUEWSConfig.from_dict(copy.deepcopy(sparse))
        cfg_prog = SUEWSConfig(**copy.deepcopy(sparse))
        cfg_update = (
            SUEWSSimulation()
            .update_config(copy.deepcopy(sparse), auto_load_forcing=False)
            .config
        )
        is_valid, errors = validate_single_file(yaml_path, current_json_schema)
        cfg_phase_a = _run_phase_a_roundtrip(sparse, tmp_path, "sparse_defaults")

        # ASSERT
        assert is_valid, f"validate_single_file rejected sparse defaults: {errors}"
        ref_engine = _normalise_engine_dict(config_to_engine_yaml(cfg_yaml))
        assert _get_yaml_path(
            ref_engine, "sites[0].properties.land_cover.paved.emis"
        ) == pytest.approx(0.95)
        assert _get_yaml_path(
            ref_engine, "sites[0].properties.land_cover.paved.alb"
        ) == pytest.approx(0.1)
        assert (
            _get_yaml_path(
                ref_engine, "sites[0].properties.land_cover.paved.soil_field_capacity"
            )
            is None
        )
        assert _get_yaml_path(
            ref_engine, "sites[0].properties.lumps.raincover"
        ) == pytest.approx(0.25)

        for label, cfg in (
            ("from_dict", cfg_dict),
            ("programmatic", cfg_prog),
            ("update_config", cfg_update),
            ("phase_a_roundtrip", cfg_phase_a),
        ):
            assert _normalise_engine_dict(config_to_engine_yaml(cfg)) == ref_engine, (
                f"{label} diverged on omitted defaulted fields"
            )


# ---------------------------------------------------------------------------
# 2. Five-way entry-point negative controls and programmatic bypass closure
# ---------------------------------------------------------------------------


def _mutate_missing_bldgh(data: dict[str, Any]) -> None:
    data["sites"][0]["properties"]["land_cover"]["bldgs"].pop("bldgh", None)


def _mutate_missing_paved_soil_depth(data: dict[str, Any]) -> None:
    data["sites"][0]["properties"]["land_cover"]["paved"].pop("soil_depth", None)


def _mutate_missing_paved_soil_store_capacity(data: dict[str, Any]) -> None:
    data["sites"][0]["properties"]["land_cover"]["paved"].pop(
        "soil_store_capacity", None
    )


def _mutate_missing_paved_saturated_hydraulic_conductivity(
    data: dict[str, Any],
) -> None:
    data["sites"][0]["properties"]["land_cover"]["paved"].pop(
        "saturated_hydraulic_conductivity", None
    )


def _mutate_missing_lai_max(data: dict[str, Any]) -> None:
    data["sites"][0]["properties"]["land_cover"]["grass"]["lai"].pop("lai_max", None)


def _mutate_missing_conductance_g_max(data: dict[str, Any]) -> None:
    data["sites"][0]["properties"]["conductance"].pop("g_max", None)


def _mutate_missing_co2_point_source(data: dict[str, Any]) -> None:
    data["model"]["physics"]["emissions"] = {"value": 12}
    data["sites"][0]["properties"]["anthropogenic_emissions"]["co2"].pop(
        "emission_co2_point_source", None
    )


def _mutate_null_non_optional_emis(data: dict[str, Any]) -> None:
    data["sites"][0]["properties"]["land_cover"]["paved"]["emis"] = {"value": None}


def _mutate_null_active_min_res_bioco2(data: dict[str, Any]) -> None:
    data["model"]["physics"]["emissions"] = {"value": 22}
    data["sites"][0]["properties"]["land_cover"]["grass"]["min_res_bioco2"] = {
        "value": None
    }


def _mutate_negative_sfr(data: dict[str, Any]) -> None:
    data["sites"][0]["properties"]["land_cover"]["paved"]["sfr"] = {"value": -0.1}


def _mutate_sentinel_sfr(data: dict[str, Any]) -> None:
    data["sites"][0]["properties"]["land_cover"]["paved"]["sfr"] = {"value": -999.0}


def _mutate_nan_sfr(data: dict[str, Any]) -> None:
    data["sites"][0]["properties"]["land_cover"]["paved"]["sfr"] = {
        "value": float("nan")
    }


def _mutate_invalid_output_freq(data: dict[str, Any]) -> None:
    data["model"]["control"]["tstep"] = {"value": 300}
    data["model"]["control"]["output"] = {"freq": 100}


def _mutate_stebbs_without_spartacus(data: dict[str, Any]) -> None:
    data["model"]["physics"]["storage_heat"] = {"value": 7}


_NEGATIVE_CASES = [
    ("missing_bldgh", _mutate_missing_bldgh, "bldgh"),
    ("missing_paved_soil_depth", _mutate_missing_paved_soil_depth, "soil_depth"),
    (
        "missing_paved_soil_store_capacity",
        _mutate_missing_paved_soil_store_capacity,
        "soil_store_capacity",
    ),
    (
        "missing_paved_saturated_hydraulic_conductivity",
        _mutate_missing_paved_saturated_hydraulic_conductivity,
        "saturated_hydraulic_conductivity",
    ),
    ("missing_lai_max", _mutate_missing_lai_max, "lai_max"),
    ("missing_conductance_g_max", _mutate_missing_conductance_g_max, "g_max"),
    (
        "missing_co2_point_source",
        _mutate_missing_co2_point_source,
        "emission_co2_point_source",
    ),
    ("null_non_optional_emis", _mutate_null_non_optional_emis, "emis"),
    (
        "null_active_min_res_bioco2",
        _mutate_null_active_min_res_bioco2,
        "min_res_bioco2",
    ),
    ("negative_sfr", _mutate_negative_sfr, "sfr"),
    ("sentinel_minus_999_sfr", _mutate_sentinel_sfr, "sfr"),
    ("nan_sfr", _mutate_nan_sfr, "sfr"),
    ("invalid_output_freq", _mutate_invalid_output_freq, "frequency"),
    (
        "stebbs_without_spartacus",
        _mutate_stebbs_without_spartacus,
        "SPARTACUS",
    ),
]


class TestFiveWayEntryPointNegativeParity:
    """All five public entry points reject invalid configurations equivalently."""

    @pytest.mark.parametrize(
        ("case_name", "mutator", "expected_token"), _NEGATIVE_CASES
    )
    def test_five_entrypoints_reject_invalid_configs_equivalently(
        self,
        compact_valid_dict,
        current_json_schema,
        tmp_path,
        case_name,
        mutator,
        expected_token,
    ):
        """Every single-field fault is rejected across all 5 public entry points."""
        # ARRANGE
        bad = copy.deepcopy(compact_valid_dict)
        mutator(bad)
        yaml_path = _write_yaml(tmp_path / f"bad_{case_name}.yml", bad)

        # 1. SUEWSConfig.from_yaml
        with pytest.raises(ValueError) as exc_yaml:
            SUEWSConfig.from_yaml(str(yaml_path))
        assert expected_token.lower() in str(exc_yaml.value).lower()

        # 2. SUEWSConfig.from_dict
        with pytest.raises(ValueError) as exc_dict:
            SUEWSConfig.from_dict(copy.deepcopy(bad))
        assert expected_token.lower() in str(exc_dict.value).lower()

        # 3. Programmatic SUEWSConfig(**kwargs) at the execution handoff
        try:
            prog_cfg = SUEWSConfig(**copy.deepcopy(bad))
        except ValueError as exc_prog_init:
            assert expected_token.lower() in str(exc_prog_init).lower()
        else:
            # Programmatic construction succeeded (e.g. _yaml_raw-deferred check);
            # every execution/export/state boundary MUST reject it before lossy
            # conversion or Rust execution.
            with pytest.raises(ValueError) as exc_eng:
                config_to_engine_yaml(prog_cfg)
            assert expected_token.lower() in str(exc_eng.value).lower()
            with pytest.raises(ValueError):
                _grid_config_jsons(prog_cfg)
            with pytest.raises(ValueError):
                SUEWSSimulation().update_config(prog_cfg, auto_load_forcing=False)
            with pytest.raises(ValueError):
                prog_cfg.to_df_state()

        # 4. SUEWSSimulation.update_config(dict)
        with pytest.raises(ValueError) as exc_upd:
            SUEWSSimulation().update_config(copy.deepcopy(bad), auto_load_forcing=False)
        assert expected_token.lower() in str(exc_upd.value).lower()

        # 5. suews validate (validate_single_file)
        is_valid, errors = validate_single_file(yaml_path, current_json_schema)
        assert not is_valid, f"validate_single_file accepted invalid case {case_name}"
        assert errors

    def test_programmatic_bypass_rejected_atomically_on_existing_simulation(
        self, compact_valid_dict
    ):
        """Handing an incomplete programmatic config to update_config leaves sim intact."""
        # ARRANGE
        sim = SUEWSSimulation().update_config(
            copy.deepcopy(compact_valid_dict), auto_load_forcing=False
        )
        original_cfg = sim.config
        incomplete = copy.deepcopy(compact_valid_dict)
        _mutate_missing_bldgh(incomplete)
        prog_cfg = SUEWSConfig(**incomplete)

        # ACT / ASSERT: programmatic object built, but rejected atomically at handoff
        assert prog_cfg.sites[0].properties.land_cover.bldgs.bldgh is None
        with pytest.raises(ValueError, match="bldgh"):
            sim.update_config(prog_cfg, auto_load_forcing=False)
        assert sim.config is original_cfg

    def test_model_construct_and_unchecked_from_dict_rejected_before_execution(
        self, compact_valid_dict
    ):
        """Unvalidated model_construct() and unchecked from_dict() cannot reach the engine."""
        # ARRANGE 1: unchecked from_dict leaves raw dict sub-models
        unchecked = SUEWSConfig.from_dict(
            copy.deepcopy(compact_valid_dict), use_conditional_validation=False
        )
        # ARRANGE 2: model_construct with out-of-range sfr and STEBBS without SPARTACUS
        valid = SUEWSConfig.from_dict(copy.deepcopy(compact_valid_dict))
        bad_sfr = copy.deepcopy(valid)
        bad_sfr.sites[0].properties.land_cover.paved.sfr = -0.25
        constructed_sfr = SUEWSConfig.model_construct(
            model=bad_sfr.model, sites=bad_sfr.sites
        )
        bad_stebbs_phys = ModelPhysics.model_construct(**{
            k: getattr(valid.model.physics, k)
            for k in type(valid.model.physics).model_fields
        })
        object.__setattr__(bad_stebbs_phys, "storage_heat", 7)
        constructed_stebbs = SUEWSConfig.model_construct(
            model=Model.model_construct(
                control=valid.model.control, physics=bad_stebbs_phys
            ),
            sites=copy.deepcopy(valid.sites),
        )

        # ACT / ASSERT
        for candidate, pattern in (
            (unchecked, "validated Model"),
            (constructed_sfr, "sfr"),
            (constructed_stebbs, "SPARTACUS"),
        ):
            with pytest.raises(ValueError, match=pattern):
                config_to_engine_yaml(candidate)
            with pytest.raises(ValueError, match=pattern):
                _grid_config_jsons(candidate)
            with pytest.raises(ValueError, match=pattern):
                SUEWSSimulation().update_config(candidate, auto_load_forcing=False)
            with pytest.raises(ValueError, match=pattern):
                candidate.to_df_state()

    @pytest.mark.parametrize(
        ("label", "mutate", "expected"),
        [
            pytest.param(
                "bldgh_none",
                lambda cfg: setattr(
                    cfg.sites[0].properties.land_cover.bldgs, "bldgh", None
                ),
                "bldgh",
                id="bldgh_none",
            ),
            pytest.param(
                "lai_max_none",
                lambda cfg: setattr(
                    cfg.sites[0].properties.land_cover.grass.lai, "lai_max", None
                ),
                "lai_max",
                id="lai_max_none",
            ),
            pytest.param(
                "emissions_22_missing_co2_point",
                lambda cfg: (
                    setattr(cfg.model.physics, "emissions", 22),
                    setattr(
                        cfg.sites[0].properties.anthropogenic_emissions.co2,
                        "emission_co2_point_source",
                        None,
                    ),
                ),
                "emission_co2_point_source",
                id="emissions_22_missing_co2_point",
            ),
            pytest.param(
                "output_freq_mismatch",
                lambda cfg: setattr(cfg.model.control.output, "freq", 100),
                "frequency",
                id="output_freq_mismatch",
            ),
            pytest.param(
                "paved_sfr_negative",
                lambda cfg: setattr(
                    cfg.sites[0].properties.land_cover.paved, "sfr", -0.1
                ),
                "sfr",
                id="paved_sfr_negative",
            ),
            pytest.param(
                "paved_soil_depth_none",
                lambda cfg: setattr(
                    cfg.sites[0].properties.land_cover.paved, "soil_depth", None
                ),
                "soil_depth",
                id="paved_soil_depth_none",
            ),
        ],
    )
    def test_post_construction_in_place_mutations_rejected_before_run(
        self, compact_valid_dict, short_sample_forcing, label, mutate, expected
    ):
        """In-place mutations on a valid SUEWSSimulation fail across all 5 engine boundaries."""
        assert label
        df_short = short_sample_forcing(SHORT_RUN_STEPS)
        sim = SUEWSSimulation().update_config(
            SUEWSConfig.from_dict(copy.deepcopy(compact_valid_dict)),
            auto_load_forcing=False,
        )
        sim.update_forcing(df_short)
        mutate(sim.config)

        with pytest.raises(ValueError, match=expected):
            config_to_engine_yaml(sim.config)
        with pytest.raises(ValueError, match=expected):
            _grid_config_jsons(sim.config)
        with pytest.raises(ValueError, match=expected):
            sim.config.to_df_state()
        with pytest.raises(ValueError, match=expected):
            SUEWSSimulation().update_config(sim.config, auto_load_forcing=False)
        with pytest.raises(ValueError, match=expected):
            sim.run(end_date=df_short.index[-1])

    def test_from_dict_without_land_cover_blocked_before_engine_execution(
        self, compact_valid_dict, short_sample_forcing, tmp_path
    ):
        """``from_dict(sample minus land_cover)`` cannot slip into the Rust bridge.

        With ``roughness_sublayer=2`` (sample default), ``_validate_rsl``
        rejects the missing ``land_cover`` at parse time. With
        ``roughness_sublayer=0``, omitting ``land_cover`` bypasses
        ``_yaml_raw``-gated checks at parse time (so sparse metadata fixtures
        still load), while Pydantic default factories materialise all 7
        surfaces with ``sfr = 1/7`` and ``None`` required fields. Execution
        boundaries must reject that sparse config before Rust runs.
        """
        # ARRANGE 1: default roughness_sublayer=2 is already rejected at from_dict
        df_short = short_sample_forcing(SHORT_RUN_STEPS)
        no_lc_rsl2 = copy.deepcopy(compact_valid_dict)
        del no_lc_rsl2["sites"][0]["properties"]["land_cover"]
        with pytest.raises(ValueError, match=r"faibldg|bldgh"):
            SUEWSConfig.from_dict(copy.deepcopy(no_lc_rsl2))

        # ARRANGE 2: roughness_sublayer=0 with default initial_states bypasses
        # _yaml_raw site checks at parse time...
        no_lc = copy.deepcopy(no_lc_rsl2)
        no_lc["model"]["physics"]["roughness_sublayer"] = {"value": 0}
        no_lc["sites"][0].pop("initial_states", None)
        yaml_path = _write_yaml(tmp_path / "no_land_cover.yml", no_lc)

        sparse_cfg = SUEWSConfig.from_dict(copy.deepcopy(no_lc))
        # ...but every engine execution/export handoff rejects it immediately.
        with pytest.raises(ValueError, match="bldgh"):
            config_to_engine_yaml(sparse_cfg)
        with pytest.raises(ValueError, match="bldgh"):
            _grid_config_jsons(sparse_cfg)
        with pytest.raises(ValueError, match="bldgh"):
            SUEWSSimulation().update_config(sparse_cfg, auto_load_forcing=False)

        # Even if loaded via sparse YAML path for incremental patching, run() blocks it.
        sim = SUEWSSimulation().update_config(str(yaml_path), auto_load_forcing=False)
        sim.update_forcing(df_short)
        with pytest.raises(ValueError, match="bldgh"):
            sim.run(end_date=df_short.index[-1])


# ---------------------------------------------------------------------------
# 3. Public consumer end-to-end verification (shipped CLI + SUEWSSimulation)
# ---------------------------------------------------------------------------


class TestPublicConsumerVerification:
    """Exercise shipped ``suews validate`` CLI and ``SUEWSSimulation`` end-to-end."""

    def test_shipped_cli_phase_a_artefact_runs_in_suews_simulation(self, tmp_path):
        """Shipped ``suews validate -p A`` emits a YAML artefact that runs cleanly in SUEWSSimulation."""
        # ARRANGE: copy real packaged sample data (config + 24-row sibling forcing)
        pos_dir = tmp_path / "positive"
        pos_dir.mkdir()
        sample_yaml = _copy_sample_data(pos_dir)

        # ACT 1: invoke shipped top-level ``suews validate -p A`` CLI
        runner = CliRunner()
        phase_a_result = runner.invoke(
            suews_cli,
            [
                "validate",
                "-p",
                "A",
                "--forcing",
                "off",
                "--format",
                "json",
                str(sample_yaml),
            ],
        )
        assert phase_a_result.exit_code == 0, phase_a_result.output
        phase_a_envelope = json.loads(phase_a_result.stdout)
        assert phase_a_envelope["status"] in {"success", "warning"}
        updated_yaml = Path(phase_a_envelope["data"]["updated_yaml"])
        assert updated_yaml.is_file()

        # ACT 2: load validated artefact into SUEWSSimulation and run short simulation
        sim_base = SUEWSSimulation(str(updated_yaml))
        df_forcing_short = sim_base.forcing.df.iloc[:SHORT_RUN_STEPS]
        sim_base.update_forcing(df_forcing_short)
        df_base = sim_base.run(end_date=df_forcing_short.index[-1]).df

        # ASSERT positive consumer outputs are finite and respond to a resolved parameter edit
        for var in ("QN", "QH", "QE", "QS"):
            series = df_base[("SUEWS", var)]
            assert np.isfinite(series.to_numpy()).all(), f"Non-finite {var} in output"

        sim_perturbed = SUEWSSimulation().update_config(
            str(updated_yaml), auto_load_forcing=False
        )
        sim_perturbed.update_config(
            {"sites": {0: {"properties": {"land_cover": {"paved": {"emis": 0.85}}}}}},
            auto_load_forcing=False,
        )
        sim_perturbed.update_forcing(df_forcing_short)
        df_pert = sim_perturbed.run(end_date=df_forcing_short.index[-1]).df
        max_dqn = float(
            (df_pert[("SUEWS", "QN")] - df_base[("SUEWS", "QN")]).abs().max()
        )
        assert max_dqn > 0.1, (
            f"Expected paved.emis change 0.95 -> 0.85 to shift QN, got max |dQN|={max_dqn}"
        )

    def test_shipped_cli_validate_accepts_positive_and_rejects_negative_control(
        self, tmp_path
    ):
        """Shipped ``suews validate -p C`` accepts valid sample and rejects single fault."""
        # ARRANGE
        pos_dir = tmp_path / "positive"
        pos_dir.mkdir()
        pos_yaml = _copy_sample_data(pos_dir)

        neg_dir = tmp_path / "negative"
        neg_dir.mkdir()
        neg_yaml = _copy_sample_data(neg_dir)
        neg_data = yaml.safe_load(neg_yaml.read_text(encoding="utf-8"))
        neg_data["sites"][0]["properties"]["land_cover"]["bldgs"]["bldgh"] = {
            "value": None
        }
        _write_yaml(neg_yaml, neg_data)

        # ACT: invoke shipped top-level ``suews validate -p C --format json``
        runner = CliRunner()
        pos_result = runner.invoke(
            suews_cli,
            ["validate", "-p", "C", "--format", "json", str(pos_yaml)],
        )
        neg_result = runner.invoke(
            suews_cli,
            ["validate", "-p", "C", "--format", "json", str(neg_yaml)],
        )

        # ASSERT: positive passes with JSON envelope, negative fails naming bldgh
        assert pos_result.exit_code == 0, pos_result.output
        pos_envelope = json.loads(pos_result.stdout)
        assert pos_envelope["status"] in {"success", "warning"}
        assert Path(pos_envelope["data"]["updated_yaml"]).is_file()

        assert neg_result.exit_code == 1, neg_result.output
        neg_envelope = json.loads(neg_result.stdout)
        assert neg_envelope["status"] == "error"
        neg_report_path = Path(neg_envelope["data"]["report_file"])
        assert neg_report_path.is_file()
        assert (
            "bldgh"
            in (
                json.dumps(neg_envelope["errors"])
                + neg_report_path.read_text(encoding="utf-8")
            ).lower()
        )

        # Public SUEWSSimulation consumer rejects the same faulty file
        with pytest.raises(ValueError, match="bldgh"):
            SUEWSSimulation().update_config(str(neg_yaml), auto_load_forcing=False)


# ---------------------------------------------------------------------------
# 4. Shrinking to_df_state() vs config_to_engine_yaml() shadow-default ratchet
# ---------------------------------------------------------------------------


class TestDfStateEngineYamlParityRatchet:
    """Pin and ratchet down ``to_df_state()`` vs ``config_to_engine_yaml()`` divergences."""

    def test_full_sample_config_df_state_matches_engine_yaml_except_soil_density(
        self, sample_dict
    ):
        """Every populated sample field agrees between ``to_df_state`` and engine YAML."""
        # ARRANGE
        cfg = SUEWSConfig.from_dict(copy.deepcopy(sample_dict))
        nlayer = int(
            _unwrap(sample_dict["sites"][0]["properties"]["vertical_layers"]["nlayer"])
        )

        # ACT
        df = cfg.to_df_state()
        engine = _normalise_engine_dict(config_to_engine_yaml(cfg))
        divergences = _collect_df_yaml_divergences(df, engine, nlayer)

        # ASSERT: only the 7 unpopulated soil_density (-999.0 vs None) gaps diverge
        expected_paths = {
            f"sites[0].properties.land_cover.{surf}.soil_density" for surf in SURFACES
        }
        assert {d["yaml_path"] for d in divergences} == expected_paths
        for entry in divergences:
            assert entry["df_value"] == pytest.approx(-999.0)
            assert entry["yaml_value"] is None

    def test_sparse_config_df_state_vs_engine_yaml_shrinking_ratchet(self, sample_dict):
        """All remaining shadow defaults on sparse configs match the pinned ratchet baseline."""
        # ARRANGE
        baseline = json.loads(GAPS_BASELINE_PATH.read_text(encoding="utf-8"))
        assert baseline["schema_version"] == 2
        gaps = baseline["gaps"]
        accounting = baseline["native_receiver_accounting"]
        assert len(gaps) > 0
        assert len(accounting) == 35

        valid_gap_statuses = {
            "native_zero_fallback",
            "native_required_unconditionally",
            "native_required_when_active_zero_when_inactive",
            "ehc_empty_when_nlayer0_zero_when_allocated",
            "unread_by_native",
        }
        valid_accounting_statuses = valid_gap_statuses | {
            "retired_required_when_active_zero_when_inactive",
        }
        repo_root = GAPS_BASELINE_PATH.parents[2]
        for root_name, meta in accounting.items():
            assert set(meta.keys()) == {
                "native_receiver",
                "native_member",
                "observed_by",
            }, f"Unexpected accounting keys for {root_name}: {meta.keys()}"
            assert meta["native_receiver"] in valid_accounting_statuses
            assert meta["native_member"]
            assert meta["observed_by"]
            for rel_path in meta["observed_by"]:
                assert (repo_root / rel_path).is_file(), (
                    f"Missing observer test {rel_path} for {root_name}"
                )

        seen_keys: set[tuple[tuple[str, ...], str]] = set()
        for gap in gaps:
            assert set(gap.keys()) == {
                "df_col",
                "yaml_path",
                "df_value",
                "yaml_value",
                "owner_issue",
                "native_receiver",
            }
            assert gap["owner_issue"].startswith("#")
            assert gap["owner_issue"][1:].isdigit()
            assert gap["native_receiver"] in valid_gap_statuses
            root_name = gap["df_col"][0]
            assert root_name in accounting, (
                f"Unaccounted df_col root in parity gaps: {root_name}"
            )
            assert accounting[root_name]["native_receiver"] == gap["native_receiver"], (
                f"Mismatched native_receiver classification for {root_name}"
            )
            key = (tuple(gap["df_col"]), gap["yaml_path"])
            assert key not in seen_keys, f"Duplicate baseline key: {key}"
            seen_keys.add(key)

        # Verify all three distinct native receiver semantics are represented explicitly
        # (never treating every absent YAML key as an observed 0.0 native fallback).
        assert {g["native_receiver"] for g in gaps} == valid_gap_statuses

        sparse = _build_sparse_executable_dict(sample_dict)
        cfg = SUEWSConfig.from_dict(sparse)

        # ACT
        df = cfg.to_df_state()
        engine = _normalise_engine_dict(config_to_engine_yaml(cfg))
        observed = _collect_df_yaml_divergences(df, engine, nlayer=1)

        # ASSERT: exact bidirectional shrinking ratchet
        _assert_matches_shrinking_ratchet(observed, gaps)

    @staticmethod
    def _build_asymmetric_sparse_native_dict(
        sample_dict: dict[str, Any],
    ) -> dict[str, Any]:
        """Build a sparse config with active paved/bldgs/evetr and omitted inactive veg."""
        base = _build_sparse_executable_dict(sample_dict)
        base["model"]["physics"]["emissions"] = {"value": 12}
        base["sites"][0]["properties"]["conductance"] = copy.deepcopy(
            sample_dict["sites"][0]["properties"]["conductance"]
        )
        sample_lc = sample_dict["sites"][0]["properties"]["land_cover"]
        lc = base["sites"][0]["properties"]["land_cover"]
        lc["paved"]["sfr"] = {"value": 0.45}
        lc["bldgs"]["sfr"] = {"value": 0.35}
        lc["bldgs"]["bldgh"] = {"value": 15.0}
        lc["bldgs"]["faibldg"] = {"value": 0.35}
        for soil_key in (
            "soil_depth",
            "soil_store_capacity",
            "saturated_hydraulic_conductivity",
        ):
            lc["bldgs"][soil_key] = copy.deepcopy(sample_lc["bldgs"][soil_key])
            lc["evetr"][soil_key] = copy.deepcopy(sample_lc["evetr"][soil_key])
        lc["evetr"]["sfr"] = {"value": 0.20}
        lc["evetr"]["height_evergreen_tree"] = {"value": 15.0}
        lc["evetr"]["fai_evergreen_tree"] = {"value": 0.25}
        lc["evetr"]["alpha_bio_co2"] = {"value": 0.8}
        lc["evetr"]["beta_bio_co2"] = {"value": 0.6}
        lc["evetr"]["theta_bio_co2"] = {"value": 1.2}
        lc["evetr"]["resp_a"] = {"value": 0.8}
        lc["evetr"]["resp_b"] = {"value": 1.1}
        lc["evetr"]["min_res_bioco2"] = {"value": 0.1}
        lc["evetr"]["lai"] = copy.deepcopy(sample_lc["evetr"]["lai"])
        lc["evetr"]["lai"].update({
            "base_temperature": {"value": 5.0},
            "base_temperature_senescence": {"value": 10.0},
            "gdd_full": {"value": 300.0},
            "sdd_full": {"value": -450.0},
            "lai_max": {"value": 5.0},
            "lai_power": {
                "growth_lai": {"value": 0.04},
                "growth_gdd": {"value": 0.04},
                "senescence_lai": {"value": 0.04},
                "senescence_sdd": {"value": 0.04},
            },
        })
        for inactive in ("dectr", "grass", "bsoil", "water"):
            lc[inactive]["sfr"] = {"value": 0.0}
        # Fortran `update_gddlai` reads LAI parameters unconditionally across all three
        # vegetated surfaces regardless of `sfr`, enforced by `apply_lai_overrides` (#2002).
        for inactive_veg in ("dectr", "grass"):
            lc[inactive_veg]["lai"] = copy.deepcopy(sample_lc[inactive_veg]["lai"])
        return base

    @pytest.mark.medium
    def test_sparse_asymmetric_config_reaches_native_receiver_end_to_end(
        self, compact_valid_dict, short_sample_forcing
    ):
        """Sparse asymmetric supplied values reach the compiled native kernel while unread fields do not."""
        df_short = short_sample_forcing(4)
        base = self._build_asymmetric_sparse_native_dict(compact_valid_dict)

        def _run_short(raw_cfg: dict[str, Any], forcing):
            cfg = SUEWSConfig.from_dict(raw_cfg)
            sim = SUEWSSimulation().update_config(cfg, auto_load_forcing=False)
            sim.update_forcing(forcing)
            return sim.run(end_date=forcing.index[-1]).xs("SUEWS", axis=1, level=0)

        # ACT 1: baseline vs unread soil_density toggle (must be bit-identical).
        with_density = copy.deepcopy(base)
        with_density["sites"][0]["properties"]["land_cover"]["paved"][
            "soil_density"
        ] = {"value": 1650.0}
        suews_base = _run_short(base, df_short)
        suews_density = _run_short(with_density, df_short)
        cols = ["QN", "QH", "QE", "QS", "U10", "FcRespi"]
        np.testing.assert_array_equal(
            suews_base[cols].to_numpy(dtype=float),
            suews_density[cols].to_numpy(dtype=float),
        )

        # ACT 2: shifting supplied evetr.resp_a (0.8 -> 4.0) reaches Fortran FcRespi.
        high_resp = copy.deepcopy(base)
        high_resp["sites"][0]["properties"]["land_cover"]["evetr"]["resp_a"] = {
            "value": 4.0
        }
        fc_base = suews_base["FcRespi"].to_numpy(dtype=float)
        fc_high = _run_short(high_resp, df_short)["FcRespi"].to_numpy(dtype=float)
        assert np.isfinite(fc_base).all() and np.isfinite(fc_high).all()
        assert np.max(np.abs(fc_high - fc_base)) > 1e-4, (
            "Expected supplied evetr.resp_a override to shift native FcRespi output"
        )

        # ACT 3: shifting supplied bldgs.bldgh (15.0 -> 30.0) reaches Fortran U10.
        tall_bldg = copy.deepcopy(base)
        tall_bldg["sites"][0]["properties"]["land_cover"]["bldgs"]["bldgh"] = {
            "value": 30.0
        }
        u10_base = suews_base["U10"].to_numpy(dtype=float)
        u10_tall = _run_short(tall_bldg, df_short)["U10"].to_numpy(dtype=float)
        assert np.isfinite(u10_base).all() and np.isfinite(u10_tall).all()
        assert np.max(np.abs(u10_tall - u10_base)) > 1e-4, (
            "Expected supplied bldgs.bldgh override to shift native U10 output"
        )

        # ACT 4: shifting supplied snow.temperature_rain_snow_threshold (0.0 -> 10.0)
        # across Tair = 5.0 C with an existing snowpack and rain reaches Fortran MeltHeat.
        df_rain_on_snow = df_short.copy()
        df_rain_on_snow["Tair"] = 5.0
        df_rain_on_snow["rain"] = 5.0
        snow_low = copy.deepcopy(base)
        snow_low["model"]["physics"]["snow"] = {"value": 1}
        snow_low["sites"][0]["initial_states"]["snowalb"] = {"value": 0.7}
        snow_low["sites"][0]["initial_states"]["paved"].update({
            "snowfrac": {"value": 1.0},
            "snowpack": {"value": 50.0},
            "snowdens": {"value": 200.0},
        })
        snow_low["sites"][0]["properties"]["snow"].update({
            "temperature_rain_snow_threshold": {"value": 0.0},
            "snow_density_min": {"value": 100.0},
            "snow_density_max": {"value": 400.0},
        })
        snow_high = copy.deepcopy(snow_low)
        snow_high["sites"][0]["properties"]["snow"][
            "temperature_rain_snow_threshold"
        ] = {"value": 10.0}
        snow_cols = ["QH", "QE", "QS", "SnowCh", "MeltWater", "RO"]
        s_low = _run_short(snow_low, df_rain_on_snow)[snow_cols].to_numpy(dtype=float)
        s_high = _run_short(snow_high, df_rain_on_snow)[snow_cols].to_numpy(dtype=float)
        assert np.isfinite(s_low).all() and np.isfinite(s_high).all()
        assert np.max(np.abs(s_high - s_low)) > 1e-3, (
            "Expected supplied temperature_rain_snow_threshold override to shift rain-on-snow output"
        )

    def test_parity_ratchet_fails_on_new_shadow_default_and_stale_entry(
        self, sample_dict
    ):
        """Proves a newly introduced shadow default OR a stale fixed entry fails the ratchet."""
        # ARRANGE
        baseline = json.loads(GAPS_BASELINE_PATH.read_text(encoding="utf-8"))["gaps"]
        sparse = _build_sparse_executable_dict(sample_dict)
        cfg = SUEWSConfig.from_dict(sparse)
        df = cfg.to_df_state()
        engine = _normalise_engine_dict(config_to_engine_yaml(cfg))

        # ACT / ASSERT 1: inject a brand-new shadow default in df_state (raincover)
        df_mutated = df.copy()
        df_mutated.iloc[0, df_mutated.columns.get_loc(("raincover", "0"))] = 0.77
        observed_new = _collect_df_yaml_divergences(df_mutated, engine, nlayer=1)
        with pytest.raises(AssertionError, match="unpinned new df_state/YAML"):
            _assert_matches_shrinking_ratchet(observed_new, baseline)

        # ACT / ASSERT 2: simulate an engine-YAML omission with a non-zero df_state default
        engine_omitted = copy.deepcopy(engine)
        del engine_omitted["sites"][0]["properties"]["lumps"]["drainrt"]
        observed_omitted = _collect_df_yaml_divergences(df, engine_omitted, nlayer=1)
        with pytest.raises(AssertionError, match=r"lumps\.drainrt"):
            _assert_matches_shrinking_ratchet(observed_omitted, baseline)

        # ACT / ASSERT 3: simulate fixing one gap while leaving a stale baseline entry
        stale_baseline = [
            *baseline,
            {
                "df_col": ["raincover", "0"],
                "yaml_path": "sites[0].properties.lumps.raincover",
                "df_value": 0.25,
                "yaml_value": None,
                "owner_issue": "#2309",
            },
        ]
        observed_clean = _collect_df_yaml_divergences(df, engine, nlayer=1)
        with pytest.raises(AssertionError, match="stale resolved entry"):
            _assert_matches_shrinking_ratchet(observed_clean, stale_baseline)

    def test_inactive_soil_missingness_survives_df_state_roundtrip_and_blocks_reactivation(
        self, compact_valid_dict, short_sample_forcing
    ):
        """Missing soil params stay ``None`` across ``to_df_state()`` -> ``from_df_state()`` and block reactivation.

        Verifies both halves of the prospective soil-missingness contract:
        1. Active non-water surfaces (``paved.sfr > 0``) missing ``soil_depth``,
           ``soil_store_capacity``, or ``saturated_hydraulic_conductivity`` are
           rejected by ``validate_for_engine()`` across all public boundaries
           (including ``to_df_state()`` before conversion).
        2. Moving ``paved`` area temporarily to ``grass`` (``paved.sfr == 0``),
           omitting ``paved``'s three soil parameters, saving via ``to_df_state()``,
           and reloading via ``from_df_state()`` (or legacy ``SUEWSSimulation.from_state()``)
           preserves ``None`` (via ``NaN`` in ``df_state``) rather than fabricating
           ``150.0 / 150.0 / 0.0001``. Restoring original surface areas then fails
           equivalently on both the in-memory original and the reloaded frame across
           all five public engine boundaries.
        """
        # ARRANGE 1: active paved missing all 3 required soil parameters
        df_short = short_sample_forcing(SHORT_RUN_STEPS)
        paved_none = copy.deepcopy(compact_valid_dict)
        paved_lc = paved_none["sites"][0]["properties"]["land_cover"]["paved"]
        paved_lc.pop("soil_depth", None)
        paved_lc.pop("soil_store_capacity", None)
        paved_lc.pop("saturated_hydraulic_conductivity", None)
        cfg_paved_none = SUEWSConfig(**paved_none)

        # ASSERT 1: blocked before to_df_state() conversion AND before Rust export
        for boundary in (
            cfg_paved_none.to_df_state,
            lambda: config_to_engine_yaml(cfg_paved_none),
            lambda: _grid_config_jsons(cfg_paved_none),
            lambda: SUEWSSimulation().update_config(
                cfg_paved_none, auto_load_forcing=False
            ),
        ):
            with pytest.raises(ValueError) as exc_active:
                boundary()
            err_msg = str(exc_active.value)
            assert "soil_depth" in err_msg
            assert "soil_store_capacity" in err_msg
            assert "saturated_hydraulic_conductivity" in err_msg

        # ARRANGE 2: move paved area temporarily to grass (paved.sfr == 0) and omit paved soil params
        inactive_paved = copy.deepcopy(compact_valid_dict)
        lc = inactive_paved["sites"][0]["properties"]["land_cover"]
        orig_paved_sfr = float(_unwrap(lc["paved"]["sfr"]))
        orig_grass_sfr = float(_unwrap(lc["grass"]["sfr"]))
        lc["grass"]["sfr"] = {"value": orig_grass_sfr + orig_paved_sfr}
        lc["paved"]["sfr"] = {"value": 0.0}
        lc["paved"].pop("soil_depth", None)
        lc["paved"].pop("soil_store_capacity", None)
        lc["paved"].pop("saturated_hydraulic_conductivity", None)
        cfg_orig = SUEWSConfig.from_dict(inactive_paved)

        # ACT: save via to_df_state() and reload via from_df_state() and from_state()
        engine_direct = _normalise_engine_dict(config_to_engine_yaml(cfg_orig))
        df_inactive = cfg_orig.to_df_state()
        reloaded = SUEWSConfig.from_df_state(df_inactive)
        engine_reloaded = _normalise_engine_dict(config_to_engine_yaml(reloaded))

        # ASSERT 2: missingness is preserved as NaN in df_state and None in reloaded config
        for col in (
            ("soildepth", "(0,)"),
            ("soilstorecap_surf", "(0,)"),
            ("sathydraulicconduct", "(0,)"),
        ):
            assert pd.isna(df_inactive.iloc[0][col]), f"Expected NaN in {col}"
        for field in (
            "soil_depth",
            "soil_store_capacity",
            "saturated_hydraulic_conductivity",
        ):
            assert (
                _get_yaml_path(
                    engine_direct, f"sites[0].properties.land_cover.paved.{field}"
                )
                is None
            )
            assert (
                _get_yaml_path(
                    engine_reloaded, f"sites[0].properties.land_cover.paved.{field}"
                )
                is None
            )
            assert getattr(reloaded.sites[0].properties.land_cover.paved, field) is None

        # Legacy SUEWSSimulation.from_state() reloads None and runs cleanly while paved is inactive
        with pytest.warns(DeprecationWarning, match="from_state"):
            sim_from_state = SUEWSSimulation.from_state(df_inactive)
        sim_from_state.update_forcing(df_short)
        out_inactive = sim_from_state.run(end_date=df_short.index[-1])
        assert np.isfinite(out_inactive.df[("SUEWS", "QN")].to_numpy()).all()
        assert (
            sim_from_state.config.sites[0].properties.land_cover.paved.soil_depth
            is None
        )

        # ACT / ASSERT 3: restoring original areas rejects reactivation identically
        # on BOTH the in-memory original AND the df_state-reloaded configuration
        for candidate in (cfg_orig, reloaded, sim_from_state.config):
            cand_lc = candidate.sites[0].properties.land_cover
            cand_lc.paved.sfr = RefValue(orig_paved_sfr)
            cand_lc.grass.sfr = RefValue(orig_grass_sfr)
            for boundary in (
                lambda c=candidate: config_to_engine_yaml(c),
                lambda c=candidate: _grid_config_jsons(c),
                candidate.to_df_state,
                lambda c=candidate: SUEWSSimulation().update_config(
                    c, auto_load_forcing=False
                ),
            ):
                with pytest.raises(ValueError) as exc_react:
                    boundary()
                react_err = str(exc_react.value)
                assert "soil_depth" in react_err
                assert "soil_store_capacity" in react_err
                assert "saturated_hydraulic_conductivity" in react_err

        with pytest.raises(ValueError, match="soil_depth"):
            sim_from_state.run(end_date=df_short.index[-1])

    def test_explicit_150_150_00001_soil_values_preserved_through_df_state_roundtrip(
        self, compact_valid_dict, short_sample_forcing
    ):
        """Genuine concrete ``150.0 / 150.0 / 0.0001`` soil values survive ``to_df_state()`` -> ``from_df_state()``."""
        # ARRANGE: move paved area temporarily to grass, with EXPLICIT 150 / 150 / 0.0001
        df_short = short_sample_forcing(SHORT_RUN_STEPS)
        explicit_cfg = copy.deepcopy(compact_valid_dict)
        lc = explicit_cfg["sites"][0]["properties"]["land_cover"]
        orig_paved_sfr = float(_unwrap(lc["paved"]["sfr"]))
        orig_grass_sfr = float(_unwrap(lc["grass"]["sfr"]))
        lc["grass"]["sfr"] = {"value": orig_grass_sfr + orig_paved_sfr}
        lc["paved"]["sfr"] = {"value": 0.0}
        lc["paved"]["soil_depth"] = {"value": 150.0}
        lc["paved"]["soil_store_capacity"] = {"value": 150.0}
        lc["paved"]["saturated_hydraulic_conductivity"] = {"value": 0.0001}
        cfg = SUEWSConfig.from_dict(explicit_cfg)

        # ACT: round-trip through df_state and restore original surface areas
        df_state = cfg.to_df_state()
        reloaded = SUEWSConfig.from_df_state(df_state)
        reloaded_lc = reloaded.sites[0].properties.land_cover
        reloaded_lc.paved.sfr = RefValue(orig_paved_sfr)
        reloaded_lc.grass.sfr = RefValue(orig_grass_sfr)

        # ASSERT: explicit finite 150 / 150 / 0.0001 are preserved untouched and run cleanly
        assert float(df_state.iloc[0][("soildepth", "(0,)")]) == pytest.approx(150.0)
        assert float(df_state.iloc[0][("soilstorecap_surf", "(0,)")]) == pytest.approx(
            150.0
        )
        assert float(
            df_state.iloc[0][("sathydraulicconduct", "(0,)")]
        ) == pytest.approx(0.0001)
        engine = _normalise_engine_dict(config_to_engine_yaml(reloaded))
        assert _get_yaml_path(
            engine, "sites[0].properties.land_cover.paved.soil_depth"
        ) == pytest.approx(150.0)
        assert _get_yaml_path(
            engine, "sites[0].properties.land_cover.paved.soil_store_capacity"
        ) == pytest.approx(150.0)
        assert _get_yaml_path(
            engine,
            "sites[0].properties.land_cover.paved.saturated_hydraulic_conductivity",
        ) == pytest.approx(0.0001)

        sim = SUEWSSimulation().update_config(reloaded, auto_load_forcing=False)
        sim.update_forcing(df_short)
        out = sim.run(end_date=df_short.index[-1])
        assert np.isfinite(out.df[("SUEWS", "QN")].to_numpy()).all()

    def test_omitted_lai_max_laimax_df_default_vs_engine_yaml(self, sample_dict):
        """Omitted ``lai_max`` uses ``LAIMAX_DF_DEFAULT`` (10.0) in Python/df_state but is absent in YAML."""
        # ARRANGE: inactive grass surface with omitted lai_max and omitted alb_id
        sparse = _build_sparse_executable_dict(sample_dict)
        sparse["sites"][0]["initial_states"]["grass"].pop("alb_id", None)
        grass_cfg = sparse["sites"][0]["properties"]["land_cover"]["grass"]
        alb_min = float(_unwrap(grass_cfg["alb_min"]))
        alb_max = float(_unwrap(grass_cfg["alb_max"]))
        lai_id = float(_unwrap(sparse["sites"][0]["initial_states"]["grass"]["lai_id"]))
        lai_min = float(LAIParams.model_fields["lai_min"].default)
        expected_ratio = min(
            max((lai_id - lai_min) / (LAIParams.LAIMAX_DF_DEFAULT - lai_min), 0.0), 1.0
        )
        # Grass uses reversed relationship: higher LAI -> lower albedo
        expected_alb_id = alb_max - (alb_max - alb_min) * expected_ratio

        # ACT
        cfg = SUEWSConfig.from_dict(sparse)
        df = cfg.to_df_state()
        engine = _normalise_engine_dict(config_to_engine_yaml(cfg))

        # ASSERT
        assert float(LAIParams.LAIMAX_DF_DEFAULT) == pytest.approx(10.0)
        assert float(
            _unwrap(cfg.sites[0].initial_states.grass.alb_id)
        ) == pytest.approx(expected_alb_id)
        assert float(df.iloc[0][("laimax", "(2,)")]) == pytest.approx(
            LAIParams.LAIMAX_DF_DEFAULT
        )
        assert (
            _get_yaml_path(engine, "sites[0].properties.land_cover.grass.lai.lai_max")
            is None
        )

    def test_sdd_id_null_rejected_and_finite_sentinels_shift_daily_state(
        self, compact_valid_dict, short_sample_forcing, tmp_path
    ):
        """Pin ``sdd_id`` null rejection and finite sentinel sensitivity in ``DailyState``.

        Refs #2001, #2002, #2099:
        1. Direct YAML ``initial_states.dectr.sdd_id: {value: null}`` is rejected
           with site/surface/field attribution.
        2. Finite sentinels (``11.0 / 22.0 / 33.0``) survive both ``to_df_state()``
           -> ``from_df_state()`` and ``config_to_engine_yaml()``, and shift
           ``DailyState`` ``SDD_*`` outputs relative to explicit ``0.0``.
        """
        # ARRANGE: 1-day forcing slice (288 x 300 s) so DailyState updates
        df_day = short_sample_forcing(288)
        end_date = df_day.index[-1]

        # 1. Direct YAML {value: null} on dectr.sdd_id is rejected
        null_dict = copy.deepcopy(compact_valid_dict)
        null_dict["sites"][0]["initial_states"]["dectr"]["sdd_id"] = {"value": None}
        null_yaml = _write_yaml(tmp_path / "sdd_null.yml", null_dict)
        with pytest.raises(ValueError, match="sdd_id"):
            SUEWSConfig.from_yaml(str(null_yaml))
        with pytest.raises(ValueError, match="sdd_id"):
            SUEWSConfig.from_dict(copy.deepcopy(null_dict))

        # 2. Explicit 0.0 control vs finite sentinels (11 / 22 / 33)
        zero_dict = copy.deepcopy(compact_valid_dict)
        for surf in VEG_SURFACES:
            zero_dict["sites"][0]["initial_states"][surf]["sdd_id"] = {"value": 0.0}
        cfg_zero = SUEWSConfig.from_dict(zero_dict)
        sim_zero = SUEWSSimulation().update_config(cfg_zero, auto_load_forcing=False)
        sim_zero.update_forcing(df_day)
        df_out_zero = sim_zero.run(end_date=end_date).df

        sentinel_dict = copy.deepcopy(compact_valid_dict)
        sentinels = {"evetr": 11.0, "dectr": 22.0, "grass": 33.0}
        for surf, val in sentinels.items():
            sentinel_dict["sites"][0]["initial_states"][surf]["sdd_id"] = {"value": val}
        cfg_sentinel = SUEWSConfig.from_dict(sentinel_dict)
        rebuilt_sentinel = SUEWSConfig.from_df_state(cfg_sentinel.to_df_state())
        engine_sentinel = _normalise_engine_dict(
            config_to_engine_yaml(rebuilt_sentinel)
        )
        for surf, val in sentinels.items():
            assert _get_yaml_path(
                engine_sentinel, f"sites[0].initial_states.{surf}.sdd_id"
            ) == pytest.approx(val)
        sim_sentinel = SUEWSSimulation().update_config(
            rebuilt_sentinel, auto_load_forcing=False
        )
        sim_sentinel.update_forcing(df_day)
        df_out_sentinel = sim_sentinel.run(end_date=end_date).df
        for col in ("SDD_EveTr", "SDD_DecTr", "SDD_Grass"):
            diff = (
                df_out_sentinel[("DailyState", col)] - df_out_zero[("DailyState", col)]
            ).dropna()
            assert not diff.empty and float(diff.abs().max()) > 1.0, (
                f"Expected finite sdd_id sentinel to shift DailyState.{col}"
            )

    def test_sdd_id_nan_and_float_df_state_none_to_nan_restoration_parity(
        self, compact_valid_dict, tmp_path
    ):
        """Both YAML ``.nan`` and float ``df_state`` ``None -> NaN`` on ``sdd_id`` are rejected.

        Refs #2001, #2002, #2099:
        Schema ``2026.6.dev16`` rejects non-finite ``sdd_id`` at both
        ``SUEWSConfig.from_yaml()`` and ``SUEWSConfig.from_df_state()`` so
        neither YAML ``{value: .nan}`` nor pandas float ``None -> NaN``
        coercion can serialise ``null`` into engine YAML or silently zero
        senescence.
        """
        zero_dict = copy.deepcopy(compact_valid_dict)
        for surf in VEG_SURFACES:
            zero_dict["sites"][0]["initial_states"][surf]["sdd_id"] = {"value": 0.0}
        cfg_zero = SUEWSConfig.from_dict(zero_dict)
        sim_zero = SUEWSSimulation().update_config(cfg_zero, auto_load_forcing=False)

        # 1. Direct YAML {value: .nan} on dectr.sdd_id is rejected at from_yaml()
        nan_dict = copy.deepcopy(zero_dict)
        nan_dict["sites"][0]["initial_states"]["dectr"]["sdd_id"] = {
            "value": float("nan")
        }
        nan_yaml = _write_yaml(tmp_path / "sdd_nan.yml", nan_dict)
        with pytest.raises(ValueError, match=r"(?s)dectr\.sdd_id.*finite"):
            SUEWSConfig.from_yaml(str(nan_yaml))

        # 2. Float df_state None assignment -> NaN is rejected at from_df_state()
        df_state = sim_zero.state_init.copy()
        df_state.loc[df_state.index[0], ("sdd_id", "(1,)")] = None
        assert pd.isna(df_state.loc[df_state.index[0], ("sdd_id", "(1,)")])
        with pytest.raises(ValueError, match=r"(?s)dectr\.sdd_id.*finite"):
            SUEWSConfig.from_df_state(df_state)

    @pytest.mark.parametrize("method", [6, 8], ids=["dyohm_6", "dyohm_bldg_8"])
    @pytest.mark.parametrize("wrapped", [True, False], ids=["wrapped", "bare"])
    def test_dyohm_building_layers_validate_bare_and_wrapped_arrays(
        self, compact_valid_dict, method, wrapped
    ):
        """DyOHM building validation accepts finite bare/wrapped arrays and rejects bad elements.

        Covers ``DYOHM_BUILDING_LAYERS_REQUIRED`` (``dz``, ``k``, ``rho_cp``)
        and ``lambda_c`` across both ``RefValue``-wrapped and bare Python list/float
        representations (as produced by ``from_df_state()`` or bare YAML) for valid,
        ``None``, ``NaN``, ``+Inf``, ``-Inf``, and ``bool`` values.
        """
        # ARRANGE: valid DyOHM building config in wrapped or bare form
        base = copy.deepcopy(compact_valid_dict)
        base["model"]["physics"]["storage_heat"] = (
            {"value": method} if wrapped else method
        )
        props = base["sites"][0]["properties"]
        props["lambda_c"] = {"value": 0.4} if wrapped else 0.4
        th = props["land_cover"]["bldgs"]["thermal_layers"]
        valid_arrays = {
            "dz": [0.2, 0.1, 0.1, 0.5, 1.5],
            "k": [1.2, 1.1, 1.1, 1.5, 1.6],
            "rho_cp": [1.2e6, 1.1e6, 1.1e6, 1.5e6, 1.6e6],
        }
        for name, values in valid_arrays.items():
            th[name] = {"value": list(values)} if wrapped else list(values)

        # ACT & ASSERT 1: valid config passes from_dict, to_df_state -> from_df_state, and validate_for_engine
        cfg_valid = SUEWSConfig.from_dict(copy.deepcopy(base))
        cfg_valid.validate_for_engine()
        cfg_roundtrip = SUEWSConfig.from_df_state(cfg_valid.to_df_state())
        cfg_roundtrip.validate_for_engine()

        # Each field has distinct invalid inputs; the valid input above is shared.
        for field_name, valid_values in valid_arrays.items():
            # ACT & ASSERT 2: invalid elements rejected in both forms at load and handoff
            for bad_elem in (
                None,
                float("nan"),
                float("inf"),
                float("-inf"),
                True,
                False,
            ):
                bad = copy.deepcopy(base)
                bad_list = list(valid_values)
                bad_list[1] = bad_elem
                bad["sites"][0]["properties"]["land_cover"]["bldgs"]["thermal_layers"][
                    field_name
                ] = {"value": bad_list} if wrapped else bad_list
                with pytest.raises(ValueError, match=rf"thermal_layers\.{field_name}"):
                    SUEWSConfig.from_dict(bad)

            for bad_elem in (None, float("nan"), float("inf"), float("-inf"), True):
                mutated = cfg_valid.model_copy(deep=True)
                bad_list = list(valid_values)
                bad_list[1] = bad_elem
                mutated_val = RefValue(bad_list) if wrapped else list(bad_list)
                object.__setattr__(
                    mutated.sites[0].properties.land_cover.bldgs.thermal_layers,
                    field_name,
                    mutated_val,
                )
                with pytest.raises(ValueError, match=rf"thermal_layers\.{field_name}"):
                    mutated.validate_for_engine()

        # ACT & ASSERT 3: lambda_c missing/non-finite/bool rejected in both bare & wrapped forms
        for bad_lam in (None, float("nan"), float("inf"), float("-inf"), True, False):
            bad_lam_dict = copy.deepcopy(base)
            bad_lam_dict["sites"][0]["properties"]["lambda_c"] = (
                {"value": bad_lam} if wrapped else bad_lam
            )
            with pytest.raises(ValueError, match="lambda_c"):
                SUEWSConfig.from_dict(bad_lam_dict)

        for bad_lam in (None, float("nan"), float("inf"), float("-inf"), True):
            mutated_lam = cfg_valid.model_copy(deep=True)
            object.__setattr__(
                mutated_lam.sites[0].properties,
                "lambda_c",
                RefValue(bad_lam) if wrapped else bad_lam,
            )
            with pytest.raises(ValueError, match="lambda_c"):
                mutated_lam.validate_for_engine()

    @staticmethod
    @pytest.mark.parametrize("flag", [True, False], ids=["true", "false"])
    @pytest.mark.parametrize("wrapped", [True, False], ids=["wrapped", "bare"])
    @pytest.mark.parametrize("target", ["dz", "k", "rho_cp", "lambda_c"])
    def test_dyohm_boolean_inputs_rejected_at_yaml_and_simulation_boundaries(
        compact_valid_dict, tmp_path, target, wrapped, flag
    ):
        """Booleans in DyOHM inputs are refused before Pydantic can coerce them.

        Lax float coercion would turn ``True``/``False`` into ``1.0``/``0.0``
        before ``_validate_storage`` sees them, so the rejection has to hold at
        ``SUEWSConfig.from_yaml`` and ``SUEWSSimulation.update_config`` too.
        """
        base = copy.deepcopy(compact_valid_dict)
        base["model"]["physics"]["storage_heat"] = {"value": 6}
        props = base["sites"][0]["properties"]
        props["lambda_c"] = {"value": 0.4}
        th = props["land_cover"]["bldgs"]["thermal_layers"]
        th["dz"] = {"value": [0.2, 0.1, 0.1, 0.5, 1.5]}
        th["k"] = {"value": [1.2, 1.1, 1.1, 1.5, 1.6]}
        th["rho_cp"] = {"value": [1.2e6, 1.1e6, 1.1e6, 1.5e6, 1.6e6]}
        if target == "lambda_c":
            props["lambda_c"] = {"value": flag} if wrapped else flag
            match = "lambda_c"
        else:
            values = list(th[target]["value"])
            values[1] = flag
            th[target] = {"value": values} if wrapped else values
            match = rf"thermal_layers\.{target}"
        path_yaml = tmp_path / "dyohm_bool.yml"
        path_yaml.write_text(yaml.safe_dump(base, sort_keys=False), encoding="utf-8")

        with pytest.raises(ValueError, match=match):
            SUEWSConfig.from_yaml(str(path_yaml))
        with pytest.raises(ValueError, match=match):
            SUEWSSimulation().update_config(
                copy.deepcopy(base), auto_load_forcing=False
            )
