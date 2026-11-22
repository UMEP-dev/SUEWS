"""Regression tests for SPARTACUS above-canopy norm_perim_air_veg reset (Refs #2172).

When urban buildings extend above the vegetation canopy
(veg_fraction(jlay) <= min_vegetation_fraction) with veg_contact_fraction_const > 0,
unreset norm_perim_air_veg carried the previous vegetated layer's perimeter into
radsurf_urban_lw.F90 and radsurf_urban_sw.F90, breaking LW radiative budget closure
and under-intercepting SW on above-canopy walls.
"""

from __future__ import annotations

import copy
from importlib.resources import as_file, files
from pathlib import Path
import shutil

import numpy as np
import pandas as pd
import pytest
import yaml

from supy import SUEWSSimulation

pytestmark = [
    pytest.mark.physics,
    pytest.mark.core,
    pytest.mark.contract("C-RESPONSE", "C-BALANCE"),
]

STEBBS_FIXTURE_DIR = (
    Path(__file__).resolve().parents[1] / "fixtures" / "data_test" / "stebbs_test"
)
# Native SPARTACUS LW integration leaves a ~0.002-0.008 W m-2 closure residual;
# unpatched above-canopy runs fail by ~6.3-45.3 W m-2.
LW_BUDGET_ATOL = 0.02
SW_BUDGET_ATOL = 1e-8
# Near-black canopy (albedo = 1e-5) keeps upward reflected SW from lower layers
# below ~1e-2 W m-2, whereas unpatched radsurf_urban_sw shifts KIn_Wall_3 by ~93 W m-2.
NEAR_BLACK_ALBEDO = 1e-5
NEAR_BLACK_SW_ATOL = 0.05
NUM_CANOPY_LAYERS = 3


def _load_yaml(path: Path) -> dict:
    with path.open("r", encoding="utf-8") as fh:
        return yaml.safe_load(fh)


def _dump_yaml(data: dict, path: Path) -> None:
    with path.open("w", encoding="utf-8") as fh:
        yaml.safe_dump(data, fh, sort_keys=False, default_flow_style=False)


def _compute_budget_residuals(df_output: pd.DataFrame) -> tuple[pd.Series, pd.Series]:
    """Return independent SPARTACUS LW and SW canopy energy conservation residuals."""
    sp = df_output["SPARTACUS"]
    lw_sinks = sp["LNet_Grnd"].astype(float).copy()
    sw_sinks = sp["KNet_Grnd"].astype(float).copy()
    for layer in range(1, NUM_CANOPY_LAYERS + 1):
        lw_sinks = (
            lw_sinks
            + sp[f"sfr_Roof_{layer}"].fillna(0.0) * sp[f"LNet_Roof_{layer}"].fillna(0.0)
            + sp[f"sfr_Wall_{layer}"].fillna(0.0) * sp[f"LNet_Wall_{layer}"].fillna(0.0)
            + sp[f"LCAAbs_{layer}"].fillna(0.0)
            + sp[f"LVegAbs_{layer}"].fillna(0.0)
        )
        sw_sinks = (
            sw_sinks
            + sp[f"sfr_Roof_{layer}"].fillna(0.0) * sp[f"KNet_Roof_{layer}"].fillna(0.0)
            + sp[f"sfr_Wall_{layer}"].fillna(0.0) * sp[f"KNet_Wall_{layer}"].fillna(0.0)
            + sp[f"KCAAbs_{layer}"].fillna(0.0)
            + sp[f"KVegAbs_{layer}"].fillna(0.0)
        )
    lw_resid = sp["LTopNet"].astype(float) - lw_sinks
    sw_resid = sp["KTopNet"].astype(float) - sw_sinks
    return lw_resid, sw_resid


def _assert_no_spartacus_warnings(kernel_warnings: pd.DataFrame) -> None:
    if kernel_warnings.empty:
        return
    spartacus_warnings = kernel_warnings[
        kernel_warnings["location"].str.contains("SPARTACUS", case=False, na=False)
    ]
    assert spartacus_warnings.empty, spartacus_warnings.to_string(index=False)


def _make_sample_spartacus_config(
    work_dir: Path,
    *,
    veg_contact_fraction_const: float,
    full_height_canopy: bool = False,
    black_canopy: bool = False,
    start_time: str = "2012-06-13",
    end_time: str = "2012-06-13",
) -> Path:
    """Write a self-contained sample-derived SPARTACUS YAML and forcing into work_dir."""
    with as_file(files("supy") / "sample_data") as sample_dir:
        cfg = _load_yaml(sample_dir / "sample_config.yml")
        forcing_dst = work_dir / "Kc_2012_data_60.txt"
        if not forcing_dst.exists():
            shutil.copy2(sample_dir / "Kc_2012_data_60.txt", forcing_dst)

    cfg = copy.deepcopy(cfg)
    cfg["model"]["control"]["start_time"] = start_time
    cfg["model"]["control"]["end_time"] = end_time
    cfg["model"]["control"]["forcing"]["file"] = {"value": "Kc_2012_data_60.txt"}
    cfg["model"]["control"]["output"]["groups"] = ["SUEWS", "SPARTACUS", "EHC"]
    cfg["model"]["control"]["output"]["include_provisional"] = True
    cfg["model"]["control"]["output"]["freq"] = 300
    cfg["model"]["physics"]["net_radiation"] = {"value": 1003}
    cfg["model"]["physics"]["storage_heat"] = {"value": 5}

    props = cfg["sites"][0]["properties"]
    props["spartacus"]["veg_contact_fraction_const"] = {
        "value": veg_contact_fraction_const
    }
    props["spartacus"]["use_sw_direct_albedo"] = {"value": 0.0 if black_canopy else 1.0}

    if full_height_canopy:
        props["land_cover"]["dectr"]["height_deciduous_tree"] = {"value": 22.0}
        props["land_cover"]["evetr"]["height_evergreen_tree"] = {"value": 22.0}
        props["vertical_layers"]["veg_frac"] = {"value": [0.01, 0.02, 0.01]}
        props["vertical_layers"]["veg_scale"] = {"value": [10.0, 10.0, 10.0]}
    else:
        # Layer 3 (15-22 m) extends above the 13.1 m tree canopy.
        props["vertical_layers"]["veg_frac"]["value"][2] = 0.0
        props["vertical_layers"]["veg_scale"]["value"][2] = 0.0

    if black_canopy:
        # Near-zero SW albedos and scattering suppress upward SW reflection from
        # vegetated layers 1-2 into vegetation-free layer 3 while staying away
        # from zero-albedo solver degeneracies.
        props["land_cover"]["dectr"]["sfr"] = {"value": 0.15}
        props["land_cover"]["paved"]["sfr"] = {"value": 0.30}
        props["vertical_layers"]["veg_frac"] = {"value": [0.15, 0.15, 0.0]}
        props["vertical_layers"]["veg_scale"] = {"value": [10.0, 10.0, 0.0]}
        props["vertical_layers"]["building_scale"] = {"value": [20.0, 20.0, 20.0]}
        props["spartacus"]["veg_ssa_sw"] = {"value": NEAR_BLACK_ALBEDO}
        props["spartacus"]["air_ssa_sw"] = {"value": NEAR_BLACK_ALBEDO}
        for surface in ("paved", "bldgs", "evetr", "dectr", "grass", "bsoil", "water"):
            lc = props["land_cover"][surface]
            if "alb" in lc:
                lc["alb"] = {"value": NEAR_BLACK_ALBEDO}
            if "alb_min" in lc:
                lc["alb_min"] = {"value": NEAR_BLACK_ALBEDO}
            if "alb_max" in lc:
                lc["alb_max"] = {"value": NEAR_BLACK_ALBEDO}
        init_states = cfg["sites"][0]["initial_states"]
        for surface in ("evetr", "dectr", "grass"):
            if "alb_id" in init_states[surface]:
                init_states[surface]["alb_id"] = {"value": NEAR_BLACK_ALBEDO}
        for facet_group in ("roofs", "walls"):
            for layer_props in props["vertical_layers"][facet_group]:
                layer_props["alb"] = {"value": NEAR_BLACK_ALBEDO}

    tag = (
        f"sample_c{veg_contact_fraction_const}_full{int(full_height_canopy)}"
        f"_black{int(black_canopy)}.yml"
    )
    yaml_path = work_dir / tag
    _dump_yaml(cfg, yaml_path)
    return yaml_path


def _make_stebbs_spartacus_config(
    work_dir: Path,
    *,
    veg_contact_fraction_const: float,
    start_time: str | None = "2017-08-26 06:00:00",
    end_time: str | None = "2017-08-26 14:00:00",
) -> Path:
    """Write a self-contained STEBBS fixture YAML with normalised dry-period rain."""
    forcing_dst = work_dir / "test_2017_data_15_mp_dryrain.txt"
    if not forcing_dst.exists():
        raw_lines = (
            (STEBBS_FIXTURE_DIR / "test_2017_data_15_mp.txt")
            .read_text(encoding="utf-8")
            .splitlines()
        )
        rain_idx = raw_lines[0].split().index("rain")
        cleaned = [raw_lines[0]]
        for line in raw_lines[1:]:
            if not line.strip():
                continue
            tokens = line.split("\t") if "\t" in line else line.split()
            if float(tokens[rain_idx]) < 0.0:
                tokens[rain_idx] = "0"
            cleaned.append("\t".join(tokens))
        forcing_dst.write_text("\n".join(cleaned) + "\n", encoding="utf-8")

    cfg = _load_yaml(STEBBS_FIXTURE_DIR / "sample_config.yml")
    if start_time is not None:
        cfg["model"]["control"]["start_time"] = start_time
    if end_time is not None:
        cfg["model"]["control"]["end_time"] = end_time
    cfg["model"]["control"]["forcing"]["file"] = {"value": forcing_dst.name}
    cfg["model"]["control"]["output"]["groups"] = [
        "SUEWS",
        "SPARTACUS",
        "EHC",
        "STEBBS",
    ]
    cfg["model"]["control"]["output"]["include_provisional"] = True
    cfg["model"]["control"]["output"]["freq"] = 300
    cfg["sites"][0]["properties"]["spartacus"]["veg_contact_fraction_const"] = {
        "value": veg_contact_fraction_const
    }
    yaml_path = work_dir / f"stebbs_c{veg_contact_fraction_const}.yml"
    _dump_yaml(cfg, yaml_path)
    return yaml_path


def test_above_canopy_lw_budget_closes_with_veg_contact(tmp_path):
    """Above-canopy urban SPARTACUS with veg_contact_fraction_const=0.5 closes LW budget."""
    # ARRANGE
    yaml_path = _make_sample_spartacus_config(
        tmp_path, veg_contact_fraction_const=0.5, full_height_canopy=False
    )

    # ACT
    sim = SUEWSSimulation(str(yaml_path))
    result = sim.run(n_jobs=1)
    _assert_no_spartacus_warnings(result.kernel_warnings)
    lw_resid, sw_resid = _compute_budget_residuals(result.df)

    # ASSERT
    assert np.isfinite(lw_resid.to_numpy()).all()
    assert np.isfinite(sw_resid.to_numpy()).all()
    np.testing.assert_allclose(
        lw_resid.to_numpy(),
        0.0,
        rtol=0.0,
        atol=LW_BUDGET_ATOL,
        equal_nan=False,
        err_msg="SPARTACUS LW radiative budget failed to close above the canopy",
    )
    np.testing.assert_allclose(
        sw_resid.to_numpy(),
        0.0,
        rtol=0.0,
        atol=SW_BUDGET_ATOL,
        equal_nan=False,
        err_msg="SPARTACUS SW radiative budget failed to close above the canopy",
    )


def test_stebbs_above_canopy_lw_budget_closes_with_default_veg_contact(tmp_path):
    """Bundled STEBBS geometry with default veg_contact_fraction_const=0.5 closes LW budget."""
    # ARRANGE
    yaml_path = _make_stebbs_spartacus_config(tmp_path, veg_contact_fraction_const=0.5)

    # ACT
    sim = SUEWSSimulation(str(yaml_path))
    result = sim.run(n_jobs=1)
    _assert_no_spartacus_warnings(result.kernel_warnings)
    lw_resid, sw_resid = _compute_budget_residuals(result.df)

    # ASSERT
    assert np.isfinite(lw_resid.to_numpy()).all()
    assert np.isfinite(sw_resid.to_numpy()).all()
    np.testing.assert_allclose(
        lw_resid.to_numpy(),
        0.0,
        rtol=0.0,
        atol=LW_BUDGET_ATOL,
        equal_nan=False,
        err_msg="STEBBS-SPARTACUS LW budget failed to close with veg_contact_fraction_const=0.5",
    )
    np.testing.assert_allclose(
        sw_resid.to_numpy(),
        0.0,
        rtol=0.0,
        atol=SW_BUDGET_ATOL,
        equal_nan=False,
        err_msg="STEBBS-SPARTACUS SW budget failed to close with veg_contact_fraction_const=0.5",
    )


def test_above_canopy_sw_wall_interception_invariant_in_near_black_canopy(tmp_path):
    """In a near-black canopy, veg_contact_fraction_const cannot alter above-canopy SW walls."""
    # ARRANGE
    yaml_c0 = _make_sample_spartacus_config(
        tmp_path,
        veg_contact_fraction_const=0.0,
        full_height_canopy=False,
        black_canopy=True,
        start_time="2012-06-13 10:00:00",
        end_time="2012-06-13 14:00:00",
    )
    yaml_c5 = _make_sample_spartacus_config(
        tmp_path,
        veg_contact_fraction_const=0.5,
        full_height_canopy=False,
        black_canopy=True,
        start_time="2012-06-13 10:00:00",
        end_time="2012-06-13 14:00:00",
    )

    # ACT
    res_c0 = SUEWSSimulation(str(yaml_c0)).run(n_jobs=1)
    res_c5 = SUEWSSimulation(str(yaml_c5)).run(n_jobs=1)
    _assert_no_spartacus_warnings(res_c0.kernel_warnings)
    _assert_no_spartacus_warnings(res_c5.kernel_warnings)
    sp_c0 = res_c0.df["SPARTACUS"]
    sp_c5 = res_c5.df["SPARTACUS"]

    # ASSERT: all compared wall SW series must be strictly finite (no matching-NaN bypass)
    for col in ("KIn_Wall_1", "KIn_Wall_3", "KNet_Wall_3"):
        assert np.all(np.isfinite(sp_c0[col].to_numpy(dtype=float)))
        assert np.all(np.isfinite(sp_c5[col].to_numpy(dtype=float)))

    # Non-vacuous daytime wall irradiance and active lower-layer contact effect
    assert float(sp_c0["KIn_Wall_3"].max()) > 50.0
    assert float((sp_c5["KIn_Wall_1"] - sp_c0["KIn_Wall_1"]).abs().max()) > 1.0

    # Layer 3 has zero vegetation and negligible upward SW reflection from layers 1-2.
    for col in ("KIn_Wall_3", "KNet_Wall_3"):
        np.testing.assert_allclose(
            sp_c5[col].to_numpy(dtype=float),
            sp_c0[col].to_numpy(dtype=float),
            rtol=0.0,
            atol=NEAR_BLACK_SW_ATOL,
            equal_nan=False,
            err_msg=f"Above-canopy {col} changed with veg_contact_fraction_const in a near-black canopy",
        )


@pytest.mark.parametrize(
    "case_name",
    ["sample_zero_contact", "sample_full_canopy", "stebbs_zero_contact"],
)
def test_negative_control_budgets_close(tmp_path, case_name):
    """Negative controls (veg_contact=0 or full-height canopy) remain well-posed and closed."""
    # ARRANGE
    if case_name == "sample_zero_contact":
        yaml_path = _make_sample_spartacus_config(
            tmp_path, veg_contact_fraction_const=0.0, full_height_canopy=False
        )
    elif case_name == "sample_full_canopy":
        yaml_path = _make_sample_spartacus_config(
            tmp_path, veg_contact_fraction_const=0.5, full_height_canopy=True
        )
    else:
        yaml_path = _make_stebbs_spartacus_config(
            tmp_path, veg_contact_fraction_const=0.0
        )

    # ACT
    result = SUEWSSimulation(str(yaml_path)).run(n_jobs=1)
    _assert_no_spartacus_warnings(result.kernel_warnings)
    lw_resid, sw_resid = _compute_budget_residuals(result.df)

    # ASSERT
    assert np.isfinite(lw_resid.to_numpy()).all()
    assert np.isfinite(sw_resid.to_numpy()).all()
    np.testing.assert_allclose(
        lw_resid.to_numpy(), 0.0, rtol=0.0, atol=LW_BUDGET_ATOL, equal_nan=False
    )
    np.testing.assert_allclose(
        sw_resid.to_numpy(), 0.0, rtol=0.0, atol=SW_BUDGET_ATOL, equal_nan=False
    )
