"""End-to-end regressions for snow OHM parameter wiring (gh#2109).

Verifies that ``properties.snow.ohm_coef.*`` and ``ohm_threshold_*`` travel
through ``SUEWSConfig``, ``df_state``, ``config_to_engine_yaml``, the Rust
bridge (``SNOW_PRM``), the Fortran C-API, and ``SUEWS_cal_Qs`` into ``OHM``:

1. Under ``snow_use = 1`` with an OHM-calling storage-heat scheme, the
   ``winter_wet`` snow coefficients drive ``snowState%delta_qi``,
   ``("SUEWS", "QMFreeze")``, and ``("SUEWS", "QH")`` quantitatively while
   closing ``QN + QF + QMRain = QH + QE + QS + QM + QMFreeze``.
2. Changing only ``summer_dry``, ``summer_wet``, ``winter_dry``, or
   ``ohm_threshold_*`` leaves ``deltaQi`` and ``QMFreeze`` unchanged because
   ``suews_phys_ohm.f95`` evaluates snow storage heat unconditionally from
   the ``winter_wet`` regime ``OHM_coef(nsurf + 1, 3, 1:3)``.
3. Under ``snow_use = 0``, perturbing all 14 snow OHM parameters leaves every
   output column bit-for-bit identical.
4. Standalone ``suews-engine`` matches ``SUEWSSimulation`` on snow OHM fluxes
   and enforces ``BR-02`` rejection for missing/null active ``winter_wet``
   coefficients while permitting absence when ``snow_use = 0`` (``BR-03``).
"""

from __future__ import annotations

from importlib import import_module
from importlib.resources import as_file, files
import json
import os
from pathlib import Path
import subprocess

from conftest import TIMESTEPS_PER_DAY, run_cli_command
import numpy as np
import pandas as pd
from pyarrow import ipc
import pytest
import yaml

from supy import SUEWSSimulation
from supy.cmd.suews_cli import cli as suews_cli
from supy.data_model import SUEWSConfig

_run_rust = import_module("supy._run_rust")

pytestmark = [
    pytest.mark.physics,
    pytest.mark.contract("C-BIND", "C-RESPONSE", "C-STRICT", "C-ERROR"),
]

OHM_SEASONS = ("summer_dry", "summer_wet", "winter_dry", "winter_wet")
NON_WATER_SURFACES = ("paved", "bldgs", "evetr", "dectr", "grass", "bsoil")
ALL_SURFACES = (*NON_WATER_SURFACES, "water")
N_STEPS = TIMESTEPS_PER_DAY  # 1 day (288 x 5-minute steps)
COLD_AIR_TEMP_C = -5.0
INITIAL_SNOWPACK_MM = 50.0
INITIAL_SNOWDENS_KG_M3 = 150.0
INITIAL_SNOWALB = 0.8


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


def _build_snow_case(
    *,
    snow_use: int = 1,
    storage_heat: int = 1,
    snow_ohm_overrides: dict[str, dict[str, float]] | None = None,
    threshold_overrides: tuple[float, float] | None = None,
    n_steps: int = N_STEPS,
) -> tuple[SUEWSSimulation, pd.DataFrame, float]:
    """Build a deterministic cold snow-covered simulation and return snow fraction."""
    sim = SUEWSSimulation.from_sample_data()
    forcing = sim.forcing.df.iloc[:n_steps].copy()
    forcing["rain"] = 0.0
    forcing["Tair"] = COLD_AIR_TEMP_C
    forcing["RH"] = 80.0
    forcing["U"] = 3.0
    forcing["pres"] = 1013.0

    sites = [site.model_dump() for site in sim.config.sites]
    site = sites[0]
    snow_covered_sfr = 0.0
    for surf in ALL_SURFACES:
        is_land = surf != "water" and snow_use == 1
        frac = 1.0 if is_land else 0.0
        state = site["initial_states"][surf]
        state.update(
            snowfrac=frac,
            snowpack=INITIAL_SNOWPACK_MM if is_land else 0.0,
            snowdens=INITIAL_SNOWDENS_KG_M3 if is_land else 0.0,
            snowwater=0.0,
            icefrac=0.0,
        )
        if is_land:
            sfr_entry = site["properties"]["land_cover"][surf]["sfr"]
            sfr_val = sfr_entry["value"] if isinstance(sfr_entry, dict) else sfr_entry
            snow_covered_sfr += float(sfr_val) * frac
    site["initial_states"]["snowalb"] = INITIAL_SNOWALB

    if snow_ohm_overrides:
        for season, coefs in snow_ohm_overrides.items():
            for key, value in coefs.items():
                site["properties"]["snow"]["ohm_coef"][season][key] = {"value": value}

    if threshold_overrides is not None:
        thresh_sw, thresh_wd = threshold_overrides
        site["properties"]["snow"]["ohm_threshold_summer_winter"] = {"value": thresh_sw}
        site["properties"]["snow"]["ohm_threshold_wet_dry"] = {"value": thresh_wd}

    sim.update_config(
        {
            "sites": sites,
            "model": {
                "physics": {
                    "net_radiation": 3,
                    "snow_use": snow_use,
                    "storage_heat": storage_heat,
                },
                "control": {"output": {"freq": 300}},
            },
        },
        auto_load_forcing=False,
    )
    sim.update_forcing(forcing)
    return sim, forcing, snow_covered_sfr


def _run_with_snow_state(
    sim: SUEWSSimulation,
    forcing: pd.DataFrame,
) -> tuple[pd.DataFrame, dict[str, float]]:
    """Run ``SUEWSSimulation`` and decode its final ``SnowState`` via public checkpoint."""
    df_out = sim.run(
        start_date=forcing.index[0], end_date=forcing.index[-1], n_jobs=1
    ).df
    checkpoint = sim.checkpoint
    assert checkpoint is not None, "SUEWSSimulation must populate checkpoint"
    state_json = next(iter(checkpoint.grid_states.values()))
    snow_payload = json.loads(state_json)["state"]["members"]["snow_state"]
    bridge = import_module("supy.suews_bridge")
    snow_state = bridge.SnowState.from_values_payload(
        snow_payload["schema_version"], snow_payload["values"]
    )
    return df_out, snow_state.to_dict()


def _assert_snow_energy_closure(df_out: pd.DataFrame) -> None:
    """Verify QN + QF + QMRain = QH + QE + QS + QM + QMFreeze at every step."""
    suews = df_out.xs("SUEWS", level="group", axis=1).xs(1, level="grid")
    residual = (suews["QN"] + suews["QF"] + suews["QMRain"]) - (
        suews["QH"] + suews["QE"] + suews["QS"] + suews["QM"] + suews["QMFreeze"]
    )
    np.testing.assert_allclose(residual.to_numpy(), 0.0, rtol=0.0, atol=1e-8)


@pytest.mark.core
def test_snow_ohm_winter_wet_drives_delta_qi_qmfreeze_and_qh() -> None:
    """Snow ``winter_wet`` OHM coefficients must drive ``deltaQi``, ``QMFreeze``, and ``QH``."""
    # ARRANGE
    pert_ww = {"a1": 0.9, "a2": 0.9, "a3": -50.0}
    cases = {
        "base": _build_snow_case(),
        "a3": _build_snow_case(snow_ohm_overrides={"winter_wet": {"a3": -50.0}}),
        "ww": _build_snow_case(snow_ohm_overrides={"winter_wet": pert_ww}),
        "all": _build_snow_case(
            snow_ohm_overrides={season: dict(pert_ww) for season in OHM_SEASONS},
        ),
    }
    snow_sfr = cases["base"][2]

    # ACT
    results = {
        name: _run_with_snow_state(sim, forcing)
        for name, (sim, forcing, _) in cases.items()
    }

    # ASSERT: energy balance closes for every run
    for df_out, _ in results.values():
        _assert_snow_energy_closure(df_out)

    df_base, snow_base = results["base"]
    df_a3, snow_a3 = results["a3"]
    df_ww, snow_ww = results["ww"]
    df_all, snow_all = results["all"]

    qm_base = df_base.loc[1, ("SUEWS", "QMFreeze")].to_numpy()
    qm_ww = df_ww.loc[1, ("SUEWS", "QMFreeze")].to_numpy()

    # Baseline snow OHM is active (non-zero deltaQi and QMFreeze, not 0.000000)
    delta_qi_base = snow_base["delta_qi.paved"]
    assert abs(delta_qi_base) > 10.0, (
        f"expected non-zero baseline deltaQi, got {delta_qi_base}"
    )
    assert np.max(np.abs(qm_base)) > 10.0, "expected non-zero baseline QMFreeze"

    # Analytical first-step check: shifting only winter_wet.a3 by -20 W m-2 shifts
    # deltaQi by -20 W m-2 and QMFreeze on step 0 by -20 * sum(sfr_i * SnowFrac_i).
    assert (
        df_a3.loc[1, ("SUEWS", "QMFreeze")].to_numpy()[0] - qm_base[0]
    ) == pytest.approx(-20.0 * snow_sfr, abs=1e-6)
    assert abs(snow_a3["delta_qi.paved"] - delta_qi_base) > 15.0

    # Perturbing all three winter_wet coefficients moves deltaQi, QMFreeze, and QH strongly.
    assert abs(snow_ww["delta_qi.paved"] - delta_qi_base) > 15.0
    assert np.max(np.abs(qm_ww - qm_base)) > 15.0
    assert (
        np.max(
            np.abs(
                df_ww.loc[1, ("SUEWS", "QH")].to_numpy()
                - df_base.loc[1, ("SUEWS", "QH")].to_numpy()
            )
        )
        > 15.0
    )

    # Because suews_phys_ohm.f95 selects winter_wet (index 3) unconditionally for
    # snow, perturbing all four seasons matches perturbing winter_wet alone.
    pd.testing.assert_frame_equal(df_ww, df_all, check_exact=True)
    assert snow_ww["delta_qi.paved"] == snow_all["delta_qi.paved"]


@pytest.mark.core
def test_snow_ohm_non_winter_wet_regimes_and_thresholds_do_not_alter_delta_qi() -> None:
    """Only ``winter_wet`` feeds ``OHM_QS_cal`` for snow in ``suews_phys_ohm.f95``."""
    # ARRANGE
    sim_base, forcing, _ = _build_snow_case()
    sim_other, _, _ = _build_snow_case(
        snow_ohm_overrides={
            "summer_dry": {"a1": 0.9, "a2": 0.9, "a3": -50.0},
            "summer_wet": {"a1": 0.8, "a2": 0.8, "a3": -45.0},
            "winter_dry": {"a1": 0.7, "a2": 0.7, "a3": -40.0},
        },
        threshold_overrides=(5.0, 0.4),
    )

    # ACT
    df_base, snow_base = _run_with_snow_state(sim_base, forcing)
    df_other, snow_other = _run_with_snow_state(sim_other, forcing)

    # ASSERT
    pd.testing.assert_frame_equal(df_base, df_other, check_exact=True)
    assert snow_base["delta_qi.paved"] == snow_other["delta_qi.paved"]


@pytest.mark.core
def test_snow_ohm_perturbation_invariant_when_snow_disabled() -> None:
    """With ``snow_use = 0``, changing all 14 snow OHM parameters must be bit-identical."""
    # ARRANGE
    sim_base, forcing, _ = _build_snow_case(snow_use=0)
    pert_ww = {"a1": 0.9, "a2": 0.9, "a3": -50.0}
    sim_pert, _, _ = _build_snow_case(
        snow_use=0,
        snow_ohm_overrides={season: dict(pert_ww) for season in OHM_SEASONS},
        threshold_overrides=(4.5, 0.35),
    )

    # ACT
    df_base = sim_base.run(
        start_date=forcing.index[0], end_date=forcing.index[-1], n_jobs=1
    ).df
    df_pert = sim_pert.run(
        start_date=forcing.index[0], end_date=forcing.index[-1], n_jobs=1
    ).df

    # ASSERT
    pd.testing.assert_frame_equal(df_base, df_pert, check_exact=True)


@pytest.mark.core
def test_snow_ohm_df_state_roundtrip_preserves_all_14_fields() -> None:
    """``to_df_state`` / ``from_df_state`` must preserve snow OHM parameters at surface index 7."""
    # ARRANGE
    distinct = {
        "summer_dry": {"a1": 0.11, "a2": 0.21, "a3": -11.0},
        "summer_wet": {"a1": 0.12, "a2": 0.22, "a3": -12.0},
        "winter_dry": {"a1": 0.13, "a2": 0.23, "a3": -13.0},
        "winter_wet": {"a1": 0.14, "a2": 0.24, "a3": -14.0},
    }
    sim, _, _ = _build_snow_case(
        snow_ohm_overrides=distinct,
        threshold_overrides=(11.25, 0.77),
        n_steps=12,
    )

    # ACT
    df_state = sim.config.to_df_state()
    rebuilt = SUEWSConfig.from_df_state(df_state)
    snow = rebuilt.sites[0].properties.snow

    # ASSERT
    assert snow.ohm_threshold_summer_winter.value == pytest.approx(11.25)
    assert snow.ohm_threshold_wet_dry.value == pytest.approx(0.77)
    for season, expected in distinct.items():
        season_obj = getattr(snow.ohm_coef, season)
        for key, value in expected.items():
            assert getattr(season_obj, key).value == pytest.approx(value)


def _write_cold_hourly_forcing(dest: Path) -> Path:
    """Write 12 hourly cold-snow forcing rows for standalone/SuPy parity checks."""
    sample_dir = files("supy").joinpath("sample_data")
    forcing_raw = sample_dir.joinpath("Kc_2012_data_60.txt").read_text(encoding="utf-8")
    forcing_lines = [line for line in forcing_raw.splitlines() if line.strip()]
    header = forcing_lines[0].split()
    overrides = {
        header.index("Tair"): f"{COLD_AIR_TEMP_C:.1f}",
        header.index("rain"): "0.0",
        header.index("RH"): "80.0",
        header.index("U"): "3.0",
        header.index("pres"): "101.3",
    }
    rows = []
    for line in forcing_lines[1:13]:
        parts = line.split()
        for col_idx, val in overrides.items():
            parts[col_idx] = val
        rows.append(" ".join(parts))
    dest.write_text("\n".join([forcing_lines[0], *rows]) + "\n", encoding="utf-8")
    return dest


def _run_standalone_and_supy_pair(
    engine: Path,
    forcing_file: Path,
    out_dir: Path,
    winter_wet: dict[str, float],
) -> tuple[np.ndarray, np.ndarray]:
    """Run ``suews-engine`` and ``SUEWSSimulation`` on identical YAML/forcing inputs."""
    out_dir.mkdir()
    sim = SUEWSSimulation.from_sample_data()
    sites = [site.model_dump() for site in sim.config.sites]
    site = sites[0]
    for surf in ALL_SURFACES:
        is_land = surf != "water"
        site["initial_states"][surf].update(
            snowfrac=1.0 if is_land else 0.0,
            snowpack=INITIAL_SNOWPACK_MM if is_land else 0.0,
            snowdens=INITIAL_SNOWDENS_KG_M3 if is_land else 0.0,
            snowwater=0.0,
            icefrac=0.0,
        )
    site["initial_states"]["snowalb"] = INITIAL_SNOWALB
    for key, value in winter_wet.items():
        site["properties"]["snow"]["ohm_coef"]["winter_wet"][key] = {"value": value}

    sim.update_config(
        {
            "sites": sites,
            "model": {
                "physics": {"net_radiation": 3, "snow_use": 1, "storage_heat": 1},
                "control": {
                    "forcing": {"file": str(forcing_file)},
                    "output": {"dir": str(out_dir), "freq": 300},
                },
            },
        },
        auto_load_forcing=False,
    )
    cfg_path = out_dir / "resolved.yml"
    cfg_path.write_text(_run_rust.config_to_engine_yaml(sim.config), encoding="utf-8")

    result = subprocess.run(
        [str(engine), "run", str(cfg_path)],
        capture_output=True,
        text=True,
        timeout=60,
        check=False,
    )
    assert result.returncode == 0, f"suews-engine failed: {result.stderr}"

    arrow_path = out_dir / "suews_output.arrow"
    assert arrow_path.is_file(), f"expected {arrow_path}"
    with ipc.open_file(str(arrow_path)) as reader:
        table = reader.read_all().select(["QMFreeze", "QH"])
        engine_qm = table.column("QMFreeze").to_numpy(zero_copy_only=False).copy()
        engine_qh = table.column("QH").to_numpy(zero_copy_only=False).copy()

    sim.update_forcing(str(forcing_file))
    df_supy = sim.run(
        start_date=sim.forcing.df.index[0],
        end_date=sim.forcing.df.index[-1],
        n_jobs=1,
    ).df
    np.testing.assert_allclose(
        engine_qm,
        df_supy.loc[1, ("SUEWS", "QMFreeze")].to_numpy(),
        rtol=1e-10,
        atol=1e-9,
    )
    np.testing.assert_allclose(
        engine_qh, df_supy.loc[1, ("SUEWS", "QH")].to_numpy(), rtol=1e-10, atol=1e-9
    )
    return engine_qm, engine_qh


@pytest.mark.core
def test_snow_ohm_standalone_engine_parity_and_active_validation(
    tmp_path: Path,
) -> None:
    """Standalone ``suews-engine`` matches SuPy on snow OHM and enforces BR-02/BR-03."""
    engine = _locate_suews_engine()
    forcing_file = _write_cold_hourly_forcing(tmp_path / "cold_forcing_60.txt")

    qm_base, qh_base = _run_standalone_and_supy_pair(
        engine, forcing_file, tmp_path / "base", {"a1": 0.25, "a2": 0.6, "a3": -30.0}
    )
    qm_pert, qh_pert = _run_standalone_and_supy_pair(
        engine, forcing_file, tmp_path / "pert", {"a1": 0.9, "a2": 0.9, "a3": -50.0}
    )
    assert np.max(np.abs(qm_pert - qm_base)) > 15.0
    assert np.max(np.abs(qh_pert - qh_base)) > 15.0

    # BR-02 negative path: standalone engine rejects missing active winter_wet.a1
    base_yaml = (tmp_path / "base" / "resolved.yml").read_text(encoding="utf-8")
    bad_dir = tmp_path / "missing_active"
    bad_dir.mkdir()
    bad_doc = yaml.safe_load(base_yaml)
    bad_doc["model"]["control"]["output"]["dir"] = str(bad_dir)
    bad_doc["sites"][0]["properties"]["snow"]["ohm_coef"]["winter_wet"].pop("a1")
    bad_cfg = bad_dir / "resolved.yml"
    bad_cfg.write_text(yaml.safe_dump(bad_doc, sort_keys=False), encoding="utf-8")

    bad_res = subprocess.run(
        [str(engine), "run", str(bad_cfg)],
        capture_output=True,
        text=True,
        timeout=60,
        check=False,
    )
    assert bad_res.returncode != 0, (
        "expected standalone engine to reject missing active snow winter_wet.a1"
    )
    assert (
        "properties.snow.ohm_coef.winter_wet.a1"
        in f"{bad_res.stdout}\n{bad_res.stderr}"
    )

    # BR-03 inactive path: omitting winter_wet when snow_use = 0 succeeds
    off_dir = tmp_path / "inactive_snow_off"
    off_dir.mkdir()
    off_doc = yaml.safe_load(base_yaml)
    off_doc["model"]["physics"]["snow_use"] = {"value": 0}
    off_doc["model"]["control"]["output"]["dir"] = str(off_dir)
    off_doc["sites"][0]["properties"]["snow"].pop("ohm_coef")
    off_cfg = off_dir / "resolved.yml"
    off_cfg.write_text(yaml.safe_dump(off_doc, sort_keys=False), encoding="utf-8")

    off_res = subprocess.run(
        [str(engine), "run", str(off_cfg)],
        capture_output=True,
        text=True,
        timeout=60,
        check=False,
    )
    assert off_res.returncode == 0, (
        f"expected inactive snow_use=0 omitted ohm_coef to succeed: {off_res.stderr}"
    )


def _write_snow_ohm_entry_point_yaml(
    case_dir: Path,
    forcing_file: Path,
    *,
    snow_use: int,
    storage_heat: int = 1,
    mutate_snow=None,
) -> Path:
    """Write a short YAML configuration for Python API and ``suews run`` checks."""
    case_dir.mkdir(parents=True, exist_ok=True)
    out_dir = case_dir / "out"
    out_dir.mkdir(exist_ok=True)

    sim = SUEWSSimulation.from_sample_data()
    sites = [site.model_dump() for site in sim.config.sites]
    site = sites[0]
    for surf in ALL_SURFACES:
        is_land = surf != "water" and snow_use == 1
        site["initial_states"][surf].update(
            snowfrac=1.0 if is_land else 0.0,
            snowpack=INITIAL_SNOWPACK_MM if is_land else 0.0,
            snowdens=INITIAL_SNOWDENS_KG_M3 if is_land else 0.0,
            snowwater=0.0,
            icefrac=0.0,
        )
    site["initial_states"]["snowalb"] = INITIAL_SNOWALB
    sim.update_config(
        {
            "sites": sites,
            "model": {
                "physics": {
                    "net_radiation": 3,
                    "snow_use": snow_use,
                    "storage_heat": storage_heat,
                },
                "control": {
                    "start_time": "2012-01-01 00:05:00",
                    "end_time": "2012-01-01 12:00:00",
                    "forcing": {"file": str(forcing_file)},
                    "output": {
                        "dir": str(out_dir),
                        "format": "txt",
                        "freq": 3600,
                        "groups": ["SUEWS"],
                    },
                },
            },
        },
        auto_load_forcing=False,
    )

    doc = sim.config.model_dump(mode="json")
    snow_cfg = doc["sites"][0]["properties"]["snow"]
    if mutate_snow is not None:
        mutate_snow(snow_cfg)

    cfg_path = case_dir / "config.yml"
    cfg_path.write_text(yaml.safe_dump(doc, sort_keys=False), encoding="utf-8")
    return cfg_path


def _assert_mentions_winter_wet_coef(message: str, coef_key: str) -> None:
    """Confirm an error names the offending ``winter_wet.<coef_key>`` path."""
    lower = message.lower()
    assert "ohm_coef" in lower, f"error must name ohm_coef, got:\n{message}"
    assert "winter_wet" in lower, f"error must name winter_wet, got:\n{message}"
    assert coef_key in lower, f"error must name {coef_key}, got:\n{message}"


def _mutate_missing_a1(snow_cfg: dict) -> None:
    snow_cfg["ohm_coef"]["winter_wet"].pop("a1")


def _mutate_null_a2(snow_cfg: dict) -> None:
    snow_cfg["ohm_coef"]["winter_wet"]["a2"] = None


def _mutate_malformed_a3(snow_cfg: dict) -> None:
    snow_cfg["ohm_coef"]["winter_wet"]["a3"] = "not-a-number"


def _mutate_nan_a1(snow_cfg: dict) -> None:
    snow_cfg["ohm_coef"]["winter_wet"]["a1"] = float("nan")


def _mutate_inf_a2(snow_cfg: dict) -> None:
    snow_cfg["ohm_coef"]["winter_wet"]["a2"] = float("inf")


_ACTIVE_WINTER_WET_FAILURE_CASES = [
    pytest.param("missing_a1", _mutate_missing_a1, "a1", True, id="missing-a1"),
    pytest.param("null_a2", _mutate_null_a2, "a2", True, id="null-a2"),
    pytest.param("malformed_a3", _mutate_malformed_a3, "a3", False, id="malformed-a3"),
    pytest.param("nan_a1", _mutate_nan_a1, "a1", False, id="nonfinite-nan-a1"),
    pytest.param("inf_a2", _mutate_inf_a2, "a2", False, id="nonfinite-inf-a2"),
]


@pytest.mark.core
@pytest.mark.parametrize(
    ("case_name", "mutate_snow", "coef_key", "reaches_run"),
    _ACTIVE_WINTER_WET_FAILURE_CASES,
)
def test_snow_ohm_python_api_rejects_invalid_active_winter_wet(
    tmp_path: Path,
    case_name: str,
    mutate_snow,
    coef_key: str,
    reaches_run: bool,
) -> None:
    """``SUEWSSimulation`` / ``run()`` rejects missing, null, malformed and non-finite active snow OHM."""
    forcing_file = _write_cold_hourly_forcing(tmp_path / "cold_forcing_60.txt")
    cfg_path = _write_snow_ohm_entry_point_yaml(
        tmp_path / case_name,
        forcing_file,
        snow_use=1,
        mutate_snow=mutate_snow,
    )

    if reaches_run:
        sim = SUEWSSimulation(cfg_path)
        with pytest.raises((RuntimeError, ValueError)) as exc_info:
            sim.run(
                start_date=sim.forcing.df.index[0],
                end_date=sim.forcing.df.index[-1],
                n_jobs=1,
            )
        msg = str(exc_info.value)
        assert f"properties.snow.ohm_coef.winter_wet.{coef_key}" in msg, msg
    else:
        with pytest.raises((ValueError, RuntimeError)) as exc_info:
            sim = SUEWSSimulation(cfg_path)
            sim.run(
                start_date=sim.forcing.df.index[0],
                end_date=sim.forcing.df.index[-1],
                n_jobs=1,
            )
        _assert_mentions_winter_wet_coef(str(exc_info.value), coef_key)


@pytest.mark.core
@pytest.mark.parametrize(
    ("case_name", "mutate_snow", "coef_key", "_reaches_run"),
    _ACTIVE_WINTER_WET_FAILURE_CASES,
)
def test_snow_ohm_python_cli_rejects_invalid_active_winter_wet(
    cli_runner,
    tmp_path: Path,
    case_name: str,
    mutate_snow,
    coef_key: str,
    _reaches_run: bool,
) -> None:
    """``suews run`` exits non-zero and names the offending active snow OHM field."""
    forcing_file = _write_cold_hourly_forcing(tmp_path / "cold_forcing_60.txt")
    cfg_path = _write_snow_ohm_entry_point_yaml(
        tmp_path / f"cli_{case_name}",
        forcing_file,
        snow_use=1,
        mutate_snow=mutate_snow,
    )

    result = run_cli_command(cli_runner, suews_cli, ["run", str(cfg_path)], check=False)
    assert result.returncode != 0, (
        f"expected suews run to reject {case_name}; stdout={result.stdout}\nstderr={result.stderr}"
    )
    combined = f"{result.stdout}\n{result.stderr}"
    _assert_mentions_winter_wet_coef(combined, coef_key)


@pytest.mark.core
def test_snow_ohm_inactive_branch_succeeds_in_python_api_and_cli(
    cli_runner,
    tmp_path: Path,
) -> None:
    """With ``snow_use = 0``, omitting ``properties.snow.ohm_coef`` succeeds in API and CLI."""
    forcing_file = _write_cold_hourly_forcing(tmp_path / "cold_forcing_60.txt")

    def _omit_ohm_coef(snow_cfg: dict) -> None:
        snow_cfg["ohm_coef"] = None

    api_cfg = _write_snow_ohm_entry_point_yaml(
        tmp_path / "inactive_api",
        forcing_file,
        snow_use=0,
        mutate_snow=_omit_ohm_coef,
    )
    sim = SUEWSSimulation(api_cfg)
    out = sim.run(
        start_date=sim.forcing.df.index[0],
        end_date=sim.forcing.df.index[-1],
        n_jobs=1,
    )
    assert np.all(np.isfinite(out.df.loc[1, ("SUEWS", "QH")].to_numpy()))

    cli_cfg = _write_snow_ohm_entry_point_yaml(
        tmp_path / "inactive_cli",
        forcing_file,
        snow_use=0,
        mutate_snow=_omit_ohm_coef,
    )
    result = run_cli_command(cli_runner, suews_cli, ["run", str(cli_cfg)], check=False)
    assert result.returncode == 0, (
        f"expected inactive snow_use=0 to succeed in suews run:\n{result.stdout}\n{result.stderr}"
    )
