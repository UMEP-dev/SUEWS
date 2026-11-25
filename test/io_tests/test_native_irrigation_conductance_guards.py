"""Native resolved-YAML guards for conductance and irrigation parameters (#2002)."""

from __future__ import annotations

from copy import deepcopy
from importlib import import_module
from importlib.resources import as_file, files
import os
from pathlib import Path
import subprocess

from conftest import run_cli_command
import numpy as np
import pytest
import yaml

from supy import SUEWSSimulation
from supy.cmd.suews_cli import cli as suews_cli

_run_rust = import_module("supy._run_rust")

pytestmark = [
    pytest.mark.api,
    pytest.mark.physics,
    pytest.mark.contract("C-STRICT", "C-BIND", "C-ERROR", "C-RESPONSE"),
]


def _locate_suews_engine() -> Path:
    """Return the bundled or locally built ``suews-engine`` binary."""
    binary_name = "suews-engine.exe" if os.name == "nt" else "suews-engine"
    bundled = files("supy").joinpath("bin").joinpath(binary_name)
    with as_file(bundled) as bundled_path:
        if bundled_path.exists():
            return Path(bundled_path)

    cargo_target = os.environ.get("CARGO_TARGET_DIR")
    if cargo_target:
        candidate = Path(cargo_target) / "release" / binary_name
        if candidate.exists():
            return candidate

    repo_root = Path(__file__).resolve().parents[2]
    dev_binary = repo_root / "src" / "suews_bridge" / "target" / "release" / binary_name
    if dev_binary.exists():
        return dev_binary
    pytest.skip("Rust CLI binary not found; build with: make bridge")


def _write_short_forcing(dest: Path) -> Path:
    """Write 12 hourly forcing rows for fast engine and CLI checks."""
    sample_dir = files("supy").joinpath("sample_data")
    raw = sample_dir.joinpath("Kc_2012_data_60.txt").read_text(encoding="utf-8")
    lines = [line for line in raw.splitlines() if line.strip()]
    dest.write_text("\n".join(lines[:13]) + "\n", encoding="utf-8")
    return dest


def _resolved_sample_dict(
    forcing_file: Path, out_dir: Path
) -> tuple[dict, SUEWSSimulation]:
    """Build a short resolved YAML dict and matching simulation."""
    sim = SUEWSSimulation.from_sample_data()
    sim.update_config(
        {
            "model": {
                "control": {
                    "start_time": "2012-01-01 00:05:00",
                    "end_time": "2012-01-01 12:00:00",
                    "forcing": {"file": str(forcing_file)},
                    "output": {"dir": str(out_dir), "freq": 3600},
                }
            }
        },
        auto_load_forcing=False,
    )
    sim.update_forcing(str(forcing_file))
    doc = yaml.safe_load(_run_rust.config_to_engine_yaml(sim.config))
    return doc, sim


def _delete_path(doc: dict, path: tuple[str, ...]) -> None:
    cur = doc
    for key in path[:-1]:
        cur = cur[key]
    del cur[path[-1]]


def _set_path(doc: dict, path: tuple[str, ...], value) -> None:
    cur = doc
    for key in path[:-1]:
        cur = cur[key]
    cur[path[-1]] = value


_DAYS = (
    "monday",
    "tuesday",
    "wednesday",
    "thursday",
    "friday",
    "saturday",
    "sunday",
)


def _enable_active_irrigation(doc: dict) -> None:
    """Activate modelled irrigation on all weekdays so Fortran consumes all scalars."""
    doc["model"]["physics"]["water_use"] = {"value": 0}
    irr = doc["sites"][0]["properties"]["irrigation"]
    for day in _DAYS:
        irr["daywat"][day] = 1.0
        irr["daywatper"][day] = 1.0


_SITE_REQUIRED_CASES = [
    pytest.param(
        ("properties", "conductance", "g_max"),
        "sites[gridiv=1].properties.conductance.g_max",
        False,
        id="conductance-g_max",
    ),
    pytest.param(
        ("properties", "conductance", "kmax"),
        "sites[gridiv=1].properties.conductance.kmax",
        False,
        id="conductance-kmax",
    ),
    pytest.param(
        ("properties", "conductance", "tl"),
        "sites[gridiv=1].properties.conductance.tl",
        False,
        id="conductance-tl",
    ),
    pytest.param(
        ("properties", "irrigation", "h_maintain"),
        "sites[gridiv=1].properties.irrigation.h_maintain",
        True,
        id="irrigation-h_maintain",
    ),
    pytest.param(
        ("properties", "irrigation", "faut"),
        "sites[gridiv=1].properties.irrigation.faut",
        True,
        id="irrigation-faut",
    ),
    pytest.param(
        ("properties", "irrigation", "internalwateruse_h"),
        "sites[gridiv=1].properties.irrigation.internalwateruse_h",
        True,
        id="irrigation-internalwateruse_h",
    ),
    pytest.param(
        ("properties", "land_cover", "evetr", "ie_a"),
        "sites[gridiv=1].properties.land_cover.evetr.ie_a",
        True,
        id="irrigation-evetr-ie_a",
    ),
    pytest.param(
        ("properties", "land_cover", "grass", "ie_m"),
        "sites[gridiv=1].properties.land_cover.grass.ie_m",
        True,
        id="irrigation-grass-ie_m",
    ),
    pytest.param(
        ("properties", "irrigation", "daywat", "monday"),
        "sites[gridiv=1].properties.irrigation.daywat.monday",
        True,
        id="irrigation-daywat-monday",
    ),
    pytest.param(
        ("properties", "irrigation", "daywatper", "sunday"),
        "sites[gridiv=1].properties.irrigation.daywatper.sunday",
        True,
        id="irrigation-daywatper-sunday",
    ),
]


@pytest.mark.core
@pytest.mark.parametrize(
    ("rel_path", "expected_path", "activate_irrigation"), _SITE_REQUIRED_CASES
)
@pytest.mark.parametrize("mode", ["missing", "null", "nan", "string"])
def test_native_engine_and_pyo3_reject_invalid_conductance_and_irrigation(
    tmp_path: Path,
    rel_path: tuple[str, ...],
    expected_path: str,
    activate_irrigation: bool,
    mode: str,
) -> None:
    """Both PyO3 and standalone suews-engine reject invalid active conductance/irrigation scalars."""
    engine = _locate_suews_engine()
    forcing_file = _write_short_forcing(tmp_path / "forcing.txt")
    out_dir = tmp_path / "out"
    out_dir.mkdir()
    doc, sim = _resolved_sample_dict(forcing_file, out_dir)
    mutated = deepcopy(doc)
    if activate_irrigation:
        _enable_active_irrigation(mutated)
    site = mutated["sites"][0]

    if mode == "missing":
        _delete_path(site, rel_path)
    elif mode == "null":
        _set_path(site, rel_path, {"value": None})
    elif mode == "nan":
        _set_path(site, rel_path, {"value": float("nan")})
    elif mode == "string":
        _set_path(site, rel_path, "not-a-number")

    bad_yaml = yaml.safe_dump(mutated, sort_keys=False)

    # 1. PyO3 direct execution boundary
    rust = _run_rust._load_rust_module()
    forcing_block = _run_rust._prepare_forcing_block(sim.forcing.df)
    with pytest.raises(RuntimeError) as exc_info:
        rust.run_suews(
            bad_yaml, forcing_block.ravel(order="C").tolist(), len(sim.forcing.df)
        )
    assert expected_path in str(exc_info.value), str(exc_info.value)

    # 2. Standalone suews-engine CLI boundary
    cfg_path = tmp_path / "bad_resolved.yml"
    cfg_path.write_text(bad_yaml, encoding="utf-8")
    res = subprocess.run(
        [str(engine), "run", str(cfg_path)],
        capture_output=True,
        text=True,
        timeout=60,
        check=False,
    )
    assert res.returncode != 0, (
        f"expected engine rejection for {expected_path} ({mode})"
    )
    assert expected_path in f"{res.stdout}\n{res.stderr}", (
        f"expected {expected_path} in stderr:\n{res.stderr}"
    )


@pytest.mark.core
def test_inactive_modelled_irrigation_allows_omitted_scalars_and_preserves_outputs(
    tmp_path: Path,
) -> None:
    """With water_use=0 and all daywat=0.0, omitting/nulling inactive irrigation scalars matches bit-identically."""
    forcing_file = _write_short_forcing(tmp_path / "forcing.txt")
    out_dir = tmp_path / "out"
    out_dir.mkdir()
    doc, sim = _resolved_sample_dict(forcing_file, out_dir)
    rust = _run_rust._load_rust_module()
    forcing_flat = (
        _run_rust._prepare_forcing_block(sim.forcing.df).ravel(order="C").tolist()
    )
    n_steps = len(sim.forcing.df)

    base_out = rust.run_suews(
        yaml.safe_dump(doc, sort_keys=False), forcing_flat, n_steps
    )

    omitted = deepcopy(doc)
    site = omitted["sites"][0]
    irr = site["properties"]["irrigation"]
    irr.pop("h_maintain", None)
    irr["faut"] = {"value": None}
    irr.pop("internalwateruse_h", None)
    irr.pop("daywatper", None)
    for surf in ("evetr", "dectr", "grass"):
        site["properties"]["land_cover"][surf].pop("ie_a", None)
        site["properties"]["land_cover"][surf]["ie_m"] = None

    omitted_out = rust.run_suews(
        yaml.safe_dump(omitted, sort_keys=False), forcing_flat, n_steps
    )
    assert omitted_out[2] == n_steps
    np.testing.assert_array_equal(
        np.frombuffer(omitted_out[0], dtype=np.float64),
        np.frombuffer(base_out[0], dtype=np.float64),
    )

    # Even when inactive, malformed non-finite values are rejected.
    bad = deepcopy(doc)
    bad["sites"][0]["properties"]["irrigation"]["h_maintain"] = {"value": float("nan")}
    with pytest.raises(
        RuntimeError, match=r"sites\[gridiv=1\]\.properties\.irrigation\.h_maintain"
    ):
        rust.run_suews(yaml.safe_dump(bad, sort_keys=False), forcing_flat, n_steps)


@pytest.mark.core
def test_observed_water_use_gates_modelled_irrigation_but_requires_internalwateruse_h(
    tmp_path: Path,
) -> None:
    """Under water_use=1, modelled irrigation scalars may be omitted while internalwateruse_h stays required."""
    forcing_file = _write_short_forcing(tmp_path / "forcing.txt")
    out_dir = tmp_path / "out"
    out_dir.mkdir()
    doc, sim = _resolved_sample_dict(forcing_file, out_dir)
    doc["model"]["physics"]["water_use"] = {"value": 1}
    rust = _run_rust._load_rust_module()
    forcing_flat = (
        _run_rust._prepare_forcing_block(sim.forcing.df).ravel(order="C").tolist()
    )
    n_steps = len(sim.forcing.df)

    base_out = rust.run_suews(
        yaml.safe_dump(doc, sort_keys=False), forcing_flat, n_steps
    )

    omitted = deepcopy(doc)
    site = omitted["sites"][0]
    irr = site["properties"]["irrigation"]
    for key in ("h_maintain", "faut", "daywat", "daywatper"):
        irr.pop(key, None)
    for surf in ("evetr", "dectr", "grass"):
        site["properties"]["land_cover"][surf].pop("ie_a", None)
        site["properties"]["land_cover"][surf].pop("ie_m", None)

    omitted_out = rust.run_suews(
        yaml.safe_dump(omitted, sort_keys=False), forcing_flat, n_steps
    )
    assert omitted_out[2] == n_steps
    np.testing.assert_array_equal(
        np.frombuffer(omitted_out[0], dtype=np.float64),
        np.frombuffer(base_out[0], dtype=np.float64),
    )

    missing_iwu = deepcopy(omitted)
    missing_iwu["sites"][0]["properties"]["irrigation"].pop("internalwateruse_h", None)
    with pytest.raises(
        RuntimeError,
        match=r"sites\[gridiv=1\]\.properties\.irrigation\.internalwateruse_h",
    ):
        rust.run_suews(
            yaml.safe_dump(missing_iwu, sort_keys=False), forcing_flat, n_steps
        )


@pytest.mark.core
def test_surface_conductance_selector_required_in_engine_and_cli(
    cli_runner,
    tmp_path: Path,
) -> None:
    """Missing/malformed model.physics.surface_conductance fails in engine and suews run."""
    engine = _locate_suews_engine()
    forcing_file = _write_short_forcing(tmp_path / "forcing.txt")
    out_dir = tmp_path / "out"
    out_dir.mkdir()
    doc, _ = _resolved_sample_dict(forcing_file, out_dir)

    missing = deepcopy(doc)
    del missing["model"]["physics"]["surface_conductance"]
    cfg_missing = tmp_path / "missing_gs.yml"
    cfg_missing.write_text(yaml.safe_dump(missing, sort_keys=False), encoding="utf-8")
    res = subprocess.run(
        [str(engine), "run", str(cfg_missing)],
        capture_output=True,
        text=True,
        timeout=60,
        check=False,
    )
    assert res.returncode != 0
    assert "model.physics.surface_conductance" in f"{res.stdout}\n{res.stderr}"

    # Malformed surface_conductance in Python CLI (suews run) must exit non-zero naming it.
    malformed = deepcopy(doc)
    malformed["model"]["physics"]["surface_conductance"] = "invalid-model"
    cfg_bad = tmp_path / "bad_gs_cli.yml"
    cfg_bad.write_text(yaml.safe_dump(malformed, sort_keys=False), encoding="utf-8")
    cli_res = run_cli_command(cli_runner, suews_cli, ["run", str(cfg_bad)], check=False)
    assert cli_res.returncode != 0
    assert "surface_conductance" in f"{cli_res.stdout}\n{cli_res.stderr}".lower()


@pytest.mark.core
def test_non_default_conductance_affects_simulation_outputs() -> None:
    """Non-default conductance scalars alter kernel outputs with irrigation held fixed."""
    sim_base = SUEWSSimulation.from_sample_data()
    forcing_short = sim_base.forcing.df.iloc[:144].copy()
    forcing_short["Tair"] = 25.0
    forcing_short["kdown"] = 450.0
    forcing_short["rain"] = 0.0
    sim_base.update_forcing(forcing_short)
    df_base = sim_base.run(
        start_date=forcing_short.index[0],
        end_date=forcing_short.index[-1],
        n_jobs=1,
    ).df

    sim_cond = SUEWSSimulation.from_sample_data()
    sites_cond = [site.model_dump() for site in sim_cond.config.sites]
    sites_cond[0]["properties"]["conductance"]["g_max"] = {"value": 12.0}
    sim_cond.update_config({"sites": sites_cond}, auto_load_forcing=False)
    sim_cond.update_forcing(forcing_short)
    df_cond = sim_cond.run(
        start_date=forcing_short.index[0],
        end_date=forcing_short.index[-1],
        n_jobs=1,
    ).df

    qe_base = df_base.loc[1, ("SUEWS", "QE")].to_numpy()
    qe_cond = df_cond.loc[1, ("SUEWS", "QE")].to_numpy()
    assert np.all(np.isfinite(qe_cond))
    assert np.max(np.abs(qe_cond - qe_base)) > 0.5, (
        "non-default g_max must alter simulated latent heat flux QE"
    )


@pytest.mark.core
def test_non_default_irrigation_affects_simulation_outputs() -> None:
    """Active irrigation scalars alter water-use outputs with conductance held fixed."""
    # Two full days so update_WaterUse triggers at the end of day 1 and drives
    # day-2 external/internal water-use partitioning in SUEWS_cal_WaterUse.
    uniform_24h = {str(hour): 1.0 / 24.0 for hour in range(1, 25)}

    def _run_active_irrigation(internalwateruse_h: float):
        sim = SUEWSSimulation.from_sample_data()
        forcing_2d = sim.forcing.df.iloc[:576].copy()
        forcing_2d["Tair"] = 25.0
        forcing_2d["kdown"] = 450.0
        forcing_2d["rain"] = 0.0
        sites = [site.model_dump() for site in sim.config.sites]
        props = sites[0]["properties"]
        irr = props["irrigation"]
        irr["h_maintain"] = {"value": -999.0}
        irr["faut"] = {"value": 1.0}
        irr["ie_start"] = {"value": 1.0}
        irr["ie_end"] = {"value": 366.0}
        irr["internalwateruse_h"] = {"value": internalwateruse_h}
        for day in _DAYS:
            irr["daywat"][day] = 1.0
            irr["daywatper"][day] = 1.0
        for profile_key in ("wuprofa_24hr", "wuprofm_24hr"):
            irr[profile_key]["working_day"] = dict(uniform_24h)
            irr[profile_key]["holiday"] = dict(uniform_24h)
        props["land_cover"]["evetr"]["ie_a"] = {"value": 10.0}
        for surf in ("evetr", "dectr", "grass"):
            props["land_cover"][surf]["irrigation_fraction"] = {"value": 1.0}
        sim.update_config(
            {"model": {"physics": {"water_use": {"value": 0}}}, "sites": sites},
            auto_load_forcing=False,
        )
        sim.update_forcing(forcing_2d)
        return sim.run(
            start_date=forcing_2d.index[0],
            end_date=forcing_2d.index[-1],
            n_jobs=1,
        ).df

    df_irr_ext = _run_active_irrigation(0.0)
    df_irr_split = _run_active_irrigation(0.5)

    irr_ext = df_irr_ext.loc[1, ("SUEWS", "Irr")].to_numpy()
    wuint_ext = df_irr_ext.loc[1, ("SUEWS", "WUInt")].to_numpy()
    irr_split = df_irr_split.loc[1, ("SUEWS", "Irr")].to_numpy()
    wuint_split = df_irr_split.loc[1, ("SUEWS", "WUInt")].to_numpy()

    assert np.all(np.isfinite(irr_ext)) and np.all(np.isfinite(wuint_split))
    assert float(np.sum(irr_ext)) > 0.0, (
        "active modelled irrigation must produce positive external irrigation Irr"
    )
    assert np.allclose(wuint_ext, 0.0)
    assert float(np.sum(wuint_split)) > 0.0, (
        "positive internalwateruse_h must partition water use into WUInt"
    )
    assert float(np.sum(irr_split)) < float(np.sum(irr_ext)), (
        "positive internalwateruse_h must reduce external irrigation Irr"
    )
