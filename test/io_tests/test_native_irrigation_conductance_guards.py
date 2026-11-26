"""Native resolved-YAML guards for conductance and irrigation parameters (#2002, #2349)."""

from __future__ import annotations

from copy import deepcopy
from importlib import import_module
from importlib.resources import files
from pathlib import Path
import subprocess

from conftest import run_cli_command
import numpy as np
import pytest
import yaml

from supy import SUEWSSimulation
from supy.cmd.suews_cli import cli as suews_cli
from supy.data_model import SUEWSConfig

_run_rust = import_module("supy._run_rust")
_YAML_LOADER = getattr(yaml, "CSafeLoader", yaml.SafeLoader)
_YAML_DUMPER = getattr(yaml, "CSafeDumper", yaml.SafeDumper)

pytestmark = [
    pytest.mark.api,
    pytest.mark.physics,
    pytest.mark.contract("C-STRICT", "C-BIND", "C-ERROR", "C-RESPONSE"),
]

_DAYS = (
    "monday",
    "tuesday",
    "wednesday",
    "thursday",
    "friday",
    "saturday",
    "sunday",
)


def _dump_yaml(doc: dict) -> str:
    return yaml.dump(doc, Dumper=_YAML_DUMPER, sort_keys=False)


def _load_yaml(text: str) -> dict:
    return yaml.load(text, Loader=_YAML_LOADER)


@pytest.fixture(scope="module")
def _conductance_irrigation_base(sample_config_loaded, sample_forcing_parsed):
    """Cache base resolved YAML, short forcing text, and 144-step PyO3 forcing payload."""
    raw_forcing = (files("supy") / "sample_data" / "Kc_2012_data_60.txt").read_text(
        encoding="utf-8"
    )
    forcing_lines = [line for line in raw_forcing.splitlines() if line.strip()]
    short_forcing_text = "\n".join(forcing_lines[:13]) + "\n"

    cfg = sample_config_loaded.model_copy(deep=True)
    cfg.model.control.start_time = "2012-01-01 00:05:00"
    cfg.model.control.end_time = "2012-01-01 12:00:00"
    cfg.model.control.output.freq = 3600
    resolved_doc = _load_yaml(_run_rust.config_to_engine_yaml(cfg))
    # Establish positive baseline that the unmutated resolved dict passes SUEWSConfig.
    SUEWSConfig.from_dict(deepcopy(resolved_doc))

    _, df_forcing_12h = sample_forcing_parsed(144)
    forcing_flat_12h = (
        _run_rust._prepare_forcing_block(df_forcing_12h).ravel(order="C").tolist()
    )
    return {
        "resolved_doc": resolved_doc,
        "short_forcing_text": short_forcing_text,
        "df_forcing_12h": df_forcing_12h,
        "forcing_flat_12h": forcing_flat_12h,
        "n_steps_12h": len(df_forcing_12h),
    }


def _stage_doc(base_cache: dict, tmp_path: Path) -> tuple[dict, Path]:
    """Write short forcing file and return a defensive copy of the resolved dict."""
    forcing_file = tmp_path / "forcing.txt"
    forcing_file.write_text(base_cache["short_forcing_text"], encoding="utf-8")
    out_dir = tmp_path / "out"
    out_dir.mkdir(exist_ok=True)
    doc = deepcopy(base_cache["resolved_doc"])
    doc["model"]["control"]["forcing"]["file"] = str(forcing_file)
    doc["model"]["control"]["output"]["dir"] = str(out_dir)
    return doc, out_dir


def _enable_active_irrigation(doc: dict) -> None:
    """Activate modelled irrigation on all weekdays so Fortran consumes all scalars."""
    doc["model"]["physics"]["water_use"] = {"value": 0}
    irr = doc["sites"][0]["properties"]["irrigation"]
    for day in _DAYS:
        irr["daywat"][day] = 1.0
        irr["daywatper"][day] = 1.0


def _drop_hour_24(profile_day: dict) -> None:
    profile_day.pop("24", None)
    profile_day.pop(24, None)


_WITNESS_CASES = [
    # Conductance subsystem: missing (defaulted in SUEWSConfig per BR-01/C-EMIT), null, malformed
    pytest.param(
        False,
        lambda s: s["properties"]["conductance"].pop("g_max"),
        ("sites[gridiv=1].properties.conductance.g_max",),
        None,
        id="conductance-missing-g_max",
    ),
    pytest.param(
        False,
        lambda s: s["properties"]["conductance"].__setitem__("g_max", {"value": None}),
        ("sites[gridiv=1].properties.conductance.g_max",),
        ("conductance.g_max",),
        id="conductance-null-g_max",
    ),
    pytest.param(
        False,
        lambda s: s["properties"]["conductance"].__setitem__("g_max", "not-a-number"),
        ("sites[gridiv=1].properties.conductance.g_max",),
        ("conductance.g_max",),
        id="conductance-malformed-g_max",
    ),
    # Irrigation subsystem: missing (defaulted in SUEWSConfig per BR-01/C-EMIT), null,
    # malformed, incorrectly sized (24-hour profile missing hour 24).
    pytest.param(
        True,
        lambda s: s["properties"]["irrigation"].pop("h_maintain"),
        ("sites[gridiv=1].properties.irrigation.h_maintain",),
        None,
        id="irrigation-missing-h_maintain",
    ),
    pytest.param(
        True,
        lambda s: s["properties"]["irrigation"].__setitem__(
            "h_maintain", {"value": None}
        ),
        ("sites[gridiv=1].properties.irrigation.h_maintain",),
        ("sites[gridiv=1].properties.irrigation.h_maintain",),
        id="irrigation-null-h_maintain",
    ),
    pytest.param(
        True,
        lambda s: s["properties"]["irrigation"].__setitem__(
            "h_maintain", {"value": float("nan")}
        ),
        ("sites[gridiv=1].properties.irrigation.h_maintain",),
        ("sites[gridiv=1].properties.irrigation.h_maintain",),
        id="irrigation-malformed-nan-h_maintain",
    ),
    pytest.param(
        True,
        lambda s: _drop_hour_24(
            s["properties"]["irrigation"]["wuprofa_24hr"]["working_day"]
        ),
        (
            "sites[gridiv=1].properties.irrigation.wuprofa_24hr.working_day",
            "missing 1 hour(s): 24",
        ),
        ("wuprofa_24hr.working_day", "missing hours: [24]"),
        id="irrigation-wrong-size-wuprofa_24hr",
    ),
]


@pytest.mark.core
@pytest.mark.parametrize(
    ("activate_irrigation", "mutate_site", "expected_needles", "public_needles"),
    _WITNESS_CASES,
)
def test_conductance_and_irrigation_br02_failure_witnesses(
    tmp_path: Path,
    suews_engine_bin: Path,
    _conductance_irrigation_base: dict,
    activate_irrigation: bool,
    mutate_site,
    expected_needles: tuple[str, ...],
    public_needles: tuple[str, ...] | None,
) -> None:
    """BR-02 witnesses for conductance and irrigation across Python and suews-engine."""
    doc, out_dir = _stage_doc(_conductance_irrigation_base, tmp_path)
    if activate_irrigation:
        _enable_active_irrigation(doc)
    mutate_site(doc["sites"][0])
    bad_yaml = _dump_yaml(doc)

    # 1. PyO3 direct execution boundary
    rust = _run_rust._load_rust_module()
    with pytest.raises(RuntimeError) as exc_info:
        rust.run_suews(
            bad_yaml,
            _conductance_irrigation_base["forcing_flat_12h"],
            _conductance_irrigation_base["n_steps_12h"],
        )
    pyo3_msg = str(exc_info.value)
    for needle in expected_needles:
        assert needle in pyo3_msg, f"expected {needle!r} in {pyo3_msg!r}"

    # 2. Standalone suews-engine CLI boundary
    cfg_path = tmp_path / "bad_resolved.yml"
    cfg_path.write_text(bad_yaml, encoding="utf-8")
    res = subprocess.run(
        [str(suews_engine_bin), "run", str(cfg_path)],
        capture_output=True,
        text=True,
        timeout=60,
        check=False,
    )
    assert res.returncode != 0, f"expected engine rejection for {expected_needles[0]}"
    combined = f"{res.stdout}\n{res.stderr}"
    for needle in expected_needles:
        assert needle in combined, f"expected {needle!r} in {combined!r}"
    assert not (out_dir / "suews_output.arrow").exists()

    # 3. Public Python entry point witness (SUEWSConfig parse or SUEWSSimulation.run)
    if public_needles is not None:
        try:
            cfg = SUEWSConfig.from_dict(doc)
        except (ValueError, TypeError) as cfg_exc:
            msg = str(cfg_exc)
            for needle in public_needles:
                assert needle in msg, f"expected {needle!r} in {msg!r}"
            return
        df_forcing = _conductance_irrigation_base["df_forcing_12h"]
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


@pytest.mark.core
def test_inactive_modelled_irrigation_allows_omitted_scalars_and_preserves_outputs(
    tmp_path: Path,
    _conductance_irrigation_base: dict,
) -> None:
    """With water_use=0 and all daywat=0.0, omitting/nulling inactive irrigation scalars matches bit-identically."""
    doc, _ = _stage_doc(_conductance_irrigation_base, tmp_path)
    rust = _run_rust._load_rust_module()
    forcing_flat = _conductance_irrigation_base["forcing_flat_12h"]
    n_steps = _conductance_irrigation_base["n_steps_12h"]

    base_out = rust.run_suews(_dump_yaml(doc), forcing_flat, n_steps)

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

    omitted_out = rust.run_suews(_dump_yaml(omitted), forcing_flat, n_steps)
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
        rust.run_suews(_dump_yaml(bad), forcing_flat, n_steps)


@pytest.mark.core
def test_observed_water_use_gates_modelled_irrigation_but_requires_internalwateruse_h(
    tmp_path: Path,
    _conductance_irrigation_base: dict,
) -> None:
    """Under water_use=1, modelled irrigation scalars may be omitted while internalwateruse_h stays required."""
    doc, _ = _stage_doc(_conductance_irrigation_base, tmp_path)
    doc["model"]["physics"]["water_use"] = {"value": 1}
    rust = _run_rust._load_rust_module()
    forcing_flat = _conductance_irrigation_base["forcing_flat_12h"]
    n_steps = _conductance_irrigation_base["n_steps_12h"]

    base_out = rust.run_suews(_dump_yaml(doc), forcing_flat, n_steps)

    omitted = deepcopy(doc)
    site = omitted["sites"][0]
    irr = site["properties"]["irrigation"]
    for key in ("h_maintain", "faut", "daywat", "daywatper"):
        irr.pop(key, None)
    for surf in ("evetr", "dectr", "grass"):
        site["properties"]["land_cover"][surf].pop("ie_a", None)
        site["properties"]["land_cover"][surf].pop("ie_m", None)

    omitted_out = rust.run_suews(_dump_yaml(omitted), forcing_flat, n_steps)
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
        rust.run_suews(_dump_yaml(missing_iwu), forcing_flat, n_steps)


@pytest.mark.core
def test_surface_conductance_selector_required_in_engine_and_cli(
    cli_runner,
    tmp_path: Path,
    suews_engine_bin: Path,
    _conductance_irrigation_base: dict,
) -> None:
    """Missing/malformed model.physics.surface_conductance fails in engine and suews run."""
    doc, _ = _stage_doc(_conductance_irrigation_base, tmp_path)

    missing = deepcopy(doc)
    del missing["model"]["physics"]["surface_conductance"]
    cfg_missing = tmp_path / "missing_gs.yml"
    cfg_missing.write_text(_dump_yaml(missing), encoding="utf-8")
    res = subprocess.run(
        [str(suews_engine_bin), "run", str(cfg_missing)],
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
    cfg_bad.write_text(_dump_yaml(malformed), encoding="utf-8")
    cli_res = run_cli_command(cli_runner, suews_cli, ["run", str(cfg_bad)], check=False)
    assert cli_res.returncode != 0
    assert "surface_conductance" in f"{cli_res.stdout}\n{cli_res.stderr}".lower()


@pytest.mark.core
def test_non_default_conductance_affects_simulation_outputs(
    sample_config_loaded, sample_forcing_parsed
) -> None:
    """Non-default conductance scalars alter kernel outputs with irrigation held fixed."""
    _, forcing_short = sample_forcing_parsed(144)
    forcing_short["Tair"] = 25.0
    forcing_short["kdown"] = 450.0
    forcing_short["rain"] = 0.0

    sim_base = SUEWSSimulation()
    sim_base.update_config(
        sample_config_loaded.model_copy(deep=True), auto_load_forcing=False
    )
    sim_base.update_forcing(forcing_short)
    df_base = sim_base.run(
        start_date=forcing_short.index[0],
        end_date=forcing_short.index[-1],
        n_jobs=1,
    ).df

    cfg_cond = sample_config_loaded.model_copy(deep=True)
    cfg_cond.sites[0].properties.conductance.g_max.value = 12.0
    sim_cond = SUEWSSimulation()
    sim_cond.update_config(cfg_cond, auto_load_forcing=False)
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
def test_non_default_irrigation_affects_simulation_outputs(
    sample_config_loaded, sample_forcing_parsed
) -> None:
    """Active irrigation scalars alter water-use outputs with conductance held fixed."""
    # Two full days so update_WaterUse triggers at the end of day 1 and drives
    # day-2 external/internal water-use partitioning in SUEWS_cal_WaterUse.
    uniform_24h = {str(hour): 1.0 / 24.0 for hour in range(1, 25)}
    _, forcing_2d = sample_forcing_parsed(576)
    forcing_2d["Tair"] = 25.0
    forcing_2d["kdown"] = 450.0
    forcing_2d["rain"] = 0.0
    base_sites = [site.model_dump() for site in sample_config_loaded.sites]

    def _run_active_irrigation(internalwateruse_h: float):
        sim = SUEWSSimulation()
        sim.update_config(
            sample_config_loaded.model_copy(deep=True), auto_load_forcing=False
        )
        sites = deepcopy(base_sites)
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
