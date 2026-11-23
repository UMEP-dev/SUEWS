"""Native resolved-YAML guards for config switches, dims, and initial states (#2002)."""

from __future__ import annotations

from copy import deepcopy
from importlib import import_module
from importlib.resources import as_file, files
import os
from pathlib import Path
import subprocess

import pytest
import yaml

from supy import SUEWSSimulation

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
    """Write 12 hourly forcing rows for fast engine checks."""
    sample_dir = files("supy").joinpath("sample_data")
    raw = sample_dir.joinpath("Kc_2012_data_60.txt").read_text(encoding="utf-8")
    lines = [line for line in raw.splitlines() if line.strip()]
    dest.write_text("\n".join(lines[:13]) + "\n", encoding="utf-8")
    return dest


def _resolved_doc(forcing_file: Path, out_dir: Path) -> tuple[dict, SUEWSSimulation]:
    """Build a resolved engine YAML document with short forcing."""
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


@pytest.mark.core
@pytest.mark.parametrize(
    ("mutate", "expected_path"),
    [
        pytest.param(
            lambda d: d["model"]["physics"].pop("net_radiation"),
            "model.physics.net_radiation",
            id="missing-net_radiation",
        ),
        pytest.param(
            lambda d: d["model"]["physics"].pop("evapmethod"),
            "model.physics.evapmethod",
            id="missing-evapmethod",
        ),
        pytest.param(
            lambda d: d["model"]["control"].pop("tstep"),
            "model.control.tstep",
            id="missing-tstep",
        ),
        pytest.param(
            lambda d: d["sites"][0]["properties"]["vertical_layers"].pop("nlayer"),
            "sites[gridiv=1].properties.vertical_layers.nlayer",
            id="missing-nlayer",
        ),
        pytest.param(
            lambda d: d["sites"][0]["properties"]["vertical_layers"].pop("ndepth"),
            "sites[gridiv=1].properties.vertical_layers.ndepth",
            id="missing-ndepth",
        ),
        pytest.param(
            lambda d: d["sites"][0]["initial_states"]["paved"].pop("soilstore"),
            "sites[gridiv=1].initial_states.paved.soilstore",
            id="missing-paved-soilstore",
        ),
        pytest.param(
            lambda d: d["sites"][0]["initial_states"].pop("hdd_id"),
            "sites[gridiv=1].initial_states.hdd_id",
            id="missing-hdd_id",
        ),
        pytest.param(
            lambda d: d["sites"][0]["initial_states"]["evetr"].pop("lai_id"),
            "sites[gridiv=1].initial_states.evetr.lai_id",
            id="missing-evetr-lai_id",
        ),
        pytest.param(
            lambda d: d["sites"][0]["initial_states"].__setitem__(
                "snowalb", float("nan")
            ),
            "sites[gridiv=1].initial_states.snowalb",
            id="nan-snowalb",
        ),
    ],
)
def test_resolved_config_dims_and_states_reject_missing_or_invalid_inputs(
    tmp_path: Path,
    mutate,
    expected_path: str,
) -> None:
    """Both PyO3 and suews-engine reject missing or malformed config/dim/state inputs."""
    engine = _locate_suews_engine()
    forcing_file = _write_short_forcing(tmp_path / "forcing.txt")
    out_dir = tmp_path / "out"
    out_dir.mkdir()
    doc, sim = _resolved_doc(forcing_file, out_dir)
    mutated = deepcopy(doc)
    mutate(mutated)
    bad_yaml = yaml.safe_dump(mutated, sort_keys=False)

    rust = _run_rust._load_rust_module()
    forcing_block = _run_rust._prepare_forcing_block(sim.forcing.df)
    with pytest.raises(RuntimeError) as exc_info:
        rust.run_suews(
            bad_yaml,
            forcing_block.ravel(order="C").tolist(),
            len(sim.forcing.df),
        )
    assert expected_path in str(exc_info.value), str(exc_info.value)

    cfg_path = tmp_path / "bad_state_config.yml"
    cfg_path.write_text(bad_yaml, encoding="utf-8")
    res = subprocess.run(
        [str(engine), "run", str(cfg_path)],
        capture_output=True,
        text=True,
        timeout=60,
        check=False,
    )
    assert res.returncode != 0
    assert expected_path in f"{res.stdout}\n{res.stderr}", res.stderr


@pytest.mark.core
def test_inactive_snow_and_spartacus_permit_omitted_optional_state_and_switch(
    tmp_path: Path,
) -> None:
    """With snow_use=0 and net_radiation=3, omitting inactive snowpack/use_sw_direct_albedo succeeds."""
    engine = _locate_suews_engine()
    forcing_file = _write_short_forcing(tmp_path / "forcing.txt")
    out_dir = tmp_path / "out"
    out_dir.mkdir()
    doc, _ = _resolved_doc(forcing_file, out_dir)
    doc["sites"][0]["properties"]["spartacus"].pop("use_sw_direct_albedo", None)
    doc["sites"][0]["initial_states"]["paved"].pop("snowpack", None)
    cfg_path = tmp_path / "inactive_ok.yml"
    cfg_path.write_text(yaml.safe_dump(doc, sort_keys=False), encoding="utf-8")

    res = subprocess.run(
        [str(engine), "run", str(cfg_path)],
        capture_output=True,
        text=True,
        timeout=60,
        check=False,
    )
    assert res.returncode == 0, f"{res.stdout}\n{res.stderr}"
