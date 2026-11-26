"""Native resolved-YAML guards for SPARTACUS, vertical_layers, and EHC (#2002)."""

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


def _stebbs_resolved_doc(
    forcing_file: Path, out_dir: Path
) -> tuple[dict, SUEWSSimulation]:
    """Load the SPARTACUS + EHC resolved fixture and attach short forcing."""
    repo_root = Path(__file__).resolve().parents[2]
    fixture = repo_root / "test" / "fixtures" / "bridge" / "stebbs_resolved.yml"
    doc = yaml.safe_load(fixture.read_text(encoding="utf-8"))
    doc["model"]["physics"]["storage_heat"] = {"value": 5}
    # EHC uses J11 QF rather than the coupled STEBBS source.
    doc["model"]["physics"]["emissions"] = {"value": 2}
    doc["model"]["control"]["start_time"] = "2012-01-01 00:05:00"
    doc["model"]["control"]["end_time"] = "2012-01-01 12:00:00"
    doc["model"]["control"]["forcing"]["file"] = str(forcing_file)
    doc["model"]["control"]["output"]["dir"] = str(out_dir)
    doc["model"]["control"]["output"]["freq"] = 3600

    sim = SUEWSSimulation.from_sample_data()
    sim.update_forcing(str(forcing_file))
    return doc, sim


@pytest.mark.core
@pytest.mark.parametrize(
    ("mutate", "expected_path"),
    [
        pytest.param(
            lambda s: s["properties"]["spartacus"].pop("air_ext_sw"),
            "sites[gridiv=1].properties.spartacus.air_ext_sw",
            id="missing-spartacus-air_ext_sw",
        ),
        pytest.param(
            lambda s: s["properties"]["spartacus"].__setitem__(
                "n_stream_sw_urban", 1.5
            ),
            "sites[gridiv=1].properties.spartacus.n_stream_sw_urban",
            id="fractional-spartacus-n_stream_sw_urban",
        ),
        pytest.param(
            lambda s: s["properties"]["vertical_layers"].pop("height"),
            "sites[gridiv=1].properties.vertical_layers.height",
            id="missing-vertical_layers-height",
        ),
        pytest.param(
            lambda s: s["properties"]["vertical_layers"].pop("veg_frac"),
            "sites[gridiv=1].properties.vertical_layers.veg_frac",
            id="missing-vertical_layers-veg_frac",
        ),
        pytest.param(
            lambda s: s["properties"]["vertical_layers"]["roofs"][0].pop("alb"),
            "sites[gridiv=1].properties.vertical_layers.roofs[0].alb",
            id="missing-roof-alb",
        ),
        pytest.param(
            lambda s: s["properties"]["vertical_layers"]["walls"][0][
                "thermal_layers"
            ].pop("k"),
            "sites[gridiv=1].properties.vertical_layers.walls[0].thermal_layers.k",
            id="missing-wall-thermal-k",
        ),
        pytest.param(
            lambda s: s["properties"]["vertical_layers"]["roofs"][0].pop(
                "soil_store_capacity"
            ),
            "sites[gridiv=1].properties.vertical_layers.roofs[0].soil_store_capacity",
            id="missing-roof-soil_store_capacity",
        ),
    ],
)
def test_active_spartacus_and_ehc_reject_missing_or_invalid_inputs(
    tmp_path: Path,
    mutate,
    expected_path: str,
) -> None:
    """Both PyO3 and suews-engine reject missing/malformed active SPARTACUS/EHC fields."""
    engine = _locate_suews_engine()
    forcing_file = _write_short_forcing(tmp_path / "forcing.txt")
    out_dir = tmp_path / "out"
    out_dir.mkdir()
    doc, sim = _stebbs_resolved_doc(forcing_file, out_dir)
    mutated = deepcopy(doc)
    mutate(mutated["sites"][0])
    bad_yaml = yaml.safe_dump(mutated, sort_keys=False)

    rust = _run_rust._load_rust_module()
    forcing_block = _run_rust._prepare_forcing_block(sim.forcing.df)
    with pytest.raises(RuntimeError) as exc_info:
        rust.run_suews(
            bad_yaml, forcing_block.ravel(order="C").tolist(), len(sim.forcing.df)
        )
    assert expected_path in str(exc_info.value), str(exc_info.value)

    cfg_path = tmp_path / "bad_spartacus_ehc.yml"
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
def test_inactive_spartacus_and_ehc_permit_omitted_optional_blocks(
    tmp_path: Path,
) -> None:
    """With net_radiation=3 and storage_heat=1, omitting spartacus/veg_ext succeeds (BR-03)."""
    engine = _locate_suews_engine()
    forcing_file = _write_short_forcing(tmp_path / "forcing.txt")
    out_dir = tmp_path / "out"
    out_dir.mkdir()
    sim = SUEWSSimulation.from_sample_data()
    sim.update_config(
        {
            "model": {
                "physics": {"net_radiation": 3, "storage_heat": 1},
                "control": {
                    "start_time": "2012-01-01 00:05:00",
                    "end_time": "2012-01-01 12:00:00",
                    "forcing": {"file": str(forcing_file)},
                    "output": {"dir": str(out_dir), "freq": 3600},
                },
            }
        },
        auto_load_forcing=False,
    )
    doc = yaml.safe_load(_run_rust.config_to_engine_yaml(sim.config))
    doc["sites"][0]["properties"].pop("spartacus", None)
    doc["sites"][0]["properties"]["vertical_layers"].pop("veg_ext", None)
    cfg_path = tmp_path / "inactive_ok.yml"
    cfg_path.write_text(yaml.safe_dump(doc, sort_keys=False), encoding="utf-8")

    res = subprocess.run(
        [str(engine), "run", str(cfg_path)],
        capture_output=True,
        text=True,
        timeout=60,
        check=False,
    )
    assert res.returncode == 0, f"inactive SPARTACUS/EHC omission failed: {res.stderr}"
