"""Native resolved-YAML guards for SPARTACUS, vertical_layers, and EHC (#2002, #2349)."""

from __future__ import annotations

from copy import deepcopy
from importlib import import_module
from importlib.resources import files
from pathlib import Path
import subprocess

import pytest
import yaml

from supy import SUEWSSimulation
from supy.data_model import SUEWSConfig

_run_rust = import_module("supy._run_rust")
_YAML_LOADER = getattr(yaml, "CSafeLoader", yaml.SafeLoader)
_YAML_DUMPER = getattr(yaml, "CSafeDumper", yaml.SafeDumper)

pytestmark = [
    pytest.mark.api,
    pytest.mark.physics,
    pytest.mark.contract("C-STRICT", "C-BIND", "C-ERROR", "C-RESPONSE"),
]


def _dump_yaml(doc: dict) -> str:
    return yaml.dump(doc, Dumper=_YAML_DUMPER, sort_keys=False)


def _load_yaml(text: str) -> dict:
    return yaml.load(text, Loader=_YAML_LOADER)


@pytest.fixture(scope="module")
def _spartacus_ehc_base(sample_config_loaded, sample_forcing_parsed):
    """Cache SPARTACUS+EHC resolved fixture, sample inactive config, and short forcing."""
    raw_forcing = (files("supy") / "sample_data" / "Kc_2012_data_60.txt").read_text(
        encoding="utf-8"
    )
    forcing_lines = [line for line in raw_forcing.splitlines() if line.strip()]
    short_forcing_text = "\n".join(forcing_lines[:13]) + "\n"

    repo_root = Path(__file__).resolve().parents[2]
    fixture = repo_root / "test" / "fixtures" / "bridge" / "stebbs_resolved.yml"
    stebbs_doc = _load_yaml(fixture.read_text(encoding="utf-8"))
    stebbs_doc["model"]["physics"]["storage_heat"] = {"value": 5}
    stebbs_doc["model"]["control"]["start_time"] = "2012-01-01 00:05:00"
    stebbs_doc["model"]["control"]["end_time"] = "2012-01-01 12:00:00"
    stebbs_doc["model"]["control"]["output"]["freq"] = 3600
    stebbs_doc["model"]["control"]["output"]["include_provisional"] = True
    # Establish positive baseline that the unmutated SPARTACUS+EHC doc passes SUEWSConfig.
    SUEWSConfig.from_dict(deepcopy(stebbs_doc))

    inactive_cfg = sample_config_loaded.model_copy(deep=True)
    inactive_cfg.model.physics.net_radiation = 3
    inactive_cfg.model.physics.storage_heat = 1
    inactive_cfg.model.control.start_time = "2012-01-01 00:05:00"
    inactive_cfg.model.control.end_time = "2012-01-01 12:00:00"
    inactive_cfg.model.control.output.freq = 3600
    inactive_doc = _load_yaml(_run_rust.config_to_engine_yaml(inactive_cfg))

    _, df_forcing = sample_forcing_parsed(144)
    forcing_flat = (
        _run_rust._prepare_forcing_block(df_forcing).ravel(order="C").tolist()
    )
    return {
        "stebbs_doc": stebbs_doc,
        "inactive_doc": inactive_doc,
        "short_forcing_text": short_forcing_text,
        "df_forcing": df_forcing,
        "forcing_flat": forcing_flat,
        "n_steps": len(df_forcing),
    }


def _stage_doc(
    doc_template: dict, short_forcing_text: str, tmp_path: Path
) -> tuple[dict, Path]:
    """Write short forcing file and return a defensive copy of ``doc_template``."""
    forcing_file = tmp_path / "forcing.txt"
    forcing_file.write_text(short_forcing_text, encoding="utf-8")
    out_dir = tmp_path / "out"
    out_dir.mkdir(exist_ok=True)
    doc = deepcopy(doc_template)
    doc["model"]["control"]["forcing"]["file"] = str(forcing_file)
    doc["model"]["control"]["output"]["dir"] = str(out_dir)
    return doc, out_dir


_WITNESS_CASES = [
    # SPARTACUS subsystem: missing, null, malformed, incorrectly sized
    pytest.param(
        lambda s: s["properties"]["spartacus"].pop("air_ext_sw"),
        ("sites[gridiv=1].properties.spartacus.air_ext_sw",),
        None,
        id="spartacus-missing-air_ext_sw",
    ),
    pytest.param(
        lambda s: s["properties"]["spartacus"].__setitem__(
            "air_ext_sw", {"value": None}
        ),
        ("sites[gridiv=1].properties.spartacus.air_ext_sw",),
        ("air_ext_sw",),
        id="spartacus-null-air_ext_sw",
    ),
    pytest.param(
        lambda s: s["properties"]["spartacus"].__setitem__("n_stream_sw_urban", 1.5),
        ("sites[gridiv=1].properties.spartacus.n_stream_sw_urban",),
        ("n_stream_sw_urban",),
        id="spartacus-malformed-n_stream_sw_urban",
    ),
    pytest.param(
        lambda s: s["properties"]["vertical_layers"].__setitem__(
            "building_frac", {"value": [0.4, 0.2]}
        ),
        (
            "sites[gridiv=1].properties.vertical_layers.building_frac",
            "has 2 values",
            "requires at least 3",
        ),
        ("Building fractions array length (2)", "nlayer (3)"),
        id="spartacus-wrong-size-building_frac",
    ),
    # EHC subsystem: missing, null, malformed, incorrectly sized.
    pytest.param(
        lambda s: s["properties"]["vertical_layers"]["walls"][0]["thermal_layers"].pop(
            "k"
        ),
        ("sites[gridiv=1].properties.vertical_layers.walls[0].thermal_layers.k",),
        ("sites[gridiv=1].properties.vertical_layers.walls[0].thermal_layers.k",),
        id="ehc-missing-wall-thermal-k",
    ),
    pytest.param(
        lambda s: s["properties"]["vertical_layers"]["walls"][0][
            "thermal_layers"
        ].__setitem__("k", {"value": [None, 1.0, 1.0, 1.0, 1.0]}),
        (
            "sites[gridiv=1].properties.vertical_layers.walls[0].thermal_layers.k",
            "element [0]",
        ),
        (
            "sites[gridiv=1].properties.vertical_layers.walls[0].thermal_layers.k",
            "element [0]",
        ),
        id="ehc-null-wall-thermal-k-elem",
    ),
    pytest.param(
        lambda s: s["properties"]["vertical_layers"]["walls"][0][
            "thermal_layers"
        ].__setitem__("k", {"value": ["bad", 1.0, 1.0, 1.0, 1.0]}),
        (
            "sites[gridiv=1].properties.vertical_layers.walls[0].thermal_layers.k",
            "element [0]",
        ),
        ("walls.0.thermal_layers.k",),
        id="ehc-malformed-wall-thermal-k-elem",
    ),
    pytest.param(
        lambda s: s["properties"]["vertical_layers"].__setitem__(
            "walls", s["properties"]["vertical_layers"]["walls"][:1]
        ),
        (
            "sites[gridiv=1].properties.vertical_layers.walls",
            "has 1 values",
            "requires at least 3",
        ),
        ("Wall layers count (1)", "nlayer (3)"),
        id="ehc-wrong-size-walls",
    ),
]


@pytest.mark.core
@pytest.mark.parametrize(
    ("mutate", "expected_needles", "public_needles"), _WITNESS_CASES
)
def test_active_spartacus_and_ehc_br02_failure_witnesses(
    tmp_path: Path,
    suews_engine_bin: Path,
    _spartacus_ehc_base: dict,
    mutate,
    expected_needles: tuple[str, ...],
    public_needles: tuple[str, ...] | None,
) -> None:
    """BR-02 witnesses for SPARTACUS and EHC across Python and suews-engine."""
    doc, out_dir = _stage_doc(
        _spartacus_ehc_base["stebbs_doc"],
        _spartacus_ehc_base["short_forcing_text"],
        tmp_path,
    )
    mutate(doc["sites"][0])
    bad_yaml = _dump_yaml(doc)

    # 1. PyO3 Python-bridge witness
    rust = _run_rust._load_rust_module()
    with pytest.raises(RuntimeError) as exc_info:
        rust.run_suews(
            bad_yaml,
            _spartacus_ehc_base["forcing_flat"],
            _spartacus_ehc_base["n_steps"],
        )
    pyo3_msg = str(exc_info.value)
    for needle in expected_needles:
        assert needle in pyo3_msg, f"expected {needle!r} in {pyo3_msg!r}"

    # 2. Standalone suews-engine witness
    cfg_path = tmp_path / "bad_spartacus_ehc.yml"
    cfg_path.write_text(bad_yaml, encoding="utf-8")
    res = subprocess.run(
        [str(suews_engine_bin), "run", str(cfg_path)],
        capture_output=True,
        text=True,
        timeout=60,
        check=False,
    )
    assert res.returncode != 0
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
        df_forcing = _spartacus_ehc_base["df_forcing"]
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
def test_inactive_spartacus_and_ehc_permit_omitted_optional_blocks(
    tmp_path: Path,
    suews_engine_bin: Path,
    _spartacus_ehc_base: dict,
) -> None:
    """With net_radiation=3 and storage_heat=1, omitting spartacus/veg_ext succeeds (BR-03)."""
    doc, _ = _stage_doc(
        _spartacus_ehc_base["inactive_doc"],
        _spartacus_ehc_base["short_forcing_text"],
        tmp_path,
    )
    doc["sites"][0]["properties"].pop("spartacus", None)
    doc["sites"][0]["properties"]["vertical_layers"].pop("veg_ext", None)
    cfg_path = tmp_path / "inactive_ok.yml"
    cfg_path.write_text(_dump_yaml(doc), encoding="utf-8")

    res = subprocess.run(
        [str(suews_engine_bin), "run", str(cfg_path)],
        capture_output=True,
        text=True,
        timeout=60,
        check=False,
    )
    assert res.returncode == 0, f"inactive SPARTACUS/EHC omission failed: {res.stderr}"
