"""Native resolved-YAML guards for config switches, dims, and initial states (#2002, #2349)."""

from __future__ import annotations

from copy import deepcopy
from importlib import import_module
from importlib.resources import files
from pathlib import Path
import subprocess

import pytest
import yaml

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
def _state_config_base(sample_config_loaded, sample_forcing_parsed):
    """Cache base resolved YAML, short forcing text, and PyO3 forcing payload."""
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

    _, df_forcing = sample_forcing_parsed(144)
    forcing_flat = (
        _run_rust._prepare_forcing_block(df_forcing).ravel(order="C").tolist()
    )
    return {
        "resolved_doc": resolved_doc,
        "short_forcing_text": short_forcing_text,
        "forcing_flat": forcing_flat,
        "n_steps": len(df_forcing),
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


_WITNESS_CASES = [
    # Model config switches and structural dimensions: missing, null, malformed
    pytest.param(
        lambda d: d["model"]["physics"].pop("net_radiation"),
        "model.physics.net_radiation",
        False,
        id="config-missing-net_radiation",
    ),
    pytest.param(
        lambda d: d["model"]["control"].__setitem__("tstep", {"value": None}),
        "model.control.tstep",
        True,
        id="config-null-tstep",
    ),
    pytest.param(
        lambda d: d["sites"][0]["properties"]["vertical_layers"].__setitem__(
            "nlayer", {"value": 1.5}
        ),
        "sites[gridiv=1].properties.vertical_layers.nlayer",
        True,
        id="dims-malformed-nlayer",
    ),
    # Initial states subsystem: missing, null, malformed, incorrectly sized
    pytest.param(
        lambda d: d["sites"][0]["initial_states"]["paved"].pop("soilstore"),
        "sites[gridiv=1].initial_states.paved.soilstore",
        False,
        id="state-missing-paved-soilstore",
    ),
    pytest.param(
        lambda d: d["sites"][0]["initial_states"]["paved"].__setitem__(
            "soilstore", {"value": None}
        ),
        "sites[gridiv=1].initial_states.paved.soilstore",
        True,
        id="state-null-paved-soilstore",
    ),
    pytest.param(
        lambda d: d["sites"][0]["initial_states"].__setitem__("snowalb", float("nan")),
        "sites[gridiv=1].initial_states.snowalb",
        True,
        id="state-malformed-nan-snowalb",
    ),
    pytest.param(
        lambda d: d["sites"][0]["initial_states"].__setitem__(
            "hdd_id", {"value": [0.0, 0.0]}
        ),
        "sites[gridiv=1].initial_states.hdd_id",
        True,
        id="state-wrong-size-hdd_id",
    ),
]


@pytest.mark.core
@pytest.mark.parametrize(
    ("mutate", "expected_path", "check_public_config"), _WITNESS_CASES
)
def test_resolved_config_dims_and_states_br02_failure_witnesses(
    tmp_path: Path,
    suews_engine_bin: Path,
    _state_config_base: dict,
    mutate,
    expected_path: str,
    check_public_config: bool,
) -> None:
    """BR-02 witnesses for config switches, dims, and initial states across Python and suews-engine."""
    doc, out_dir = _stage_doc(_state_config_base, tmp_path)
    mutate(doc)
    bad_yaml = _dump_yaml(doc)

    # 1. PyO3 Python-bridge witness
    rust = _run_rust._load_rust_module()
    with pytest.raises(RuntimeError) as exc_info:
        rust.run_suews(
            bad_yaml,
            _state_config_base["forcing_flat"],
            _state_config_base["n_steps"],
        )
    assert expected_path in str(exc_info.value), str(exc_info.value)

    # 2. Standalone suews-engine witness
    cfg_path = tmp_path / "bad_state_config.yml"
    cfg_path.write_text(bad_yaml, encoding="utf-8")
    res = subprocess.run(
        [str(suews_engine_bin), "run", str(cfg_path)],
        capture_output=True,
        text=True,
        timeout=60,
        check=False,
    )
    assert res.returncode != 0
    assert expected_path in f"{res.stdout}\n{res.stderr}", res.stderr
    assert not (out_dir / "suews_output.arrow").exists()

    # 3. Public Python SUEWSConfig entry point witness
    if check_public_config:
        with pytest.raises((ValueError, TypeError)):
            SUEWSConfig.from_dict(doc)


@pytest.mark.core
def test_inactive_snow_and_spartacus_permit_omitted_optional_state_and_switch(
    tmp_path: Path,
    suews_engine_bin: Path,
    _state_config_base: dict,
) -> None:
    """With snow_use=0 and net_radiation=3, omitting inactive snowpack/use_sw_direct_albedo succeeds."""
    doc, _ = _stage_doc(_state_config_base, tmp_path)
    doc["sites"][0]["properties"]["spartacus"].pop("use_sw_direct_albedo", None)
    doc["sites"][0]["initial_states"]["paved"].pop("snowpack", None)
    cfg_path = tmp_path / "inactive_ok.yml"
    cfg_path.write_text(_dump_yaml(doc), encoding="utf-8")

    res = subprocess.run(
        [str(suews_engine_bin), "run", str(cfg_path)],
        capture_output=True,
        text=True,
        timeout=60,
        check=False,
    )
    assert res.returncode == 0, f"{res.stdout}\n{res.stderr}"
