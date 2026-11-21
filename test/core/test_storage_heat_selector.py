"""Regression tests for resolved native model.physics.storage_heat handling."""

from __future__ import annotations

from copy import deepcopy
from importlib.resources import as_file, files
import json
import os
from pathlib import Path
import subprocess

from click.testing import CliRunner
import numpy as np
import pytest
import yaml

# Intentionally exercises the private native bridge boundary.
# ruff: disable[import-private-name]
from supy import SUEWSSimulation
from supy._run_rust import (
    _alloc_output_buffers,
    _load_rust_module,
    _parse_output_block,
    _prepare_forcing_block,
    config_to_engine_yaml,
)
from supy.cmd.suews_cli import cli
from supy.data_model import SUEWSConfig
from supy.data_model.core.type import RefValue

# ruff: enable[import-private-name]

pytestmark = [
    pytest.mark.physics,
    pytest.mark.contract("C-STRICT", "C-BIND", "C-EMIT", "C-PARITY", "C-RESPONSE"),
]


def _locate_suews_engine() -> Path:
    """Locate the built standalone ``suews-engine`` binary."""
    binary_name = "suews-engine.exe" if os.name == "nt" else "suews-engine"
    for rel in (Path("bin") / binary_name, Path(binary_name)):
        bundled = files("supy").joinpath(str(rel))
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


def _sample_simulation() -> SUEWSSimulation:
    return SUEWSSimulation(str(files("supy").joinpath("sample_data/sample_config.yml")))


def _config_dict(simulation: SUEWSSimulation) -> dict:
    return simulation.config.model_dump(exclude_none=True, mode="json")


def _unwrap_value(entry):
    return entry["value"] if isinstance(entry, dict) else entry


def _run_pyo3(rust_module, payload: str, forcing, state_json: str | None = None):
    args = (
        payload,
        _prepare_forcing_block(forcing).ravel(order="C").tolist(),
        len(forcing),
    )
    if state_json is None:
        return rust_module.run_suews(*args)[:3]
    return rust_module.run_suews_with_state(*args, state_json)[:3]


_INVALID_SET_VALUES: dict[str, object] = {
    "bare-null": None,
    "wrapped-null": {"value": None},
    "empty-map": {},
    "ref-only": {"ref": {"desc": "citation"}},
    "bare-nan": float("nan"),
    "wrapped-nan": {"value": float("nan")},
    "pos-inf": float("inf"),
    "out-of-i32-high": 2147483648,
    "out-of-i32-low": -2147483649,
    "fractional": 1.5,
    "near-int": 1.0000000005,
    "bool-true": True,
    "list": [1],
    "unexpected-map": {"unexpected": 1},
    "double-wrapped": {"value": {"value": 1}},
    "empty-string": "",
}
_YAML_NONFINITE_KINDS = frozenset({"bare-nan", "wrapped-nan", "pos-inf"})
_ALIAS_INVALID_VALUES: dict[str, tuple[str, object]] = {
    "legacy-null": ("storageheatmethod", None),
    "compat-wrapped-null": ("storage_heat_method", {"value": None}),
}


def _make_invalid_payload(base_cfg: dict, kind: str) -> str:
    cfg = deepcopy(base_cfg)
    if kind == "missing-physics":
        cfg["model"].pop("physics", None)
        return json.dumps(cfg)

    phy = cfg["model"]["physics"]
    if kind == "absent":
        phy.pop("storage_heat", None)
    elif kind in _ALIAS_INVALID_VALUES:
        alias_key, alias_val = _ALIAS_INVALID_VALUES[kind]
        phy.pop("storage_heat", None)
        phy[alias_key] = deepcopy(alias_val)
    elif kind in _INVALID_SET_VALUES:
        phy["storage_heat"] = deepcopy(_INVALID_SET_VALUES[kind])
        if kind in _YAML_NONFINITE_KINDS:
            return yaml.safe_dump(cfg, sort_keys=False)
    else:
        raise ValueError(f"Unknown kind: {kind}")
    return json.dumps(cfg)


def test_source_default_is_bit_identical_to_explicit_ohm():
    """Omitting storage_heat in source config resolves to OHM (1) identically."""
    sample_path = files("supy").joinpath("sample_data/sample_config.yml")
    source_raw = yaml.safe_load(sample_path.read_text(encoding="utf-8"))
    source_omitted = deepcopy(source_raw)
    for key in ("storage_heat", "storageheatmethod", "storage_heat_method"):
        source_omitted["model"]["physics"].pop(key, None)

    resolved_default = SUEWSConfig.from_dict(source_omitted)
    default_dump = resolved_default.model_dump(exclude_none=True, mode="json")
    explicit_dump = SUEWSConfig.from_dict(source_raw).model_dump(
        exclude_none=True, mode="json"
    )

    assert _unwrap_value(default_dump["model"]["physics"]["storage_heat"]) == 1
    emitted_yaml = yaml.safe_load(config_to_engine_yaml(resolved_default))
    assert _unwrap_value(emitted_yaml["model"]["physics"]["storage_heat"]) == 1

    forcing = _sample_simulation().forcing.df.iloc[:12]
    rust = _load_rust_module()
    out_def, state_def, len_def = _run_pyo3(rust, json.dumps(default_dump), forcing)
    out_exp, state_exp, len_exp = _run_pyo3(rust, json.dumps(explicit_dump), forcing)

    assert len_def == len_exp == len(forcing)
    np.testing.assert_array_equal(out_def, out_exp)
    assert json.loads(state_def) == json.loads(state_exp)


@pytest.mark.parametrize("entry_point", ["oop", "cli"])
def test_source_storage_heat_default_through_supported_entry_points(
    entry_point: str, tmp_path: Path, monkeypatch
):
    """Both SUEWSSimulation and suews run resolve omitted storage_heat to 1."""
    sample = Path(str(files("supy").joinpath("sample_data/sample_config.yml")))
    source = yaml.safe_load(sample.read_text(encoding="utf-8"))
    source["model"]["physics"].pop("storage_heat", None)
    control = source["model"]["control"]
    forcing_file = _unwrap_value(control["forcing"]["file"])
    control["forcing"]["file"] = str(sample.parent / forcing_file)
    control["start_time"] = "2012-01-01 00:05:00"
    control["end_time"] = "2012-01-01 01:00:00"
    source_path = tmp_path / "source.yml"
    source_path.write_text(yaml.safe_dump(source), encoding="utf-8")

    completed = []

    def capture_save(simulation, **kwargs):
        completed.append(simulation)
        return []

    monkeypatch.setattr(SUEWSSimulation, "save", capture_save)
    if entry_point == "cli":
        result = CliRunner().invoke(cli, ["run", str(source_path)])
        assert result.exit_code == 0, result.output
        assert len(completed) == 1
    else:
        sim = SUEWSSimulation(source_path)
        sim.run()
        completed.append(sim)

    sh = completed[0].config.model.physics.storage_heat
    while hasattr(sh, "value"):
        sh = sh.value
    assert int(sh) == 1


@pytest.mark.parametrize(
    ("key", "value", "expected", "deprecated"),
    [
        ("storageheatmethod", 1, 1, True),
        ("storage_heat_method", {"value": 1}, 1, True),
        ("storage_heat", "ohm", 1, False),
        ("storage_heat", "dyohm", 6, False),
        ("storage_heat", {"ohm": {"include_qf": False}}, 1, False),
    ],
)
def test_source_historical_and_readable_spellings_remain_supported(
    key: str, value, expected: int, deprecated: bool
):
    """Historical aliases (with DeprecationWarning) and readable names remain accepted."""
    sample_path = files("supy").joinpath("sample_data/sample_config.yml")
    source = yaml.safe_load(sample_path.read_text(encoding="utf-8"))
    source["model"]["physics"].pop("storage_heat", None)
    if key == "storage_heat" and isinstance(value, dict) and "ohm" in value:
        source["model"]["physics"].pop("ohm_inc_qf", None)
    source["model"]["physics"][key] = value
    if deprecated:
        with pytest.warns(DeprecationWarning, match=key):
            cfg = SUEWSConfig.from_dict(source)
    else:
        cfg = SUEWSConfig.from_dict(source)
    sh = cfg.model.physics.storage_heat
    while hasattr(sh, "value"):
        sh = sh.value
    assert int(sh) == expected


@pytest.mark.parametrize("mutated", [None, RefValue(None)])
def test_validate_for_engine_rejects_live_null_storage_heat(mutated):
    """Post-construction mutation to null is rejected before native handoff."""
    sample_path = files("supy").joinpath("sample_data/sample_config.yml")
    cfg = SUEWSConfig.from_yaml(str(sample_path))
    cfg.model.physics.__dict__["storage_heat"] = mutated
    with pytest.raises(ValueError, match="storage_heat"):
        cfg.validate_for_engine()


@pytest.mark.parametrize(
    "kind",
    [
        "absent",
        "missing-physics",
        "bare-null",
        "wrapped-null",
        "empty-map",
        "ref-only",
        "bare-nan",
        "wrapped-nan",
        "pos-inf",
        "out-of-i32-high",
        "out-of-i32-low",
        "fractional",
        "near-int",
        "bool-true",
        "list",
        "unexpected-map",
        "double-wrapped",
        "empty-string",
        "legacy-null",
        "compat-wrapped-null",
    ],
)
def test_pyo3_all_entry_points_reject_invalid_resolved_storage_heat(kind: str):
    """All six PyO3 entry points reject invalid storage_heat with its full path."""
    sim = _sample_simulation()
    valid_cfg = _config_dict(sim)
    valid_json = json.dumps(valid_cfg)
    invalid_payload = _make_invalid_payload(valid_cfg, kind)
    forcing = sim.forcing.df.iloc[:1]
    forcing_arr = np.ascontiguousarray(
        _prepare_forcing_block(forcing), dtype=np.float64
    )
    forcing_list = forcing_arr.ravel(order="C").tolist()
    n_rows = len(forcing)
    rust = _load_rust_module()

    _, state_json, _ = _run_pyo3(rust, valid_json, forcing)

    match = r"model\.physics\.storage_heat"
    with pytest.raises(RuntimeError, match=match):
        rust.run_suews(invalid_payload, forcing_list, n_rows)

    with pytest.raises(RuntimeError, match=match):
        rust.run_suews_with_state(invalid_payload, forcing_list, n_rows, state_json)

    ar_data, ar_dt = _alloc_output_buffers(n_rows)
    with pytest.raises(RuntimeError, match=match):
        rust.run_suews_into(invalid_payload, forcing_arr, n_rows, ar_data, ar_dt, None)

    with pytest.raises(RuntimeError, match=r"grid 1:.*" + match):
        rust.run_suews_multi([valid_json, invalid_payload], forcing_list, n_rows, 2)

    with pytest.raises(RuntimeError, match=r"grid 1:.*" + match):
        rust.run_suews_multi_with_state(
            [valid_json, invalid_payload],
            forcing_list,
            n_rows,
            [state_json, state_json],
            2,
        )

    bufs = [_alloc_output_buffers(n_rows) for _ in range(2)]
    with pytest.raises(RuntimeError, match=r"grid 1:.*" + match):
        rust.run_suews_multi_into(
            [valid_json, invalid_payload],
            forcing_arr,
            n_rows,
            [d for d, _ in bufs],
            [t for _, t in bufs],
        )


def test_pyo3_valid_storage_heat_options_produce_distinct_scientific_fluxes():
    """Legitimate selectors reach Fortran and yield distinct physical QS and QH."""
    sim = _sample_simulation()
    base_cfg = _config_dict(sim)
    forcing = sim.forcing.df.iloc[:12].copy()
    rust = _load_rust_module()

    def run_for(mutator, met=forcing):
        cfg = deepcopy(base_cfg)
        mutator(cfg)
        out_bytes, _, length = _run_pyo3(rust, json.dumps(cfg), met)
        return _parse_output_block(out_bytes, length, grid_id=1)

    ohm = run_for(lambda c: None)
    ohm_alias = run_for(
        lambda c: (
            c["model"]["physics"].pop("storage_heat"),
            c["model"]["physics"].__setitem__("storageheatmethod", {"value": 1}),
        )
    )
    np.testing.assert_array_equal(
        ohm.loc[:, ("SUEWS", "QS")].to_numpy(),
        ohm_alias.loc[:, ("SUEWS", "QS")].to_numpy(),
    )

    dyohm = run_for(lambda c: c["model"]["physics"].__setitem__("storage_heat", 6))
    ehc = run_for(lambda c: c["model"]["physics"].__setitem__("storage_heat", 5))
    for frame in (ohm, dyohm, ehc):
        assert np.all(np.isfinite(frame.loc[:, ("SUEWS", "QS")].to_numpy()))
        assert np.all(np.isfinite(frame.loc[:, ("SUEWS", "QH")].to_numpy()))

    assert not np.allclose(
        ohm.loc[:, ("SUEWS", "QS")].to_numpy(),
        dyohm.loc[:, ("SUEWS", "QS")].to_numpy(),
    )
    assert not np.allclose(
        ohm.loc[:, ("SUEWS", "QS")].to_numpy(),
        ehc.loc[:, ("SUEWS", "QS")].to_numpy(),
    )

    # Observed storage heat (0) with valid observed qs=10.0 reproduces QS == 10.0.
    met_qs = forcing.copy()
    met_qs["qs"] = 10.0
    obs = run_for(
        lambda c: c["model"]["physics"].__setitem__("storage_heat", 0), met_qs
    )
    np.testing.assert_allclose(obs.loc[:, ("SUEWS", "QS")].to_numpy(), 10.0)


@pytest.mark.parametrize(
    ("label", "mutator"),
    [
        ("absent", lambda phy: phy.pop("storage_heat", None)),
        ("bare-null", lambda phy: phy.__setitem__("storage_heat", None)),
        ("wrapped-null", lambda phy: phy.__setitem__("storage_heat", {"value": None})),
        ("bare-nan", lambda phy: phy.__setitem__("storage_heat", float("nan"))),
        ("out-of-i32", lambda phy: phy.__setitem__("storage_heat", 2147483648)),
        ("fractional", lambda phy: phy.__setitem__("storage_heat", 1.5)),
        ("bool-true", lambda phy: phy.__setitem__("storage_heat", True)),
        ("empty-string", lambda phy: phy.__setitem__("storage_heat", "")),
    ],
)
def test_standalone_engine_rejects_invalid_resolved_storage_heat(
    tmp_path: Path, label: str, mutator
):
    """Standalone suews-engine rejects malformed storage_heat without output."""
    engine_bin = _locate_suews_engine()
    sample_dir = files("supy").joinpath("sample_data")
    src = yaml.safe_load(
        sample_dir.joinpath("sample_config.yml").read_text(encoding="utf-8")
    )
    forcing_lines = (
        sample_dir
        .joinpath("Kc_2012_data_60.txt")
        .read_text(encoding="utf-8")
        .splitlines()
    )
    forcing_path = tmp_path / "forcing.txt"
    forcing_path.write_text("\n".join(forcing_lines[:5]) + "\n", encoding="utf-8")

    cfg = yaml.safe_load(config_to_engine_yaml(SUEWSConfig.from_dict(src)))
    cfg["model"]["control"]["forcing"]["file"] = str(forcing_path)
    cfg["model"]["control"]["output"]["dir"] = str(tmp_path)
    mutator(cfg["model"]["physics"])
    yml_path = tmp_path / "resolved.yml"
    yml_path.write_text(yaml.safe_dump(cfg, sort_keys=False), encoding="utf-8")

    proc = subprocess.run(
        [str(engine_bin), "run", str(yml_path)],
        capture_output=True,
        text=True,
        timeout=60,
        check=False,
    )
    assert proc.returncode != 0, f"{label} must fail"
    assert "model.physics.storage_heat" in proc.stderr, proc.stderr
    assert not (tmp_path / "suews_output.arrow").exists()


@pytest.mark.parametrize("bad_code", [2, 4])
def test_standalone_engine_preserves_kernel_error_106_for_unsupported_int_codes(
    tmp_path: Path, bad_code: int
):
    """Unsupported i32 codes still pass YAML decode and hit Fortran error 106."""
    engine_bin = _locate_suews_engine()
    sample_dir = files("supy").joinpath("sample_data")
    src = yaml.safe_load(
        sample_dir.joinpath("sample_config.yml").read_text(encoding="utf-8")
    )
    forcing_lines = (
        sample_dir
        .joinpath("Kc_2012_data_60.txt")
        .read_text(encoding="utf-8")
        .splitlines()
    )
    forcing_path = tmp_path / "forcing.txt"
    forcing_path.write_text("\n".join(forcing_lines[:5]) + "\n", encoding="utf-8")

    cfg = yaml.safe_load(config_to_engine_yaml(SUEWSConfig.from_dict(src)))
    cfg["model"]["control"]["forcing"]["file"] = str(forcing_path)
    cfg["model"]["control"]["output"]["dir"] = str(tmp_path)
    cfg["model"]["physics"]["storage_heat"] = {"value": bad_code}
    yml_path = tmp_path / "resolved.yml"
    yml_path.write_text(yaml.safe_dump(cfg, sort_keys=False), encoding="utf-8")

    proc = subprocess.run(
        [str(engine_bin), "run", str(yml_path)],
        capture_output=True,
        text=True,
        timeout=60,
        check=False,
    )
    assert proc.returncode != 0
    assert "is not available" in proc.stderr
    assert f"StorageHeatMethod {bad_code}" in proc.stderr
