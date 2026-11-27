"""Non-positive or non-finite ``lai_max`` is rejected on active vegetated surfaces.

Refs #2149, #2083:
Active vegetated surfaces (``evetr``, ``dectr``, ``grass`` with ``sfr > 0``)
divide by ``LAIMax`` in biogenic photosynthesis and canopy conductance.
Non-positive (``<= 0``) or non-finite ``lai_max`` must fail cleanly across all
public configuration and engine routes:

1. Public config loading (``SUEWSConfig.from_yaml``, ``from_dict``,
   ``SUEWSConfig(**...)``) and annotated YAML output.
2. Post-construction live mutations before or after ``SUEWSSimulation``
   initialisation at ``update_config``, ``sim.run``, ``config_to_engine_yaml``,
   ``_grid_config_jsons``, ``to_df_state``, and ``from_state``.
3. CLI / Phase C validation (``suews validate -p C`` and
   ``collect_phase_c_issues``).
4. Direct resolved native Rust engine (``suews_bridge.run_suews``).

Inactive vegetated surfaces (``sfr == 0``) with ``lai_max == 0`` or omitted
``lai_max`` and active surfaces with small positive ``lai_max > 0`` remain
accepted.

Cost shape (gh#2349). The sample YAML and resolved engine mapping are parsed
once per module with libyaml when available. The full 36-case
``surface x bad_value`` matrix validates through ``from_dict`` and direct
``SUEWSConfig(**...)`` in memory, paired with a 6-case ``from_yaml`` matrix
covering every vegetated surface and YAML value class on disk. Simulation and
direct Rust bridge checks reuse ``sample_config_loaded`` and a two-step forcing
slice instead of re-parsing the full-year forcing file.
"""

from __future__ import annotations

import copy
from importlib import import_module
from pathlib import Path
from typing import Any
import warnings

from click.testing import CliRunner
import numpy as np
import pytest
import yaml

from supy import SUEWSSimulation
from supy.cmd.suews_cli import cli
from supy.data_model.core import SUEWSConfig
from supy.data_model.core.site import LAIParams
from supy.data_model.core.type import RefValue
from supy.data_model.validation.pipeline.phase_c import collect_phase_c_issues

_run_rust_mod = import_module("supy._run_rust")
_check_rust_available = _run_rust_mod._check_rust_available
_grid_config_jsons = _run_rust_mod._grid_config_jsons
_prepare_forcing_block = _run_rust_mod._prepare_forcing_block
_run_grid_into = _run_rust_mod._run_grid_into
config_to_engine_yaml = _run_rust_mod.config_to_engine_yaml

pytestmark = [
    pytest.mark.api,
    pytest.mark.cfg,
    pytest.mark.contract("C-VALID", "C-STRICT", "C-BIND"),
]

VEG_SURFACES: tuple[str, ...] = ("evetr", "dectr", "grass")
_YAML_LOADER = getattr(yaml, "CSafeLoader", yaml.SafeLoader)
_YAML_DUMPER = getattr(yaml, "CSafeDumper", yaml.SafeDumper)
_HANDOFF_STEPS = 2
_YAML_REJECTION_CASES = [
    pytest.param("evetr", 0, id="evetr-int-0"),
    pytest.param("dectr", {"value": 0.0}, id="dectr-ref-0"),
    pytest.param("grass", -1.0, id="grass-neg-1"),
    pytest.param("evetr", {"value": -999.0}, id="evetr-ref-sentinel-neg-999"),
    pytest.param("dectr", float("nan"), id="dectr-nan"),
    pytest.param("grass", float("inf"), id="grass-pos-inf"),
]


def _yaml_load(text: str) -> dict[str, Any]:
    return yaml.load(text, Loader=_YAML_LOADER)


def _yaml_dump(data: dict[str, Any]) -> str:
    return yaml.dump(data, Dumper=_YAML_DUMPER, sort_keys=False)


@pytest.fixture(scope="module")
def sample_dict(sample_yaml_path: Path) -> dict[str, Any]:
    """Return a parsed copy of the shipped sample configuration. READ-ONLY."""
    return _yaml_load(sample_yaml_path.read_text(encoding="utf-8"))


@pytest.fixture(scope="module")
def resolved_sample_dict(sample_config_loaded: SUEWSConfig) -> dict[str, Any]:
    """Parsed resolved engine YAML for the sample config. READ-ONLY."""
    return _yaml_load(config_to_engine_yaml(sample_config_loaded))


@pytest.fixture(scope="module")
def sample_frames(sample_forcing_parsed):
    """Sample state frame and two-step forcing slice. READ-ONLY: callers ``.copy()``."""
    return sample_forcing_parsed(_HANDOFF_STEPS)


@pytest.fixture(scope="module")
def rust_engine_context(sample_config_loaded: SUEWSConfig, sample_frames):
    """Prepared native Rust module and two-step forcing block. READ-ONLY."""
    _, short_forcing = sample_frames
    rust_module = _check_rust_available()
    forcing_block = _prepare_forcing_block(
        short_forcing.copy(),
        soil_moisture_method=int(
            sample_config_loaded.model.physics.soil_moisture.value.value
        ),
    )
    return rust_module, forcing_block, len(short_forcing)


def _sim_from_sample(
    sample_config_loaded: SUEWSConfig, short_forcing
) -> SUEWSSimulation:
    """Build a fresh ``SUEWSSimulation`` from cached sample config and two-step forcing."""
    sim = SUEWSSimulation()
    sim.update_config(
        sample_config_loaded.model_copy(deep=True), auto_load_forcing=False
    )
    sim.update_forcing(short_forcing.copy())
    return sim


def _land_cover(data: dict[str, Any], site_index: int = 0) -> dict[str, Any]:
    return data["sites"][site_index]["properties"]["land_cover"]


def _activate_surface(lc: dict[str, Any], surface: str) -> None:
    """Ensure *surface* has positive ``sfr`` while preserving ``sum(sfr) == 1``."""
    raw = lc[surface]["sfr"]
    sfr = raw["value"] if isinstance(raw, dict) else raw
    if sfr <= 0.0:
        paved_raw = lc["paved"]["sfr"]
        paved_sfr = paved_raw["value"] if isinstance(paved_raw, dict) else paved_raw
        lc["paved"]["sfr"] = {"value": round(paved_sfr - 0.05, 6)}
        lc[surface]["sfr"] = {"value": 0.05}


def _deactivate_surface(lc: dict[str, Any], surface: str) -> None:
    """Set *surface* ``sfr`` to ``0.0`` and shift its fraction onto ``paved``."""
    raw = lc[surface]["sfr"]
    sfr = raw["value"] if isinstance(raw, dict) else raw
    if sfr > 0.0:
        paved_raw = lc["paved"]["sfr"]
        paved_sfr = paved_raw["value"] if isinstance(paved_raw, dict) else paved_raw
        lc["paved"]["sfr"] = {"value": round(paved_sfr + sfr, 6)}
        lc[surface]["sfr"] = {"value": 0.0}


def _with_veg_lai_max(
    sample_dict: dict[str, Any],
    surface: str,
    lai_max_value: Any,
    *,
    active: bool = True,
    legacy_key: bool = False,
) -> dict[str, Any]:
    """Return a sample copy with *surface* configured with *lai_max_value*."""
    data = copy.deepcopy(sample_dict)
    lc = _land_cover(data)
    if active:
        _activate_surface(lc, surface)
    else:
        _deactivate_surface(lc, surface)
    lai = lc[surface]["lai"]
    lai.pop("lai_max", None)
    lai.pop("laimax", None)
    key = "laimax" if legacy_key else "lai_max"
    lai[key] = lai_max_value
    # Keep lai_min at 0 so range warning does not mask the lai_max guard.
    lai["lai_min"] = {"value": 0.0}
    return data


def _write_yaml(data: dict[str, Any], path: Path) -> Path:
    path.write_text(_yaml_dump(data), encoding="utf-8")
    return path


def _assert_active_lai_max_error(message: str, surface: str) -> None:
    assert f"properties.land_cover.{surface}.lai.lai_max" in message, message
    assert "must be positive (> 0)" in message, message
    assert f"set {surface}.sfr to 0" in message, message


@pytest.mark.parametrize("surface", VEG_SURFACES)
@pytest.mark.parametrize(
    "bad_value",
    [
        0,
        0.0,
        -0.0,
        {"value": 0.0},
        -1.0,
        {"value": -1.0},
        -999.0,
        {"value": -999.0},
        float("nan"),
        {"value": float("nan")},
        float("-inf"),
        float("inf"),
    ],
    ids=[
        "int-0",
        "float-0",
        "neg-zero",
        "ref-0",
        "neg-1",
        "ref-neg-1",
        "sentinel-neg-999",
        "ref-sentinel-neg-999",
        "nan",
        "ref-nan",
        "neg-inf",
        "pos-inf",
    ],
)
def test_from_dict_and_constructor_reject_non_positive_or_nonfinite_lai_max(
    sample_dict: dict[str, Any],
    surface: str,
    bad_value: Any,
) -> None:
    """``from_dict`` and ``SUEWSConfig(**...)`` reject every bad active ``lai_max``."""
    data = _with_veg_lai_max(sample_dict, surface, bad_value, active=True)

    with pytest.raises(ValueError) as exc_dict:
        SUEWSConfig.from_dict(copy.deepcopy(data))
    _assert_active_lai_max_error(str(exc_dict.value), surface)

    with pytest.raises(ValueError) as exc_ctor:
        SUEWSConfig(**copy.deepcopy(data))
    _assert_active_lai_max_error(str(exc_ctor.value), surface)


@pytest.mark.parametrize(("surface", "bad_value"), _YAML_REJECTION_CASES)
def test_from_yaml_rejects_non_positive_or_nonfinite_lai_max(
    sample_dict: dict[str, Any],
    tmp_path: Path,
    surface: str,
    bad_value: Any,
) -> None:
    """``from_yaml`` rejects non-positive and non-finite active ``lai_max`` from disk."""
    data = _with_veg_lai_max(sample_dict, surface, bad_value, active=True)
    yaml_path = _write_yaml(data, tmp_path / f"bad_{surface}.yml")

    with pytest.raises(ValueError) as exc_yaml:
        SUEWSConfig.from_yaml(str(yaml_path))
    _assert_active_lai_max_error(str(exc_yaml.value), surface)


@pytest.mark.parametrize("surface", VEG_SURFACES)
def test_legacy_laimax_key_zero_rejected_with_canonical_path(
    sample_dict: dict[str, Any],
    tmp_path: Path,
    surface: str,
) -> None:
    """Legacy fused ``laimax: 0`` on an active vegetated surface fails cleanly."""
    data = _with_veg_lai_max(
        sample_dict, surface, {"value": 0.0}, active=True, legacy_key=True
    )
    yaml_path = _write_yaml(data, tmp_path / f"legacy_{surface}.yml")

    with warnings.catch_warnings():
        warnings.simplefilter("ignore", (DeprecationWarning, FutureWarning))
        with pytest.raises(ValueError) as exc_info:
            SUEWSConfig.from_yaml(str(yaml_path))
    _assert_active_lai_max_error(str(exc_info.value), surface)


@pytest.mark.parametrize("surface", VEG_SURFACES)
def test_inactive_vegetated_surface_allows_zero_lai_max(
    sample_dict: dict[str, Any],
    surface: str,
) -> None:
    """``sfr == 0`` with ``lai_max == 0`` remains valid across config and engine export (#2149)."""
    data = _with_veg_lai_max(sample_dict, surface, {"value": 0.0}, active=False)

    cfg = SUEWSConfig.from_dict(data)
    cfg.validate_for_engine()
    df_state = cfg.to_df_state()
    engine_yaml = config_to_engine_yaml(cfg)

    veg_idx = VEG_SURFACES.index(surface)
    assert float(df_state.iloc[0]["sfr_surf", f"({veg_idx + 2},)"]) == pytest.approx(
        0.0
    )
    assert float(df_state.iloc[0]["laimax", f"({veg_idx},)"]) == pytest.approx(0.0)
    assert engine_yaml


@pytest.mark.parametrize("surface", VEG_SURFACES)
@pytest.mark.parametrize("small_val", [1e-6, 0.05])
def test_active_vegetated_surface_accepts_small_positive_lai_max(
    sample_dict: dict[str, Any],
    surface: str,
    small_val: float,
) -> None:
    """Small positive ``lai_max > 0`` on an active vegetated surface stays accepted."""
    data = _with_veg_lai_max(sample_dict, surface, {"value": small_val}, active=True)
    # Keep initial lai_id within [lai_min, lai_max] for clean validation.
    data["sites"][0]["initial_states"][surface]["lai_id"] = {"value": small_val}

    cfg = SUEWSConfig.from_dict(data)
    cfg.validate_for_engine()
    df_state = cfg.to_df_state()

    veg_idx = VEG_SURFACES.index(surface)
    assert float(df_state.iloc[0]["laimax", f"({veg_idx},)"]) == pytest.approx(
        small_val
    )


def test_inactive_omitted_lai_max_keeps_df_default_fallback(
    sample_dict: dict[str, Any],
) -> None:
    """Omitted ``lai_max`` on an inactive surface still uses ``LAIMAX_DF_DEFAULT``."""
    data = copy.deepcopy(sample_dict)
    lc = _land_cover(data)
    _deactivate_surface(lc, "grass")
    lc["grass"]["lai"].pop("lai_max", None)

    cfg = SUEWSConfig.from_dict(data)
    df_state = cfg.to_df_state()
    assert float(df_state.iloc[0]["laimax", "(2,)"]) == pytest.approx(
        float(LAIParams.LAIMAX_DF_DEFAULT)
    )


@pytest.mark.parametrize("surface", VEG_SURFACES)
def test_post_construction_mutation_rejected_at_all_engine_handoffs(
    sample_dict: dict[str, Any],
    sample_frames,
    surface: str,
) -> None:
    """In-place mutation to ``lai_max <= 0`` or activating a zero-``lai_max`` surface fails."""
    sample_state, short_forcing = sample_frames

    # Case 1: valid active config mutated to lai_max = 0.0
    active_data = copy.deepcopy(sample_dict)
    _activate_surface(_land_cover(active_data), surface)
    cfg = SUEWSConfig.from_dict(active_data)
    surf_obj = getattr(cfg.sites[0].properties.land_cover, surface)
    surf_obj.lai.lai_max = RefValue(0.0)

    with pytest.raises(ValueError) as exc_handoff:
        cfg.validate_for_engine()
    _assert_active_lai_max_error(str(exc_handoff.value), surface)

    with pytest.raises(ValueError) as exc_df:
        cfg.to_df_state()
    _assert_active_lai_max_error(str(exc_df.value), surface)

    with pytest.raises(ValueError) as exc_yaml:
        config_to_engine_yaml(cfg)
    _assert_active_lai_max_error(str(exc_yaml.value), surface)

    with pytest.raises(ValueError) as exc_json:
        _grid_config_jsons(cfg)
    _assert_active_lai_max_error(str(exc_json.value), surface)

    sim = SUEWSSimulation()
    with pytest.raises(ValueError) as exc_update:
        sim.update_config(cfg, auto_load_forcing=False)
    _assert_active_lai_max_error(str(exc_update.value), surface)

    # Case 2: inactive surface with lai_max = 0.0 activated after construction
    inactive_data = _with_veg_lai_max(
        sample_dict, surface, {"value": 0.0}, active=False
    )
    cfg_inactive = SUEWSConfig.from_dict(inactive_data)
    sim_mut = SUEWSSimulation()
    sim_mut.update_config(cfg_inactive, auto_load_forcing=False)
    sim_mut.update_forcing(short_forcing.copy())
    lc_inactive = cfg_inactive.sites[0].properties.land_cover
    lc_inactive.paved.sfr = RefValue(round(lc_inactive.paved.sfr.value - 0.05, 6))
    getattr(lc_inactive, surface).sfr = RefValue(0.05)

    with pytest.raises(ValueError) as exc_run:
        sim_mut.run()
    _assert_active_lai_max_error(str(exc_run.value), surface)

    # Case 3: legacy from_state path with laimax = 0 on an active surface
    df_state = sample_state.copy()
    veg_idx = VEG_SURFACES.index(surface)
    if float(df_state.iloc[0]["sfr_surf", f"({veg_idx + 2},)"]) <= 0.0:
        paved_sfr = float(df_state.iloc[0]["sfr_surf", "(0,)"])
        df_state.loc[:, ("sfr_surf", "(0,)")] = round(paved_sfr - 0.05, 6)
        df_state.loc[:, ("sfr_surf", f"({veg_idx + 2},)")] = 0.05
    df_state.loc[:, ("laimax", f"({veg_idx},)")] = 0.0
    sim_from_state = SUEWSSimulation.from_state(df_state)
    sim_from_state.update_forcing(short_forcing.copy())

    with pytest.raises(ValueError) as exc_state:
        sim_from_state.run()
    _assert_active_lai_max_error(str(exc_state.value), surface)


@pytest.mark.parametrize(
    "bad_live_value",
    [
        False,
        True,
        RefValue(False),
        RefValue(True),
        np.bool_(False),
        np.bool_(True),
        "bad",
        RefValue("bad"),
    ],
    ids=[
        "bool-false",
        "bool-true",
        "ref-bool-false",
        "ref-bool-true",
        "np-bool-false",
        "np-bool-true",
        "str-bad",
        "ref-str-bad",
    ],
)
def test_boolean_and_malformed_live_lai_max_rejected_at_handoffs(
    sample_config_loaded: SUEWSConfig,
    sample_frames,
    bad_live_value: Any,
) -> None:
    """Live boolean or malformed ``lai_max`` assignments fail at every engine handoff."""
    _, short_forcing = sample_frames
    cfg = sample_config_loaded.model_copy(deep=True)
    cfg.sites[0].properties.land_cover.grass.lai.lai_max = bad_live_value

    with pytest.raises(ValueError) as exc_handoff:
        cfg.validate_for_engine()
    _assert_active_lai_max_error(str(exc_handoff.value), "grass")

    sim_upd = SUEWSSimulation()
    with pytest.raises(ValueError) as exc_update:
        sim_upd.update_config(cfg, auto_load_forcing=False)
    _assert_active_lai_max_error(str(exc_update.value), "grass")

    sim_run = _sim_from_sample(sample_config_loaded, short_forcing)
    sim_run.config.sites[0].properties.land_cover.grass.lai.lai_max = bad_live_value
    with pytest.raises(ValueError) as exc_run:
        sim_run.run()
    _assert_active_lai_max_error(str(exc_run.value), "grass")


def test_boolean_legacy_laimax_rejected_on_load_and_dict_lai(
    sample_dict: dict[str, Any],
    sample_config_loaded: SUEWSConfig,
    tmp_path: Path,
) -> None:
    """Boolean values on legacy ``laimax`` fail on load and on dict-backed live ``lai``."""
    data = _with_veg_lai_max(
        sample_dict, "grass", {"value": False}, active=True, legacy_key=True
    )
    yaml_path = _write_yaml(data, tmp_path / "bool_legacy_laimax.yml")
    with warnings.catch_warnings():
        warnings.simplefilter("ignore", (DeprecationWarning, FutureWarning))
        with pytest.raises(ValueError):
            SUEWSConfig.from_yaml(str(yaml_path))

    cfg = sample_config_loaded.model_copy(deep=True)
    cfg.sites[0].properties.land_cover.grass.lai = {"laimax": False}
    with pytest.raises(ValueError) as exc_dict_lai:
        cfg.validate_for_engine()
    _assert_active_lai_max_error(str(exc_dict_lai.value), "grass")


def test_cli_validate_and_phase_c_reject_active_zero_lai_max_and_accept_inactive(
    sample_dict: dict[str, Any],
    tmp_path: Path,
) -> None:
    """``suews validate -p C`` and ``collect_phase_c_issues`` enforce the active ``lai_max`` guard."""
    bad_data = _with_veg_lai_max(sample_dict, "grass", {"value": 0.0}, active=True)
    bad_path = _write_yaml(bad_data, tmp_path / "cli_bad_grass.yml")

    issues = collect_phase_c_issues(str(bad_path))
    assert any(
        "properties.land_cover.grass.lai.lai_max" in issue.message
        and "must be positive (> 0)" in issue.message
        for issue in issues
    ), issues

    runner = CliRunner()
    res_bad = runner.invoke(cli, ["validate", str(bad_path), "-p", "C"])
    assert res_bad.exit_code != 0, res_bad.output
    report_path = tmp_path / "report_cli_bad_grass.txt"
    assert report_path.is_file(), res_bad.output
    report_text = report_path.read_text(encoding="utf-8")
    assert "properties.land_cover.grass.lai.lai_max" in report_text
    assert "must be positive (> 0)" in report_text

    good_data = _with_veg_lai_max(sample_dict, "grass", {"value": 0.0}, active=False)
    good_path = _write_yaml(good_data, tmp_path / "cli_inactive_grass.yml")
    assert collect_phase_c_issues(str(good_path)) == []


def test_annotated_yaml_fail_path_still_raises_and_writes_file(
    sample_dict: dict[str, Any],
    tmp_path: Path,
) -> None:
    """``auto_generate_annotated=True`` raises the active ``lai_max`` error and writes the file."""
    data = _with_veg_lai_max(sample_dict, "dectr", {"value": 0.0}, active=True)
    yaml_path = _write_yaml(data, tmp_path / "annotated_dectr.yml")

    with pytest.raises(ValueError) as exc_info:
        SUEWSConfig.from_yaml(str(yaml_path), auto_generate_annotated=True)
    _assert_active_lai_max_error(str(exc_info.value), "dectr")

    annotated_path = tmp_path / "annotated_dectr_annotated.yml"
    assert annotated_path.is_file()


@pytest.mark.parametrize("surface", VEG_SURFACES)
def test_native_rust_bridge_rejects_active_zero_lai_max_directly(
    resolved_sample_dict: dict[str, Any],
    rust_engine_context,
    surface: str,
) -> None:
    """Direct Rust engine call rejects active ``lai_max <= 0`` bypassing Python."""
    rust_module, forcing_block, n_steps = rust_engine_context
    resolved_dict = copy.deepcopy(resolved_sample_dict)
    lc = resolved_dict["sites"][0]["properties"]["land_cover"]
    _activate_surface(lc, surface)
    lc[surface]["lai"]["lai_max"] = {"value": 0.0}
    patched_yaml = _yaml_dump(resolved_dict)

    with pytest.raises(
        RuntimeError,
        match=rf"sites\[gridiv=1\]\.properties\.land_cover\.{surface}\.lai\.lai_max.*must be positive \(> 0\)",
    ):
        _run_grid_into(
            rust_module,
            patched_yaml,
            forcing_block,
            n_steps,
            grid_id=1,
        )


def test_native_rust_bridge_accepts_inactive_zero_lai_max_and_small_positive(
    resolved_sample_dict: dict[str, Any],
    rust_engine_context,
) -> None:
    """Direct Rust engine call accepts ``sfr == 0`` with ``lai_max == 0`` and small positive ``lai_max``."""
    rust_module, forcing_block, n_steps = rust_engine_context
    resolved_dict = copy.deepcopy(resolved_sample_dict)
    lc = resolved_dict["sites"][0]["properties"]["land_cover"]
    _deactivate_surface(lc, "evetr")
    lc["evetr"]["lai"]["lai_min"] = {"value": 0.0}
    lc["evetr"]["lai"]["lai_max"] = {"value": 0.0}
    lc["grass"]["lai"]["lai_min"] = {"value": 0.0}
    lc["grass"]["lai"]["lai_max"] = {"value": 0.05}
    resolved_dict["sites"][0]["initial_states"]["grass"]["lai_id"] = {"value": 0.05}
    patched_yaml = _yaml_dump(resolved_dict)

    df_output, _, _ = _run_grid_into(
        rust_module,
        patched_yaml,
        forcing_block,
        n_steps,
        grid_id=1,
    )
    assert not df_output.empty
