"""Public site labels survive private native identity allocation and restart."""

import json
import logging

import pandas as pd
import pytest

from supy import SUEWSSimulation
from supy._run_rust import (  # ruff: ignore[import-private-name]
    _grid_config_jsons,
    _load_rust_module,
    _native_grid_ids,
    _prepare_forcing_block,
    _public_grid_error,
    _run_grids_into,
    config_to_engine_yaml,
    run_suews_rust,
    run_suews_rust_multi_with_state,
    run_suews_rust_with_state,
)
from supy.suews_checkpoint import SUEWSCheckpoint

pytestmark = [pytest.mark.native, pytest.mark.contract("C-API", "C-RESTART", "C-ERROR")]

PUBLIC = [-(2**31), -19, 0, 1, 2, 2**31 - 1, -(2**31) - 1, 2**31, 2**53 + 1, 10**80 + 17]
COMPACT = [-(2**31), -19, 0, 1, 2, 2**31 - 1, 3, 4, 5, 6]


def _simulation(ids):
    sim = SUEWSSimulation.from_sample_data()
    forcing = sim.forcing.df.iloc[:12].copy()
    config = sim.config.model_copy(deep=True)
    template = config.sites[0]
    sites = []
    for index, grid in enumerate(ids):
        site = template.model_copy(deep=True)
        site.gridiv = grid
        site.properties.z.value = 18 + index * 3
        site.properties.surfacearea.value = 10000 + index * 900
        sites.append(site)
    config.sites = sites
    sim.update_config(config, auto_load_forcing=False)
    sim.update_forcing(forcing)
    return sim


def _equal_by_site(left, left_ids, right, right_ids):
    for public, compact in zip(left_ids, right_ids, strict=True):
        pd.testing.assert_frame_equal(
            left.xs(public, level="grid"), right.xs(compact, level="grid"),
            check_exact=True, check_freq=False,
        )


def test_native_identity_is_injective_stable_and_preserves_signed_ids():
    """Exceptional integers cannot collide with reserved or allocated IDs."""
    mapping = _native_grid_ids(PUBLIC)
    assert len(set(mapping.values())) == len(PUBLIC)
    assert all(-(2**31) <= grid < 2**31 for grid in mapping.values())
    assert all(mapping[grid] == grid for grid in PUBLIC[:6])
    assert mapping == _native_grid_ids(PUBLIC[::-1])
    # Independent runs are allowed to reuse native labels.
    assert _native_grid_ids([10**80 + 17]) == {10**80 + 17: 1}


def test_only_execution_copy_has_native_labels():
    """Source objects and exported YAML keep the original public identifiers."""
    sim = _simulation(PUBLIC)
    original = sim.config.model_dump(mode="json")
    exported = config_to_engine_yaml(sim.config)
    ids, payloads = _grid_config_jsons(sim.config)
    assert ids == PUBLIC
    assert [json.loads(p)["sites"][0]["gridiv"] for p in payloads] == COMPACT
    assert sim.config.model_dump(mode="json") == original
    assert config_to_engine_yaml(sim.config) == exported
    assert str(PUBLIC[-1]) in exported


def test_omitted_source_id_retains_default_one():
    """Allocation must not reinterpret the source model's default identifier."""
    sim = SUEWSSimulation.from_sample_data()
    data = sim.config.model_dump(mode="json")
    data["sites"][0].pop("gridiv")
    sim = SUEWSSimulation()
    sim.update_config(data, auto_load_forcing=False)
    ids, payloads = _grid_config_jsons(sim.config)
    assert ids == [1]
    assert json.loads(payloads[0])["sites"][0]["gridiv"] == 1


@pytest.mark.parametrize("labels", [[37, "37"], [1, True], [0, -0.0]])
def test_normalised_source_duplicates_are_rejected(labels):
    """Source aliases must not become two sites with one public output key."""
    sim = SUEWSSimulation.from_sample_data()
    data = sim.config.model_dump(mode="json")
    data["sites"] = [dict(data["sites"][0], gridiv=label) for label in labels]
    sim = SUEWSSimulation()
    with pytest.raises(ValueError, match=r"[Dd]uplicate"):
        sim.update_config(data, auto_load_forcing=False)
        _grid_config_jsons(sim.config)


@pytest.mark.parametrize("jobs", [1, 2])
def test_public_outputs_and_reversed_checkpoint_restart(tmp_path, jobs):
    """Every output and public checkpoint key survives mapping and reordering."""
    sim = _simulation(PUBLIC)
    compact = _simulation(COMPACT)
    end = sim.forcing.index[-1]
    full = sim.run(end_date=end, n_jobs=jobs).df
    control = compact.run(end_date=end, n_jobs=jobs).df
    _equal_by_site(full, PUBLIC, control, COMPACT)
    assert not full.xs(PUBLIC[0], level="grid").equals(full.xs(PUBLIC[-1], level="grid"))

    early = _simulation(PUBLIC)
    first = early.run(end_date=early.forcing.index[5], n_jobs=jobs).df
    path = tmp_path / "checkpoint.json"
    early.checkpoint.to_file(path)
    checkpoint = SUEWSCheckpoint.from_file(path)
    assert set(checkpoint.grid_states) == set(PUBLIC)
    config = early.config.model_copy(deep=True)
    config.sites.reverse()
    resumed = SUEWSSimulation.from_checkpoint(config, checkpoint)
    resumed.update_forcing(early.forcing.df)
    last = resumed.run(start_date=early.forcing.index[6], end_date=end, n_jobs=jobs).df
    _equal_by_site(full, PUBLIC, pd.concat([first, last]).sort_index(), PUBLIC)


def test_single_site_helpers_preserve_wide_label():
    """The single-site cold and restart helpers use the same private mapping."""
    grid = PUBLIC[-1]
    sim = _simulation([grid])
    first, state, _ = run_suews_rust(sim.config, sim.forcing.df.iloc[:6], grid)
    last, _, _ = run_suews_rust_with_state(sim.config, sim.forcing.df.iloc[6:], grid, state)
    assert set(first.index.get_level_values("grid")) == {grid}
    assert set(last.index.get_level_values("grid")) == {grid}


def test_source_validation_names_public_grid():
    """A failed source input is attributed to its public label before dispatch."""
    sim = _simulation([1, 2**53 + 1])
    data = sim.config.model_dump(mode="json", exclude_none=True)
    data["sites"][1]["properties"]["z"] = {"value": None}
    with pytest.raises(ValueError, match=str(2**53 + 1)):
        SUEWSSimulation().update_config(data, auto_load_forcing=False)


@pytest.mark.parametrize("parallel, grid", [(False, 2**53 + 1), (True, 10**80 + 17)])
def test_mapped_payload_native_failure_restores_public_site_path(parallel, grid):
    """Exercise a real native validation failure after private payload mapping."""
    sim = _simulation([1, grid])
    ids, payloads = _grid_config_jsons(sim.config)
    invalid = json.loads(payloads[1])
    del invalid["sites"][0]["properties"]["z"]
    payloads[1] = json.dumps(invalid)
    with pytest.raises(RuntimeError) as raised:
        _run_grids_into(
            _load_rust_module(), payloads, _prepare_forcing_block(sim.forcing.df),
            len(sim.forcing.df), ids, None, parallel, 2,
        )
    assert f"sites[gridiv={ids[1]}].properties.z" in str(raised.value)
    assert "sites[gridiv=2]" not in str(raised.value)


def test_unlabelled_native_error_uses_public_batch_identity():
    """Positional native errors and site paths use distinct index namespaces."""
    sim = _simulation([1, 2**53 + 1])
    ids, payloads = _grid_config_jsons(sim.config)
    assert _public_grid_error("grid 1: kernel failed", ids, payloads) == f"grid {ids[1]}: kernel failed"
    assert _public_grid_error("grid 1: sites[gridiv=2].properties.z missing", ids, payloads) == f"grid {ids[1]}: sites[gridiv={ids[1]}].properties.z missing"


@pytest.mark.parametrize("payload", ["sites: [{gridiv: 2}]", "sites: [", "{}"])
def test_error_attribution_accepts_yaml_and_preserves_malformed_input_errors(payload):
    """Diagnostic adaptation must not mask an error with its own parse failure."""
    grid = 2**53 + 1
    assert _public_grid_error("invalid input", [grid], [payload]) == f"grid {grid}: invalid input"
    if payload.startswith("sites: [{"):
        assert _public_grid_error("sites[gridiv=2].properties.z missing", [grid], [payload]) == f"grid {grid}: sites[gridiv={grid}].properties.z missing"


@pytest.mark.parametrize("serial", [True, False])
def test_native_checkpoint_error_names_public_grid(serial):
    """An actual native restart error with no site path still identifies its site."""
    sim = _simulation([1, 2**53 + 1])
    _, state, _ = run_suews_rust(sim.config, sim.forcing.df)
    with pytest.raises((RuntimeError, ValueError), match=str(2**53 + 1)):
        run_suews_rust_multi_with_state(
            sim.config, sim.forcing.df, {1: state, 2**53 + 1: "{}"}, serial_mode=serial,
        )


def test_kernel_warnings_keep_public_identity(caplog):
    """A real DyOHM stability warning is labelled with the public grid."""
    grid = 2**53 + 1
    sim = _simulation([grid])
    config = sim.config.model_copy(deep=True)
    config.model.physics.storage_heat = 6
    config.sites[0].properties.land_cover.paved.thermal_layers.rho_cp.value[0] = 2.0e5
    sim.update_config(config, auto_load_forcing=False)
    with caplog.at_level(logging.WARNING, logger="SuPy"):
        sim.run(end_date=sim.forcing.index[-1])
    assert not sim.kernel_warnings.empty
    assert set(sim.kernel_warnings["grid"]) == {grid}
    assert any(f"(grid {grid})" in record.getMessage() for record in caplog.records)
