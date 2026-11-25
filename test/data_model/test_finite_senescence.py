"""Finite SDD input survives normal routes; invalid live state never reaches export.

These tests use actual export and simulation entry points. No backend or
validation method is mocked. The native reader is a separate work item.
"""

import copy
import json

import numpy as np
import pandas as pd
import pytest
import yaml

from supy import SUEWSSimulation
from supy._run_rust import (  # noqa: PLC2701 - exercise all engine handoffs
    _grid_config_jsons,
    config_to_engine_yaml,
    run_suews_rust,
    run_suews_rust_multi,
)
from supy.data_model.core import SUEWSConfig
from supy.data_model.core.type import Reference, RefValue

pytestmark = [
    pytest.mark.api,
    pytest.mark.cfg,
    pytest.mark.contract(
        "C-VALID", "C-STRICT", "C-ERROR", "C-BIND", "C-ROUNDTRIP", "C-ISOLATION"
    ),
]

SURFACES = ("evetr", "dectr", "grass")
NONFINITE = [float("nan"), float("inf"), -float("inf")]
REF = {"desc": "Finite senescence regression input"}
MUTATIONS = (
    "nested",
    "typed",
    "scalar",
    "mapping",
    "unsafe_nested",
    "construct",
    "copy",
)


@pytest.fixture(scope="module")
def sample_dict(sample_yaml_path):
    return yaml.safe_load(sample_yaml_path.read_text(encoding="utf-8"))


@pytest.fixture(scope="module")
def sample_frame(sample_config_loaded):
    return sample_config_loaded.to_df_state()


def _field(data, surface):
    return data["sites"][0]["initial_states"][surface]["sdd_id"]


def _load(data, route, tmp_path):
    if route == "dict":
        return SUEWSConfig.from_dict(data)
    if route == "constructor":
        return SUEWSConfig(**data)
    path = tmp_path / "senescence.yml"
    path.write_text(yaml.safe_dump(data), encoding="utf-8")
    return SUEWSConfig.from_yaml(str(path))


def _mutate(cfg, surface, value, mutation, site_index=0):
    state = getattr(cfg.sites[site_index].initial_states, surface)
    if mutation == "nested":
        state.sdd_id.value = value
    elif mutation == "typed":
        state.sdd_id = RefValue[float](value=value, ref=Reference(**REF))
    elif mutation == "scalar":
        state.sdd_id = value
    elif mutation == "mapping":
        state.sdd_id = {"value": value, "ref": REF.copy()}
    elif mutation == "unsafe_nested":
        object.__setattr__(state.sdd_id, "value", value)  # noqa: PLC2801 - bypass assignment validation
    elif mutation == "construct":
        object.__setattr__(  # noqa: PLC2801 - replace with an unchecked model
            state, "sdd_id", RefValue[float].model_construct(value=value)
        )
    elif mutation == "copy":
        replacement = state.model_copy(update={"sdd_id": value})
        setattr(cfg.sites[site_index].initial_states, surface, replacement)
    else:
        raise AssertionError(mutation)


@pytest.mark.parametrize("route", ["dict", "yaml", "constructor"])
@pytest.mark.parametrize("wrapped", [False, True])
@pytest.mark.parametrize("surface", SURFACES)
@pytest.mark.parametrize("value", NONFINITE, ids=["nan", "inf", "minus_inf"])
def test_load_rejects_nonfinite_with_field_location(
    sample_dict, route, wrapped, surface, value, tmp_path
):
    data = copy.deepcopy(sample_dict)
    data["sites"][0]["initial_states"][surface]["sdd_id"] = (
        {"value": value, "ref": REF.copy()} if wrapped else value
    )
    with pytest.raises(ValueError) as error:
        _load(data, route, tmp_path)
    message = str(error.value)
    assert "sites" in message
    assert f"initial_states.{surface}.sdd_id" in message
    assert "finite" in message


@pytest.mark.parametrize("route", ["dict", "yaml"])
@pytest.mark.parametrize("wrapped", [False, True])
@pytest.mark.parametrize("surface", SURFACES)
@pytest.mark.parametrize("value", [-450.0, 0.0, 12.5])
def test_finite_sources_preserve_value_reference_and_form(
    sample_dict, route, wrapped, surface, value, tmp_path
):
    data = copy.deepcopy(sample_dict)
    expected = {"value": value, "ref": REF.copy()} if wrapped else value
    data["sites"][0]["initial_states"][surface]["sdd_id"] = expected
    cfg = _load(data, route, tmp_path)
    assert _field(yaml.safe_load(config_to_engine_yaml(cfg)), surface) == expected
    _, jsons = _grid_config_jsons(cfg)
    assert _field(json.loads(jsons[0]), surface) == expected
    path = tmp_path / "export.yml"
    cfg.to_yaml(str(path))
    assert _field(yaml.safe_load(path.read_text(encoding="utf-8")), surface) == expected
    # Ordinary YAML export remains loadable without changing representation.
    reloaded = SUEWSConfig.from_yaml(str(path))
    assert _field(yaml.safe_load(config_to_engine_yaml(reloaded)), surface) == expected
    column = ("sdd_id", f"({SURFACES.index(surface)},)")
    assert cfg.to_df_state().iloc[0][column] == value


@pytest.mark.parametrize("surface", SURFACES)
def test_omission_keeps_zero_default(sample_dict, surface):
    data = copy.deepcopy(sample_dict)
    data["sites"][0]["initial_states"][surface].pop("sdd_id", None)
    cfg = SUEWSConfig.from_dict(data)
    field = _field(yaml.safe_load(config_to_engine_yaml(cfg)), surface)
    assert (field["value"] if isinstance(field, dict) else field) == pytest.approx(0.0)


@pytest.mark.parametrize("surface", SURFACES)
@pytest.mark.parametrize("value", NONFINITE, ids=["nan", "inf", "minus_inf"])
def test_dataframe_rejection_names_grid_surface_field(
    sample_frame, sample_forcing_parsed, surface, value
):
    frame = sample_frame.copy()
    grid = frame.index[0]
    frame.loc[grid, ("sdd_id", f"({SURFACES.index(surface)},)")] = value
    with pytest.raises(ValueError) as error:
        SUEWSConfig.from_df_state(frame)
    assert f"gridiv={grid}" in str(error.value)
    assert f"initial_states.{surface}.sdd_id" in str(error.value)
    assert "finite" in str(error.value)
    sim = SUEWSSimulation.from_state(frame)
    _, forcing = sample_forcing_parsed(2)
    sim.update_forcing(forcing)
    with pytest.raises(ValueError) as error:
        sim.run(n_jobs=1)
    assert f"gridiv={grid}" in str(error.value)
    assert f"initial_states.{surface}.sdd_id" in str(error.value)


@pytest.mark.parametrize("surface", SURFACES)
@pytest.mark.parametrize("mutation", MUTATIONS)
@pytest.mark.parametrize("value", NONFINITE, ids=["nan", "inf", "minus_inf"])
def test_live_values_rejected_at_exports_and_simulation_handoffs(
    sample_config_loaded, sample_forcing_parsed, surface, mutation, value, tmp_path
):
    cfg = sample_config_loaded.model_copy(deep=True)
    sim = SUEWSSimulation()
    sim.update_config(cfg, auto_load_forcing=False)
    _, forcing = sample_forcing_parsed(2)
    sim.update_forcing(forcing)
    _mutate(cfg, surface, value, mutation)
    path = tmp_path / "must_not_write.yml"
    calls = {
        "dataframe": cfg.to_df_state,
        "engine_yaml": lambda: config_to_engine_yaml(cfg),
        "grid_json": lambda: _grid_config_jsons(cfg),
        "yaml_file": lambda: cfg.to_yaml(str(path)),
        "update_config": lambda: SUEWSSimulation().update_config(
            cfg, auto_load_forcing=False
        ),
        "run": lambda: sim.run(end_date=forcing.index[-1], n_jobs=1),
        "single_runner": lambda: run_suews_rust(
            cfg, forcing, grid_id=cfg.sites[0].gridiv
        ),
        "multi_runner": lambda: run_suews_rust_multi(cfg, forcing),
    }
    for handoff, call in calls.items():
        with pytest.raises(ValueError) as error:
            call()
        message = str(error.value)
        assert "sites[0]" in message, handoff
        assert f"initial_states.{surface}.sdd_id" in message, handoff
        assert "finite" in message, handoff
    assert not path.exists()


@pytest.mark.parametrize("route", ["dict", "yaml", "constructor"])
@pytest.mark.parametrize("wrapped", [False, True])
@pytest.mark.parametrize("surface", SURFACES)
@pytest.mark.parametrize(
    "value",
    [True, False, np.bool_(True), np.bool_(False)],
    ids=["true", "false", "np_true", "np_false"],
)
def test_load_and_construct_reject_boolean_sdd_id(
    sample_dict, route, wrapped, surface, value, tmp_path
):
    data = copy.deepcopy(sample_dict)
    if route == "yaml" and isinstance(value, np.bool_):
        value_for_route = bool(value)
    else:
        value_for_route = value
    data["sites"][0]["initial_states"][surface]["sdd_id"] = (
        {"value": value_for_route, "ref": REF.copy()} if wrapped else value_for_route
    )
    with pytest.raises(ValueError, match="boolean"):
        _load(data, route, tmp_path)


@pytest.mark.parametrize("surface", SURFACES)
@pytest.mark.parametrize(
    "bad_bool",
    [True, False, np.bool_(True), np.bool_(False)],
    ids=["true", "false", "np_true", "np_false"],
)
def test_nested_refvalue_assignment_rejects_booleans_and_accepts_finite(
    sample_config_loaded, surface, bad_bool
):
    """Nested ``state.sdd_id.value = True`` fails before float coercion; finite numbers work."""
    cfg = sample_config_loaded.model_copy(deep=True)
    state = getattr(cfg.sites[0].initial_states, surface)
    before = state.sdd_id.value
    with pytest.raises(ValueError, match="boolean"):
        state.sdd_id.value = bad_bool
    assert state.sdd_id.value == before
    # Replacing with a fresh RefValue binds boolean rejection too.
    state.sdd_id = RefValue[float](value=-17.0, ref=Reference(**REF))
    with pytest.raises(ValueError, match="boolean"):
        state.sdd_id.value = bad_bool
    state.sdd_id.value = 12.5
    assert state.sdd_id.value == 12.5


@pytest.mark.parametrize("mutation", ["typed", "scalar", "nested", "mapping"])
@pytest.mark.parametrize("value", [-450.0, 0.0, 12.5])
def test_finite_replacements_and_dataframe_roundtrip(
    sample_config_loaded, mutation, value
):
    cfg = sample_config_loaded.model_copy(deep=True)
    for surface in SURFACES:
        _mutate(cfg, surface, value, mutation)
    frame = cfg.to_df_state()
    rebuilt = SUEWSConfig.from_df_state(frame)
    for candidate in (cfg, rebuilt):
        data = yaml.safe_load(config_to_engine_yaml(candidate))
        for surface in SURFACES:
            field = _field(data, surface)
            assert (field["value"] if isinstance(field, dict) else field) == value
            if mutation in {"typed", "mapping"} and candidate is cfg:
                assert field["ref"] == REF


def test_finite_mapping_mutation_roundtrips_direct_single_and_multisite(
    sample_config_loaded,
):
    """Live mapping assignment exports the numeric scalar cleanly without mutating ref."""
    cfg = sample_config_loaded.model_copy(deep=True)
    second = cfg.sites[0].model_copy(deep=True)
    cfg.sites[0].gridiv, second.gridiv = 71, 19
    cfg.sites.append(second)
    expected = {71: [-17.0, 0.0, 12.5], 19: [23.0, -21.0, 5.5]}
    for site in cfg.sites:
        for surface, value in zip(SURFACES, expected[site.gridiv], strict=True):
            _mutate(cfg, surface, value, "mapping", site_index=cfg.sites.index(site))
        # Direct vegetation-state export extracts scalar from mapping without mutating it.
        dectr = site.initial_states.dectr
        direct = dectr.to_df_state(site.gridiv)
        assert direct.loc[site.gridiv, ("sdd_id", "(1,)")] == expected[site.gridiv][1]
        assert isinstance(dectr.sdd_id, dict) and dectr.sdd_id["ref"] == REF

    multi_frame = cfg.to_df_state()
    rebuilt = SUEWSConfig.from_df_state(multi_frame)
    for site in rebuilt.sites:
        for surface, value in zip(SURFACES, expected[site.gridiv], strict=True):
            assert getattr(site.initial_states, surface).sdd_id.value == value


@pytest.mark.parametrize("surface", SURFACES)
@pytest.mark.parametrize("value", [None, {"bad": "number"}, "not a number"])
def test_unsafe_malformed_field_has_named_error(sample_config_loaded, surface, value):
    cfg = sample_config_loaded.model_copy(deep=True)
    state = getattr(cfg.sites[0].initial_states, surface)
    object.__setattr__(state, "sdd_id", value)  # noqa: PLC2801 - inject invalid raw state
    with pytest.raises(ValueError) as error:
        config_to_engine_yaml(cfg)
    assert "sites[0]" in str(error.value)
    assert f"initial_states.{surface}.sdd_id" in str(error.value)


@pytest.mark.parametrize("reverse", [False, True])
def test_multisite_order_and_late_invalid_site(sample_config_loaded, reverse):
    cfg = sample_config_loaded.model_copy(deep=True)
    first = cfg.sites[0]
    second = first.model_copy(deep=True)
    first.gridiv, second.gridiv = 71, 19
    cfg.sites = [second, first] if reverse else [first, second]
    expected = {71: [-17.0, 0.0, 12.5], 19: [23.0, -21.0, 0.0]}
    for site in cfg.sites:
        for surface, value in zip(SURFACES, expected[site.gridiv], strict=True):
            getattr(site.initial_states, surface).sdd_id.value = value
    grids, jsons = _grid_config_jsons(cfg)
    assert grids == [site.gridiv for site in cfg.sites]
    rebuilt = SUEWSConfig.from_df_state(cfg.to_df_state())
    assert [site.gridiv for site in rebuilt.sites] == grids
    for grid, payload in zip(grids, jsons, strict=True):
        for surface, value in zip(SURFACES, expected[grid], strict=True):
            assert _field(json.loads(payload), surface)["value"] == value
    _mutate(cfg, "grass", float("nan"), "typed", site_index=1)
    for call in (
        cfg.to_df_state,
        lambda: config_to_engine_yaml(cfg),
        lambda: _grid_config_jsons(cfg),
    ):
        with pytest.raises(
            ValueError, match=r"sites\[1\].*initial_states\.grass\.sdd_id"
        ):
            call()


@pytest.mark.parametrize("surface", SURFACES)
@pytest.mark.parametrize(
    "route",
    [
        "dict_bare",
        "dict_wrapped",
        "yaml_bare",
        "yaml_wrapped",
        "frame",
        "nested",
        "typed",
        "scalar",
    ],
)
def test_signed_finite_state_reaches_daily_output(
    sample_dict,
    sample_config_loaded,
    sample_forcing_parsed,
    surface,
    route,
    tmp_path,
    record_property,
):
    """The sample's first day retains negative SDD, distinct from zero.

    Positive SDD remains admissible; daily phenology may reset it according
    to its existing science. This test does not impose a positive bound.
    """
    _, forcing = sample_forcing_parsed(288)
    outputs = {}
    for value in (-17.0, 0.0, 12.5):
        cfg = sample_config_loaded.model_copy(deep=True)
        if route.startswith(("dict_", "yaml_")):
            data = copy.deepcopy(sample_dict)
            data["sites"][0]["initial_states"][surface]["sdd_id"] = (
                {"value": value, "ref": REF.copy()}
                if route.endswith("wrapped")
                else value
            )
            cfg = _load(data, route.split("_")[0], tmp_path)
        elif route == "frame":
            frame = cfg.to_df_state()
            frame.loc[frame.index[0], ("sdd_id", f"({SURFACES.index(surface)},)")] = (
                value
            )
            cfg = SUEWSConfig.from_df_state(frame)
        else:
            _mutate(cfg, surface, value, route)
        sim = SUEWSSimulation()
        sim.update_config(cfg, auto_load_forcing=False)
        sim.update_forcing(forcing)
        output = sim.run(end_date=forcing.index[-1], n_jobs=1)
        fluxes = output.df["SUEWS"][["QN", "QH", "QE", "QS"]]
        assert not fluxes.empty
        assert np.isfinite(fluxes.to_numpy()).all()
        outputs[value] = output.df
    column = (
        "DailyState",
        {"evetr": "SDD_EveTr", "dectr": "SDD_DecTr", "grass": "SDD_Grass"}[surface],
    )
    for value, frame in outputs.items():
        record_property(
            f"{route}.{surface}.{value}.daily_sdd",
            json.dumps(frame[column].dropna().unique().tolist()),
        )
    assert not outputs[-17.0][column].equals(outputs[0.0][column])


@pytest.mark.parametrize("reverse", [False, True])
def test_multisite_simulation_matches_each_grid_alone(
    sample_config_loaded, sample_forcing_parsed, reverse
):
    """Distinct SDD values stay on their own grids through the actual engine."""
    cfg = sample_config_loaded.model_copy(deep=True)
    second = cfg.sites[0].model_copy(deep=True)
    cfg.sites[0].gridiv, second.gridiv = 71, 19
    cfg.sites.append(second)
    for index, site in enumerate(cfg.sites):
        for column, surface in enumerate(SURFACES):
            getattr(site.initial_states, surface).sdd_id.value = (
                -17.0 - 11 * index - column
            )
    if reverse:
        cfg.sites.reverse()
    _, forcing = sample_forcing_parsed(288)
    combined, _, _ = run_suews_rust_multi(cfg, forcing, serial_mode=True)
    assert set(combined.index.get_level_values("grid")) == {71, 19}
    for site in cfg.sites:
        single = cfg.model_copy(deep=True)
        single.sites = [site.model_copy(deep=True)]
        alone, _, _ = run_suews_rust_multi(single, forcing, serial_mode=True)
        pd.testing.assert_frame_equal(
            combined.xs(site.gridiv, level="grid"),
            alone.xs(site.gridiv, level="grid"),
            check_exact=True,
        )
