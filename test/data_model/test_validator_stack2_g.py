"""Unit and integration tests for Stack 2 g: style ledger, patch normalisation, and read-only typed model."""

from __future__ import annotations

from collections import deque
from copy import copy, deepcopy
from dataclasses import FrozenInstanceError, dataclass
import datetime
from enum import Enum, IntEnum, StrEnum
import functools
from importlib.resources import files
import ipaddress
import math
from pathlib import Path
from types import MappingProxyType
from zoneinfo import ZoneInfo

import numpy as np
from pydantic import (
    BaseModel,
    ConfigDict,
    Field,
    PrivateAttr,
    computed_field,
    field_serializer,
)
import pytest
import yaml

import supy
from supy.data_model.core.config import SUEWSConfig
from supy.data_model.validation.engine import ValidationEngine, validate_config
from supy.data_model.validation.pipeline.document import (
    ConfigDocument,
    ConfigPatch,
    MappingKey,
    PathStyle,
    StyleLedger,
    apply_nullify_tree_at_path,
    apply_patches_in_place,
    canonicalise_path,
    deep_freeze,
    format_path_tokens,
    nullify_tree_in_place,
    parse_path_tokens,
    path_exists,
    read_raw_path,
    thaw_copy,
)
from supy.data_model.validation.pipeline.patch_normaliser import (
    collect_nullifiable_entries,
    collect_nullifiable_values,
    invert_patches,
    normalise_patches,
    read_original_unwrapped,
)
from supy.data_model.validation.pipeline.patches import (
    PatchPlan,
    PlannedAdjustment,
    plan_land_cover_fraction_patches,
    plan_model_dependent_nullification_patches,
    plan_rcmethod_patches,
    plan_seasonal_patches,
    plan_setpointmethod_patches,
    plan_stebbsmethod_wwr_patches,
)
from supy.data_model.validation.pipeline.phase_b_rules import (
    RulesRegistry,
    ValidationContext,
)
from supy.data_model.validation.pipeline.style_ledger import dump_yaml_with_style
from supy.data_model.validation.rules import (
    ControlView,
    LandCoverView,
    PhysicsView,
    ReadOnlyDataclassProxy,
    ReadOnlyEnumProxy,
    ReadOnlyFieldInfo,
    ReadOnlyModelProxy,
    SiteView,
    SurfaceView,
    TypedConfigView,
    VerticalLayersView,
    freeze_model,
)

pytestmark = [
    pytest.mark.api,
    pytest.mark.cfg,
    pytest.mark.contract("C-VALID", "C-ERROR", "C-SCHEMA"),
]


def _load_sample_text() -> str:
    return (files("supy") / "sample_data" / "sample_config.yml").read_text(
        encoding="utf-8"
    )


# ---------------------------------------------------------------------------
# Pillar 1: StyleLedger & style-preserving ConfigDocument
# ---------------------------------------------------------------------------


def test_style_ledger_captures_yaml_ast_styles_lines_and_comments() -> None:
    """StyleLedger.from_yaml_text records wrapper, flow, quote, key-order, line, and comments."""
    raw_yaml = (
        "# Top file header\n"
        "# Second comment line\n"
        "name: 'London'\n"
        'description: "Urban test"\n'
        "sites:\n"
        "  - name: 'KCL'\n"
        "    gridiv: {value: 42, ref: {desc: 'site id'}}\n"
        "    properties:\n"
        "      lat: 51.5\n"
        "      lng: {value: -0.1, ref: {DOI: '10.1000/lng'}}\n"
        "      ie_start: {ref: {desc: 'disabled placeholder'}}\n"
        "      dz: [0.2, 0.1, 0.5]\n"
        "      k: {value: [1.2, 1.1, 1.5], ref: {ID: 'K1'}}\n"
    )
    doc = ConfigDocument.from_text(raw_yaml, normalise=False)
    ledger = doc.style_ledger

    assert ledger.leading_comments == ("# Top file header", "# Second comment line")
    assert ledger.wrapper_style("name") == "bare"
    assert ledger.get("name").scalar_style == "'"
    assert ledger.source_line("name") == 3
    assert ledger.get("description").scalar_style == '"'
    assert ledger.wrapper_style("sites[0].gridiv") == "ref_value"
    assert ledger.get("sites[0].gridiv").flow_style is True
    assert ledger.ref_metadata("sites[0].gridiv") == {"desc": "site id"}
    assert ledger.wrapper_style("sites[0].properties.lat") == "bare"
    assert ledger.wrapper_style("sites[0].properties.lng") == "ref_value"
    assert ledger.wrapper_style("sites[0].properties.ie_start") == "ref_only"
    assert ledger.ref_metadata("sites[0].properties.ie_start") == {
        "desc": "disabled placeholder"
    }
    assert ledger.get("sites[0].properties.dz").flow_style is True
    assert ledger.get("sites[0].properties.k").flow_style is True
    assert ledger.get("sites[0].properties.k").value_flow_style is True

    # Ledger entries and ref metadata are deeply immutable.
    with pytest.raises(TypeError):
        ledger.entries["name"] = PathStyle()  # type: ignore[index]
    with pytest.raises(TypeError):
        ledger.ref_metadata("sites[0].gridiv")["desc"] = "mutated"  # type: ignore[index]


def test_style_ledger_preserves_style_across_delete_reinsert_rename_and_dump() -> None:
    """Deleting and re-inserting or renaming paths preserves authored style and YAML layout."""
    raw_yaml = (
        "# Author header\n"
        "name: 'London'\n"
        "label: plain_text\n"
        "sites:\n"
        "  - gridiv: 1\n"
        "    properties:\n"
        "      soildepth: {value: 350.0, ref: {desc: 'legacy depth'}}\n"
        "      snowalb: 0.5\n"
        "      dz: [0.2, 0.1, 0.5]\n"
    )
    doc = ConfigDocument.from_text(raw_yaml, normalise=True)

    # Rename normalised soildepth -> soil_depth while carrying style and ref.
    assert doc.read("sites[0].properties.soil_depth") == pytest.approx(350.0)
    assert (
        doc.style_ledger.wrapper_style("sites[0].properties.soil_depth") == "ref_value"
    )
    assert doc.style_ledger.ref_metadata("sites[0].properties.soil_depth") == {
        "desc": "legacy depth"
    }

    # Delete bare snowalb and wrapped soil_depth, then re-set both:
    # the style ledger restores bare style for snowalb and RefValue+ref for soil_depth.
    after_del = doc.apply_patches([
        ConfigPatch("sites[0].properties.snowalb", "delete"),
        ConfigPatch("sites[0].properties.soil_depth", "delete"),
    ])
    assert "snowalb" not in after_del.data["sites"][0]["properties"]
    assert "soil_depth" not in after_del.data["sites"][0]["properties"]

    restored = after_del.apply_patches([
        ConfigPatch("sites[0].properties.snowalb", "set", 0.6),
        ConfigPatch("sites[0].properties.soil_depth", "set", 400.0),
        ConfigPatch("name", "set", "Paris"),
        # Patch plain string to YAML-reserved string "true": must remain str on round-trip!
        ConfigPatch("label", "set", "true"),
    ])
    assert restored.data["sites"][0]["properties"]["snowalb"] == pytest.approx(0.6)
    assert restored.data["sites"][0]["properties"]["soil_depth"] == {
        "value": 400.0,
        "ref": {"desc": "legacy depth"},
    }

    dumped = restored.dump_yaml(preserve_style=True, preserve_leading_comments=True)
    assert dumped.startswith("# Author header\n")
    assert "name: 'Paris'" in dumped
    assert "[0.2, 0.1, 0.5]" in dumped
    assert "{value: 400.0, ref: {desc: legacy depth}}" in dumped
    # Round-trip invariant: yaml.safe_load matches data exactly (including str "true").
    reloaded = yaml.safe_load(dumped)
    assert reloaded == restored.data
    assert reloaded["label"] == "true"
    assert isinstance(reloaded["label"], str)


def test_style_ledger_dominant_style_and_remap_paths() -> None:
    """dominant_wrapper_style reflects majority authoring style and remap_paths copies subtrees."""
    bare_ledger = StyleLedger.from_mapping({"a": 1, "b": 2, "c": {"value": 3}})
    assert bare_ledger.dominant_wrapper_style == "bare"

    wrapped_ledger = StyleLedger.from_mapping({
        "a": {"value": 1},
        "b": {"value": 2},
        "c": 3,
    })
    assert wrapped_ledger.dominant_wrapper_style == "ref_value"

    remapped = bare_ledger.remap_paths({"c": "nested.c"})
    assert remapped.wrapper_style("nested.c") == "ref_value"
    assert "c" in remapped and "nested.c" in remapped
    assert StyleLedger.empty().paths() == ()


# ---------------------------------------------------------------------------
# Pillar 2: Patch Normalisation Carrying Original Values & Inversion
# ---------------------------------------------------------------------------


def test_normalise_patches_captures_original_values_existence_and_style() -> None:
    """normalise_patches fills old_value, had_original, original_style, and original_ref."""
    doc = ConfigDocument.from_dict(
        {
            "sites": [
                {
                    "properties": {
                        "lat": {"value": 51.5, "ref": {"desc": "obs"}},
                        "lng": -0.1,
                        "nullable": None,
                        "placeholder": {"ref": {"desc": "keep"}},
                    }
                }
            ]
        },
        normalise=False,
    )
    snapshot = deepcopy(doc.data)

    raw_patches = [
        ConfigPatch("sites[0].properties.lat", "set", 52.0),
        ConfigPatch("sites[0].properties.lng", "set", 0.2),
        ConfigPatch("sites[0].properties.nullable", "set", 10.0),
        ConfigPatch(
            "sites[0].properties.brand_new", "set", 99.0, default_wrapped=False
        ),
        ConfigPatch("sites[0].properties.placeholder", "set", 7.0),
    ]
    normalised = normalise_patches(raw_patches, doc)

    # Target document was never mutated.
    assert doc.data == snapshot
    assert len(normalised) == 5

    by_path = {p.path: p for p in normalised}
    assert by_path["sites[0].properties.lat"].old_value == pytest.approx(51.5)
    assert by_path["sites[0].properties.lat"].had_original is True
    assert by_path["sites[0].properties.lat"].original_style == "ref_value"
    assert by_path["sites[0].properties.lat"].original_ref == {"desc": "obs"}

    assert by_path["sites[0].properties.lng"].old_value == pytest.approx(-0.1)
    assert by_path["sites[0].properties.lng"].had_original is True
    assert by_path["sites[0].properties.lng"].original_style == "bare"

    assert by_path["sites[0].properties.nullable"].old_value is None
    assert by_path["sites[0].properties.nullable"].had_original is True

    assert by_path["sites[0].properties.brand_new"].old_value is None
    assert by_path["sites[0].properties.brand_new"].had_original is False

    assert by_path["sites[0].properties.placeholder"].old_value is None
    assert by_path["sites[0].properties.placeholder"].had_original is True
    assert by_path["sites[0].properties.placeholder"].original_style == "ref_only"
    assert by_path["sites[0].properties.placeholder"].original_ref == {"desc": "keep"}

    # Apply forward, then invert: exact lossless round-trip back to original doc.data!
    applied = doc.apply_patches(normalised)
    reverted = applied.apply_patches(invert_patches(normalised))
    assert reverted.data == snapshot


def test_normalise_patches_expands_nullify_tree_matches_in_place_and_inverts() -> None:
    """Expanding nullify_tree matches nullify_tree_in_place and inverts losslessly."""
    tree = {
        "ref": {"desc": "section provenance"},
        "stebbs": {
            "alpha": {"value": 1.5, "ref": {"ID": "A1"}},
            "beta": 2.5,
            "vec": {"value": [1.0, 2.0], "ref": {"ID": "V1"}},
            "bare_vec": [3.0, 4.0],
            "ref_only": {"ref": {"desc": "placeholder"}},
            "keep_key": 9.9,
            "wrapped_list": {
                "value": [
                    {"value": 10, "ref": {"source": "obs"}},
                    {"ref": {"source": "empty"}},
                ],
                "ref": {"source": "array"},
            },
        },
    }
    doc = ConfigDocument.from_dict(tree, normalise=False)
    nullify_patch = ConfigPatch(
        "stebbs", "nullify_tree", skip_keys=("keep_key",), reason="STEBBS disabled"
    )

    # 1. Expanded atomic patches carry original values and match nullify_tree_in_place.
    expanded = doc.normalise_patches([nullify_patch], expand_nullify_tree=True)
    assert all(p.action == "set" for p in expanded)
    assert all(p.had_original is True for p in expanded)
    old_by_path = {p.path: p.old_value for p in expanded}
    assert old_by_path == {
        "stebbs.alpha": 1.5,
        "stebbs.beta": 2.5,
        "stebbs.vec": [1.0, 2.0],
        "stebbs.bare_vec[0]": 3.0,
        "stebbs.bare_vec[1]": 4.0,
        "stebbs.wrapped_list.value[0]": 10,
    }

    expected_in_place = deepcopy(tree)
    nullify_tree_in_place(
        expected_in_place["stebbs"], skip_keys=frozenset({"keep_key"})
    )
    applied = doc.apply_patches(expanded)
    assert applied.data == expected_in_place

    # Inverting expanded patches restores the exact original tree including ref metadata.
    reverted = applied.apply_patches(invert_patches(expanded))
    assert reverted.data == tree

    # 2. Snapshot mode (expand_nullify_tree=False) retains nullify_tree with original leaf map.
    unexpanded = doc.normalise_patches([nullify_patch], expand_nullify_tree=False)
    assert len(unexpanded) == 1
    assert unexpanded[0].action == "nullify_tree"
    assert unexpanded[0].had_original is True
    assert unexpanded[0].old_value == {
        "alpha": 1.5,
        "beta": 2.5,
        "vec": [1.0, 2.0],
        "bare_vec[0]": 3.0,
        "bare_vec[1]": 4.0,
        "wrapped_list.value[0]": 10,
    }


def test_normalise_patches_coalescing_barriers_and_type_safe_noops() -> None:
    """Coalescing keeps initial old_value, respects ancestor/descendant barriers, and drops true no-ops."""
    doc = ConfigDocument.from_dict(
        {"a": 1, "b": {"x": 10}, "flag": True, "nan_val": float("nan")},
        normalise=False,
    )

    # Sequential same-path chain: set(a, 2) -> set(a, 3) coalesces with old_value=1.
    chained = doc.normalise_patches([
        ConfigPatch("a", "set", 2),
        ConfigPatch("a", "set", 3),
    ])
    assert len(chained) == 1
    assert chained[0].old_value == 1
    assert chained[0].new_value == 3

    # Set then delete on originally absent key cancels out completely.
    cancelled = doc.normalise_patches([
        ConfigPatch("absent_key", "set", 42),
        ConfigPatch("absent_key", "delete"),
    ])
    assert cancelled == []

    # Delete then restore to original value cancels out as a net no-op.
    restored_noop = doc.normalise_patches([
        ConfigPatch("a", "delete"),
        ConfigPatch("a", "set", 1),
    ])
    assert restored_noop == []

    # Type-safe equality: setting int 1 -> bool True is NOT a no-op; nan -> nan IS a no-op.
    type_change = doc.normalise_patches([
        ConfigPatch("a", "set", True),
        ConfigPatch("flag", "set", 1),
        ConfigPatch("nan_val", "set", float("nan")),
        ConfigPatch("never_existed", "delete"),
    ])
    assert [p.path for p in type_change] == ["a", "flag"]
    assert type_change[0].old_value == 1 and type_change[0].new_value is True
    assert type_change[1].old_value is True and type_change[1].new_value == 1

    # Ancestor/descendant barrier: set(b.x, 20) -> delete(b) -> set(b.x, 30) must NOT merge across delete(b).
    barrier_patches = [
        ConfigPatch("b.x", "set", 20),
        ConfigPatch("b", "delete"),
        ConfigPatch("b.x", "set", 30, default_wrapped=False),
    ]
    norm_barrier = doc.normalise_patches(barrier_patches)
    assert len(norm_barrier) == 3
    assert (
        doc.apply_patches(norm_barrier).data == doc.apply_patches(barrier_patches).data
    )
    assert doc.apply_patches(norm_barrier).apply_patches(
        invert_patches(norm_barrier)
    ).data == {
        "a": 1,
        "b": {"x": 10},
        "flag": True,
        "nan_val": pytest.approx(float("nan"), nan_ok=True),
    }


def test_patch_plan_normalised_and_phase_b_planners_carry_original_values() -> None:
    """All Phase B patch planners carry original values and invert cleanly via PatchPlan.normalised."""
    cfg = {
        "model": {
            "physics": {
                "stebbs": {
                    "enabled": False,
                    "parameters": "default",
                    "capacitance": 0,
                    "setpoint": 0,
                },
                "emissions": 2,
            }
        },
        "sites": [
            {
                "gridiv": 7,
                "properties": {
                    "lat": 51.5,
                    "lng": -0.1,
                    "timezone": 1,
                    "land_cover": {
                        "paved": {"sfr": "0.600002"},
                        "bldgs": {"sfr": {"value": 0.4, "ref": {"ID": "b"}}},
                    },
                    "stebbs": {
                        "internal_shading": {"value": 1, "ref": {"ID": "sh"}},
                    },
                    "anthropogenic_emissions": {
                        "co2": {"emission_co2_point_source": 1.2},
                    },
                    "building_archetype": {
                        "fraction_heat_capacity_wall_external": 0.25,
                        "profile_setpoint_temperature_heating_air": {
                            "working_day": {"1": 20.0},
                        },
                    },
                },
                "initial_states": {
                    "snowalb": 0.55,
                },
            }
        ],
    }
    doc = ConfigDocument.from_dict(cfg, normalise=False)

    lc_plan = plan_land_cover_fraction_patches(doc.data)
    assert lc_plan.patches[0].old_value == "0.600002"
    assert lc_plan.patches[0].had_original is True

    null_plan = plan_model_dependent_nullification_patches(doc.data)
    assert all(p.action == "nullify_tree" for p in null_plan.patches)
    assert all(isinstance(p.old_value, dict) and p.old_value for p in null_plan.patches)
    assert all(p.had_original is True for p in null_plan.patches)

    norm_null = null_plan.normalised(doc)
    assert all(p.action == "set" for p in norm_null.patches)
    assert all(p.had_original is True for p in norm_null.patches)
    applied = norm_null.apply_to_document(doc)
    reverted = applied.apply_patches(norm_null.invert())
    assert reverted.data == doc.data

    # Seasonal delete of snowalb + timezone patch also round-trip losslessly.
    seasonal = plan_seasonal_patches(doc.data, "2012-07-01", 2012).normalised(doc)
    assert seasonal.patches
    assert (
        seasonal.apply_to_document(doc).apply_patches(seasonal.invert()).data
        == doc.data
    )

    # Empty planners for inactive options normalise cleanly to empty plans.
    assert plan_rcmethod_patches(doc.data).normalised(doc).patches == []
    assert plan_stebbsmethod_wwr_patches(doc.data).normalised(doc).patches == []
    sp_plan = plan_setpointmethod_patches(doc.data, skip_when_stebbs_off=False)
    assert sp_plan.patches[0].old_value == {"1": 20.0}
    sp_norm = sp_plan.normalised(doc)
    assert (
        sp_norm.apply_to_document(doc).apply_patches(sp_norm.invert()).data == doc.data
    )


def test_patch_normaliser_error_guards_and_defensive_copies() -> None:
    """Error cases raise ValueError and ConfigPatch isolates mutable caller payloads."""
    caller_list = [1.0, 2.0]
    patch = ConfigPatch("x", "set", new_value=caller_list, old_value={"k": [3]})
    caller_list.append(99.0)
    assert patch.new_value == [1.0, 2.0]

    with pytest.raises(ValueError, match="unexpanded 'nullify_tree'"):
        invert_patches([ConfigPatch("a", "nullify_tree", had_original=True)])
    with pytest.raises(ValueError, match="had_original is unknown"):
        invert_patches([ConfigPatch("a", "set", 1)])
    with pytest.raises(ValueError, match="Malformed YAML path"):
        canonicalise_path("sites[0]..lat")
    with pytest.raises(ValueError, match="Unsupported patch action"):
        normalise_patches([ConfigPatch("a", "bogus")], {})  # type: ignore[arg-type]

    assert format_path_tokens(parse_path_tokens("sites[0].properties.lat")) == (
        "sites[0].properties.lat"
    )
    assert path_exists({"a": None}, "a") is True
    assert path_exists({"a": None}, "b") is False
    assert read_raw_path({"a": {"value": 5}}, "a") == {"value": 5}
    assert read_original_unwrapped({"a": {"value": 5}}, "a") == (True, 5)
    assert read_original_unwrapped({}, "a") == (False, None)
    assert collect_nullifiable_entries({"x": 1}, prefix="p") == [("p.x", 1, None)]
    assert collect_nullifiable_values({"x": 1}) == {"x": 1}


# ---------------------------------------------------------------------------
# Pillar 3: Read-Only Typed Model (TypedConfigView + ReadOnlyModelProxy)
# ---------------------------------------------------------------------------


def test_typed_config_view_is_deeply_immutable_and_provides_typed_subviews() -> None:
    """TypedConfigView deeply freezes raw dicts, stebbs_block, and exposes typed sub-views."""
    raw = {
        "model": {
            "control": {
                "start_time": "2012-01-01",
                "end_time": "2012-01-02",
                "tstep": {"value": 300},
                "forcing": {"file": "forcing.txt"},
            },
            "physics": {
                "net_radiation": 1003,
                "storage_heat": 5,
                "stebbs": {"enabled": True, "parameters": "default", "capacitance": 0},
            },
        },
        "sites": [
            {
                "name": "London",
                "gridiv": {"value": 12},
                "properties": {
                    "lat": {"value": 51.5},
                    "lng": -0.1,
                    "alt": 40.0,
                    "timezone": 0,
                    "z": 35.0,
                    "land_cover": {
                        "paved": {"sfr": 0.4},
                        "bldgs": {"sfr": {"value": 0.6}},
                    },
                    "vertical_layers": {
                        "nlayer": {"value": 2},
                        "height": [0.0, 10.0, 20.0],
                        "building_frac": [0.6, 0.3],
                        "veg_frac": [0.0, 0.0],
                    },
                },
            }
        ],
    }

    view = TypedConfigView.from_mapping(raw)
    assert isinstance(view.control, ControlView)
    assert isinstance(view.physics, PhysicsView)
    assert isinstance(view.sites[0], SiteView)
    assert view.control.start_time == "2012-01-01"
    assert view.control.end_time == "2012-01-02"
    assert view.control.tstep == pytest.approx(300.0)
    assert view.control.forcing_file == "forcing.txt"

    site = view.sites[0]
    assert site.lat == pytest.approx(51.5)
    assert site.lng == pytest.approx(-0.1)
    assert site.alt == pytest.approx(40.0)
    assert site.timezone == pytest.approx(0.0)
    assert site.z == pytest.approx(35.0)

    lc_view = site.land_cover_view
    assert isinstance(lc_view, LandCoverView)
    assert isinstance(lc_view.surface("bldgs"), SurfaceView)
    assert lc_view.sfr("bldgs") == pytest.approx(0.6)
    assert lc_view.active_surfaces() == ("paved", "bldgs")
    assert dict(lc_view.fraction_map()) == {"paved": 0.4, "bldgs": 0.6}

    vl_view = site.vertical_layers_view
    assert isinstance(vl_view, VerticalLayersView)
    assert vl_view.nlayer == 2
    assert vl_view.heights == (0.0, 10.0, 20.0)
    assert vl_view.building_frac == (0.6, 0.3)
    assert vl_view.last_nonzero_height == pytest.approx(20.0)

    # Mutating caller's dict after creating view does not affect the frozen view.
    raw["sites"][0]["properties"]["lat"]["value"] = -80.0
    assert site.lat == pytest.approx(51.5)

    # Every container on the view is deeply immutable.
    with pytest.raises(FrozenInstanceError):
        view.physics = PhysicsView()  # type: ignore[misc]
    with pytest.raises(TypeError):
        view.raw["model"] = {}  # type: ignore[index]
    with pytest.raises(TypeError):
        view.physics.raw["storage_heat"] = 1  # type: ignore[index]
    with pytest.raises(TypeError):
        view.physics.stebbs_block["capacitance"] = 2  # type: ignore[index]
    with pytest.raises(TypeError):
        site.properties["lat"] = 0.0  # type: ignore[index]
    with pytest.raises(TypeError):
        site.land_cover["paved"]["sfr"] = 0.9  # type: ignore[index]
    with pytest.raises(TypeError):
        lc_view.surfaces["paved"] = SurfaceView("paved")  # type: ignore[index]
    assert isinstance(deep_freeze({"k": [1, {2}]}), MappingProxyType)


def test_readonly_model_proxy_blocks_mutations_and_isolates_from_caller() -> None:
    """freeze_model wraps SUEWSConfig in a deeply immutable proxy isolated from caller edits."""
    sample_dict = yaml.safe_load(_load_sample_text())
    cfg = SUEWSConfig.from_dict(deepcopy(sample_dict))
    orig_lat = cfg.sites[0].properties.lat.value

    ro = freeze_model(cfg)
    assert isinstance(ro, ReadOnlyModelProxy)
    assert freeze_model(ro) is ro
    assert ro.model_type is SUEWSConfig
    assert ro == cfg
    assert isinstance(ro.model_fields_set, frozenset)
    assert isinstance(ro.sites, tuple)
    assert isinstance(ro.sites[0], ReadOnlyModelProxy)
    assert ro.sites[0].properties.lat.value == orig_lat

    # Mutating the original SUEWSConfig does NOT leak into the frozen proxy.
    cfg.sites[0].properties.lat.value = -45.0
    assert ro.sites[0].properties.lat.value == orig_lat
    assert ro != cfg

    # Every mutation attempt on the proxy raises TypeError.
    with pytest.raises(TypeError, match="Cannot mutate attribute"):
        ro.sites = []  # type: ignore[misc]
    with pytest.raises(TypeError, match="Cannot mutate attribute"):
        ro.sites[0].properties.lat.value = 10.0
    with pytest.raises(TypeError, match="Cannot delete attribute"):
        del ro.model
    with pytest.raises(AttributeError):
        _ = ro._target

    # model_dump() returns a fresh copy; mutating it never affects the proxy.
    dumped = ro.model_dump(mode="json", round_trip=True)
    dumped["sites"][0]["properties"]["lat"]["value"] = 0.0
    assert ro.sites[0].properties.lat.value == orig_lat

    # TypedConfigView.from_config and proxy.to_typed_view agree with ConfigDocument.typed_view.
    doc = ConfigDocument.from_dict(sample_dict, normalise=True)
    v_doc = doc.typed_view()
    v_proxy = ro.to_typed_view()
    assert v_proxy.sites[0].lat == pytest.approx(v_doc.sites[0].lat)
    assert v_proxy.sites[0].gridid == v_doc.sites[0].gridid

    # ValidationContext auto-freezes typed_model and pre-builds cached view.
    ctx = ValidationContext(yaml_data=sample_dict, typed_model=cfg)
    assert isinstance(ctx.typed_model, ReadOnlyModelProxy)
    assert ctx.view is ctx.view
    assert ctx.view.sites[0].gridid == v_doc.sites[0].gridid

    with pytest.raises(TypeError, match="typed_model must be"):
        ValidationContext(yaml_data=sample_dict, typed_model="invalid")  # type: ignore[arg-type]

    # Public supy.validate_config accepts a ReadOnlyModelProxy directly.
    report = supy.validate_config(ro, pipeline="ABC", forcing="off")
    assert report.is_valid


def test_readonly_model_proxy_freezes_numpy_arrays_and_phase_b_reuses_cached_view(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    """NumPy arrays inside models are non-writeable and RulesRegistry reuses context.view."""

    class _ArrayModel(BaseModel):
        model_config = ConfigDict(arbitrary_types_allowed=True)
        arr: np.ndarray

    m = _ArrayModel(arr=np.array([1.0, 2.0, 3.0]))
    ro = freeze_model(m)
    assert ro.arr.flags.writeable is False
    with pytest.raises(ValueError, match="read-only"):
        ro.arr[0] = 99.0

    # Verify RulesRegistry.run_validation uses context.view without calling from_mapping per rule.
    sample_dict = yaml.safe_load(_load_sample_text())
    ctx = ValidationContext(yaml_data=sample_dict, model_year=2012)

    from_mapping_calls = 0
    orig_from_mapping = TypedConfigView.from_mapping.__func__

    @classmethod
    def _counting_from_mapping(cls, data):
        nonlocal from_mapping_calls
        from_mapping_calls += 1
        return orig_from_mapping(cls, data)

    monkeypatch.setattr(TypedConfigView, "from_mapping", _counting_from_mapping)
    results = RulesRegistry(context=ctx).run_validation()
    assert results
    assert from_mapping_calls == 0
    assert math.isfinite(ctx.view.sites[0].lat or 0.0)


def test_coalescing_preserves_insertion_wrapper_representation_and_rollback() -> None:
    """Coalescing keeps the representation established by the first insertion and rolls back ancestors/raw entries."""
    # Both orders of default_wrapped on newly inserted keys match sequential application.
    for first_wrap, second_wrap in ((False, True), (True, False)):
        patches = [
            ConfigPatch("a", "set", 1, default_wrapped=first_wrap),
            ConfigPatch("a", "set", 2, default_wrapped=second_wrap),
        ]
        seq_target: dict[str, object] = {}
        apply_patches_in_place(seq_target, patches)

        norm = normalise_patches(patches, {})
        norm_target: dict[str, object] = {}
        apply_patches_in_place(norm_target, norm)
        assert norm_target == seq_target

        apply_patches_in_place(norm_target, invert_patches(norm))
        assert norm_target == {}

    # Implicit parent creation is undone cleanly for both missing and null ancestors.
    for initial in ({}, {"a": None}):
        base = deepcopy(initial)
        patches = [
            ConfigPatch("a.b.c", "set", 1, default_wrapped=False),
            ConfigPatch("a.d", "set", 2, default_wrapped=False),
        ]
        norm = normalise_patches(patches, base)
        working = deepcopy(base)
        apply_patches_in_place(working, norm)
        assert working == {"a": {"b": {"c": 1}, "d": 2}}
        apply_patches_in_place(working, invert_patches(norm))
        assert working == initial

    # Bare overwritten by RefValue dict restores exact bare entry on invert.
    bare_base = {"a": 1}
    wrap_patches = normalise_patches(
        [ConfigPatch("a", "set", {"value": 2}, default_wrapped=False)], bare_base
    )
    bare_work = deepcopy(bare_base)
    apply_patches_in_place(bare_work, wrap_patches)
    assert bare_work == {"a": {"value": 2}}
    apply_patches_in_place(bare_work, invert_patches(wrap_patches))
    assert bare_work == {"a": 1}

    # Explicit {"value": 1, "ref": None} deleted and restored retains the "ref": None key.
    null_ref_base = {"a": {"value": 1, "ref": None}}
    del_patches = normalise_patches([ConfigPatch("a", "delete")], null_ref_base)
    del_work = deepcopy(null_ref_base)
    apply_patches_in_place(del_work, del_patches)
    assert del_work == {}
    apply_patches_in_place(del_work, invert_patches(del_patches))
    assert del_work == {"a": {"value": 1, "ref": None}}


def test_nullification_preserves_key_identity_and_unexpanded_scalar_targets() -> None:
    """Expanded nullification matches direct nullification for int/punctuated keys; unexpanded keeps scalar roots."""
    raw_special = {
        "profile": {
            1: 7,
            "1": 8,
            "a.b": 3,
            "x[0]": 4,
        }
    }
    direct = deepcopy(raw_special)
    assert apply_nullify_tree_at_path(direct, "profile") is True
    assert direct == {"profile": {1: None, "1": None, "a.b": None, "x[0]": None}}

    expanded = normalise_patches(
        [ConfigPatch("profile", "nullify_tree")], raw_special, expand_nullify_tree=True
    )
    via_expanded = deepcopy(raw_special)
    apply_patches_in_place(via_expanded, expanded)
    assert via_expanded == direct

    apply_patches_in_place(via_expanded, invert_patches(expanded))
    assert via_expanded == raw_special

    # Unexpanded nullify_tree on a scalar target keeps the patch and records {"": old}.
    scalar_base = {"x": 1}
    unexpanded = normalise_patches(
        [ConfigPatch("x", "nullify_tree")], scalar_base, expand_nullify_tree=False
    )
    assert len(unexpanded) == 1
    assert unexpanded[0].action == "nullify_tree"
    assert unexpanded[0].old_value == {"": 1}

    applied_unexp = deepcopy(scalar_base)
    apply_patches_in_place(applied_unexp, unexpanded)
    applied_exp = deepcopy(scalar_base)
    apply_patches_in_place(
        applied_exp,
        normalise_patches(
            [ConfigPatch("x", "nullify_tree")], scalar_base, expand_nullify_tree=True
        ),
    )
    assert applied_unexp == applied_exp == {"x": None}


def test_defensive_copying_immutability_and_scalar_sequence_quoting() -> None:
    """Proxy-backed inputs are detached, tuple payloads copied, and scalar list quotes preserved."""
    # Caller-supplied MappingProxyType backing dicts cannot mutate StyleLedger/PathStyle/ConfigPatch.
    backing_entries = {"a": PathStyle(wrapper_style="bare")}
    ledger = StyleLedger(entries=MappingProxyType(backing_entries))
    backing_entries["b"] = PathStyle(wrapper_style="ref_value")
    assert "b" not in ledger

    backing_ref = {"desc": "orig", "nested": {"k": 1}}
    style = PathStyle(wrapper_style="ref_value", ref=MappingProxyType(backing_ref))
    backing_ref["desc"] = "mutated"
    backing_ref["nested"]["k"] = 99
    assert style.ref is not None
    assert style.ref["desc"] == "orig"
    assert style.ref["nested"]["k"] == 1

    backing_patch_ref = {"desc": "patch-ref"}
    payload = ([1], {"k": [2]})
    old_payload = ([3],)
    patch = ConfigPatch(
        "x",
        "set",
        payload,
        old_value=old_payload,
        original_ref=MappingProxyType(backing_patch_ref),
    )
    backing_patch_ref["desc"] = "tampered"
    payload[0].append(99)
    payload[1]["k"].append(88)
    old_payload[0].append(77)
    assert patch.original_ref is not None
    assert patch.original_ref["desc"] == "patch-ref"
    assert patch.new_value == ([1], {"k": [2]})
    assert patch.old_value == ([3],)

    # Scalar sequence elements preserve authored single/double quotes (bare and wrapped).
    seq_yaml = "names: ['London', \"Paris\"]\nwrapped:\n  value: ['Alpha', \"Beta\"]\n"
    doc = ConfigDocument.from_text(seq_yaml, normalise=False)
    dumped = doc.dump_yaml(preserve_style=True)
    assert "'London'" in dumped
    assert '"Paris"' in dumped
    assert "'Alpha'" in dumped
    assert '"Beta"' in dumped
    assert yaml.safe_load(dumped) == yaml.safe_load(seq_yaml)


def test_readonly_proxy_numpy_immutability_and_field_metadata() -> None:
    """Proxy blocks setflags(write=True), freezes object arrays, and detaches model_fields metadata."""

    class _RichModel(BaseModel):
        model_config = ConfigDict(arbitrary_types_allowed=True)
        arr: np.ndarray = Field(
            description="numeric array",
            json_schema_extra={"unit": "m", "nested": {"tag": "a"}},
        )
        obj_arr: np.ndarray = Field(description="object array")

    orig_desc = _RichModel.model_fields["arr"].description
    orig_meta_len = len(_RichModel.model_fields["arr"].metadata)
    m = _RichModel(
        arr=np.array([1.0, 2.0, 3.0]),
        obj_arr=np.array([{"k": [1, 2]}, {"k": [3]}], dtype=object),
    )
    ro = freeze_model(m)

    # Immutable bytes-backed numeric array cannot be re-enabled via setflags(write=True).
    assert ro.arr.flags.writeable is False
    with pytest.raises(ValueError, match="WRITEABLE"):
        ro.arr.setflags(write=True)
    with pytest.raises(ValueError, match="read-only"):
        ro.arr[0] = 42.0

    # Object array is recursively frozen so contained dicts/lists cannot be mutated.
    assert isinstance(ro.obj_arr, tuple)
    assert isinstance(ro.obj_arr[0], MappingProxyType)
    assert isinstance(ro.obj_arr[0]["k"], tuple)
    with pytest.raises(TypeError):
        ro.obj_arr[0]["k"] = (99,)

    # model_fields exposes deeply immutable ReadOnlyFieldInfo snapshots, leaving class metadata intact.
    field_info = ro.model_fields["arr"]
    assert isinstance(field_info, ReadOnlyFieldInfo)
    assert field_info.description == "numeric array"
    with pytest.raises(FrozenInstanceError):
        field_info.description = "tampered"  # type: ignore[misc]
    assert isinstance(field_info.metadata, tuple)
    with pytest.raises(TypeError):
        field_info.json_schema_extra["unit"] = "km"
    with pytest.raises(TypeError):
        field_info.json_schema_extra["nested"]["tag"] = "z"
    assert _RichModel.model_fields["arr"].description == orig_desc
    assert len(_RichModel.model_fields["arr"].metadata) == orig_meta_len
    assert _RichModel.model_fields["arr"].json_schema_extra == {
        "unit": "m",
        "nested": {"tag": "a"},
    }


def test_vertical_layers_nlayer_and_proxy_source_dir_validation(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """VerticalLayersView.nlayer rejects fractional/infinite values and proxy validation keeps _yaml_path."""
    assert VerticalLayersView({"nlayer": 2}).nlayer == 2
    assert VerticalLayersView({"nlayer": 2.0}).nlayer == 2
    assert VerticalLayersView({"nlayer": "3"}).nlayer == 3
    assert VerticalLayersView({"nlayer": "3.0"}).nlayer == 3
    assert VerticalLayersView({"nlayer": 2.9}).nlayer is None
    assert VerticalLayersView({"nlayer": "2.9"}).nlayer is None
    assert VerticalLayersView({"nlayer": float("inf")}).nlayer is None
    assert VerticalLayersView({"nlayer": float("-inf")}).nlayer is None
    assert VerticalLayersView({"nlayer": float("nan")}).nlayer is None
    assert VerticalLayersView({"nlayer": ".inf"}).nlayer is None

    # Validating a ReadOnlyModelProxy loaded from YAML preserves _yaml_path across chdir.
    cfg_dir = tmp_path / "site_cfg"
    other_dir = tmp_path / "other_cwd"
    cfg_dir.mkdir()
    other_dir.mkdir()

    sample_dict = yaml.safe_load(_load_sample_text())
    forcing_file = cfg_dir / "forcing_2012.txt"
    forcing_file.write_text(
        "iy id it imin qn qh qe qs qf U RH Tair pres rain kdown snow ldown fcld Wuh xsmd lai kdiff kdir wdir\n"
        "2012 180 12 0 100 50 30 20 10 2.0 50 20.0 101.3 0.0 500 0 300 0 0 0 1 100 400 180\n"
        "2012 180 13 0 100 50 30 20 10 2.0 50 20.0 101.3 0.0 500 0 300 0 0 0 1 100 400 180\n",
        encoding="utf-8",
    )
    sample_dict["model"]["control"]["forcing_file"] = {"value": "forcing_2012.txt"}
    sample_dict["model"]["control"].pop("start_time", None)
    sample_dict["model"]["control"].pop("end_time", None)
    cfg_path = cfg_dir / "config.yml"
    cfg_path.write_text(yaml.safe_dump(sample_dict, sort_keys=False), encoding="utf-8")

    cfg = SUEWSConfig.from_yaml(str(cfg_path))
    ro_cfg = freeze_model(cfg)
    monkeypatch.chdir(other_dir)

    import supy.data_model.validation.engine as engine_mod  # noqa: PLC0415

    captured_base_dirs: list[Path | None] = []
    orig_phase_b = engine_mod._run_phase_b_in_memory

    def _spy_phase_b(*args, **kwargs):
        captured_base_dirs.append(kwargs.get("base_dir"))
        return orig_phase_b(*args, **kwargs)

    monkeypatch.setattr(engine_mod, "_run_phase_b_in_memory", _spy_phase_b)
    rep_cfg = supy.validate_config(cfg, pipeline="B", forcing="off")
    rep_ro = supy.validate_config(ro_cfg, pipeline="B", forcing="off")
    assert captured_base_dirs == [cfg_dir.resolve(), cfg_dir.resolve()]
    assert [i.code for i in rep_ro.issues] == [i.code for i in rep_cfg.issues]
    assert rep_ro.is_valid == rep_cfg.is_valid


def test_patch_plan_normalised_reconciles_adjustments_with_surviving_patches() -> None:
    """Cancelling or coalescing adjustments in PatchPlan.normalised() never leaves phantom records."""
    # Cancelling adjustments (1 -> 2 -> 1) yield empty patches AND empty adjustments.
    cancel_plan = PatchPlan()
    cancel_plan.record(
        PlannedAdjustment(parameter="a", old_value=1, new_value=2, reason="first"),
        ConfigPatch("a", "set", 2, default_wrapped=False),
    )
    cancel_plan.record(
        PlannedAdjustment(parameter="a", old_value=2, new_value=1, reason="second"),
        ConfigPatch("a", "set", 1, default_wrapped=False),
    )
    norm_cancel = cancel_plan.normalised({"a": 1})
    assert norm_cancel.patches == []
    assert norm_cancel.adjustments == []

    # Coalescing adjustments (1 -> 2 -> 3) reconcile adjustment records with the single surviving patch.
    merge_plan = PatchPlan()
    merge_plan.record(
        PlannedAdjustment(parameter="a", old_value=1, new_value=2, reason="first"),
        ConfigPatch("a", "set", 2, default_wrapped=False),
    )
    merge_plan.record(
        PlannedAdjustment(parameter="a", old_value=2, new_value=3, reason="second"),
        ConfigPatch("a", "set", 3, default_wrapped=False),
    )
    norm_merge = merge_plan.normalised({"a": 1})
    assert len(norm_merge.patches) == 1
    assert norm_merge.patches[0].old_value == 1
    assert norm_merge.patches[0].new_value == 3
    assert len(norm_merge.adjustments) == 1
    assert norm_merge.adjustments[0].old_value == 1
    assert norm_merge.adjustments[0].new_value == 3
    assert norm_merge.adjustments[0].patches == tuple(norm_merge.patches)


def test_coalescing_sibling_rollback_forward_write_style_and_ref_only_null() -> None:
    """Coalescing preserves sibling rollback order, forward write style, and ref-only null agreement."""
    # Interleaved sibling writes under an implicitly created ancestor invert without error.
    interleaved = [
        ConfigPatch("a.b", "set", 1, default_wrapped=False),
        ConfigPatch("a.c", "set", 2, default_wrapped=False),
        ConfigPatch("a.b", "set", 3, default_wrapped=False),
    ]
    norm_interleaved = normalise_patches(interleaved, {})
    work: dict[str, object] = {}
    apply_patches_in_place(work, norm_interleaved)
    assert work == {"a": {"b": 3, "c": 2}}
    apply_patches_in_place(work, invert_patches(norm_interleaved))
    assert work == {}

    # Normalisation records forward write_style/write_ref so replay matches with and without a ledger.
    bare_insert = normalise_patches(
        [ConfigPatch("a", "set", 2, original_style="bare")], {}
    )
    replayed_empty: dict[str, object] = {}
    apply_patches_in_place(replayed_empty, bare_insert)
    assert replayed_empty == {"a": 2}
    apply_patches_in_place(replayed_empty, invert_patches(bare_insert))
    assert replayed_empty == {}

    ref_doc = ConfigDocument.from_dict(
        {"a": {"value": 1, "ref": {"ID": "A1"}}}, normalise=False
    )
    uncoalesced = ref_doc.normalise_patches(
        [ConfigPatch("a", "delete"), ConfigPatch("a", "set", 2)], coalesce=False
    )
    plan_no_ledger = PatchPlan(patches=uncoalesced)
    replayed_dict = plan_no_ledger.apply_to_dict(ref_doc.data)
    assert replayed_dict == {"a": {"value": 2, "ref": {"ID": "A1"}}}
    replayed_doc = ref_doc.apply_patches(uncoalesced)
    assert replayed_doc.data == replayed_dict

    # Ref-only placeholder with explicit ref: None agrees between direct and normalised application.
    ref_null_base = {"a": {"ref": None}}
    direct_ref_null = deepcopy(ref_null_base)
    apply_patches_in_place(direct_ref_null, [ConfigPatch("a", "set", None)])
    norm_ref_null = normalise_patches([ConfigPatch("a", "set", None)], ref_null_base)
    applied_ref_null = deepcopy(ref_null_base)
    apply_patches_in_place(applied_ref_null, norm_ref_null)
    assert direct_ref_null == applied_ref_null == {"a": {"ref": None}}


def test_ledger_mapping_key_paths_and_typed_view_direct_construction() -> None:
    """Ledger preserves wrapper/ref across delete/reinsert for int and punctuated keys; direct TypedConfigView keeps control."""
    raw_profile = {
        "profile": {
            1: {"value": 7, "ref": {"ID": "int-key"}},
            "1": {"value": 8, "ref": {"ID": "str-key"}},
            "a.b": {"value": 9, "ref": {"ID": "dot-key"}},
        }
    }
    doc = ConfigDocument.from_dict(raw_profile, normalise=False)
    for key, expected_id, new_val in (
        (1, "int-key", 17),
        ("1", "str-key", 18),
        ("a.b", "dot-key", 19),
    ):
        toks = ("profile", MappingKey(key)) if key != "1" else ("profile", "1")
        path = format_path_tokens(toks)
        patches = [
            ConfigPatch(path, "delete", path_tokens=toks),
            ConfigPatch(path, "set", new_val, path_tokens=toks),
        ]
        updated = doc.apply_patches(patches)
        assert updated.data["profile"][key] == {
            "value": new_val,
            "ref": {"ID": expected_id},
        }

    # Direct three-argument TypedConfigView(raw, physics, sites) derives control from raw.
    raw_cfg = {
        "model": {
            "control": {
                "start_time": "2012-01-01",
                "end_time": "2012-01-02",
            },
            "physics": {},
        },
        "sites": [],
    }
    direct_view = TypedConfigView(raw_cfg, PhysicsView(), ())
    assert direct_view.start_time == "2012-01-01"
    assert direct_view.end_time == "2012-01-02"
    explicit_empty = TypedConfigView(raw_cfg, PhysicsView(), (), control=ControlView())
    assert explicit_empty.start_time is None


def test_readonly_proxy_freezes_deque_dataclass_array_metadata_and_rejects_unknown_mutable() -> (
    None
):
    """Proxy freezes deque/dataclass fields, isolates NumPy view metadata, and rejects unfreezable objects."""

    @dataclass
    class _MutableDC:
        items: list[int]
        label: str

    class _CustomMutable:
        def __init__(self, val: int) -> None:
            self.val = val

        def bump(self) -> None:
            self.val += 1

    class _ComplexModel(BaseModel):
        model_config = ConfigDict(arbitrary_types_allowed=True)
        queue: deque[int]
        dc: _MutableDC
        arr: np.ndarray
        arrays: list[np.ndarray]
        custom: _CustomMutable | None = None

    m = _ComplexModel(
        queue=deque([1, 2]),
        dc=_MutableDC(items=[10, 20], label="ok"),
        arr=np.array([1.0, 2.0, 3.0]),
        arrays=[np.array([4.0, 5.0])],
        custom=_CustomMutable(5),
    )
    ro = freeze_model(m)

    # deque is frozen as an immutable tuple; snapshot and model_dump remain untouched.
    assert ro.queue == (1, 2)
    assert isinstance(ro.queue, tuple)

    # Dataclass field is wrapped in ReadOnlyDataclassProxy with frozen nested containers.
    assert isinstance(ro.dc, ReadOnlyDataclassProxy)
    assert ro.dc.items == (10, 20)
    assert ro.dc.label == "ok"
    with pytest.raises(TypeError, match="Cannot mutate attribute"):
        ro.dc.label = "bad"
    with pytest.raises(AttributeError):
        _ = ro.dc._internal

    # Mutating NumPy view metadata (shape/dtype) on one read cannot affect subsequent reads.
    first_view = ro.arr
    first_view.shape = (1, 3)
    first_view.dtype = np.int32
    assert ro.arr.shape == (3,)
    assert ro.arr.dtype == np.float64

    nested_view = ro.arrays[0]
    nested_view.shape = (1, 2)
    assert ro.arrays[0].shape == (2,)

    # Unrecognised custom mutable objects are rejected explicitly instead of leaking mutable state.
    with pytest.raises(TypeError, match="cannot safely freeze"):
        _ = ro.custom


def test_round3_patch_token_barriers_and_non_ref_mapping_coalesce() -> None:
    """Mixed MappingKey/str barriers and non-RefValue delete+set preserve exact semantics."""
    # Major 1: MappingKey("a") and "a" share the same ancestor barrier.
    mixed_patches = [
        ConfigPatch("a.b", "set", 2, default_wrapped=False),
        ConfigPatch("a", "delete", path_tokens=(MappingKey("a"),)),
        ConfigPatch("a.b", "set", 3, default_wrapped=False),
    ]
    base_mixed = {"a": {"b": 1}}
    norm_mixed = normalise_patches(mixed_patches, base_mixed)
    direct_mixed = deepcopy(base_mixed)
    apply_patches_in_place(direct_mixed, mixed_patches)
    replayed_mixed = deepcopy(base_mixed)
    apply_patches_in_place(replayed_mixed, norm_mixed)
    assert replayed_mixed == direct_mixed == {"a": {"b": 3}}
    apply_patches_in_place(replayed_mixed, invert_patches(norm_mixed))
    assert replayed_mixed == base_mixed

    # Major 3: delete+set on a non-RefValue dict containing "value" + extra keys does not turn into an update.
    unit_base = {"a": {"value": 1, "unit": "m"}}
    unit_patches = [
        ConfigPatch("a", "delete"),
        ConfigPatch("a", "set", 2, default_wrapped=False),
    ]
    norm_unit = normalise_patches(unit_patches, unit_base)
    replayed_unit = deepcopy(unit_base)
    apply_patches_in_place(replayed_unit, norm_unit)
    assert replayed_unit == {"a": 2}
    apply_patches_in_place(replayed_unit, invert_patches(norm_unit))
    assert replayed_unit == unit_base


def test_round3_ref_only_populate_nullify_and_strict_key_type_equality() -> None:
    """Populating then nullifying a ref-only placeholder agrees across modes, and bool vs int keys differ."""
    # Minor 4: populating a ref-only placeholder then nullifying agrees between direct and expanded.
    ref_doc = ConfigDocument.from_dict({"a": {"ref": {"ID": "x"}}}, normalise=False)
    pop_null = [ConfigPatch("a", "set", 1), ConfigPatch("a", "nullify_tree")]
    direct_pn = ref_doc.apply_patches(pop_null)
    norm_pn = ref_doc.normalise_patches(pop_null, expand_nullify_tree=True)
    expanded_pn = ref_doc.apply_patches(norm_pn)
    assert (
        expanded_pn.data == direct_pn.data == {"a": {"ref": {"ID": "x"}, "value": None}}
    )
    assert expanded_pn.apply_patches(invert_patches(norm_pn)).data == ref_doc.data

    # Minor 7: _strict_equal distinguishes int key 1 from bool key True.
    key_type_base = {"a": {1: "x"}}
    key_type_norm = normalise_patches(
        [ConfigPatch("a", "set", {True: "x"}, default_wrapped=False)], key_type_base
    )
    assert len(key_type_norm) == 1
    replayed_kt = deepcopy(key_type_base)
    apply_patches_in_place(replayed_kt, key_type_norm)
    assert next(iter(replayed_kt["a"].keys())) is True


def test_round3_deep_freeze_surfaces_yaml_keys_profile_ledger_and_proxy_edge_cases() -> (
    None
):
    """deep_freeze protects arrays/views, YAML key decoding & profile ledger align, structured scalars & sets work."""
    # Major 2: TypedConfigView, ConfigDocument.frozen_data, and ValidationContext freeze NumPy arrays.
    arr_raw = {"a": np.array([1.0, 2.0])}
    tv = TypedConfigView.from_mapping(arr_raw)
    with pytest.raises(ValueError, match="read-only"):
        tv.raw["a"][0] = 9.0
    doc_arr = ConfigDocument(arr_raw)
    with pytest.raises(ValueError, match="read-only"):
        doc_arr.frozen_data()["a"][0] = 9.0
    ctx_arr = ValidationContext(yaml_data=arr_raw)
    with pytest.raises(ValueError, match="read-only"):
        ctx_arr.yaml_data["a"][0] = 9.0

    # Minor 5 & 6: YAML 01/yes/float keys and hourly profile normalisation preserve ledger provenance.
    yaml_text = (
        "sites:\n"
        "  - properties:\n"
        "      irrigation:\n"
        "        wuprofa_24hr:\n"
        "          working_day:\n"
        "            01:\n"
        "              value: 0.5\n"
        "              ref:\n"
        "                ID: h1\n"
        "            '02':\n"
        "              value: 0.25\n"
        "              ref:\n"
        "                ID: h2\n"
        "extra:\n"
        "  yes:\n"
        "    value: 10\n"
        "    ref:\n"
        "      ID: bool-key\n"
    )
    prof_doc = ConfigDocument.from_text(yaml_text, normalise=True)
    h1_path = "sites[0].properties.irrigation.wuprofa_24hr.working_day.1"
    h2_path = "sites[0].properties.irrigation.wuprofa_24hr.working_day.2"
    assert prof_doc.style_ledger.ref_metadata(h1_path) == {"ID": "h1"}
    assert prof_doc.style_ledger.ref_metadata(h2_path) == {"ID": "h2"}
    assert prof_doc.style_ledger.ref_metadata("extra[True]") == {"ID": "bool-key"}

    rebuilt = prof_doc.apply_patches([
        ConfigPatch(h1_path, "delete"),
        ConfigPatch(h1_path, "set", 0.75),
    ])
    assert rebuilt.data["sites"][0]["properties"]["irrigation"]["wuprofa_24hr"][
        "working_day"
    ]["1"] == {"value": 0.75, "ref": {"ID": "h1"}}

    # Minor 8 & 9: Structured np.void scalars are frozen and sets of frozen models/dataclasses work.
    class _FrozenChild(BaseModel):
        model_config = ConfigDict(frozen=True)
        code: int

    @dataclass(frozen=True)
    class _FrozenDC:
        tag: str

    class _EdgeModel(BaseModel):
        model_config = ConfigDict(arbitrary_types_allowed=True)
        void_scalar: object
        obj_void: object
        model_set: set[_FrozenChild]
        dc_set: set[_FrozenDC]

    num_void = np.array([(1,)], dtype=[("x", "i4")])[0]
    obj_void = np.array([({"k": [1]},)], dtype=[("payload", object)])[0]
    em = _EdgeModel(
        void_scalar=num_void,
        obj_void=obj_void,
        model_set={_FrozenChild(code=1), _FrozenChild(code=2)},
        dc_set={_FrozenDC(tag="a"), _FrozenDC(tag="b")},
    )
    ro_em = freeze_model(em)
    assert isinstance(ro_em.void_scalar, MappingProxyType)
    assert ro_em.void_scalar["x"] == 1
    with pytest.raises(TypeError):
        ro_em.void_scalar["x"] = 2
    assert ro_em.obj_void["payload"]["k"] == (1,)
    with pytest.raises(TypeError):
        ro_em.obj_void["payload"]["k"] = (9,)

    assert isinstance(ro_em.model_set, frozenset)
    assert len(ro_em.model_set) == 2
    assert _FrozenChild(code=1) in ro_em.model_set
    assert isinstance(ro_em.dc_set, frozenset)
    assert _FrozenDC(tag="a") in ro_em.dc_set


def test_confirm_round_array_metadata_and_idempotent_refreeze() -> None:
    """Frozen array views resist metadata mutation and deep_freeze accepts already-frozen proxies."""
    arr_raw = {
        "a": np.array([1.0, 2.0]),
        "nested": [np.array([3.0, 4.0])],
        "struct": np.array([(1, [5.0, 6.0])], dtype=[("x", "i4"), ("y", "f8", (2,))]),
    }
    ctx_arr = ValidationContext(yaml_data=arr_raw)
    surfaces = (
        ctx_arr.view.raw,
        ctx_arr.yaml_data,
        TypedConfigView.from_mapping(arr_raw).raw,
        ConfigDocument(arr_raw).frozen_data(),
    )
    for surface in surfaces:
        v_a = surface["a"]
        v_a.dtype = np.int64
        v_a.shape = (2, 1)
        assert surface["a"].dtype == np.float64
        assert surface["a"].shape == (2,)
        assert np.array_equal(surface["a"], np.array([1.0, 2.0]))

        v_nested = surface["nested"][0]
        v_nested.dtype = np.int64
        v_nested.shape = (1, 2)
        assert surface["nested"][0].dtype == np.float64
        assert surface["nested"][0].shape == (2,)

        v_struct = surface["struct"]
        v_struct.dtype.names = ("bad_x", "bad_y")
        assert surface["struct"].dtype.names == ("x", "y")

    class _MapArrayModel(BaseModel):
        model_config = ConfigDict(arbitrary_types_allowed=True)
        arr_map: dict[str, np.ndarray]

    ro_map = freeze_model(_MapArrayModel(arr_map={"a": np.array([1.0, 2.0])}))
    held_map = ro_map.arr_map
    held_map["a"].dtype = np.int64
    held_map["a"].shape = (2, 1)
    assert held_map["a"].dtype == np.float64
    assert held_map["a"].shape == (2,)

    class _NestedModel(BaseModel):
        value: int

    @dataclass
    class _NestedDC:
        items: list[int]

    child_m = _NestedModel(value=42)
    child_dc = _NestedDC(items=[1, 2])
    frozen_m = deep_freeze(child_m)
    frozen_dc = deep_freeze(child_dc)
    assert deep_freeze(frozen_m) is frozen_m
    assert deep_freeze(frozen_dc) is frozen_dc

    nested_raw = {"extra": child_m, "dc": child_dc}
    tv_nested = TypedConfigView.from_mapping(nested_raw)
    assert isinstance(tv_nested.raw["extra"], ReadOnlyModelProxy)
    assert tv_nested.raw["extra"].value == 42
    assert isinstance(tv_nested.raw["dc"], ReadOnlyDataclassProxy)
    assert tv_nested.raw["dc"].items == (1, 2)

    ctx_nested = ValidationContext(yaml_data=nested_raw)
    assert isinstance(ctx_nested.view.raw["extra"], ReadOnlyModelProxy)
    assert ctx_nested.view.raw["extra"].value == 42
    assert isinstance(ctx_nested.view.raw["dc"], ReadOnlyDataclassProxy)
    assert ctx_nested.view.raw["dc"].items == (1, 2)


def test_confirm_round_unsorted_profile_alias_barriers_and_representation_noops() -> (
    None
):
    """Unsorted profile ledger alignment, dict-key alias barriers, and style-changing writes hold."""
    unsorted_yaml = (
        "sites:\n"
        "  - properties:\n"
        "      irrigation:\n"
        "        wuprofa_24hr:\n"
        "          working_day:\n"
        "            2:\n"
        "              value: 0.25\n"
        "              ref:\n"
        "                ID: hour-two\n"
        "            '01':\n"
        "              value: 0.5\n"
        "              ref:\n"
        "                ID: hour-one\n"
    )
    unsorted_doc = ConfigDocument.from_text(unsorted_yaml, normalise=True)
    p1 = "sites[0].properties.irrigation.wuprofa_24hr.working_day.1"
    p2 = "sites[0].properties.irrigation.wuprofa_24hr.working_day.2"
    assert unsorted_doc.style_ledger.ref_metadata(p1) == {"ID": "hour-one"}
    assert unsorted_doc.style_ledger.ref_metadata(p2) == {"ID": "hour-two"}
    rebuilt = unsorted_doc.apply_patches([
        ConfigPatch(p1, "delete"),
        ConfigPatch(p1, "set", 0.9),
        ConfigPatch(p2, "delete"),
        ConfigPatch(p2, "set", 0.1),
    ])
    wd = rebuilt.data["sites"][0]["properties"]["irrigation"]["wuprofa_24hr"][
        "working_day"
    ]
    assert wd["1"] == {"value": 0.9, "ref": {"ID": "hour-one"}}
    assert wd["2"] == {"value": 0.1, "ref": {"ID": "hour-two"}}

    alias_base = {"a": {1: 0}}
    alias_patches = [
        ConfigPatch(
            "a[1]", "set", 1, path_tokens=("a", MappingKey(1)), default_wrapped=False
        ),
        ConfigPatch(
            "a[True]",
            "set",
            2,
            path_tokens=("a", MappingKey(True)),
            default_wrapped=False,
        ),
        ConfigPatch(
            "a[1]", "set", 3, path_tokens=("a", MappingKey(1)), default_wrapped=False
        ),
    ]
    direct_alias = deepcopy(alias_base)
    apply_patches_in_place(direct_alias, alias_patches)
    norm_alias = normalise_patches(alias_patches, alias_base)
    replayed_alias = deepcopy(alias_base)
    apply_patches_in_place(replayed_alias, norm_alias)
    assert replayed_alias == direct_alias == {"a": {1: 3}}
    apply_patches_in_place(replayed_alias, invert_patches(norm_alias))
    assert replayed_alias == alias_base

    for rep_base, rep_patch, expected in (
        (
            {"a": {"value": None, "ref": {"ID": "r"}}},
            [ConfigPatch("a", "set", None, write_style="ref_only")],
            {"a": {"ref": {"ID": "r"}}},
        ),
        (
            {"a": None},
            [
                ConfigPatch(
                    "a", "set", None, write_style="ref_only", write_ref={"ID": "r"}
                )
            ],
            {"a": {"ref": {"ID": "r"}}},
        ),
    ):
        direct_rep = deepcopy(rep_base)
        apply_patches_in_place(direct_rep, rep_patch)
        norm_rep = normalise_patches(rep_patch, rep_base)
        assert len(norm_rep) == 1
        replayed_rep = deepcopy(rep_base)
        apply_patches_in_place(replayed_rep, norm_rep)
        assert replayed_rep == direct_rep == expected
        apply_patches_in_place(replayed_rep, invert_patches(norm_rep))
        assert replayed_rep == rep_base


def test_confirm_r2_non_ref_value_coalesce_and_plain_dict_wrapping() -> None:
    """Non-RefValue value-dict coalescing and plain-dict delete+set wrapping match direct apply."""
    # Blocker 1: scalar -> non-RefValue mapping containing "value" -> scalar preserves sibling keys.
    unit_base = {"a": 0}
    unit_patches = [
        ConfigPatch("a", "set", {"value": 1, "unit": "m"}, default_wrapped=False),
        ConfigPatch("a", "set", 2, default_wrapped=False),
    ]
    direct_unit = deepcopy(unit_base)
    apply_patches_in_place(direct_unit, unit_patches)
    norm_unit = normalise_patches(unit_patches, unit_base)
    replayed_unit = deepcopy(unit_base)
    apply_patches_in_place(replayed_unit, norm_unit)
    assert replayed_unit == direct_unit == {"a": {"value": 2, "unit": "m"}}
    apply_patches_in_place(replayed_unit, invert_patches(norm_unit))
    assert replayed_unit == unit_base

    # Representation-changing set (bare -> ref_only -> ref_value) must not collapse and drop ref.
    rep_chain_base = {"a": None}
    rep_chain = [
        ConfigPatch("a", "set", None, write_style="ref_only", write_ref={"ID": "r1"}),
        ConfigPatch("a", "set", 5),
    ]
    direct_chain = deepcopy(rep_chain_base)
    apply_patches_in_place(direct_chain, rep_chain)
    norm_chain = normalise_patches(rep_chain, rep_chain_base)
    replayed_chain = deepcopy(rep_chain_base)
    apply_patches_in_place(replayed_chain, norm_chain)
    assert replayed_chain == direct_chain == {"a": {"ref": {"ID": "r1"}, "value": 5}}
    apply_patches_in_place(replayed_chain, invert_patches(norm_chain))
    assert replayed_chain == rep_chain_base

    # Major 1: plain dict target does not synthesise a StyleLedger that overrides default_wrapped=True.
    plain_base = {"a": 1}
    del_set_patches = [ConfigPatch("a", "delete"), ConfigPatch("a", "set", 2)]
    direct_plain = deepcopy(plain_base)
    apply_patches_in_place(direct_plain, del_set_patches)
    norm_plain = normalise_patches(del_set_patches, plain_base)
    replayed_plain = deepcopy(plain_base)
    apply_patches_in_place(replayed_plain, norm_plain)
    assert replayed_plain == direct_plain == {"a": {"value": 2}}
    apply_patches_in_place(replayed_plain, invert_patches(norm_plain))
    assert replayed_plain == plain_base

    # ConfigDocument target still preserves bare style via its real StyleLedger.
    bare_doc = ConfigDocument.from_dict(plain_base, normalise=False)
    assert (
        bare_doc.apply_patches(bare_doc.normalise_patches(del_set_patches)).data
        == bare_doc.apply_patches(del_set_patches).data
        == {"a": 2}
    )


def test_confirm_r2_aliased_delete_restores_original_key_type() -> None:
    """Deleting int key 1 via aliased MappingKey(True) restores int key 1 on invert."""
    alias_del_base = {"a": {1: "orig"}}
    alias_del_patches = [
        ConfigPatch("a[True]", "delete", path_tokens=("a", MappingKey(True)))
    ]
    norm_alias_del = normalise_patches(alias_del_patches, alias_del_base)
    replayed_alias_del = deepcopy(alias_del_base)
    apply_patches_in_place(replayed_alias_del, norm_alias_del)
    assert replayed_alias_del == {"a": {}}
    apply_patches_in_place(replayed_alias_del, invert_patches(norm_alias_del))
    assert replayed_alias_del == {"a": {1: "orig"}}
    restored_key = next(iter(replayed_alias_del["a"].keys()))
    assert type(restored_key) is int
    assert restored_key == 1

    # Also verify aliased delete+set replaces int key 1 with bool True forward and restores int 1 on invert.
    alias_replace = [
        ConfigPatch("a[True]", "delete", path_tokens=("a", MappingKey(True))),
        ConfigPatch(
            "a[True]",
            "set",
            "orig",
            path_tokens=("a", MappingKey(True)),
            default_wrapped=False,
        ),
    ]
    norm_replace = normalise_patches(alias_replace, alias_del_base)
    replayed_replace = deepcopy(alias_del_base)
    apply_patches_in_place(replayed_replace, norm_replace)
    assert type(next(iter(replayed_replace["a"].keys()))) is bool
    apply_patches_in_place(replayed_replace, invert_patches(norm_replace))
    assert type(next(iter(replayed_replace["a"].keys()))) is int


def test_confirm_r2_enum_freezing() -> None:
    """Enum members are wrapped in ReadOnlyEnumProxy and mutable payloads are rejected."""

    class _SafeEnum(Enum):
        TUP = (1, "ok")

    class _SafeIntEnum(IntEnum):
        ONE = 1

    class _SafeStrEnum(StrEnum):
        ALPHA = "alpha"

    class _MutableListEnum(Enum):
        BAD = [1, 2]  # noqa: RUF012

    class _MutableNestedEnum(Enum):
        BAD = ([1, 2],)

    class _MutableAttrEnum(IntEnum):
        BAD = 1

        def __init__(self, _val: int) -> None:
            self.extra = [1, 2]

    class _EnumModel(BaseModel):
        model_config = ConfigDict(arbitrary_types_allowed=True)
        safe_enum: _SafeEnum = _SafeEnum.TUP
        safe_int_enum: _SafeIntEnum = _SafeIntEnum.ONE
        safe_str_enum: _SafeStrEnum = _SafeStrEnum.ALPHA
        bad_enum: object = _MutableListEnum.BAD
        bad_attr_enum: object = _MutableAttrEnum.BAD

    frozen_safe_enum = deep_freeze(_SafeEnum.TUP)
    assert isinstance(frozen_safe_enum, ReadOnlyEnumProxy)
    assert frozen_safe_enum == _SafeEnum.TUP
    assert deep_freeze(_SafeIntEnum.ONE) == _SafeIntEnum.ONE
    assert int(deep_freeze(_SafeIntEnum.ONE)) == 1
    assert deep_freeze(_SafeStrEnum.ALPHA) == _SafeStrEnum.ALPHA
    with pytest.raises(TypeError, match="Cannot mutate attribute"):
        frozen_safe_enum.extra = "mutated"  # type: ignore[attr-defined]
    assert not hasattr(_SafeEnum.TUP, "extra")
    assert thaw_copy(frozen_safe_enum) is frozen_safe_enum
    with pytest.raises(TypeError, match="Enum singleton"):
        frozen_safe_enum.unwrap_copy()
    for bad_member in (
        _MutableListEnum.BAD,
        _MutableNestedEnum.BAD,
        _MutableAttrEnum.BAD,
    ):
        with pytest.raises(TypeError, match="mutable payload"):
            deep_freeze(bad_member)

    ro = freeze_model(_EnumModel())
    assert isinstance(ro.safe_enum, ReadOnlyEnumProxy)
    assert ro.safe_enum == _SafeEnum.TUP
    assert ro.safe_int_enum == _SafeIntEnum.ONE
    assert ro.safe_str_enum == _SafeStrEnum.ALPHA
    with pytest.raises(TypeError, match="Cannot mutate attribute"):
        ro.safe_enum.extra = "mutated"
    with pytest.raises(TypeError, match="mutable payload"):
        _ = ro.bad_enum
    with pytest.raises(TypeError, match="mutable payload"):
        _ = ro.bad_attr_enum
    assert isinstance(ro.model_fields["bad_enum"].default, str)


def test_confirm_r2_slice_freezing() -> None:
    """Slice bounds are deeply frozen, isolate NumPy views, and reject unfreezable objects."""

    class _CustomMutable:
        def __init__(self, x: int) -> None:
            self.x = x

    class _SliceModel(BaseModel):
        model_config = ConfigDict(arbitrary_types_allowed=True)
        mut_slice: object = Field(
            default_factory=lambda: slice([1, 2], {"k": [3]}, None)
        )
        arr_slice: object = Field(
            default_factory=lambda: slice(np.array([1.0, 2.0]), None, None)
        )

    frozen_mut_slice = deep_freeze(slice([1, 2], {"k": [3]}, None))
    assert isinstance(frozen_mut_slice, slice)
    assert frozen_mut_slice.start == (1, 2)
    assert isinstance(frozen_mut_slice.stop, MappingProxyType)
    assert frozen_mut_slice.stop["k"] == (3,)
    with pytest.raises(TypeError):
        frozen_mut_slice.stop["k"] = (9,)  # type: ignore[index]

    frozen_slice_map = deep_freeze({"s": slice(np.array([1.0, 2.0]), None, None)})
    s1 = frozen_slice_map["s"]
    s1.start.shape = (2, 1)
    s1.start.dtype = np.int64
    s2 = frozen_slice_map["s"]
    assert s2.start.shape == (2,)
    assert s2.start.dtype == np.float64
    with pytest.raises(TypeError, match="cannot safely freeze"):
        deep_freeze(slice(_CustomMutable(1), None, None))

    ro = freeze_model(_SliceModel())
    assert ro.mut_slice.start == (1, 2)
    assert isinstance(ro.mut_slice.stop, MappingProxyType)
    assert ro.mut_slice.stop["k"] == (3,)
    ro_s1 = ro.arr_slice
    ro_s1.start.shape = (2, 1)
    ro_s1.start.dtype = np.int64
    assert ro.arr_slice.start.shape == (2,)
    assert ro.arr_slice.start.dtype == np.float64


def test_confirm_r2_proxy_copy_thaw_and_field_default_arrays() -> None:
    """Proxies survive copy/deepcopy/thaw and cached ReadOnlyFieldInfo array defaults isolate views."""

    @dataclass
    class _NestedDC:
        items: list[int]

    class _NestedModel(BaseModel):
        value: int

    class _ProxyCopyModel(BaseModel):
        model_config = ConfigDict(arbitrary_types_allowed=True)
        default_arr: np.ndarray = Field(
            default=np.array([10.0, 20.0]),
            json_schema_extra={"arr": np.array([30.0, 40.0])},
        )
        child_model: _NestedModel = Field(default_factory=lambda: _NestedModel(value=7))
        child_dc: _NestedDC = Field(default_factory=lambda: _NestedDC(items=[1, 2]))

    ro = freeze_model(_ProxyCopyModel())
    # Major 3: copy/deepcopy on ReadOnlyModelProxy & ReadOnlyDataclassProxy and ConfigDocument.from_dict(frozen_data()).
    assert copy(ro).child_model.value == 7
    assert deepcopy(ro).child_model.value == 7
    assert copy(ro.child_dc).items == (1, 2)
    assert deepcopy(ro.child_dc).items == (1, 2)
    with pytest.raises(TypeError, match="Cannot mutate attribute"):
        deepcopy(ro).child_model.value = 99
    with pytest.raises(TypeError, match="Cannot mutate attribute"):
        deepcopy(ro.child_dc).items = [9]

    doc_nested = ConfigDocument({
        "model": _NestedModel(value=11),
        "dc": _NestedDC(items=[3, 4]),
        "sl": slice([5, 6], None, None),
    })
    rehydrated = ConfigDocument.from_dict(doc_nested.frozen_data(), normalise=False)
    assert isinstance(rehydrated.data["model"], _NestedModel)
    assert isinstance(rehydrated.data["dc"], _NestedDC)
    assert rehydrated.data["model"].value == 11
    assert rehydrated.data["dc"].items == [3, 4]
    assert rehydrated.data["sl"] == slice([5, 6], None, None)
    rehydrated.data["model"].value = 999
    rehydrated.data["dc"].items.append(5)
    assert doc_nested.data["model"].value == 11
    assert doc_nested.data["dc"].items == [3, 4]
    assert thaw_copy(ro.child_model) == _NestedModel(value=7)
    assert thaw_copy(ro.child_dc) == _NestedDC(items=[1, 2])

    # Major 4: ReadOnlyFieldInfo.default / json_schema_extra NumPy arrays isolate view metadata across reads.
    field_info = ro.model_fields["default_arr"]
    def_v1 = field_info.default
    def_v1.shape = (2, 1)
    def_v1.dtype = np.int64
    def_v2 = ro.model_fields["default_arr"].default
    assert def_v2.shape == (2,)
    assert def_v2.dtype == np.float64
    assert np.array_equal(def_v2, np.array([10.0, 20.0]))

    extra_v1 = field_info.json_schema_extra["arr"]
    extra_v1.shape = (2, 1)
    extra_v1.dtype = np.int64
    extra_v2 = ro.model_fields["default_arr"].json_schema_extra["arr"]
    assert extra_v2.shape == (2,)
    assert extra_v2.dtype == np.float64
    assert field_info == ro.model_fields["default_arr"]
    assert deepcopy(field_info) is field_info


def test_confirm_r2_reversed_tuple_typed_key_order_and_engine_annotations() -> None:
    """Materialising tuple supports reversed(), StyleLedger keeps typed key_order, and engine accepts ReadOnlyModelProxy."""
    # Minor 1: reversed() on _ReadOnlyMaterialisingTuple works and isolates view metadata.
    frozen_seq = deep_freeze([np.array([1.0, 2.0]), np.array([3.0, 4.0])])
    rev_items = list(reversed(frozen_seq))
    assert len(rev_items) == 2
    assert np.array_equal(rev_items[0], np.array([3.0, 4.0]))
    assert np.array_equal(rev_items[1], np.array([1.0, 2.0]))
    rev_items[0].shape = (2, 1)
    rev_items[0].dtype = np.int64
    assert frozen_seq[1].shape == (2,)
    assert frozen_seq[1].dtype == np.float64

    # Minor 2: StyleLedger preserves typed keys (1 vs "1") in key_order and round-trip reordering.
    yaml_mixed_keys = "'1': str-first\n1: int-second\n"
    ledger_yaml = StyleLedger.from_yaml_text(yaml_mixed_keys)
    root_order = ledger_yaml.get("")
    assert root_order is not None
    assert len(root_order.key_order) == 2
    assert type(root_order.key_order[0]) is str and root_order.key_order[0] == "1"
    assert type(root_order.key_order[1]) is int and root_order.key_order[1] == 1

    dumped_mixed = dump_yaml_with_style(
        {1: "int-second", "1": "str-first"}, ledger_yaml
    )
    reloaded_keys = list(yaml.safe_load(dumped_mixed).keys())
    assert [type(k) for k in reloaded_keys] == [str, int]
    assert reloaded_keys == ["1", 1]

    ledger_map = StyleLedger.from_mapping({1: "int-first", "1": "str-second"})
    map_order = ledger_map.get("")
    assert map_order is not None
    assert [type(k) for k in map_order.key_order] == [int, str]
    dumped_map = dump_yaml_with_style({"1": "str-second", 1: "int-first"}, ledger_map)
    reloaded_map_keys = list(yaml.safe_load(dumped_map).keys())
    assert [type(k) for k in reloaded_map_keys] == [int, str]
    assert reloaded_map_keys == [1, "1"]

    # Minor 3: ValidationEngine.validate and validate_config type annotations include ReadOnlyModelProxy.
    assert "ReadOnlyModelProxy" in ValidationEngine.validate.__annotations__["source"]
    assert "ReadOnlyModelProxy" in validate_config.__annotations__["source"]


def test_astra_r1_physics_view_nested_forms_and_aliased_yaml_anchors() -> None:
    """PhysicsView reads nested orthogonal forms and normalise_patches preserves aliased mappings."""
    # Major 1: PhysicsView / TypedConfigView resolve nested storage_heat and kdown_split_method.
    pv_ohm = PhysicsView({"storage_heat": {"ohm": {"include_qf": True}}})
    assert pv_ohm.code("storage_heat") == 1
    assert pv_ohm.read("ohm_inc_qf") == 1
    tv_kdown = TypedConfigView.from_mapping({
        "model": {
            "physics": {"kdown_split_method": {"constant": {"sw_dn_direct_frac": 0.6}}}
        }
    })
    assert tv_kdown.physics.code("kdown_split_method") == 2

    # Majors 3/4/5 (R2): Aliased mutable containers are explicitly rejected by normalise_patches,
    # while ConfigDocument.from_text detaches YAML anchors so normalisation and inversion are exact.
    anchor_yaml = "a: &shared\n  x: 0\nb: *shared\n"
    anchor_base = yaml.safe_load(anchor_yaml)
    assert anchor_base["a"] is anchor_base["b"]
    anchor_patches = [
        ConfigPatch("a.x", "set", 1, default_wrapped=False),
        ConfigPatch("b.x", "set", 0, default_wrapped=False),
    ]
    with pytest.raises(ValueError, match="aliased or cyclic"):
        normalise_patches(anchor_patches, anchor_base)
    shared_wrapper = {"value": 0}
    with pytest.raises(ValueError, match="aliased or cyclic"):
        normalise_patches(
            [ConfigPatch("a", "set", 1)], {"a": shared_wrapper, "b": shared_wrapper}
        )
    anchor_doc = ConfigDocument.from_text(anchor_yaml, normalise=False)
    assert anchor_doc.data["a"] is not anchor_doc.data["b"]
    norm_anchor_doc = anchor_doc.normalise_patches(anchor_patches)
    applied_anchor = anchor_doc.apply_patches(norm_anchor_doc)
    assert applied_anchor.data == {"a": {"x": 1}, "b": {"x": 0}}
    assert applied_anchor.apply_patches(invert_patches(norm_anchor_doc)).data == (
        anchor_doc.data
    )


def test_astra_r1_numpy_patches_and_mutable_subclass_rejection() -> None:
    """NumPy-valued patches normalise/invert cleanly and mutable primitive subclasses are rejected."""
    # Major 3: NumPy arrays in patches handle changed, unchanged, dtype, shape, and NaN cleanly.
    arr_base = {"a": np.array([1, 2]), "b": np.array([1.0, np.nan])}
    arr_patches = [
        ConfigPatch("a", "set", np.array([3, 4]), default_wrapped=False),
        ConfigPatch("b", "set", np.array([1.0, np.nan]), default_wrapped=False),
    ]
    norm_arr = normalise_patches(arr_patches, arr_base)
    assert len(norm_arr) == 1
    replayed_arr = deepcopy(arr_base)
    apply_patches_in_place(replayed_arr, norm_arr)
    assert np.array_equal(replayed_arr["a"], np.array([3, 4]))
    apply_patches_in_place(replayed_arr, invert_patches(norm_arr))
    assert np.array_equal(replayed_arr["a"], np.array([1, 2]))

    # Major 4: Mutable str/int subclasses are rejected instead of leaking writable __dict__.
    class _Label(str):
        pass

    label = _Label("site")
    label.tags = []
    with pytest.raises(TypeError, match="cannot safely freeze"):
        deep_freeze({"label": label})

    class _LabelModel(BaseModel):
        model_config = ConfigDict(arbitrary_types_allowed=True)
        label: object

    ro_label = freeze_model(_LabelModel(label=label))
    with pytest.raises(TypeError, match="cannot safely freeze"):
        _ = ro_label.label


def test_astra_r1_ledger_immutability_relocations_profile_order_and_null_ref() -> None:
    """StyleLedger deeply freezes ref metadata, tracks STEBBS/profile relocations, and keeps ref: null."""
    # Minor 5: StyleLedger.ref_metadata deeply freezes NumPy arrays and bytearrays.
    ledger_arr = StyleLedger.from_mapping({
        "a": {"value": 1, "ref": {"samples": np.array([1, 2]), "raw": bytearray(b"ab")}}
    })
    ref_meta = ledger_arr.ref_metadata("a")
    assert ref_meta is not None
    assert ref_meta["raw"] == b"ab"
    with pytest.raises(ValueError, match="read-only"):
        ref_meta["samples"][0] = 99
    ref_view = ref_meta["samples"]
    ref_view.shape = (2, 1)
    assert ledger_arr.ref_metadata("a")["samples"].shape == (2,)  # type: ignore[index]

    # Minor 6: STEBBS flat physics relocation preserves ledger provenance on canonical path.
    stebbs_doc = ConfigDocument.from_dict(
        {"model": {"physics": {"rcmethod": {"value": 0, "ref": {"ID": "src-rc"}}}}},
        normalise=True,
    )
    canon_rc = "model.physics.stebbs.capacitance"
    assert stebbs_doc.style_ledger.ref_metadata(canon_rc) == {"ID": "src-rc"}
    rebuilt_rc = stebbs_doc.apply_patches([
        ConfigPatch(canon_rc, "delete"),
        ConfigPatch(canon_rc, "set", 1),
    ])
    assert rebuilt_rc.data["model"]["physics"]["stebbs"]["capacitance"] == {
        "value": 1,
        "ref": {"ID": "src-rc"},
    }

    # Minor 7: Profile-key remapping preserves authored key order on YAML dump.
    unsorted_prof_yaml = (
        "sites:\n"
        "  - properties:\n"
        "      irrigation:\n"
        "        wuprofa_24hr:\n"
        "          working_day:\n"
        "            2: 0.25\n"
        "            1: 0.5\n"
    )
    prof_doc = ConfigDocument.from_text(unsorted_prof_yaml, normalise=True)
    dumped_prof = prof_doc.dump_yaml(preserve_style=True)
    reloaded_wd = yaml.safe_load(dumped_prof)["sites"][0]["properties"]["irrigation"][
        "wuprofa_24hr"
    ]["working_day"]
    assert list(reloaded_wd.keys()) == ["2", "1"]

    # Minor 8: Delete/reinsert preserves explicit ref: null representations.
    ref_null_only = ConfigDocument.from_dict({"a": {"ref": None}}, normalise=False)
    assert ref_null_only.apply_patches([
        ConfigPatch("a", "delete"),
        ConfigPatch("a", "set", None),
    ]).data == {"a": {"ref": None}}
    val_ref_null = ConfigDocument.from_dict(
        {"a": {"value": 1, "ref": None}}, normalise=False
    )
    assert val_ref_null.apply_patches([
        ConfigPatch("a", "delete"),
        ConfigPatch("a", "set", 2),
    ]).data == {"a": {"value": 2, "ref": None}}


def test_astra_r1_adjustment_reconcile_and_context_annotation() -> None:
    """Coalescing an unowned patch reconciles adjustment new_value and ValidationContext accepts BaseModel."""
    # Minor 9: Owned patch coalesced with trailing unowned patch updates adjustment new_value.
    plan = PatchPlan()
    plan.record(
        PlannedAdjustment(parameter="a", old_value=1, new_value=2, reason="first"),
        ConfigPatch("a", "set", 2, default_wrapped=False),
    )
    plan.patches.append(ConfigPatch("a", "set", 3, default_wrapped=False))
    norm_plan = plan.normalised({"a": 1})
    assert len(norm_plan.patches) == 1
    assert norm_plan.patches[0].new_value == 3
    assert len(norm_plan.adjustments) == 1
    assert norm_plan.adjustments[0].old_value == 1
    assert norm_plan.adjustments[0].new_value == 3

    # Nit 10: ValidationContext.typed_model annotation includes BaseModel and ReadOnlyModelProxy.
    typed_ann = ValidationContext.__annotations__["typed_model"]
    assert "BaseModel" in str(typed_ann)
    assert "ReadOnlyModelProxy" in str(typed_ann)


def test_astra_r2_frozen_keys_bound_builtins_ip_and_eager_enum_snapshot() -> None:
    """Mutable mapping keys, bound built-in methods, and ipaddress objects are rejected; Enum attrs snapshot eagerly."""

    class _Label(str):
        pass

    label_key = _Label("k")
    label_key.tags = []
    # Major 6: mutable str-subclass keys are rejected in deep_freeze and ReadOnlyModelProxy.
    with pytest.raises(TypeError, match="cannot safely freeze"):
        deep_freeze({label_key: 1})

    class _EdgeProxyModel(BaseModel):
        model_config = ConfigDict(arbitrary_types_allowed=True)
        keyed: dict[object, int] = Field(default_factory=dict)
        callback: object = None
        net: object = None
        addr: object = None

    items: list[int] = []
    ro_edge = freeze_model(
        _EdgeProxyModel(
            keyed={label_key: 1},
            callback=items.append,
            net=ipaddress.ip_network("192.0.2.0/24"),
            addr=ipaddress.ip_address("192.0.2.1"),
        )
    )
    with pytest.raises(TypeError, match="cannot safely freeze"):
        _ = ro_edge.keyed
    # Major 7: bound built-in mutating method is rejected.
    with pytest.raises(TypeError, match="cannot safely freeze"):
        _ = ro_edge.callback
    assert items == []
    # Major 8: mutable ipaddress objects are rejected on both surfaces.
    with pytest.raises(TypeError, match="cannot safely freeze"):
        deep_freeze({"net": ipaddress.ip_network("192.0.2.0/24")})
    with pytest.raises(TypeError, match="cannot safely freeze"):
        _ = ro_edge.net
    with pytest.raises(TypeError, match="cannot safely freeze"):
        _ = ro_edge.addr

    # Minor 9: Enum public attributes are snapshotted at freeze_model() time, before first read.
    class _MutableAttrAfterFreeze(Enum):
        ITEM = 1

        def __init__(self, _val: int) -> None:
            self.label = "before"

    class _EnumHolder(BaseModel):
        member: _MutableAttrAfterFreeze = _MutableAttrAfterFreeze.ITEM

    ro_holder = freeze_model(_EnumHolder())
    _MutableAttrAfterFreeze.ITEM.label = "after"
    try:
        assert ro_holder.member.label == "before"
    finally:
        _MutableAttrAfterFreeze.ITEM.label = "before"


def test_astra_r2_null_ref_replay_numpy_plan_ownership_and_yaml_merge_keys() -> None:
    """Explicit ref: null replays without ledger, NumPy PatchPlan ownership matches safely, and YAML merge keys keep styles."""
    # Minor 10: normalised delete+set patches preserve explicit ref: null via apply_to_dict without a ledger.
    for raw_doc, new_val, expected in (
        ({"a": {"value": 1, "ref": None}}, 2, {"a": {"value": 2, "ref": None}}),
        ({"a": {"ref": None}}, None, {"a": {"ref": None}}),
    ):
        doc = ConfigDocument.from_dict(raw_doc, normalise=False)
        plan = PatchPlan(
            patches=[ConfigPatch("a", "delete"), ConfigPatch("a", "set", new_val)]
        )
        norm_plan = plan.normalised(doc)
        assert norm_plan.apply_to_dict(raw_doc) == expected

    # Minor 11: PatchPlan.normalised handles multiple same-path NumPy array patches without ValueError.
    p1 = ConfigPatch("a", "set", np.array([1.0, 2.0]), default_wrapped=False)
    p2 = ConfigPatch("a", "set", np.array([3.0, 4.0]), default_wrapped=False)
    np_plan = PatchPlan(
        patches=[p1, p2],
        adjustments=[
            PlannedAdjustment(
                parameter="a", old_value=None, new_value="arr", patches=(p2,)
            )
        ],
    )
    norm_np = np_plan.normalised({"a": np.array([0.0, 0.0])})
    assert len(norm_np.patches) == 1
    assert np.array_equal(norm_np.patches[0].new_value, np.array([3.0, 4.0]))
    assert len(norm_np.adjustments) == 1

    # Minor 12: YAML merge keys (<<: *anchor) succeed and preserve quoting/flow styles across the document.
    merge_yaml = (
        "defaults: &def\n"
        "  quoted: 'hello'\n"
        "section:\n"
        "  <<: *def\n"
        '  extra: "world"\n'
        "  flow_list: [1, 2, 3]\n"
    )
    ledger_merge = StyleLedger.from_yaml_text(merge_yaml)
    assert ledger_merge.get("section.quoted") is not None
    assert ledger_merge.get("section.quoted").scalar_style == "'"  # type: ignore[union-attr]
    assert ledger_merge.get("section.extra").scalar_style == '"'  # type: ignore[union-attr]
    assert ledger_merge.get("section.flow_list").flow_style is True  # type: ignore[union-attr]
    doc_merge = ConfigDocument.from_text(merge_yaml, normalise=False)
    assert doc_merge.style_ledger.source_line("section.extra") == 5
    dumped_merge = doc_merge.dump_yaml(preserve_style=True)
    assert "'hello'" in dumped_merge
    assert '"world"' in dumped_merge
    assert "[1, 2, 3]" in dumped_merge


def test_astra_r3_double_wrap_bare_containers_and_set_equality() -> None:
    """Wrapped insertions do not double-wrap, bare compound containers survive delete+set, and sets distinguish bool/int."""
    # Major 1: absent-key insertion of explicit RefValue / ref-only payload never double-wraps.
    for payload in (
        {"value": 1},
        {"value": 1, "ref": {"ID": "r"}},
        {"ref": {"ID": "r"}},
    ):
        patches = [ConfigPatch("a", "set", payload, default_wrapped=False)]
        direct: dict[str, object] = {}
        apply_patches_in_place(direct, patches)
        norm = normalise_patches(patches, {})
        replayed: dict[str, object] = {}
        apply_patches_in_place(replayed, norm)
        assert replayed == direct == {"a": payload}
        apply_patches_in_place(replayed, invert_patches(norm))
        assert replayed == {}

    # Major 2: bare compound sequences and section mappings stay bare after delete + set.
    doc_sites = ConfigDocument.from_dict({"sites": [{"gridiv": 1}]}, normalise=False)
    updated_sites = doc_sites.apply_patches([
        ConfigPatch("sites", "delete"),
        ConfigPatch("sites", "set", [{"gridiv": 2}]),
    ])
    assert updated_sites.data == {"sites": [{"gridiv": 2}]}
    yaml_sites = ConfigDocument.from_text("sites:\n  - gridiv: 1\n", normalise=False)
    assert yaml_sites.apply_patches([
        ConfigPatch("sites", "delete"),
        ConfigPatch("sites", "set", [{"gridiv": 2}]),
    ]).data == {"sites": [{"gridiv": 2}]}

    # Minor 4: set-valued patches distinguish bool from int ({1} vs {True}).
    set_base = {"x": {1}}
    set_norm = normalise_patches(
        [ConfigPatch("x", "set", {True}, default_wrapped=False)], set_base
    )
    assert len(set_norm) == 1
    replayed_set = deepcopy(set_base)
    apply_patches_in_place(replayed_set, set_norm)
    assert type(next(iter(replayed_set["x"]))) is bool

    # Minor 5: orthogonal physics relocation preserves ledger provenance on canonical path.
    ortho_doc = ConfigDocument.from_dict(
        {
            "model": {
                "physics": {
                    "kdown_split_method": {
                        "constant": {
                            "sw_dn_direct_frac": {
                                "value": 0.6,
                                "ref": {"ID": "frac-src"},
                            }
                        }
                    }
                }
            }
        },
        normalise=True,
    )
    frac_path = "model.physics.sw_dn_direct_frac"
    assert ortho_doc.style_ledger.ref_metadata(frac_path) == {"ID": "frac-src"}
    rebuilt_frac = ortho_doc.apply_patches([
        ConfigPatch(frac_path, "delete"),
        ConfigPatch(frac_path, "set", 0.7),
    ])
    assert rebuilt_frac.data["model"]["physics"]["sw_dn_direct_frac"] == {
        "value": 0.7,
        "ref": {"ID": "frac-src"},
    }


def test_astra_r3_model_dump_isolation_float_enum_eager_fields_and_copy_payload() -> (
    None
):
    """model_dump/property isolation, float-valued enums, eager field/array-enum snapshots, MappingProxyType, and _copy_payload hold."""
    from supy.data_model.core.timezone_enum import TimezoneOffset  # noqa: PLC0415

    # Major 3: model_dump(fallback=...) and @property cannot mutate the stored model snapshot.
    class _Payload:
        def __init__(self, items: list[int]) -> None:
            self.items = items

    class _DumpModel(BaseModel):
        model_config = ConfigDict(arbitrary_types_allowed=True)
        payload: _Payload
        val: int = 1

        @property
        def mutating_prop(self) -> int:
            self.payload.items.append(999)
            return self.val

    dm = _DumpModel(payload=_Payload([1]))
    ro_dm = freeze_model(dm)

    def _mutating_fallback(obj: object) -> str:
        if isinstance(obj, _Payload):
            obj.items.append(42)
        return "ok"

    dumped = ro_dm.model_dump(mode="json", fallback=_mutating_fallback)
    assert dumped["payload"] == "ok"
    assert ro_dm.mutating_prop == 1
    assert ro_dm.unwrap_copy().payload.items == [1]

    # Minor 6: float-derived enum (TimezoneOffset) preserves numeric reads via SiteView.
    sv = SiteView(
        index=0,
        raw={"properties": {"timezone": TimezoneOffset.UTC_PLUS_5_30}},
        name="s",
    )
    assert sv.timezone == pytest.approx(5.5)

    # Minor 7 & 8: model_fields metadata and object-array enums are snapshotted at freeze_model() time.
    class _ArrEnum(Enum):
        ITEM = 1

        def __init__(self, _val: int) -> None:
            self.label = "before"

    class _MetaModel(BaseModel):
        model_config = ConfigDict(arbitrary_types_allowed=True)
        x: int = Field(default=1, description="before")
        arr: np.ndarray = Field(
            default_factory=lambda: np.array([_ArrEnum.ITEM], dtype=object)
        )
        ro_map: object = Field(default_factory=lambda: MappingProxyType({"k": 1}))

    ro_meta = freeze_model(_MetaModel())
    orig_desc = _MetaModel.model_fields["x"].description
    _MetaModel.model_fields["x"].description = "after"
    _ArrEnum.ITEM.label = "after"
    try:
        assert ro_meta.model_fields["x"].description == "before"
        assert ro_meta.arr[0].label == "before"
        # Minor 9: MappingProxyType inside a model freezes without deepcopy TypeError.
        assert ro_meta.ro_map["k"] == 1
    finally:
        _MetaModel.model_fields["x"].description = orig_desc
        _ArrEnum.ITEM.label = "before"

    # Minor 10: _copy_payload detaches mutable primitive subclasses on ConfigPatch.
    class _MutLabel(str):
        pass

    lbl = _MutLabel("orig")
    lbl.tags = ["a"]
    patch_lbl = ConfigPatch("x", "set", lbl)
    lbl.tags.append("mutated")
    assert patch_lbl.new_value.tags == ["a"]


def test_r10_r1_provenance_replay_physics_selector_ledger_numeric_noops_and_single_line_block_scalars() -> (
    None
):
    """Mapping->doc replay never resurrects ref, physics selector ledger refreshes, numeric no-ops & single-line blocks work."""
    # Major 1: mapping-normalised delete+set does NOT resurrect prior ledger ref on ConfigDocument replay.
    raw_with_ref = {"a": {"value": 1, "ref": {"ID": "r1"}}}
    doc_with_ref = ConfigDocument.from_dict(raw_with_ref, normalise=False)
    del_set = [ConfigPatch("a", "delete"), ConfigPatch("a", "set", 2)]
    norm_from_map = normalise_patches(del_set, raw_with_ref)
    assert doc_with_ref.apply_patches(norm_from_map).data == {"a": {"value": 2}}
    # Re-normalising the mapping-normalised patches against ConfigDocument also keeps ref absent.
    renorm = doc_with_ref.normalise_patches(norm_from_map)
    assert doc_with_ref.apply_patches(renorm).data == {"a": {"value": 2}}
    # Explicit ref: null (write_has_ref=True, write_ref=None) never resurrects ledger ref {"ID": "r1"}.
    null_ref_replay = doc_with_ref.apply_patches([
        ConfigPatch("a", "delete"),
        ConfigPatch("a", "set", 2, write_style="ref_value", write_has_ref=True),
    ])
    assert null_ref_replay.data == {"a": {"value": 2, "ref": None}}

    # Major 4: physics normalisation refreshes selector PathStyle entries so delete+reinsert keeps wrapper & provenance.
    sh_doc = ConfigDocument.from_dict(
        {
            "model": {
                "physics": {
                    "storage_heat": {
                        "ohm": {
                            "include_qf": True,
                            "ref": {"ID": "ohm-src"},
                        }
                    },
                    "kdown_split_method": {
                        "constant": {
                            "sw_dn_direct_frac": {
                                "value": 0.6,
                                "ref": {"ID": "frac-src"},
                            },
                            "ref": {"ID": "kdown-src"},
                        }
                    },
                }
            }
        },
        normalise=True,
    )
    sh_path = "model.physics.storage_heat"
    kd_path = "model.physics.kdown_split_method"
    assert sh_doc.style_ledger.wrapper_style(sh_path) == "ref_value"
    assert sh_doc.style_ledger.ref_metadata(sh_path) == {"ID": "ohm-src"}
    assert sh_doc.style_ledger.wrapper_style(kd_path) == "ref_value"
    assert sh_doc.style_ledger.ref_metadata(kd_path) == {"ID": "kdown-src"}
    rebuilt_phys = sh_doc.apply_patches([
        ConfigPatch(sh_path, "delete"),
        ConfigPatch(sh_path, "set", 3),
        ConfigPatch(kd_path, "delete"),
        ConfigPatch(kd_path, "set", 1),
    ])
    assert rebuilt_phys.data["model"]["physics"]["storage_heat"] == {
        "value": 3,
        "ref": {"ID": "ohm-src"},
    }
    assert rebuilt_phys.data["model"]["physics"]["kdown_split_method"] == {
        "value": 1,
        "ref": {"ID": "kdown-src"},
    }
    # Also verify scheme + wrapped ohm_inc_qf form refreshes both selector and relocated sub-option provenance.
    sh_scheme_doc = ConfigDocument.from_dict(
        {
            "model": {
                "physics": {
                    "storage_heat": {
                        "scheme": "ohm",
                        "ohm_inc_qf": {"value": 1, "ref": {"ID": "qf-src"}},
                        "ref": {"ID": "scheme-src"},
                    }
                }
            }
        },
        normalise=True,
    )
    assert sh_scheme_doc.style_ledger.ref_metadata(sh_path) == {"ID": "scheme-src"}
    assert sh_scheme_doc.style_ledger.ref_metadata("model.physics.ohm_inc_qf") == {
        "ID": "qf-src"
    }
    assert sh_scheme_doc.apply_patches([
        ConfigPatch(sh_path, "delete"),
        ConfigPatch(sh_path, "set", 4),
    ]).data["model"]["physics"]["storage_heat"] == {
        "value": 4,
        "ref": {"ID": "scheme-src"},
    }

    # Minor 1: numeric type and signed-zero changes are NOT dropped as no-ops.
    num_base = {
        "i": 1,
        "z": 0.0,
        "nz": -0.0,
        "arr": np.array([0.0, np.nan]),
        "c": complex(0.0, 0.0),
    }
    num_patches = [
        ConfigPatch("i", "set", 1.0, default_wrapped=False),
        ConfigPatch("z", "set", -0.0, default_wrapped=False),
        ConfigPatch("nz", "set", -0.0, default_wrapped=False),  # true no-op
        ConfigPatch("arr", "set", np.array([-0.0, np.nan]), default_wrapped=False),
        ConfigPatch("c", "set", complex(-0.0, 0.0), default_wrapped=False),
    ]
    norm_num = normalise_patches(num_patches, num_base)
    assert [p.path for p in norm_num] == ["i", "z", "arr", "c"]
    replayed_num = deepcopy(num_base)
    apply_patches_in_place(replayed_num, norm_num)
    assert type(replayed_num["i"]) is float
    assert math.copysign(1.0, replayed_num["z"]) == -1.0
    assert bool(np.signbit(replayed_num["arr"][0])) is True
    assert math.copysign(1.0, replayed_num["c"].real) == -1.0
    apply_patches_in_place(replayed_num, invert_patches(norm_num))
    assert type(replayed_num["i"]) is int
    assert math.copysign(1.0, replayed_num["z"]) == 1.0
    assert bool(np.signbit(replayed_num["arr"][0])) is False
    assert math.copysign(1.0, replayed_num["c"].real) == 1.0

    # Minor 2: valid single-line |- and >- block scalars round-trip with block style preserved.
    block_yaml = (
        "lit: |-\n"
        "  single line literal\n"
        "fld: >-\n"
        "  single line folded\n"
        "wrapped:\n"
        "  value: |-\n"
        "    wrapped single line\n"
    )
    block_doc = ConfigDocument.from_text(block_yaml, normalise=False)
    dumped_block = block_doc.dump_yaml(preserve_style=True)
    assert "lit: |-\n  single line literal" in dumped_block
    assert "fld: >-\n  single line folded" in dumped_block
    assert "value: |-\n    wrapped single line" in dumped_block
    assert yaml.safe_load(dumped_block) == block_doc.data
    # Control/NEL character in a block-styled scalar falls back cleanly and preserves exact value.
    nel_doc = block_doc.apply_patches([ConfigPatch("lit", "set", "a\x85b")])
    dumped_nel = nel_doc.dump_yaml(preserve_style=True)
    assert yaml.safe_load(dumped_nel)["lit"] == "a\x85b"


def test_r10_r1_readonly_export_isolation_bound_methods_and_enums() -> None:
    """unwrap_copy/model_dump isolate bound methods and prevent mutable Enum singleton leaks."""

    # Major 2a: bound built-in method, MethodWrapperType, and partial are detached on freeze & unwrap_copy / model_dump.
    class _CallableModel(BaseModel):
        model_config = ConfigDict(arbitrary_types_allowed=True)
        cb: object
        setter: object
        part: object

    caller_list = [1]
    cm = _CallableModel(
        cb=caller_list.append,
        setter=caller_list.__setitem__,
        part=functools.partial(caller_list.append, 2),
    )
    ro_cm = freeze_model(cm)
    unwrapped_cm = ro_cm.unwrap_copy()
    unwrapped_cm.cb(99)
    unwrapped_cm.setter(0, 77)
    unwrapped_cm.part()
    dumped_cm = ro_cm.model_dump()
    dumped_cm["cb"](88)
    assert caller_list == [1]
    ro_cm.unwrap_copy().cb(55)
    assert caller_list == [1]

    # Major 2b: Enum singletons cannot leak mutably via unwrap_copy() or model_dump(mode="python").
    class _AttrEnum(Enum):
        ITEM = 1

        def __init__(self, _val: int) -> None:
            self.label = "orig"

    @dataclass
    class _EnumDC:
        member: _AttrEnum

    class _EnumExportModel(BaseModel):
        member: _AttrEnum = _AttrEnum.ITEM
        dc: _EnumDC = Field(default_factory=lambda: _EnumDC(member=_AttrEnum.ITEM))

    ro_enum_model = freeze_model(_EnumExportModel())
    with pytest.raises(TypeError, match="Enum singleton"):
        ro_enum_model.unwrap_copy()
    with pytest.raises(TypeError, match="Enum singleton"):
        ro_enum_model.member.unwrap_copy()
    with pytest.raises(TypeError, match="Enum singleton"):
        ro_enum_model.dc.unwrap_copy()

    _AttrEnum.ITEM.label = "mutated_after_freeze"
    try:
        dumped_py = ro_enum_model.model_dump()
        assert isinstance(dumped_py["member"], ReadOnlyEnumProxy)
        assert dumped_py["member"] == _AttrEnum.ITEM
        assert dumped_py["member"].label == "orig"
        with pytest.raises(TypeError, match="Cannot mutate attribute"):
            dumped_py["member"].label = "hacked"
        assert ro_enum_model.model_dump(mode="json")["member"] == 1
    finally:
        _AttrEnum.ITEM.label = "orig"


def test_r10_r1_mapping_proxy_private_attr_and_cycle_snapshot() -> None:
    """BaseModel containing MappingProxyType preserves PrivateAttr, model_extra, and cycles."""

    class _ProxyPrivateModel(BaseModel):
        model_config = ConfigDict(arbitrary_types_allowed=True, extra="allow")
        ro_map: object
        self_ref: object = None
        _secret: list[int] = PrivateAttr(default_factory=lambda: [10, 20])

        @property
        def secret_sum(self) -> int:
            return sum(self._secret)

    pm = _ProxyPrivateModel(ro_map=MappingProxyType({"k": [1]}), extra_val=42)
    pm._secret = [3, 4]
    pm.self_ref = pm
    ro_pm = freeze_model(pm)
    pm._secret.append(999)
    assert ro_pm.ro_map["k"] == (1,)
    assert ro_pm.secret_sum == 7
    assert ro_pm.extra_val == 42
    unwrapped_pm = ro_pm.unwrap_copy()
    assert unwrapped_pm._secret == [3, 4]
    assert unwrapped_pm.self_ref is unwrapped_pm
    assert unwrapped_pm.model_extra == {"extra_val": 42}
    unwrapped_pm._secret.append(100)
    assert ro_pm.secret_sum == 7


def test_r10_r2_stebbs_master_ledger_complex_nan_arrays_property_enum_and_numpy_dtype_isolation() -> (
    None
):
    """STEBBS master->section refreshes ledger, complex-NaN arrays are not no-ops, properties/serializers isolate enums, and NumPy dtypes detach."""
    # Major 1: STEBBS master toggle decomposition refreshes model.physics.stebbs to a bare container
    # while keeping provenance on model.physics.stebbs.enabled.
    stebbs_master_doc = ConfigDocument.from_dict(
        {"model": {"physics": {"stebbs": {"value": 1, "ref": {"ID": "src"}}}}},
        normalise=True,
    )
    assert (
        stebbs_master_doc.style_ledger.wrapper_style("model.physics.stebbs") == "bare"
    )
    assert (
        stebbs_master_doc.style_ledger.get("model.physics.stebbs").is_container is True
    )  # type: ignore[union-attr]
    assert stebbs_master_doc.style_ledger.ref_metadata("model.physics.stebbs") is None
    assert stebbs_master_doc.style_ledger.ref_metadata(
        "model.physics.stebbs.enabled"
    ) == {"ID": "src"}
    saved_block = stebbs_master_doc.read("model.physics.stebbs")
    rebuilt_stebbs = stebbs_master_doc.apply_patches([
        ConfigPatch("model.physics.stebbs", "delete"),
        ConfigPatch("model.physics.stebbs", "set", saved_block, default_wrapped=False),
    ])
    assert rebuilt_stebbs.data["model"]["physics"]["stebbs"] == saved_block
    assert "enabled" in rebuilt_stebbs.data["model"]["physics"]["stebbs"]
    assert "value" not in rebuilt_stebbs.data["model"]["physics"]["stebbs"]

    # Major 2: complex NumPy arrays with NaN in one component and differing other component are NOT no-ops.
    c_base = {
        "a": np.array([complex(np.nan, 1.0)]),
        "b": np.array([complex(np.nan, 0.0)]),
    }
    c_patches = [
        ConfigPatch(
            "a", "set", np.array([complex(np.nan, 2.0)]), default_wrapped=False
        ),
        ConfigPatch(
            "b", "set", np.array([complex(np.nan, -0.0)]), default_wrapped=False
        ),
    ]
    c_norm = normalise_patches(c_patches, c_base)
    assert [p.path for p in c_norm] == ["a", "b"]
    c_replayed = deepcopy(c_base)
    apply_patches_in_place(c_replayed, c_norm)
    assert c_replayed["a"][0].imag == pytest.approx(2.0)
    assert bool(np.signbit(c_replayed["b"][0].imag)) is True
    # Identical complex NaN + same imag stays a no-op.
    assert (
        normalise_patches(
            [
                ConfigPatch(
                    "a", "set", np.array([complex(np.nan, 1.0)]), default_wrapped=False
                )
            ],
            c_base,
        )
        == []
    )

    # Major 4: NumPy structured array and np.void snapshot copies detach mutable dtype.names.
    class _StructArrModel(BaseModel):
        model_config = ConfigDict(arbitrary_types_allowed=True)
        arr: np.ndarray
        scalar: object

    orig_arr = np.array([(1,)], dtype=[("x", "i4")])
    orig_void = np.array([(2,)], dtype=[("y", "i4")])[0]
    ro_sa = freeze_model(_StructArrModel(arr=orig_arr, scalar=orig_void))
    orig_arr.dtype.names = ("renamed_x",)
    orig_void.dtype.names = ("renamed_y",)
    assert ro_sa.arr.dtype.names == ("x",)
    assert "y" in ro_sa.scalar
    unwrapped_sa = ro_sa.unwrap_copy()
    unwrapped_sa.arr.dtype.names = ("unwrapped_x",)
    assert ro_sa.arr.dtype.names == ("x",)


def test_r10_r2_property_serializer_enum_rejection() -> None:
    """Properties, computed fields, and custom serializers on models with Enums are rejected explicitly."""

    class _PropEnum(Enum):
        ITEM = 1

        def __init__(self, _val: int) -> None:
            self.label = "before"

    class _PropEnumModel(BaseModel):
        member: _PropEnum = _PropEnum.ITEM

        @property
        def read_label(self) -> str:
            return self.member.label

        @property
        def is_item(self) -> bool:
            return self.member is _PropEnum.ITEM

        @property
        def mutate_label(self) -> str:
            self.member.label = "hacked"  # type: ignore[misc]
            return "bad"

        @field_serializer("member")
        def _ser_member(self, val: _PropEnum) -> str:
            return val.label

    ro_pe = freeze_model(_PropEnumModel())
    _PropEnum.ITEM.label = "after"
    try:
        assert ro_pe.member.label == "before"
        with pytest.raises(TypeError, match="Enum singletons"):
            _ = ro_pe.read_label
        with pytest.raises(TypeError, match="Enum singletons"):
            _ = ro_pe.is_item
        with pytest.raises(TypeError, match="Enum singletons"):
            _ = ro_pe.mutate_label
        with pytest.raises(TypeError, match="Enum singletons"):
            ro_pe.model_dump()
        with pytest.raises(TypeError, match="Enum singletons"):
            ro_pe.model_dump(mode="json")
        assert _PropEnum.ITEM.label == "after"
    finally:
        _PropEnum.ITEM.label = "before"


def test_r10_r3_tuple_baseline_nullify_bare_closures_repr_suews_int_enum_and_tzinfo() -> (
    None
):
    """Tuple baselines preserve type, nullify never resurrects removed ref, closures rejected, repr pure, SUEWS int enums & tzinfo hardened."""
    from supy.data_model.core.model import NetRadiationMethod  # noqa: PLC0415

    # R2 Major 1: baseline tuple replaced by list is NOT dropped as a no-op and inverts back to tuple.
    tup_base = {"a": (1, 2)}
    tup_patch = [ConfigPatch("a", "set", [1, 2], default_wrapped=False)]
    direct_tup = deepcopy(tup_base)
    apply_patches_in_place(direct_tup, tup_patch)
    norm_tup = normalise_patches(tup_patch, tup_base)
    assert len(norm_tup) == 1
    replayed_tup = deepcopy(tup_base)
    apply_patches_in_place(replayed_tup, norm_tup)
    assert replayed_tup == direct_tup == {"a": [1, 2]}
    assert type(replayed_tup["a"]) is list
    apply_patches_in_place(replayed_tup, invert_patches(norm_tup))
    assert replayed_tup == {"a": (1, 2)}
    assert type(replayed_tup["a"]) is tuple

    # R2 Major 2: raw replacement of ref_only -> bare followed by nullify_tree stays bare None.
    ref_doc = ConfigDocument.from_dict({"a": {"ref": {"ID": "src"}}}, normalise=False)
    seq = [
        ConfigPatch("a", "set", 1, replace_raw=True),
        ConfigPatch("a", "nullify_tree"),
    ]
    direct_null = ref_doc.apply_patches(seq)
    norm_null = ref_doc.normalise_patches(seq, expand_nullify_tree=True)
    replayed_null = ref_doc.apply_patches(norm_null)
    assert direct_null.data == replayed_null.data == {"a": None}
    assert replayed_null.apply_patches(invert_patches(norm_null)).data == ref_doc.data

    # R2 Major 3: closures capturing caller state are rejected rather than passed through by deepcopy.
    captured: list[int] = []

    class _ClosureModel(BaseModel):
        model_config = ConfigDict(arbitrary_types_allowed=True)
        fn: object

    def _record_item(item: int) -> None:
        captured.append(item)

    with pytest.raises(TypeError, match="Cannot safely detach function/closure"):
        freeze_model(_ClosureModel(fn=_record_item))
    assert captured == []

    # R2 Major 4: SUEWS ordinary Enum with custom __int__ converts via int() on ReadOnlyEnumProxy.
    frozen_nrad = deep_freeze(NetRadiationMethod.LDOWN_AIR)
    assert isinstance(frozen_nrad, ReadOnlyEnumProxy)
    assert int(frozen_nrad) == 3

    # R2 Major 5: repr(ro) does not execute mutating computed_field against the stored snapshot.
    class _ReprMutModel(BaseModel):
        items: list[int] = Field(default_factory=lambda: [1])

        @computed_field(repr=True)
        @property
        def sneaky(self) -> int:
            self.items.append(999)
            return len(self.items)

    ro_repr = freeze_model(_ReprMutModel())
    rendered = repr(ro_repr)
    assert "items=(1,)" in rendered
    assert ro_repr.items == (1,)

    # R2 Minor 6: aware datetime with mutable custom tzinfo is rejected, while UTC/ZoneInfo remain allowed.
    class _MutableTZ(datetime.tzinfo):
        def __init__(self, hours: int) -> None:
            self.hours = hours

        def utcoffset(self, dt: datetime.datetime | None) -> datetime.timedelta:
            return datetime.timedelta(hours=self.hours)

        def dst(self, dt: datetime.datetime | None) -> datetime.timedelta:
            return datetime.timedelta(0)

        def tzname(self, dt: datetime.datetime | None) -> str:
            return f"UTC+{self.hours}"

    safe_dt = datetime.datetime(2025, 1, 1, tzinfo=datetime.UTC)
    safe_zi = datetime.datetime(2025, 1, 1, tzinfo=ZoneInfo("UTC"))
    assert deep_freeze(safe_dt) is safe_dt
    assert deep_freeze(safe_zi) is safe_zi
    mut_dt = datetime.datetime(2025, 1, 1, tzinfo=_MutableTZ(1))
    with pytest.raises(TypeError, match="cannot safely freeze"):
        deep_freeze({"when": mut_dt})
