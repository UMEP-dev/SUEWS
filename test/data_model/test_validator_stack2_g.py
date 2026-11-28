"""Unit and integration tests for Stack 2 g: style ledger, patch normalisation, and read-only typed model."""

from __future__ import annotations

from collections import deque
from copy import copy, deepcopy
from dataclasses import FrozenInstanceError, dataclass
import datetime
from enum import Enum, IntEnum, StrEnum
from fractions import Fraction
import functools
from importlib.resources import files
import ipaddress
import math
from pathlib import Path, PurePosixPath, PureWindowsPath
from types import MappingProxyType
from uuid import UUID
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
    ReadOnlyTypeInfo,
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
    assert ro.model_type == ReadOnlyTypeInfo(
        name="SUEWSConfig",
        qualname="SUEWSConfig",
        module=SUEWSConfig.__module__,
    )
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

    with pytest.raises(TypeError, match="unvetted fallback"):
        ro_dm.model_dump(mode="json", fallback=_mutating_fallback)
    with pytest.raises(TypeError, match="unvetted property"):
        _ = ro_dm.mutating_prop
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
    with pytest.raises(TypeError, match="unvetted property"):
        _ = ro_pm.secret_sum
    assert ro_pm.extra_val == 42
    unwrapped_pm = ro_pm.unwrap_copy()
    assert unwrapped_pm._secret == [3, 4]
    assert unwrapped_pm.secret_sum == 7
    assert unwrapped_pm.self_ref is unwrapped_pm
    assert unwrapped_pm.model_extra == {"extra_val": 42}
    unwrapped_pm._secret.append(100)
    assert ro_pm.unwrap_copy()._secret == [3, 4]


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


def test_r10_r4_tuple_nullify_proxy_eq_hash_custom_deepcopy_and_payload_enum_scan() -> (
    None
):
    """Tuple nullification matches direct apply, eq/hash isolate snapshots, custom __deepcopy__ rejected, and nested payload enums detected."""
    # R3 Major 1: bare and RefValue-wrapped tuples nullify as leaves matching direct nullify_tree_in_place.
    for raw_tup, expected_null in (
        ({"a": (1, 2)}, {"a": None}),
        (
            {"a": {"value": (1, 2), "ref": {"ID": "t"}}},
            {"a": {"value": None, "ref": {"ID": "t"}}},
        ),
    ):
        for expand in (True, False):
            direct_t = deepcopy(raw_tup)
            apply_patches_in_place(direct_t, [ConfigPatch("a", "nullify_tree")])
            norm_t = normalise_patches(
                [ConfigPatch("a", "nullify_tree")], raw_tup, expand_nullify_tree=expand
            )
            replayed_t = deepcopy(raw_tup)
            apply_patches_in_place(replayed_t, norm_t)
            assert replayed_t == direct_t == expected_null
            if expand:
                assert type(norm_t[0].old_value) is tuple
                apply_patches_in_place(replayed_t, invert_patches(norm_t))
                assert replayed_t == raw_tup
                restored_val = (
                    replayed_t["a"]["value"]
                    if isinstance(replayed_t["a"], dict)
                    else replayed_t["a"]
                )
                assert type(restored_val) is tuple

    # R3 Major 2: __eq__ and __hash__ never mutate stored snapshots and Enum __eq__ never leaks singleton to reflected operand.
    class _MutEqHashModel(BaseModel):
        items: list[int] = Field(default_factory=lambda: [1])

        def __eq__(self, other: object) -> bool:
            self.items.append(99)
            return isinstance(other, _MutEqHashModel)

        def __hash__(self) -> int:
            self.items.append(88)
            return 7

    m_eq = _MutEqHashModel()
    ro_eq = freeze_model(m_eq)
    with pytest.raises(TypeError, match="unvetted custom __eq__"):
        _ = ro_eq == m_eq
    with pytest.raises(TypeError, match="unvetted custom __hash__"):
        hash(ro_eq)
    assert ro_eq.items == (1,)
    assert ro_eq.unwrap_copy().items == [1]

    class _SpyEnum(Enum):
        ITEM = 1

        def __init__(self, _val: int) -> None:
            self.label = "orig"

    class _SpyOperand:
        __hash__ = None  # type: ignore[assignment]

        def __eq__(self, other: object) -> bool:
            other.label = "hacked"  # type: ignore[attr-defined]
            return True

    ro_spy_enum = deep_freeze(_SpyEnum.ITEM)
    with pytest.raises(TypeError, match="Cannot mutate attribute"):
        _ = ro_spy_enum == _SpyOperand()
    assert _SpyEnum.ITEM.label == "orig"

    # R3 Major 3: custom __deepcopy__ returning shallow shared state is rejected explicitly.
    class _ShallowBox:
        def __init__(self, items: list[int]) -> None:
            self.items = items

        def __deepcopy__(self, _memo: dict[int, object]) -> _ShallowBox:
            return _ShallowBox(self.items)

    class _BoxModel(BaseModel):
        model_config = ConfigDict(arbitrary_types_allowed=True)
        box: _ShallowBox

    with pytest.raises(TypeError, match="opaque or custom copy semantics"):
        freeze_model(_BoxModel(box=_ShallowBox([1])))

    # R3 Major 4: Enum inside plain payload object is detected by _contains_enum and blocks property evaluation.
    class _PayloadWithEnum:
        def __init__(self, member: _SpyEnum) -> None:
            self.member = member

    class _NestedEnumPayloadModel(BaseModel):
        model_config = ConfigDict(arbitrary_types_allowed=True)
        payload: _PayloadWithEnum

        @property
        def mutate_nested_enum(self) -> str:
            self.payload.member.label = "mutated"
            return self.payload.member.label

    ro_nested_enum = freeze_model(
        _NestedEnumPayloadModel(payload=_PayloadWithEnum(_SpyEnum.ITEM))
    )
    with pytest.raises(TypeError, match="Enum singletons"):
        _ = ro_nested_enum.mutate_nested_enum
    assert _SpyEnum.ITEM.label == "orig"


def test_r10_r5_enum_hooks_nested_serializers_tuple_plan_and_engine() -> None:
    """Cover R4 M1-M4: enum hooks, nested serializers, tuple plan consistency, and engine boundary."""
    from typing import Annotated  # noqa: PLC0415

    from pydantic import PlainSerializer  # noqa: PLC0415

    from supy.data_model.validation.engine import validate_config  # noqa: PLC0415

    # M1: Enum __bool__/__int__/__float__ hooks never mutate the live singleton.
    class _HookEnum(Enum):
        ITEM = 1

        def __init__(self, _val: int) -> None:
            self.label = "orig"

        def __bool__(self) -> bool:
            self.label = "mutated_bool"
            return True

        def __int__(self) -> int:
            self.label = "mutated_int"
            return 99

        def __float__(self) -> float:
            self.label = "mutated_float"
            return 99.0

    ro_hook = deep_freeze(_HookEnum.ITEM)
    with pytest.raises(TypeError, match="custom __bool__"):
        bool(ro_hook)
    with pytest.raises(TypeError, match="custom __int__"):
        int(ro_hook)
    with pytest.raises(TypeError, match="custom __float__"):
        float(ro_hook)
    assert _HookEnum.ITEM.label == "orig"

    # M2: Nested Annotated[Enum, PlainSerializer(...)] inside container is blocked on model_dump.
    class _SerEnum(Enum):
        ITEM = 1

        def __init__(self, _val: int) -> None:
            self.label = "orig"

    def _mut_ser(member: _SerEnum) -> int:
        member.label = "mutated_ser"
        return member.value

    class _NestedSerModel(BaseModel):
        items: list[Annotated[_SerEnum, PlainSerializer(_mut_ser)]]

    ro_nser = freeze_model(_NestedSerModel(items=[_SerEnum.ITEM]))
    with pytest.raises(TypeError, match="Enum singletons"):
        ro_nser.model_dump(mode="json")
    assert _SerEnum.ITEM.label == "orig"

    # M3: PatchPlan.apply_to_dict and PatchPlan.normalised(base).apply_to_dict agree on tuple & frozen bases.
    tuple_base = {"a": (1, 2)}
    null_plan = PatchPlan(patches=[ConfigPatch("a", "nullify_tree")])
    assert null_plan.apply_to_dict(tuple_base) == {"a": None}
    assert null_plan.normalised(tuple_base).apply_to_dict(tuple_base) == {"a": None}
    frozen_list_doc = ConfigDocument.from_dict({"a": [1, 2]})
    frozen_view = frozen_list_doc.frozen_data()
    assert null_plan.apply_to_dict(frozen_view) == {"a": [None, None]}
    assert null_plan.normalised(frozen_view).apply_to_dict(frozen_view) == {
        "a": [None, None]
    }
    assert frozen_list_doc.apply_patches(
        frozen_list_doc.normalise_patches(null_plan.patches)
    ).data == {"a": [None, None]}

    # M4: Un-snapshotable private attribute on SUEWSConfig does not escape validate_config.
    cfg = SUEWSConfig()
    object.__setattr__(cfg, "_callback", lambda: None)
    report = validate_config(cfg, pipeline="B", forcing="off")
    assert report is not None


def test_r10_r5_merge_keys_defaultdict_mangled_slots_cycles_identity_zero_sign_and_v0() -> (
    None
):
    """Cover R4 m1-m7: wrapped merge keys, defaultdict, mangled slots, cycles, eq=False, zero sign, and V0."""
    from collections import defaultdict  # noqa: PLC0415
    from decimal import Decimal  # noqa: PLC0415

    # m1: Merge keys inside wrapped mapping values preserve StyleLedger styles.
    merge_yaml = (
        "defaults: &defaults {x: 1}\n"
        "label: 'quoted'\n"
        "wrapped:\n"
        "  value:\n"
        "    <<: *defaults\n"
        "    y: 2\n"
    )
    merge_ledger = StyleLedger.from_yaml_text(merge_yaml)
    assert merge_ledger.get("label") is not None
    assert merge_ledger.get("label").scalar_style == "'"
    assert merge_ledger.get("wrapped.value.x") is not None

    # m2: defaultdict factory preserved across snapshot & unwrap_copy.
    class _DDModel(BaseModel):
        model_config = ConfigDict(arbitrary_types_allowed=True)
        counts: defaultdict[str, int]

    dd = defaultdict(int, {"a": 1})
    ro_dd = freeze_model(_DDModel(counts=dd))
    unwrapped_dd = ro_dd.unwrap_copy()
    assert isinstance(unwrapped_dd.counts, defaultdict)
    assert unwrapped_dd.counts["missing"] == 0
    assert "missing" not in dd

    # m3: Name-mangled private slots preserved across snapshot & unwrap_copy.
    @dataclass
    class _MangledSlotDC:
        __slots__ = ("__cache", "x")
        x: int

        def __post_init__(self) -> None:
            self.__cache = [self.x * 2]

        @property
        def doubled(self) -> int:
            return self.__cache[0]

    ro_mangled = ReadOnlyDataclassProxy(_MangledSlotDC(5))
    with pytest.raises(TypeError, match="unvetted property"):
        _ = ro_mangled.doubled
    assert ro_mangled.unwrap_copy().doubled == 10

    # m4: Cyclic containers raise TypeError instead of RecursionError on read & deep_freeze.
    cyc: list[object] = []
    cyc.append(cyc)
    with pytest.raises(TypeError, match="cyclic container"):
        deep_freeze(cyc)

    class _CyclicModel(BaseModel):
        model_config = ConfigDict(arbitrary_types_allowed=True)
        payload: object

    ro_cyc = freeze_model(_CyclicModel(payload=cyc))
    with pytest.raises(TypeError, match="cyclic container"):
        _ = ro_cyc.payload

    # m5: Identity-equality dataclass proxy is equal to itself and has stable hash.
    @dataclass(eq=False)
    class _IdDC:
        x: int

    ro_id = ReadOnlyDataclassProxy(_IdDC(1))
    ro_id_alias = ro_id
    assert ro_id == ro_id_alias
    assert hash(ro_id) == hash(ro_id_alias)

    # m6: Strict no-op check preserves signed-zero changes in sets and Decimal.
    set_norm = normalise_patches(
        [ConfigPatch("s", "set", {-0.0}, default_wrapped=False)],
        {"s": {0.0}},
    )
    assert len(set_norm) == 1
    dec_norm = normalise_patches(
        [ConfigPatch("d", "set", Decimal("-0"), default_wrapped=False)],
        {"d": Decimal("0")},
    )
    assert len(dec_norm) == 1

    # m7: Zero-itemsize NumPy arrays materialise cleanly as read-only views.
    zero_arr = np.empty((2,), dtype="V0")
    frozen_zero = deep_freeze(zero_arr)
    assert frozen_zero.shape == (2,)
    assert frozen_zero.dtype == np.dtype("V0")
    assert not frozen_zero.flags.writeable


def test_r10_r6_nested_dataclass_computed_field_and_engine_proxy_guard() -> None:
    """Cover R5 M1-M2: nested Pydantic dataclass computed fields and engine proxy guard."""
    from pydantic import computed_field, field_serializer  # noqa: PLC0415
    from pydantic.dataclasses import dataclass as pdataclass  # noqa: PLC0415

    from supy.data_model.validation.engine import validate_config  # noqa: PLC0415

    # M1: Nested Pydantic dataclass @computed_field on Enum-bearing instance is blocked.
    class _DcEnum(Enum):
        ITEM = 1

        def __init__(self, _val: int) -> None:
            self.label = "orig"

    @pdataclass
    class _NestedPdc:
        member: _DcEnum

        @computed_field
        @property
        def mutated_label(self) -> str:
            self.member.label = "mutated_dc"
            return self.member.label

    class _OuterModel(BaseModel):
        dc: _NestedPdc

    ro_outer = freeze_model(_OuterModel(dc=_NestedPdc(member=_DcEnum.ITEM)))
    with pytest.raises(TypeError, match="Enum singletons"):
        ro_outer.model_dump()
    assert _DcEnum.ITEM.label == "orig"

    # M2: validate_config on a ReadOnlyModelProxy enforces serialisation isolation guard.
    class _MutConfig(SUEWSConfig):
        custom_enum: _DcEnum = _DcEnum.ITEM

        @field_serializer("custom_enum")
        def _ser_enum(self, val: _DcEnum) -> int:
            val.label = "mutated_engine"
            return val.value

    ro_mut_cfg = freeze_model(_MutConfig())
    with pytest.raises(TypeError, match="Enum singletons"):
        ro_mut_cfg.model_dump()
    report = validate_config(ro_mut_cfg, pipeline="B", forcing="off")
    assert report.has_errors
    assert _DcEnum.ITEM.label == "orig"


def test_r10_r6_masked_array_type_noop_nan_keys_memoryview_enum_drift_and_cycles() -> (
    None
):
    """Cover R5 m1-m8: masked arrays, type no-ops, NaN keys, memoryview, enum drift/hash, cycles."""
    import array  # noqa: PLC0415
    from enum import StrEnum  # noqa: PLC0415

    # m1: NumPy masked array is rejected explicitly instead of silently losing its mask.
    masked = np.ma.array([1.0, 2.0], mask=[False, True])

    class _ArrModel(BaseModel):
        model_config = ConfigDict(arbitrary_types_allowed=True)
        payload: object

    with pytest.raises(TypeError, match="opaque or custom copy semantics"):
        freeze_model(_ArrModel(payload=masked))
    with pytest.raises(TypeError, match="NumPy subclass"):
        ConfigPatch("a", "set", masked)

    # m2: bytes vs bytearray and StrEnum vs str are not dropped as no-ops.
    class _SE(StrEnum):
        A = "a"

    assert (
        len(
            normalise_patches(
                [ConfigPatch("x", "set", bytearray(b"a"), default_wrapped=False)],
                {"x": b"a"},
            )
        )
        == 1
    )
    assert (
        len(
            normalise_patches(
                [ConfigPatch("x", "set", "a", default_wrapped=False)],
                {"x": _SE.A},
            )
        )
        == 1
    )

    # m3: Distinct NaN mapping keys do not collapse during no-op detection.
    nan_a = float("nan")
    nan_b = float("nan")
    nan_norm = normalise_patches(
        [ConfigPatch("x", "set", {nan_a: 9, nan_b: 2}, replace_raw=True)],
        {"x": {nan_a: 1, nan_b: 2}},
    )
    assert len(nan_norm) == 1

    # m4: memoryview preserves format and shape across freeze, property, and unwrap_copy.
    class _MvModel(BaseModel):
        model_config = ConfigDict(arbitrary_types_allowed=True)
        payload: object

        @property
        def first(self) -> int:
            return int(self.payload[0])

    buf = array.array("I", [257])
    ro_mv = freeze_model(_MvModel(payload=memoryview(buf)))
    buf[0] = 999
    with pytest.raises(TypeError, match="unvetted property"):
        _ = ro_mv.first
    unwrapped_mv = ro_mv.unwrap_copy().payload
    assert unwrapped_mv.format == "I"
    assert unwrapped_mv[0] == 257


def test_r10_r6_enum_drift_identity_hash_and_object_array_cycles() -> None:
    """Cover R5 m5-m8: enum drift/identity-hash and self-referential object array/enum cycles."""

    # m5 & m6: Enum snapshots freeze name/value/hash against live _value_/_name_ tampering & keep object.__hash__.
    class _IdHashEnum(Enum):
        ITEM = 1
        __hash__ = object.__hash__

    ro_id_enum = deep_freeze(_IdHashEnum.ITEM)
    assert ro_id_enum == _IdHashEnum.ITEM
    assert hash(ro_id_enum) == hash(_IdHashEnum.ITEM)

    class _DriftEnum(Enum):
        ITEM = 1

    class _DriftModel(BaseModel):
        e: _DriftEnum

    ro_drift = freeze_model(_DriftModel(e=_DriftEnum.ITEM))
    orig_hash = hash(ro_drift.e)
    _DriftEnum.ITEM._value_ = 2
    _DriftEnum.ITEM._name_ = "RENAMED"
    try:
        assert ro_drift.e.name == "ITEM"
        assert ro_drift.e.value == 1
        assert hash(ro_drift.e) == orig_hash
        with pytest.raises(TypeError, match="mutated externally"):
            ro_drift.model_dump(mode="json")
    finally:
        _DriftEnum.ITEM._value_ = 1
        _DriftEnum.ITEM._name_ = "ITEM"

    # m7 & m8: Self-referential object array and cyclic Enum attrs raise TypeError instead of RecursionError.
    class _ObjArrModel(BaseModel):
        model_config = ConfigDict(arbitrary_types_allowed=True)
        payload: object

    self_arr = np.empty(1, dtype=object)
    self_arr[0] = self_arr
    with pytest.raises(TypeError, match="cyclic container"):
        deep_freeze(self_arr)
    ro_self_arr = freeze_model(_ObjArrModel(payload=self_arr))
    with pytest.raises(TypeError, match="cyclic container"):
        _ = ro_self_arr.payload

    class _CyclicEnum(Enum):
        ITEM = 1

    _CyclicEnum.ITEM.peer = _CyclicEnum.ITEM
    try:
        with pytest.raises(TypeError, match="mutable payload"):
            deep_freeze(_CyclicEnum.ITEM)
    finally:
        del _CyclicEnum.ITEM.peer


def test_r10_r7_model_dump_override_type_descriptors_bound_methods_and_engine_drift() -> (
    None
):
    """Cover R6 M1-M4: overridden model_dump, ReadOnlyTypeInfo, MethodType rejection, and engine drift."""
    from supy.data_model.core.model import NetRadiationMethod  # noqa: PLC0415
    from supy.data_model.validation.engine import validate_config  # noqa: PLC0415

    # M1: Overridden model_dump on an Enum-bearing model is rejected before execution.
    class _OvEnum(Enum):
        ITEM = 1

        def __init__(self, _val: int) -> None:
            self.label = "orig"

    class _OvDumpModel(BaseModel):
        e: _OvEnum = _OvEnum.ITEM

        def model_dump(self, *args: object, **kwargs: object) -> dict[str, object]:
            del args, kwargs
            self.e.label = "mutated_override"
            return {}

    ro_ov = freeze_model(_OvDumpModel())
    with pytest.raises(TypeError, match="Enum singletons"):
        ro_ov.model_dump()
    assert _OvEnum.ITEM.label == "orig"

    # M2: enum_type, model_type, dataclass_type, and field annotations return immutable descriptors, never live classes.
    frozen_e = deep_freeze(_OvEnum.ITEM)
    assert isinstance(frozen_e.enum_type, ReadOnlyTypeInfo)
    assert frozen_e.enum_type.name == "_OvEnum"
    assert isinstance(ro_ov.model_type, ReadOnlyTypeInfo)
    assert isinstance(ro_ov.model_fields["e"].annotation, ReadOnlyTypeInfo)
    assert not isinstance(ro_ov.model_fields["e"].annotation, type)

    # M3: Python MethodType bound methods are rejected so global state cannot be mutated.
    events: list[int] = []

    class _Sink:
        def add(self, value: int) -> None:
            events.append(value)

    class _SinkModel(BaseModel):
        model_config = ConfigDict(arbitrary_types_allowed=True)
        callback: object

    with pytest.raises(TypeError, match="Python bound method"):
        freeze_model(_SinkModel(callback=_Sink().add))
    assert events == []

    # M4: validate_config detects post-freeze Enum _value_ drift and returns structured error.
    cfg = SUEWSConfig()
    ro_cfg = freeze_model(cfg)
    orig_val = NetRadiationMethod.LDOWN_AIR._value_
    NetRadiationMethod.LDOWN_AIR._value_ = 9999
    try:
        report = validate_config(ro_cfg, pipeline="C", forcing="off")
        assert report.has_errors
    finally:
        NetRadiationMethod.LDOWN_AIR._value_ = orig_val


def test_r10_r7_list_subclass_tuple_cycle_slot_metadata_and_refvalue_eq() -> None:
    """Cover R6 m1-m5: list subclass, tuple/list cycle identity, drift base64, slot metadata, RefValue eq."""
    from supy.data_model.core.type import RefValue  # noqa: PLC0415

    # m1: List subclasses are rejected explicitly instead of silently losing attributes.
    class _Samples(list):
        def __init__(self, values: list[float], unit: str) -> None:
            super().__init__(values)
            self.unit = unit

    class _SampleModel(BaseModel):
        model_config = ConfigDict(arbitrary_types_allowed=True)
        samples: object

    with pytest.raises(TypeError, match="opaque or custom copy semantics"):
        freeze_model(_SampleModel(samples=_Samples([1.0], "K")))

    # m2: Tuple/list cycles preserve exact cycle object identity after snapshotting.
    items: list[object] = []
    payload = (items,)
    items.append(payload)

    class _CycleTupleModel(BaseModel):
        model_config = ConfigDict(arbitrary_types_allowed=True)
        payload: object

    unwrapped_cyc = freeze_model(_CycleTupleModel(payload=payload)).unwrap_copy()
    assert unwrapped_cyc.payload[0][0] is unwrapped_cyc.payload
    assert unwrapped_cyc.payload is not payload

    # m3: Drift rejection preserves ConfigDict(ser_json_bytes="base64") contract (no silent UTF-8 fallback).
    class _B64Enum(Enum):
        ITEM = 1

    class _B64Model(BaseModel):
        model_config = ConfigDict(ser_json_bytes="base64")
        e: _B64Enum = _B64Enum.ITEM
        raw: bytes = b"\xff"

    ro_b64 = freeze_model(_B64Model())
    assert ro_b64.model_dump(mode="json") == {"e": 1, "raw": "_w=="}
    _B64Enum.ITEM._value_ = 2
    try:
        with pytest.raises(TypeError, match="mutated externally"):
            ro_b64.model_dump(mode="json")
    finally:
        _B64Enum.ITEM._value_ = 1

    # m4: ReadOnlyFieldInfo, ReadOnlyTypeInfo, PathStyle, and StyleLedger have no writable __dict__.
    info = ro_b64.model_fields["raw"]
    assert not hasattr(info, "__dict__")
    assert not hasattr(ro_b64.model_type, "__dict__")
    ledger = StyleLedger.from_yaml_text("a: 1\n")
    assert not hasattr(ledger, "__dict__")
    assert not hasattr(ledger.get("a"), "__dict__")

    # m5: Frozen RefValue preserves scalar equality.
    ro_ref = freeze_model(RefValue(3))
    assert ro_ref == 3
    assert ro_ref != 4
    assert RefValue(3) == ro_ref


def test_r10_r8_slotted_views_dtype_metadata_vetted_hooks_and_physics_enum() -> None:
    """Cover R7 M1-M4 and m5: slotted views, dtype metadata Enum rejection, vetted hooks, PhysicsView Enum."""
    from typing import ClassVar  # noqa: PLC0415

    from pydantic import field_serializer  # noqa: PLC0415

    from supy.data_model.core.model import NetRadiationMethod  # noqa: PLC0415
    from supy.data_model.validation.pipeline.phase_b_rules.rules_core import (  # noqa: PLC0415
        ValidationContext,
    )

    # M1: ValidationContext and TypedConfigView hierarchy have no writable __dict__.
    ctx = ValidationContext(yaml_data={"model": {"physics": {}}, "sites": []})
    assert not hasattr(ctx, "__dict__")
    assert not hasattr(ctx.view, "__dict__")
    assert not hasattr(ctx.view.physics, "__dict__")
    assert not hasattr(ctx.view.control, "__dict__")

    # M2: NumPy dtype metadata carrying Enum or mutable payload is rejected on freeze & detach.
    class _MetaEnum(Enum):
        ITEM = 1

    arr_enum_meta = np.array([1], dtype=np.dtype("i4", metadata={"m": _MetaEnum.ITEM}))
    with pytest.raises(TypeError, match="mutable or Enum metadata"):
        deep_freeze(arr_enum_meta)

    class _MetaModel(BaseModel):
        model_config = ConfigDict(arbitrary_types_allowed=True)
        arr: object

    with pytest.raises(TypeError, match="mutable or Enum metadata"):
        freeze_model(_MetaModel(arr=arr_enum_meta))

    # M3 & M4: Enum-free ClassVar-mutating property/serializer rejected; vetted SUEWS properties still work.
    class _ClassVarModel(BaseModel):
        shared: ClassVar[list[int]] = []
        x: int = 1

        @property
        def bad_prop(self) -> int:
            self.shared.append(1)
            return self.x

        @field_serializer("x")
        def _bad_ser(self, val: int) -> int:
            self.shared.append(2)
            return val

    ro_cv = freeze_model(_ClassVarModel())
    with pytest.raises(TypeError, match="unvetted property"):
        _ = ro_cv.bad_prop
    with pytest.raises(TypeError, match="unvetted"):
        ro_cv.model_dump()
    assert _ClassVarModel.shared == []

    # Vetted SUEWS property works cleanly on frozen SUEWSConfig.
    ro_cfg = freeze_model(SUEWSConfig())
    _ = ro_cfg.model.control.forcing_file

    # m5: PhysicsView.code handles Enum values inside physics-family mappings after freezing.
    pv = PhysicsView({
        "net_radiation": {"narp": {"value": NetRadiationMethod.LDOWN_AIR}}
    })
    assert pv.code("net_radiation") == 3


def test_r10_r8_dtype_metadata_noop_reflexive_eq_partial_state_and_cycles() -> None:
    """Cover R7 m1-m4: metadata-only array patches, reflexive nested eq=False, partial state/cycles, namedtuple."""
    from typing import NamedTuple  # noqa: PLC0415

    # m1: Metadata-only NumPy dtype patch is NOT dropped as a no-op.
    arr_m = np.array([1], dtype=np.dtype("i4", metadata={"unit": "m"}))
    arr_k = np.array([1], dtype=np.dtype("i4", metadata={"unit": "K"}))
    meta_norm = normalise_patches(
        [ConfigPatch("a", "set", arr_k, replace_raw=True)],
        {"a": arr_m},
    )
    assert len(meta_norm) == 1

    # m2: Proxy equality is reflexive even with nested @dataclass(eq=False) tokens.
    @dataclass(eq=False)
    class _Token:
        tag: str

    @dataclass
    class _Holder:
        tok: _Token

    class _PydHolder(BaseModel):
        model_config = ConfigDict(arbitrary_types_allowed=True)
        tok: _Token

    ro_dc = ReadOnlyDataclassProxy(_Holder(_Token("t")))
    ro_dc_alias = ro_dc
    assert ro_dc == ro_dc_alias
    ro_pyd = freeze_model(_PydHolder(tok=_Token("t")))
    ro_pyd_alias = ro_pyd
    assert ro_pyd == ro_pyd_alias

    # m3 & m4: functools.partial preserves extra __dict__ state, rejects cycles; namedtuple subclass with extra state rejected.
    class _ObjModel(BaseModel):
        model_config = ConfigDict(arbitrary_types_allowed=True)
        val: object

    p_extra = functools.partial(pow, 2)
    p_extra.label = "original"
    ro_part = freeze_model(_ObjModel(val=p_extra))
    p_extra.label = "changed"
    assert ro_part.unwrap_copy().val.label == "original"

    p_cyc = functools.partial(max)
    p_cyc.keywords["default"] = p_cyc
    with pytest.raises(TypeError, match=r"cyclic functools\.partial"):
        freeze_model(_ObjModel(val=p_cyc))

    class _BasePt(NamedTuple):
        x: int

    class _SubPt(_BasePt):
        pass

    sub_pt = _SubPt(1)
    sub_pt.unit = "m"
    with pytest.raises(TypeError, match="opaque or custom copy semantics"):
        freeze_model(_ObjModel(val=sub_pt))


def test_r10_r9_singleton_new_metadata_hooks_reinit_and_from_config_isolation() -> None:
    """Cover R8 M1-M4: Singleton.__new__, metadata deepcopy/repr hooks, proxy re-init, TypedConfigView.from_config."""
    from typing import ClassVar  # noqa: PLC0415

    from pydantic import field_serializer  # noqa: PLC0415

    # M1: BaseModel overriding __new__ as singleton still clones into an isolated snapshot.
    class _SingletonModel(BaseModel):
        cached: ClassVar[object] = None
        x: int

        def __new__(cls, **kwargs: object) -> _SingletonModel:
            del kwargs
            if cls.cached is None:
                cls.cached = object.__new__(cls)
            return cls.cached  # type: ignore[return-value]

    orig_s = _SingletonModel(x=1)
    ro_s = freeze_model(orig_s)
    orig_s.x = 2
    assert ro_s.x == 1

    # M2: Field default/metadata with custom __deepcopy__/__repr__ never executes hooks on freeze.
    hook_events: list[str] = []

    class _HookDefault:
        def __deepcopy__(self, _memo: dict[int, object]) -> _HookDefault:
            hook_events.append("deepcopy")
            return self

        def __repr__(self) -> str:
            hook_events.append("repr")
            return "HookDefault"

    class _MetaHookModel(BaseModel):
        model_config = ConfigDict(arbitrary_types_allowed=True)
        payload: object = _HookDefault()

    mh = _MetaHookModel(payload=None)
    hook_events.clear()
    ro_mh = freeze_model(mh)
    assert hook_events == []
    assert ro_mh.model_fields["payload"].default == "<_HookDefault>"

    # M3: Existing model/dataclass/enum proxies reject __init__ re-initialisation.
    @dataclass
    class _SimpleDC:
        v: int

    class _SimpleE(Enum):
        A = 1

    ro_dc = ReadOnlyDataclassProxy(_SimpleDC(1))
    ro_e = deep_freeze(_SimpleE.A)
    with pytest.raises(TypeError, match="re-initialised"):
        ro_s.__init__(_SingletonModel(x=9))
    with pytest.raises(TypeError, match="re-initialised"):
        ro_dc.__init__(_SimpleDC(9))
    with pytest.raises(TypeError, match="re-initialised"):
        ro_e.__init__(_SimpleE.A)
    assert ro_s.x == 1
    assert ro_dc.v == 1

    # M4: TypedConfigView.from_config routes raw models through vetted serializer isolation.
    class _MutSerCfg(BaseModel):
        items: list[int] = Field(default_factory=lambda: [1])

        @field_serializer("items")
        def _mut_ser(self, val: list[int]) -> list[int]:
            self.items.append(99)
            return val

    raw_mut = _MutSerCfg()
    with pytest.raises(TypeError, match="unvetted"):
        TypedConfigView.from_config(raw_mut)
    assert raw_mut.items == [1]


def test_r10_r9_composite_hash_enum_type_drift_stebbs_intenum_doc_tuples_and_temporal() -> (
    None
):
    """Cover R8 m1-m6: stable composite hash, exact-type Enum drift, STEBBS IntEnum, doc tuples, drop_noops, temporal."""

    # m1: Composite proxy hashes remain stable for nested eq=False tokens and post-freeze Enum _name_ changes.
    @dataclass(eq=False)
    class _EqFalseTok:
        v: int

    @dataclass(frozen=True)
    class _TokHolder:
        tok: _EqFalseTok

    ro_th = ReadOnlyDataclassProxy(_TokHolder(_EqFalseTok(1)))
    assert hash(ro_th) == hash(ro_th)

    class _HashEnum(Enum):
        ITEM = 1

    @dataclass(frozen=True)
    class _EnumHolderDC:
        e: _HashEnum

    ro_eh = ReadOnlyDataclassProxy(_EnumHolderDC(_HashEnum.ITEM))
    h_before = hash(ro_eh)
    _HashEnum.ITEM._name_ = "TAMPERED"
    try:
        assert hash(ro_eh) == h_before
    finally:
        _HashEnum.ITEM._name_ = "ITEM"

    # m2: Enum drift detects 1 -> 1.0 / True and 0.0 -> -0.0 representation changes.
    class _TypeDriftEnum(Enum):
        ONE = 1
        ZERO = 0.0

    class _TypeDriftModel(BaseModel):
        e: _TypeDriftEnum

    ro_one = freeze_model(_TypeDriftModel(e=_TypeDriftEnum.ONE))
    _TypeDriftEnum.ONE._value_ = 1.0
    try:
        with pytest.raises(TypeError, match="mutated externally"):
            ro_one.model_dump(mode="json")
    finally:
        _TypeDriftEnum.ONE._value_ = 1

    ro_zero = freeze_model(_TypeDriftModel(e=_TypeDriftEnum.ZERO))
    _TypeDriftEnum.ZERO._value_ = -0.0
    try:
        with pytest.raises(TypeError, match="mutated externally"):
            ro_zero.model_dump(mode="json")
    finally:
        _TypeDriftEnum.ZERO._value_ = 0.0

    # m3: STEBBS IntEnum inputs work through PhysicsView.stebbs_method.
    class _Choice(IntEnum):
        DEFAULT = 1

    assert (
        PhysicsView({
            "stebbs": {"enabled": True, "parameters": _Choice.DEFAULT}
        }).stebbs_method
        == 1
    )
    assert PhysicsView({"stebbs": _Choice.DEFAULT}).stebbs_method == 1

    # m4: Raw tuple patches stay tuples across ConfigDocument.clone/apply_patches/normalise.
    doc_t = ConfigDocument.from_dict({"a": 0}, normalise=False)
    doc_t = doc_t.apply_patches([ConfigPatch("a", "set", (1, 2), replace_raw=True)])
    assert isinstance(doc_t.data["a"], tuple)
    assert isinstance(doc_t.apply_patches([]).data["a"], tuple)
    norm_t = doc_t.normalise_patches([
        ConfigPatch("a", "set", [1, 2], replace_raw=True)
    ])
    assert len(norm_t) == 1
    assert isinstance(
        doc_t.apply_patches(norm_t).apply_patches(invert_patches(norm_t)).data["a"],
        tuple,
    )

    # m5: drop_noops=False preserves unexpanded nullify_tree on already-null target.
    kept_null = normalise_patches(
        [ConfigPatch("a", "nullify_tree")],
        {"a": None},
        expand_nullify_tree=False,
        drop_noops=False,
        coalesce=False,
    )
    assert len(kept_null) == 1
    assert kept_null[0].action == "nullify_tree"
    assert kept_null[0].old_value == {}

    # m6: NumPy temporal scalar unit changes (D vs s) are NOT dropped as no-ops.
    assert (
        len(
            normalise_patches(
                [
                    ConfigPatch(
                        "t", "set", np.datetime64("2020-01-01", "s"), replace_raw=True
                    )
                ],
                {"t": np.datetime64("2020-01-01", "D")},
            )
        )
        == 1
    )
    assert (
        len(
            normalise_patches(
                [ConfigPatch("d", "set", np.timedelta64(1, "s"), replace_raw=True)],
                {"d": np.timedelta64(1000, "ms")},
            )
        )
        == 1
    )


def test_r10_r10_class_backed_dataclass_defaults_and_getattribute_descriptors() -> None:
    """Cover R9 M1-M2: class-backed init=False dataclass defaults detached; __getattribute__/descriptors blocked."""
    from dataclasses import field as dc_field  # noqa: PLC0415

    # M1: Class-backed dataclass default (init=False, default=...) is defensively detached into snapshot.
    @dataclass(eq=False)
    class _ChildBag:
        items: list[int]

    @dataclass
    class _ParentDefault:
        child: _ChildBag = dc_field(init=False, default=_ChildBag([1]))

    orig_parent = _ParentDefault()
    ro_parent = ReadOnlyDataclassProxy(orig_parent)
    unwrapped_parent = ro_parent.unwrap_copy()
    unwrapped_parent.child.items.append(2)
    assert orig_parent.child.items == [1]
    orig_parent.child.items.append(99)
    assert ro_parent.child.items == (1,)

    # M2: Custom __getattribute__ and field descriptors cannot execute against stored snapshots.
    leaked: list[object] = []

    @dataclass
    class _EvilGetAttrDC:
        x: int
        y: int

        def __getattribute__(self, name: str) -> object:
            if name == "x":
                leaked.append(self)
            return object.__getattribute__(self, name)

    with pytest.raises(TypeError, match="__getattribute__"):
        ReadOnlyDataclassProxy(_EvilGetAttrDC(1, 2))
    assert leaked == []

    class _LeakDesc:
        def __get__(self, inst: object, _owner: object = None) -> int:
            if inst is not None:
                leaked.append(inst)
            return 42

    @dataclass
    class _DescDC:
        x: int = dc_field(init=False, default=_LeakDesc())  # type: ignore[assignment]

    with pytest.raises(TypeError, match="unvetted descriptor"):
        ReadOnlyDataclassProxy(_DescDC())
    assert leaked == []


def test_r10_r10_dst_fold_signed_zero_coalesce_unnamed_flag_nested_enum_and_orthogonal() -> (
    None
):
    """Cover R9 m1-m6: DST fold, signed-zero coalescing, unnamed Flag, nested Enum drift, eq/hash, orthogonal."""
    import datetime as dt  # noqa: PLC0415
    from enum import Flag, StrEnum  # noqa: PLC0415
    from zoneinfo import ZoneInfo  # noqa: PLC0415

    # m1: Distinct DST fold instants are NOT dropped as no-ops.
    before_dst = dt.datetime(
        2024, 10, 27, 1, 30, tzinfo=ZoneInfo("Europe/London"), fold=0
    )
    after_dst = before_dst.replace(fold=1)
    dst_norm = normalise_patches(
        [ConfigPatch("t", "set", after_dst, replace_raw=True)],
        {"t": before_dst},
    )
    assert len(dst_norm) == 1

    # m2: Coalescing preserves delete(0.0) + set(-0.0) key-sign changes.
    sz_base: dict[str, object] = {"p": {0.0: 1}}
    sz_patches = [
        ConfigPatch("p[0.0]", "delete", path_tokens=("p", MappingKey(0.0))),
        ConfigPatch(
            "p[-0.0]",
            "set",
            1,
            default_wrapped=False,
            path_tokens=("p", MappingKey(-0.0)),
        ),
    ]
    sz_norm = normalise_patches(sz_patches, sz_base)
    sz_replayed = deepcopy(sz_base)
    apply_patches_in_place(sz_replayed, sz_norm)
    only_key = next(iter(sz_replayed["p"]))  # type: ignore[arg-type]
    assert bool(np.signbit(only_key)) is True

    # m3: Unnamed zero-valued Flag keeps name=None, matches hash, and dumps without false drift.
    class _Bits(Flag):
        A = 1

    class _BitsModel(BaseModel):
        b: _Bits = _Bits(0)

    ro_b0 = deep_freeze(_Bits(0))
    assert ro_b0.name is None
    assert hash(ro_b0) == hash(_Bits(0))
    ro_bm = freeze_model(_BitsModel())
    assert ro_bm.model_dump(mode="json") == {"b": 0}

    # m4: Nested Enum values do not trigger false drift on model_dump.
    class _InnerE(Enum):
        ITEM = 1

    class _OuterE(Enum):
        ITEM = _InnerE.ITEM

    class _NestedEModel(BaseModel):
        e: _OuterE = _OuterE.ITEM

    ro_ne = freeze_model(_NestedEModel())
    assert ro_ne.model_dump()["e"] == _OuterE.ITEM

    # m5: Enum snapshots frozen before vs after _name_ mutation disagree consistently on == and hash.
    class _MutNameE(Enum):
        ITEM = 1

    p_snap = deep_freeze(_MutNameE.ITEM)
    _MutNameE.ITEM._name_ = "CHANGED"
    try:
        q_snap = deep_freeze(_MutNameE.ITEM)
        assert p_snap != q_snap
        assert hash(p_snap) != hash(q_snap)
    finally:
        _MutNameE.ITEM._name_ = "ITEM"

    # m6: Orthogonal physics forms accept IntEnum / StrEnum inside frozen PhysicsView.
    class _YesNo(IntEnum):
        YES = 1

    class _SchemeName(StrEnum):
        NARP = "narp"

    assert (
        PhysicsView({"storage_heat": {"ohm": {"include_qf": _YesNo.YES}}}).code(
            "storage_heat"
        )
        == 1
    )
    assert (
        PhysicsView({
            "net_radiation": {"scheme": _SchemeName.NARP, "ldown": "air"}
        }).code("net_radiation")
        == 3
    )


def test_r10_m1_copy_on_read_model_and_dataclass_isolation() -> None:
    """R10-M1: Path and Fraction stay typed while isolating CPython private-slot mutation; UUID is rejected."""
    from typing import Annotated  # noqa: PLC0415

    @dataclass
    class _StdlibDC:
        p: PurePosixPath
        f: Fraction

    class _StdlibModel(BaseModel):
        model_config = ConfigDict(arbitrary_types_allowed=True)
        path_val: Annotated[Path, Fraction(1, 4)] = Field(default=Path("/default/path"))
        pure_win: PureWindowsPath = PureWindowsPath("C:/suews/forcing.txt")
        ratio: Fraction = Fraction(1, 3)
        nested: _StdlibDC

    orig_path = Path("/orig/forcing.txt")
    orig_win = PureWindowsPath("C:/orig/win.txt")
    orig_frac = Fraction(1, 3)
    model = _StdlibModel(
        path_val=orig_path,
        pure_win=orig_win,
        ratio=orig_frac,
        nested=_StdlibDC(PurePosixPath("/dc/a"), Fraction(4, 9)),
    )
    ro = freeze_model(model)

    # Caller mutating private CPython slots after freeze_model cannot leak into the proxy.
    orig_path._str = "/caller/corrupted"  # type: ignore[attr-defined]
    orig_win._str = "Z:/caller/corrupted"  # type: ignore[attr-defined]
    orig_frac._numerator = 999  # type: ignore[attr-defined]

    read_p1, read_w1, read_f1 = ro.path_val, ro.pure_win, ro.ratio
    assert isinstance(read_p1, Path) and isinstance(read_w1, PureWindowsPath)
    assert isinstance(read_f1, Fraction)
    assert str(read_p1) == "/orig/forcing.txt" and str(read_w1) == "C:\\orig\\win.txt"
    assert read_f1 == Fraction(1, 3)

    # Hostile rule mutating received copies cannot poison cached proxy state.
    read_p1._str = "/rule/corrupted"  # type: ignore[attr-defined]
    read_w1._str = "Z:/rule/corrupted"  # type: ignore[attr-defined]
    read_f1._numerator = 777  # type: ignore[attr-defined]

    assert ro.path_val is not read_p1 and str(ro.path_val) == "/orig/forcing.txt"
    assert ro.pure_win is not read_w1 and str(ro.pure_win) == "C:\\orig\\win.txt"
    assert ro.ratio is not read_f1 and ro.ratio == Fraction(1, 3)

    # Dataclass proxy, field metadata, unwrap_copy, and model_dump stay isolated too.
    ro.nested.p._str = "/dc/corrupted"
    ro.nested.f._numerator = 123
    assert str(ro.nested.p) == "/dc/a" and ro.nested.f == Fraction(4, 9)

    ro.model_fields["path_val"].default._str = "/mutated/default"
    ro.model_fields["path_val"].metadata[0]._numerator = 99
    assert str(ro.model_fields["path_val"].default) == "/default/path"
    assert ro.model_fields["path_val"].metadata[0] == Fraction(1, 4)

    unwrapped = ro.unwrap_copy()
    unwrapped.path_val._str = "/mutated/unwrap"  # type: ignore[attr-defined]
    unwrapped.ratio._numerator = 888  # type: ignore[attr-defined]
    assert str(ro.path_val) == "/orig/forcing.txt" and ro.ratio == Fraction(1, 3)

    class _DumpModel(BaseModel):
        model_config = ConfigDict(arbitrary_types_allowed=True)
        p: object

    ro_dump = freeze_model(_DumpModel(p=Path("/dump/path")))
    dumped = ro_dump.model_dump()
    dumped["p"]._str = "/mutated/dump"
    assert str(ro_dump.p) == "/dump/path"

    # UUID exposes the process-wide SafeUUID Enum singleton via .is_safe, so freezing rejects it.
    with pytest.raises(TypeError, match="UUID"):
        deep_freeze({"u": UUID(int=1)})


def test_r10_m1_copy_on_read_containers_enums_and_rejections() -> None:
    """R10-M1: Frozen mappings/tuples/sets/slices/Enums isolate Path/Fraction and reject unsafe carriers."""
    from dataclasses import field as dc_field  # noqa: PLC0415

    class _StdlibEnum(Enum):
        PATH = PurePosixPath("/enum/path")
        PAIR = (PurePosixPath("/enum/pair"), Fraction(3, 7))

    key_p = PurePosixPath("/key/one")
    tuple_key = (PurePosixPath("/key/tuple"), 1)
    val_p = Path("/val/a")
    frac = Fraction(3, 8)
    frozen = deep_freeze({
        "map": {key_p: val_p, tuple_key: frac},
        "seq": [val_p, frac],
        "bag": {key_p, frac},
        "win": slice(val_p, frac, None),
        "ep": _StdlibEnum.PATH,
        "epair": _StdlibEnum.PAIR,
    })

    key_p._str = "/caller/key"  # type: ignore[attr-defined]
    tuple_key[0]._str = "/caller/tkey"  # type: ignore[attr-defined]
    val_p._str = "/caller/val"  # type: ignore[attr-defined]
    frac._numerator = 999  # type: ignore[attr-defined]

    assert frozen["map"][PurePosixPath("/key/one")] == Path("/val/a")
    assert frozen["map"][(PurePosixPath("/key/tuple"), 1)] == Fraction(3, 8)
    for k in frozen["map"]:
        if isinstance(k, PurePosixPath):
            k._str = "/rule/key"  # type: ignore[attr-defined]
    assert PurePosixPath("/key/one") in frozen["map"]

    frozen["seq"][0]._str = "/rule/seq"
    assert str(frozen["seq"][0]) == "/val/a"
    assert hash(frozen["bag"]) == hash(
        frozenset({PurePosixPath("/key/one"), Fraction(3, 8)})
    )
    for item in frozen["bag"]:
        if isinstance(item, Fraction):
            item._numerator = 555  # type: ignore[attr-defined]
    assert Fraction(3, 8) in frozen["bag"]

    frozen["win"].start._str = "/rule/slice"
    frozen["win"].stop._numerator = 666
    assert str(frozen["win"].start) == "/val/a"
    assert frozen["win"].stop == Fraction(3, 8)

    frozen["ep"].value._str = "/rule/enum"
    assert str(frozen["ep"].value) == "/enum/path"
    assert frozen["ep"] == _StdlibEnum.PATH
    assert frozen["epair"] == _StdlibEnum.PAIR

    thawed = thaw_copy(frozen["map"])
    thawed[(PurePosixPath("/key/tuple"), 1)]._numerator = 222
    assert frozen["map"][(PurePosixPath("/key/tuple"), 1)] == Fraction(3, 8)

    # NumPy dtype.metadata rejects Fraction (deepcopy(dtype) aliases Fraction).
    dt_frac = np.dtype("float64", metadata={"r": Fraction(1, 2)})
    with pytest.raises(TypeError, match="metadata"):
        deep_freeze(np.zeros(2, dtype=dt_frac))

    # Slotted dataclass with class-backed Path default is rejected.
    @dataclass
    class _SlottedDefaultPath:
        __slots__ = ()
        p: PurePosixPath = dc_field(init=False, default=PurePosixPath("/cls/default"))

    with pytest.raises(TypeError, match="slotted dataclass"):
        ReadOnlyDataclassProxy(_SlottedDefaultPath())


def test_r10_m2_overridden_model_fields_set_and_model_extra_not_executed() -> None:
    """R10-M2: Overridden model_fields_set / model_extra properties on BaseModel are bypassed."""
    calls: list[str] = []

    class _AdversarialPropsModel(BaseModel):
        model_config = ConfigDict(extra="allow")
        x: int = 1

        @property
        def model_fields_set(self) -> set[str]:  # type: ignore[override]
            calls.append("model_fields_set")
            return {"spoofed"}

        @property
        def model_extra(self) -> dict[str, object]:  # type: ignore[override]
            calls.append("model_extra")
            return {"spoofed": 999}

    inst = _AdversarialPropsModel(x=7, extra_field=42)  # type: ignore[call-arg]
    ro = freeze_model(inst)
    assert calls == []
    assert ro.model_fields_set == frozenset({"x", "extra_field"})
    assert ro.extra_field == 42
    assert "extra_field=42" in repr(ro)
    assert ro == freeze_model(_AdversarialPropsModel(x=7, extra_field=42))  # type: ignore[call-arg]
    assert ro.model_dump() == {"x": 7, "extra_field": 42}
    unwrapped = ro.unwrap_copy()
    assert unwrapped.x == 7
    assert object.__getattribute__(unwrapped, "__pydantic_extra__") == {
        "extra_field": 42
    }
    assert calls == []


def test_r10_m1_pre_enriched_patch_write_metadata_parity() -> None:
    """R10-m1: Partially pre-enriched patches behave identically across direct apply and normalisation."""
    base = {"a": {"value": 1, "ref": {"ID": "r1"}}}
    pre_enriched = ConfigPatch(
        path="a",
        action="set",
        new_value=None,
        had_original=True,
        original_style="ref_only",
        original_ref={"ID": "r1"},
    )
    direct = deepcopy(base)
    apply_patches_in_place(direct, [pre_enriched])

    normalised = normalise_patches([pre_enriched], base)
    via_norm = deepcopy(base)
    apply_patches_in_place(via_norm, normalised)

    assert direct == via_norm == {"a": {"value": None, "ref": {"ID": "r1"}}}

    # Unnormalised patch (had_original=None) with original_style="ref_only" honours hint on both paths.
    hinted = ConfigPatch(
        path="a",
        action="set",
        new_value=None,
        original_style="ref_only",
        original_ref={"ID": "r1"},
    )
    direct_hinted = deepcopy(base)
    apply_patches_in_place(direct_hinted, [hinted])
    via_norm_hinted = deepcopy(base)
    apply_patches_in_place(via_norm_hinted, normalise_patches([hinted], base))
    assert direct_hinted == via_norm_hinted == {"a": {"ref": {"ID": "r1"}}}


def test_r10_m2_strict_equal_slice_with_numpy_array_bounds() -> None:
    """R10-m2: _strict_equal handles slice payloads carrying NumPy arrays without ValueError."""
    base_same = {"s": slice(np.array([1, 2]), None, 1)}
    noop = normalise_patches(
        [ConfigPatch("s", "set", slice(np.array([1, 2]), None, 1), replace_raw=True)],
        base_same,
    )
    assert noop == []

    changed = normalise_patches(
        [ConfigPatch("s", "set", slice(np.array([1, 3]), None, 1), replace_raw=True)],
        base_same,
    )
    assert len(changed) == 1

    # Signed-zero and NaN slice bounds stay distinct/equal strictly as both values and keys.
    sz_slice = normalise_patches(
        [ConfigPatch("s", "set", slice(-0.0, 1), replace_raw=True)],
        {"s": slice(0.0, 1)},
    )
    assert len(sz_slice) == 1
    nan_key_noop = normalise_patches(
        [ConfigPatch("m", "set", {slice(float("nan"), 1): 1}, replace_raw=True)],
        {"m": {slice(float("nan"), 1): 1}},
    )
    assert nan_key_noop == []


def test_r11_namedtuple_new_str_subclass_meta_nan_keys_tuple_nullify_and_ledger_keys() -> (
    None
):
    """Cover Astra R1 findings: caching NamedTuple, str subclass metadata, NaN keys, tuple nullify, ledger key_order."""
    from typing import NamedTuple  # noqa: PLC0415

    # 1. NamedTuple subclass with caching __new__ cannot return the caller's original instance.
    class _BaseNT(NamedTuple):
        items: list[int]

    cached: list[_BaseNT] = []

    class _CachingNT(_BaseNT):
        __slots__ = ()

        def __new__(cls, items: list[int]) -> _CachingNT:
            if cached:
                return cached[0]  # type: ignore[return-value]
            inst = tuple.__new__(cls, (items,))
            cached.append(inst)
            return inst

    class _NTModel(BaseModel):
        model_config = ConfigDict(arbitrary_types_allowed=True)
        nt: _CachingNT

    orig_list = [1, 2]
    ro_nt = freeze_model(_NTModel(nt=_CachingNT(orig_list)))
    orig_list.append(99)
    assert ro_nt.nt == ((1, 2),)
    assert ro_nt.unwrap_copy().nt.items == [1, 2]

    # 2. Field alias/title/description strip mutable str subclasses to exact str.
    class _TaggedStr(str):
        tags: list[str]

        def __new__(cls, text: str) -> _TaggedStr:
            obj = super().__new__(cls, text)
            obj.tags = ["orig"]
            return obj

    class _MetaStrModel(BaseModel):
        x: int = Field(
            default=1,
            alias=_TaggedStr("ax"),
            title=_TaggedStr("tx"),
            description=_TaggedStr("dx"),
        )

    ro_meta = freeze_model(_MetaStrModel())
    info = ro_meta.model_fields["x"]
    assert type(info.alias) is str and info.alias == "ax"
    assert type(info.title) is str and info.title == "tx"
    assert type(info.description) is str and info.description == "dx"

    # 3. Coalescing keeps writes to distinct NaN mapping keys separate.
    n1 = float("nan")
    n2 = float("nan")
    nan_base: dict[str, object] = {"p": {n1: 0, n2: 0}}
    nan_patches = [
        ConfigPatch(
            "p[n1]", "set", 1, default_wrapped=False, path_tokens=("p", MappingKey(n1))
        ),
        ConfigPatch(
            "p[n2]", "set", 2, default_wrapped=False, path_tokens=("p", MappingKey(n2))
        ),
    ]
    nan_norm = normalise_patches(nan_patches, nan_base)
    assert len(nan_norm) == 2
    nan_out = deepcopy(nan_base)
    apply_patches_in_place(nan_out, nan_norm)
    assert nan_out["p"] == {n1: 1, n2: 2}  # type: ignore[comparison-overlap]

    # 4. Expanded and direct nullify_tree both reject tuple-parent targets (including empty dicts).
    for tup_base in ({"a": ({"b": 1},)}, {"a": ({},)}):
        p_null = ConfigPatch("a[0]", "nullify_tree")
        with pytest.raises(ValueError, match="nullify_tree"):
            apply_patches_in_place(deepcopy(tup_base), [p_null])
        with pytest.raises(ValueError, match="nullify_tree"):
            normalise_patches([p_null], tup_base, expand_nullify_tree=True)

    # 5. StyleLedger / PathStyle deeply freeze key_order entries against Fraction private-slot mutation.
    k_frac = Fraction(1, 3)
    ledger = StyleLedger.from_mapping({k_frac: 1})
    k_frac._numerator = 999  # type: ignore[attr-defined]
    read_k = ledger.get("").key_order[0]
    assert read_k == Fraction(1, 3)
    read_k._numerator = 777
    assert ledger.get("").key_order[0] == Fraction(1, 3)
    ps = PathStyle(key_order=(Fraction(2, 5),))
    ps.key_order[0]._numerator = 888
    assert ps.key_order[0] == Fraction(2, 5)


def test_r12_namedtuple_iter_phase_b_fallback_re_pattern_and_enum_getters() -> None:
    """Cover Astra R2 findings: NamedTuple __iter__, Phase B fallback, re.Pattern subclass, Enum getters."""
    import re  # noqa: PLC0415
    from typing import NamedTuple  # noqa: PLC0415

    # 1. NamedTuple subclass with custom __iter__ cannot leak snapshot during freezing.
    leaked: list[object] = []

    class _BaseIterNT(NamedTuple):
        items: list[int]

    class _LeakyIterNT(_BaseIterNT):
        __slots__ = ()

        def __iter__(self) -> object:  # type: ignore[override]
            leaked.append(self)
            return tuple.__iter__(self)

    class _IterNTModel(BaseModel):
        model_config = ConfigDict(arbitrary_types_allowed=True)
        nt: object

    with pytest.raises(TypeError, match="Cannot safely detach"):
        freeze_model(_IterNTModel(nt=_LeakyIterNT([1, 2])))
    with pytest.raises(TypeError, match="custom __iter__"):
        deep_freeze(_LeakyIterNT([1, 2]))
    assert leaked == []

    # 2. Phase B fallback when freeze_model fails never mutates caller's model via model_dump.
    class _SideEffectConfig(SUEWSConfig):
        _audit: list[str] = PrivateAttr(default_factory=list)

        def model_dump(self, *args: object, **kwargs: object) -> dict[str, object]:
            self._audit.append("dumped")
            return super().model_dump(*args, **kwargs)  # type: ignore[arg-type,misc]

    cfg = _SideEffectConfig()
    object.__setattr__(cfg, "_callback", lambda: None)
    assert validate_config(cfg, pipeline="B", forcing="off") is not None
    assert cfg._audit == []

    # 3. re.Pattern carrying a mutable str subclass is rejected while plain patterns pass.
    class _TaggedPatternStr(str):
        pass

    tagged_text = _TaggedPatternStr("example")
    tagged_text.tags = []  # type: ignore[attr-defined]
    assert deep_freeze({"ok": re.compile("plain")})["ok"].pattern == "plain"
    with pytest.raises(TypeError, match="Pattern"):
        deep_freeze({"bad": re.compile(tagged_text)})

    class _PatternModel(BaseModel):
        model_config = ConfigDict(arbitrary_types_allowed=True)
        pat: object

    with pytest.raises(TypeError, match="Pattern"):
        freeze_model(_PatternModel(pat=re.compile(tagged_text)))

    # 4. Enum subclasses overriding .value / .name / __getattribute__ are rejected without invocation.
    enum_calls: list[str] = []

    class _ChangingEnum(Enum):
        ITEM = 1

        @property
        def value(self) -> int:
            enum_calls.append("value")
            self._value_ += 1
            return self._value_

    with pytest.raises(TypeError, match="overridden"):
        deep_freeze(_ChangingEnum.ITEM)
    assert enum_calls == []
    assert object.__getattribute__(_ChangingEnum.ITEM, "_value_") == 1

    class _ChangingNameEnum(Enum):
        ITEM = 1

        @property
        def name(self) -> str:
            enum_calls.append("name")
            return "SPOOFED"

    with pytest.raises(TypeError, match="overridden"):
        deep_freeze(_ChangingNameEnum.ITEM)
    assert enum_calls == []


def test_r13_identity_keys_prim_sub_slots_tz_mutable_enum_and_nan_ledger() -> None:
    """Cover Astra R3 findings: identity keys, str subclass hook, shadowed slot, timezone/ZoneInfo, late-mutated Enum, NaN ledger."""
    # 1. Identity-based object() keys round-trip losslessly; custom identity keys fail closed.
    obj_key = object()
    obj_data: dict[object, object] = {obj_key: 1}
    obj_patch = ConfigPatch(
        "key", "set", 2, path_tokens=(MappingKey(obj_key),), default_wrapped=False
    )
    obj_norm = normalise_patches([obj_patch], obj_data)
    assert obj_norm[0].had_original is True and obj_norm[0].old_value == 1
    apply_patches_in_place(obj_data, obj_norm)
    assert obj_data == {obj_key: 2}
    apply_patches_in_place(obj_data, invert_patches(obj_norm))
    assert obj_data == {obj_key: 1}

    class _CustomIdKey:
        pass

    with pytest.raises(ValueError, match="identity-based mapping key"):
        normalise_patches(
            [ConfigPatch("k", "set", 2, path_tokens=(MappingKey(_CustomIdKey()),))],
            {_CustomIdKey(): 1},
        )

    # 2. Primitive subclass cloning does not invoke overridden __str__ hooks.
    class _HookLabel(str):
        tags: list[str]

        def __str__(self) -> str:
            self.tags.append("called")
            return str.__str__(self)

    lbl = _HookLabel("x")
    lbl.tags = []

    class _AnyHolder(BaseModel):
        model_config = ConfigDict(arbitrary_types_allowed=True)
        x: object

    ro_lbl = freeze_model(_AnyHolder(x=lbl))
    assert lbl.tags == []
    assert ro_lbl.unwrap_copy().x == "x" and lbl.tags == []

    # 3. Shadowed inherited __slots__ raises before invoking property getters/setters.
    slot_calls: list[object] = []

    class _SlotParent:
        __slots__ = ("x",)

    class _SlotChild(_SlotParent):
        __slots__ = ()

        @property
        def x(self) -> int:
            slot_calls.append(self)
            return 1

    with pytest.raises(TypeError, match="shadowed"):
        freeze_model(_AnyHolder(x=_SlotChild()))
    assert slot_calls == []

    # 4. datetime.timezone and ZoneInfo carrying str/timedelta subclasses are rejected.
    class _SubName(str):
        pass

    class _SubDelta(datetime.timedelta):
        pass

    bad_name = _SubName("UTC")
    bad_name.tags = []  # type: ignore[attr-defined]
    bad_tz = datetime.timezone(datetime.timedelta(0), bad_name)
    bad_tz_off = datetime.timezone(_SubDelta(hours=1))
    bad_zi = ZoneInfo.no_cache(bad_name)
    for bad_obj in (
        bad_tz,
        bad_tz_off,
        bad_zi,
        datetime.datetime(2024, 1, 1, tzinfo=bad_tz),
        datetime.time(12, 0, tzinfo=bad_zi),
    ):
        with pytest.raises(TypeError):
            deep_freeze({"t": bad_obj})
        with pytest.raises(TypeError):
            freeze_model(_AnyHolder(x=bad_obj))

    # 5. Enum mutable at freeze_model time remains permanently rejected even if mutated immutable later.
    class _LateChoice(Enum):
        ITEM = 1

        def __init__(self, _value: int) -> None:
            self.tags: object = []

    ro_late = freeze_model(_AnyHolder(x=_LateChoice.ITEM))
    _LateChoice.ITEM.tags = ("after",)
    with pytest.raises(TypeError, match="freeze time"):
        _ = ro_late.x
    with pytest.raises(TypeError):
        ro_late.model_dump()

    # 6. Distinct NaN keys colliding in StyleLedger do not restore another key's ref metadata.
    n1, n2 = float("nan"), float("nan")
    doc_nan = ConfigDocument.from_dict(
        {
            "p": {
                n1: {"value": 1, "ref": {"ID": "first"}},
                n2: {"value": 2, "ref": {"ID": "second"}},
            }
        },
        normalise=False,
    )
    assert doc_nan.style_ledger.ref_metadata("p[nan]") is None
    updated_nan = doc_nan.apply_patches([
        ConfigPatch("p[n1]", "delete", path_tokens=("p", MappingKey(n1))),
        ConfigPatch("p[n1]", "set", 3, path_tokens=("p", MappingKey(n1))),
    ])
    assert updated_nan.data["p"][n1] == {"value": 3}


def test_r14_key_hash_dict_prop_tuple_sub_key_redeclared_slot_slotted_default_and_tzname() -> (
    None
):
    """Cover Astra R4 findings: key __hash__ hook, __dict__ property, tuple-sub keys, redeclared slots, slotted scalar default, tzname."""
    from dataclasses import field as dc_field  # noqa: PLC0415

    # 1. Mapping/set reconstruction rejects keys with unvetted __hash__ before invoking them.
    hash_captured: list[object] = []

    class _SpyHashKey:
        def __init__(self, payload: list[int]) -> None:
            self.payload = payload

        def __hash__(self) -> int:
            hash_captured.append(self)
            return 1

    class _Holder(BaseModel):
        model_config = ConfigDict(arbitrary_types_allowed=True)
        a: object
        b: object = None

    shared = [1]
    spy_key = _SpyHashKey(shared)
    h_dict = _Holder(a={spy_key: 1}, b=shared)
    h_tup = _Holder(a={(spy_key, 1): 1}, b=shared)
    h_set = _Holder(a={spy_key}, b=shared)
    hash_captured.clear()
    for holder in (h_dict, h_tup, h_set):
        with pytest.raises(TypeError, match="unvetted __hash__"):
            freeze_model(holder)
    assert hash_captured == []

    # 2. Shadowed __dict__ property raises without invoking the property or mutating caller state.
    dict_calls: list[str] = []

    @dataclass
    class _EvilDictDC:
        items: list[int]

        @property
        def __dict__(self) -> dict[str, object]:  # type: ignore[override]
            dict_calls.append("called")
            self.items.append(99)
            return {"items": self.items}

    orig_dc = _EvilDictDC([1])
    with pytest.raises(TypeError, match="__dict__ is shadowed"):
        ReadOnlyDataclassProxy(orig_dc)
    assert dict_calls == [] and orig_dc.items == [1]

    # 3. Identity-hashed tuple and frozenset subclasses as mapping keys fail closed.
    class _IdTuple(tuple):  # type: ignore[type-arg]
        __eq__ = object.__eq__
        __hash__ = object.__hash__

    class _IdFrozenSet(frozenset):  # type: ignore[type-arg]
        __eq__ = object.__eq__
        __hash__ = object.__hash__

    for bad_key in (_IdTuple((1, 2)), _IdFrozenSet((1, 2))):
        with pytest.raises(ValueError, match="identity-based mapping key"):
            normalise_patches(
                [
                    ConfigPatch(
                        "p[k]", "set", "new", path_tokens=("p", MappingKey(bad_key))
                    )
                ],
                {"p": {bad_key: "old"}},
            )

    # 4. Redeclared inherited __slots__ preserves both parent and child slot storage on unwrap_copy.
    class _SlotBase:
        __slots__ = ("x",)

    class _SlotDerived(_SlotBase):
        __slots__ = ("x",)

    sd = _SlotDerived()
    _SlotBase.x.__set__(sd, [10])
    _SlotDerived.x.__set__(sd, [20])
    unwrapped_sd = freeze_model(_Holder(a=sd)).unwrap_copy().a
    assert _SlotBase.x.__get__(unwrapped_sd, _SlotBase) == [10]
    assert _SlotDerived.x.__get__(unwrapped_sd, _SlotDerived) == [20]

    # 5. Slotted dataclass with scalar init=False class-backed default is rejected (no live class leak).
    @dataclass
    class _SlottedScalarDefault:
        __slots__ = ()
        x: int = dc_field(init=False, default=1)

    with pytest.raises(TypeError, match="class-backed default"):
        ReadOnlyDataclassProxy(_SlottedScalarDefault())

    # 6. No-op detection preserves datetime.timezone name changes (and hash-key distinctions).
    tz_a = datetime.timezone(datetime.timedelta(hours=1), "A")
    tz_b = datetime.timezone(datetime.timedelta(hours=1), "B")
    dt_a = datetime.datetime(2024, 1, 1, tzinfo=tz_a)
    dt_b = datetime.datetime(2024, 1, 1, tzinfo=tz_b)
    norm_tz = normalise_patches(
        [ConfigPatch("t", "set", dt_b, replace_raw=True)],
        {"t": dt_a},
    )
    assert len(norm_tz) == 1
    replayed_tz = {"t": dt_a}
    apply_patches_in_place(replayed_tz, norm_tz)
    assert replayed_tz["t"].tzname() == "B"
    norm_tz_key = normalise_patches(
        [ConfigPatch("m", "set", {dt_b: 1}, replace_raw=True)],
        {"m": {dt_a: 1}},
    )
    assert len(norm_tz_key) == 1


def test_r15_yaml_merge_override_frozen_reinit_and_numpy_metadata_isolation() -> None:
    """Cover Astra R5 M1, M2, M5: YAML merge override provenance, one-time init, and NumPy metadata hooks."""
    # M1: YAML merge + explicit override preserves winning ref metadata across delete + reinsert.
    for yaml_text in (
        "defaults: &d {x: {value: 1, ref: {ID: base}}, y: 10}\n"
        "section: {<<: *d, x: {value: 2, ref: {ID: override}}}\n",
        "defaults: &d {x: {value: 1, ref: {ID: base}}, y: 10}\n"
        "section: {x: {value: 2, ref: {ID: override}}, <<: *d}\n",
    ):
        doc = ConfigDocument.from_dict(
            yaml.safe_load(yaml_text), raw_text=yaml_text, normalise=False
        )
        assert doc.style_ledger.ref_metadata("section.x") == {"ID": "override"}
        updated = doc.apply_patches([
            ConfigPatch("section.x", "delete"),
            ConfigPatch("section.x", "set", 3),
        ])
        assert updated.data["section"]["x"] == {"value": 3, "ref": {"ID": "override"}}

    # M2: Frozen metadata, ledger, typed view, and patch objects forbid repeated __init__.
    class _MetaModel(BaseModel):
        x: int = 1

    ro_meta = freeze_model(_MetaModel())
    field_info = ro_meta.model_fields["x"]
    with pytest.raises(TypeError, match="cannot be re-initialised"):
        field_info.__init__("x", int, [99], False, None, None, None, None, (), None)
    assert field_info.default == 1
    with pytest.raises(TypeError, match="cannot be re-initialised"):
        ro_meta.model_type.__init__("Evil", "Evil", "evil")
    doc_reinit = ConfigDocument.from_dict({"a": {"value": 1, "ref": {"ID": "k"}}})
    clone_reinit = doc_reinit.clone()
    with pytest.raises(TypeError, match="cannot be re-initialised"):
        doc_reinit.style_ledger.__init__()
    assert clone_reinit.style_ledger.ref_metadata("a") == {"ID": "k"}
    path_style = doc_reinit.style_ledger.get("a")
    assert path_style is not None
    with pytest.raises(TypeError, match="cannot be re-initialised"):
        path_style.__init__()
    tview = doc_reinit.typed_view()
    with pytest.raises(TypeError, match="cannot be re-initialised"):
        tview.__init__(tview.raw, tview.physics, tview.sites)

    # M5: NumPy dtype metadata rejects custom dict subclasses before items() or __deepcopy__.
    meta_events: list[str] = []

    class _SpyDict(dict):  # type: ignore[type-arg]
        def items(self):  # type: ignore[override]
            meta_events.append("items")
            return super().items()

    class _StickyDict(dict):  # type: ignore[type-arg]
        def __deepcopy__(self, _memo: object) -> _StickyDict:
            return self

    for bad_meta in (_SpyDict({"k": 1}), _StickyDict({"k": 1})):
        bad_dt = np.dtype(np.int64, metadata={"nested": bad_meta})
        bad_arr = np.array([1, 2], dtype=bad_dt)
        with pytest.raises(TypeError, match="NumPy dtype"):
            deep_freeze(bad_arr)
    assert meta_events == []
    good_dt = np.dtype(np.int64, metadata={"nested": {"k": (1, 2)}})
    good_frozen = deep_freeze(np.array([1, 2], dtype=good_dt))
    assert good_frozen.dtype.metadata["nested"] == {"k": (1, 2)}


def test_r15_copy_hooks_dump_overrides_and_literal_repr() -> None:
    """Cover Astra R5 M3, M4, M6: copy hooks without __getattr__/property, dump shadowing, Literal repr."""
    from typing import Literal  # noqa: PLC0415

    # M3: Plain object and str subclass with custom __getattr__ / property copy hooks do not fire on freeze.
    getattr_events: list[str] = []

    class _SpyAttrObj:
        def __init__(self) -> None:
            self.items = [1]

        def __getattr__(self, name: str) -> object:
            getattr_events.append(name)
            raise AttributeError(name)

    class _PropCopyStr(str):
        pass

    _PropCopyStr.__deepcopy__ = property(  # type: ignore[assignment]
        lambda self: getattr_events.append("prop_deepcopy") or (lambda _m: self)
    )

    class _Holder(BaseModel):
        model_config = ConfigDict(arbitrary_types_allowed=True)
        a: object
        b: object = None

    ro_attr = freeze_model(_Holder(a=_SpyAttrObj()))
    assert ro_attr.unwrap_copy().a.items == [1]
    with pytest.raises(TypeError, match="opaque or custom copy semantics"):
        freeze_model(_Holder(a=_PropCopyStr("x")))
    assert getattr_events == []

    # M4: Instance-shadowed model_dump and __pydantic_serializer__ are blocked without executing hooks.
    dump_events: list[str] = []

    class _EvilCallable:
        def __init__(self, log: list[str]) -> None:
            self.log = log

        def __call__(self, *_args: object, **_kwargs: object) -> dict[str, int]:
            self.log.append("called")
            return {"evil": 1}

    m_dump = _Holder(a=1)
    object.__setattr__(m_dump, "model_dump", _EvilCallable(dump_events))
    with pytest.raises(TypeError, match="instance-shadowed dump"):
        freeze_model(m_dump).model_dump()
    m_ser = _Holder(a=1)
    object.__setattr__(m_ser, "__pydantic_serializer__", _EvilCallable(dump_events))
    with pytest.raises(TypeError, match="instance-shadowed dump"):
        freeze_model(m_ser).model_dump()
    assert dump_events == []

    # M6: Literal[Enum] annotation renders structurally without invoking Enum.__repr__.
    repr_events: list[str] = []

    class _ReprEnum(Enum):
        ITEM = 1

        def __repr__(self) -> str:
            repr_events.append("repr")
            return "EVIL"

    class _LitModel(BaseModel):
        mode: Literal[_ReprEnum.ITEM] = _ReprEnum.ITEM

    lit_inst = _LitModel()
    repr_events.clear()
    ro_lit = freeze_model(lit_inst)
    assert repr_events == []
    assert ro_lit.model_fields["mode"].annotation == "Literal[_ReprEnum.ITEM]"


def test_r15_exec_forged_dataclass_hooks_and_detached_attr_dict_keys() -> None:
    """Cover Astra R5 M7 & M8: exec()-forged dataclass/Pydantic hooks and detached str-subclass dict keys."""
    from pydantic import PrivateAttr  # noqa: PLC0415

    class _Holder(BaseModel):
        model_config = ConfigDict(arbitrary_types_allowed=True)
        a: object
        _priv: int = PrivateAttr(default=1)

    # M7: exec()-forged dataclass __hash__/__eq__ are rejected without executing; genuine frozen DC passes.
    exec_events: list[object] = []
    ns: dict[str, object] = {"exec_events": exec_events}
    exec(
        "def __hash__(self):\n    exec_events.append(self)\n    return hash((self.x,))\n"
        "def __eq__(self, other):\n    exec_events.append(self)\n    return self.x == other.x\n",
        ns,
    )

    @dataclass(frozen=True)
    class _ForgedHashDC:
        x: int

    _ForgedHashDC.__hash__ = ns["__hash__"]  # type: ignore[method-assign]
    h_forged = _Holder(a={_ForgedHashDC(1): "v"})
    exec_events.clear()
    with pytest.raises(TypeError, match="unvetted __hash__"):
        freeze_model(h_forged)

    @dataclass
    class _ForgedEqDC:
        x: int

    _ForgedEqDC.__eq__ = ns["__eq__"]  # type: ignore[method-assign]
    ro_feq = ReadOnlyDataclassProxy(_ForgedEqDC(1))
    with pytest.raises(TypeError, match="unvetted custom __eq__"):
        _ = ro_feq == _ForgedEqDC(1)
    assert exec_events == []

    @dataclass(frozen=True)
    class _GoodDC:
        x: int

    assert freeze_model(_Holder(a={_GoodDC(7): "ok"})).a[_GoodDC(7)] == "ok"

    # M8: str-subclass keys in model __dict__, __pydantic_private__, dataclass, and plain object are detached.
    class _TaggedStr(str):
        pass

    def _tagged(name: str) -> _TaggedStr:
        k = _TaggedStr(name)
        k.tags = ["orig"]  # type: ignore[attr-defined]
        return k

    tk_model, tk_priv, tk_dc = _tagged("a"), _tagged("_priv"), _tagged("x")
    m_key = _Holder(a=1)
    m_key.__dict__.pop("a")
    m_key.__dict__[tk_model] = 1
    m_key.__pydantic_private__.pop("_priv")  # type: ignore[union-attr]
    m_key.__pydantic_private__[tk_priv] = 2  # type: ignore[index]
    ro_key = freeze_model(m_key)
    tk_model.tags.append("mut")  # type: ignore[attr-defined]
    tk_priv.tags.append("mut")  # type: ignore[attr-defined]
    unwrapped_m = ro_key.unwrap_copy()
    uk_m = next(iter(unwrapped_m.__dict__.keys()))
    uk_p = next(iter(unwrapped_m.__pydantic_private__.keys()))  # type: ignore[union-attr]
    assert uk_m is not tk_model and uk_m.tags == ["orig"]  # type: ignore[attr-defined]
    assert uk_p is not tk_priv and uk_p.tags == ["orig"]  # type: ignore[attr-defined]
    dc_inst = _GoodDC(1)
    dc_inst.__dict__.pop("x")
    dc_inst.__dict__[tk_dc] = 1
    ro_dc_k = ReadOnlyDataclassProxy(dc_inst)
    tk_dc.tags.append("mut")  # type: ignore[attr-defined]
    uk_dc = next(iter(ro_dc_k.unwrap_copy().__dict__.keys()))
    assert uk_dc is not tk_dc and uk_dc.tags == ["orig"]  # type: ignore[attr-defined]


def test_r16_numpy_dtype_titles_and_non_function_serializers() -> None:
    """Cover Astra R6 M1 & M2: structured NumPy dtype titles and to-string/format serializers."""
    from pydantic_core import core_schema  # noqa: PLC0415

    # M1: Structured dtype titles isolate dict mutations in both directions and reject unsafe titles.
    orig_title = {"k": 1}
    dt_dict_title = np.dtype({
        "names": ["x"],
        "formats": ["i4"],
        "titles": [orig_title],
    })
    arr_title = np.array([(5,)], dtype=dt_dict_title)

    class _ArrHolder(BaseModel):
        model_config = ConfigDict(arbitrary_types_allowed=True)
        arr: np.ndarray

    ro_title = freeze_model(_ArrHolder(arr=arr_title))
    frozen_map = deep_freeze({"a": arr_title})
    orig_title["k"] = 999
    assert ro_title.arr.dtype.fields["x"][2] == {"k": 1}
    assert frozen_map["a"].dtype.fields["x"][2] == {"k": 1}
    with pytest.raises(TypeError):
        ro_title.arr.dtype.fields["x"][2]["k"] = 777
    assert ro_title.arr.dtype.fields["x"][2] == {"k": 1}
    assert orig_title == {"k": 999}

    title_events: list[str] = []

    class _SpyTitleDict(dict):  # type: ignore[type-arg]
        def items(self):  # type: ignore[override]
            title_events.append("items")
            return super().items()

    class _TitleEnum(Enum):
        TAG = 1

    for bad_title in (["mutable"], _TitleEnum.TAG, _SpyTitleDict({"a": 1})):
        bad_dt = np.dtype({"names": ["x"], "formats": ["i4"], "titles": [bad_title]})
        with pytest.raises(TypeError, match="mutable or Enum metadata"):
            deep_freeze(np.array([(1,)], dtype=bad_dt))
    assert title_events == []

    # M2: to-string and format serializers are rejected before invoking __str__ or __format__.
    ser_events: list[str] = []

    class _StrPayload:
        def __init__(self) -> None:
            self.tag = "orig"

        def __str__(self) -> str:
            ser_events.append("str")
            return "evil"

        @classmethod
        def __get_pydantic_core_schema__(
            cls, _src: object, _handler: object
        ) -> core_schema.CoreSchema:
            return core_schema.is_instance_schema(
                cls, serialization=core_schema.to_string_ser_schema()
            )

    class _FmtPayload:
        def __init__(self) -> None:
            self.tag = "orig"

        def __format__(self, _spec: str) -> str:
            ser_events.append("format")
            return "evil"

        @classmethod
        def __get_pydantic_core_schema__(
            cls, _src: object, _handler: object
        ) -> core_schema.CoreSchema:
            return core_schema.is_instance_schema(
                cls, serialization=core_schema.format_ser_schema("")
            )

    class _ToStrModel(BaseModel):
        p: _StrPayload

    class _FmtModel(BaseModel):
        p: _FmtPayload

    with pytest.raises(TypeError, match="unvetted custom serializer"):
        freeze_model(_ToStrModel(p=_StrPayload())).model_dump(mode="json")
    with pytest.raises(TypeError, match="unvetted custom serializer"):
        freeze_model(_FmtModel(p=_FmtPayload())).model_dump(mode="json")
    assert ser_events == []
