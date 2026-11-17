"""The data model emits every value the Rust bridge would otherwise default.

gh#1991: the bridge is a pass-through and must not supply values of its own.
Each key below used to reach the kernel only through a bridge fallback
(``unwrap_or`` or a zero-initialised struct) because the supy dump omitted
it. The data model now carries a documented default for each, under the
exact path the bridge reads, with the value the bridge used to substitute,
so the kernel input is unchanged.
"""

from importlib.resources import files

from pydantic import ValidationError
import pandas as pd
import pytest
import yaml

from supy.data_model.core import SUEWSConfig
from supy.data_model.core.hydro import StorageDrainParams
from supy.data_model.core.model import ModelControl, ModelPhysics
from supy.data_model.core.surface import VerticalLayers

pytestmark = [pytest.mark.api, pytest.mark.cfg]


def _sample_dump():
    """Dump the sample configuration exactly as supy hands it to the bridge."""
    data = yaml.safe_load(
        files("supy")
        .joinpath("sample_data/sample_config.yml")
        .read_text(encoding="utf-8")
    )
    return SUEWSConfig(**data).model_dump(exclude_none=True, mode="json")


def _unwrap(value):
    return value["value"] if isinstance(value, dict) else value


@pytest.mark.parametrize(
    "store_max", [0.0, 2.0, {"value": 2.0, "ref": {"desc": "Maximum source"}}]
)
@pytest.mark.parametrize(
    "absent_cap", [{}, {"store_cap": None}, {"store_cap": {"value": None}}]
)
def test_storage_capacity_default_and_state_roundtrip(store_max, absent_cap):
    """Resolve absent capacities without borrowing the maximum's reference."""
    params = StorageDrainParams(store_max=store_max, **absent_cap)
    expected = _unwrap(store_max)
    assert params.store_cap.value == pytest.approx(expected)
    assert params.store_cap.ref is None
    df_state = params.to_df_state(grid_id=1, surf_idx=0)
    assert df_state.loc[1, ("storedrainprm", "(5, 0)")] == pytest.approx(expected)
    restored = StorageDrainParams.from_df_state(df_state, grid_id=1, surf_idx=0)
    pd.testing.assert_frame_equal(restored.to_df_state(1, 0), df_state)


@pytest.mark.parametrize("capacity", [0.0, 0.7])
def test_explicit_storage_capacity_keeps_its_reference(capacity):
    params = StorageDrainParams(
        store_max=2.0,
        store_cap={"value": capacity, "ref": {"desc": "Capacity source"}},
    )
    assert params.store_cap.value == pytest.approx(capacity)
    assert params.store_cap.ref.desc == "Capacity source"
    df_state = params.to_df_state(1, 0)
    restored = StorageDrainParams.from_df_state(df_state, 1, 0)
    assert restored.store_cap.value == pytest.approx(capacity)
    pd.testing.assert_frame_equal(restored.to_df_state(1, 0), df_state)


@pytest.mark.parametrize(
    "absent_max", [{}, {"store_max": None}, {"store_max": {"value": None}}]
)
def test_storage_capacity_stays_absent_without_maximum(absent_max):
    assert StorageDrainParams(**absent_max).store_cap is None
    params = StorageDrainParams(store_cap={"value": None}, **absent_max)
    assert params.store_cap.value is None


def test_bridge_yaml_contains_resolved_storage_capacity():
    data = _sample_dump()
    storage = data["sites"][0]["properties"]["land_cover"]["paved"]["storage_drain_params"]
    storage.pop("store_cap", None)
    storage["store_max"] = 2.0
    config = SUEWSConfig(**data)
    emitted = yaml.safe_load(
        yaml.safe_dump(config.model_dump(exclude_none=True, mode="json"))
    )
    storage = emitted["sites"][0]["properties"]["land_cover"]["paved"]["storage_drain_params"]
    assert _unwrap(storage["store_cap"]) == pytest.approx(2.0)


@pytest.mark.parametrize(
    ("path", "expected"),
    [
        (("model", "control", "diag_qs"), 0),
        (("model", "control", "flag_test"), 0),
        (("model", "physics", "evapmethod"), 2),
        (("sites", 0, "properties", "vertical_layers", "ndepth"), 5),
    ],
)
def test_dump_carries_former_bridge_default(path, expected):
    """Each key is present in the dump with the value the bridge substituted."""
    node = _sample_dump()
    for key in path:
        node = node[key]
    assert _unwrap(node) == expected
    # The bridge reads these with an integer reader: a boolean would be
    # skipped and the bridge's own value used instead.
    assert type(_unwrap(node)) is int


@pytest.mark.parametrize(
    ("model_cls", "field", "bad"),
    [
        (ModelControl, "diag_qs", 2),
        (ModelControl, "flag_test", -1),
        (ModelPhysics, "evapmethod", 3),
        (VerticalLayers, "ndepth", 4),
    ],
)
def test_out_of_range_value_is_rejected(model_cls, field, bad):
    with pytest.raises(ValidationError):
        model_cls(**{field: bad})


def test_internal_switches_stay_out_of_user_export(tmp_path):
    """The new keys are internal: a clean export does not show them to users."""
    data = yaml.safe_load(
        files("supy")
        .joinpath("sample_data/sample_config.yml")
        .read_text(encoding="utf-8")
    )
    path_out = tmp_path / "clean.yml"
    SUEWSConfig(**data).to_yaml(str(path_out), include_internal=False)
    exported = yaml.safe_load(path_out.read_text(encoding="utf-8"))
    assert "diag_qs" not in exported["model"]["control"]
    assert "flag_test" not in exported["model"]["control"]
    assert "evapmethod" not in exported["model"]["physics"]
    assert "ndepth" not in exported["sites"][0]["properties"]["vertical_layers"]


def test_derived_store_cap_follows_an_updated_store_max():
    """The resolved store_cap is not user input, so a partial update re-derives it.

    SUEWSSimulation.update_config merges a patch onto
    ``model_dump(exclude_unset=True)``; a store_cap pinned there would keep
    the old store_max after the user changes store_max.
    """
    params = StorageDrainParams(store_max={"value": 1.5})
    assert params.store_cap.value == pytest.approx(1.5)
    assert "store_cap" not in params.model_fields_set
    base = params.model_dump(exclude_unset=True, round_trip=True, mode="json")
    assert "store_cap" not in base
    base["store_max"] = {"value": 4.0}
    assert StorageDrainParams(**base).store_cap.value == pytest.approx(4.0)


def test_explicit_store_cap_stays_user_input():
    params = StorageDrainParams(store_max={"value": 1.5}, store_cap={"value": 0.7})
    assert "store_cap" in params.model_fields_set
    base = params.model_dump(exclude_unset=True, round_trip=True, mode="json")
    base["store_max"] = {"value": 4.0}
    assert StorageDrainParams(**base).store_cap.value == pytest.approx(0.7)
