import pytest
from pydantic import ValidationError

from supy.data_model.core.model import KdownSplitMethod, ModelPhysics


def _method_code(value):
    while hasattr(value, "value"):
        value = value.value
    return int(value)


def test_kdown_split_method_defaults_to_epw():
    physics = ModelPhysics()

    assert _method_code(physics.kdown_split_method) == KdownSplitMethod.EPW.value


@pytest.mark.parametrize("method", list(KdownSplitMethod))
def test_kdown_split_method_roundtrips_through_df_state(method):
    physics = ModelPhysics(kdown_split_method=method)
    state = physics.to_df_state(grid_id=1)

    restored = ModelPhysics.from_df_state(state, grid_id=1)

    assert state.loc[1, ("kdown_split_method", "0")] == method.value
    assert _method_code(restored.kdown_split_method) == method.value


def test_legacy_df_state_defaults_to_epw():
    state = ModelPhysics().to_df_state(grid_id=1).drop(
        columns=[("kdown_split_method", "0")]
    )

    restored = ModelPhysics.from_df_state(state, grid_id=1)

    assert _method_code(restored.kdown_split_method) == KdownSplitMethod.EPW.value


@pytest.mark.parametrize("value", [0, 4])
def test_kdown_split_method_rejects_unknown_values(value):
    with pytest.raises(ValidationError):
        ModelPhysics(kdown_split_method=value)
