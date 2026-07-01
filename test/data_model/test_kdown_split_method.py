import pytest
from pydantic import ValidationError

from supy.data_model.core.config import SUEWSConfig
from supy.data_model.core.model import KdownSplitMethod, ModelPhysics

pytestmark = pytest.mark.api


def _method_code(value):
    while hasattr(value, "value"):
        value = value.value
    return int(value)


def _scalar(value):
    while hasattr(value, "value"):
        value = value.value
    return value


def test_kdown_split_method_defaults_to_epw():
    physics = ModelPhysics()

    assert _method_code(physics.kdown_split_method) == KdownSplitMethod.EPW.value
    assert _scalar(physics.sw_dn_direct_frac) == 0.5


@pytest.mark.parametrize("method", list(KdownSplitMethod))
def test_kdown_split_method_roundtrips_through_df_state(method):
    physics = ModelPhysics(kdown_split_method=method)
    state = physics.to_df_state(grid_id=1)

    restored = ModelPhysics.from_df_state(state, grid_id=1)

    assert state.loc[1, ("kdown_split_method", "0")] == method.value
    assert _method_code(restored.kdown_split_method) == method.value
    assert state.loc[1, ("sw_dn_direct_frac", "0")] == 0.5
    assert _scalar(restored.sw_dn_direct_frac) == 0.5


@pytest.mark.parametrize(
    ("name", "method"),
    [
        ("forcing", KdownSplitMethod.FORCING),
        ("constant", KdownSplitMethod.CONSTANT),
        ("epw", KdownSplitMethod.EPW),
    ],
)
def test_kdown_split_method_accepts_readable_names(name, method):
    physics = ModelPhysics(kdown_split_method=name)

    assert _method_code(physics.kdown_split_method) == method.value


def test_kdown_split_constant_owns_direct_fraction():
    physics = ModelPhysics(
        kdown_split_method={"constant": {"sw_dn_direct_frac": 0.42}}
    )
    state = physics.to_df_state(grid_id=1)

    assert _method_code(physics.kdown_split_method) == KdownSplitMethod.CONSTANT.value
    assert _scalar(physics.sw_dn_direct_frac) == 0.42
    assert state.loc[1, ("kdown_split_method", "0")] == KdownSplitMethod.CONSTANT.value
    assert state.loc[1, ("sw_dn_direct_frac", "0")] == 0.42


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


def test_config_migrates_legacy_spartacus_fraction_to_model_physics():
    cfg = SUEWSConfig.model_validate(
        {
            "model": {"physics": {"kdown_split_method": "constant"}},
            "sites": [
                {
                    "gridiv": 1,
                    "properties": {
                        "spartacus": {"sw_dn_direct_frac": {"value": 0.33}}
                    },
                }
            ],
        }
    )
    state = cfg.to_df_state()

    assert _scalar(cfg.model.physics.sw_dn_direct_frac) == 0.33
    assert _scalar(cfg.sites[0].properties.spartacus.sw_dn_direct_frac) == 0.33
    assert state.loc[1, ("sw_dn_direct_frac", "0")] == 0.33


def test_config_migrates_matching_legacy_spartacus_fractions():
    cfg = SUEWSConfig.model_validate(
        {
            "model": {"physics": {"kdown_split_method": "constant"}},
            "sites": [
                {
                    "gridiv": 1,
                    "properties": {"spartacus": {"sw_dn_direct_frac": 0.33}},
                },
                {
                    "gridiv": 2,
                    "properties": {
                        "spartacus": {"sw_dn_direct_frac": {"value": 0.33}}
                    },
                },
            ],
        }
    )
    state = cfg.to_df_state()

    assert _scalar(cfg.model.physics.sw_dn_direct_frac) == 0.33
    assert [
        _scalar(site.properties.spartacus.sw_dn_direct_frac)
        for site in cfg.sites
    ] == [0.33, 0.33]
    assert state.loc[1, ("sw_dn_direct_frac", "0")] == 0.33
    assert state.loc[2, ("sw_dn_direct_frac", "0")] == 0.33


def test_config_rejects_mixed_legacy_spartacus_fractions():
    with pytest.raises(ValidationError, match="multiple distinct values"):
        SUEWSConfig.model_validate(
            {
                "model": {"physics": {"kdown_split_method": "constant"}},
                "sites": [
                    {
                        "gridiv": 1,
                        "properties": {
                            "spartacus": {"sw_dn_direct_frac": 0.2}
                        },
                    },
                    {
                        "gridiv": 2,
                        "properties": {
                            "spartacus": {"sw_dn_direct_frac": 0.8}
                        },
                    },
                ],
            }
        )


def test_model_owned_kdown_direct_fraction_overrides_spartacus_slot():
    cfg = SUEWSConfig.model_validate(
        {
            "model": {
                "physics": {
                    "kdown_split_method": {
                        "constant": {"sw_dn_direct_frac": 0.61}
                    }
                }
            },
            "sites": [
                {
                    "gridiv": 1,
                    "properties": {"spartacus": {"sw_dn_direct_frac": 0.2}},
                }
            ],
        }
    )

    assert _scalar(cfg.model.physics.sw_dn_direct_frac) == 0.61
    assert _scalar(cfg.sites[0].properties.spartacus.sw_dn_direct_frac) == 0.61
    assert cfg.to_df_state().loc[1, ("sw_dn_direct_frac", "0")] == 0.61
