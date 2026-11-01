"""BEERS selection through the public output saver (gh#1974)."""

import numpy as np
import pandas as pd
import pytest
import yaml

from supy.data_model.core import SUEWSConfig
from supy.data_model.output import (
    OUTPUT_GROUP_SCOPES,
    OUTPUT_REGISTRY,
    PROVISIONAL_OUTPUT_GROUPS,
    OutputContractScope,
)
from supy.suews_output import SUEWSOutput

pytestmark = pytest.mark.api


@pytest.fixture
def output_frames():
    """One day at 30 minutes, with known hourly BEERS means."""
    index = pd.MultiIndex.from_product(
        [[1], pd.date_range("2020-06-01 00:30", periods=48, freq="30min")],
        names=["grid", "datetime"],
    )
    df_registry = OUTPUT_REGISTRY.to_dataframe()
    columns = pd.MultiIndex.from_tuples(
        [
            (group, var)
            for group in ["SUEWS", "DailyState", "BEERS"]
            for var in df_registry.loc[group].index
        ],
        names=["group", "var"],
    )
    df_output = pd.DataFrame(np.nan, index=index, columns=columns)
    df_output["SUEWS"] = 100.0
    df_output.loc[index[-1], "DailyState"] = 2.0
    df_output["BEERS"] = np.repeat(
        np.arange(48, dtype=float)[:, None], len(df_registry.loc["BEERS"]), axis=1
    )
    df_state = pd.DataFrame(
        [[0]],
        index=pd.Index([1], name="grid"),
        columns=pd.MultiIndex.from_tuples([("snowuse", "0")], names=["var", "ind_dim"]),
    )
    return df_output, df_state


def _assert_txt_files(paths, expected_groups):
    assert len(paths) == len(expected_groups)
    assert {path.name for path in paths} == {
        "1_2020_DailyState.txt" if group == "DailyState" else f"1_2020_{group}_60.txt"
        for group in expected_groups
    }
    for path in paths:
        df_saved = pd.read_csv(path, sep=r"\s+")
        assert not df_saved.empty
        if "_BEERS_" in path.name:
            assert set(OUTPUT_REGISTRY.to_dataframe().loc["BEERS"].index) <= set(
                df_saved.columns
            )
            # Each hourly row is the mean of two consecutive half-hours.
            np.testing.assert_array_equal(df_saved["Tmrt"], np.arange(24) * 2 + 0.5)


@pytest.mark.parametrize("with_config", [False, True])
def test_default_txt_groups_unchanged(output_frames, tmp_path, with_config):
    config = SUEWSConfig() if with_config else None
    output = SUEWSOutput(*output_frames, config=config)
    _assert_txt_files(output.save(tmp_path), ["SUEWS", "DailyState"])


@pytest.mark.parametrize("with_config", [False, True])
@pytest.mark.parametrize(
    ("groups", "provisional"),
    [
        (["BEERS"], True),
        (["SUEWS", "DailyState", "BEERS"], True),
        (["SUEWS", "DailyState"], False),
        ([], False),
    ],
)
def test_explicit_txt_groups_override_config(
    output_frames, tmp_path, with_config, groups, provisional
):
    config = (
        SUEWSConfig(model={"control": {"output": {"groups": ["SUEWS"]}}})
        if with_config
        else None
    )
    output = SUEWSOutput(*output_frames, config=config)
    if provisional:
        with pytest.warns(UserWarning, match="provisional output group"):
            paths = output.save(
                tmp_path / "selected", groups=groups, include_provisional=True
            )
    else:
        paths = output.save(tmp_path / "selected", groups=groups)
    _assert_txt_files(paths, groups)
    # An override is local to this save; neither config nor held data is filtered.
    assert output.groups == ["SUEWS", "DailyState", "BEERS"]
    _assert_txt_files(
        output.save(tmp_path / "again"),
        ["SUEWS"] if with_config else ["SUEWS", "DailyState"],
    )


@pytest.mark.parametrize("with_config", [False, True])
def test_provisional_group_needs_opt_in(output_frames, tmp_path, with_config):
    config = SUEWSConfig() if with_config else None
    output = SUEWSOutput(*output_frames, config=config)
    with pytest.raises(ValueError, match="BEERS are provisional"):
        output.save(tmp_path, groups=["SUEWS", "BEERS"])
    assert not list(tmp_path.iterdir())


def test_explicit_false_overrides_configured_opt_in(output_frames, tmp_path):
    config = SUEWSConfig(model={"control": {"output": {"include_provisional": True}}})
    output = SUEWSOutput(*output_frames, config=config)
    with pytest.raises(ValueError, match="include_provisional"):
        output.save(tmp_path, groups=["BEERS"], include_provisional=False)


def _config_with_groups(source, tmp_path, dict_output):
    dict_config = {"model": {"control": {"output": dict_output}}}
    if source == "yaml":
        path_config = tmp_path / "config.yml"
        path_config.write_text(yaml.safe_dump(dict_config), encoding="utf-8")
        return SUEWSConfig.from_yaml(str(path_config))
    if source == "dict":
        return SUEWSConfig.from_dict(dict_config)
    return SUEWSConfig(**dict_config)


@pytest.mark.parametrize("source", ["yaml", "dict", "object"])
def test_config_selects_beers_txt_with_opt_in(output_frames, tmp_path, source):
    config = _config_with_groups(
        source, tmp_path, {"groups": ["BEERS"], "include_provisional": True}
    )
    output = SUEWSOutput(*output_frames, config=config)
    with pytest.warns(UserWarning, match="provisional output group"):
        paths = output.save(tmp_path / "saved")
    _assert_txt_files(paths, ["BEERS"])


@pytest.mark.parametrize("source", ["yaml", "dict", "object"])
def test_config_provisional_group_warns_then_refuses(output_frames, tmp_path, source):
    with pytest.warns(UserWarning, match="names provisional group"):
        config = _config_with_groups(source, tmp_path, {"groups": ["BEERS"]})
    output = SUEWSOutput(*output_frames, config=config)
    with pytest.raises(ValueError, match="BEERS are provisional"):
        output.save(tmp_path / "saved")
    # The save-time opt-in overrides the configured default.
    with pytest.warns(UserWarning, match="provisional output group"):
        paths = output.save(tmp_path / "saved", include_provisional=True)
    _assert_txt_files(paths, ["BEERS"])


def test_provisional_groups_follow_output_contract():
    assert PROVISIONAL_OUTPUT_GROUPS == {
        str(group.value)
        for group, scope in OUTPUT_GROUP_SCOPES.items()
        if scope is OutputContractScope.PROVISIONAL
    }
    assert "BEERS" in PROVISIONAL_OUTPUT_GROUPS
    assert not PROVISIONAL_OUTPUT_GROUPS & {"SUEWS", "DailyState"}


def test_config_still_rejects_unknown_groups():
    with pytest.raises(ValueError, match="Invalid output groups"):
        SUEWSConfig.from_dict({
            "model": {"control": {"output": {"groups": ["not_a_group"]}}}
        })


@pytest.mark.parametrize("groups", [None, ["BEERS"], []])
def test_parquet_keeps_all_groups(output_frames, tmp_path, groups):
    config = SUEWSConfig(
        model={"control": {"output": {"format": "parquet", "groups": ["SUEWS"]}}}
    )
    output = SUEWSOutput(*output_frames, config=config)
    with pytest.warns(
        FutureWarning, match="Non-streaming Parquet output is deprecated"
    ):
        paths = output.save(tmp_path, groups=groups)
    assert {path.name for path in paths} == {
        "SUEWS_output.parquet",
        "SUEWS_metadata.parquet",
    }
    df_saved = pd.read_parquet(tmp_path / "SUEWS_output.parquet")
    assert df_saved.columns.equals(output_frames[0].columns)
    np.testing.assert_array_equal(df_saved["BEERS", "Tmrt"], np.arange(24) * 2 + 0.5)
    np.testing.assert_array_equal(df_saved["SUEWS", "QN"], np.full(24, 100.0))
