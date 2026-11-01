"""BEERS selection through the public output saver (gh#1974)."""

import numpy as np
import pandas as pd
import pytest
import yaml

from supy.data_model.core import SUEWSConfig
from supy.data_model.output import OUTPUT_REGISTRY
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
@pytest.mark.parametrize("groups", [["BEERS"], ["SUEWS", "DailyState", "BEERS"], []])
def test_explicit_txt_groups_override_config(
    output_frames, tmp_path, with_config, groups
):
    config = (
        SUEWSConfig(model={"control": {"output": {"groups": ["SUEWS"]}}})
        if with_config
        else None
    )
    output = SUEWSOutput(*output_frames, config=config)
    _assert_txt_files(output.save(tmp_path / "selected", groups=groups), groups)
    # An override is local to this save; neither config nor held data is filtered.
    assert output.groups == ["SUEWS", "DailyState", "BEERS"]
    _assert_txt_files(
        output.save(tmp_path / "again"),
        ["SUEWS"] if with_config else ["SUEWS", "DailyState"],
    )


@pytest.mark.parametrize("source", ["yaml", "dict", "object"])
def test_config_selects_beers_txt(output_frames, tmp_path, source):
    dict_config = {"model": {"control": {"output": {"groups": ["BEERS"]}}}}
    if source == "yaml":
        path_config = tmp_path / "config.yml"
        path_config.write_text(yaml.safe_dump(dict_config), encoding="utf-8")
        config = SUEWSConfig.from_yaml(str(path_config))
    elif source == "dict":
        config = SUEWSConfig.from_dict(dict_config)
    else:
        config = SUEWSConfig(**dict_config)
    output = SUEWSOutput(*output_frames, config=config)
    _assert_txt_files(output.save(tmp_path / "saved"), ["BEERS"])


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
