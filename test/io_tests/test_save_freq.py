"""Output frequency precedence in ``SUEWSOutput.save``.

An explicit ``save(freq_s=...)`` must win over ``model.control.output.freq``,
as ``groups=`` does; without it the configured frequency applies, then hourly.
"""

import numpy as np
import pandas as pd
import pytest

from supy.data_model.core import SUEWSConfig
from supy.data_model.output import OUTPUT_REGISTRY
from supy.suews_output import SUEWSOutput

pytestmark = pytest.mark.api


@pytest.fixture
def output_frames():
    """One day of SUEWS and DailyState output at 30 minutes."""
    index = pd.MultiIndex.from_product(
        [[1], pd.date_range("2020-06-01 00:30", periods=48, freq="30min")],
        names=["grid", "datetime"],
    )
    df_registry = OUTPUT_REGISTRY.to_dataframe()
    columns = pd.MultiIndex.from_tuples(
        [
            (group, var)
            for group in ["SUEWS", "DailyState"]
            for var in df_registry.loc[group].index
        ],
        names=["group", "var"],
    )
    df_output = pd.DataFrame(np.nan, index=index, columns=columns)
    df_output["SUEWS"] = 100.0
    df_output.loc[index[-1], "DailyState"] = 2.0
    df_state = pd.DataFrame(
        [[0]],
        index=pd.Index([1], name="grid"),
        columns=pd.MultiIndex.from_tuples([("snowuse", "0")], names=["var", "ind_dim"]),
    )
    return df_output, df_state


def _suews_file(paths, minutes):
    """Return the one SUEWS TXT file, checking its frequency suffix."""
    suews = [path for path in paths if "_SUEWS_" in path.name]
    assert [path.name for path in suews] == [f"1_2020_SUEWS_{minutes}.txt"]
    return suews[0]


def _n_rows(path):
    return len(pd.read_csv(path, sep=r"\s+"))


@pytest.mark.parametrize(
    ("config_freq", "freq_s", "minutes", "n_rows"),
    [
        # An explicit argument overrides the configured frequency, both finer
        # and coarser than it.
        (3600, 1800, 30, 48),
        (1800, 7200, 120, 12),
        # Without one, the configured frequency applies.
        (3600, None, 60, 24),
        (1800, None, 30, 48),
        # Without either, output is hourly; an explicit value still applies.
        (None, None, 60, 24),
        (None, 7200, 120, 12),
    ],
)
def test_save_freq_precedence(
    output_frames, tmp_path, config_freq, freq_s, minutes, n_rows
):
    # ARRANGE
    config = (
        SUEWSConfig(model={"control": {"output": {"freq": config_freq}}})
        if config_freq is not None
        else None
    )
    output = SUEWSOutput(*output_frames, config=config)

    # ACT
    paths = output.save(tmp_path, freq_s=freq_s)

    # ASSERT: the file is named for, and resampled to, the resolved frequency.
    assert _n_rows(_suews_file(paths, minutes)) == n_rows


def test_explicit_freq_is_local_to_one_save(output_frames, tmp_path):
    config = SUEWSConfig(model={"control": {"output": {"freq": 3600}}})
    output = SUEWSOutput(*output_frames, config=config)

    output.save(tmp_path / "explicit", freq_s=1800)
    paths = output.save(tmp_path / "configured")

    assert config.model.control.output.freq == 3600
    assert _n_rows(_suews_file(paths, 60)) == 24
