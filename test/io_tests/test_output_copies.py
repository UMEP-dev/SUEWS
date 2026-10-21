"""Output copies stay independent after the gh#1849 memory changes.

The run output is no longer deep-copied on the way into ``SUEWSOutput``, on
entry to the text and parquet savers, or by an unconditional ``sort_index``
after joining grids and chunks. These tests pin the contracts that the deep
copies used to guarantee: what users receive is still their own copy, saving
leaves the stored table untouched, and joined output is still sorted.
"""

from pathlib import Path

import numpy as np
import pandas as pd
import pytest

from supy._run_rust import _concat_output_frames
from supy._save import save_df_output
from supy.suews_output import SUEWSOutput

pytestmark = pytest.mark.api


def _make_df_output(n_steps: int = 48, grid: int = 1) -> pd.DataFrame:
    """Build a small output frame with SUEWS and DailyState groups."""
    idx_dt = pd.date_range("2012-01-01 00:05", periods=n_steps, freq="5min")
    index = pd.MultiIndex.from_product([[grid], idx_dt], names=["grid", "datetime"])
    columns = pd.MultiIndex.from_tuples(
        [("SUEWS", "QH"), ("SUEWS", "QE"), ("DailyState", "LAI_Grass")],
        names=["group", "var"],
    )
    rng = np.random.default_rng(1849)
    values = rng.random((n_steps, len(columns)))
    values[:-1, 2] = np.nan  # DailyState is written once a day
    return pd.DataFrame(values, index=index, columns=columns)


class TestSUEWSOutputCopies:
    def test_df_is_independent_of_stored_table(self):
        output = SUEWSOutput(df_output=_make_df_output(), df_state_final=None)
        df_before = output.df

        df_user = output.df
        df_user.iloc[:, 0] = -1.0

        pd.testing.assert_frame_equal(output.df, df_before)

    def test_group_accessor_is_independent_of_stored_table(self):
        output = SUEWSOutput(df_output=_make_df_output(), df_state_final=None)
        df_before = output.df

        df_group = output.get_group("SUEWS")
        df_group.iloc[:, 0] = -1.0

        pd.testing.assert_frame_equal(output.df, df_before)

    def test_caller_relabelling_does_not_reach_stored_table(self):
        df_output = _make_df_output()
        output = SUEWSOutput(df_output=df_output, df_state_final=None)
        df_before = output.df

        df_output.index = df_output.index.set_levels([99], level="grid")
        df_output.columns = df_output.columns.set_levels(["X", "Y"], level="group")

        pd.testing.assert_frame_equal(output.df, df_before)


@pytest.fixture(scope="module")
def df_short_output(short_sample_sim) -> pd.DataFrame:
    """Output of one run of the one-month short sample, shared by the module."""
    return short_sample_sim().run().df


class TestSaveLeavesInputUntouched:
    @pytest.mark.parametrize("save_tstep", [False, True])
    def test_txt_save_does_not_mutate_input(
        self, tmp_path: Path, df_short_output, save_tstep
    ):
        df_output = df_short_output.copy()
        df_before = df_output.copy()

        save_df_output(
            df_output,
            freq_s=3600,
            site="T",
            path_dir_save=tmp_path,
            save_tstep=save_tstep,
            output_groups=["SUEWS", "DailyState"],
        )

        pd.testing.assert_frame_equal(df_output, df_before)

    def test_txt_save_matches_save_of_deep_copy(self, tmp_path: Path, df_short_output):
        """Saving the shared table writes the same files as saving a copy."""
        df_output = df_short_output.copy()
        dir_shared = tmp_path / "shared"
        dir_copy = tmp_path / "copy"
        dir_shared.mkdir()
        dir_copy.mkdir()

        list_shared = save_df_output(
            df_output, freq_s=3600, site="T", path_dir_save=dir_shared
        )
        list_copy = save_df_output(
            df_output.copy(deep=True), freq_s=3600, site="T", path_dir_save=dir_copy
        )

        names_shared = sorted(Path(p).name for p in list_shared)
        names_copy = sorted(Path(p).name for p in list_copy)
        assert names_shared == names_copy
        assert names_shared
        for name in names_shared:
            text_shared = (dir_shared / name).read_text(encoding="utf-8")
            text_copy = (dir_copy / name).read_text(encoding="utf-8")
            assert text_shared == text_copy


class TestConcatOutputFrames:
    def test_single_frame_is_returned_without_copy(self):
        df_output = _make_df_output()

        df_joined = _concat_output_frames([df_output])

        assert df_joined is df_output

    def test_out_of_order_frames_are_sorted(self):
        df_grid2 = _make_df_output(grid=2)
        df_grid1 = _make_df_output(grid=1)

        df_joined = _concat_output_frames([df_grid2, df_grid1])

        assert df_joined.index.is_monotonic_increasing
        pd.testing.assert_frame_equal(
            df_joined, pd.concat([df_grid2, df_grid1]).sort_index()
        )

    def test_consecutive_chunks_match_sorted_concat(self):
        df_output = _make_df_output(n_steps=96)
        list_chunks = [df_output.iloc[:40], df_output.iloc[40:]]

        df_joined = _concat_output_frames(list_chunks)

        pd.testing.assert_frame_equal(df_joined, df_output)
