"""Output copies stay independent after the gh#1849 memory changes.

The run output is no longer deep-copied on the way into ``SUEWSOutput``, on
each ``.df`` or group access, on entry to the text and parquet savers, or by an
unconditional ``sort_index`` after joining grids and chunks. These tests pin
the contracts that the deep copies used to guarantee: nothing a user does to
what they receive reaches the stored table, saving leaves the stored table
untouched, and joined output is still sorted.

``.df`` shares the stored table instead of copying it. With pandas
copy-on-write (always on from pandas 3) the shared frame behaves as a copy;
without it (pandas 2) it is a read-only view, so a partial in-place write
raises ``ValueError`` instead of changing the stored table.
"""

from pathlib import Path

import numpy as np
from packaging.version import Version
import pandas as pd
import pytest

from supy._run_rust import _concat_output_frames
from supy._save import save_df_output
from supy.suews_output import SUEWSOutput, _copy_on_write_active

pytestmark = pytest.mark.api

COW = _copy_on_write_active()


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


def _partial_writes():
    """In-place writes into part of a frame, each as (id, function)."""
    return [
        ("iloc_cell", lambda df: df.iloc.__setitem__((0, 0), -1.0)),
        ("loc_mask", lambda df: df.loc.__setitem__(df["SUEWS", "QH"] > 0.5, -1.0)),
        ("at", lambda df: df.at.__setitem__((df.index[0], ("SUEWS", "QE")), -1.0)),
        ("fillna_inplace", lambda df: df.fillna(-1.0, inplace=True)),
        ("bfill_inplace", lambda df: df.bfill(inplace=True)),
    ]


class TestSUEWSOutputSharedFrame:
    def test_df_shares_the_stored_table(self):
        output = SUEWSOutput(df_output=_make_df_output(), df_state_final=None)

        assert np.shares_memory(output.df.to_numpy(), output._df_output.to_numpy())

    def test_df_of_a_real_run_shares_the_stored_table(self, short_sample_sim):
        """The kernel's output is one buffer, so `.df` costs no copy of it."""
        output = short_sample_sim().run()

        assert np.shares_memory(output.df.to_numpy(), output._df_output.to_numpy())

    @pytest.mark.parametrize(
        "write",
        [w for _, w in _partial_writes()],
        ids=[i for i, _ in _partial_writes()],
    )
    def test_partial_write_never_reaches_stored_table(self, write):
        output = SUEWSOutput(df_output=_make_df_output(), df_state_final=None)
        df_stored = output._df_output.copy()

        df_user = output.df
        if COW:
            write(df_user)
            assert not df_user.equals(df_stored)
        else:
            with pytest.raises(ValueError, match="read-only"):
                write(df_user)

        pd.testing.assert_frame_equal(output._df_output, df_stored)

    # pandas 2 reports the read-only buffer as a dtype problem, then replaces
    # the column: the assignment works and the stored table is untouched.
    @pytest.mark.filterwarnings(
        "ignore:Setting an item of incompatible dtype:FutureWarning"
    )
    def test_whole_column_assignment_never_reaches_stored_table(self):
        output = SUEWSOutput(df_output=_make_df_output(), df_state_final=None)
        df_stored = output._df_output.copy()

        df_user = output.df
        df_user.iloc[:, 0] = -1.0
        df_user["SUEWS", "QE"] = -2.0
        df_user["SUEWS", "new"] = -3.0

        np.testing.assert_array_equal(df_user.iloc[:, 0].to_numpy(), -1.0)
        np.testing.assert_array_equal(df_user["SUEWS", "QE"].to_numpy(), -2.0)
        pd.testing.assert_frame_equal(output._df_output, df_stored)

    def test_copy_of_df_is_writable_and_independent(self):
        output = SUEWSOutput(df_output=_make_df_output(), df_state_final=None)
        df_stored = output._df_output.copy()

        df_user = output.df.copy()
        df_user.iloc[0, 0] = -1.0
        df_user.fillna(-1.0, inplace=True)

        assert df_user.iloc[0, 0] == pytest.approx(-1.0)
        pd.testing.assert_frame_equal(output._df_output, df_stored)

    def test_axis_renaming_never_reaches_stored_table(self):
        output = SUEWSOutput(df_output=_make_df_output(), df_state_final=None)

        df_user = output.df
        df_user.index.names = ["g", "t"]
        df_user.columns.names = ["a", "b"]

        assert list(output._df_output.index.names) == ["grid", "datetime"]
        assert list(output._df_output.columns.names) == ["group", "var"]
        assert len(output.times) == len(output._df_output)

    def test_group_accessor_never_reaches_stored_table(self):
        output = SUEWSOutput(df_output=_make_df_output(), df_state_final=None)
        df_stored = output._df_output.copy()

        df_group = output.get_group("SUEWS")
        try:
            df_group.iloc[0, 0] = -1.0
        except ValueError:
            # pandas 2 may select the group as a read-only view
            assert not COW

        pd.testing.assert_frame_equal(output._df_output, df_stored)

    @pytest.mark.parametrize("dtype_extra", [float, int], ids=["two_blocks", "mixed"])
    def test_frame_not_in_one_buffer_is_returned_as_copy(self, dtype_extra):
        """Without one shared buffer (e.g. after resampling) `.df` copies."""
        df_output = _make_df_output()
        df_output["snow", "SWE"] = np.ones(len(df_output), dtype=dtype_extra)
        output = SUEWSOutput(df_output=df_output, df_state_final=None)
        df_stored = output._df_output.copy()

        df_user = output.df
        df_user.iloc[0, 0] = -1.0

        assert df_user.iloc[0, 0] == pytest.approx(-1.0)
        pd.testing.assert_frame_equal(output._df_output, df_stored)

    def test_extension_dtype_is_kept(self):
        output = SUEWSOutput(
            df_output=_make_df_output().astype("Float64"), df_state_final=None
        )
        df_stored = output._df_output.copy()

        df_user = output.df
        df_user.iloc[0, 0] = -1.0

        assert (df_user.dtypes == "Float64").all()
        pd.testing.assert_frame_equal(output._df_output, df_stored)

    def test_copy_on_write_is_detected(self):
        if Version(pd.__version__).major >= 3:
            assert COW
        else:
            cow_option = pd.get_option("mode.copy_on_write")
            assert (cow_option is True) is COW


class TestSUEWSOutputCallerFrame:
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
