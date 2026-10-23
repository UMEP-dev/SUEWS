"""Output copies stay independent after the gh#1849 memory changes.

The run output is no longer deep-copied on the way into ``SUEWSOutput``, on
each ``.df`` or group access, on entry to the text and parquet savers, or by an
unconditional ``sort_index`` after joining grids and chunks. These tests pin
the contracts that the deep copies used to guarantee: nothing a user does to
what they receive reaches the stored table and saving leaves the stored table
untouched. That multi-grid and chunked output, now written into one table,
still comes out sorted is pinned in ``test/core/test_output_buffers.py``.

``.df`` shares the stored table instead of copying it. With pandas
copy-on-write (always on from pandas 3) the shared frame behaves as a copy;
without it (pandas 2) it is a read-only view, so a partial in-place write
raises ``ValueError`` instead of changing the stored table. Group access
selects from the stored table and returns a writable, independent frame on
every pandas version.
"""

from pathlib import Path
import tracemalloc
import warnings

import numpy as np
from packaging.version import Version
import pandas as pd
import pytest

from supy._save import save_df_output
from supy.suews_output import SUEWSOutput, _copy_on_write_active

pytestmark = pytest.mark.api

COW = _copy_on_write_active()


def _make_df_output(
    n_steps: int = 48, grid: int = 1, daily_first: bool = False
) -> pd.DataFrame:
    """Build a small output frame with SUEWS and DailyState groups.

    The kernel writes SUEWS before DailyState, so the group level of its
    columns is not sorted; ``daily_first`` builds the sorted order instead,
    for which pandas selects a group as a slice rather than by a mask.
    """
    idx_dt = pd.date_range("2012-01-01 00:05", periods=n_steps, freq="5min")
    index = pd.MultiIndex.from_product([[grid], idx_dt], names=["grid", "datetime"])
    list_columns = [("SUEWS", "QH"), ("SUEWS", "QE"), ("DailyState", "LAI_Grass")]
    if daily_first:
        list_columns = list_columns[-1:] + list_columns[:-1]
    columns = pd.MultiIndex.from_tuples(list_columns, names=["group", "var"])
    rng = np.random.default_rng(1849)
    values = rng.random((n_steps, len(columns)))
    i_daily = list_columns.index(("DailyState", "LAI_Grass"))
    values[:-1, i_daily] = np.nan  # DailyState is written once a day
    return pd.DataFrame(values, index=index, columns=columns)


def _column_iadd(df: pd.DataFrame) -> None:
    df["SUEWS", "QH"] += 1.0


def _partial_writes():
    """In-place writes into part of a frame, each as (id, function)."""
    return [
        ("iloc_cell", lambda df: df.iloc.__setitem__((0, 0), -1.0)),
        ("loc_mask", lambda df: df.loc.__setitem__(df["SUEWS", "QH"] > 0.5, -1.0)),
        ("at", lambda df: df.at.__setitem__((df.index[0], ("SUEWS", "QE")), -1.0)),
        ("fillna_inplace", lambda df: df.fillna(-1.0, inplace=True)),
        ("bfill_inplace", lambda df: df.bfill(inplace=True)),
        ("column_iadd", _column_iadd),
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

    def test_extracted_column_inplace_arithmetic_never_reaches_stored_table(self):
        output = SUEWSOutput(df_output=_make_df_output(), df_state_final=None)
        df_stored = output._df_output.copy()

        ser_user = output.df["SUEWS", "QH"]
        if COW:
            ser_user += 1.0
            np.testing.assert_allclose(
                ser_user.to_numpy(), df_stored["SUEWS", "QH"].to_numpy() + 1.0
            )
        else:
            with pytest.raises(ValueError, match="read-only"):
                ser_user += 1.0

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


def _write_group(df_group: pd.DataFrame) -> None:
    """Write into a group frame the ways a user might."""
    df_group.iloc[0, 0] = -1.0
    df_group.loc[df_group["QE"] > 0.5, "QE"] = -2.0
    df_group["QH"] += 1.0


@pytest.mark.parametrize("daily_first", [False, True], ids=["kernel_order", "sorted"])
class TestSUEWSOutputGroupFrame:
    """Group access returns a writable, independent frame on every pandas."""

    def test_group_matches_selection(self, daily_first):
        output = SUEWSOutput(
            df_output=_make_df_output(daily_first=daily_first), df_state_final=None
        )

        for group in ("SUEWS", "DailyState"):
            pd.testing.assert_frame_equal(
                output.get_group(group), output._df_output[group]
            )
        pd.testing.assert_frame_equal(output.SUEWS, output._df_output["SUEWS"])

    # No SettingWithCopyWarning either: the group frame is not a slice of
    # the stored table as far as pandas 2 is concerned.
    @pytest.mark.filterwarnings("error")
    def test_group_is_writable_and_independent(self, daily_first):
        output = SUEWSOutput(
            df_output=_make_df_output(daily_first=daily_first), df_state_final=None
        )
        df_stored = output._df_output.copy()

        df_group = output.get_group("SUEWS")
        _write_group(df_group)

        assert df_group.iloc[0, 0] == pytest.approx(0.0)
        pd.testing.assert_frame_equal(output._df_output, df_stored)

    def test_group_axis_renaming_never_reaches_stored_table(self, daily_first):
        output = SUEWSOutput(
            df_output=_make_df_output(daily_first=daily_first), df_state_final=None
        )

        df_group = output.get_group("SUEWS")
        df_group.index.names = ["g", "t"]
        df_group.columns.name = "v"

        assert list(output._df_output.index.names) == ["grid", "datetime"]
        assert list(output._df_output.columns.names) == ["group", "var"]

    @pytest.mark.parametrize("kind", ["two_blocks", "mixed", "extension"], ids=str)
    def test_group_of_frame_not_in_one_buffer(self, daily_first, kind):
        df_output = _make_df_output(daily_first=daily_first)
        if kind == "extension":
            df_output = df_output.astype("Float64")
        else:
            dtype_extra = float if kind == "two_blocks" else int
            df_output["SUEWS", "SWE"] = np.ones(len(df_output), dtype=dtype_extra)
        output = SUEWSOutput(df_output=df_output, df_state_final=None)
        df_stored = output._df_output.copy()

        df_group = output.get_group("SUEWS")
        pd.testing.assert_frame_equal(df_group, output._df_output["SUEWS"])
        df_group.iloc[0, 0] = -1.0

        assert df_group.iloc[0, 0] == pytest.approx(-1.0)
        pd.testing.assert_frame_equal(output._df_output, df_stored)

    def test_group_of_two_block_frame_costs_no_table_copy(self, daily_first):
        """Selecting a group never builds a copy of the whole table."""
        n_steps = 2000
        df_output = _make_df_output(n_steps=n_steps, daily_first=daily_first)
        # A second float block, many times the size of the group
        df_debug = pd.DataFrame(
            np.random.default_rng(1849).random((n_steps, 200)),
            index=df_output.index,
            columns=pd.MultiIndex.from_product(
                [["debug"], [f"v{i:03d}" for i in range(200)]], names=["group", "var"]
            ),
        )
        df_output = pd.concat([df_output, df_debug], axis=1)
        output = SUEWSOutput(df_output=df_output, df_state_final=None)
        n_bytes_table = output._df_output.to_numpy().nbytes

        tracemalloc.start()
        try:
            df_group = output.get_group("SUEWS")
            _, n_bytes_peak = tracemalloc.get_traced_memory()
        finally:
            tracemalloc.stop()

        assert df_group.shape == (n_steps, 2)
        assert n_bytes_peak < n_bytes_table / 4


def test_group_of_a_real_run_is_writable_and_independent(short_sample_sim):
    output = short_sample_sim().run()
    df_stored = output._df_output["SUEWS"].copy()

    for df_group in (output.get_group("SUEWS"), output.SUEWS):
        with warnings.catch_warnings():
            warnings.simplefilter("error")
            _write_group(df_group)
        assert not df_group.equals(df_stored)

    pd.testing.assert_frame_equal(output._df_output["SUEWS"], df_stored)


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
