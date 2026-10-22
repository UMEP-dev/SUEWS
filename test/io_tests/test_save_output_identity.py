"""``save_df_output`` writes the same files, in the same order, as before gh#1849.

gh#1849 rewrote how ``save_df_output`` walks the held output (groups as
views, one index shift per table, one grid-group-year at a time). The
expected hashes below were produced by the saver as it stood before that
rewrite, on a small deterministic output table, so these tests pin both the
saved bytes and the returned file list (order and duplicates included)
across the main option sets. The exceptions are the ``save_tstep`` cases
at the native timestep: without DailyState (the two ``index_shared`` cases)
the old saver wrote the files with every row one step early and listed them
repeatedly, so their hashes were regenerated deliberately when that was
fixed; with DailyState (``native_with_dailystate``) it wrote and listed the
non-daily files twice with identical bytes, and now does so once.
``test_save_tstep_native_step`` checks their rows directly.

The table is synthetic rather than a model run, so the hashes do not move
with the platform-dependent kernel. Values are multiples of 1/8, so
resampled sums are exact and means are correctly rounded on every
platform. The aggregation rules are frozen in ``_AGGM`` so a change to the
output registry does not move the hashes either. Line endings are
normalised before hashing because the writer uses ``os.linesep``.

To regenerate (only for a deliberate change to the saved output), run this
file as a script with ``--generate`` and paste the printed ``_EXPECTED``.
"""

from functools import partial
import hashlib
from pathlib import Path
import sys
import tempfile

import numpy as np
import pandas as pd
import pytest

from supy import _post, _save  # ruff: ignore[import-private-name]

pytestmark = pytest.mark.api

# Frozen aggregation rules: real variable names (the SUEWS group is filtered
# by output level through the registry), fixed methods.
_AGGM = {
    "SUEWS": {
        "QN": "mean",
        "QH": "mean",
        "QE": "mean",
        "Rain": "sum",
        "SMD": "last",
        "T2": "mean",
    },
    "RSL": {"zH_RSL": "mean", "Lc": "mean"},
    "DailyState": {"HDD1_h": "last", "LAI_Grass": "last"},
}


def _make_df_output(start: str = "2012-12-30 00:05", n_day: int = 4) -> pd.DataFrame:
    """Return a two-grid 5-min output table with SUEWS, RSL and DailyState."""
    idx_dt = pd.date_range(start, periods=n_day * 288, freq="5min")
    index = pd.MultiIndex.from_product([[1, 2], idx_dt], names=["grid", "datetime"])
    columns = pd.MultiIndex.from_tuples(
        [(group, var) for group, dict_var in _AGGM.items() for var in dict_var],
        names=["group", "var"],
    )
    i_row = np.arange(len(index))[:, None]
    i_col = np.arange(len(columns))[None, :]
    values = (((i_row * 7 + i_col * 13) % 97) - 40) / 8.0
    # a few missing values, written as -999
    values[(i_row * 5 + i_col) % 211 == 3] = np.nan
    # DailyState holds values only at the last step of each day
    idx_all = index.get_level_values("datetime")
    is_end_of_day = (idx_all.hour == 0) & (idx_all.minute == 0)
    is_daily = columns.get_level_values("group") == "DailyState"
    values[np.ix_(~is_end_of_day, is_daily)] = np.nan
    return pd.DataFrame(values, index=index, columns=columns)


def _make_df_state_final() -> pd.DataFrame:
    """Per-grid timezone and a daylight window with both transitions in range."""
    columns = pd.MultiIndex.from_tuples(
        [("timezone", "0"), ("startdls", "0"), ("enddls", "0")],
        names=["var", "ind_dim"],
    )
    # daylight from DOY 2 to DOY 366: on until 31 Dec 2012 00:00, back on at
    # 2 Jan 2013 00:00
    return pd.DataFrame(
        [[1.0, 2.0, 366.0], [5.5, 2.0, 366.0]],
        index=pd.Index([1, 2], name="grid"),
        columns=columns,
    )


# option sets: (frame arguments, save_df_output arguments)
_CASES = {
    "default": ({}, {}),
    "dailystate_only": ({}, {"output_groups": ["DailyState"]}),
    "all_groups_save_tstep": (
        {},
        {"output_groups": ["SUEWS", "RSL", "DailyState"], "save_tstep": True},
    ),
    "utc": ({}, {"timestamp_reference": "utc"}),
    "daylight": ({}, {"timestamp_reference": "daylight"}),
    # save_tstep at the native timestep without DailyState: the native and
    # "resampled" tables are the same table, which is saved once, with
    # end-of-period labels like every other path. Before the fix the second
    # pass started from the index the first pass had already shifted, so the
    # files carried every row one step early and were listed repeatedly; a
    # run starting 00:05 on 1 January lost its first step.
    "index_shared": (
        {},
        {"output_groups": ["SUEWS", "RSL"], "save_tstep": True, "freq_s": 300},
    ),
    "index_shared_jan1": (
        {"start": "2013-01-01 00:05", "n_day": 2},
        {"output_groups": ["SUEWS", "RSL"], "save_tstep": True, "freq_s": 300},
    ),
    # the same with DailyState: the non-daily files used to be written and
    # listed twice (identical bytes); now each is written and listed once
    "native_with_dailystate": (
        {},
        {
            "output_groups": ["SUEWS", "RSL", "DailyState"],
            "save_tstep": True,
            "freq_s": 300,
        },
    ),
}


def _sha256_text(path: Path) -> str:
    return hashlib.sha256(path.read_bytes().replace(b"\r\n", b"\n")).hexdigest()


def _save_case(case: str, dir_save: Path) -> list:
    """Save one option set; return ``[(file name, sha256), ...]`` in order."""
    kwargs_frame, kwargs_save = _CASES[case]
    resample_before = _save._resample_output
    _save._resample_output = partial(_post._resample_output, dict_aggm=_AGGM)
    try:
        list_path = _save.save_df_output(
            _make_df_output(**kwargs_frame),
            site="T",
            path_dir_save=dir_save,
            df_state_final=_make_df_state_final(),
            **kwargs_save,
        )
    finally:
        _save._resample_output = resample_before
    return [(Path(p).name, _sha256_text(dir_save / Path(p).name)) for p in list_path]


# hashes from the saver before gh#1849 (pandas 2.3.3, Linux); the two
# index_shared cases from the fixed saver. index_shared repeats the 5-min
# hashes of all_groups_save_tstep (same rows, same pass), and repeated hashes
# across cases are expected: values depend only on the global row number.
# fmt: off
_EXPECTED = {
    "default": [
        ("T1_2012_DailyState.txt", "94cf92ac083a08f0d9ef14d33960d0cc46d6776ae8cb146e6303c16310bd126f"),
        ("T1_2013_DailyState.txt", "36e2aa232e415dadab47721c51a688f42b46fe2c36c8972a2d610dc958599353"),
        ("T2_2012_DailyState.txt", "75d880f2a16e54014f2a55a3f296163f37ee23a6e601e1ea29d92dc369aded6c"),
        ("T2_2013_DailyState.txt", "2ee142e7af93eed1364b6c50e4d3858e388911759047036dafb0995394302c16"),
        ("T1_2012_SUEWS_60.txt", "8ce518eec2f2bdeb7fc736280248c68d61b7bdb90ddbf96df5749c4d00b6f8de"),
        ("T1_2013_SUEWS_60.txt", "825cdca26b6e49debeabb0d92b1f4abe6c587adfb8f10d82ae583c872621b8d6"),
        ("T2_2012_SUEWS_60.txt", "c3fb30a5e04c5605228404b20f12a86ce8acdf5b18ff96cec69b6a3b50611342"),
        ("T2_2013_SUEWS_60.txt", "414f8fb65142fa5d39419f32a1d245be675a110e0579ebec47607adbd4758dc4"),
    ],
    "dailystate_only": [
        ("T1_2012_DailyState.txt", "94cf92ac083a08f0d9ef14d33960d0cc46d6776ae8cb146e6303c16310bd126f"),
        ("T1_2013_DailyState.txt", "36e2aa232e415dadab47721c51a688f42b46fe2c36c8972a2d610dc958599353"),
        ("T2_2012_DailyState.txt", "75d880f2a16e54014f2a55a3f296163f37ee23a6e601e1ea29d92dc369aded6c"),
        ("T2_2013_DailyState.txt", "2ee142e7af93eed1364b6c50e4d3858e388911759047036dafb0995394302c16"),
    ],
    "all_groups_save_tstep": [
        ("T1_2012_SUEWS_5.txt", "ef093d637d425fc6ea21467c2c5d16006dfcb5a65ce0e8da1ca03b87563e80b2"),
        ("T1_2013_SUEWS_5.txt", "5e0ee063e09713653b12df4a85e3dc00b76c688d81d9cbe78f8034a32027561e"),
        ("T1_2012_RSL_5.txt", "0fca5f847bfad86aa24b68605b603ca80fcd3d428778bd24fad5ca6ae0aa5d10"),
        ("T1_2013_RSL_5.txt", "37c82fca44ab4cfaf12007a9410ee20f4f9f5d3571b22ddc5e35d189a685b172"),
        ("T1_2012_DailyState.txt", "3b676dcbd230aae16c87f17b30f4f980dc5adf0e0f586e6030a26ea654e718df"),
        ("T1_2013_DailyState.txt", "bee27f80bdf171df02cf6e74f539fb990d33ccf9f0bc834d284c25d58f09da12"),
        ("T2_2012_SUEWS_5.txt", "986892d61c58657ac1f5ca73d5deb91f6463a99e18b012089e5ccc596d73dc07"),
        ("T2_2013_SUEWS_5.txt", "7ca0e8a4250da91a410c9b1a0c88ecd7c34b28f70e37cd5166b99a53fab11d9a"),
        ("T2_2012_RSL_5.txt", "fc2375189bc35b80409736ca2767e40c65d5c912b786e3119596cc1e0f30f1f9"),
        ("T2_2013_RSL_5.txt", "a46da013c725b421a9df081d80553e07d59df4e4be06d9c7ab6ad35fa5b0b443"),
        ("T2_2012_DailyState.txt", "b248b7e569ca962020608fff92024ecb22ed1a5655d6d698e810412c43597794"),
        ("T2_2013_DailyState.txt", "0b62c2909960b1615ee367a4c28895ab25775dc799c4df09985a377cc7519d97"),
        ("T1_2012_SUEWS_60.txt", "8ce518eec2f2bdeb7fc736280248c68d61b7bdb90ddbf96df5749c4d00b6f8de"),
        ("T1_2013_SUEWS_60.txt", "825cdca26b6e49debeabb0d92b1f4abe6c587adfb8f10d82ae583c872621b8d6"),
        ("T1_2012_RSL_60.txt", "6bc61a22b591fc4cf8ebe5f5f01e020d48f5f069f4e59694f5121bae6c591940"),
        ("T1_2013_RSL_60.txt", "bd8c022b7c1a7da7fee435a583fb2b69c74ff61364c972372666dceabc07bb62"),
        ("T2_2012_SUEWS_60.txt", "c3fb30a5e04c5605228404b20f12a86ce8acdf5b18ff96cec69b6a3b50611342"),
        ("T2_2013_SUEWS_60.txt", "414f8fb65142fa5d39419f32a1d245be675a110e0579ebec47607adbd4758dc4"),
        ("T2_2012_RSL_60.txt", "2a529b08f4648742db4407823c48cd0da843a39757dffa3176a9ca56ff19c06c"),
        ("T2_2013_RSL_60.txt", "7b263bf48ffe1f9dae3292d4df09607d9497976e7d210b94cb2d241b74fc1437"),
    ],
    "utc": [
        ("T1_2012_DailyState_UTC.txt", "15ec012b315e7efcfc4e98736e5760432b5972294dc0a9329b0f07e683e0caf0"),
        ("T1_2013_DailyState_UTC.txt", "d5b29005ef04d755d96209fcdda65e344dd33e0b1d87a774be0b532cc0836c84"),
        ("T2_2012_DailyState_UTC.txt", "77d3136e05602e07ad8917ff749ce56709d29af3568971b962234c7619a16494"),
        ("T2_2013_DailyState_UTC.txt", "f543cc9bbea0b7481f5c034425aba7c9f9bd448e97d7cf1942f54d7509957600"),
        ("T1_2012_SUEWS_60_UTC.txt", "7b6d8eb255d9fc762cb57cf65d96737da8547de021509bf56c95b63252e184fb"),
        ("T1_2013_SUEWS_60_UTC.txt", "fbd57e28273ac75c6d80db9a8d450f677fc502e09e48dd5e3329e25c312bb9c1"),
        ("T2_2012_SUEWS_60_UTC.txt", "d26fa8dd15c8f64fa64e52cc737786e8f6be50274ee224764fbed2d91fd15834"),
        ("T2_2013_SUEWS_60_UTC.txt", "a7ca495e074705c26546ed25e992d4dbae7bc35b9af588af00ba6438ccb50401"),
    ],
    "daylight": [
        ("T1_2012_DailyState_DAYLIGHT.txt", "94cf92ac083a08f0d9ef14d33960d0cc46d6776ae8cb146e6303c16310bd126f"),
        ("T1_2013_DailyState_DAYLIGHT.txt", "734c5ebb31c95ba85bce1983a45b56f5dad13fc0dbc60dd3fecb32d3305650c9"),
        ("T2_2012_DailyState_DAYLIGHT.txt", "75d880f2a16e54014f2a55a3f296163f37ee23a6e601e1ea29d92dc369aded6c"),
        ("T2_2013_DailyState_DAYLIGHT.txt", "f27a018b6448ba55fe4058a8b5160a79173e725bf3937f63ca1926ec30de34d9"),
        ("T1_2012_SUEWS_60_DAYLIGHT.txt", "724468527f584bce19bbec756131ed21e6b2b1bf76790eead591ec37a1955852"),
        ("T1_2013_SUEWS_60_DAYLIGHT.txt", "b34b3b26070a7bcabce2658096e5040683d240880fce81671ee3da1bb4fc123d"),
        ("T2_2012_SUEWS_60_DAYLIGHT.txt", "47254002bd344a75b82f0f71a3a5d1d7bfad4c6c04b5e5bdc9f44dfa9a90c423"),
        ("T2_2013_SUEWS_60_DAYLIGHT.txt", "86a0a35c7dc20095b9f27a6684825eb01291cd6c3fa91e9998854973be5bafc6"),
    ],
    "index_shared": [
        ("T1_2012_SUEWS_5.txt", "ef093d637d425fc6ea21467c2c5d16006dfcb5a65ce0e8da1ca03b87563e80b2"),
        ("T1_2013_SUEWS_5.txt", "5e0ee063e09713653b12df4a85e3dc00b76c688d81d9cbe78f8034a32027561e"),
        ("T1_2012_RSL_5.txt", "0fca5f847bfad86aa24b68605b603ca80fcd3d428778bd24fad5ca6ae0aa5d10"),
        ("T1_2013_RSL_5.txt", "37c82fca44ab4cfaf12007a9410ee20f4f9f5d3571b22ddc5e35d189a685b172"),
        ("T2_2012_SUEWS_5.txt", "986892d61c58657ac1f5ca73d5deb91f6463a99e18b012089e5ccc596d73dc07"),
        ("T2_2013_SUEWS_5.txt", "7ca0e8a4250da91a410c9b1a0c88ecd7c34b28f70e37cd5166b99a53fab11d9a"),
        ("T2_2012_RSL_5.txt", "fc2375189bc35b80409736ca2767e40c65d5c912b786e3119596cc1e0f30f1f9"),
        ("T2_2013_RSL_5.txt", "a46da013c725b421a9df081d80553e07d59df4e4be06d9c7ab6ad35fa5b0b443"),
    ],
    "index_shared_jan1": [
        ("T1_2013_SUEWS_5.txt", "1d2ab420a521ede55518dab9561e8776ddfd872f5658533777eb864a35a25f9d"),
        ("T1_2013_RSL_5.txt", "71d49d55b80582d01ee41e771c5981eed81d5186182c2ec27effbf97b99010ad"),
        ("T2_2013_SUEWS_5.txt", "5e0ee063e09713653b12df4a85e3dc00b76c688d81d9cbe78f8034a32027561e"),
        ("T2_2013_RSL_5.txt", "37c82fca44ab4cfaf12007a9410ee20f4f9f5d3571b22ddc5e35d189a685b172"),
    ],
}
# fmt: on
# One pass at the native timestep writes exactly the native-pass files of
# all_groups_save_tstep (its first twelve entries), each listed once.
_EXPECTED["native_with_dailystate"] = _EXPECTED["all_groups_save_tstep"][:12]


@pytest.mark.parametrize("case", list(_CASES))
def test_save_df_output_files_unchanged(case, tmp_path):
    list_saved = _save_case(case, tmp_path)

    assert list_saved == [tuple(item) for item in _EXPECTED[case]]
    # nothing else is written
    assert sorted(p.name for p in tmp_path.iterdir()) == sorted({
        name for name, _ in list_saved
    })


def _read_rows(path: Path) -> pd.DataFrame:
    """Return a saved text file's rows indexed by their timestamps."""
    df_file = pd.read_csv(path, sep=r"\s+")
    idx_dt = (
        pd.to_datetime(df_file["Year"].astype(str), format="%Y")
        + pd.to_timedelta(df_file["DOY"] - 1, unit="D")
        + pd.to_timedelta(df_file["Hour"], unit="h")
        + pd.to_timedelta(df_file["Min"], unit="min")
    )
    df_file.index = pd.DatetimeIndex(idx_dt, name="datetime")
    return df_file.drop(columns=["Year", "DOY", "Hour", "Min", "Dectime"])


@pytest.mark.parametrize(
    "case", ["index_shared", "index_shared_jan1", "native_with_dailystate"]
)
def test_save_tstep_native_step(case, tmp_path):
    """``save_tstep`` at the native timestep saves every step once, end-labelled.

    Each file is listed once, and each grid's rows, read back from its files,
    carry exactly the output timestamps (the end of each step, as every other
    save path labels them) with the output values, the first step included.
    """
    kwargs_frame, _ = _CASES[case]
    df_output = _make_df_output(**kwargs_frame)
    list_saved = _save_case(case, tmp_path)
    list_name = [name for name, _ in list_saved]
    assert len(list_name) == len(set(list_name))

    for grid in (1, 2):
        df_expected = df_output.xs(grid, level="grid")["RSL"].fillna(-999.0)
        list_path = sorted(tmp_path.glob(f"T{grid}_*_RSL_5.txt"))
        assert list_path
        df_saved = pd.concat([_read_rows(path) for path in list_path])
        assert df_saved.index.equals(df_expected.index)
        np.testing.assert_array_equal(
            df_saved[df_expected.columns].to_numpy(), df_expected.to_numpy()
        )
        idx_suews = pd.concat([
            _read_rows(path) for path in sorted(tmp_path.glob(f"T{grid}_*_SUEWS_5.txt"))
        ]).index
        assert idx_suews.equals(df_expected.index)


if __name__ == "__main__" and "--generate" in sys.argv:
    dict_out = {}
    for case_name in _CASES:
        with tempfile.TemporaryDirectory() as dir_tmp:
            dict_out[case_name] = _save_case(case_name, Path(dir_tmp))
            if "--show" in sys.argv:
                for name, _ in dict.fromkeys(dict_out[case_name]):
                    lines = (
                        (Path(dir_tmp) / name).read_text(encoding="utf-8").splitlines()
                    )
                    print(f"# {case_name} {name}: {lines[1][:40]!r}", file=sys.stderr)
    print("_EXPECTED = {")
    for case_name, list_saved in dict_out.items():
        print(f'    "{case_name}": [')
        for name, sha in list_saved:
            print(f'        ("{name}", "{sha}"),')
        print("    ],")
    print("}")
