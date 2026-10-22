"""``save_df_output`` writes the same files, in the same order, as before gh#1849.

gh#1849 rewrote how ``save_df_output`` walks the held output (groups as
views, one index shift per table, one grid-group-year at a time). The
expected hashes below were produced by the saver as it stood before that
rewrite, on a small deterministic output table, so these tests pin both the
saved bytes and the returned file list (order and duplicates included)
across the main option sets.

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
    # "resampled" tables are the same table, and the second pass starts from
    # the index the first pass already shifted (see save_df_output). These
    # two cases pin that long-standing behaviour as it is (rows labelled one
    # step early, files written more than once, repeated list entries); a
    # deliberate fix changes their hashes.
    "index_shared": (
        {},
        {"output_groups": ["SUEWS", "RSL"], "save_tstep": True, "freq_s": 300},
    ),
    "index_shared_jan1": (
        {"start": "2013-01-01 00:05", "n_day": 2},
        {"output_groups": ["SUEWS", "RSL"], "save_tstep": True, "freq_s": 300},
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


# hashes from the saver before gh#1849 (pandas 2.3.3, Linux)
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
        ("T1_2012_SUEWS_5.txt", "5ecc57e90831bd1217eb536de384fecff36cb090fcc8b9b4eaf54936f3c43102"),
        ("T1_2013_SUEWS_5.txt", "e72a3678bdfbb3659f7c1940dc82941f264147edc03abda72f037048c0c5a887"),
        ("T1_2012_RSL_5.txt", "77db84da475bf5ea70eceafb673301c0c29a053f9ba93b5f07634173974acb63"),
        ("T1_2013_RSL_5.txt", "7cb02ada2598bb19772c9dde66ab9c38278eacd6a4d6443bb05b3bf60e0d6261"),
        ("T2_2012_SUEWS_5.txt", "946741d92084a013a0696ec871cf31c85a8ccfcdfce239280a447e68049de395"),
        ("T2_2013_SUEWS_5.txt", "1ccc58d6a732e727d1b7dd3e7873d76566f4e8fb8649933d39c20ee65b388ac4"),
        ("T2_2012_RSL_5.txt", "c8d5d3a2f0a59c674e4a7482a7e1477c8b7528f5dd27da4b2d679f356244785a"),
        ("T2_2013_RSL_5.txt", "2a3ae6d6c9022d1a13ee340c2c56b40860063e141a2a139452d55099705701ed"),
        ("T1_2012_SUEWS_5.txt", "5ecc57e90831bd1217eb536de384fecff36cb090fcc8b9b4eaf54936f3c43102"),
        ("T1_2013_SUEWS_5.txt", "e72a3678bdfbb3659f7c1940dc82941f264147edc03abda72f037048c0c5a887"),
        ("T1_2012_RSL_5.txt", "77db84da475bf5ea70eceafb673301c0c29a053f9ba93b5f07634173974acb63"),
        ("T1_2013_RSL_5.txt", "7cb02ada2598bb19772c9dde66ab9c38278eacd6a4d6443bb05b3bf60e0d6261"),
        ("T2_2012_SUEWS_5.txt", "946741d92084a013a0696ec871cf31c85a8ccfcdfce239280a447e68049de395"),
        ("T2_2013_SUEWS_5.txt", "1ccc58d6a732e727d1b7dd3e7873d76566f4e8fb8649933d39c20ee65b388ac4"),
        ("T2_2012_RSL_5.txt", "c8d5d3a2f0a59c674e4a7482a7e1477c8b7528f5dd27da4b2d679f356244785a"),
        ("T2_2013_RSL_5.txt", "2a3ae6d6c9022d1a13ee340c2c56b40860063e141a2a139452d55099705701ed"),
    ],
    "index_shared_jan1": [
        ("T1_2013_SUEWS_5.txt", "8033b951be6047e8e2eb488d184bff4149278063a5317dca7f58eaf11394b2f4"),
        ("T1_2013_RSL_5.txt", "85fc6518a72689e3b6067c8c6d250c5ef989828b1e4f0bf0c4b24c817bc5d95f"),
        ("T2_2013_SUEWS_5.txt", "e72a3678bdfbb3659f7c1940dc82941f264147edc03abda72f037048c0c5a887"),
        ("T2_2013_RSL_5.txt", "7cb02ada2598bb19772c9dde66ab9c38278eacd6a4d6443bb05b3bf60e0d6261"),
        ("T1_2013_SUEWS_5.txt", "8033b951be6047e8e2eb488d184bff4149278063a5317dca7f58eaf11394b2f4"),
        ("T1_2013_SUEWS_5.txt", "8033b951be6047e8e2eb488d184bff4149278063a5317dca7f58eaf11394b2f4"),
        ("T1_2013_RSL_5.txt", "85fc6518a72689e3b6067c8c6d250c5ef989828b1e4f0bf0c4b24c817bc5d95f"),
        ("T1_2013_RSL_5.txt", "85fc6518a72689e3b6067c8c6d250c5ef989828b1e4f0bf0c4b24c817bc5d95f"),
        ("T2_2013_SUEWS_5.txt", "e72a3678bdfbb3659f7c1940dc82941f264147edc03abda72f037048c0c5a887"),
        ("T2_2013_SUEWS_5.txt", "e72a3678bdfbb3659f7c1940dc82941f264147edc03abda72f037048c0c5a887"),
        ("T2_2013_RSL_5.txt", "7cb02ada2598bb19772c9dde66ab9c38278eacd6a4d6443bb05b3bf60e0d6261"),
        ("T2_2013_RSL_5.txt", "7cb02ada2598bb19772c9dde66ab9c38278eacd6a4d6443bb05b3bf60e0d6261"),
    ],
}
# fmt: on


@pytest.mark.parametrize("case", list(_CASES))
def test_save_df_output_files_unchanged(case, tmp_path):
    list_saved = _save_case(case, tmp_path)

    assert list_saved == [tuple(item) for item in _EXPECTED[case]]
    # nothing else is written
    assert sorted(p.name for p in tmp_path.iterdir()) == sorted({
        name for name, _ in list_saved
    })


if __name__ == "__main__" and "--generate" in sys.argv:
    dict_out = {}
    for case_name in _CASES:
        with tempfile.TemporaryDirectory() as dir_tmp:
            dict_out[case_name] = _save_case(case_name, Path(dir_tmp))
            if "--show" in sys.argv:
                for name, _ in dict.fromkeys(dict_out[case_name]):
                    lines = (Path(dir_tmp) / name).read_text("utf-8").splitlines()
                    print(f"# {case_name} {name}: {lines[1][:40]!r}", file=sys.stderr)
    print("_EXPECTED = {")
    for case_name, list_saved in dict_out.items():
        print(f'    "{case_name}": [')
        for name, sha in list_saved:
            print(f'        ("{name}", "{sha}"),')
        print("    ],")
    print("}")
