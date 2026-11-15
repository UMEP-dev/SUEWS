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

Output contract 2.0.0 deliberately updates the SUEWS hashes for rain rates
and scientific notation; all non-SUEWS hashes remain unchanged.

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
        ("T1_2012_SUEWS_60.txt", "ea9d9e3d5c62ddbe02c5b665b2f3773cf985d30a5037eed2cc7d4a7d577e1d47"),
        ("T1_2013_SUEWS_60.txt", "d632dde45cb20d338bd4107fbce487e7fdcdd5b648d80bb81b3c53f62e1242bd"),
        ("T2_2012_SUEWS_60.txt", "3d9c795d72770c978c865965d8e04a2bcd315d10a1ab410aa6935cad2ba8147c"),
        ("T2_2013_SUEWS_60.txt", "4bbd0b737e847c3c2ade4c406e3b01aa20bcf568df7985695ebb0494972ed792"),
    ],
    "dailystate_only": [
        ("T1_2012_DailyState.txt", "94cf92ac083a08f0d9ef14d33960d0cc46d6776ae8cb146e6303c16310bd126f"),
        ("T1_2013_DailyState.txt", "36e2aa232e415dadab47721c51a688f42b46fe2c36c8972a2d610dc958599353"),
        ("T2_2012_DailyState.txt", "75d880f2a16e54014f2a55a3f296163f37ee23a6e601e1ea29d92dc369aded6c"),
        ("T2_2013_DailyState.txt", "2ee142e7af93eed1364b6c50e4d3858e388911759047036dafb0995394302c16"),
    ],
    "all_groups_save_tstep": [
        ("T1_2012_SUEWS_5.txt", "357e17df6563fc4386add2ededabcd83e571e2fd4907fc70ff9aecf0c8940276"),
        ("T1_2013_SUEWS_5.txt", "6e3f3fa5f71994c731647279a2374e668f0fb10acd0d0d939d7f7f300c5c44c1"),
        ("T1_2012_RSL_5.txt", "0fca5f847bfad86aa24b68605b603ca80fcd3d428778bd24fad5ca6ae0aa5d10"),
        ("T1_2013_RSL_5.txt", "37c82fca44ab4cfaf12007a9410ee20f4f9f5d3571b22ddc5e35d189a685b172"),
        ("T1_2012_DailyState.txt", "3b676dcbd230aae16c87f17b30f4f980dc5adf0e0f586e6030a26ea654e718df"),
        ("T1_2013_DailyState.txt", "bee27f80bdf171df02cf6e74f539fb990d33ccf9f0bc834d284c25d58f09da12"),
        ("T2_2012_SUEWS_5.txt", "38a66720b384456fc7b8203285f42670cdb94e5eadd8f44269087d6cd6550baf"),
        ("T2_2013_SUEWS_5.txt", "466048dcf6f189cf0614361cf91d0a4cf1e5eb0cd4547a0711f11b1822c8b7da"),
        ("T2_2012_RSL_5.txt", "fc2375189bc35b80409736ca2767e40c65d5c912b786e3119596cc1e0f30f1f9"),
        ("T2_2013_RSL_5.txt", "a46da013c725b421a9df081d80553e07d59df4e4be06d9c7ab6ad35fa5b0b443"),
        ("T2_2012_DailyState.txt", "b248b7e569ca962020608fff92024ecb22ed1a5655d6d698e810412c43597794"),
        ("T2_2013_DailyState.txt", "0b62c2909960b1615ee367a4c28895ab25775dc799c4df09985a377cc7519d97"),
        ("T1_2012_SUEWS_60.txt", "ea9d9e3d5c62ddbe02c5b665b2f3773cf985d30a5037eed2cc7d4a7d577e1d47"),
        ("T1_2013_SUEWS_60.txt", "d632dde45cb20d338bd4107fbce487e7fdcdd5b648d80bb81b3c53f62e1242bd"),
        ("T1_2012_RSL_60.txt", "6bc61a22b591fc4cf8ebe5f5f01e020d48f5f069f4e59694f5121bae6c591940"),
        ("T1_2013_RSL_60.txt", "bd8c022b7c1a7da7fee435a583fb2b69c74ff61364c972372666dceabc07bb62"),
        ("T2_2012_SUEWS_60.txt", "3d9c795d72770c978c865965d8e04a2bcd315d10a1ab410aa6935cad2ba8147c"),
        ("T2_2013_SUEWS_60.txt", "4bbd0b737e847c3c2ade4c406e3b01aa20bcf568df7985695ebb0494972ed792"),
        ("T2_2012_RSL_60.txt", "2a529b08f4648742db4407823c48cd0da843a39757dffa3176a9ca56ff19c06c"),
        ("T2_2013_RSL_60.txt", "7b263bf48ffe1f9dae3292d4df09607d9497976e7d210b94cb2d241b74fc1437"),
    ],
    "utc": [
        ("T1_2012_DailyState_UTC.txt", "15ec012b315e7efcfc4e98736e5760432b5972294dc0a9329b0f07e683e0caf0"),
        ("T1_2013_DailyState_UTC.txt", "d5b29005ef04d755d96209fcdda65e344dd33e0b1d87a774be0b532cc0836c84"),
        ("T2_2012_DailyState_UTC.txt", "77d3136e05602e07ad8917ff749ce56709d29af3568971b962234c7619a16494"),
        ("T2_2013_DailyState_UTC.txt", "f543cc9bbea0b7481f5c034425aba7c9f9bd448e97d7cf1942f54d7509957600"),
        ("T1_2012_SUEWS_60_UTC.txt", "7c760e827f123e84706fcd390fad6d4caf2957802758d16a30ab56ddf7938294"),
        ("T1_2013_SUEWS_60_UTC.txt", "3d4c94c519d2bfe9a0bed04fda90e42a013d723a2e50254fa740e7b9c825b862"),
        ("T2_2012_SUEWS_60_UTC.txt", "a8102bb5bda7e64f208000005653ab1e8f86770412d19f935fd565ea1ad1f44c"),
        ("T2_2013_SUEWS_60_UTC.txt", "34309e648abcb6eff8de025ee6bce01548773947c334e98caa85b1bc2a856723"),
    ],
    "daylight": [
        ("T1_2012_DailyState_DAYLIGHT.txt", "94cf92ac083a08f0d9ef14d33960d0cc46d6776ae8cb146e6303c16310bd126f"),
        ("T1_2013_DailyState_DAYLIGHT.txt", "734c5ebb31c95ba85bce1983a45b56f5dad13fc0dbc60dd3fecb32d3305650c9"),
        ("T2_2012_DailyState_DAYLIGHT.txt", "75d880f2a16e54014f2a55a3f296163f37ee23a6e601e1ea29d92dc369aded6c"),
        ("T2_2013_DailyState_DAYLIGHT.txt", "f27a018b6448ba55fe4058a8b5160a79173e725bf3937f63ca1926ec30de34d9"),
        ("T1_2012_SUEWS_60_DAYLIGHT.txt", "03e4f8c007e7910c965b2bff83ddc8368732ce367fd5d155ff8b4121f44fded3"),
        ("T1_2013_SUEWS_60_DAYLIGHT.txt", "87362c055f8846fb783485148666305ea393c81baff1c615e1924dcae26d66ac"),
        ("T2_2012_SUEWS_60_DAYLIGHT.txt", "c2c9967fbe598dc75a2fa820a3653ee179a9c457f5fd6f7b97e626d0a3ff755f"),
        ("T2_2013_SUEWS_60_DAYLIGHT.txt", "652a9994134a5e09adccfec112d6a76dde18d7064497ae4710cba08619073bda"),
    ],
    "index_shared": [
        ("T1_2012_SUEWS_5.txt", "357e17df6563fc4386add2ededabcd83e571e2fd4907fc70ff9aecf0c8940276"),
        ("T1_2013_SUEWS_5.txt", "6e3f3fa5f71994c731647279a2374e668f0fb10acd0d0d939d7f7f300c5c44c1"),
        ("T1_2012_RSL_5.txt", "0fca5f847bfad86aa24b68605b603ca80fcd3d428778bd24fad5ca6ae0aa5d10"),
        ("T1_2013_RSL_5.txt", "37c82fca44ab4cfaf12007a9410ee20f4f9f5d3571b22ddc5e35d189a685b172"),
        ("T2_2012_SUEWS_5.txt", "38a66720b384456fc7b8203285f42670cdb94e5eadd8f44269087d6cd6550baf"),
        ("T2_2013_SUEWS_5.txt", "466048dcf6f189cf0614361cf91d0a4cf1e5eb0cd4547a0711f11b1822c8b7da"),
        ("T2_2012_RSL_5.txt", "fc2375189bc35b80409736ca2767e40c65d5c912b786e3119596cc1e0f30f1f9"),
        ("T2_2013_RSL_5.txt", "a46da013c725b421a9df081d80553e07d59df4e4be06d9c7ab6ad35fa5b0b443"),
    ],
    "index_shared_jan1": [
        ("T1_2013_SUEWS_5.txt", "30a0a227bb22e2a9b8c05a32269b7178cca9d8dc5e95da6d45ddee0a5faaaf29"),
        ("T1_2013_RSL_5.txt", "71d49d55b80582d01ee41e771c5981eed81d5186182c2ec27effbf97b99010ad"),
        ("T2_2013_SUEWS_5.txt", "6e3f3fa5f71994c731647279a2374e668f0fb10acd0d0d939d7f7f300c5c44c1"),
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
    # The tables retain their exact bytes; each has an auxiliary units sidecar.
    assert sorted(p.name for p in tmp_path.iterdir()) == sorted({
        filename for name, _ in list_saved for filename in (name, name + ".units.json")
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
