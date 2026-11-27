"""Differential equivalence and attrs-isolation tests for Hotspot A.

Compares the attrs-stripped implementations of ``with_daily_coverage``,
``prepare_output``, ``_resample_output``, ``_output_group_frequencies``,
``_save_groups`` and ``_parquet_chunk_frame`` against verbatim master
(``141f0c347b``) copies in ``_master_attrs_reference.py`` across resampling,
unit conversion (including water-rate ``mm`` columns requiring coverage),
text saving and streamed Parquet saving.
"""

import copy
import importlib.util
import json
from pathlib import Path

from conftest import TIMESTEPS_PER_DAY
import pandas as pd
import pyarrow.parquet as pq
import pytest

import supy._output_metadata as output_metadata_mod
from supy._output_metadata import prepare_output, strip_attrs, with_daily_coverage
import supy._post as post_mod
from supy._post import _resample_output
import supy._save as save_mod
from supy._save import (
    StreamedParquetParts,
    save_df_output,
    save_df_output_parquet,
)
from supy.data_model.output import OUTPUT_REGISTRY
import supy.suews_output as suews_output_mod

pytestmark = [
    pytest.mark.native,
    pytest.mark.contract("C-RESAMPLE", "C-ISOLATION", "C-SAVE"),
]

DAYS_SAMPLE = 3
TSTEP_S = 300


def _load_master_reference():
    """Load the sibling master-reference helper without relying on sys.path."""
    path_ref = Path(__file__).parent / "_master_attrs_reference.py"
    spec = importlib.util.spec_from_file_location("_master_attrs_reference", path_ref)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


MASTER_REF = _load_master_reference()


def _patch_master(monkeypatch) -> None:
    """Bind all changed output/resample/save functions to their master copies."""
    monkeypatch.setattr(
        output_metadata_mod, "with_daily_coverage", MASTER_REF.with_daily_coverage
    )
    monkeypatch.setattr(
        output_metadata_mod, "prepare_output", MASTER_REF.prepare_output
    )
    monkeypatch.setattr(
        post_mod, "with_daily_coverage", MASTER_REF.with_daily_coverage
    )
    monkeypatch.setattr(post_mod, "_resample_output", MASTER_REF._resample_output)
    monkeypatch.setattr(
        save_mod, "with_daily_coverage", MASTER_REF.with_daily_coverage
    )
    monkeypatch.setattr(save_mod, "prepare_output", MASTER_REF.prepare_output)
    monkeypatch.setattr(save_mod, "_resample_output", MASTER_REF._resample_output)
    monkeypatch.setattr(
        save_mod, "_output_group_frequencies", MASTER_REF._output_group_frequencies
    )
    monkeypatch.setattr(save_mod, "_save_groups", MASTER_REF._save_groups)
    monkeypatch.setattr(
        save_mod, "_parquet_chunk_frame", MASTER_REF._parquet_chunk_frame
    )
    monkeypatch.setattr(
        suews_output_mod, "with_daily_coverage", MASTER_REF.with_daily_coverage
    )
    monkeypatch.setattr(
        suews_output_mod, "prepare_output", MASTER_REF.prepare_output
    )


def _run_short_sample(short_sample_sim, list_gridiv=None):
    """Run a 3-day slice of the short sample for 1 or 2 grids."""
    sim = short_sample_sim()
    forcing = sim.forcing.df.iloc[: DAYS_SAMPLE * TIMESTEPS_PER_DAY].copy()
    update = {
        "model": {
            "control": {
                "start_time": str(forcing.index[0]),
                "end_time": str(forcing.index[-1]),
            }
        }
    }
    if list_gridiv is not None:
        config = sim.config.model_copy(deep=True)
        site0 = config.sites[0]
        sites = []
        for pos, gridiv in enumerate(list_gridiv):
            site = site0.model_copy(deep=True)
            site.gridiv = gridiv
            site.name = f"grid{gridiv}"
            site.properties.lat.value += 10.0 * pos
            sites.append(site)
        update["sites"] = [
            site.model_dump(exclude_unset=True, round_trip=True, mode="json")
            for site in sites
        ]
    sim.update_config(update, auto_load_forcing=False)
    sim.update_forcing(forcing)
    return sim.run()


def _write_streamed_parquet(df_in, df_state_final, dir_pq: Path, freq_s: int):
    """Write *df_in* as two streamed Parquet chunks into *dir_pq*."""
    dir_pq.mkdir(parents=True, exist_ok=True)
    cutoff = pd.Timestamp("2012-01-02 00:00")
    dt_level = df_in.index.get_level_values("datetime")
    chunks = [
        df_in.loc[dt_level <= cutoff].copy(),
        df_in.loc[dt_level > cutoff].copy(),
    ]
    stream = StreamedParquetParts(dir_pq / "SUEWS_output.parquet", tstep_s=TSTEP_S)
    stream.prepare()
    for idx, chunk in enumerate(chunks, start=1):
        stream.start_chunk(2012, idx)
        save_df_output_parquet(
            chunk,
            df_state_final,
            freq_s=freq_s,
            path_dir_save=dir_pq,
            save_state=False,
            streamed_parquet=stream,
        )
    return stream.finish()


@pytest.fixture(scope="module")
def sample_outputs(short_sample_sim):
    """Module-scoped 1-grid and 2-grid 3-day sample outputs."""
    return {
        "1grid": _run_short_sample(short_sample_sim, list_gridiv=None),
        "2grids": _run_short_sample(short_sample_sim, list_gridiv=[2, 1]),
    }


@pytest.mark.parametrize("case", ["1grid", "2grids"])
@pytest.mark.parametrize("with_units", [False, True])
@pytest.mark.parametrize(
    "freq",
    [pd.Timedelta(TSTEP_S, "s"), pd.Timedelta(3600, "s"), "D"],
    ids=["300s", "3600s", "daily"],
)
def test_resample_matches_master_reference(sample_outputs, case, with_units, freq):
    """Stripped resampling matches the verbatim master path bit for bit."""
    output = sample_outputs[case]
    source = output.to_units({"Rain": "mm h-1"}) if with_units else output
    df_in = source.df.copy()
    df_in.attrs["custom_meta"] = {"case": case, "items": [1, 2]}
    attrs_before = copy.deepcopy(df_in.attrs)

    actual = _resample_output(df_in, freq=freq)
    expected = MASTER_REF._resample_output(df_in, freq=freq)

    pd.testing.assert_frame_equal(actual, expected, check_exact=True)
    assert actual.attrs == expected.attrs
    assert df_in.attrs == attrs_before
    if freq == pd.Timedelta(TSTEP_S, "s"):
        assert actual is df_in


@pytest.mark.parametrize("case", ["1grid", "2grids"])
def test_resample_output_attrs_are_independent(sample_outputs, case):
    """Mutating one resampled frame's attrs never affects another or its source."""
    output = sample_outputs[case].to_units({"Rain": "mm h-1"})
    df_in = output.df.copy()
    df_in.attrs["custom_meta"] = {"nested": ["a", "b"]}
    source_snapshot = copy.deepcopy(df_in.attrs)

    rsmp_a = _resample_output(df_in, freq=pd.Timedelta(3600, "s"))
    rsmp_b = _resample_output(df_in, freq=pd.Timedelta(3600, "s"))
    snapshot_b = copy.deepcopy(rsmp_b.attrs)

    assert rsmp_a.attrs == rsmp_b.attrs
    assert rsmp_a.attrs is not rsmp_b.attrs
    assert rsmp_a.attrs is not df_in.attrs

    # Mutate every nested mutable container in rsmp_a.attrs in place.
    rsmp_a.attrs["custom_meta"]["nested"].append("mutated")
    rsmp_a.attrs["supy_output_units"]["SUEWS"]["Rain"] = "m s-1"
    rsmp_a.attrs["supy_water_rate_columns"].append(("SUEWS", "fake"))
    rsmp_a.attrs["supy_output_daily_coverage_s"]["extra"] = 1.0
    rsmp_a.attrs["supy_output_coverage_s"]["extra"] = 2.0

    assert rsmp_b.attrs == snapshot_b
    assert df_in.attrs == source_snapshot

    # Verify the same isolation through SUEWSOutput.resample().
    out_a = output.resample("h")
    out_b = output.resample("h")
    df_out_a = out_a.df
    df_out_b = out_b.df
    out_b_snapshot = copy.deepcopy(df_out_b.attrs)
    df_out_a.attrs["supy_output_units"]["SUEWS"]["Rain"] = "m s-1"
    df_out_a.attrs["supy_output_daily_coverage_s"]["extra"] = 9.0

    assert out_b.df.attrs == out_b_snapshot
    assert output.df.attrs["supy_output_units"]["SUEWS"]["Rain"] == "mm h-1"


def test_with_daily_coverage_and_prepare_output_match_master(sample_outputs):
    """Unit conversion on native mm water-rate columns matches master exactly."""
    df_native = sample_outputs["2grids"].df.copy()
    df_native.attrs["custom_meta"] = {"tags": ["x"]}

    # Exercise with_daily_coverage from scratch when other attrs are present.
    df_no_cov = df_native.copy()
    df_no_cov.attrs = {"custom_meta": {"tags": ["x"]}}
    cov_new = with_daily_coverage(df_no_cov, interval_s=TSTEP_S)
    cov_master = MASTER_REF.with_daily_coverage(df_no_cov, interval_s=TSTEP_S)
    pd.testing.assert_frame_equal(cov_new, cov_master, check_exact=True)
    assert cov_new.attrs == cov_master.attrs
    assert cov_new.attrs["custom_meta"] is not df_no_cov.attrs["custom_meta"]

    # Exercise prepare_output touching water-rate columns (mm -> coverage),
    # daily water amount (P_day) and standard unit conversion (T2).
    attrs_before = copy.deepcopy(df_native.attrs)
    units_req = {"Rain": "mm h-1", "P_day": "m", "T2": "K"}
    prep_new = prepare_output(
        df_native, units=units_req, interval_s=TSTEP_S, write=True
    )
    prep_master = MASTER_REF.prepare_output(
        df_native, units=units_req, interval_s=TSTEP_S, write=True
    )
    prep_second = prepare_output(
        df_native, units=units_req, interval_s=TSTEP_S, write=True
    )

    pd.testing.assert_frame_equal(prep_new, prep_master, check_exact=True)
    assert prep_new.attrs == prep_master.attrs
    assert df_native.attrs == attrs_before

    # Verify independent ownership across returned prepare_output frames.
    snapshot_second = copy.deepcopy(prep_second.attrs)
    prep_new.attrs["custom_meta"]["tags"].append("mutated")
    prep_new.attrs["supy_output_units"]["SUEWS"]["Rain"] = "m s-1"
    prep_new.attrs["supy_water_rate_columns"].append(("SUEWS", "fake"))
    prep_new.attrs["supy_output_daily_coverage_s"]["extra"] = 1.0
    assert prep_second.attrs == snapshot_second
    assert df_native.attrs == attrs_before


@pytest.mark.parametrize(
    ("case", "freq_s", "timestamp_reference"),
    [
        ("1grid", TSTEP_S, "follow"),
        ("1grid", 3600, "follow"),
        ("2grids", TSTEP_S, "follow"),
        ("2grids", 3600, "follow"),
        ("2grids", 3600, "local_standard_time"),
    ],
    ids=[
        "1grid-300s-follow",
        "1grid-3600s-follow",
        "2grids-300s-follow",
        "2grids-3600s-follow",
        "2grids-3600s-lst",
    ],
)
def test_save_text_and_streamed_parquet_match_master(
    sample_outputs, tmp_path, monkeypatch, case, freq_s, timestamp_reference
):
    """Full text and streamed Parquet save paths match master byte for byte."""
    output = sample_outputs[case]
    df_in = output.df.copy()
    df_in.attrs["custom_meta"] = {"case": case}
    attrs_before = copy.deepcopy(df_in.attrs)

    dir_txt_new = tmp_path / "txt_new"
    dir_txt_master = tmp_path / "txt_master"
    dir_pq_new = tmp_path / "pq_new"
    dir_pq_master = tmp_path / "pq_master"
    dir_txt_new.mkdir()
    dir_txt_master.mkdir()

    # Run new code first.
    save_df_output(
        df_in,
        freq_s=freq_s,
        path_dir_save=dir_txt_new,
        output_groups=["SUEWS", "DailyState"],
        timestamp_reference=timestamp_reference,
        df_state_final=output.state_final,
    )
    _write_streamed_parquet(df_in, output.state_final, dir_pq_new, freq_s)
    assert df_in.attrs == attrs_before

    # Run verbatim master code with monkeypatched module bindings.
    with monkeypatch.context() as mp:
        _patch_master(mp)
        save_df_output(
            df_in,
            freq_s=freq_s,
            path_dir_save=dir_txt_master,
            output_groups=["SUEWS", "DailyState"],
            timestamp_reference=timestamp_reference,
            df_state_final=output.state_final,
        )
        _write_streamed_parquet(df_in, output.state_final, dir_pq_master, freq_s)
    assert df_in.attrs == attrs_before

    # Text outputs and sidecars: identical file sets and identical bytes.
    txt_names_new = sorted(p.name for p in dir_txt_new.iterdir())
    txt_names_master = sorted(p.name for p in dir_txt_master.iterdir())
    assert txt_names_new == txt_names_master
    for name in txt_names_new:
        assert (dir_txt_new / name).read_bytes() == (
            dir_txt_master / name
        ).read_bytes(), name

    # Streamed Parquet outputs: identical file sets, exact tables, attrs and schemas.
    pq_names_new = sorted(p.name for p in dir_pq_new.iterdir())
    pq_names_master = sorted(p.name for p in dir_pq_master.iterdir())
    assert pq_names_new == pq_names_master

    parts_new = sorted(
        p.name for p in (dir_pq_new / "SUEWS_output.parquet").iterdir()
    )
    parts_master = sorted(
        p.name for p in (dir_pq_master / "SUEWS_output.parquet").iterdir()
    )
    assert parts_new == parts_master

    df_pq_new = pd.read_parquet(dir_pq_new / "SUEWS_output.parquet")
    df_pq_master = pd.read_parquet(dir_pq_master / "SUEWS_output.parquet")
    pd.testing.assert_frame_equal(df_pq_new, df_pq_master, check_exact=True)
    assert df_pq_new.attrs == df_pq_master.attrs

    for part_name in parts_new:
        schema_new = pq.read_schema(dir_pq_new / "SUEWS_output.parquet" / part_name)
        schema_master = pq.read_schema(
            dir_pq_master / "SUEWS_output.parquet" / part_name
        )
        assert set(schema_new.metadata) == set(schema_master.metadata)
        for key in schema_new.metadata:
            if key in (b"PANDAS_ATTRS", b"pandas"):
                assert json.loads(schema_new.metadata[key]) == json.loads(
                    schema_master.metadata[key]
                )
            else:
                assert schema_new.metadata[key] == schema_master.metadata[key]
        assert [field.metadata for field in schema_new] == [
            field.metadata for field in schema_master
        ]

    # Metadata sidecar matches in every field except its wall-clock creation_time.
    meta_new = pd.read_parquet(dir_pq_new / "SUEWS_metadata.parquet").drop(
        columns=["creation_time"]
    )
    meta_master = pd.read_parquet(dir_pq_master / "SUEWS_metadata.parquet").drop(
        columns=["creation_time"]
    )
    pd.testing.assert_frame_equal(meta_new, meta_master, check_exact=True)


class _CopyTracker:
    """Payload whose ``__deepcopy__`` records every copy made by any binding."""

    def __init__(self, log: list[str], value: int = 1) -> None:
        self.log = log
        self.value = value

    def __deepcopy__(self, memo):
        self.log.append("deepcopy")
        return _CopyTracker(self.log, self.value)

    def __eq__(self, other):
        return isinstance(other, _CopyTracker) and self.value == other.value


def test_strip_attrs_avoids_deepcopy_and_leaves_source_untouched():
    """``strip_attrs`` does zero deep copies; boundaries copy the payload once."""
    index = pd.MultiIndex.from_product(
        [[1, 2], pd.date_range("2012-01-01 00:05", periods=4, freq="5min")],
        names=["grid", "datetime"],
    )
    columns = pd.MultiIndex.from_tuples(
        [("SUEWS", "QH"), ("SUEWS", "Rain")], names=["group", "var"]
    )
    df = pd.DataFrame(1.0, index=index, columns=columns)
    log: list[str] = []
    tracker = _CopyTracker(log, value=7)
    df.attrs = {
        "supy_output_daily_coverage_s": {
            f"1|2012-{day:03d}": 86400.0 for day in range(1, 367)
        },
        "custom_meta": {"nested": [1, 2, 3]},
        "tracker": tracker,
    }

    bare = strip_attrs(df)

    assert len(log) == 0
    assert bare.attrs == {}
    assert df.attrs["tracker"] is tracker
    assert bare.index.equals(df.index)
    assert bare.columns.equals(df.columns)
    pd.testing.assert_frame_equal(bare, df, check_exact=True)

    # Confirm the tracker is live: boundary operations copy the payload once.
    # The frame comparison above copies attrs through pandas itself, so reset.
    log.clear()
    prep = prepare_output(df, units={"Rain": "mm h-1"}, interval_s=TSTEP_S)
    assert len(log) == 1
    assert prep.attrs["tracker"] == tracker
    assert prep.attrs["tracker"] is not tracker

    log.clear()
    rsmp = _resample_output(df, freq=pd.Timedelta(3600, "s"))
    assert len(log) == 1
    assert rsmp.attrs["tracker"] == tracker
    assert rsmp.attrs["tracker"] is not tracker


def test_allows_duplicate_labels_flag_matches_master(sample_outputs):
    """``flags.allows_duplicate_labels=False`` matches master across prep and resample."""
    df_in = sample_outputs["1grid"].df.copy()
    df_in.flags.allows_duplicate_labels = False

    bare = strip_attrs(df_in)
    assert bare.flags.allows_duplicate_labels is False
    assert df_in.flags.allows_duplicate_labels is False

    units_req = {"Rain": "mm h-1", "T2": "K"}
    prep_new = prepare_output(
        df_in, units=units_req, interval_s=TSTEP_S, write=True
    )
    prep_master = MASTER_REF.prepare_output(
        df_in, units=units_req, interval_s=TSTEP_S, write=True
    )
    pd.testing.assert_frame_equal(prep_new, prep_master, check_exact=True)
    assert prep_new.attrs == prep_master.attrs
    assert (
        prep_new.flags.allows_duplicate_labels
        is prep_master.flags.allows_duplicate_labels
    )
    assert prep_new.flags.allows_duplicate_labels is False

    rsmp_new = _resample_output(df_in, freq=pd.Timedelta(3600, "s"))
    rsmp_master = MASTER_REF._resample_output(df_in, freq=pd.Timedelta(3600, "s"))
    pd.testing.assert_frame_equal(rsmp_new, rsmp_master, check_exact=True)
    assert rsmp_new.attrs == rsmp_master.attrs
    assert (
        rsmp_new.flags.allows_duplicate_labels
        is rsmp_master.flags.allows_duplicate_labels
    )


def test_resample_custom_callable_reads_attrs_like_master(sample_outputs):
    """User-supplied agg callables still see ``Series.attrs`` as on master."""
    df_in = sample_outputs["1grid"].df.loc[:, ["SUEWS"]].copy()
    df_in.attrs["scale"] = 2.5
    custom_aggm = {
        "SUEWS": {
            "QH": lambda ser: ser.mean() * ser.attrs["scale"],
            "Rain": "sum",
        }
    }

    actual = _resample_output(
        df_in, freq=pd.Timedelta(3600, "s"), dict_aggm=custom_aggm
    )
    expected = MASTER_REF._resample_output(
        df_in, freq=pd.Timedelta(3600, "s"), dict_aggm=custom_aggm
    )

    pd.testing.assert_frame_equal(actual, expected, check_exact=True)
    assert actual.attrs == expected.attrs
    assert actual.attrs["scale"] == 2.5
    assert not actual[("SUEWS", "QH")].isna().all()


def test_resample_mutated_default_dict_var_aggm_matches_master(
    sample_outputs, monkeypatch
):
    """Mutating ``dict_var_aggm`` in place with a callable still matches master."""
    df_in = sample_outputs["1grid"].df.loc[:, ["SUEWS"]].copy()
    df_in.attrs["scale"] = 3.0
    monkeypatch.setitem(
        post_mod.dict_var_aggm["SUEWS"],
        "T2",
        lambda ser: ser.mean() * ser.attrs["scale"],
    )

    actual = _resample_output(df_in, freq=pd.Timedelta(3600, "s"))
    expected = MASTER_REF._resample_output(
        df_in, freq=pd.Timedelta(3600, "s"), dict_aggm=post_mod.dict_var_aggm
    )

    pd.testing.assert_frame_equal(actual, expected, check_exact=True)
    assert actual.attrs == expected.attrs
    assert not actual[("SUEWS", "T2")].isna().all()


def test_resample_default_and_builtin_rules_take_stripped_path(
    sample_outputs, monkeypatch
):
    """Default, registry-built and string-only rules take the stripped path."""
    df_in = sample_outputs["1grid"].df.copy()
    strip_calls = 0
    real_strip_attrs = post_mod.strip_attrs

    def counting_strip(df):
        nonlocal strip_calls
        strip_calls += 1
        return real_strip_attrs(df)

    monkeypatch.setattr(post_mod, "strip_attrs", counting_strip)

    # Default dict_var_aggm uses the stripped fast path.
    _resample_output(df_in, freq=pd.Timedelta(3600, "s"))
    assert strip_calls == 1

    # Fresh registry aggregation rules also use the stripped fast path.
    _resample_output(
        df_in,
        freq=pd.Timedelta(3600, "s"),
        dict_aggm=OUTPUT_REGISTRY.get_aggregation_rules(),
    )
    assert strip_calls == 2

    # String-only custom rules also use the stripped fast path.
    _resample_output(
        df_in,
        freq=pd.Timedelta(3600, "s"),
        dict_aggm={"SUEWS": {"QH": "mean", "Rain": "sum"}},
    )
    assert strip_calls == 3

    # User-supplied callable skips stripping so attrs stay visible to it.
    _resample_output(
        df_in,
        freq=pd.Timedelta(3600, "s"),
        dict_aggm={"SUEWS": {"QH": lambda ser: ser.mean()}},
    )
    assert strip_calls == 3
