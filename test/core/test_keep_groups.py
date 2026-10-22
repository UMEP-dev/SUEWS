"""Tests for the opt-in ``run(keep_groups=...)`` prototype (gh#1849).

The contract: the kept groups' columns, labels and index equal those of a
run without ``keep_groups`` at the same ``chunk_day``, exactly; dropped
groups are absent; the default path is untouched.
"""

from pathlib import Path

import pandas as pd
import pytest

pytestmark = pytest.mark.api

TIMESTEPS_PER_DAY = 288


def _run_loaded(sim, **kwargs):
    """Run exactly the forcing loaded on ``sim`` (see test_checkpoint)."""
    index = sim.forcing.index
    return sim.run(start_date=index[0], end_date=index[-1], **kwargs)


def _sim_with_forcing(short_sample_sim, n_rows, list_gridiv=None):
    """Short-sample simulation on its first *n_rows* forcing rows.

    With *list_gridiv*, the site is replicated once per entry with that
    ``gridiv``, in the given order.
    """
    sim = short_sample_sim()
    forcing = sim.forcing.df.iloc[:n_rows]
    if list_gridiv is not None:
        config = sim.config.model_copy(deep=True)
        site0 = config.sites[0]
        list_site = []
        for gridiv in list_gridiv:
            site = site0.model_copy(deep=True)
            site.gridiv = gridiv
            site.name = f"grid{gridiv}"
            list_site.append(site)
        config.sites = list_site
        sim.update_config(config, auto_load_forcing=False)
    sim.update_forcing(forcing)
    return sim


def _assert_kept_equal(df_kept, df_full, list_group):
    """The kept frame equals the full frame restricted to *list_group*."""
    assert df_kept.columns.get_level_values("group").unique().tolist() == list_group
    pd.testing.assert_frame_equal(
        df_kept,
        df_full.loc[:, df_full.columns.get_level_values("group").isin(list_group)],
        check_exact=True,
    )


@pytest.mark.parametrize("chunk_day", [3660, 1])
def test_keep_groups_equals_full_run(short_sample_sim, chunk_day):
    """Kept columns equal a full run at the same chunk_day, bit for bit."""
    n_rows = 2 * TIMESTEPS_PER_DAY + 12
    output_full = _run_loaded(
        _sim_with_forcing(short_sample_sim, n_rows), chunk_day=chunk_day, n_jobs=1
    )
    # Unordered and mixed-case names come back in kernel order and spelling.
    output_kept = _run_loaded(
        _sim_with_forcing(short_sample_sim, n_rows),
        chunk_day=chunk_day,
        n_jobs=1,
        keep_groups=["debug", "suews", "DailyState"],
    )
    _assert_kept_equal(output_kept.df, output_full.df, ["SUEWS", "DailyState", "debug"])
    # The final model state, and so any continuation, is unaffected.
    assert output_kept.checkpoint.grid_states == output_full.checkpoint.grid_states


@pytest.mark.parametrize("chunk_day", [3660, 1])
def test_keep_groups_two_grids(short_sample_sim, chunk_day):
    """Two grids listed out of ID order, run in parallel: same frame."""
    n_rows = TIMESTEPS_PER_DAY + 24
    output_full = _run_loaded(
        _sim_with_forcing(short_sample_sim, n_rows, list_gridiv=[2, 1]),
        chunk_day=chunk_day,
    )
    output_kept = _run_loaded(
        _sim_with_forcing(short_sample_sim, n_rows, list_gridiv=[2, 1]),
        chunk_day=chunk_day,
        keep_groups=["SUEWS", "RSL"],
    )
    assert output_kept.grids == [1, 2]
    _assert_kept_equal(output_kept.df, output_full.df, ["SUEWS", "RSL"])


def test_keep_groups_save_matches_full_run(short_sample_sim, tmp_path):
    """Saved txt files are byte-identical to those of a full run."""
    n_rows = 2 * TIMESTEPS_PER_DAY
    dict_bytes = {}
    for label, run_kw in [("full", {}), ("kept", {"keep_groups": ["SUEWS"]})]:
        sim = _sim_with_forcing(short_sample_sim, n_rows)
        _run_loaded(sim, n_jobs=1, **run_kw)
        list_path = sim.save(tmp_path / label)
        dict_bytes[label] = {
            Path(p).name: Path(p).read_bytes()
            for p in list_path
            if Path(p).suffix == ".txt"
        }
    assert dict_bytes["kept"]
    assert dict_bytes["kept"] == dict_bytes["full"]


def test_keep_groups_dropped_group_access_raises(short_sample_sim):
    """Dropped groups and their variables are reported as not found."""
    df_full = _run_loaded(_sim_with_forcing(short_sample_sim, 24), n_jobs=1).df
    output = _run_loaded(
        _sim_with_forcing(short_sample_sim, 24), n_jobs=1, keep_groups=["SUEWS"]
    )
    assert output.groups == ["SUEWS"]
    assert output.get_group("SUEWS").shape[0] == 24
    with pytest.raises(ValueError, match="not found"):
        output.get_group("RSL")
    var_rsl_only = next(
        var for var in df_full["RSL"].columns if var not in df_full["SUEWS"].columns
    )
    with pytest.raises((KeyError, ValueError)):
        output.get_variable(var_rsl_only)


@pytest.mark.parametrize(
    ("keep_groups", "match"),
    [
        (["SUEWS", "NoSuchGroup"], "unknown output groups"),
        ([], "at least one"),
        (["RSL"], "would drop"),
    ],
)
def test_keep_groups_rejected_before_run(short_sample_sim, keep_groups, match):
    """Unknown, empty, and save-incompatible selections fail up front.

    The short sample saves SUEWS, so keeping only RSL would make ``save()``
    write nothing for SUEWS.
    """
    sim = _sim_with_forcing(short_sample_sim, 12)
    with pytest.raises(ValueError, match=match):
        _run_loaded(sim, n_jobs=1, keep_groups=keep_groups)
    assert sim._df_output is None


def test_keep_groups_recorded_in_run_metadata(short_sample_sim):
    """The kept groups are recorded with the run's provenance metadata."""
    sim = _sim_with_forcing(short_sample_sim, 12)
    _run_loaded(sim, n_jobs=1, keep_groups=["SUEWS"])
    assert sim._run_metadata["keep_groups"] == ["SUEWS"]

    sim_default = _sim_with_forcing(short_sample_sim, 12)
    _run_loaded(sim_default, n_jobs=1)
    assert "keep_groups" not in sim_default._run_metadata
