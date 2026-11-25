"""Bit-identical continuation and checkpoint regression tests for STEBBS (gh#2107).

Verifies all three layers of the STEBBS restart fix across both coupled
(``storage_heat_method = 7``) and uncoupled (``storage_heat_method = 1``)
configurations over the 48-hour STEBBS test window (2017-08-26 to 2017-08-27):

1. Full ``STEBBS_STATE%buildings(:)`` serialisation across Fortran C-API,
   Rust bridge, and Python JSON checkpoints (``stebbs_state`` schema v3).
2. Sub-daily / inner-iteration rollback synchronisation between
   ``flagState%stebbs_bldg_init`` and ``stebbsState%buildings(:)``.
3. Preservation of evolved building temperatures across chunked runs,
   streamed output runs, and disk JSON ``from_checkpoint`` resumes without
   early ``SUEWS_cal_Qn`` unallocated reads or thermal rewinds.
4. Loud rejection of legacy v2 / incomplete STEBBS checkpoints (BR-06) and
   parity with the standalone ``suews-engine`` CLI binary.
"""

from __future__ import annotations

import copy
import importlib
import json
import math
from pathlib import Path
import subprocess

import numpy as np
import pandas as pd
import pyarrow as pa
from pyarrow import ipc
import pytest

from supy.cmd import rust_bridge
from supy.suews_checkpoint import SUEWSCheckpoint
from supy.suews_sim import SUEWSSimulation

pytestmark = [
    pytest.mark.physics,
    pytest.mark.rust,
    pytest.mark.contract("C-STEBBS-RESTART", "C-RESTART", "C-CODEC"),
]

STEBBS_FIXTURE_DIR = (
    Path(__file__).resolve().parents[1] / "fixtures" / "data_test" / "stebbs_test"
)
STEBBS_CONFIG_PATH = STEBBS_FIXTURE_DIR / "sample_config.yml"
TIMESTEPS_PER_DAY = 288
EXPECTED_STEBBS_COLUMNS = 82
SENTINEL_MISSING = -999.0


def _run_rust_module():
    """Load the internal Rust runner module dynamically."""
    return importlib.import_module("supy._run_rust")


def _load_stebbs_config_and_forcing(
    storage_heat_method: int,
) -> tuple[object, pd.DataFrame]:
    """Load the 48-hour STEBBS fixture with the requested storage-heat coupling."""
    base_sim = SUEWSSimulation(STEBBS_CONFIG_PATH)
    df_forcing = base_sim.forcing.df.loc["2017-08-26":"2017-08-27"].copy()
    # The fixture uses -999 as a dry-period sentinel; normalise for OOP validation.
    df_forcing["rain"] = df_forcing["rain"].clip(lower=0)
    assert len(df_forcing) == 2 * TIMESTEPS_PER_DAY

    physics_update: dict[str, object] = {
        "storage_heat": {"value": storage_heat_method},
    }
    if storage_heat_method == 1:
        # SPARTACUS (net_radiation=1003) requires a facet-resolved storage heat method
        # (EHC=5 or coupled STEBBS=7); uncoupled STEBBS (storage_heat=1) pairs with NARP (3).
        physics_update["net_radiation"] = {"value": 3}
    base_sim.update_config(
        {"model": {"physics": physics_update}},
        auto_load_forcing=False,
    )
    cfg = copy.deepcopy(base_sim.config)
    return cfg, df_forcing


def _run_window(
    sim: SUEWSSimulation,
    forcing: pd.DataFrame,
    **kwargs,
):
    """Run ``sim`` over the exact timestamp bounds of ``forcing``."""
    sim.update_forcing(forcing)
    return sim.run(
        start_date=forcing.index[0],
        end_date=forcing.index[-1],
        **kwargs,
    )


def _assert_bit_identical_groups(
    actual_df: pd.DataFrame,
    reference_df: pd.DataFrame,
    *,
    label: str,
) -> None:
    """Assert IEEE-754 bit-identical match and zero NaN/Inf across SUEWS and STEBBS."""
    assert actual_df.index.equals(reference_df.index), f"{label}: index mismatch"
    for group in ("STEBBS", "SUEWS"):
        ref_grp = reference_df[group]
        act_grp = actual_df[group]
        assert list(act_grp.columns) == list(ref_grp.columns), (
            f"{label}: {group} columns diverged"
        )
        if group == "STEBBS":
            assert ref_grp.shape[1] == EXPECTED_STEBBS_COLUMNS

        act_vals = act_grp.to_numpy(dtype=np.float64, copy=False)
        ref_vals = ref_grp.to_numpy(dtype=np.float64, copy=False)
        if group == "STEBBS":
            assert np.isfinite(ref_vals).all(), (
                f"{label}: continuous reference contains non-finite STEBBS values"
            )
            assert np.isfinite(act_vals).all(), (
                f"{label}: resumed/chunked run contains non-finite STEBBS values"
            )
        else:
            assert np.array_equal(np.isnan(act_vals), np.isnan(ref_vals)), (
                f"{label}: SUEWS NaN mask diverged"
            )
            assert np.array_equal(np.isinf(act_vals), np.isinf(ref_vals)), (
                f"{label}: SUEWS Inf mask diverged"
            )

        bit_mismatches = int(
            np.count_nonzero(act_vals.view(np.uint64) != ref_vals.view(np.uint64))
        )
        assert bit_mismatches == 0, (
            f"{label}: {group} has {bit_mismatches} IEEE-754 bit mismatches"
        )
        pd.testing.assert_frame_equal(
            act_grp,
            ref_grp,
            check_exact=True,
            check_dtype=True,
        )


def _assert_checkpoints_identical(
    actual_ckpt: SUEWSCheckpoint,
    reference_ckpt: SUEWSCheckpoint,
    *,
    label: str,
) -> None:
    """Assert identical timer, schema v3 stebbs_state dims, and full JSON state payloads."""
    assert actual_ckpt is not None and reference_ckpt is not None
    assert actual_ckpt.grid_states.keys() == reference_ckpt.grid_states.keys()
    for grid_id in reference_ckpt.grid_states:
        ref_payload = json.loads(reference_ckpt.grid_states[grid_id])
        act_payload = json.loads(actual_ckpt.grid_states[grid_id])
        assert act_payload["timer"] == ref_payload["timer"], (
            f"{label}: timer mismatch for grid {grid_id}"
        )
        stebbs_member = act_payload["state"]["members"]["stebbs_state"]
        assert stebbs_member["schema_version"] == 3
        assert stebbs_member["dims"]["buildings"] == [1]
        assert stebbs_member["dims"]["bldg_nlayer"][0] >= 1
        assert act_payload == ref_payload, (
            f"{label}: final checkpoint JSON payload diverged for grid {grid_id}"
        )


def _write_forcing_text_kpa(forcing: pd.DataFrame, forcing_file: Path) -> None:
    """Write cleaned forcing rows using the kPa pressure convention of raw forcing files."""
    cols = [c for c in forcing.columns if c != "isec"]
    with forcing_file.open("w", encoding="utf-8", newline="\n") as fh:
        fh.write(" ".join(cols) + "\n")
        for row in forcing[cols].itertuples(index=False, name=None):
            formatted = []
            for col, val in zip(cols, row, strict=True):
                if col in {"iy", "id", "it", "imin"}:
                    formatted.append(str(int(val)))
                elif col == "pres" and not math.isclose(
                    float(val), SENTINEL_MISSING, rel_tol=0.0, abs_tol=0.0
                ):
                    formatted.append(repr(float(val) / 10.0))
                else:
                    formatted.append(repr(float(val)))
            fh.write(" ".join(formatted) + "\n")


def _assert_engine_arrow_matches(arrow_path: Path, ref_grid: pd.DataFrame) -> None:
    """Verify standalone suews-engine Arrow columns match Python SUEWS+STEBBS."""
    with pa.memory_map(str(arrow_path), "rb") as source:
        reader = ipc.open_file(source)
        table = reader.read_all()
        schema_names = set(reader.schema.names)
        # SUEWS columns use named Arrow fields; non-SUEWS groups use `{group}_{idx}`
        # with indices 0..4 reserved for the 5 datetime columns (iy, id, it, imin, dectime).
        for group in ("STEBBS", "SUEWS"):
            ref_grp = ref_grid[group]
            for col_idx, col in enumerate(ref_grp.columns):
                arrow_col = col if group == "SUEWS" else f"STEBBS_{col_idx + 5}"
                assert arrow_col in schema_names, (
                    f"missing {group}.{col} ({arrow_col}) in Arrow output"
                )
                cli_vals = table.column(arrow_col).to_numpy(zero_copy_only=False).copy()
                py_vals = ref_grp[col].to_numpy(dtype=np.float64, copy=False)
                assert cli_vals.shape == py_vals.shape
                if group == "STEBBS":
                    assert np.isfinite(cli_vals).all(), (
                        f"suews-engine STEBBS.{col} contains non-finite values"
                    )
                    np.testing.assert_allclose(
                        cli_vals,
                        py_vals,
                        rtol=1e-10,
                        atol=1e-10,
                        err_msg=f"suews-engine STEBBS.{col} diverged from Python",
                    )
                else:
                    is_missing = np.isclose(
                        cli_vals, SENTINEL_MISSING, rtol=0.0, atol=0.0
                    )
                    cli_masked = np.where(is_missing, np.nan, cli_vals)
                    assert np.array_equal(np.isnan(cli_masked), np.isnan(py_vals)), (
                        f"suews-engine SUEWS.{col} NaN mask mismatch"
                    )
                    np.testing.assert_allclose(
                        cli_masked,
                        py_vals,
                        rtol=1e-10,
                        atol=1e-10,
                        equal_nan=True,
                        err_msg=f"suews-engine SUEWS.{col} diverged from Python",
                    )


@pytest.fixture(scope="module")
def stebbs_continuous_baselines():
    """Compute uninterrupted 48-hour STEBBS reference runs once per storage-heat mode."""
    baselines = {}
    for storage_heat_method in (7, 1):
        cfg, df_forcing = _load_stebbs_config_and_forcing(storage_heat_method)
        sim = SUEWSSimulation(copy.deepcopy(cfg))
        out = _run_window(sim, df_forcing)
        baselines[storage_heat_method] = {
            "config": cfg,
            "forcing": df_forcing,
            "sim": sim,
            "df": out.df,
            "checkpoint": sim.checkpoint,
        }
    return baselines


def _locate_engine() -> Path:
    """Locate the compiled standalone suews-engine CLI binary or skip cleanly."""
    try:
        bundled = Path(rust_bridge._bridge_binary())
    except FileNotFoundError:
        bundled = None
    else:
        if bundled.is_file():
            return bundled

    repo_root = Path(__file__).resolve().parents[2]
    binary_name = "suews-engine.exe" if Path("/").anchor != "/" else "suews-engine"
    dev_binary = repo_root / "src" / "suews_bridge" / "target" / "release" / binary_name
    if dev_binary.is_file():
        return dev_binary

    pytest.skip("suews-engine CLI binary not built; build with: make bridge")


@pytest.mark.core
@pytest.mark.parametrize(
    "storage_heat_method",
    [
        pytest.param(7, id="coupled_sh7", marks=pytest.mark.medium),
        pytest.param(1, id="uncoupled_sh1"),
    ],
)
def test_stebbs_chunk_day_bit_identical(
    stebbs_continuous_baselines,
    storage_heat_method: int,
) -> None:
    """chunk_day=1 produces 0.0 bit-identical SUEWS+STEBBS outputs and checkpoint."""
    ref = stebbs_continuous_baselines[storage_heat_method]
    sim = SUEWSSimulation(copy.deepcopy(ref["config"]))
    out = _run_window(sim, ref["forcing"], chunk_day=1)

    _assert_bit_identical_groups(
        out.df,
        ref["df"],
        label=f"chunk_day=1 (sh={storage_heat_method})",
    )
    _assert_checkpoints_identical(
        sim.checkpoint,
        ref["checkpoint"],
        label=f"chunk_day=1 (sh={storage_heat_method})",
    )


@pytest.mark.core
@pytest.mark.parametrize(
    "storage_heat_method",
    [
        pytest.param(7, id="coupled_sh7", marks=pytest.mark.medium),
        pytest.param(1, id="uncoupled_sh1"),
    ],
)
def test_stebbs_stream_chunk_bit_identical(
    stebbs_continuous_baselines,
    storage_heat_method: int,
    tmp_path: Path,
) -> None:
    """output.stream_chunk=1 produces 0.0 bit-identical streamed SUEWS+STEBBS outputs."""
    ref = stebbs_continuous_baselines[storage_heat_method]
    forcing = ref["forcing"]
    out_dir_stream = tmp_path / f"stream_sh{storage_heat_method}"
    out_dir_whole = tmp_path / f"whole_sh{storage_heat_method}"
    out_dir_stream.mkdir()
    out_dir_whole.mkdir()

    def _configure_output(
        sim_obj: SUEWSSimulation, out_dir: Path, *, stream: bool
    ) -> None:
        output_cfg: dict[str, object] = {
            "format": "parquet",
            "freq": 300,
            "dir": str(out_dir),
            "groups": ["SUEWS", "STEBBS"],
            "include_provisional": True,
        }
        if stream:
            output_cfg["stream_chunk"] = 1
        sim_obj.update_config(
            {
                "model": {
                    "control": {
                        "start_time": str(forcing.index[0]),
                        "end_time": str(forcing.index[-1]),
                        "output": output_cfg,
                    }
                }
            },
            auto_load_forcing=False,
        )
        sim_obj.update_forcing(forcing)

    # Save the already-computed continuous reference as Parquet to establish the
    # exact resampled output baseline without re-running the 48-hour simulation.
    sim_whole = copy.deepcopy(ref["sim"])
    sim_whole.update_config(
        {
            "model": {
                "control": {
                    "start_time": str(forcing.index[0]),
                    "end_time": str(forcing.index[-1]),
                    "output": {
                        "format": "parquet",
                        "freq": 300,
                        "dir": str(out_dir_whole),
                        "groups": ["SUEWS", "STEBBS"],
                        "include_provisional": True,
                    },
                }
            }
        },
        auto_load_forcing=False,
    )
    sim_whole.save(out_dir_whole)

    sim = SUEWSSimulation(copy.deepcopy(ref["config"]))
    _configure_output(sim, out_dir_stream, stream=True)
    sim.run(start_date=forcing.index[0], end_date=forcing.index[-1])

    whole_paths = sorted(out_dir_whole.glob("*SUEWS_output*.parquet"))
    stream_paths = sorted(out_dir_stream.glob("*SUEWS_output*.parquet"))
    assert len(whole_paths) == 1 and len(stream_paths) == 1
    whole_df = pd.read_parquet(whole_paths[0]).sort_index()
    streamed = pd.read_parquet(stream_paths[0]).sort_index()
    _assert_bit_identical_groups(
        streamed,
        whole_df,
        label=f"stream_chunk=1 vs whole parquet (sh={storage_heat_method})",
    )
    pd.testing.assert_frame_equal(
        streamed["STEBBS"],
        ref["df"]["STEBBS"],
        check_exact=True,
        check_dtype=True,
    )

    _assert_checkpoints_identical(
        sim.checkpoint,
        ref["checkpoint"],
        label=f"stream_chunk=1 (sh={storage_heat_method})",
    )


@pytest.mark.core
@pytest.mark.parametrize(
    "storage_heat_method",
    [
        pytest.param(7, id="coupled_sh7", marks=pytest.mark.medium),
        pytest.param(1, id="uncoupled_sh1"),
    ],
)
@pytest.mark.parametrize("split_step", [2, 144, 288])
def test_stebbs_disk_json_checkpoint_resume_bit_identical(
    stebbs_continuous_baselines,
    storage_heat_method: int,
    split_step: int,
    tmp_path: Path,
) -> None:
    """Disk JSON checkpoint save + from_checkpoint resume is 0.0 bit-identical."""
    ref = stebbs_continuous_baselines[storage_heat_method]
    forcing = ref["forcing"]

    sim1 = SUEWSSimulation(copy.deepcopy(ref["config"]))
    out1 = _run_window(sim1, forcing.iloc[:split_step])

    ckpt_path = sim1.checkpoint.to_file(
        tmp_path / f"stebbs_sh{storage_heat_method}_step{split_step}.json"
    )
    loaded = SUEWSCheckpoint.from_file(ckpt_path)

    # Confirm serialised JSON contains populated v3 STEBBS buildings state.
    grid_payload = json.loads(next(iter(loaded.grid_states.values())))
    stebbs_member = grid_payload["state"]["members"]["stebbs_state"]
    assert stebbs_member["schema_version"] == 3
    assert stebbs_member["dims"]["buildings"] == [1]
    assert len(stebbs_member["values"]) > 155

    sim2 = SUEWSSimulation.from_checkpoint(copy.deepcopy(ref["config"]), loaded)
    out2 = _run_window(sim2, forcing.iloc[split_step:])

    combined_df = pd.concat([out1.df, out2.df], axis=0)
    _assert_bit_identical_groups(
        combined_df,
        ref["df"],
        label=f"from_checkpoint(step={split_step}, sh={storage_heat_method})",
    )
    _assert_checkpoints_identical(
        sim2.checkpoint,
        ref["checkpoint"],
        label=f"from_checkpoint(step={split_step}, sh={storage_heat_method})",
    )


def _mutated_checkpoint(
    raw_v3: dict,
    grid_id: int,
    last_timestamp: str | None,
    stebbs_override: dict,
) -> tuple[dict, SUEWSCheckpoint]:
    """Build a mutated checkpoint copy with a replaced ``stebbs_state`` member."""
    mutated = copy.deepcopy(raw_v3)
    mutated["state"]["members"]["stebbs_state"] = stebbs_override
    ckpt = SUEWSCheckpoint.from_grid_states(
        {grid_id: json.dumps(mutated)},
        last_timestamp=last_timestamp,
    )
    return mutated, ckpt


def _two_step_v3_checkpoint(ref: dict) -> tuple[dict, str | None, int]:
    """Run two STEBBS steps and return ``(raw_v3_payload, last_timestamp, grid_id)``."""
    grid_id = ref["config"].sites[0].gridiv
    sim = SUEWSSimulation(copy.deepcopy(ref["config"]))
    _run_window(sim, ref["forcing"].iloc[:2])
    raw_v3 = json.loads(sim.checkpoint.grid_states[grid_id])
    return raw_v3, sim.checkpoint.last_timestamp, grid_id


@pytest.mark.core
def test_stebbs_old_v2_checkpoint_rejected(stebbs_continuous_baselines) -> None:
    """Old v2 STEBBS checkpoints fail loudly at both Python and Rust boundaries (BR-06)."""
    ref = stebbs_continuous_baselines[7]
    raw_v3, last_ts, grid_id = _two_step_v3_checkpoint(ref)
    base_values = raw_v3["state"]["members"]["stebbs_state"]["values"][:155]

    legacy_v2, legacy_ckpt = _mutated_checkpoint(
        raw_v3,
        grid_id,
        last_ts,
        {"schema_version": 2, "values": base_values},
    )
    with pytest.raises(ValueError, match=r"stebbs_state.*schema version"):
        legacy_ckpt.validate_for_continuation()
    with pytest.raises(ValueError, match=r"stebbs_state.*schema version"):
        SUEWSSimulation.from_checkpoint(copy.deepcopy(ref["config"]), legacy_ckpt)

    run_rust_mod = _run_rust_module()
    rust_bridge_mod = run_rust_mod._check_rust_available()
    cfg_yaml = run_rust_mod.config_to_engine_yaml(copy.deepcopy(ref["config"]))
    forcing_flat = (
        run_rust_mod
        ._prepare_forcing_block(ref["forcing"].iloc[2:4])
        .reshape(-1)
        .tolist()
    )
    with pytest.raises(
        RuntimeError, match=r"unsupported stebbs_state schema version 2"
    ):
        rust_bridge_mod.run_suews_with_state(
            cfg_yaml, forcing_flat, 2, json.dumps(legacy_v2)
        )


@pytest.mark.core
def test_stebbs_incomplete_v3_checkpoints_rejected(
    stebbs_continuous_baselines,
) -> None:
    """Incomplete or layer-mismatched v3 STEBBS checkpoints fail loudly (BR-06)."""
    ref = stebbs_continuous_baselines[7]
    forcing_next = ref["forcing"].iloc[2:4]
    raw_v3, last_ts, grid_id = _two_step_v3_checkpoint(ref)
    stebbs_v3 = raw_v3["state"]["members"]["stebbs_state"]

    # 1. v3 envelope with stebbs_bldg_init == 1 but empty buildings (0 buildings).
    empty_values = list(stebbs_v3["values"][:155])
    empty_values[153] = 0.0
    _, empty_ckpt = _mutated_checkpoint(
        raw_v3,
        grid_id,
        last_ts,
        {
            "schema_version": 3,
            "dims": {"buildings": [0], "bldg_nlayer": [0]},
            "values": empty_values,
        },
    )
    sim_bad = SUEWSSimulation.from_checkpoint(copy.deepcopy(ref["config"]), empty_ckpt)
    with pytest.raises(RuntimeError, match=r"STEBBS_STATE%buildings"):
        _run_window(sim_bad, forcing_next)

    # 2. v3 envelope with mismatched bldg_nlayer (config nlayer != checkpoint bldg_nlayer).
    old_nlayer = stebbs_v3["dims"]["bldg_nlayer"][0]
    stebbs_vals = list(stebbs_v3["values"])
    _, bad_nlayer_ckpt = _mutated_checkpoint(
        raw_v3,
        grid_id,
        last_ts,
        {
            "schema_version": 3,
            "dims": {"buildings": [1], "bldg_nlayer": [old_nlayer + 2]},
            "values": [*stebbs_vals[:-3], 20.0, 20.0, 20.0, 20.0, *stebbs_vals[-3:]],
        },
    )
    sim_bad_nl = SUEWSSimulation.from_checkpoint(
        copy.deepcopy(ref["config"]), bad_nlayer_ckpt
    )
    with pytest.raises(RuntimeError, match=r"bldg_nlayer"):
        _run_window(sim_bad_nl, forcing_next)


@pytest.mark.core
@pytest.mark.medium
def test_stebbs_standalone_engine_cli_parity(
    stebbs_continuous_baselines,
    tmp_path: Path,
) -> None:
    """Standalone suews-engine CLI matches Python continuous and chunked STEBBS."""
    engine = _locate_engine()
    ref = stebbs_continuous_baselines[7]
    forcing = ref["forcing"]
    grid_id = ref["config"].sites[0].gridiv

    run_dir = tmp_path / "engine_stebbs"
    run_dir.mkdir()
    forcing_file = run_dir / "stebbs_forcing_48h.txt"
    _write_forcing_text_kpa(forcing, forcing_file)

    cfg = copy.deepcopy(ref["config"])
    cfg.model.control.forcing.file = forcing_file.name
    cfg.model.control.output.dir = str(run_dir)
    cfg.model.control.start_time = str(forcing.index[0])
    cfg.model.control.end_time = str(forcing.index[-1])
    cfg_path = run_dir / "stebbs_resolved.yml"
    cfg_path.write_text(
        _run_rust_module().config_to_engine_yaml(cfg),
        encoding="utf-8",
    )

    # Nightly checked builds (-O0 -fcheck=all) need ~120-145 s for the 48-hour
    # coupled STEBBS+SPARTACUS run under xdist worker contention (gh#2307).
    result = subprocess.run(
        [str(engine), "run", str(cfg_path)],
        capture_output=True,
        text=True,
        timeout=300,
        check=False,
    )
    assert result.returncode == 0, (
        f"suews-engine failed ({result.returncode}): {result.stderr}"
    )

    arrow_path = run_dir / "suews_output.arrow"
    assert arrow_path.is_file(), "suews-engine did not produce suews_output.arrow"
    _assert_engine_arrow_matches(arrow_path, ref["df"].xs(grid_id, level="grid"))
