"""Cross-route file-run period enforcement regressions for ``suews-engine run``."""

from __future__ import annotations

from datetime import datetime, timedelta
from importlib import import_module
from importlib.resources import as_file, files
import os
from pathlib import Path
import subprocess

from click.testing import CliRunner
import numpy as np
import pandas as pd
import pyarrow as pa
from pyarrow import ipc
import pytest
import yaml

from supy import SUEWSSimulation
from supy.cmd.suews_cli import cli
from supy.data_model import SUEWSConfig

_run_rust = import_module("supy._run_rust")

pytestmark = [
    pytest.mark.physics,
    pytest.mark.contract("C-CLOCK", "C-FORCING", "C-STRICT", "C-PARITY"),
]

CHECK_VARS = ("QH", "QE", "QS", "QN", "QF", "Tsurf")


def _locate_suews_engine() -> Path:
    """Return the bundled or locally built ``suews-engine`` binary."""
    binary_name = "suews-engine.exe" if os.name == "nt" else "suews-engine"
    bundled = files("supy").joinpath("bin").joinpath(binary_name)
    with as_file(bundled) as bundled_path:
        if bundled_path.exists():
            return Path(bundled_path)

    repo_root = Path(__file__).resolve().parents[2]
    dev_binary = repo_root / "src" / "suews_bridge" / "target" / "release" / binary_name
    if dev_binary.exists():
        return dev_binary
    pytest.skip("Rust CLI binary not found; build with: make bridge")


def _load_sample_config_dict() -> dict:
    sample_dir = files("supy").joinpath("sample_data")
    with sample_dir.joinpath("sample_config.yml").open(encoding="utf-8") as handle:
        return yaml.safe_load(handle)


def _sample_forcing_lines() -> list[str]:
    sample_dir = files("supy").joinpath("sample_data")
    raw = sample_dir.joinpath("Kc_2012_data_60.txt").read_text(encoding="utf-8")
    return [line for line in raw.splitlines() if line.strip()]


def _write_hourly_and_5min_forcing(
    work_dir: Path, start_row: int = 1, hours: int = 48
) -> dict[str, Path]:
    """Write paired hourly (60-min) and 5-minute forcing files spanning ``hours``."""
    lines = _sample_forcing_lines()
    hourly_path = work_dir / "weather_60.txt"
    hourly_path.write_text(
        "\n".join([lines[0], *lines[start_row : start_row + hours]]) + "\n",
        encoding="utf-8",
    )

    # Produce the exact 5-minute forcing series through SuPy's loader so both
    # routes read the identical 5-minute file on the forcing clock.
    cfg = _load_sample_config_dict()
    control = cfg["model"]["control"]
    control["forcing"]["file"] = hourly_path.name
    control["start_time"] = None
    control["end_time"] = None
    bootstrap_path = work_dir / "_bootstrap.yml"
    bootstrap_path.write_text(yaml.safe_dump(cfg, sort_keys=False), encoding="utf-8")
    df5 = SUEWSSimulation(bootstrap_path).forcing.df.copy()
    bootstrap_path.unlink()

    header = lines[0].split()
    col_map = {c.lower(): c for c in df5.columns}
    out_lines = [" ".join(header)]
    for _, row in df5.iterrows():
        vals: list[str] = []
        for col in header:
            low = col.lower()
            if low in {"iy", "id", "it", "imin"}:
                vals.append(str(round(float(row[col_map[low]]))))
            elif low in col_map:
                v = float(row[col_map[low]])
                if low == "pres" and np.isfinite(v) and v > -900.0:
                    # Forcing files store pressure in kPa; in-memory df5 holds hPa.
                    v /= 10.0
                vals.append("-999" if not np.isfinite(v) or v <= -900.0 else f"{v:.6f}")
            else:
                vals.append("-999")
        out_lines.append(" ".join(vals))

    fivemin_path = work_dir / "weather_5.txt"
    fivemin_path.write_text("\n".join(out_lines) + "\n", encoding="utf-8")
    return {"60": hourly_path, "5": fivemin_path}


@pytest.fixture(scope="module")
def paired_48h_forcing(tmp_path_factory: pytest.TempPathFactory) -> dict[str, Path]:
    """Generate the shared 48-hour hourly and 5-minute forcing files once."""
    return _write_hourly_and_5min_forcing(tmp_path_factory.mktemp("forcing48h"))


def _copy_forcing(src: Path, dest_dir: Path) -> str:
    dest = dest_dir / src.name
    dest.write_text(src.read_text(encoding="utf-8"), encoding="utf-8")
    return dest.name


def _read_engine_arrow(arrow_path: Path, variables=CHECK_VARS) -> pd.DataFrame:
    """Read timestamp columns and selected SUEWS fluxes from Arrow IPC output."""
    cols = ["iy", "id", "it", "imin", *variables]
    with pa.memory_map(str(arrow_path), "rb") as source:
        reader = ipc.open_file(source)
        table = reader.read_all().select(cols)
        data = {
            name: table.column(name).to_numpy(zero_copy_only=False).copy()
            for name in cols
        }
        del table, reader

    timestamps = [
        datetime(int(iy), 1, 1)
        + timedelta(days=int(doy) - 1, hours=int(it), minutes=int(imin))
        for iy, doy, it, imin in zip(
            data["iy"], data["id"], data["it"], data["imin"], strict=True
        )
    ]
    return pd.DataFrame(
        {var: data[var] for var in variables},
        index=pd.DatetimeIndex(timestamps, name="datetime"),
    )


def _write_case_configs(
    case_dir: Path,
    forcing_name: str,
    *,
    start_time: str | None,
    end_time: str | None,
    timestamp_reference: str = "local_standard_time",
    timezone: float | None = None,
    output_dir: Path | None = None,
) -> tuple[Path, Path, Path]:
    """Write source YAML and resolved engine YAML in ``case_dir``."""
    cfg = _load_sample_config_dict()
    control = cfg["model"]["control"]
    control["forcing"]["file"] = forcing_name
    control["forcing"]["timestamp_reference"] = timestamp_reference
    control["start_time"] = start_time
    control["end_time"] = end_time
    control["output"]["freq"] = 300
    out_dir = output_dir if output_dir is not None else case_dir / "out"
    control["output"]["dir"] = str(out_dir)
    if timezone is not None:
        cfg["sites"][0]["properties"]["timezone"] = {"value": timezone}

    source_path = case_dir / "source.yml"
    source_path.write_text(yaml.safe_dump(cfg, sort_keys=False), encoding="utf-8")

    resolved_path = case_dir / "resolved.yml"
    resolved_path.write_text(
        _run_rust.config_to_engine_yaml(SUEWSConfig.from_dict(cfg)),
        encoding="utf-8",
    )
    return source_path, resolved_path, out_dir


def _run_engine_cli(binary: Path, resolved_path: Path) -> subprocess.CompletedProcess:
    return subprocess.run(
        [str(binary), "run", str(resolved_path)],
        capture_output=True,
        text=True,
        timeout=120,
        check=False,
    )


@pytest.mark.e2e
@pytest.mark.parametrize("resolution", ["60", "5"])
@pytest.mark.parametrize(
    (
        "case_name",
        "start_time",
        "end_time",
        "expected_rows",
        "expected_first",
        "expected_last",
    ),
    [
        ("unbounded", None, None, 576, "2012-01-01 00:05:00", "2012-01-03 00:00:00"),
        (
            "right_edge",
            "2012-01-01",
            "2012-01-01 12:00",
            144,
            "2012-01-01 00:05:00",
            "2012-01-01 12:00:00",
        ),
        (
            "inside_late_start",
            "2012-01-01 12:05",
            "2012-01-02 12:00",
            288,
            "2012-01-01 12:05:00",
            "2012-01-02 12:00:00",
        ),
        (
            "between_rows",
            "2012-01-01 06:03",
            "2012-01-01 18:02",
            144,
            "2012-01-01 06:05:00",
            "2012-01-01 18:00:00",
        ),
        (
            "whole_second_day",
            "2012-01-02",
            "2012-01-02",
            288,
            "2012-01-02 00:05:00",
            "2012-01-03 00:00:00",
        ),
        (
            "start_only",
            "2012-01-02 06:05",
            None,
            216,
            "2012-01-02 06:05:00",
            "2012-01-03 00:00:00",
        ),
        (
            "end_only",
            None,
            "2012-01-01 12:00",
            144,
            "2012-01-01 00:05:00",
            "2012-01-01 12:00:00",
        ),
        (
            "slash_explicit_timestamps",
            "2012/01/01 06:05",
            "2012/01/01 18:00",
            144,
            "2012-01-01 06:05:00",
            "2012-01-01 18:00:00",
        ),
        (
            "unpadded_date_only",
            "2012-1-2",
            "2012-1-2",
            288,
            "2012-01-02 00:05:00",
            "2012-01-03 00:00:00",
        ),
    ],
)
def test_engine_and_public_routes_match_across_period_cases(
    tmp_path: Path,
    paired_48h_forcing: dict[str, Path],
    resolution: str,
    case_name: str,
    start_time: str | None,
    end_time: str | None,
    expected_rows: int,
    expected_first: str,
    expected_last: str,
):
    """Engine and Python routes select identical timestamps and fluxes across a 48h span."""
    binary = _locate_suews_engine()
    forcing_name = _copy_forcing(paired_48h_forcing[resolution], tmp_path)

    source_path, resolved_path, out_dir = _write_case_configs(
        tmp_path,
        forcing_name,
        start_time=start_time,
        end_time=end_time,
        output_dir=tmp_path / f"engine_out_{resolution}_{case_name}",
    )

    sim = SUEWSSimulation(source_path)
    py_df = sim.run().df.droplevel("grid")["SUEWS"][list(CHECK_VARS)]

    res = _run_engine_cli(binary, resolved_path)
    assert res.returncode == 0, f"{resolution}/{case_name}: {res.stderr}"
    eng_df = _read_engine_arrow(out_dir / "suews_output.arrow")

    assert len(eng_df) == expected_rows == len(py_df)
    assert eng_df.index[0] == pd.Timestamp(expected_first)
    assert eng_df.index[-1] == pd.Timestamp(expected_last)
    assert eng_df.index.equals(py_df.index)

    for var in CHECK_VARS:
        np.testing.assert_allclose(
            eng_df[var].to_numpy(),
            py_df[var].to_numpy(),
            rtol=1e-9,
            atol=1e-9,
            err_msg=f"{resolution}/{case_name}: {var} diverged between routes",
        )


def _run_engine_frame(
    binary: Path,
    case_dir: Path,
    forcing_name: str,
    *,
    start_time: str | None,
    end_time: str | None,
    out_dir: Path,
) -> tuple[Path, pd.DataFrame]:
    source_path, resolved_path, _ = _write_case_configs(
        case_dir,
        forcing_name,
        start_time=start_time,
        end_time=end_time,
        output_dir=out_dir,
    )
    res = _run_engine_cli(binary, resolved_path)
    assert res.returncode == 0, res.stderr
    return source_path, _read_engine_arrow(out_dir / "suews_output.arrow")


@pytest.mark.e2e
def test_late_start_negative_controls_against_ignoring_period_and_post_run_cropping(
    tmp_path: Path,
    paired_48h_forcing: dict[str, Path],
):
    """Prove pre-kernel slicing: late start differs from post-run cropping of full run."""
    binary = _locate_suews_engine()
    forcing_name = _copy_forcing(paired_48h_forcing["60"], tmp_path)

    _, full_df = _run_engine_frame(
        binary,
        tmp_path,
        forcing_name,
        start_time=None,
        end_time=None,
        out_dir=tmp_path / "out_full",
    )
    assert len(full_df) == 576

    late_dir = tmp_path / "late"
    late_dir.mkdir()
    _copy_forcing(paired_48h_forcing["60"], late_dir)
    late_source, eng_late = _run_engine_frame(
        binary,
        late_dir,
        forcing_name,
        start_time="2012-01-01 12:05",
        end_time="2012-01-02 12:00",
        out_dir=late_dir / "out_late",
    )
    py_late = (
        SUEWSSimulation(late_source)
        .run()
        .df.droplevel("grid")["SUEWS"][list(CHECK_VARS)]
    )

    # Negative control 1: period was not ignored (strictly shorter than 576-row span).
    assert len(eng_late) == 288 < len(full_df)
    assert eng_late.index[0] == pd.Timestamp("2012-01-01 12:05:00")
    assert eng_late.index.equals(py_late.index)

    # Negative control 2: post-run cropping of full_df diverges on state-dependent fluxes,
    # while true pre-kernel engine run matches the Python route.
    cropped = full_df.loc["2012-01-01 12:05:00":"2012-01-02 12:00:00"]
    assert cropped.index.equals(eng_late.index)
    for flux in ("QH", "QS"):
        diff = float(
            np.max(np.abs(cropped[flux].to_numpy() - eng_late[flux].to_numpy()))
        )
        assert diff > 1.0, (
            f"expected post-run crop {flux} divergence > 1 W m-2, got {diff}"
        )

    for var in CHECK_VARS:
        np.testing.assert_allclose(
            eng_late[var].to_numpy(),
            py_late[var].to_numpy(),
            rtol=1e-9,
            atol=1e-9,
        )


@pytest.mark.e2e
def test_leap_day_cross_route_parity_on_longer_forcing_span(tmp_path: Path):
    """Inclusive leap-day bounds select exact 2012-02-29 rows across both routes."""
    binary = _locate_suews_engine()
    lines = _sample_forcing_lines()
    leap_start = next(
        idx
        for idx, line in enumerate(lines[1:], start=1)
        if line.split()[:4] == ["2012", "59", "1", "0"]
    )
    forcing_files = _write_hourly_and_5min_forcing(
        tmp_path, start_row=leap_start, hours=72
    )

    source_path, resolved_path, out_dir = _write_case_configs(
        tmp_path,
        forcing_files["60"].name,
        start_time="2012-02-29",
        end_time="2012-02-29",
        output_dir=tmp_path / "out_leap",
    )
    py_df = (
        SUEWSSimulation(source_path)
        .run()
        .df.droplevel("grid")["SUEWS"][list(CHECK_VARS)]
    )
    res = _run_engine_cli(binary, resolved_path)
    assert res.returncode == 0, res.stderr
    eng_df = _read_engine_arrow(out_dir / "suews_output.arrow")

    assert len(eng_df) == 288 == len(py_df)
    assert eng_df.index[0] == pd.Timestamp("2012-02-29 00:05:00")
    assert eng_df.index[-1] == pd.Timestamp("2012-03-01 00:00:00")
    assert eng_df.index.equals(py_df.index)
    for var in CHECK_VARS:
        np.testing.assert_allclose(
            eng_df[var].to_numpy(),
            py_df[var].to_numpy(),
            rtol=1e-9,
            atol=1e-9,
        )


@pytest.mark.e2e
@pytest.mark.parametrize(
    ("clock", "timezone"),
    [
        ("local_standard_time", 0.0),
        ("local_standard_time", 5.75),
        ("utc", 5.75),
        ("utc", -3.5),
    ],
)
def test_full_valid_source_regression_on_representative_clocks(
    tmp_path: Path,
    paired_48h_forcing: dict[str, Path],
    clock: str,
    timezone: float,
):
    """Both ``suews run`` and ``suews-engine run`` honour bounds on the forcing clock."""
    binary = _locate_suews_engine()
    forcing_name = _copy_forcing(paired_48h_forcing["60"], tmp_path)

    pub_out = tmp_path / "public_out"
    eng_out = tmp_path / "engine_out"

    source_path, _, _ = _write_case_configs(
        tmp_path,
        forcing_name,
        start_time="2012-01-01 06:05",
        end_time="2012-01-01 18:00",
        timestamp_reference=clock,
        timezone=timezone,
        output_dir=pub_out,
    )

    # Full valid-source public CLI run.
    pub_res = CliRunner().invoke(cli, ["run", str(source_path)])
    assert pub_res.exit_code == 0, pub_res.output
    suews_txt = next(pub_out.glob("*_SUEWS_*.txt"))
    pub_table = pd.read_csv(suews_txt, sep=r"\s+")
    pub_timestamps = pd.DatetimeIndex([
        datetime(int(r.Year), 1, 1)
        + timedelta(days=int(r.DOY) - 1, hours=int(r.Hour), minutes=int(r.Min))
        for r in pub_table.itertuples(index=False)
    ])

    # Standalone native engine run on resolved YAML.
    _, resolved_path, _ = _write_case_configs(
        tmp_path,
        forcing_name,
        start_time="2012-01-01 06:05",
        end_time="2012-01-01 18:00",
        timestamp_reference=clock,
        timezone=timezone,
        output_dir=eng_out,
    )
    eng_res = _run_engine_cli(binary, resolved_path)
    assert eng_res.returncode == 0, eng_res.stderr
    eng_df = _read_engine_arrow(eng_out / "suews_output.arrow")

    assert len(pub_table) == 144 == len(eng_df)
    assert eng_df.index[0] == pd.Timestamp("2012-01-01 06:05:00")
    assert eng_df.index[-1] == pd.Timestamp("2012-01-01 18:00:00")
    assert eng_df.index.equals(pub_timestamps)

    for var in CHECK_VARS:
        # Public txt output rounds to fixed decimal places (~1e-4).
        np.testing.assert_allclose(
            eng_df[var].to_numpy(),
            pub_table[var].to_numpy(),
            rtol=1e-3,
            atol=1e-2,
            err_msg=f"clock={clock} tz={timezone}: {var} mismatch vs suews run",
        )


@pytest.mark.e2e
def test_utc_nonzero_timezone_shifts_physics_without_shifting_forcing_clock_bounds(
    tmp_path: Path,
    paired_48h_forcing: dict[str, Path],
):
    """Under UTC forcing, changing timezone shifts solar/diurnal physics, not bounds."""
    binary = _locate_suews_engine()
    forcing_name = _copy_forcing(paired_48h_forcing["60"], tmp_path)

    frames = []
    for tag, tz in (("plus575", 5.75), ("minus350", -3.5)):
        _, resolved_path, out_dir = _write_case_configs(
            tmp_path,
            forcing_name,
            start_time="2012-01-01 06:05",
            end_time="2012-01-01 18:00",
            timestamp_reference="utc",
            timezone=tz,
            output_dir=tmp_path / f"out_{tag}",
        )
        res = _run_engine_cli(binary, resolved_path)
        assert res.returncode == 0, res.stderr
        df = _read_engine_arrow(out_dir / "suews_output.arrow")
        assert len(df) == 144
        assert df.index[0] == pd.Timestamp("2012-01-01 06:05:00")
        assert df.index[-1] == pd.Timestamp("2012-01-01 18:00:00")
        frames.append(df)

    max_qf_diff = float(
        np.max(np.abs(frames[0]["QF"].to_numpy() - frames[1]["QF"].to_numpy()))
    )
    max_qn_diff = float(
        np.max(np.abs(frames[0]["QN"].to_numpy() - frames[1]["QN"].to_numpy()))
    )
    assert max_qf_diff > 0.1 or max_qn_diff > 0.1


@pytest.mark.e2e
@pytest.mark.parametrize(
    ("label", "start_time", "end_time", "expected_needles"),
    [
        (
            "reversed",
            "2012-01-02",
            "2012-01-01",
            ("model.control.start_time", "model.control.end_time"),
        ),
        (
            "outside",
            "2012-01-10",
            "2012-01-11",
            ("does not overlap", "model.control.start_time", "model.control.end_time"),
        ),
        (
            "between_adjacent_rows",
            "2012-01-01 00:06",
            "2012-01-01 00:09",
            ("does not overlap", "model.control.start_time", "model.control.end_time"),
        ),
        (
            "partial_early_start",
            "2011-12-31",
            "2012-01-01",
            ("does not cover", "model.control.start_time"),
        ),
        (
            "partial_late_end",
            "2012-01-01",
            "2012-01-05",
            ("does not cover", "model.control.end_time"),
        ),
        ("invalid_leap", "2013-02-29", None, ("model.control.start_time",)),
        (
            "tz_suffix",
            None,
            "2012-01-01T12:00:00Z",
            ("model.control.end_time", "timezone"),
        ),
        ("wrapped_null", {"value": None}, None, ("model.control.start_time",)),
        ("int_bound", 20120101, None, ("model.control.start_time",)),
        ("bool_bound", None, True, ("model.control.end_time",)),
        ("list_bound", [], None, ("model.control.start_time",)),
        ("empty_map", {}, None, ("model.control.start_time",)),
    ],
)
def test_engine_rejects_invalid_and_uncovered_periods_without_writing_outputs(
    tmp_path: Path,
    paired_48h_forcing: dict[str, Path],
    label: str,
    start_time,
    end_time,
    expected_needles: tuple[str, ...],
):
    """Invalid or uncovered bounds fail with field-specific errors and zero files created."""
    binary = _locate_suews_engine()
    forcing_name = _copy_forcing(paired_48h_forcing["60"], tmp_path)

    # Start from a valid resolved YAML, then patch start_time / end_time directly
    # so malformed YAML shapes reach the native binary unchanged.
    _, resolved_path, _ = _write_case_configs(
        tmp_path,
        forcing_name,
        start_time=None,
        end_time=None,
        output_dir=tmp_path / f"never_created_{label}",
    )
    out_dir = tmp_path / f"never_created_{label}"
    doc = yaml.safe_load(resolved_path.read_text(encoding="utf-8"))
    control = doc["model"]["control"]
    if start_time is None:
        control.pop("start_time", None)
    else:
        control["start_time"] = start_time
    if end_time is None:
        control.pop("end_time", None)
    else:
        control["end_time"] = end_time
    resolved_path.write_text(yaml.safe_dump(doc, sort_keys=False), encoding="utf-8")

    res = _run_engine_cli(binary, resolved_path)
    assert res.returncode != 0, f"{label}: expected failure, got exit 0"
    for needle in expected_needles:
        assert needle in res.stderr, (
            f"{label}: missing {needle!r} in stderr:\n{res.stderr}"
        )
    assert not out_dir.exists(), f"{label}: failed run created {out_dir}"


def _assert_outside_buffer_chunk_runs(
    config: SUEWSConfig, forcing: pd.DataFrame
) -> None:
    shifted = forcing.iloc[:3].copy()
    shifted.index = pd.date_range("2011-12-31 23:55", periods=3, freq="5min")
    shifted["iy"] = shifted.index.year
    shifted["id"] = shifted.index.day_of_year
    shifted["it"] = shifted.index.hour
    shifted["imin"] = shifted.index.minute
    payload = _run_rust.config_to_engine_yaml(config)
    block = _run_rust._prepare_forcing_block(shifted)
    result = _run_rust._check_rust_available().run_suews(
        payload, block.ravel(order="C").tolist(), 3
    )
    assert result[2] == 3


def test_direct_buffer_and_high_level_override_semantics_preserved(
    tmp_path: Path,
    paired_48h_forcing: dict[str, Path],
):
    """Direct-buffer chunks and SUEWSSimulation.run() overrides remain unaffected."""
    forcing_name = _copy_forcing(paired_48h_forcing["60"], tmp_path)
    source_path, _, _ = _write_case_configs(
        tmp_path,
        forcing_name,
        start_time="2012-01-01",
        end_time="2012-01-02",
        output_dir=tmp_path / "unused_out",
    )
    sim = SUEWSSimulation(source_path)
    config = sim.config
    forcing = sim.forcing.df
    grid = config.sites[0].gridiv

    # High-level override narrows the run without mutating config bounds.
    override_out = sim.run(start_date="2012-01-01 12:05", end_date="2012-01-01 13:00")
    assert len(override_out.df) == 12
    assert config.model.control.start_time == "2012-01-01"
    assert config.model.control.end_time == "2012-01-02"

    # Direct-buffer initial + resumed chunks smaller than config bounds.
    chunk1 = forcing.iloc[24:28]
    chunk2 = forcing.iloc[28:32]
    out1, state1, _ = _run_rust.run_suews_rust(config, chunk1, grid_id=grid)
    out2, _, _ = _run_rust.run_suews_rust_with_state(config, chunk2, grid, state1)
    assert out1.index.get_level_values("datetime").tolist() == chunk1.index.tolist()
    assert out2.index.get_level_values("datetime").tolist() == chunk2.index.tolist()

    _assert_outside_buffer_chunk_runs(config, forcing)
    assert not (tmp_path / "unused_out").exists()


def test_config_to_engine_yaml_canonicalises_and_validates_bounds(tmp_path: Path):
    """Python-owned YAML resolution canonicalises non-ISO bounds and rejects invalid ones."""
    cfg = _load_sample_config_dict()
    control = cfg["model"]["control"]
    control["forcing"]["file"] = str(tmp_path / "dummy.txt")
    control["output"]["dir"] = str(tmp_path / "out")
    control["start_time"] = "2012/01/01 06:05"
    control["end_time"] = "2012-1-2"

    resolved = yaml.safe_load(
        _run_rust.config_to_engine_yaml(SUEWSConfig.from_dict(cfg))
    )
    assert resolved["model"]["control"]["start_time"] == "2012-01-01 06:05:00"
    assert resolved["model"]["control"]["end_time"] == "2012-01-02"

    for bad_value, needle in (
        ("2012-01-01T06:05:00Z", "timezone-naive"),
        ("NaT", "NaT"),
        ("not-a-date", "parseable"),
    ):
        cfg_bad = _load_sample_config_dict()
        cfg_bad["model"]["control"]["forcing"]["file"] = str(tmp_path / "dummy.txt")
        cfg_bad["model"]["control"]["output"]["dir"] = str(tmp_path / "out")
        cfg_bad["model"]["control"]["start_time"] = bad_value
        with pytest.raises(ValueError, match=needle):
            _run_rust.config_to_engine_yaml(SUEWSConfig.from_dict(cfg_bad))
