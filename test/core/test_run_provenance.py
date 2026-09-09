"""Provenance sidecar written by ``SUEWSSimulation.save()``.

These tests exercise the real producer/consumer flow: a short sample run is
saved, and the resulting ``provenance.json`` is read back through the same
readers that ``suews diagnose`` and the MCP run resource use. Nothing here
manufactures the sidecar by hand.
"""

from __future__ import annotations

import hashlib
import json
from pathlib import Path

from click.testing import CliRunner
import pandas as pd
import pytest

from supy._provenance import (
    PROVENANCE_FILENAME,
    read_provenance,
    requested_bound_to_str,
)
from supy.cmd.diagnose_run import diagnose_run_cmd
from supy.diagnostics import check_provenance_present
from supy.suews_sim import SUEWSSimulation

pytestmark = pytest.mark.api

SHORT_STEPS = 24


def _sha256(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


@pytest.fixture(scope="module")
def saved_run(tmp_path_factory) -> dict:
    """Run the sample case for a few steps and save it in a fresh directory."""
    sim = SUEWSSimulation.from_sample_data()
    forcing_index = sim.forcing.df.index
    end_date = forcing_index[SHORT_STEPS - 1]
    sim.run(end_date=end_date, n_jobs=1)
    out_dir = tmp_path_factory.mktemp("run")
    paths = sim.save(out_dir)
    return {"sim": sim, "out_dir": out_dir, "paths": paths, "end_date": end_date}


def test_save_writes_provenance_last(saved_run):
    paths = [Path(p) for p in saved_run["paths"]]
    assert paths[-1].name == PROVENANCE_FILENAME
    assert paths[-1].is_file()
    assert (saved_run["out_dir"] / PROVENANCE_FILENAME).is_file()


def test_provenance_identifies_config_and_forcing(saved_run):
    prov = read_provenance(saved_run["out_dir"])
    sim = saved_run["sim"]

    assert prov["format_version"] == 1
    assert prov["supy_version"]
    assert "git_commit" in prov

    config_path = sim._config_path
    assert prov["config"]["source"] == "file"
    assert prov["config"]["name"] == config_path.name
    assert prov["config"]["sha256"] == _sha256(config_path)
    assert prov["config"]["size_bytes"] == config_path.stat().st_size
    assert prov["config"]["schema_version"]
    assert prov["config"]["sites"] == [site.name for site in sim.config.sites]
    assert prov["config"]["grids"]

    forcing = prov["forcing"]
    assert forcing["source"] == "files"
    assert len(forcing["files"]) == 1
    forcing_file = config_path.parent / sim.config.model.control.forcing.file.value
    assert forcing["files"][0]["name"] == forcing_file.name
    assert forcing["files"][0]["sha256"] == _sha256(forcing_file)
    # The frame handed to the kernel is resampled from the 60-min file, so
    # its effective hash is a separate identity from the source file.
    assert forcing["effective_sha256"] != forcing["files"][0]["sha256"]
    assert forcing["effective_n_rows"] == SHORT_STEPS
    assert prov["config"]["effective_sha256"]


def test_provenance_records_requested_and_actual_period(saved_run):
    prov = read_provenance(saved_run["out_dir"])
    sim = saved_run["sim"]
    period = prov["period"]

    # Requested: start fell back to the config, end was the explicit argument.
    assert pd.Timestamp(period["requested"]["end"]) == pd.Timestamp(
        saved_run["end_date"]
    )
    # The raw value is the config string; ``start`` is the resolved instant
    # (a date-only start is that day's midnight).
    assert period["requested"]["start_raw"] == requested_bound_to_str(
        sim.config.model.control.start_time
    )
    assert pd.Timestamp(period["requested"]["start"]) == pd.Timestamp(
        sim.config.model.control.start_time
    )

    # Actual: what was simulated, i.e. the first SHORT_STEPS forcing rows.
    df_output = sim.output.df
    actual_start = pd.Timestamp(period["actual"]["start"])
    actual_end = pd.Timestamp(period["actual"]["end"])
    times = df_output.index.get_level_values("datetime")
    assert actual_start == times.min()
    assert actual_end == times.max()
    assert period["actual"]["n_timesteps"] == SHORT_STEPS
    assert period["tstep_s"] == 300
    # ``run()`` published ``_run_period``: the explicit end was covered, so
    # nothing was clipped under the default strict policy.
    assert period["clipped"] is False
    assert period["policy"] == "strict"

    assert prov["timestamps"]["convention"] == "interval_end"
    assert prov["timestamps"]["forcing_reference"] == "local_standard_time"
    assert prov["timestamps"]["output_reference"] in {
        "follow",
        "utc",
        "local_standard_time",
        "daylight",
    }


def test_provenance_lists_saved_files_and_run_options(saved_run):
    prov = read_provenance(saved_run["out_dir"])
    saved_names = {Path(p).name for p in saved_run["paths"]} - {PROVENANCE_FILENAME}
    listed = set(prov["output"]["files"])
    if prov["output"]["checkpoint"]:
        listed.add(prov["output"]["checkpoint"])
        assert prov["output"]["checkpoint"].endswith("_checkpoint.json")
    assert listed == saved_names
    assert prov["output"]["format"] == "txt"
    assert prov["output"]["frequency_s"] > 0

    run = prov["run"]
    assert run["interface"] == "python"
    assert run["command"] is None
    assert run["n_jobs"] == 1
    assert run["continued_from_checkpoint"] is False
    assert pd.Timestamp(run["started_at"]) <= pd.Timestamp(run["ended_at"])


def test_provenance_contains_no_absolute_paths(saved_run):
    text = (saved_run["out_dir"] / PROVENANCE_FILENAME).read_text(encoding="utf-8")
    assert str(Path.home()) not in text
    assert str(saved_run["out_dir"]) not in text
    assert str(saved_run["sim"]._config_path.parent) not in text
    # ASCII only, per project logging/file conventions.
    text.encode("ascii")


def test_diagnose_sees_provenance_from_real_run(saved_run):
    out_dir = saved_run["out_dir"]
    assert check_provenance_present(out_dir).passed

    result = CliRunner().invoke(diagnose_run_cmd, [str(out_dir), "--format", "json"])
    envelope = json.loads(result.stdout)
    checks = {c["name"]: c for c in envelope["data"]["checks"]}
    assert checks["provenance_present"]["passed"] is True
    assert checks["output_files_present"]["passed"] is True


def test_provenance_consumes_run_period_contract(tmp_path):
    """The ``_run_period`` record written by ``run()`` is the single source of truth."""
    sim = SUEWSSimulation.from_sample_data()
    sim.update_forcing(sim.forcing.df.iloc[:SHORT_STEPS].copy())
    index = sim.forcing.df.index
    # The config requests the whole sample year; clip to the loaded window so
    # the record carries a genuine requested-versus-actual difference.
    sim.run(n_jobs=1, clip_to_forcing=True)
    assert sim._run_period["clipped"] is True
    sim.save(tmp_path)
    period = read_provenance(tmp_path)["period"]
    assert period["requested"]["start"] == "2012-01-01T00:00:00"
    assert period["requested"]["start_raw"] == "2012-01-01"
    assert period["requested"]["end_raw"] == "2012-12-31"
    assert pd.Timestamp(period["requested"]["end"]) == pd.Timestamp("2013-01-01")
    assert pd.Timestamp(period["actual"]["start"]) == index[0]
    assert pd.Timestamp(period["actual"]["end"]) == index[-1]
    assert period["clipped"] is True
    assert period["policy"] == "clip"


def test_output_format_follows_yaml_when_no_kwarg(tmp_path):
    """A YAML ``output.format: parquet`` run must not be labelled txt."""
    sim = SUEWSSimulation.from_sample_data()
    sim.update_config({"model": {"control": {"output": {"format": "parquet"}}}})
    sim.update_forcing(sim.forcing.df.iloc[:SHORT_STEPS].copy())
    sim.run(end_date=sim.forcing.index[-1], n_jobs=1)
    paths = sim.save(tmp_path)
    assert any(Path(p).suffix == ".parquet" for p in paths)
    prov = read_provenance(tmp_path)
    assert prov["output"]["format"] == "parquet"
    assert any(name.endswith(".parquet") for name in prov["output"]["files"])


def test_inputs_replaced_after_run_do_not_relabel_output(tmp_path):
    """Provenance describes the inputs that produced the output, not the
    inputs attached to the object at save time."""
    sim = SUEWSSimulation.from_sample_data()
    sim.update_forcing(sim.forcing.df.iloc[:SHORT_STEPS].copy())
    sim.run(end_date=sim.forcing.index[-1], n_jobs=1)
    original_config_hash = sim._run_metadata["inputs"]["config"]["effective_sha256"]
    original_forcing_hash = sim._run_metadata["inputs"]["forcing"]["effective_sha256"]

    # Replace both inputs without rerunning: a different forcing slice from
    # a file (so the source now reports files) and a config edit.
    forcing_path = sim._config_path.parent / sim.config.model.control.forcing.file.value
    sim.update_forcing(forcing_path)
    sim.update_config({"model": {"control": {"tstep": 600}}})
    assert sim._run_completed

    sim.save(tmp_path)
    prov = read_provenance(tmp_path)
    assert prov["forcing"]["source"] == "in-memory"
    assert "files" not in prov["forcing"]
    assert prov["forcing"]["effective_sha256"] == original_forcing_hash
    assert prov["forcing"]["effective_n_rows"] == SHORT_STEPS
    assert prov["config"]["effective_sha256"] == original_config_hash
    assert prov["period"]["tstep_s"] == 300
    assert prov["period"]["actual"]["n_timesteps"] == SHORT_STEPS


def test_edited_in_memory_config_changes_effective_hash(tmp_path):
    sim_a = SUEWSSimulation.from_sample_data()
    sim_a.update_forcing(sim_a.forcing.df.iloc[:SHORT_STEPS].copy())
    sim_a.run(end_date=sim_a.forcing.index[-1], n_jobs=1)
    sim_b = SUEWSSimulation.from_sample_data()
    sim_b.update_config({"model": {"control": {"tstep": 600}}})
    sim_b.update_forcing(sim_b.forcing.df.iloc[:SHORT_STEPS].copy())
    sim_b.run(end_date=sim_b.forcing.index[-1], n_jobs=1)
    hash_a = sim_a._run_metadata["inputs"]["config"]["effective_sha256"]
    hash_b = sim_b._run_metadata["inputs"]["config"]["effective_sha256"]
    assert hash_a != hash_b
    # Same source file on both, so the file identity is unchanged.
    assert (
        sim_a._run_metadata["inputs"]["config"]["sha256"]
        == sim_b._run_metadata["inputs"]["config"]["sha256"]
    )


def test_in_memory_forcing_is_reported_truthfully(tmp_path):
    sim = SUEWSSimulation.from_sample_data()
    sim.update_forcing(sim.forcing.df.iloc[:SHORT_STEPS].copy())
    sim.run(end_date=sim.forcing.index[-1], n_jobs=1)
    sim.save(tmp_path)
    prov = read_provenance(tmp_path)
    assert prov["forcing"]["source"] == "in-memory"
    assert "files" not in prov["forcing"]
    assert prov["forcing"]["effective_sha256"]
    assert prov["period"]["actual"]["n_timesteps"] == SHORT_STEPS


def test_cli_run_records_command(tmp_path):
    """``suews run`` records its command line in the sidecar."""
    from supy.cmd.SUEWS import SUEWS as run_cmd

    sim = SUEWSSimulation.from_sample_data()
    src = sim._config_path.parent
    # Copy the sample case into a scratch directory with a shortened forcing
    # file so the CLI run stays quick.
    cfg_text = (src / "sample_config.yml").read_text(encoding="utf-8")
    forcing_name = sim.config.model.control.forcing.file.value
    lines = (src / forcing_name).read_text(encoding="utf-8").splitlines(keepends=True)
    (tmp_path / forcing_name).write_text(
        "".join(lines[: SHORT_STEPS + 1]), encoding="utf-8"
    )
    # The shortened file covers one day, so request one day.
    assert 'end_time: "2012-12-31"' in cfg_text
    cfg_text = cfg_text.replace('end_time: "2012-12-31"', 'end_time: "2012-01-01"')
    cfg_path = tmp_path / "case.yml"
    cfg_path.write_text(cfg_text, encoding="utf-8")

    result = CliRunner().invoke(run_cmd, [str(cfg_path)])
    assert result.exit_code == 0, result.output
    # The sample config saves to ``./Output`` relative to the config file.
    prov = read_provenance(tmp_path / "Output")
    assert prov["run"]["interface"] == "cli"
    assert prov["run"]["command"] == "suews run case.yml"
    assert prov["config"]["name"] == "case.yml"
