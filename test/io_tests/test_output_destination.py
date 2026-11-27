"""Output destinations across source exports, public saving and file execution."""

from copy import deepcopy
from datetime import datetime, timedelta
from importlib import import_module
from importlib.resources import files
import json
from pathlib import Path
import shutil
import subprocess

from click.testing import CliRunner
import pytest
import yaml

from supy import SUEWSSimulation
from supy.cmd.suews_cli import cli
from supy.data_model import SUEWSConfig

bridge = import_module("supy._run_rust")

pytestmark = [pytest.mark.native, pytest.mark.os_compat, pytest.mark.contract("C-SAVE")]


def _source(tmp_path, destination):
    sample = files("supy").joinpath("sample_data")
    data = yaml.safe_load(
        sample.joinpath("sample_config.yml").read_text(encoding="utf-8")
    )
    control = data["model"]["control"]
    control["output"].pop("dir", None)
    if destination is not None:
        control["output"]["dir"] = destination
    control["start_time"] = "2012-01-01 00:05:00"
    control["end_time"] = "2012-01-01 01:00:00"
    control["output"]["freq"] = 300
    config_dir = tmp_path / "config"
    config_dir.mkdir()
    forcing = control["forcing"]["file"]
    forcing = forcing["value"] if isinstance(forcing, dict) else forcing
    lines = sample.joinpath(forcing).read_text(encoding="utf-8").splitlines()
    (config_dir / forcing).write_text("\n".join(lines[:13]) + "\n", encoding="utf-8")
    source = config_dir / "source.yml"
    source.write_text(yaml.safe_dump(data), encoding="utf-8")
    return source, SUEWSConfig.from_dict(data)


@pytest.mark.parametrize("destination", [None, ".", "specified", "absolute"])
def test_source_and_runtime_exports(tmp_path, monkeypatch, destination):
    """Resolve only runtime omission and preserve supplied source paths."""
    if destination == "absolute":
        destination = str(tmp_path / "absolute")
    _, config = _source(tmp_path, destination)
    caller = tmp_path / "caller"
    caller.mkdir()
    monkeypatch.chdir(caller)
    expected = str(caller) if destination is None else destination
    engine = yaml.safe_load(bridge.config_to_engine_yaml(config))
    assert engine["model"]["control"]["output"]["dir"] == expected
    _, payloads = bridge._grid_config_jsons(config)
    assert all(
        json.loads(p)["model"]["control"]["output"].get("dir") == destination
        for p in payloads
    )
    assert config.model.control.output.dir == destination
    for data in (
        config.model_dump(mode="json", exclude_none=True),
        json.loads(config.model_dump_json(exclude_none=True)),
    ):
        assert data["model"]["control"]["output"].get("dir") == destination
    path = tmp_path / "roundtrip.yml"
    config.to_yaml(str(path))
    restored = yaml.safe_load(path.read_text(encoding="utf-8"))
    assert restored["model"]["control"]["output"].get("dir") == destination
    other = tmp_path / "other"
    other.mkdir()
    monkeypatch.chdir(other)
    next_export = yaml.safe_load(bridge.config_to_engine_yaml(config))
    assert next_export["model"]["control"]["output"]["dir"] == (
        str(other) if destination is None else destination
    )


def test_buffer_run_needs_no_current_directory(tmp_path, monkeypatch):
    """A buffer-only simulation can run after its working directory disappears."""
    source, _ = _source(tmp_path, None)
    simulation = SUEWSSimulation(source)
    assert simulation.config.model.control.output.dir is None
    assert not simulation.forcing.df.empty

    def unavailable_cwd():
        raise FileNotFoundError("current working directory unavailable")

    with monkeypatch.context() as context:
        context.setattr(Path, "cwd", staticmethod(unavailable_cwd))
        result = simulation.run()
    expected = [datetime(2012, 1, 1) + timedelta(minutes=5 * i) for i in range(1, 13)]
    assert result.df.index.get_level_values("datetime").tolist() == expected
    assert result.df.index.is_unique


@pytest.mark.parametrize("continuation", [False, True], ids=["initial", "resume"])
def test_single_grid_buffer_run_needs_no_current_directory(
    tmp_path, monkeypatch, continuation
):
    """Both native single-grid entry points return the expected forcing times."""
    source, _ = _source(tmp_path, None)
    simulation = SUEWSSimulation(source)
    config = simulation.config
    forcing = simulation.forcing.df
    grid = config.sites[0].gridiv
    selected = forcing.iloc[:2]
    if continuation:
        _, state, _ = bridge.run_suews_rust(config, selected, grid_id=grid)
        assert state is not None
        selected = forcing.iloc[2:4]

    def unavailable_cwd():
        raise FileNotFoundError("current working directory unavailable")

    with monkeypatch.context() as context:
        context.setattr(Path, "cwd", staticmethod(unavailable_cwd))
        if continuation:
            output, _, _ = bridge.run_suews_rust_with_state(
                config, selected, grid, state
            )
        else:
            output, _, _ = bridge.run_suews_rust(config, selected, grid)
    assert output.index.get_level_values("datetime").tolist() == selected.index.tolist()
    assert output.index.get_level_values("grid").unique().tolist() == [grid]


def _engine():
    path = Path(str(files("supy").joinpath("bin/suews-engine")))
    if path.is_file():
        return str(path)
    binary = shutil.which("suews-engine")
    if binary:
        return binary
    pytest.skip("Build/install the matching engine before running destination tests")


def test_multigrid_buffer_export_preserves_omitted_destination(tmp_path, monkeypatch):
    """Buffer-only site payloads can omit a file destination on every grid."""
    _, config = _source(tmp_path, None)
    data = config.model_dump(mode="json", exclude_none=True, round_trip=True)
    second = deepcopy(data["sites"][0])
    second["gridiv"] = 2
    data["sites"].append(second)
    config = SUEWSConfig.from_dict(data)
    monkeypatch.chdir(tmp_path)
    grid_ids, payloads = bridge._grid_config_jsons(config)
    assert len(set(grid_ids)) == len(payloads) == 2
    for payload in payloads:
        data = json.loads(payload)
        assert len(data["sites"]) == 1
        assert "dir" not in data["model"]["control"]["output"]
    assert config.model.control.output.dir is None


@pytest.mark.parametrize("serialisation", ["yaml", "json"])
@pytest.mark.parametrize("exclude_none", [False, True])
def test_direct_source_dump_library_calls_write_no_files(
    tmp_path, monkeypatch, serialisation, exclude_none
):
    """Keep live source dumps usable by buffer-only native consumers."""
    source, _ = _source(tmp_path, None)
    simulation = SUEWSSimulation(source)
    data = simulation.config.model_dump(mode="json", exclude_none=exclude_none)
    assert data["model"]["control"]["output"].get("dir") is None
    payload = yaml.safe_dump(data) if serialisation == "yaml" else json.dumps(data)
    forcing = bridge._prepare_forcing_block(simulation.forcing.df.iloc[:2])
    monkeypatch.chdir(tmp_path)
    before = {p: p.read_bytes() for p in tmp_path.rglob("*") if p.is_file()}
    result = bridge._check_rust_available().run_suews(
        payload, forcing.ravel(order="C").tolist(), 2
    )
    assert result[2] == 2
    assert {p: p.read_bytes() for p in tmp_path.rglob("*") if p.is_file()} == before


@pytest.mark.e2e
@pytest.mark.parametrize(
    "destination", [None, pytest.param("", id="empty-string"), ".", "specified", "absolute"]
)
@pytest.mark.parametrize("route", ["public", "engine", "save"])
def test_actual_output_destinations(tmp_path, monkeypatch, destination, route):
    """Observe files in the intended destination from a distinct caller cwd."""
    if destination == "absolute":
        destination = str(tmp_path / "absolute")
    source, config = _source(tmp_path, destination)
    caller = tmp_path / "caller"
    caller.mkdir()
    monkeypatch.chdir(caller)
    expected = caller if destination is None else source.parent / destination
    if route == "engine":
        binary = _engine()
        resolved = source.parent / "resolved.yml"
        resolved.write_text(bridge.config_to_engine_yaml(config), encoding="utf-8")
        # Omission captures the export cwd, even when execution moves elsewhere.
        launch = tmp_path / "launch"
        launch.mkdir()
        before = set(tmp_path.rglob("*"))
        result = subprocess.run(
            [binary, "run", str(resolved)],
            cwd=launch,
            capture_output=True,
            text=True,
            check=False,
        )
        assert result.returncode == 0, result.stdout + result.stderr
    elif route == "public":
        if destination == "":
            expected = caller
        before = set(tmp_path.rglob("*"))
        result = CliRunner().invoke(cli, ["run", str(source)])
        assert result.exit_code == 0, result.output
    else:
        simulation = SUEWSSimulation(source)
        simulation.run()
        expected = caller / "override"
        before = set(tmp_path.rglob("*"))
        simulation.save("override")
    written = {p for p in set(tmp_path.rglob("*")) - before if p.is_file()}
    assert written
    assert {p.parent.resolve() for p in written} == {expected.resolve()}
    assert all(p.stat().st_size > 0 for p in written)
    if route == "engine":
        assert {p.name for p in written} == {"suews_output.arrow"}
    else:
        assert len(written) == 4
        assert any(p.suffix == ".txt" for p in written)
        assert any(p.name.endswith(".units.json") for p in written)
        assert any(p.name.endswith("_checkpoint.json") for p in written)
        assert any(p.name == "provenance.json" for p in written)


@pytest.mark.parametrize("destination", [None, "modern_results"])
def test_source_output_block_takes_precedence_over_legacy_path(
    tmp_path, monkeypatch, destination
):
    """An output block selects the launch directory or its own configured path."""
    source, _ = _source(tmp_path, destination)
    data = yaml.safe_load(source.read_text(encoding="utf-8"))
    data["model"]["control"]["output_file"] = {"path": "legacy_results"}
    source.write_text(yaml.safe_dump(data), encoding="utf-8")
    caller = tmp_path / "caller"
    caller.mkdir()
    monkeypatch.chdir(caller)
    before = set(tmp_path.rglob("*"))

    result = CliRunner().invoke(cli, ["run", str(source)])

    assert result.exit_code == 0, result.output
    expected = caller if destination is None else source.parent / destination
    written = [p for p in set(tmp_path.rglob("*")) - before if p.is_file()]
    assert {p.parent for p in written} == {expected}
    assert any(p.name.endswith(".txt") and p.stat().st_size for p in written)
    assert not (source.parent / "legacy_results").exists()


@pytest.mark.e2e
@pytest.mark.parametrize(
    "bad", ["missing", None, {"value": None}, 7, True, [], {}, {"value": []}]
)
def test_engine_rejects_bad_destination_without_outputs(tmp_path, bad):
    """Reject malformed destinations without creating or changing files."""
    binary = _engine()
    source, config = _source(tmp_path, None)
    data = yaml.safe_load(bridge.config_to_engine_yaml(config))
    output = data["model"]["control"]["output"]
    if bad == "missing":
        output.pop("dir")
    else:
        output["dir"] = bad
    data["model"]["control"]["output_file"] = {"path": "legacy"}
    resolved = source.parent / "resolved.yml"
    resolved.write_text(yaml.safe_dump(data), encoding="utf-8")
    before = {p: p.read_bytes() for p in tmp_path.rglob("*") if p.is_file()}
    result = subprocess.run(
        [binary, "run", str(resolved)],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )
    assert result.returncode != 0
    assert "model.control.output.dir" in result.stderr
    assert "suews run" in result.stderr
    assert {p: p.read_bytes() for p in tmp_path.rglob("*") if p.is_file()} == before
