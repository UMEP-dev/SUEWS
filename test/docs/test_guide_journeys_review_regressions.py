"""Reader outcomes, source drift and host settings keep separate authority."""

import argparse
import json
import os
from pathlib import Path
import sys

import pytest

pytest.importorskip("docutils", reason="Source binding requires optional docutils")
ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT))
from scripts.suews.guide_journeys import environment, runner  # noqa: E402
from scripts.suews.guide_journeys.manifest import load  # noqa: E402

pytestmark = pytest.mark.api


@pytest.mark.parametrize("probe_kind", ["required", "diagnostic"])
def test_probe_cannot_satisfy_reader_outcomes(tmp_path, monkeypatch, probe_kind):
    """A successful probe cannot supply the reader's missing file or stdout."""
    specs = [
        {
            "id": "reader",
            "kind": "documented",
            "source_lines": [1, 1],
            "command": "echo reader",
            "promises": [],
        },
        {
            "id": "probe",
            "kind": "probe",
            "source_lines": None,
            "probe_kind": probe_kind,
            "command": "touch promised.txt",
            "promises": [{"kind": "creates", "path": "promised.txt"}],
        },
    ]
    monkeypatch.setattr(runner, "bind_journey", lambda *args: specs)
    monkeypatch.setattr(runner, "resolve_install", lambda *args: {"command": "unused"})
    monkeypatch.setattr(
        runner,
        "prepare",
        lambda *args: (
            dict(os.environ),
            {"package_preflight": {"supy_version": "test"}},
        ),
    )
    work = tmp_path / "fixture" / "work"
    work.mkdir(parents=True)
    wheel = tmp_path / "unused.whl"
    wheel.write_bytes(b"fixture")
    args = argparse.Namespace(
        output=tmp_path,
        wheel=wheel,
        wheelhouse=tmp_path,
        pip_version="unused",
        revision="unused",
    )
    record = {"input_sha256s": {}}
    runner.execute_journey(
        ROOT,
        {"installation": {}},
        None,
        {
            "id": "fixture",
            "inputs": {},
            "outcomes": [
                {"kind": "creates", "path": "promised.txt"},
                {"kind": "diagnostic", "stream": "stdout", "pattern": "reader"},
            ],
        },
        args,
        record,
    )
    assert all(step["status"] == "passed" for step in record["steps"])
    assert record["steps"][-1]["promises"][0]["passed"]
    assert not record["path_outcomes"][0]["passed"]
    assert record["path_outcomes"][0]["observed"]["after"] is None
    assert record["path_outcomes"][1]["passed"]
    assert record["status"] == "failed"
    assert record["accepted"] is False


@pytest.mark.parametrize("check_only", [True, False])
def test_install_hash_drift_is_stale_before_setup(
    tmp_path, monkeypatch, capsys, check_only
):
    """Both CLI modes reject an independently changed installation source."""
    manifest = load(ROOT / "test/docs/journeys/transition_guide.yml")
    original_read = runner.read_page
    installation = manifest["installation"]
    original_command = runner.resolve_install(ROOT, "HEAD", installation)["command"]

    def changed_page(root, revision, path):
        text = original_read(root, revision, path)
        if path == installation["guide"]:
            return text.replace(original_command, original_command + " --upgrade")
        return text

    args = argparse.Namespace(
        manifest=ROOT / "test/docs/journeys/transition_guide.yml",
        revision="HEAD",
        journey=None,
        check=check_only,
        output=tmp_path,
    )
    monkeypatch.chdir(ROOT)
    monkeypatch.setattr(runner, "arguments", lambda: args)
    monkeypatch.setattr(runner, "read_page", changed_page)
    assert runner.main() == 1
    if check_only:
        result = json.loads(capsys.readouterr().out)
        assert "Installation source changed" in result["stale"]
        assert result["steps"] == []
    else:
        result = json.loads((tmp_path / "staleness.json").read_text(encoding="utf-8"))
        assert result["status"] == "stale"
        assert "Installation source changed" in result["failure"]
    assert not (tmp_path / "summary.json").exists()


def test_prepare_isolates_host_configuration(tmp_path, monkeypatch):
    """Even environment seeding cannot inherit model flags or host XDG paths."""
    for key in (
        "SUEWS_BACKEND",
        "SUEWS_TEST_UNKNOWN",
        "XDG_CACHE_HOME",
        "XDG_CONFIG_DIRS",
    ):
        monkeypatch.setenv(key, "/host/settings")
    captured = {}

    class SeedObserved(Exception):
        pass

    def seed(*args, **kwargs):
        captured.update(kwargs["env"])
        raise SeedObserved

    monkeypatch.setattr(environment.subprocess, "run", seed)
    with pytest.raises(SeedObserved):
        environment.prepare(
            tmp_path / "journey",
            tmp_path / "wheel",
            tmp_path,
            "26.3",
            "pip install supy",
        )
    assert not any(key.startswith("SUEWS_") for key in captured)
    home = Path(captured["HOME"])
    for key in (
        "XDG_CACHE_HOME",
        "XDG_CONFIG_HOME",
        "XDG_DATA_HOME",
        "XDG_STATE_HOME",
        "XDG_RUNTIME_DIR",
        "XDG_CONFIG_DIRS",
        "XDG_DATA_DIRS",
    ):
        directory = Path(captured[key])
        assert directory.is_relative_to(home)
        assert directory.is_dir()
    assert Path(captured["XDG_RUNTIME_DIR"]).stat().st_mode & 0o777 == 0o700
