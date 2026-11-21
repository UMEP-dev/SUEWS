"""Installation is bound to page bytes and retains offline wheelhouse policy."""

import os
from pathlib import Path
import subprocess
import sys

import pytest

pytest.importorskip("docutils", reason="Source binding requires optional docutils")
ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT))
from scripts.suews.guide_journeys import environment, runner  # noqa: E402
from scripts.suews.guide_journeys.extractor import digest  # noqa: E402

pytestmark = pytest.mark.api


def test_install_is_extracted_at_revision_and_executed_literally(tmp_path):
    """A dirty page cannot replace the selected revision's installation text."""
    page = tmp_path / "page.rst"
    text = (ROOT / "test/fixtures/guide_journeys/python_page.rst").read_text(
        encoding="utf-8"
    )
    text = text.replace("pip install supy", "pip install supy --upgrade")
    page.write_text(text, encoding="utf-8")
    subprocess.run(["git", "init", "-q", str(tmp_path)], check=True)
    subprocess.run(["git", "add", "page.rst"], cwd=tmp_path, check=True)
    subprocess.run(
        [
            "git",
            "-c",
            "user.name=Test",
            "-c",
            "user.email=test@example.org",
            "commit",
            "-qm",
            "Fixture page",
        ],
        cwd=tmp_path,
        check=True,
    )
    page.write_text(text.replace("--upgrade", "--pre"), encoding="utf-8")
    spec = {
        "guide": "page.rst",
        "title": "Small reader journey",
        "block": 0,
        "command": 0,
        "span_type": "shell",
        "sha256": digest("pip install supy --upgrade"),
    }
    binding = runner.resolve_install(tmp_path, "HEAD", spec)
    assert binding["command"] == "pip install supy --upgrade"
    assert binding["source_lines"] == [10, 10]
    # A recorder executable is deliberately NOT an installer: no venv or install.
    executable = tmp_path / "pip"
    executable.write_text(
        '#!/bin/sh\nprintf "%s\\n" "$*" "$PIP_NO_INDEX" "$PIP_FIND_LINKS"\n',
        encoding="utf-8",
    )
    executable.chmod(0o755)
    env = dict(
        os.environ, PATH=str(tmp_path), PIP_NO_INDEX="1", PIP_FIND_LINKS="wheelhouse"
    )
    result = environment.install(binding["command"], tmp_path, env)
    assert result["literal_command"] == binding["command"]
    assert result["stdout"] == "install supy --upgrade\n1\nwheelhouse\n"


@pytest.mark.parametrize(
    "command",
    [
        "pip --isolated install supy",
        "pip install https://example.org/supy.whl",
        "pip install supy --index-url https://example.org",
        "pip install ./supy",
    ],
)
def test_install_cannot_escape_wheelhouse(tmp_path, command):
    """An unsupported installer line is a finding, never silently rewritten."""
    with pytest.raises(ValueError, match=r"install|wheelhouse"):
        environment.install(command, tmp_path, {})
