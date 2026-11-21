"""The executed page comes from the requested Git revision."""

from pathlib import Path
import subprocess
import sys

import pytest

pytest.importorskip("docutils", reason="Source binding requires optional docutils")
ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT))
from scripts.suews.guide_journeys import runner  # noqa: E402

pytestmark = pytest.mark.api


def test_revision_read_ignores_dirty_page(tmp_path):
    """A dirty checkout cannot silently replace the recorded source."""
    def git(*args):
        return subprocess.check_output(["git", "-C", str(tmp_path), *args], text=True).strip()

    git("init", "-q")
    page = tmp_path / "guide.rst"
    page.write_text("committed page\n", encoding="utf-8")
    git("add", "guide.rst")
    git("-c", "user.name=Test", "-c", "user.email=test@example.org", "commit", "-qm", "Page")
    revision = git("rev-parse", "HEAD")
    page.write_text("uncommitted replacement\n", encoding="utf-8")
    assert runner.read_page(tmp_path, revision, "guide.rst") == "committed page\n"
    with pytest.raises(subprocess.CalledProcessError):
        runner.read_page(tmp_path, "missing-revision", "guide.rst")
