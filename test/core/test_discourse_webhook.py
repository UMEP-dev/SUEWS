"""Collect the Web Crypto webhook regressions in the normal pytest suite."""

from pathlib import Path
import shutil
import subprocess

import pytest

pytestmark = pytest.mark.api


def test_webhook_boundaries():
    node = shutil.which("node")
    if node is None:
        pytest.skip("Node.js is not installed; required for the webhook Web Crypto tests")
    result = subprocess.run(
        [node, "--test", str(Path(__file__).with_name("worker.test.mjs"))],
        capture_output=True, text=True, timeout=30, check=False,
    )
    assert result.returncode == 0, result.stdout + result.stderr
