"""Behavioural branch-protection checks; run with python -m unittest discover."""
import os
from pathlib import Path
import subprocess
import unittest

import pytest

pytestmark = [
    pytest.mark.api,
    pytest.mark.skipif(
        os.name == "nt",
        reason="These Linux workflow regressions require POSIX shell executable fixtures",
    ),
]


class GateTests(unittest.TestCase):
    def test_detection_must_succeed(self):
        script = Path(__file__).resolve().parents[2] / ".github/scripts/pr-gate.sh"
        for needs_build in ("true", "false"):
            for status in ("success", "cancelled", "failure", "skipped", "", "unknown"):
                with self.subTest(status=status, needs_build=needs_build):
                    result = subprocess.run(
                        ["bash", str(script)],
                        env={**os.environ, "DETECT_CHANGES_RESULT": status,
                             "NEEDS_BUILD": needs_build,
                             "BUILD_WHEELS_RESULT": "success",
                             "BUILD_MCP_RESULT": "success",
                             "TEST_BRIDGE_RESULT": "success"},
                        capture_output=True, text=True, check=False,
                    )
                    self.assertEqual(result.returncode, 0 if status == "success" else 1,
                                     result.stdout + result.stderr)
