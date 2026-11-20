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


def _run_pr_gate(env_overrides: dict[str, str]) -> subprocess.CompletedProcess[str]:
    script = Path(__file__).resolve().parents[2] / ".github/scripts/pr-gate.sh"
    base_env = {
        **os.environ,
        "DETECT_CHANGES_RESULT": "success",
        "NEEDS_BUILD": "true",
        "BUILD_WHEELS_RESULT": "success",
        "BUILD_MCP_RESULT": "success",
        "TEST_BRIDGE_RESULT": "success",
        "CHECK_MARKERS_RESULT": "success",
        "PHYSICS_LABEL_RESULT": "success",
        "PHYSICS_CHANGE": "false",
    }
    for key in (
        "RUST_TEST_RESULT",
        "RUST_CLIPPY_RESULT",
        "SCHEMA_AUDIT_RESULT",
        "DATA_INTERFACE_AUDIT_RESULT",
        "ENCODING_AUDIT_RESULT",
        "WORKFLOW_SECURITY_RESULT",
        "DEPENDENCY_AUDIT_RESULT",
    ):
        base_env.pop(key, None)
    base_env.update(env_overrides)
    return subprocess.run(
        ["bash", str(script)],
        env=base_env,
        capture_output=True,
        text=True,
        check=False,
    )


class GateTests(unittest.TestCase):
    def test_detection_must_succeed(self):
        for needs_build in ("true", "false"):
            for status in ("success", "cancelled", "failure", "skipped", "", "unknown"):
                with self.subTest(status=status, needs_build=needs_build):
                    result = _run_pr_gate({
                        "DETECT_CHANGES_RESULT": status,
                        "NEEDS_BUILD": needs_build,
                    })
                    self.assertEqual(
                        result.returncode,
                        0 if status == "success" else 1,
                        result.stdout + result.stderr,
                    )

    def test_old_base_workflow_without_stage1_env_vars_still_passes(self):
        result = _run_pr_gate({})
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertIn(
            "not reported by caller workflow (pre-Stage-1 base)", result.stdout
        )

    def test_stage1_consolidated_producers_observability_mode(self):
        all_green = _run_pr_gate({
            "RUST_TEST_RESULT": "success",
            "RUST_CLIPPY_RESULT": "success",
            "SCHEMA_AUDIT_RESULT": "success",
            "DATA_INTERFACE_AUDIT_RESULT": "success",
            "ENCODING_AUDIT_RESULT": "success",
            "WORKFLOW_SECURITY_RESULT": "success",
            "DEPENDENCY_AUDIT_RESULT": "skipped",
        })
        self.assertEqual(all_green.returncode, 0, all_green.stdout + all_green.stderr)
        self.assertIn("[OK] rust_test: success", all_green.stdout)
        self.assertIn("[OK] schema_version_audit: success", all_green.stdout)
        self.assertIn("[INFO] dependency_audit: skipped", all_green.stdout)

        # Stage 1 logs a warning for a non-success folded producer without
        # turning on Stage 2 fail-closed enforcement before Stage 1 lands on master.
        stage1_warn = _run_pr_gate({
            "RUST_TEST_RESULT": "failure",
            "SCHEMA_AUDIT_RESULT": "success",
            "DATA_INTERFACE_AUDIT_RESULT": "success",
        })
        self.assertEqual(
            stage1_warn.returncode, 0, stage1_warn.stdout + stage1_warn.stderr
        )
        self.assertIn(
            "::warning::Consolidated producer 'rust_test' finished with status 'failure'",
            stage1_warn.stdout,
        )
        self.assertIn("Stage 2 W1-1b", stage1_warn.stdout)

    def test_core_required_gates_remain_fail_closed(self):
        for var in (
            "BUILD_WHEELS_RESULT",
            "BUILD_MCP_RESULT",
            "TEST_BRIDGE_RESULT",
            "CHECK_MARKERS_RESULT",
            "PHYSICS_LABEL_RESULT",
        ):
            with self.subTest(failing_var=var):
                result = _run_pr_gate({var: "failure"})
                self.assertEqual(result.returncode, 1, result.stdout + result.stderr)
