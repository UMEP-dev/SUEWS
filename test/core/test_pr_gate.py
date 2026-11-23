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
    pytest.mark.contract("C-TOOLING"),
]

_UNCONDITIONAL_RESULT_VARS = (
    "SCHEMA_AUDIT_RESULT",
    "DATA_INTERFACE_AUDIT_RESULT",
)

_PATH_FILTERED_PRODUCERS = (
    ("RUST_TEST_RESULT", "RUST_TEST_CHANGED"),
    ("RUST_CLIPPY_RESULT", "RUST_CLIPPY_CHANGED"),
    ("ENCODING_AUDIT_RESULT", "ENCODING_AUDIT_CHANGED"),
    ("WORKFLOW_SECURITY_RESULT", "WORKFLOW_SECURITY_CHANGED"),
    ("DEPENDENCY_AUDIT_RESULT", "DEPENDENCY_AUDIT_CHANGED"),
    ("FORTRAN_UNINIT_RESULT", "FORTRAN_UNINIT_CHANGED"),
    ("RUST_CAPI_OBSERVER_RESULT", "RUST_CAPI_OBSERVER_CHANGED"),
)

_ALL_CONSOLIDATED_ENV_VARS = (
    "EVENT_NAME",
    "CORE_GUARD_RESULT",
    "TEST_TIER",
    "LOWEST_DIRECT_DEPS_RESULT",
    "LOWEST_DIRECT_DEPS_REQUIRED",
    *_UNCONDITIONAL_RESULT_VARS,
    *(var for pair in _PATH_FILTERED_PRODUCERS for var in pair),
)


def _stage2_green_env(event_name: str = "pull_request") -> dict[str, str]:
    """Return a clean Stage 2 caller environment with all 9 producers green/skipped."""
    env: dict[str, str] = {
        "EVENT_NAME": event_name,
        "SCHEMA_AUDIT_RESULT": "success",
        "DATA_INTERFACE_AUDIT_RESULT": "success",
    }
    for result_var, changed_var in _PATH_FILTERED_PRODUCERS:
        env[result_var] = "success"
        env[changed_var] = "true"
    return env


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
    for key in _ALL_CONSOLIDATED_ENV_VARS:
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
                        **_stage2_green_env(),
                        "DETECT_CHANGES_RESULT": status,
                        "NEEDS_BUILD": needs_build,
                    })
                    self.assertEqual(
                        result.returncode,
                        0 if status == "success" else 1,
                        result.stdout + result.stderr,
                    )

    def test_pre_stage2_old_base_workflow_compatibility_and_fail_closed_failures(self):
        # Pre-Stage-1 open PR caller (omits EVENT_NAME and all 9 producer vars)
        pre_stage1 = _run_pr_gate({})
        self.assertEqual(
            pre_stage1.returncode, 0, pre_stage1.stdout + pre_stage1.stderr
        )
        self.assertIn("pre-Stage-2 base", pre_stage1.stdout)

        # Stage 1 open PR caller (passes Stage 1 results without EVENT_NAME or *_CHANGED)
        stage1_green = _run_pr_gate({
            "SCHEMA_AUDIT_RESULT": "success",
            "DATA_INTERFACE_AUDIT_RESULT": "success",
            "RUST_TEST_RESULT": "success",
            "RUST_CLIPPY_RESULT": "skipped",
            "ENCODING_AUDIT_RESULT": "success",
            "WORKFLOW_SECURITY_RESULT": "skipped",
            "DEPENDENCY_AUDIT_RESULT": "skipped",
        })
        self.assertEqual(
            stage1_green.returncode, 0, stage1_green.stdout + stage1_green.stderr
        )

        # Even for pre-Stage-2 callers, reported failures and unconditional skips fail closed
        stage1_fail = _run_pr_gate({
            "SCHEMA_AUDIT_RESULT": "success",
            "DATA_INTERFACE_AUDIT_RESULT": "success",
            "RUST_TEST_RESULT": "failure",
        })
        self.assertEqual(stage1_fail.returncode, 1, stage1_fail.stdout)
        self.assertIn("rust_test", stage1_fail.stdout)

        uncond_skip = _run_pr_gate({
            "SCHEMA_AUDIT_RESULT": "skipped",
            "DATA_INTERFACE_AUDIT_RESULT": "success",
        })
        self.assertEqual(uncond_skip.returncode, 1, uncond_skip.stdout)

    def test_stage2_all_nine_producers_fail_closed_across_pr_and_merge_group(self):
        for event_name in ("pull_request", "merge_group"):
            with self.subTest(event=event_name, case="all-success"):
                res = _run_pr_gate(_stage2_green_env(event_name))
                self.assertEqual(res.returncode, 0, res.stdout + res.stderr)
                self.assertIn("[OK] fortran_uninit: success", res.stdout)
                self.assertIn("[OK] rust_capi_observer: success", res.stdout)

            # Unconditional audits fail closed on skipped, failure, cancelled, or empty
            for var in _UNCONDITIONAL_RESULT_VARS:
                for bad in ("skipped", "failure", "cancelled", ""):
                    with self.subTest(event=event_name, var=var, bad=bad):
                        env = {**_stage2_green_env(event_name), var: bad}
                        res = _run_pr_gate(env)
                        self.assertEqual(res.returncode, 1, res.stdout + res.stderr)

            # Path-filtered producers:
            # - changed=false + skipped -> PASS
            # - changed=true + skipped  -> FAIL CLOSED (unexpected skip)
            # - changed=true + failure/cancelled -> FAIL CLOSED
            # - changed=false + failure -> FAIL CLOSED
            # - invalid/empty changed flag -> FAIL CLOSED
            for result_var, changed_var in _PATH_FILTERED_PRODUCERS:
                with self.subTest(
                    event=event_name, var=result_var, case="unchanged-skip-ok"
                ):
                    env = {
                        **_stage2_green_env(event_name),
                        changed_var: "false",
                        result_var: "skipped",
                    }
                    res = _run_pr_gate(env)
                    self.assertEqual(res.returncode, 0, res.stdout + res.stderr)

                for bad_status in ("skipped", "failure", "cancelled", ""):
                    with self.subTest(
                        event=event_name,
                        var=result_var,
                        case=f"changed-true-{bad_status}",
                    ):
                        env = {
                            **_stage2_green_env(event_name),
                            changed_var: "true",
                            result_var: bad_status,
                        }
                        res = _run_pr_gate(env)
                        self.assertEqual(res.returncode, 1, res.stdout + res.stderr)

                with self.subTest(
                    event=event_name, var=result_var, case="unchanged-failure-fails"
                ):
                    env = {
                        **_stage2_green_env(event_name),
                        changed_var: "false",
                        result_var: "failure",
                    }
                    res = _run_pr_gate(env)
                    self.assertEqual(res.returncode, 1, res.stdout + res.stderr)

                with self.subTest(
                    event=event_name, var=changed_var, case="empty-changed-fails"
                ):
                    env = {
                        **_stage2_green_env(event_name),
                        changed_var: "",
                        result_var: "skipped",
                    }
                    res = _run_pr_gate(env)
                    self.assertEqual(res.returncode, 1, res.stdout + res.stderr)

    def test_stage2_missing_env_vars_fail_closed(self):
        for event_name in ("pull_request", "merge_group"):
            # Missing result var in Stage 2 mode must fail closed
            for missing_var in (
                *_UNCONDITIONAL_RESULT_VARS,
                *(rv for rv, _ in _PATH_FILTERED_PRODUCERS),
                *(cv for _, cv in _PATH_FILTERED_PRODUCERS),
            ):
                with self.subTest(event=event_name, missing=missing_var):
                    env = _stage2_green_env(event_name)
                    env.pop(missing_var)
                    res = _run_pr_gate(env)
                    self.assertEqual(res.returncode, 1, res.stdout + res.stderr)

    def test_core_required_gates_remain_fail_closed(self):
        for var in (
            "BUILD_WHEELS_RESULT",
            "BUILD_MCP_RESULT",
            "TEST_BRIDGE_RESULT",
            "CHECK_MARKERS_RESULT",
            "PHYSICS_LABEL_RESULT",
            "LOWEST_DIRECT_DEPS_RESULT",
        ):
            with self.subTest(failing_var=var):
                result = _run_pr_gate({**_stage2_green_env(), var: "failure"})
                self.assertEqual(result.returncode, 1, result.stdout + result.stderr)

    def test_lowest_direct_deps_gate_allows_skipped_and_fails_on_failure(self):
        for event_name in ("pull_request", "merge_group"):
            for required, status, expected_rc in (
                ("false", "success", 0),
                ("false", "skipped", 0),
                ("false", "failure", 1),
                ("false", "cancelled", 1),
                ("true", "success", 0),
                ("true", "skipped", 1),
                ("true", "failure", 1),
                ("true", "cancelled", 1),
            ):
                with self.subTest(event=event_name, required=required, status=status):
                    result = _run_pr_gate({
                        **_stage2_green_env(event_name),
                        "LOWEST_DIRECT_DEPS_REQUIRED": required,
                        "LOWEST_DIRECT_DEPS_RESULT": status,
                    })
                    self.assertEqual(
                        result.returncode, expected_rc, result.stdout + result.stderr
                    )

    def test_core_guard_required_at_ready_tiers_when_the_wheel_built(self):
        cases = (
            # tier, build_wheels, core_guard, expected rc
            ("standard", "success", "success", 0),
            ("physics-full", "success", "success", 0),
            ("standard", "success", "skipped", 1),
            ("standard", "success", "failure", 1),
            ("physics-full", "success", "cancelled", 1),
            ("standard", "success", "unset", 1),
            # Draft tiers run a subset; the guard is skipped there.
            ("smoke", "success", "skipped", 0),
            ("core", "success", "skipped", 0),
            ("smoke", "success", "failure", 1),
        )
        for event_name in ("pull_request", "merge_group"):
            for tier, wheels, guard, expected_rc in cases:
                with self.subTest(event=event_name, tier=tier, guard=guard):
                    env = {
                        **_stage2_green_env(event_name),
                        "TEST_TIER": tier,
                        "BUILD_WHEELS_RESULT": wheels,
                        "TEST_BRIDGE_RESULT": wheels,
                    }
                    if guard != "unset":
                        env["CORE_GUARD_RESULT"] = guard
                    result = _run_pr_gate(env)
                    self.assertEqual(
                        result.returncode, expected_rc, result.stdout + result.stderr
                    )

    def test_docs_only_run_needs_no_core_guard(self):
        """needs-build false skips every test lane: the one documented exemption."""
        result = _run_pr_gate({
            **_stage2_green_env(),
            "NEEDS_BUILD": "false",
            "TEST_TIER": "standard",
            "BUILD_WHEELS_RESULT": "skipped",
            "BUILD_MCP_RESULT": "skipped",
            "TEST_BRIDGE_RESULT": "skipped",
            "CORE_GUARD_RESULT": "skipped",
        })
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
