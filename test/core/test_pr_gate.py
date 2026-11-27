"""Behavioural branch-protection checks; run with python -m unittest discover."""

import os
from pathlib import Path
import subprocess
import unittest

import pytest

pytestmark = [
    pytest.mark.tooling,
    pytest.mark.skipif(
        os.name == "nt",
        reason="These Linux workflow regressions require POSIX shell executable fixtures",
    ),
    pytest.mark.contract("C-TOOLING"),
]


@pytest.fixture(autouse=True)
def _isolate_axis_schema_env(monkeypatch: pytest.MonkeyPatch) -> None:
    """Keep tests isolated from an ambient GATE_AXIS_SCHEMA=2 CI environment."""
    monkeypatch.delenv("GATE_AXIS_SCHEMA", raising=False)


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

_SCHEMA2_LANE_VARS = (
    "TOOLING_RESULT",
    "UNIT_RESULT",
    "NATIVE_RESULT",
    "PHYSICS_RESULT",
    "OS_COMPAT_RESULT",
    "OS_COMPAT_REQUIRED",
    "ABI3_SMOKE_RESULT",
    "ABI3_SMOKE_PYTHON",
    "ABI3_SMOKE_REQUIRED",
)

_ALL_CONSOLIDATED_ENV_VARS = (
    "GATE_AXIS_SCHEMA",
    "EVENT_NAME",
    "CORE_GUARD_RESULT",
    "TEST_TIER",
    "LOWEST_DIRECT_DEPS_RESULT",
    "LOWEST_DIRECT_DEPS_REQUIRED",
    *_UNCONDITIONAL_RESULT_VARS,
    *(var for pair in _PATH_FILTERED_PRODUCERS for var in pair),
    *_SCHEMA2_LANE_VARS,
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


def _schema2_green_env(
    event_name: str = "pull_request",
    tier: str = "physics-core",
) -> dict[str, str]:
    """Return a clean Schema 2 caller environment with all required lanes green."""
    return {
        **_stage2_green_env(event_name),
        "GATE_AXIS_SCHEMA": "2",
        "TEST_TIER": tier,
        "CORE_GUARD_RESULT": "success",
        "TOOLING_RESULT": "success",
        "UNIT_RESULT": "success",
        "NATIVE_RESULT": "success",
        "PHYSICS_RESULT": "success",
        "OS_COMPAT_RESULT": "success",
        "ABI3_SMOKE_RESULT": "success" if event_name == "pull_request" else "skipped",
    }


def _run_pr_gate(
    env_overrides: dict[str, str],
    *,
    drop_vars: tuple[str, ...] = (),
) -> subprocess.CompletedProcess[str]:
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
    for key in drop_vars:
        base_env.pop(key, None)
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

    def test_schema1_parity_and_invalid_gate_axis_schema_fails_closed(self):
        """Unset, empty, and '1' match pinned Schema 1 output; invalid values fail closed."""
        base_env = {
            **_stage2_green_env(),
            "TEST_TIER": "standard",
            "CORE_GUARD_RESULT": "success",
        }
        unset = _run_pr_gate(base_env)
        empty = _run_pr_gate({**base_env, "GATE_AXIS_SCHEMA": ""})
        explicit1 = _run_pr_gate({**base_env, "GATE_AXIS_SCHEMA": "1"})
        self.assertEqual(unset.returncode, 0, unset.stdout + unset.stderr)
        self.assertEqual(empty.returncode, 0, empty.stdout + empty.stderr)
        self.assertEqual(explicit1.returncode, 0, explicit1.stdout + explicit1.stderr)
        self.assertEqual(empty.stdout, unset.stdout)
        self.assertEqual(explicit1.stdout, unset.stdout)
        # Pin concrete base-branch Schema 1 stdout lines (not just self-consistency).
        for expected_line in (
            "=== Job Results ===",
            "api cross-CPython lanes: success",
            "[OK] Every test file declares physics or api",
            "[OK] Standard build passed",
            "[OK] API tests passed on all matrix cells",
            "[OK] Every core test passed on Linux",
            "[OK] All validations passed",
        ):
            self.assertIn(expected_line, unset.stdout)
        self.assertNotIn("gate_axis_schema", unset.stdout)

        # Schema 1 failure path matches pinned base behaviour across unset / '1'.
        fail_unset = _run_pr_gate({**base_env, "TEST_BRIDGE_RESULT": "failure"})
        fail_v1 = _run_pr_gate({
            **base_env,
            "GATE_AXIS_SCHEMA": "1",
            "TEST_BRIDGE_RESULT": "failure",
        })
        self.assertEqual(fail_unset.returncode, 1)
        self.assertEqual(fail_v1.returncode, 1)
        self.assertEqual(fail_v1.stdout, fail_unset.stdout)
        self.assertIn("[X] API cross-CPython tests failed", fail_unset.stdout)

        for bad in ("0", "3", "v2", "true", " 2", "2 "):
            with self.subTest(bad_schema=bad):
                res = _run_pr_gate({**base_env, "GATE_AXIS_SCHEMA": bad})
                self.assertEqual(res.returncode, 1, res.stdout + res.stderr)
                self.assertIn("Invalid GATE_AXIS_SCHEMA", res.stderr)

    def test_schema2_green_across_ready_tiers_and_merge_group(self):
        """Schema 2 passes when every required axis lane and core_guard succeed."""
        for event_name, tiers in (
            ("pull_request", ("physics-core", "physics-full")),
            ("merge_group", ("physics-full",)),
        ):
            for tier in tiers:
                with self.subTest(event=event_name, tier=tier):
                    env = _schema2_green_env(event_name, tier)
                    # Schema 2 callers need not pass legacy TEST_BRIDGE_RESULT.
                    res = _run_pr_gate(env, drop_vars=("TEST_BRIDGE_RESULT",))
                    self.assertEqual(res.returncode, 0, res.stdout + res.stderr)
                    self.assertIn("gate_axis_schema: 2", res.stdout)
                    self.assertIn("[OK] tooling: success", res.stdout)
                    self.assertIn("[OK] unit: success", res.stdout)
                    self.assertIn("[OK] native: success", res.stdout)
                    self.assertIn("[OK] physics: success", res.stdout)
                    self.assertIn("[OK] os_compat: success", res.stdout)

    def test_schema2_required_lanes_fail_closed_on_missing_skipped_or_failed(self):
        """Every required Schema 2 lane fails closed on unset, empty, skipped, failure, or cancelled."""
        required_pr_lanes = (
            "TOOLING_RESULT",
            "UNIT_RESULT",
            "NATIVE_RESULT",
            "PHYSICS_RESULT",
            "OS_COMPAT_RESULT",
            "ABI3_SMOKE_RESULT",
        )
        for lane_var in required_pr_lanes:
            with self.subTest(lane=lane_var, case="missing"):
                env = _schema2_green_env("pull_request", "physics-core")
                env.pop(lane_var)
                res = _run_pr_gate(env, drop_vars=("TEST_BRIDGE_RESULT",))
                self.assertEqual(res.returncode, 1, res.stdout + res.stderr)

            for bad_status in ("", "skipped", "failure", "cancelled", "unknown"):
                with self.subTest(lane=lane_var, status=bad_status):
                    env = {
                        **_schema2_green_env("pull_request", "physics-core"),
                        lane_var: bad_status,
                    }
                    res = _run_pr_gate(env, drop_vars=("TEST_BRIDGE_RESULT",))
                    self.assertEqual(res.returncode, 1, res.stdout + res.stderr)

        # On merge_group, abi3_smoke defaults to not required (skipped passes),
        # but failure/cancelled/unset/empty or explicit ABI3_SMOKE_REQUIRED=true fail closed.
        for bad_mq_abi3 in ("failure", "cancelled", "", "unset"):
            with self.subTest(merge_group_abi3=bad_mq_abi3):
                env = _schema2_green_env("merge_group", "physics-full")
                if bad_mq_abi3 == "unset":
                    env.pop("ABI3_SMOKE_RESULT")
                else:
                    env["ABI3_SMOKE_RESULT"] = bad_mq_abi3
                res = _run_pr_gate(env, drop_vars=("TEST_BRIDGE_RESULT",))
                self.assertEqual(res.returncode, 1, res.stdout + res.stderr)

        for req_var, res_var in (
            ("OS_COMPAT_REQUIRED", "OS_COMPAT_RESULT"),
            ("ABI3_SMOKE_REQUIRED", "ABI3_SMOKE_RESULT"),
        ):
            with self.subTest(explicit_required=req_var):
                env = {
                    **_schema2_green_env("merge_group", "physics-full"),
                    req_var: "true",
                    res_var: "skipped",
                }
                res = _run_pr_gate(env, drop_vars=("TEST_BRIDGE_RESULT",))
                self.assertEqual(res.returncode, 1, res.stdout + res.stderr)

            # Explicit =false overrides cannot suppress mandatory wheel-building PR coverage.
            with self.subTest(suppress_mandatory=req_var):
                env = {
                    **_schema2_green_env("pull_request", "physics-core"),
                    req_var: "false",
                    res_var: "skipped",
                }
                res = _run_pr_gate(env, drop_vars=("TEST_BRIDGE_RESULT",))
                self.assertEqual(res.returncode, 1, res.stdout + res.stderr)

            for bad_flag in ("", "yes", "1"):
                with self.subTest(invalid_flag=req_var, value=bad_flag):
                    env = {
                        **_schema2_green_env("pull_request", "physics-core"),
                        req_var: bad_flag,
                    }
                    res = _run_pr_gate(env, drop_vars=("TEST_BRIDGE_RESULT",))
                    self.assertEqual(res.returncode, 1, res.stdout + res.stderr)

        # Even on merge_group, OS_COMPAT_REQUIRED=false cannot suppress mandatory os_compat when wheels build.
        res_mq_suppress = _run_pr_gate(
            {
                **_schema2_green_env("merge_group", "physics-full"),
                "OS_COMPAT_REQUIRED": "false",
                "OS_COMPAT_RESULT": "skipped",
            },
            drop_vars=("TEST_BRIDGE_RESULT",),
        )
        self.assertEqual(
            res_mq_suppress.returncode,
            1,
            res_mq_suppress.stdout + res_mq_suppress.stderr,
        )

        # Trusted ABI3_SMOKE_PYTHON="[]" (e.g. requires-python >=3.15 PR with no extra CPython)
        # permits ABI3_SMOKE_RESULT=skipped, while non-empty or malformed lists still fail closed.
        res_empty_abi3_ok = _run_pr_gate(
            {
                **_schema2_green_env("pull_request", "physics-core"),
                "ABI3_SMOKE_PYTHON": "[]",
                "ABI3_SMOKE_RESULT": "skipped",
            },
            drop_vars=("TEST_BRIDGE_RESULT",),
        )
        self.assertEqual(
            res_empty_abi3_ok.returncode,
            0,
            res_empty_abi3_ok.stdout + res_empty_abi3_ok.stderr,
        )

        res_nonempty_abi3_skip = _run_pr_gate(
            {
                **_schema2_green_env("pull_request", "physics-core"),
                "ABI3_SMOKE_PYTHON": '["cp315"]',
                "ABI3_SMOKE_RESULT": "skipped",
            },
            drop_vars=("TEST_BRIDGE_RESULT",),
        )
        self.assertEqual(
            res_nonempty_abi3_skip.returncode,
            1,
            res_nonempty_abi3_skip.stdout + res_nonempty_abi3_skip.stderr,
        )

        for bad_abi3_json in ("", "not-json", "{}", '["bad-tag"]', "[315]"):
            with self.subTest(bad_abi3_python=bad_abi3_json):
                res_bad_json = _run_pr_gate(
                    {
                        **_schema2_green_env("pull_request", "physics-core"),
                        "ABI3_SMOKE_PYTHON": bad_abi3_json,
                    },
                    drop_vars=("TEST_BRIDGE_RESULT",),
                )
                self.assertEqual(
                    res_bad_json.returncode,
                    1,
                    res_bad_json.stdout + res_bad_json.stderr,
                )

    def test_schema2_core_guard_enforces_event_tier_policy_and_rejects_reduced_tiers(
        self,
    ):
        """Schema 2 requires event-valid tiers and core_guard==success on every wheel build."""
        cases = (
            # event_name, physics_change, tier, core_guard, expected_rc
            ("pull_request", "false", "physics-core", "success", 0),
            ("pull_request", "false", "physics-full", "success", 0),
            ("pull_request", "true", "physics-full", "success", 0),
            ("merge_group", "false", "physics-full", "success", 0),
            # Missing, skipped, failed, or cancelled core_guard fails closed.
            ("pull_request", "false", "physics-core", "skipped", 1),
            ("pull_request", "false", "physics-core", "failure", 1),
            ("pull_request", "false", "physics-core", "cancelled", 1),
            ("pull_request", "false", "physics-core", "unset", 1),
            ("pull_request", "false", "physics-full", "skipped", 1),
            ("merge_group", "false", "physics-full", "skipped", 1),
            # Reduced/non-policy tiers cannot bypass Schema 2 core_guard or full physics.
            ("pull_request", "false", "smoke", "skipped", 1),
            ("pull_request", "false", "smoke", "success", 1),
            ("pull_request", "false", "core", "skipped", 1),
            ("pull_request", "false", "cfg", "skipped", 1),
            ("pull_request", "false", "standard", "success", 1),
            ("pull_request", "false", "all", "skipped", 1),
            ("pull_request", "true", "physics-core", "success", 1),
            ("merge_group", "false", "physics-core", "success", 1),
            ("merge_group", "false", "standard", "success", 1),
            ("merge_group", "false", "smoke", "skipped", 1),
            ("pull_request", "false", "", "success", 1),
            ("pull_request", "false", "physics_core", "success", 1),
        )
        for event_name, phys_change, tier, guard, expected_rc in cases:
            with self.subTest(
                event=event_name,
                physics_change=phys_change,
                tier=tier,
                guard=guard,
            ):
                env = {
                    **_schema2_green_env(event_name, tier),
                    "PHYSICS_CHANGE": phys_change,
                }
                if guard == "unset":
                    env.pop("CORE_GUARD_RESULT")
                else:
                    env["CORE_GUARD_RESULT"] = guard
                res = _run_pr_gate(env, drop_vars=("TEST_BRIDGE_RESULT",))
                self.assertEqual(res.returncode, expected_rc, res.stdout + res.stderr)

    def test_schema2_docs_only_and_physics_change_without_code_filters(self):
        """Schema 2 handles docs-only PRs and label-only physics PRs fail-closed."""
        docs_env = {
            **_schema2_green_env("pull_request", "physics-core"),
            "NEEDS_BUILD": "false",
            "BUILD_WHEELS_RESULT": "skipped",
            "BUILD_MCP_RESULT": "skipped",
            "TOOLING_RESULT": "skipped",
            "UNIT_RESULT": "skipped",
            "NATIVE_RESULT": "skipped",
            "PHYSICS_RESULT": "skipped",
            "OS_COMPAT_RESULT": "skipped",
            "ABI3_SMOKE_RESULT": "skipped",
            "CORE_GUARD_RESULT": "skipped",
        }
        res_docs = _run_pr_gate(docs_env, drop_vars=("TEST_BRIDGE_RESULT",))
        self.assertEqual(res_docs.returncode, 0, res_docs.stdout + res_docs.stderr)

        # Missing or failing lane on docs-only still fails closed in Schema 2.
        for bad_var in (
            "TOOLING_RESULT",
            "UNIT_RESULT",
            "NATIVE_RESULT",
            "PHYSICS_RESULT",
            "CORE_GUARD_RESULT",
        ):
            with self.subTest(docs_missing=bad_var):
                env = dict(docs_env)
                env.pop(bad_var)
                res = _run_pr_gate(env, drop_vars=("TEST_BRIDGE_RESULT",))
                self.assertEqual(res.returncode, 1, res.stdout + res.stderr)

        # Label-only physics change (NEEDS_BUILD=false, PHYSICS_CHANGE=true):
        # wheels are required, so native, physics, os_compat, and (on PR) abi3_smoke must succeed.
        label_only = {
            **docs_env,
            "PHYSICS_CHANGE": "true",
            "TEST_TIER": "physics-full",
            "BUILD_WHEELS_RESULT": "success",
            "NATIVE_RESULT": "success",
            "PHYSICS_RESULT": "success",
            "OS_COMPAT_RESULT": "success",
            "ABI3_SMOKE_RESULT": "success",
            "CORE_GUARD_RESULT": "success",
        }
        res_label_ok = _run_pr_gate(label_only, drop_vars=("TEST_BRIDGE_RESULT",))
        self.assertEqual(
            res_label_ok.returncode, 0, res_label_ok.stdout + res_label_ok.stderr
        )

        for req_lane in (
            "NATIVE_RESULT",
            "PHYSICS_RESULT",
            "OS_COMPAT_RESULT",
            "ABI3_SMOKE_RESULT",
        ):
            with self.subTest(label_only_skip=req_lane):
                res_skip = _run_pr_gate(
                    {**label_only, req_lane: "skipped"},
                    drop_vars=("TEST_BRIDGE_RESULT",),
                )
                self.assertEqual(
                    res_skip.returncode, 1, res_skip.stdout + res_skip.stderr
                )

        # Label-only physics change on merge_group allows abi3_smoke=skipped,
        # but still requires native, physics, and os_compat.
        mq_label_only = {
            **label_only,
            "EVENT_NAME": "merge_group",
            "ABI3_SMOKE_RESULT": "skipped",
        }
        res_mq_label_ok = _run_pr_gate(mq_label_only, drop_vars=("TEST_BRIDGE_RESULT",))
        self.assertEqual(
            res_mq_label_ok.returncode,
            0,
            res_mq_label_ok.stdout + res_mq_label_ok.stderr,
        )

        # Schema 2 never falls back to pre-Stage-2 lenient mode when EVENT_NAME is unset.
        no_event = _schema2_green_env("pull_request", "physics-core")
        no_event.pop("EVENT_NAME")
        res_no_event = _run_pr_gate(no_event, drop_vars=("TEST_BRIDGE_RESULT",))
        self.assertEqual(
            res_no_event.returncode, 1, res_no_event.stdout + res_no_event.stderr
        )

        # Invalid NEEDS_BUILD or failing legacy TEST_BRIDGE_RESULT fails closed.
        res_bad_nb = _run_pr_gate(
            {**_schema2_green_env(), "NEEDS_BUILD": ""},
            drop_vars=("TEST_BRIDGE_RESULT",),
        )
        self.assertEqual(
            res_bad_nb.returncode, 1, res_bad_nb.stdout + res_bad_nb.stderr
        )
        res_bad_bridge = _run_pr_gate(
            {**_schema2_green_env(), "TEST_BRIDGE_RESULT": "failure"},
        )
        self.assertEqual(
            res_bad_bridge.returncode, 1, res_bad_bridge.stdout + res_bad_bridge.stderr
        )
