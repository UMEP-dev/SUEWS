"""Contract tests for the per-platform test layout (scripts/suews/ci_test_layout.py)."""

from __future__ import annotations

import json

import pytest

from scripts.suews.ci_test_layout import (
    SCHEMA_V2_UNSHARDED_TIERS,
    UNSHARDED_TIERS,
    abi3_smoke_cells,
    api_cells,
    effective_split_tests,
    main,
    native_cells,
    os_compat_cells,
    physics_shards,
)

pytestmark = [pytest.mark.api, pytest.mark.contract("C-TOOLING")]


def test_ready_linux_pull_request_layout() -> None:
    """cp312 in four shards at the full tier, cp315 whole at core-bookend."""
    cells = api_cells(["cp312", "cp315"], "standard", "cp312", 4, True, "cp312")
    assert cells == [
        *(
            {"python_version": "cp312", "shard": f"{k}-of-4", "tier": "standard"}
            for k in range(1, 5)
        ),
        {"python_version": "cp315", "shard": "", "tier": "core-bookend"},
    ]
    assert physics_shards(True, "standard", 2) == ["1-of-2", "2-of-2"]
    assert physics_shards(True, "physics-full", 3) == ["1-of-3", "2-of-3", "3-of-3"]


def test_merge_queue_and_nightly_layouts() -> None:
    queue = api_cells(["cp312"], "standard", "cp312", 4, False, "cp312")
    assert [c["shard"] for c in queue] == ["1-of-4", "2-of-4", "3-of-4", "4-of-4"]
    assert {c["tier"] for c in queue} == {"standard"}
    nightly = api_cells(
        ["cp312", "cp313", "cp314", "cp315"], "all", "", 2, False, "cp312"
    )
    assert nightly == [
        {"python_version": p, "shard": "", "tier": "all"}
        for p in ("cp312", "cp313", "cp314", "cp315")
    ]
    assert physics_shards(False, "all", 2) == [""]


@pytest.mark.parametrize("tier", ["smoke", "core", "cfg"])
def test_draft_tiers_are_never_sharded(tier: str) -> None:
    """An empty shard would exit pytest with code 5 and must stay inside the wheel job (#2284)."""
    for shard_python in ("cp312", "all"):
        cells = api_cells(["cp312", "cp315"], tier, shard_python, 4, False, "cp312")
        assert all(not c["shard"] for c in cells)
    assert physics_shards(True, tier, 2) == [""]
    assert effective_split_tests(True, tier) is False


def test_effective_split_tests_and_cli_output(tmp_path, monkeypatch, capsys) -> None:
    assert effective_split_tests(True, "standard") is True
    assert effective_split_tests(True, "physics-full") is True
    assert effective_split_tests(False, "standard") is False

    output = tmp_path / "smoke_out"
    monkeypatch.setenv("GITHUB_OUTPUT", str(output))
    assert (
        main([
            "--api-python",
            '["cp312"]',
            "--tier",
            "smoke",
            "--split-tests",
            "--physics-shards",
            "2",
        ])
        == 0
    )
    lines = dict(
        line.split("=", 1) for line in output.read_text(encoding="utf-8").splitlines()
    )
    assert lines["effective_split_tests"] == "false"
    assert json.loads(lines["physics_shards"]) == [""]


def test_split_layout_has_at_least_two_physics_shards() -> None:
    """One unsharded physics job would reuse the wheel job's metrics artefact name."""
    assert physics_shards(True, "standard", 1) == ["1-of-2", "2-of-2"]


def test_windows_full_matrix_layout_shards_all_cells() -> None:
    """Windows pull-request full-matrix layout splits both cp312 and cp315 into two shards (#2240)."""
    cells = api_cells(["cp312", "cp315"], "standard", "all", 2, False, "cp312")
    assert [(c["python_version"], c["shard"]) for c in cells] == [
        ("cp312", "1-of-2"),
        ("cp312", "2-of-2"),
        ("cp315", "1-of-2"),
        ("cp315", "2-of-2"),
    ]
    single = api_cells(["cp312", "cp315"], "standard", "cp312", 2, False, "cp312")
    assert [(c["python_version"], c["shard"]) for c in single] == [
        ("cp312", "1-of-2"),
        ("cp312", "2-of-2"),
        ("cp315", ""),
    ]


def test_cli_writes_github_outputs(tmp_path, monkeypatch, capsys) -> None:
    output = tmp_path / "out"
    monkeypatch.setenv("GITHUB_OUTPUT", str(output))
    assert (
        main([
            "--api-python",
            '["cp312","cp315"]',
            "--tier",
            "standard",
            "--shard-python",
            "cp312",
            "--shard-count",
            "4",
            "--bookend-core",
            "--split-tests",
            "--physics-shards",
            "2",
        ])
        == 0
    )
    lines = dict(
        line.split("=", 1) for line in output.read_text(encoding="utf-8").splitlines()
    )
    assert len(json.loads(lines["api_cells"])) == 5
    assert json.loads(lines["physics_shards"]) == ["1-of-2", "2-of-2"]
    assert lines["effective_split_tests"] == "true"


def test_schema2_python_pr_layout_keeps_physics_core_unsharded_and_shards_native() -> (
    None
):
    """In schema 2, physics-core stays in the wheel job while native shards normally."""
    assert UNSHARDED_TIERS == ("smoke", "core", "cfg")
    assert SCHEMA_V2_UNSHARDED_TIERS == ("smoke", "core", "cfg", "physics-core")
    assert effective_split_tests(True, "physics-core", schema=2) is False
    assert physics_shards(True, "physics-core", 2, schema=2) == [""]
    assert native_cells("cp312", "physics-core", shard_count=2) == [
        {
            "python_version": "cp312",
            "shard": "1-of-2",
            "tier": "physics-core",
            "lane": "native",
        },
        {
            "python_version": "cp312",
            "shard": "2-of-2",
            "tier": "physics-core",
            "lane": "native",
        },
    ]
    assert abi3_smoke_cells(["cp312", "cp315"], "physics-core", "cp312") == [
        {
            "python_version": "cp315",
            "shard": "",
            "tier": "physics-core",
            "lane": "abi3_smoke",
        },
    ]
    assert os_compat_cells("cp312", "physics-core", include=True) == [
        {
            "python_version": "cp312",
            "shard": "",
            "tier": "physics-core",
            "lane": "os_compat",
        },
    ]
    assert os_compat_cells("cp312", "physics-core", include=False) == []


def test_schema2_full_physics_merge_queue_nightly_and_draft_layouts() -> None:
    """Schema 2 splits full physics, drops abi3_smoke on merge queue, and keeps drafts unsharded."""
    assert effective_split_tests(True, "standard", schema=2) is True
    assert physics_shards(True, "standard", 2, schema=2) == ["1-of-2", "2-of-2"]
    assert physics_shards(True, "physics-full", 3, schema=2) == [
        "1-of-3",
        "2-of-3",
        "3-of-3",
    ]
    assert abi3_smoke_cells(["cp312"], "standard", "cp312") == []
    assert abi3_smoke_cells(
        ["cp312", "cp313", "cp314", "cp315", "cp315"], "all", "cp312"
    ) == [
        {"python_version": p, "shard": "", "tier": "all", "lane": "abi3_smoke"}
        for p in ("cp313", "cp314", "cp315")
    ]
    for draft_tier in ("smoke", "core", "cfg"):
        assert native_cells("cp312", draft_tier, shard_count=4) == [
            {
                "python_version": "cp312",
                "shard": "",
                "tier": draft_tier,
                "lane": "native",
            }
        ]
        assert effective_split_tests(True, draft_tier, schema=2) is False
        assert physics_shards(True, draft_tier, 2, schema=2) == [""]


def test_schema2_cli_outputs_via_flag_and_env(tmp_path, monkeypatch) -> None:
    """CLI supports --schema 2 and GATE_AXIS_SCHEMA=2 without altering schema 1 defaults."""
    out_flag = tmp_path / "schema2_flag"
    monkeypatch.setenv("GITHUB_OUTPUT", str(out_flag))
    monkeypatch.delenv("GATE_AXIS_SCHEMA", raising=False)
    assert (
        main([
            "--schema",
            "2",
            "--tier",
            "physics-core",
            "--api-python",
            '["cp312","cp315"]',
            "--full-python",
            "cp312",
            "--native-shards",
            "2",
            "--include-os-compat",
            "--split-tests",
            "--physics-shards",
            "2",
        ])
        == 0
    )
    lines = dict(
        line.split("=", 1) for line in out_flag.read_text(encoding="utf-8").splitlines()
    )
    assert json.loads(lines["api_cells"]) == []
    assert len(json.loads(lines["native_cells"])) == 2
    assert json.loads(lines["abi3_smoke_cells"]) == [
        {
            "python_version": "cp315",
            "shard": "",
            "tier": "physics-core",
            "lane": "abi3_smoke",
        }
    ]
    assert json.loads(lines["os_compat_cells"]) == [
        {
            "python_version": "cp312",
            "shard": "",
            "tier": "physics-core",
            "lane": "os_compat",
        }
    ]
    assert json.loads(lines["physics_shards"]) == [""]
    assert lines["effective_split_tests"] == "false"

    out_env = tmp_path / "schema2_env"
    monkeypatch.setenv("GITHUB_OUTPUT", str(out_env))
    monkeypatch.setenv("GATE_AXIS_SCHEMA", "2")
    assert (
        main([
            "--tier",
            "standard",
            "--abi3-smoke-python",
            '["cp313","cp315"]',
            "--split-tests",
            "--physics-shards",
            "2",
        ])
        == 0
    )
    env_lines = dict(
        line.split("=", 1) for line in out_env.read_text(encoding="utf-8").splitlines()
    )
    assert [c["python_version"] for c in json.loads(env_lines["abi3_smoke_cells"])] == [
        "cp313",
        "cp315",
    ]
    assert json.loads(env_lines["os_compat_cells"]) == []
    assert json.loads(env_lines["physics_shards"]) == ["1-of-2", "2-of-2"]
    assert env_lines["effective_split_tests"] == "true"


def test_schema_validation_and_cross_schema_cli_flags_fail_closed(
    monkeypatch, capsys
) -> None:
    """Invalid schemas, mismatched flags, and unknown schema 2 tiers fail closed."""
    monkeypatch.delenv("GATE_AXIS_SCHEMA", raising=False)
    with pytest.raises(ValueError, match="unsupported axis schema"):
        effective_split_tests(True, "standard", schema=True)  # type: ignore[arg-type]
    with pytest.raises(ValueError, match="unknown schema 2 tier"):
        effective_split_tests(True, "physics_core", schema=2)

    assert main(["--tier", "standard"]) == 2
    assert (
        main(["--api-python", '["cp312"]', "--tier", "standard", "--include-os-compat"])
        == 2
    )
    assert main(["--schema", "2", "--tier", "standard", "--bookend-core"]) == 2
    assert main(["--schema", "2", "--tier", "physics_core"]) == 2
    assert (
        main(["--schema", "2", "--tier", "standard", "--abi3-smoke-python", "{}"]) == 2
    )

    monkeypatch.setenv("GATE_AXIS_SCHEMA", "3")
    assert main(["--api-python", '["cp312"]', "--tier", "standard"]) == 2
