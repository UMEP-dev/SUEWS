"""Contract tests for the per-platform test layout (scripts/suews/ci_test_layout.py)."""

from __future__ import annotations

import json

import pytest

from scripts.suews.ci_test_layout import (
    api_cells,
    effective_split_tests,
    main,
    physics_shards,
)

pytestmark = [pytest.mark.api, pytest.mark.contract("C-TOOLING")]


def test_ready_linux_pull_request_layout() -> None:
    """cp312 in four shards at the full tier, cp315 whole at core-bookend."""
    cells = api_cells(["cp312", "cp315"], "standard", "cp312", 4, True, "cp312")
    assert cells == [
        *({"python_version": "cp312", "shard": f"{k}-of-4", "tier": "standard"} for k in range(1, 5)),
        {"python_version": "cp315", "shard": "", "tier": "core-bookend"},
    ]
    assert physics_shards(True, "standard", 2) == ["1-of-2", "2-of-2"]
    assert physics_shards(True, "physics-full", 3) == ["1-of-3", "2-of-3", "3-of-3"]


def test_merge_queue_and_nightly_layouts() -> None:
    queue = api_cells(["cp312"], "standard", "cp312", 4, False, "cp312")
    assert [c["shard"] for c in queue] == ["1-of-4", "2-of-4", "3-of-4", "4-of-4"]
    assert {c["tier"] for c in queue} == {"standard"}
    nightly = api_cells(["cp312", "cp313", "cp314", "cp315"], "all", "", 2, False, "cp312")
    assert nightly == [{"python_version": p, "shard": "", "tier": "all"} for p in ("cp312", "cp313", "cp314", "cp315")]
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
    assert main([
        "--api-python", '["cp312","cp315"]', "--tier", "standard", "--shard-python", "cp312",
        "--shard-count", "4", "--bookend-core", "--split-tests", "--physics-shards", "2",
    ]) == 0
    lines = dict(line.split("=", 1) for line in output.read_text(encoding="utf-8").splitlines())
    assert len(json.loads(lines["api_cells"])) == 5
    assert json.loads(lines["physics_shards"]) == ["1-of-2", "2-of-2"]
    assert lines["effective_split_tests"] == "true"
