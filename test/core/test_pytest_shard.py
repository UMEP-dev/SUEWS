"""Contract tests for the deterministic api-lane shard plugin."""

from __future__ import annotations

import os
from pathlib import Path
import subprocess
import sys

import pytest

from scripts.suews.pytest_shard import parse_shard, shard_of

pytestmark = [pytest.mark.api, pytest.mark.contract("C-TOOLING")]

PROJECT_ROOT = Path(__file__).resolve().parents[2]


def test_parse_shard_accepts_k_of_n_and_rejects_the_rest() -> None:
    assert parse_shard("1/2") == (1, 2)
    assert parse_shard(" 2/2 ") == (2, 2)
    for bad in ("0/2", "3/2", "1/0", "1-of-2", "a/b", ""):
        with pytest.raises(ValueError):
            parse_shard(bad)


def test_shard_of_is_stable_and_spreads_node_ids() -> None:
    """The split is a fixed function of the node id, not of the process."""
    node_ids = [f"test/core/test_x.py::test_case[{i}]" for i in range(2000)]
    first = [shard_of(n, 2) for n in node_ids]
    assert first == [shard_of(n, 2) for n in node_ids]
    assert set(first) == {1, 2}
    # A SHA-256 split of 2000 ids lands far inside 40-60%.
    assert 800 < first.count(1) < 1200
    # Pinned values: a change of hash or byte order would silently move
    # every test to a different shard between a branch and master.
    assert [shard_of(f"t::{i}", 2) for i in range(8)] == [1, 1, 1, 1, 1, 1, 2, 2]
    assert [shard_of(f"t::{i}", 3) for i in range(6)] == [3, 2, 2, 3, 2, 2]


def _collect(tmp_path: Path, *extra: str) -> set[str]:
    env = os.environ.copy()
    env["PYTHONPATH"] = os.pathsep.join(
        filter(None, (str(PROJECT_ROOT), env.get("PYTHONPATH")))
    )
    result = subprocess.run(
        [
            sys.executable,
            "-m",
            "pytest",
            "--confcutdir",
            str(tmp_path),
            "-p",
            "no:cacheprovider",
            "--collect-only",
            "-q",
            *extra,
        ],
        cwd=tmp_path,
        env=env,
        check=False,
        capture_output=True,
        text=True,
    )
    assert result.returncode == 0, result.stdout + result.stderr
    return {line for line in result.stdout.splitlines() if "::" in line}


def test_shards_partition_the_selected_tests(tmp_path: Path) -> None:
    """Shards 1/2 and 2/2 are disjoint and together equal the unsharded run."""
    (tmp_path / "test_sample.py").write_text(
        "import pytest\n\n"
        "@pytest.mark.parametrize('i', range(40))\n"
        "def test_case(i):\n"
        "    pass\n",
        encoding="utf-8",
    )
    whole = _collect(tmp_path, "test_sample.py")
    plugin = ("-p", "scripts.suews.pytest_shard")
    first = _collect(tmp_path, *plugin, "--suews-shard", "1/2", "test_sample.py")
    second = _collect(tmp_path, *plugin, "--suews-shard", "2/2", "test_sample.py")

    assert len(whole) == 40
    assert first and second
    assert first.isdisjoint(second)
    assert first | second == whole
    # Loaded without the option, the plugin selects everything.
    assert _collect(tmp_path, *plugin, "test_sample.py") == whole
