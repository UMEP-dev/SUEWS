"""Contract tests for queue-pr fast-lane ordering and live merge-queue controls."""

from __future__ import annotations

import json
import os
from pathlib import Path
import shutil
import subprocess
import sys

import pytest

pytestmark = [
    pytest.mark.api,
    pytest.mark.contract("C-TOOLING"),
    pytest.mark.skipif(
        sys.platform == "win32"
        or shutil.which("bash") is None
        or shutil.which("jq") is None,
        reason="queue-pr shell scripts require bash and jq",
    ),
]

REPO_ROOT = Path(__file__).resolve().parents[2]
QUEUE_READY_SCRIPT = REPO_ROOT / ".claude/skills/queue-pr/scripts/queue-ready-prs.sh"
MANAGE_QUEUE_SCRIPT = (
    REPO_ROOT / ".claude/skills/queue-pr/scripts/manage-merge-queue.sh"
)


def _write_fake_gh(bin_dir: Path, log_path: Path) -> None:
    """Install a stub gh CLI driven by FAKE_GH_* environment variables."""
    stub = bin_dir / "gh"
    stub.write_text(
        """#!/usr/bin/env python3
import json
import os
import subprocess
import sys

args = sys.argv[1:]
with open(os.environ["FAKE_GH_LOG"], "a", encoding="utf-8") as fh:
    fh.write(json.dumps(args) + "\\n")

if args[:2] == ["auth", "status"]:
    sys.exit(0)

if args[:2] == ["repo", "view"]:
    print("UMEP-dev/SUEWS")
    sys.exit(0)

if args[:2] == ["pr", "list"]:
    data = json.loads(os.environ["FAKE_GH_PRS_JSON"])
    jq_expr = args[args.index("--jq") + 1]
    proc = subprocess.run(
        ["jq", "-r", jq_expr],
        input=json.dumps(data),
        text=True,
        capture_output=True,
        check=True,
    )
    sys.stdout.write(proc.stdout)
    sys.exit(0)

if args[:2] == ["pr", "diff"]:
    pr_num = args[2]
    files_map = json.loads(os.environ["FAKE_GH_FILES_BY_PR"])
    for path in files_map.get(pr_num, []):
        print(path)
    sys.exit(0)

if args[:2] == ["pr", "merge"]:
    print(f"Queued {args[2]}")
    sys.exit(0)

if args[:2] == ["api", "graphql"]:
    query = ""
    variables = {}
    i = 2
    while i < len(args):
        if args[i] in ("-f", "-F") and i + 1 < len(args):
            key, _, val = args[i + 1].partition("=")
            if key == "query":
                query = val
            else:
                variables[key] = val
            i += 2
        else:
            i += 1
    if "dequeuePullRequest" in query:
        print(json.dumps({"data": {"dequeuePullRequest": {"mergeQueueEntry": {"id": "MQE_1", "position": 2}}}}))
        sys.exit(0)
    if "enqueuePullRequest" in query:
        print(json.dumps({"data": {"enqueuePullRequest": {"mergeQueueEntry": {"position": 1, "state": "AWAITING_CHECKS"}}}}))
        sys.exit(0)
    if "pullRequest(number:" in query:
        pr_map = json.loads(os.environ.get("FAKE_GH_PR_GRAPHQL", "{}"))
        print(json.dumps(pr_map[str(variables["number"])]))
        sys.exit(0)
    print(os.environ.get("FAKE_GH_QUEUE_JSON", '{"data":{"repository":{"viewerPermission":"ADMIN","mergeQueue":null}}}'))
    sys.exit(0)

if args[:1] == ["api"] and "search/issues" in args:
    data = json.loads(os.environ["FAKE_GH_PRS_JSON"])
    print(len(data))
    sys.exit(0)

sys.stderr.write(f"Unhandled gh invocation: {args}\\n")
sys.exit(1)
""",
        encoding="utf-8",
    )
    stub.chmod(0o755)


def _sample_pr(
    number: int,
    title: str,
    created: str,
    labels: list[str] | None = None,
) -> dict[str, object]:
    return {
        "number": number,
        "title": title,
        "headRefName": f"branch-{number}",
        "headRefOid": f"{number:040x}",
        "isDraft": False,
        "reviewDecision": "APPROVED",
        "mergeStateStatus": "CLEAN",
        "mergeable": "MERGEABLE",
        "updatedAt": created,
        "createdAt": created,
        "url": f"https://github.com/UMEP-dev/SUEWS/pull/{number}",
        "author": {"login": "sunt05"},
        "labels": [{"name": name} for name in (labels or [])],
    }


@pytest.mark.smoke
def test_queue_ready_prs_orders_fast_lanes_first_and_skips_live_queued(
    tmp_path: Path,
) -> None:
    """Fast CI lanes sort ahead of native/physics lanes, and live-queued PRs are skipped."""
    # ARRANGE
    bin_dir = tmp_path / "bin"
    bin_dir.mkdir()
    log_path = tmp_path / "gh.log"
    _write_fake_gh(bin_dir, log_path)

    prs = [
        _sample_pr(10, "Physics change", "2026-11-20T00:00:00Z", ["0-physics:change"]),
        _sample_pr(11, "Fortran kernel fix", "2026-11-20T01:00:00Z"),
        _sample_pr(12, "Python test tweak", "2026-11-20T02:00:00Z"),
        _sample_pr(13, "Docs typo fix", "2026-11-20T03:00:00Z"),
        _sample_pr(14, "Already queued PR", "2026-11-20T04:00:00Z"),
        _sample_pr(15, "CI gate speed-up", "2026-11-20T05:00:00Z"),
        _sample_pr(16, "Fortran plus workflow tweak", "2026-11-20T06:00:00Z"),
    ]
    files_by_pr = {
        "10": ["src/suews/src/suews_phys_ohm.f95"],
        "11": ["src/suews/src/suews_util_time.f95"],
        "12": ["test/core/test_load.py"],
        "13": ["docs/source/index.rst"],
        "14": ["README.md"],
        "15": [".github/workflows/build-publish_to_pypi.yml"],
        "16": [
            "src/suews/src/suews_util_meteo.f95",
            ".github/scripts/determine-matrix.sh",
        ],
    }
    queue_json = {
        "data": {
            "repository": {
                "viewerPermission": "ADMIN",
                "mergeQueue": {
                    "id": "MQ_1",
                    "configuration": {
                        "maximumEntriesToBuild": 5,
                        "maximumEntriesToMerge": 5,
                        "mergeMethod": "SQUASH",
                        "mergingStrategy": "ALLGREEN",
                    },
                    "entries": {
                        "nodes": [
                            {
                                "id": "MQE_14",
                                "position": 1,
                                "state": "AWAITING_CHECKS",
                                "estimatedTimeToMerge": 180,
                                "headCommit": {"oid": "0" * 40},
                                "pullRequest": {
                                    "id": "PR_14",
                                    "number": 14,
                                    "title": "Already queued PR",
                                    "headRefOid": f"{14:040x}",
                                    "state": "OPEN",
                                    "isDraft": False,
                                },
                            }
                        ]
                    },
                },
            }
        }
    }

    env = {
        **os.environ,
        "PATH": f"{bin_dir}:{os.environ.get('PATH', '')}",
        "FAKE_GH_LOG": str(log_path),
        "FAKE_GH_PRS_JSON": json.dumps(prs),
        "FAKE_GH_FILES_BY_PR": json.dumps(files_by_pr),
        "FAKE_GH_QUEUE_JSON": json.dumps(queue_json),
    }

    # ACT
    res_all = subprocess.run(
        ["bash", str(QUEUE_READY_SCRIPT), "--base", "master"],
        cwd=REPO_ROOT,
        env=env,
        capture_output=True,
        text=True,
        check=False,
    )
    res_fast = subprocess.run(
        [
            "bash",
            str(QUEUE_READY_SCRIPT),
            "--base",
            "master",
            "--lane",
            "fast",
            "--max-batch",
            "1",
            "--enqueue",
        ],
        cwd=REPO_ROOT,
        env=env,
        capture_output=True,
        text=True,
        check=False,
    )

    # ASSERT
    assert res_all.returncode == 0, res_all.stdout + res_all.stderr
    order_lines = [
        line
        for line in res_all.stdout.splitlines()
        if line.startswith(("1. #", "2. #", "3. #", "4. #", "5. #", "6. #"))
    ]
    assert [line.split()[1] for line in order_lines] == [
        "#15",
        "#13",
        "#12",
        "#11",
        "#16",
        "#10",
    ]
    assert "lane=ci-infra" in res_all.stdout
    assert "lane=no-build" in res_all.stdout
    assert "lane=fast-linux" in res_all.stdout
    assert "lane=native-build" in res_all.stdout
    assert "lane=physics-full" in res_all.stdout
    assert "- #14 Already queued PR - already in merge queue" in res_all.stdout

    assert res_fast.returncode == 0, res_fast.stdout + res_fast.stderr
    assert "- enqueue #15 (ci-infra): requested" in res_fast.stdout
    assert "- enqueue #13: deferred (max_batch=1 reached)" in res_fast.stdout
    assert "- enqueue #12: deferred (max_batch=1 reached)" in res_fast.stdout
    assert "outside fast lane (native-build" in res_fast.stdout
    assert "outside fast lane (physics-full" in res_fast.stdout


@pytest.mark.smoke
def test_manage_merge_queue_status_dequeue_and_jump(tmp_path: Path) -> None:
    """manage-merge-queue.sh inspects live queue, dequeues, and jumps with head warning."""
    # ARRANGE
    bin_dir = tmp_path / "bin"
    bin_dir.mkdir()
    log_path = tmp_path / "gh.log"
    _write_fake_gh(bin_dir, log_path)

    queue_json = {
        "data": {
            "repository": {
                "viewerPermission": "ADMIN",
                "mergeQueue": {
                    "id": "MQ_1",
                    "configuration": {
                        "maximumEntriesToBuild": 5,
                        "maximumEntriesToMerge": 5,
                        "mergeMethod": "SQUASH",
                        "mergingStrategy": "ALLGREEN",
                    },
                    "entries": {
                        "nodes": [
                            {
                                "id": "MQE_1",
                                "position": 1,
                                "state": "AWAITING_CHECKS",
                                "estimatedTimeToMerge": 600,
                                "headCommit": {"oid": "a" * 40},
                                "pullRequest": {
                                    "id": "PR_100",
                                    "number": 100,
                                    "title": "Slow physics PR",
                                    "headRefOid": "a" * 40,
                                    "state": "OPEN",
                                    "isDraft": False,
                                },
                            },
                            {
                                "id": "MQE_2",
                                "position": 2,
                                "state": "QUEUED",
                                "estimatedTimeToMerge": 900,
                                "headCommit": {"oid": "b" * 40},
                                "pullRequest": {
                                    "id": "PR_200",
                                    "number": 200,
                                    "title": "CI gate fix",
                                    "headRefOid": "b" * 40,
                                    "state": "OPEN",
                                    "isDraft": False,
                                },
                            },
                        ]
                    },
                },
            }
        }
    }
    pr_graphql = {
        "200": {
            "data": {
                "repository": {
                    "viewerPermission": "ADMIN",
                    "pullRequest": {
                        "id": "PR_200",
                        "number": 200,
                        "title": "CI gate fix",
                        "state": "OPEN",
                        "isDraft": False,
                        "headRefOid": "b" * 40,
                        "baseRefName": "master",
                        "mergeQueueEntry": {
                            "id": "MQE_2",
                            "position": 2,
                            "state": "QUEUED",
                        },
                    },
                }
            }
        }
    }

    env = {
        **os.environ,
        "PATH": f"{bin_dir}:{os.environ.get('PATH', '')}",
        "FAKE_GH_LOG": str(log_path),
        "FAKE_GH_QUEUE_JSON": json.dumps(queue_json),
        "FAKE_GH_PR_GRAPHQL": json.dumps(pr_graphql),
    }

    # ACT
    res_status = subprocess.run(
        ["bash", str(MANAGE_QUEUE_SCRIPT), "status", "--base", "master"],
        cwd=REPO_ROOT,
        env=env,
        capture_output=True,
        text=True,
        check=False,
    )
    res_dequeue = subprocess.run(
        ["bash", str(MANAGE_QUEUE_SCRIPT), "dequeue", "200", "--base", "master"],
        cwd=REPO_ROOT,
        env=env,
        capture_output=True,
        text=True,
        check=False,
    )
    res_jump = subprocess.run(
        ["bash", str(MANAGE_QUEUE_SCRIPT), "jump", "200", "--base", "master"],
        cwd=REPO_ROOT,
        env=env,
        capture_output=True,
        text=True,
        check=False,
    )

    # ASSERT
    assert res_status.returncode == 0, res_status.stdout + res_status.stderr
    assert "1. #100 [AWAITING_CHECKS, eta=10m" in res_status.stdout
    assert "2. #200 [QUEUED, eta=15m" in res_status.stdout

    assert res_dequeue.returncode == 0, res_dequeue.stdout + res_dequeue.stderr
    assert "[OK] Dequeued PR #200 (was position=2, state=QUEUED)" in res_dequeue.stdout

    assert res_jump.returncode == 0, res_jump.stdout + res_jump.stderr
    assert "WARNING: Jumping PR #200 ahead of position 1 (#100" in res_jump.stderr
    assert "[OK] Jumped PR #200 to position 1" in res_jump.stdout

    calls = [
        json.loads(line) for line in log_path.read_text(encoding="utf-8").splitlines()
    ]
    joined = [" ".join(c) for c in calls]
    assert any("dequeuePullRequest" in c and "pr_id=PR_200" in c for c in joined)
    assert any(
        "enqueuePullRequest" in c and "jump: true" in c and f"head_oid={'b' * 40}" in c
        for c in joined
    )
