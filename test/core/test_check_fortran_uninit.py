"""Tests for scripts/lint/check_fortran_uninit.py (gh#2095).

The lint compiles every SUEWS Fortran unit with ``gfortran -O2 -Wuninitialized
-Wmaybe-uninitialized`` (without ``-finit-real=zero``) and enforces a shrinking
ratchet keyed by ``(file, proc, var, kind)``. These unit tests verify
diagnostic parsing, Fortran procedure-span resolution, both ratchet failure
directions (new warnings and stale baseline entries), baseline serialisation,
unit discovery, and workflow pinning without invoking a host compiler.
"""

from __future__ import annotations

import importlib.util
import json
import os
from pathlib import Path
import re
import subprocess
import sys

import pytest
import yaml

pytestmark = [pytest.mark.tooling, pytest.mark.contract("C-TOOLING")]


REPO_ROOT = Path(__file__).resolve().parents[2]
SCRIPT_PATH = REPO_ROOT / "scripts" / "lint" / "check_fortran_uninit.py"
SCRIPT_SPEC = importlib.util.spec_from_file_location(
    "check_fortran_uninit", SCRIPT_PATH
)
assert SCRIPT_SPEC is not None
assert SCRIPT_SPEC.loader is not None
check_fortran_uninit = importlib.util.module_from_spec(SCRIPT_SPEC)
sys.modules[SCRIPT_SPEC.name] = check_fortran_uninit
SCRIPT_SPEC.loader.exec_module(check_fortran_uninit)


SAMPLE_FORTRAN = """\
MODULE module_example
   IMPLICIT NONE

   TYPE :: dts_demo
      REAL(KIND(1D0)) :: val
   END TYPE dts_demo

   INTERFACE
      SUBROUTINE external_iface(x)
         REAL(KIND(1D0)), INTENT(IN) :: x
      END SUBROUTINE external_iface
   END INTERFACE

CONTAINS

   FUNCTION psi_demo(zl) RESULT(psih)
      REAL(KIND(1D0)), INTENT(IN) :: zl
      REAL(KIND(1D0)) :: psih
      IF (zl > 0.0D0) psih = zl
   END FUNCTION

   PURE REAL(KIND(1D0)) FUNCTION typed_fn(a)
      REAL(KIND(1D0)), INTENT(IN) :: a
      typed_fn = a
   END FUNCTION typed_fn

   SUBROUTINE outer_sub(x, y)
      REAL(KIND(1D0)), INTENT(IN) :: x
      REAL(KIND(1D0)), INTENT(OUT) :: y
      REAL(KIND(1D0)) :: uninit_local
      y = x + uninit_local
   CONTAINS
      SUBROUTINE inner_helper(v)
         REAL(KIND(1D0)), INTENT(OUT) :: v
         REAL(KIND(1D0)) :: hidden
         v = hidden
      END SUBROUTINE inner_helper
   END SUBROUTINE outer_sub

END MODULE module_example
"""


def _make_stub_repo(
    tmp_path: Path,
    baseline_keys: list[check_fortran_uninit.DiagnosticKey] | None = None,
    compiler: str = check_fortran_uninit.PINNED_COMPILER,
) -> Path:
    """Create a minimal repo layout carrying Makefile and optional baseline."""
    makefile = tmp_path / "src" / "suews" / "Makefile"
    makefile.parent.mkdir(parents=True, exist_ok=True)
    makefile.write_text("# stub\n", encoding="utf-8")
    if baseline_keys is not None:
        check_fortran_uninit.write_baseline(
            tmp_path / check_fortran_uninit.BASELINE_PATH,
            baseline_keys,
            unit_count=114,
            compiler=compiler,
        )
    return tmp_path


def test_build_procedure_spans_and_resolution():
    """Procedure spans track functions, typed functions, and nested CONTAINS."""
    spans = check_fortran_uninit.build_procedure_spans(SAMPLE_FORTRAN)
    names = {span.name for span in spans}
    assert names == {"psi_demo", "typed_fn", "outer_sub", "outer_sub::inner_helper"}

    lines = SAMPLE_FORTRAN.splitlines()
    psi_header = next(i for i, ln in enumerate(lines, 1) if "FUNCTION psi_demo" in ln)
    psi_end = next(i for i, ln in enumerate(lines, 1) if ln.strip() == "END FUNCTION")
    typed_body = next(i for i, ln in enumerate(lines, 1) if "typed_fn = a" in ln)
    outer_body = next(
        i for i, ln in enumerate(lines, 1) if "y = x + uninit_local" in ln
    )
    inner_body = next(i for i, ln in enumerate(lines, 1) if "v = hidden" in ln)
    iface_line = next(i for i, ln in enumerate(lines, 1) if "external_iface" in ln)

    assert check_fortran_uninit.resolve_procedure(spans, psi_header) == "psi_demo"
    assert check_fortran_uninit.resolve_procedure(spans, psi_end) == "psi_demo"
    assert check_fortran_uninit.resolve_procedure(spans, typed_body) == "typed_fn"
    assert check_fortran_uninit.resolve_procedure(spans, outer_body) == "outer_sub"
    assert (
        check_fortran_uninit.resolve_procedure(spans, inner_body)
        == "outer_sub::inner_helper"
    )
    assert check_fortran_uninit.resolve_procedure(spans, iface_line) == "<top-level>"


def test_canonicalise_var_name():
    """Compiler-generated numeric temp suffixes normalise; real vars stay verbatim."""
    assert (
        check_fortran_uninit.canonicalise_var_name("heat_state_shadow.38")
        == "heat_state_shadow.<tmp>"
    )
    assert check_fortran_uninit.canonicalise_var_name("TSURF_0_K") == "tsurf_0_k"
    assert check_fortran_uninit.canonicalise_var_name("z0m_zh") == "z0m_zh"
    assert check_fortran_uninit.canonicalise_var_name("x1") == "x1"


def test_parse_gfortran_diagnostics_multiline_inline_and_filters(tmp_path):
    """Parser handles multi-line GCC 14, inline GCC 11, curly quotes, and ignores others."""
    src = tmp_path / "src" / "suews" / "src" / "example.f95"
    src.parent.mkdir(parents=True, exist_ok=True)
    src.write_text(SAMPLE_FORTRAN, encoding="utf-8")

    lines = SAMPLE_FORTRAN.splitlines()
    psi_end = next(i for i, ln in enumerate(lines, 1) if ln.strip() == "END FUNCTION")
    outer_body = next(
        i for i, ln in enumerate(lines, 1) if "y = x + uninit_local" in ln
    )
    inner_body = next(i for i, ln in enumerate(lines, 1) if "v = hidden" in ln)

    stderr = (
        f"{src}:{psi_end}:15:\n\n"
        f"   {psi_end} |    END FUNCTION\n"
        "      |               ^\n"
        "Warning: 'psih' may be used uninitialized [-Wmaybe-uninitialized]\n"
        f"{src}:16:34:\n\n"
        "note: 'psih' was declared here\n"
        f"{src}:{outer_body}:15:\n\n"
        f"   {outer_body} |       y = x + uninit_local\n"
        "      |               ^\n"
        "Warning: \u2018uninit_local\u2019 is used uninitialized [-Wuninitialized]\n"
        f"{src}:{inner_body}:12: "
        "Warning: 'hidden' may be used uninitialized in this function "
        "[-Wmaybe-uninitialized]\n"
        f"{src}:{outer_body}:10:\n\n"
        "Warning: Legacy Extension: REAL array index at (1)\n"
    )

    hits = check_fortran_uninit.parse_gfortran_diagnostics(stderr, tmp_path)
    keys = [hit.key for hit in hits]
    assert keys == [
        check_fortran_uninit.DiagnosticKey(
            file="src/suews/src/example.f95",
            proc="psi_demo",
            var="psih",
            kind="maybe-uninitialized",
        ),
        check_fortran_uninit.DiagnosticKey(
            file="src/suews/src/example.f95",
            proc="outer_sub",
            var="uninit_local",
            kind="uninitialized",
        ),
        check_fortran_uninit.DiagnosticKey(
            file="src/suews/src/example.f95",
            proc="outer_sub::inner_helper",
            var="hidden",
            kind="maybe-uninitialized",
        ),
    ]


def test_ratchet_clean_match_passes(tmp_path, monkeypatch, capsys):
    """Matching warning set exits 0."""
    key = check_fortran_uninit.DiagnosticKey(
        file="src/suews/src/suews_phys_narp.f95",
        proc="narp",
        var="tsurf_0_k",
        kind="maybe-uninitialized",
    )
    repo = _make_stub_repo(tmp_path, [key])
    monkeypatch.setattr(
        check_fortran_uninit,
        "compile_and_collect",
        lambda _root, gfortran="gfortran": (
            [check_fortran_uninit.DiagnosticHit(key=key, line=445)],
            114,
            check_fortran_uninit.PINNED_COMPILER,
        ),
    )

    rc = check_fortran_uninit.main(["check_fortran_uninit", str(repo)])
    assert rc == 0
    assert "[OK]" in capsys.readouterr().out


def test_ratchet_new_warning_fails_with_key(tmp_path, monkeypatch, capsys):
    """A newly introduced warning fails and prints (file, proc, var, kind)."""
    existing = check_fortran_uninit.DiagnosticKey(
        file="src/suews/src/suews_phys_narp.f95",
        proc="narp",
        var="tsurf_0_k",
        kind="maybe-uninitialized",
    )
    new_key = check_fortran_uninit.DiagnosticKey(
        file="src/suews/src/suews_util_meteo.f95",
        proc="sat_vap_press",
        var="uninit_seed_probe",
        kind="uninitialized",
    )
    repo = _make_stub_repo(tmp_path, [existing])
    monkeypatch.setattr(
        check_fortran_uninit,
        "compile_and_collect",
        lambda _root, gfortran="gfortran": (
            [
                check_fortran_uninit.DiagnosticHit(key=existing, line=445),
                check_fortran_uninit.DiagnosticHit(key=new_key, line=41),
            ],
            114,
            check_fortran_uninit.PINNED_COMPILER,
        ),
    )

    rc = check_fortran_uninit.main(["check_fortran_uninit", str(repo)])
    assert rc == 1
    err = capsys.readouterr().err
    assert "new Fortran uninitialised-variable warning" in err
    assert (
        "(src/suews/src/suews_util_meteo.f95, sat_vap_press, "
        "uninit_seed_probe, uninitialized)" in err
    )


def test_ratchet_stale_baseline_fails_with_key(tmp_path, monkeypatch, capsys):
    """A resolved warning still present in the baseline fails (shrinking ratchet)."""
    remaining = check_fortran_uninit.DiagnosticKey(
        file="src/suews/src/suews_phys_narp.f95",
        proc="narp",
        var="tsurf_0_k",
        kind="maybe-uninitialized",
    )
    resolved = check_fortran_uninit.DiagnosticKey(
        file="src/suews/src/suews_phys_resist.f95",
        proc="suews_cal_roughnessparameters",
        var="z0m_zh",
        kind="maybe-uninitialized",
    )
    repo = _make_stub_repo(tmp_path, [remaining, resolved])
    monkeypatch.setattr(
        check_fortran_uninit,
        "compile_and_collect",
        lambda _root, gfortran="gfortran": (
            [check_fortran_uninit.DiagnosticHit(key=remaining, line=445)],
            114,
            check_fortran_uninit.PINNED_COMPILER,
        ),
    )

    rc = check_fortran_uninit.main(["check_fortran_uninit", str(repo)])
    assert rc == 1
    err = capsys.readouterr().err
    assert "stale baseline" in err
    assert (
        "(src/suews/src/suews_phys_resist.f95, suews_cal_roughnessparameters, "
        "z0m_zh, maybe-uninitialized)" in err
    )


def test_write_baseline_and_report_round_trip(tmp_path, monkeypatch, capsys):
    """--write-baseline writes deterministic JSON and subsequent check exits 0."""
    key_b = check_fortran_uninit.DiagnosticKey(
        file="src/suews/src/b.f95",
        proc="sub_b",
        var="v_b",
        kind="uninitialized",
    )
    key_a = check_fortran_uninit.DiagnosticKey(
        file="src/suews/src/a.f95",
        proc="sub_a",
        var="v_a",
        kind="maybe-uninitialized",
    )
    repo = _make_stub_repo(tmp_path)
    monkeypatch.setattr(
        check_fortran_uninit,
        "compile_and_collect",
        lambda _root, gfortran="gfortran": (
            [
                check_fortran_uninit.DiagnosticHit(key=key_b, line=20),
                check_fortran_uninit.DiagnosticHit(key=key_a, line=10),
                check_fortran_uninit.DiagnosticHit(key=key_a, line=12),
            ],
            114,
            check_fortran_uninit.PINNED_COMPILER,
        ),
    )

    assert (
        check_fortran_uninit.main([
            "check_fortran_uninit",
            str(repo),
            "--write-baseline",
        ])
        == 0
    )
    payload = json.loads(
        (repo / check_fortran_uninit.BASELINE_PATH).read_text(encoding="utf-8")
    )
    assert payload["entry_count"] == 2
    assert payload["entries"] == [key_a.to_dict(), key_b.to_dict()]

    assert check_fortran_uninit.main(["check_fortran_uninit", str(repo)]) == 0
    assert (
        check_fortran_uninit.main(["check_fortran_uninit", str(repo), "--report"]) == 0
    )
    report = capsys.readouterr().out
    assert "(src/suews/src/a.f95, sub_a, v_a, maybe-uninitialized)" in report


def _tracked_rel_paths(pattern: str, fallback_dir: Path, glob_pat: str) -> set[str]:
    """Return repo-relative tracked paths via git, falling back to filesystem glob."""
    try:
        res = subprocess.run(
            ["git", "ls-files", pattern],
            cwd=REPO_ROOT,
            check=True,
            capture_output=True,
            text=True,
        )
        paths = {line.strip() for line in res.stdout.splitlines() if line.strip()}
        if paths:
            return paths
    except (OSError, subprocess.CalledProcessError):
        pass
    return {
        p.relative_to(REPO_ROOT).as_posix()
        for p in fallback_dir.glob(glob_pat)
        if p.is_file()
    }


def test_live_repo_unit_discovery_and_committed_baseline_contract():
    """Live repo unit discovery matches Makefile/C-API sources and pinned workflow."""
    units = check_fortran_uninit.discover_fortran_units(REPO_ROOT)
    assert len(units) > 0
    assert len({unit.rel_path for unit in units}) == len(units)

    by_group: dict[str, set[str]] = {"spartacus": set(), "suews": set(), "c_api": set()}
    for unit in units:
        assert unit.group in by_group
        by_group[unit.group].add(unit.rel_path)
        if unit.source_path is not None:
            assert unit.source_path.is_file(), f"Missing Fortran unit: {unit.rel_path}"

    assert all(by_group.values()), f"Empty Fortran unit group: {by_group}"

    expected_suews = _tracked_rel_paths(
        "src/suews/src/*.f95", REPO_ROOT / "src" / "suews" / "src", "*.f95"
    ) | {check_fortran_uninit.GENERATED_VERSION_REL}
    expected_capi = _tracked_rel_paths(
        "src/suews_bridge/c_api/*.f95",
        REPO_ROOT / "src" / "suews_bridge" / "c_api",
        "*.f95",
    )
    assert by_group["suews"] == expected_suews
    assert by_group["c_api"] == expected_capi

    baseline_path = REPO_ROOT / check_fortran_uninit.BASELINE_PATH
    keys, payload = check_fortran_uninit.load_baseline(baseline_path)
    assert isinstance(payload["unit_count"], int) and payload["unit_count"] > 0
    assert payload["entry_count"] == len(keys)
    assert keys == sorted(set(keys))
    assert payload["container_image"] == check_fortran_uninit.PINNED_CONTAINER_IMAGE
    assert payload["compiler"] == check_fortran_uninit.PINNED_COMPILER
    for key in keys:
        assert key.kind in {"uninitialized", "maybe-uninitialized"}
        assert (REPO_ROOT / key.file).is_file(), f"Missing file in baseline: {key.file}"

    _assert_fortran_uninit_workflow_contract()


def _assert_fortran_uninit_workflow_contract() -> None:
    """Assert pinned container, workflow_call/concurrency isolation, and base wiring."""
    workflow_path = REPO_ROOT / ".github" / "workflows" / "fortran-uninit.yml"
    workflow_text = workflow_path.read_text(encoding="utf-8")
    workflow = yaml.safe_load(workflow_text)
    triggers = workflow[True] if True in workflow else workflow["on"]
    assert {"pull_request", "workflow_call", "workflow_dispatch"} <= set(triggers)
    assert "merge_group" not in triggers
    assert {"base_sha", "head_sha"} <= set(triggers["workflow_call"]["inputs"])
    assert workflow["permissions"] == {"contents": "read"}
    assert "github.workflow" in str(workflow["concurrency"]["group"])

    (job,) = workflow["jobs"].values()
    image = job["container"]["image"]
    assert image == check_fortran_uninit.PINNED_CONTAINER_IMAGE
    assert re.search(r"@sha256:[0-9a-f]{64}$", image)
    assert "-finit-real=zero" not in check_fortran_uninit.KERNEL_BASE_FLAGS
    assert "-finit-real=zero" not in check_fortran_uninit.CAPI_BASE_FLAGS
    assert "--allow-baseline-growth" not in workflow_text

    checkout_step = next(
        step
        for step in job["steps"]
        if str(step.get("uses", "")).startswith("actions/checkout@")
    )
    assert checkout_step["with"]["fetch-depth"] == 0
    assert checkout_step["with"]["persist-credentials"] is False
    assert checkout_step["with"]["ref"] == "${{ inputs.head_sha }}"

    check_step = job["steps"][-1]
    assert check_step["env"]["EVENT_NAME"] == "${{ github.event_name }}"
    assert "github.event.pull_request.head.sha" in check_step["env"]["PR_HEAD_SHA"]
    assert "github.event.pull_request.base.sha" in check_step["env"]["BASE_SHA"]
    assert "github.event.merge_group.base_sha" in check_step["env"]["BASE_SHA"]
    assert "HEAD^2" in check_step["run"]
    assert "HEAD^1" in check_step["run"]
    assert "PR_HEAD_SHA" in check_step["run"]
    assert '--base-ref "$BASE_SHA"' in check_step["run"]


def _init_git_repo(repo_root: Path) -> None:
    """Initialise a throwaway git repository for --base-ref tests."""
    subprocess.run(["git", "init", str(repo_root)], check=True, capture_output=True)
    subprocess.run(
        ["git", "-C", str(repo_root), "config", "user.name", "Test"],
        check=True,
        capture_output=True,
    )
    subprocess.run(
        ["git", "-C", str(repo_root), "config", "user.email", "test@example.com"],
        check=True,
        capture_output=True,
    )


def _git_commit_all(repo_root: Path, message: str) -> str:
    subprocess.run(
        ["git", "-C", str(repo_root), "add", "-A"], check=True, capture_output=True
    )
    subprocess.run(
        ["git", "-C", str(repo_root), "commit", "-m", message],
        check=True,
        capture_output=True,
    )
    out = subprocess.run(
        ["git", "-C", str(repo_root), "rev-parse", "HEAD"],
        check=True,
        capture_output=True,
        text=True,
    )
    return out.stdout.strip()


def test_shrink_only_rejects_added_and_swapped_keys_and_allows_removals(
    tmp_path, monkeypatch, capsys
):
    """--base-baseline forbids adding or swapping keys even at equal/lower count."""
    key_a = check_fortran_uninit.DiagnosticKey(
        file="src/suews/src/a.f95", proc="sub_a", var="v_a", kind="maybe-uninitialized"
    )
    key_b = check_fortran_uninit.DiagnosticKey(
        file="src/suews/src/b.f95", proc="sub_b", var="v_b", kind="maybe-uninitialized"
    )
    key_c = check_fortran_uninit.DiagnosticKey(
        file="src/suews/src/c.f95", proc="sub_c", var="v_c", kind="uninitialized"
    )

    base_path = tmp_path / "base.json"
    check_fortran_uninit.write_baseline(
        base_path,
        [key_a, key_b],
        unit_count=114,
        compiler=check_fortran_uninit.PINNED_COMPILER,
    )

    # Case 1: added key with higher total count -> FAIL (even though warnings match candidate)
    repo_add = _make_stub_repo(tmp_path / "add", [key_a, key_b, key_c])
    monkeypatch.setattr(
        check_fortran_uninit,
        "compile_and_collect",
        lambda _root, gfortran="gfortran": (
            [
                check_fortran_uninit.DiagnosticHit(key=key_a, line=1),
                check_fortran_uninit.DiagnosticHit(key=key_b, line=2),
                check_fortran_uninit.DiagnosticHit(key=key_c, line=3),
            ],
            114,
            check_fortran_uninit.PINNED_COMPILER,
        ),
    )
    rc_add = check_fortran_uninit.main([
        "check_fortran_uninit",
        str(repo_add),
        "--base-baseline",
        str(base_path),
    ])
    assert rc_add == 1
    err_add = capsys.readouterr().err
    assert "shrink-only" in err_add
    assert key_c.format_short() in err_add

    # Case 2: swapped key at equal total count (remove key_b, add key_c) -> FAIL
    repo_swap = _make_stub_repo(tmp_path / "swap", [key_a, key_c])
    monkeypatch.setattr(
        check_fortran_uninit,
        "compile_and_collect",
        lambda _root, gfortran="gfortran": (
            [
                check_fortran_uninit.DiagnosticHit(key=key_a, line=1),
                check_fortran_uninit.DiagnosticHit(key=key_c, line=3),
            ],
            114,
            check_fortran_uninit.PINNED_COMPILER,
        ),
    )
    rc_swap = check_fortran_uninit.main([
        "check_fortran_uninit",
        str(repo_swap),
        "--base-baseline",
        str(base_path),
    ])
    assert rc_swap == 1
    err_swap = capsys.readouterr().err
    assert "shrink-only" in err_swap
    assert key_c.format_short() in err_swap

    # Case 3: swapped key at LOWER total count (remove key_a & key_b, add key_c) -> FAIL
    repo_lower = _make_stub_repo(tmp_path / "lower", [key_c])
    monkeypatch.setattr(
        check_fortran_uninit,
        "compile_and_collect",
        lambda _root, gfortran="gfortran": (
            [check_fortran_uninit.DiagnosticHit(key=key_c, line=3)],
            114,
            check_fortran_uninit.PINNED_COMPILER,
        ),
    )
    rc_lower = check_fortran_uninit.main([
        "check_fortran_uninit",
        str(repo_lower),
        "--base-baseline",
        str(base_path),
    ])
    assert rc_lower == 1
    assert key_c.format_short() in capsys.readouterr().err

    # Case 4: genuine removal (key_b resolved & removed) -> PASS
    repo_shrink = _make_stub_repo(tmp_path / "shrink", [key_a])
    monkeypatch.setattr(
        check_fortran_uninit,
        "compile_and_collect",
        lambda _root, gfortran="gfortran": (
            [check_fortran_uninit.DiagnosticHit(key=key_a, line=1)],
            114,
            check_fortran_uninit.PINNED_COMPILER,
        ),
    )
    rc_shrink = check_fortran_uninit.main([
        "check_fortran_uninit",
        str(repo_shrink),
        "--base-baseline",
        str(base_path),
    ])
    assert rc_shrink == 0
    assert "shrunk by 1" in capsys.readouterr().out


def test_base_ref_bootstrap_shrink_rejection_and_invalid_ref(
    tmp_path, monkeypatch, capsys
):
    """--base-ref handles first-introduction bootstrap, shrink-only, and invalid refs."""
    key_a = check_fortran_uninit.DiagnosticKey(
        file="src/suews/src/a.f95", proc="sub_a", var="v_a", kind="maybe-uninitialized"
    )
    key_b = check_fortran_uninit.DiagnosticKey(
        file="src/suews/src/b.f95", proc="sub_b", var="v_b", kind="uninitialized"
    )

    repo = _make_stub_repo(tmp_path / "gitrepo", baseline_keys=None)
    _init_git_repo(repo)
    base_without_file = _git_commit_all(repo, "base without baseline")

    # Introduce baseline in working tree -> bootstrap against base_without_file passes
    check_fortran_uninit.write_baseline(
        repo / check_fortran_uninit.BASELINE_PATH,
        [key_a],
        unit_count=114,
        compiler=check_fortran_uninit.PINNED_COMPILER,
    )
    monkeypatch.setattr(
        check_fortran_uninit,
        "compile_and_collect",
        lambda _root, gfortran="gfortran": (
            [check_fortran_uninit.DiagnosticHit(key=key_a, line=10)],
            114,
            check_fortran_uninit.PINNED_COMPILER,
        ),
    )
    rc_boot = check_fortran_uninit.main([
        "check_fortran_uninit",
        str(repo),
        "--base-ref",
        base_without_file,
    ])
    assert rc_boot == 0
    assert "bootstrap:" in capsys.readouterr().err

    # Commit the baseline; adding key_b in candidate must now FAIL against that base
    base_with_file = _git_commit_all(repo, "add initial baseline")
    check_fortran_uninit.write_baseline(
        repo / check_fortran_uninit.BASELINE_PATH,
        [key_a, key_b],
        unit_count=114,
        compiler=check_fortran_uninit.PINNED_COMPILER,
    )
    monkeypatch.setattr(
        check_fortran_uninit,
        "compile_and_collect",
        lambda _root, gfortran="gfortran": (
            [
                check_fortran_uninit.DiagnosticHit(key=key_a, line=10),
                check_fortran_uninit.DiagnosticHit(key=key_b, line=20),
            ],
            114,
            check_fortran_uninit.PINNED_COMPILER,
        ),
    )
    rc_added = check_fortran_uninit.main([
        "check_fortran_uninit",
        str(repo),
        "--base-ref",
        base_with_file,
    ])
    assert rc_added == 1
    assert key_b.format_short() in capsys.readouterr().err

    # Non-existent / unfetched base ref must fail loudly (never silently bootstrap)
    rc_bad_ref = check_fortran_uninit.main([
        "check_fortran_uninit",
        str(repo),
        "--base-ref",
        "deadbeefdeadbeefdeadbeefdeadbeefdeadbeef",
    ])
    assert rc_bad_ref == 1
    assert "could not be resolved" in capsys.readouterr().err


def test_compiler_image_bump_permits_rebaseline_and_rejects_spoofs(
    tmp_path, monkeypatch, capsys
):
    """Genuine lockstep GCC image bumps permit re-baselining; spoofed JSON metadata fails."""
    key_a = check_fortran_uninit.DiagnosticKey(
        file="src/suews/src/a.f95", proc="sub_a", var="v_a", kind="maybe-uninitialized"
    )
    key_b = check_fortran_uninit.DiagnosticKey(
        file="src/suews/src/b.f95", proc="sub_b", var="v_b", kind="uninitialized"
    )
    new_image = (
        "public.ecr.aws/docker/library/gcc"
        "@sha256:1111111111111111111111111111111111111111111111111111111111111111"
    )
    old_compiler = "GNU Fortran (GCC) 14.2.0"
    new_compiler = "GNU Fortran (GCC) 15.1.0"

    base_path = tmp_path / "base.json"
    check_fortran_uninit.write_baseline(
        base_path, [key_a], unit_count=114, compiler=old_compiler
    )
    base_data = json.loads(base_path.read_text(encoding="utf-8"))
    base_data["container_image"] = (
        "public.ecr.aws/docker/library/gcc"
        "@sha256:0000000000000000000000000000000000000000000000000000000000000000"
    )
    base_path.write_text(json.dumps(base_data, indent=2) + "\n", encoding="utf-8")

    repo = _make_stub_repo(tmp_path / "bump_repo", [key_a, key_b])
    wf_path = repo / check_fortran_uninit.WORKFLOW_PATH
    wf_path.parent.mkdir(parents=True, exist_ok=True)
    wf_path.write_text(
        f"jobs:\n  lint:\n    container:\n      image: {new_image}\n", encoding="utf-8"
    )

    cand_path = repo / check_fortran_uninit.BASELINE_PATH
    cand_data = json.loads(cand_path.read_text(encoding="utf-8"))
    cand_data["container_image"] = new_image
    cand_data["compiler"] = new_compiler
    cand_path.write_text(json.dumps(cand_data, indent=2) + "\n", encoding="utf-8")

    both_hits = [
        check_fortran_uninit.DiagnosticHit(key=key_a, line=1),
        check_fortran_uninit.DiagnosticHit(key=key_b, line=2),
    ]
    cli_args = [
        "check_fortran_uninit",
        str(repo),
        "--base-baseline",
        str(base_path),
    ]

    # Case 1: Spoofed JSON only (workflow/PINNED_CONTAINER_IMAGE still old) -> FAIL
    monkeypatch.setattr(
        check_fortran_uninit,
        "compile_and_collect",
        lambda _root, gfortran="gfortran": (both_hits, 114, new_compiler),
    )
    assert check_fortran_uninit.main(cli_args) == 1
    assert "does not match PINNED_CONTAINER_IMAGE" in capsys.readouterr().err

    # Case 2: Missing or unparseable workflow image -> FAIL closed
    monkeypatch.setattr(check_fortran_uninit, "PINNED_CONTAINER_IMAGE", new_image)
    wf_path.unlink()
    assert check_fortran_uninit.main(cli_args) == 1
    assert "could not read pinned container.image" in capsys.readouterr().err
    wf_path.write_text("jobs:\n  lint:\n    runs-on: ubuntu-latest\n", encoding="utf-8")
    assert check_fortran_uninit.main(cli_args) == 1
    assert "could not read pinned container.image" in capsys.readouterr().err
    wf_path.write_text(
        f"jobs:\n  lint:\n    container:\n      image: {new_image}\n", encoding="utf-8"
    )

    # Case 3: Stale PINNED_COMPILER -> FAIL closed
    assert check_fortran_uninit.main(cli_args) == 1
    assert "does not match PINNED_COMPILER" in capsys.readouterr().err

    # Case 4: Lockstep script + workflow + PINNED_COMPILER, but running compiler mismatch -> FAIL
    monkeypatch.setattr(check_fortran_uninit, "PINNED_COMPILER", new_compiler)
    monkeypatch.setattr(
        check_fortran_uninit,
        "compile_and_collect",
        lambda _root, gfortran="gfortran": (both_hits, 114, old_compiler),
    )
    assert check_fortran_uninit.main(cli_args) == 1
    assert "does not match running compiler" in capsys.readouterr().err

    # Case 5: Genuine full lockstep bump with matching running compiler -> PASS
    monkeypatch.setattr(
        check_fortran_uninit,
        "compile_and_collect",
        lambda _root, gfortran="gfortran": (both_hits, 114, new_compiler),
    )
    assert check_fortran_uninit.main(cli_args) == 0
    assert "compiler-image bump detected" in capsys.readouterr().err

    # Case 6: Genuine bump still fails if actual warnings do not match candidate JSON
    monkeypatch.setattr(
        check_fortran_uninit,
        "compile_and_collect",
        lambda _root, gfortran="gfortran": (both_hits[:1], 114, new_compiler),
    )
    assert check_fortran_uninit.main(cli_args) == 1
    assert "stale baseline" in capsys.readouterr().err


def _run_fortran_uninit_base_resolution(
    repo: Path, *, event_name: str, pr_head_sha: str, base_sha: str
) -> str:
    """Execute the exact POSIX sh base-resolution block from fortran-uninit.yml."""
    workflow = yaml.safe_load(
        (REPO_ROOT / check_fortran_uninit.WORKFLOW_PATH).read_text(encoding="utf-8")
    )
    (job,) = workflow["jobs"].values()
    run_block = str(job["steps"][-1]["run"])
    # Keep only the base-resolution logic and emit the resolved BASE_SHA.
    resolve_only = run_block.split('if [ -n "$BASE_SHA" ]; then', 1)[0]
    script = (
        "set -eu\n"
        + "\n".join(
            line
            for line in resolve_only.splitlines()
            if "gfortran --version" not in line
        )
        + '\nprintf "%s" "$BASE_SHA"\n'
    )
    res = subprocess.run(
        ["sh", "-c", script],
        cwd=repo,
        env={
            **os.environ,
            "EVENT_NAME": event_name,
            "PR_HEAD_SHA": pr_head_sha,
            "BASE_SHA": base_sha,
        },
        capture_output=True,
        text=True,
        check=True,
    )
    return res.stdout


def test_fortran_uninit_workflow_base_resolution_matrix(tmp_path):
    """Workflow resolves HEAD^1 ONLY on synthetic PR merges; preserves merge_group/dispatch."""
    repo = tmp_path / "wf_base_repo"
    repo.mkdir()
    _init_git_repo(repo)

    # Base B1 on master
    (repo / "a.txt").write_text("b1\n", encoding="utf-8")
    b1 = _git_commit_all(repo, "b1")
    subprocess.run(
        ["git", "-C", str(repo), "branch", "-M", "master"],
        check=True,
        capture_output=True,
    )

    # PR1 branch from B1
    subprocess.run(
        ["git", "-C", str(repo), "checkout", "-b", "pr1", b1],
        check=True,
        capture_output=True,
    )
    (repo / "pr1.txt").write_text("pr1\n", encoding="utf-8")
    pr1_head = _git_commit_all(repo, "pr1")

    # Advance master to B2 (moving master)
    subprocess.run(
        ["git", "-C", str(repo), "checkout", "master"],
        check=True,
        capture_output=True,
    )
    (repo / "m.txt").write_text("b2\n", encoding="utf-8")
    b2 = _git_commit_all(repo, "b2")

    # 1. Synthetic pull_request merge commit (HEAD^1=B2, HEAD^2=pr1_head) -> resolves B2
    subprocess.run(
        ["git", "-C", str(repo), "checkout", "--detach", b2],
        check=True,
        capture_output=True,
    )
    subprocess.run(
        ["git", "-C", str(repo), "merge", "--no-ff", "-m", "synthetic pr1", pr1_head],
        check=True,
        capture_output=True,
    )
    assert (
        _run_fortran_uninit_base_resolution(
            repo, event_name="pull_request", pr_head_sha=pr1_head, base_sha=b1
        )
        == b2
    )

    # 2. Non-synthetic pull_request merge tip (HEAD is itself pr_head, so HEAD^2 != PR_HEAD_SHA)
    merged_pr_head = subprocess.check_output(
        ["git", "-C", str(repo), "rev-parse", "HEAD"], text=True
    ).strip()
    assert (
        _run_fortran_uninit_base_resolution(
            repo, event_name="pull_request", pr_head_sha=merged_pr_head, base_sha=b1
        )
        == b1
    )

    # 3. 2-PR merge_group batch (HEAD = merge PR2 onto (merge PR1 onto B2)) -> keeps B2
    subprocess.run(
        ["git", "-C", str(repo), "checkout", "-b", "pr2", b2],
        check=True,
        capture_output=True,
    )
    (repo / "pr2.txt").write_text("pr2\n", encoding="utf-8")
    pr2_head = _git_commit_all(repo, "pr2")
    subprocess.run(
        ["git", "-C", str(repo), "checkout", "--detach", merged_pr_head],
        check=True,
        capture_output=True,
    )
    subprocess.run(
        ["git", "-C", str(repo), "merge", "--no-ff", "-m", "batch pr2", pr2_head],
        check=True,
        capture_output=True,
    )
    assert (
        _run_fortran_uninit_base_resolution(
            repo, event_name="merge_group", pr_head_sha="", base_sha=b2
        )
        == b2
    )

    # 4. workflow_dispatch on a merge tip -> keeps BASE_SHA empty
    assert not _run_fortran_uninit_base_resolution(
        repo, event_name="workflow_dispatch", pr_head_sha="", base_sha=""
    )


def _load_snan_summariser():
    path = REPO_ROOT / "scripts" / "lint" / "summarise_snan_report.py"
    spec = importlib.util.spec_from_file_location("summarise_snan_report", path)
    assert spec is not None
    assert spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


def test_summarise_snan_report_flags_nan_failures_and_respects_strict(tmp_path, capsys):
    """JUnit summariser classifies NaN/non-finite failures and gates only in --strict."""
    summariser = _load_snan_summariser()
    expected_path = tmp_path / "expected_failures.json"
    expected_path.write_text(
        json.dumps({
            "description": "Synthetic allowlist fixture for advisory vs strict mode.",
            "entries": [
                {
                    "node": "test.physics.test_option_sweep::test_option_value_runs_finite_and_conserves_water[roughness_length_momentum=ALTERNATIVE]",
                    "issue": "#2105",
                    "reason": "Synthetic allowlisted failure",
                }
            ],
        }),
        encoding="utf-8",
    )
    xml_path = tmp_path / "junit.xml"
    xml_path.write_text(
        """\
<?xml version="1.0" encoding="utf-8"?>
<testsuites>
  <testsuite name="pytest" tests="667" errors="0" failures="1" skipped="6" time="216.1">
    <testcase classname="test.physics.test_option_sweep"
              name="test_option_value_runs_finite_and_conserves_water[roughness_length_momentum=ALTERNATIVE]"
              time="1.2">
      <failure message="RuntimeError: simulation failed (code 110): cal_Stab: non-finite roughness length">traceback</failure>
    </testcase>
  </testsuite>
</testsuites>
""",
        encoding="utf-8",
    )
    md_out = tmp_path / "summary.md"

    rc_advisory = summariser.main([
        "summarise_snan_report",
        str(xml_path),
        "--expected-failures",
        str(expected_path),
        "--pytest-exit-code",
        "1",
        "--markdown",
        str(md_out),
        "--compiler",
        "GNU Fortran (GCC) 14.2.0",
        "--github-annotations",
    ])
    assert rc_advisory == 0
    stdout = capsys.readouterr().out
    assert "::warning title=Fortran sNaN pilot (NaN/non-finite)::" in stdout
    md_text = md_out.read_text(encoding="utf-8")
    assert "roughness_length_momentum=ALTERNATIVE" in md_text
    assert "**NaN / non-finite tagged failures:** 1" in md_text

    rc_strict = summariser.main([
        "summarise_snan_report",
        str(xml_path),
        "--expected-failures",
        str(expected_path),
        "--pytest-exit-code",
        "1",
        "--strict",
    ])
    assert rc_strict == 1


def test_summarise_snan_report_rejects_zero_executed_and_empty_junit(tmp_path, capsys):
    """All-skipped (0 passed, 1 skipped) and empty JUnit reports always exit non-zero."""
    summariser = _load_snan_summariser()

    all_skipped = tmp_path / "all_skipped.xml"
    all_skipped.write_text(
        """\
<?xml version="1.0" encoding="utf-8"?>
<testsuites>
  <testsuite name="pytest" tests="1" errors="0" failures="0" skipped="1" time="0.1">
    <testcase classname="test.physics.test_dummy" name="test_only_skipped" time="0.0">
      <skipped message="skipped"/>
    </testcase>
  </testsuite>
</testsuites>
""",
        encoding="utf-8",
    )
    summary = summariser.parse_junit_xml(all_skipped)
    assert summary.executed == 0
    assert "All `-m physics` tests passed" not in summariser.render_markdown(summary)

    for extra in ([], ["--strict"]):
        rc = summariser.main(["summarise_snan_report", str(all_skipped), *extra])
        assert rc == 1
        assert "No physics tests executed" in capsys.readouterr().err

    empty_xml = tmp_path / "empty.xml"
    empty_xml.write_text(
        """\
<?xml version="1.0" encoding="utf-8"?>
<testsuites>
  <testsuite name="pytest" tests="0" errors="0" failures="0" skipped="0" time="0.0"/>
</testsuites>
""",
        encoding="utf-8",
    )
    for extra in ([], ["--strict"]):
        rc_empty = summariser.main(["summarise_snan_report", str(empty_xml), *extra])
        assert rc_empty == 1
        assert "recorded zero tests" in capsys.readouterr().err


def test_summarise_snan_report_fails_hard_on_real_pytest_collection_error(
    tmp_path, capsys
):
    """A real pytest collection error (exit 2 + JUnit <error>) fails hard even in advisory mode."""
    summariser = _load_snan_summariser()
    broken_test = tmp_path / "test_broken_collect.py"
    broken_test.write_text(
        'raise RuntimeError("boom during collection")\n', encoding="utf-8"
    )
    junit_xml = tmp_path / "collect_error.xml"

    proc = subprocess.run(
        [
            sys.executable,
            "-m",
            "pytest",
            "--noconftest",
            "-o",
            "addopts=",
            str(broken_test),
            f"--junitxml={junit_xml}",
        ],
        capture_output=True,
        text=True,
        check=False,
    )
    assert proc.returncode == 2
    assert junit_xml.is_file()

    # With raw --pytest-exit-code 2 (how the workflow invokes it): hard failure
    rc_with_code = summariser.main([
        "summarise_snan_report",
        str(junit_xml),
        "--pytest-exit-code",
        str(proc.returncode),
    ])
    assert rc_with_code == 1
    assert "exited abnormally with code 2" in capsys.readouterr().err

    # Even without --pytest-exit-code, collection <error> / zero executed bodies fails hard
    rc_without_code = summariser.main(["summarise_snan_report", str(junit_xml)])
    assert rc_without_code == 1
    assert "collection error" in capsys.readouterr().err


@pytest.mark.parametrize("bad_code", [2, 3, 4, 5, -11])
def test_summarise_snan_report_rejects_abnormal_pytest_exit_codes(
    tmp_path, capsys, bad_code
):
    """Non-0/1 pytest exit codes always fail hard even if JUnit recorded passing tests."""
    summariser = _load_snan_summariser()
    xml_path = tmp_path / "passing.xml"
    xml_path.write_text(
        """\
<?xml version="1.0" encoding="utf-8"?>
<testsuites>
  <testsuite name="pytest" tests="5" errors="0" failures="0" skipped="0" time="1.0"/>
</testsuites>
""",
        encoding="utf-8",
    )

    rc = summariser.main([
        "summarise_snan_report",
        str(xml_path),
        "--pytest-exit-code",
        str(bad_code),
    ])
    assert rc == 1
    assert f"code {bad_code}" in capsys.readouterr().err


def test_summarise_snan_report_fails_hard_on_mixed_pass_and_xdist_worker_error(
    tmp_path, capsys
):
    """Any JUnit <error> (e.g. xdist worker crash alongside a pass) fails hard in advisory mode."""
    summariser = _load_snan_summariser()
    xml_path = tmp_path / "xdist_crash.xml"
    xml_path.write_text(
        """\
<?xml version="1.0" encoding="utf-8"?>
<testsuites>
  <testsuite name="pytest" tests="2" errors="1" failures="0" skipped="0" time="4.2">
    <testcase classname="test.physics.test_ok" name="test_passes" time="1.0"/>
    <testcase classname="test.physics.test_crash" name="test_worker_killed" time="3.2">
      <error message="worker 'gw0' crashed while running 'test_worker_killed' (exit 86)">INTERNALERROR</error>
    </testcase>
  </testsuite>
</testsuites>
""",
        encoding="utf-8",
    )

    rc = summariser.main([
        "summarise_snan_report",
        str(xml_path),
        "--pytest-exit-code",
        "1",
        "--github-annotations",
    ])
    assert rc == 1
    captured = capsys.readouterr()
    assert "JUnit <error>" in captured.err
    assert "::error title=Fortran sNaN pilot (error)::" in captured.out

    # Exit code 1 with 0 failures and 0 errors in JUnit must also fail hard
    clean_xml = tmp_path / "clean_but_rc1.xml"
    clean_xml.write_text(
        """\
<?xml version="1.0" encoding="utf-8"?>
<testsuites>
  <testsuite name="pytest" tests="3" errors="0" failures="0" skipped="0" time="1.0"/>
</testsuites>
""",
        encoding="utf-8",
    )
    rc_mismatch = summariser.main([
        "summarise_snan_report",
        str(clean_xml),
        "--pytest-exit-code",
        "1",
    ])
    assert rc_mismatch == 1
    assert "recorded 0 failures and 0 errors" in capsys.readouterr().err


def test_summarise_snan_report_allowlist_passes_known_and_fails_new_failure(
    tmp_path, capsys
):
    """Only allowlisted failures stay advisory; any NEW failure exits 1."""
    summariser = _load_snan_summariser()
    committed_expected_path = (
        REPO_ROOT / "scripts" / "lint" / "fortran_snan_expected_failures.json"
    )
    # After gh#2105 is fixed, the committed sNaN allowlist has no remaining entries.
    assert summariser.load_expected_failures(committed_expected_path) == {}

    known_node = (
        "test.physics.test_example::test_known_snan_case[example=ALLOWLISTED]"
    )
    synthetic_expected = tmp_path / "expected_failures.json"
    synthetic_expected.write_text(
        json.dumps({
            "description": "Synthetic allowlist fixture for summarise_snan_report tests.",
            "entries": [
                {
                    "node": known_node,
                    "issue": "#9999",
                    "reason": "Synthetic allowlisted failure",
                }
            ],
        }),
        encoding="utf-8",
    )

    # Allowlisted-only failure -> advisory mode exits 0, --strict exits 1
    allowlisted_xml = tmp_path / "allowlisted_only.xml"
    allowlisted_xml.write_text(
        """\
<?xml version="1.0" encoding="utf-8"?>
<testsuites>
  <testsuite name="pytest" tests="10" errors="0" failures="1" skipped="0" time="5.0">
    <testcase classname="test.physics.test_example"
              name="test_known_snan_case[example=ALLOWLISTED]"
              time="1.2">
      <failure message="RuntimeError: simulation failed (code 110): non-finite value">tb</failure>
    </testcase>
  </testsuite>
</testsuites>
""",
        encoding="utf-8",
    )
    rc_advisory = summariser.main([
        "summarise_snan_report",
        str(allowlisted_xml),
        "--expected-failures",
        str(synthetic_expected),
        "--pytest-exit-code",
        "1",
    ])
    assert rc_advisory == 0
    capsys.readouterr()

    rc_strict = summariser.main([
        "summarise_snan_report",
        str(allowlisted_xml),
        "--expected-failures",
        str(synthetic_expected),
        "--pytest-exit-code",
        "1",
        "--strict",
    ])
    assert rc_strict == 1
    capsys.readouterr()

    # Known allowlisted + NEW unexpected failure -> advisory mode MUST exit 1
    mixed_xml = tmp_path / "mixed_failures.xml"
    mixed_xml.write_text(
        """\
<?xml version="1.0" encoding="utf-8"?>
<testsuites>
  <testsuite name="pytest" tests="10" errors="0" failures="2" skipped="0" time="5.0">
    <testcase classname="test.physics.test_example"
              name="test_known_snan_case[example=ALLOWLISTED]"
              time="1.2">
      <failure message="RuntimeError: simulation failed (code 110): non-finite value">tb</failure>
    </testcase>
    <testcase classname="test.physics.test_new_regression"
              name="test_unexpected_nan"
              time="0.8">
      <failure message="AssertionError: NaN in output QH">tb</failure>
    </testcase>
  </testsuite>
</testsuites>
""",
        encoding="utf-8",
    )
    rc_new = summariser.main([
        "summarise_snan_report",
        str(mixed_xml),
        "--expected-failures",
        str(synthetic_expected),
        "--pytest-exit-code",
        "1",
        "--github-annotations",
    ])
    assert rc_new == 1
    captured = capsys.readouterr()
    assert "test.physics.test_new_regression::test_unexpected_nan" in captured.err
    assert "::warning title=Fortran sNaN pilot (NaN/non-finite)::" in captured.out
    assert "::error title=Fortran sNaN pilot (NaN/non-finite)::" in captured.out

    # Clean passing run notes allowlisted failure did not occur
    all_pass_xml = tmp_path / "all_pass.xml"
    all_pass_xml.write_text(
        """\
<?xml version="1.0" encoding="utf-8"?>
<testsuites>
  <testsuite name="pytest" tests="10" errors="0" failures="0" skipped="0" time="5.0"/>
</testsuites>
""",
        encoding="utf-8",
    )
    rc_pass = summariser.main([
        "summarise_snan_report",
        str(all_pass_xml),
        "--expected-failures",
        str(synthetic_expected),
        "--pytest-exit-code",
        "0",
    ])
    assert rc_pass == 0
    assert "did not fail in this run" in capsys.readouterr().out


def test_snan_nightly_workflow_is_isolated_and_no_trap():
    """Nightly sNaN workflow stays isolated from shipped wheels and wires allowlist + exit code."""
    workflow_path = REPO_ROOT / ".github" / "workflows" / "fortran-snan-nightly.yml"
    text = workflow_path.read_text(encoding="utf-8")
    workflow = yaml.safe_load(text)
    triggers = workflow[True] if True in workflow else workflow["on"]
    assert set(triggers) == {"schedule", "workflow_dispatch"}

    (job,) = workflow["jobs"].values()
    assert job["env"]["SUEWS_INIT_REAL"] == "snan"
    assert "cibuildwheel" not in text
    assert "build-suews" not in text
    assert "wheelhouse" not in text
    run_blocks = "\n".join(str(step.get("run", "")) for step in job["steps"])
    assert "-ffpe-trap=invalid" not in run_blocks
    assert "-finit-real=snan" in run_blocks
    assert "PIPESTATUS[0]" in run_blocks
    assert '--pytest-exit-code "$PYTEST_EXIT"' in run_blocks
    assert (
        "--expected-failures scripts/lint/fortran_snan_expected_failures.json"
        in run_blocks
    )
