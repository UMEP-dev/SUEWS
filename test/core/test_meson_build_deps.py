"""Guard tests for incremental build dependency tracking in src/supy/meson.build."""

from __future__ import annotations

import importlib.util
from pathlib import Path
import re
import subprocess

import pytest

pytestmark = pytest.mark.api

REPO_ROOT = Path(__file__).resolve().parents[2]
MESON_BUILD = REPO_ROOT / "src" / "supy" / "meson.build"


def _load_helper(name: str, rel_path: str):
    path = REPO_ROOT / rel_path
    spec = importlib.util.spec_from_file_location(name, path)
    assert spec is not None and spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def _tracked_files(prefix: str) -> list[Path] | None:
    """Return tracked files under ``prefix`` when inside a git checkout, else None."""
    try:
        res = subprocess.run(
            ["git", "ls-files", prefix],
            cwd=REPO_ROOT,
            check=True,
            capture_output=True,
            text=True,
        )
    except (OSError, subprocess.CalledProcessError):
        return None
    return [REPO_ROOT / line for line in res.stdout.splitlines() if line.strip()]


def test_meson_build_lists_all_tracked_suews_fortran_sources() -> None:
    # ARRANGE
    text = MESON_BUILD.read_text(encoding="utf-8")
    match = re.search(r"src_f95_all\s*=\s*\[(.*?)\]", text, re.S)
    assert match is not None, "src_f95_all list not found in src/supy/meson.build"
    declared = set(re.findall(r"'([^']+\.f95)'", match.group(1)))

    tracked = _tracked_files("src/suews/src")
    if tracked is not None:
        expected = {p.name for p in tracked if p.suffix == ".f95"}
    else:
        expected = {
            p.name
            for p in (REPO_ROOT / "src" / "suews" / "src").glob("*.f95")
            if p.name != "suews_ctrl_ver.f95"
        }

    # ACT
    missing = sorted(expected - declared)

    # ASSERT
    assert not missing, f"src/supy/meson.build omits Fortran sources: {missing}"


def test_meson_build_declares_makefiles_cargo_lock_and_depfiles() -> None:
    # ARRANGE
    text = MESON_BUILD.read_text(encoding="utf-8")

    # ASSERT
    for token in (
        "Makefile.gfortran",
        "Makefile.ifort",
        "'run_make.py'",
        "'../suews_bridge/Cargo.lock'",
        "'build_rust_bridge.py'",
        "depfile: 'suews_objects.d'",
        "depfile: 'rust_bridge_artifacts.d'",
        "'@DEPFILE@'",
    ):
        assert token in text, f"Expected {token!r} in src/supy/meson.build"


def test_build_rust_bridge_depfile_covers_all_rs_and_c_api_sources() -> None:
    # ARRANGE
    helper = _load_helper("suews_build_rust_bridge", "src/supy/build_rust_bridge.py")

    # ACT
    deps = set(helper.collect_depfile_inputs(REPO_ROOT))

    # ASSERT
    bridge_root = REPO_ROOT / "src" / "suews_bridge"
    expected_rs = {p.resolve() for p in (bridge_root / "src").rglob("*.rs")}
    expected_c_api = {p.resolve() for p in (bridge_root / "c_api").rglob("*.f95")}
    assert expected_rs and expected_rs.issubset(deps), (
        f"Missing .rs files from depfile: {sorted(expected_rs - deps)[:5]}"
    )
    assert expected_c_api and expected_c_api.issubset(deps), (
        f"Missing c_api/*.f95 files from depfile: {sorted(expected_c_api - deps)[:5]}"
    )
    for required in (
        bridge_root / "Cargo.toml",
        bridge_root / "Cargo.lock",
        bridge_root / "build.rs",
        REPO_ROOT / "src" / "supy" / "build_rust_bridge.py",
    ):
        assert required.resolve() in deps


def test_run_make_depfile_covers_ext_lib_sources() -> None:
    # ARRANGE
    helper = _load_helper("suews_run_make", "src/supy/run_make.py")
    suews_dir = REPO_ROOT / "src" / "suews"

    # ACT
    deps = set(helper.collect_ext_lib_inputs(suews_dir))

    # ASSERT
    spartacus_f90 = {
        p.resolve()
        for p in (suews_dir / "ext_lib" / "spartacus-surface").rglob("*.F90")
    }
    assert spartacus_f90 and spartacus_f90.issubset(deps)


def test_depfile_writer_escapes_spaces_and_dollar(tmp_path: Path) -> None:
    # ARRANGE
    helper = _load_helper("suews_build_rust_bridge", "src/supy/build_rust_bridge.py")
    depfile = tmp_path / "out.d"
    dep_a = tmp_path / "dir with space" / "a$1.rs"
    dep_b = tmp_path / "plain.f95"

    # ACT
    helper.write_depfile(depfile, "src/supy/suews_bridge.abi3.so", [dep_a, dep_b])
    text = depfile.read_text(encoding="utf-8")

    # ASSERT
    assert text.startswith("src/supy/suews_bridge.abi3.so: ")
    assert "dir\\ with\\ space/a$$1.rs" in text
    assert dep_b.as_posix() in text
