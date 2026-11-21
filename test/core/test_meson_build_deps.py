"""Guard tests for incremental build dependency tracking in src/supy/meson.build."""

from __future__ import annotations

import importlib.util
import os
from pathlib import Path
import re
import shutil
import subprocess
import sys

import pytest

pytestmark = [pytest.mark.api, pytest.mark.contract("C-TOOLING")]

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


SUEWS_SRC_TRACKED_SUFFIXES = {".f95", ".f90", ".F90", ".c", ".h"}
SUEWS_MAKEFILE = REPO_ROOT / "src" / "suews" / "Makefile"


def _extract_meson_string_list(text: str, name: str) -> set[str]:
    match = re.search(rf"{re.escape(name)}\s*=\s*\[(.*?)\]", text, re.S)
    assert match is not None, f"{name} list not found in src/supy/meson.build"
    return set(re.findall(r"'([^']+)'", match.group(1)))


def test_meson_build_lists_all_tracked_suews_sources() -> None:
    # ARRANGE
    text = MESON_BUILD.read_text(encoding="utf-8")
    declared_f95 = _extract_meson_string_list(text, "src_f95_all")
    declared_c = _extract_meson_string_list(text, "src_c_all")
    declared = declared_f95 | declared_c

    tracked = _tracked_files("src/suews/src")
    if tracked is not None:
        expected = {p.name for p in tracked if p.suffix in SUEWS_SRC_TRACKED_SUFFIXES}
    else:
        expected = {
            p.name
            for p in (REPO_ROOT / "src" / "suews" / "src").iterdir()
            if p.is_file()
            and p.suffix in SUEWS_SRC_TRACKED_SUFFIXES
            and p.name != "suews_ctrl_ver.f95"
        }

    # ACT
    missing = sorted(expected - declared)

    # ASSERT
    assert not missing, f"src/supy/meson.build omits SUEWS sources: {missing}"
    assert "strptime.c" in declared_c
    assert "suews_ctrl_error_tls.c" in declared_c
    assert "depend_files: [src_f95_files, src_c_files, makefile_deps]" in text


def test_suews_makefile_object_rules_depend_on_makefiles() -> None:
    # ARRANGE
    text = SUEWS_MAKEFILE.read_text(encoding="utf-8")

    # ASSERT
    for rule_header in (
        "src/suews_phys_spartacus.o: src/suews_phys_spartacus.f95 src/suews_phys_beers.o Makefile Makefile.$(PROFILE) | libspartacus",
        "%.o: %$(suffix_code) Makefile Makefile.$(PROFILE)",
        "%.o: %.c Makefile Makefile.$(PROFILE)",
    ):
        assert rule_header in text, f"Expected rule header {rule_header!r} in Makefile"


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


def test_run_make_collect_build_config_files_covers_makefiles() -> None:
    # ARRANGE
    helper = _load_helper("suews_run_make", "src/supy/run_make.py")
    suews_dir = REPO_ROOT / "src" / "suews"

    # ACT
    rel_paths = {
        p.relative_to(suews_dir).as_posix()
        for p in helper.collect_build_config_files(suews_dir)
    }

    # ASSERT
    for required in (
        "Makefile",
        "Makefile.gfortran",
        "Makefile.ifort",
        "ext_lib/spartacus-surface/Makefile",
        "ext_lib/spartacus-surface/Makefile_include.gfortran",
    ):
        assert required in rel_paths, f"Missing build config input: {required}"
    for excluded in (
        "Makefile.deps",
        "ext_lib/spartacus-surface/doc/Makefile",
        "ext_lib/spartacus-surface/test/urban/Makefile",
    ):
        assert (suews_dir / excluded).is_file(), f"Expected fixture file {excluded}"
        assert excluded not in rel_paths, (
            f"Non-build Makefile should not be hashed: {excluded}"
        )
    assert not any(
        any(part in {"doc", "docs", "test", "tests"} for part in Path(p).parts)
        for p in rel_paths
    ), f"Unused doc/test Makefiles should not be hashed: {sorted(rel_paths)}"


def _make_synthetic_suews_tree(root: Path) -> Path:
    suews_dir = root / "suews"
    (suews_dir / "src").mkdir(parents=True)
    (suews_dir / "mod").mkdir(parents=True)
    (suews_dir / "lib").mkdir(parents=True)
    (suews_dir / "ext_lib" / "spartacus-surface" / "radsurf").mkdir(parents=True)

    (suews_dir / "Makefile").write_text(
        "CFLAGS_C = -O2 -fPIC -Wall\n", encoding="utf-8"
    )
    (suews_dir / "Makefile.gfortran").write_text("OPTFLAGS = -O3\n", encoding="utf-8")
    (suews_dir / "Makefile.ifort").write_text("OPTFLAGS = -O3\n", encoding="utf-8")
    (suews_dir / "Makefile.deps").write_text(
        "src/suews_phys_lai.o: src/suews_ctrl_const.o\n", encoding="utf-8"
    )
    (suews_dir / "ext_lib" / "spartacus-surface" / "Makefile").write_text(
        "all:\n", encoding="utf-8"
    )
    return suews_dir


def test_run_make_fingerprint_tracks_flags_compiler_profile_and_makefiles(
    tmp_path: Path,
) -> None:
    # ARRANGE
    helper = _load_helper("suews_run_make", "src/supy/run_make.py")
    suews_dir = _make_synthetic_suews_tree(tmp_path)
    base_env: dict[str, str] = {}

    # ACT
    fp_base = helper.compute_build_fingerprint(
        suews_dir, fc=None, profile="release", profile_args=["DEBUG="], environ=base_env
    )
    fp_repeat = helper.compute_build_fingerprint(
        suews_dir, fc=None, profile="release", profile_args=["DEBUG="], environ=base_env
    )
    fp_checked = helper.compute_build_fingerprint(
        suews_dir,
        fc=None,
        profile="checked",
        profile_args=["DEBUG=1"],
        environ=base_env,
    )
    fp_fc = helper.compute_build_fingerprint(
        suews_dir,
        fc="/usr/bin/gfortran-14",
        profile="release",
        profile_args=["DEBUG="],
        environ=base_env,
    )
    fp_env = helper.compute_build_fingerprint(
        suews_dir,
        fc=None,
        profile="release",
        profile_args=["DEBUG="],
        environ={"SINGLE_PRECISION": "1"},
    )

    # ACT: routine Makefile.deps edge updates must not invalidate all objects
    (suews_dir / "Makefile.deps").write_text(
        "src/suews_phys_lai.o: src/suews_ctrl_const.o src/suews_type_surface.o\n",
        encoding="utf-8",
    )
    fp_deps_change = helper.compute_build_fingerprint(
        suews_dir, fc=None, profile="release", profile_args=["DEBUG="], environ=base_env
    )

    (suews_dir / "Makefile.gfortran").write_text("OPTFLAGS = -O0\n", encoding="utf-8")
    fp_opt_change = helper.compute_build_fingerprint(
        suews_dir, fc=None, profile="release", profile_args=["DEBUG="], environ=base_env
    )

    (suews_dir / "ext_lib" / "spartacus-surface" / "Makefile").write_text(
        "all:\n\t@true\n", encoding="utf-8"
    )
    fp_ext_change = helper.compute_build_fingerprint(
        suews_dir, fc=None, profile="release", profile_args=["DEBUG="], environ=base_env
    )

    # ASSERT
    assert fp_base == fp_repeat
    assert fp_deps_change == fp_base
    assert len({fp_base, fp_checked, fp_fc, fp_env, fp_opt_change, fp_ext_change}) == 6


def test_run_make_invalidate_stale_artifacts_cleans_only_on_change(
    tmp_path: Path,
) -> None:
    # ARRANGE
    helper = _load_helper("suews_run_make", "src/supy/run_make.py")
    suews_dir = _make_synthetic_suews_tree(tmp_path)

    src_f95 = suews_dir / "src" / "suews_phys_lai.f95"
    src_c = suews_dir / "src" / "strptime.c"
    src_f95.write_text("! source\n", encoding="utf-8")
    src_c.write_text("/* source */\n", encoding="utf-8")

    artifacts = [
        suews_dir / "src" / "suews_phys_lai.o",
        suews_dir / "mod" / "module_phys_lai.mod",
        suews_dir / "lib" / "libsuewsphys.a",
        suews_dir / "ext_lib" / "spartacus-surface" / "radsurf" / "radsurf_urban_sw.o",
    ]
    for art in artifacts:
        art.write_bytes(b"compiled")

    fp_o3 = helper.compute_build_fingerprint(
        suews_dir, fc=None, profile="release", profile_args=["DEBUG="], environ={}
    )
    fp_file = helper.write_build_fingerprint(suews_dir, fp_o3)

    # ACT 1: unchanged fingerprint preserves compiled artefacts and depfile tracks shared stamp
    removed_same = helper.invalidate_stale_artifacts(suews_dir, fp_o3)
    depfile_inputs = set(helper.collect_depfile_inputs(suews_dir))

    # ASSERT 1
    assert removed_same == []
    assert all(art.is_file() for art in artifacts)
    assert fp_file == suews_dir / helper.BUILD_FINGERPRINT_FILENAME
    assert fp_file.resolve() in depfile_inputs

    # ACT 2: changing Makefile.gfortran flags invalidates and removes all compiled artefacts
    (suews_dir / "Makefile.gfortran").write_text("OPTFLAGS = -O0\n", encoding="utf-8")
    fp_o0 = helper.compute_build_fingerprint(
        suews_dir, fc=None, profile="release", profile_args=["DEBUG="], environ={}
    )
    removed_changed = helper.invalidate_stale_artifacts(suews_dir, fp_o0)

    # ASSERT 2
    assert set(removed_changed) == set(artifacts)
    assert not any(art.exists() for art in artifacts)
    assert not fp_file.exists()
    assert src_f95.is_file()
    assert src_c.is_file()


def test_run_make_cross_builddir_profile_switch_invalidates_shared_objects(
    tmp_path: Path,
) -> None:
    # ARRANGE: two independent build dirs sharing one src/suews object tree
    helper = _load_helper("suews_run_make", "src/supy/run_make.py")
    suews_dir = _make_synthetic_suews_tree(tmp_path)
    build_a = tmp_path / "build-release"
    build_b = tmp_path / "build-checked"
    (build_a / "src" / "supy").mkdir(parents=True)
    (build_b / "src" / "supy").mkdir(parents=True)

    lai_o = suews_dir / "src" / "suews_phys_lai.o"
    lai_mod = suews_dir / "mod" / "module_phys_lai.mod"
    phys_a = suews_dir / "lib" / "libsuewsphys.a"
    rad_o = (
        suews_dir / "ext_lib" / "spartacus-surface" / "radsurf" / "radsurf_urban_sw.o"
    )
    shared_artifacts = [lai_o, lai_mod, phys_a, rad_o]

    fp_release = helper.compute_build_fingerprint(
        suews_dir, fc=None, profile="release", profile_args=["DEBUG="], environ={}
    )
    fp_checked = helper.compute_build_fingerprint(
        suews_dir, fc=None, profile="checked", profile_args=["DEBUG=1"], environ={}
    )

    # ACT 1: build dir A builds release and writes its Meson stamp + shared fingerprint
    for art in shared_artifacts:
        art.write_bytes(b"release-obj")
    helper.write_build_fingerprint(suews_dir, fp_release)
    (build_a / "src" / "supy" / "suews_objects").touch()

    # ACT 2: build dir B builds checked against the same src/suews tree
    removed_by_b = helper.invalidate_stale_artifacts(suews_dir, fp_checked)
    for art in shared_artifacts:
        art.write_bytes(b"checked-obj")
    helper.write_build_fingerprint(suews_dir, fp_checked)
    (build_b / "src" / "supy" / "suews_objects").touch()

    # ACT 3: build dir A returns in release; must detect B's checked objects and clean
    removed_by_a_return = helper.invalidate_stale_artifacts(suews_dir, fp_release)

    # ASSERT
    assert set(removed_by_b) == set(shared_artifacts)
    assert set(removed_by_a_return) == set(shared_artifacts)
    assert not any(art.exists() for art in shared_artifacts)
    assert not helper.fingerprint_path_for_suews(suews_dir).exists()


def test_gitignore_and_makefile_handle_shared_suews_build_state() -> None:
    # ARRANGE
    gitignore = (REPO_ROOT / ".gitignore").read_text(encoding="utf-8")
    makefile = SUEWS_MAKEFILE.read_text(encoding="utf-8")

    # ASSERT
    assert "src/suews/.suews_build_fingerprint" in gitignore
    assert "src/suews/.suews_build.lock" in gitignore
    assert "rm -f .suews_build_fingerprint" in makefile


def test_run_make_fingerprint_tracks_init_real_mode_with_zero_default(
    tmp_path: Path,
) -> None:
    # ARRANGE
    helper = _load_helper("suews_run_make", "src/supy/run_make.py")
    suews_dir = _make_synthetic_suews_tree(tmp_path)
    obj = suews_dir / "src" / "suews_phys_resist.o"
    obj.write_bytes(b"zero-obj")

    # ACT: unset and explicit "zero" share one fingerprint; "snan" diverges
    fp_unset = helper.compute_build_fingerprint(
        suews_dir, fc=None, profile="release", profile_args=["DEBUG="], environ={}
    )
    fp_zero = helper.compute_build_fingerprint(
        suews_dir,
        fc=None,
        profile="release",
        profile_args=["DEBUG="],
        environ={"SUEWS_INIT_REAL": "zero"},
    )
    fp_snan = helper.compute_build_fingerprint(
        suews_dir,
        fc=None,
        profile="release",
        profile_args=["DEBUG="],
        environ={"SUEWS_INIT_REAL": "snan"},
    )
    fp_empty = helper.compute_build_fingerprint(
        suews_dir,
        fc=None,
        profile="release",
        profile_args=["DEBUG="],
        environ={"SUEWS_INIT_REAL": ""},
    )

    helper.write_build_fingerprint(suews_dir, fp_zero)
    removed_unset = helper.invalidate_stale_artifacts(suews_dir, fp_unset)
    removed_to_snan = helper.invalidate_stale_artifacts(suews_dir, fp_snan)
    obj.write_bytes(b"snan-obj")
    helper.write_build_fingerprint(suews_dir, fp_snan)
    removed_to_zero = helper.invalidate_stale_artifacts(suews_dir, fp_zero)

    # ASSERT
    assert fp_unset == fp_zero
    assert fp_snan != fp_zero
    assert fp_empty != fp_zero
    assert removed_unset == []
    assert removed_to_snan == [obj]
    assert removed_to_zero == [obj]


def test_run_make_tracks_all_makefile_conditional_assign_vars() -> None:
    # ARRANGE: parse every top-level `VAR ?= <default>` in the SUEWS Makefiles
    helper = _load_helper("suews_run_make", "src/supy/run_make.py")
    suews_dir = REPO_ROOT / "src" / "suews"
    profile_arg_vars = {
        arg.split("=", 1)[0]
        for profile in helper.BUILD_PROFILES
        for arg in helper.make_args_for_profile(profile)
        if "=" in arg
    }
    discovered: dict[str, str] = {}
    for name in ("Makefile", "Makefile.gfortran", "Makefile.ifort"):
        text = (suews_dir / name).read_text(encoding="utf-8")
        for line in text.splitlines():
            stripped = line.strip()
            if not stripped or stripped.startswith("#"):
                continue
            match = re.match(r"^([A-Z][A-Z0-9_]*)\s*\?=\s*(.*?)\s*(?:#.*)?$", stripped)
            if match is not None:
                discovered[match.group(1)] = match.group(2)

    # ACT: every ?= variable must be overridden via profile_args or tracked in _MAKE_ENV_KEYS
    tracked_env_keys = set(helper._MAKE_ENV_KEYS)
    untracked = sorted(
        var
        for var in discovered
        if var not in profile_arg_vars and var not in tracked_env_keys
    )

    # ASSERT
    assert "SUEWS_INIT_REAL" in discovered
    assert "SUEWS_INIT_REAL" in tracked_env_keys
    assert helper._MAKE_ENV_DEFAULTS["SUEWS_INIT_REAL"] == discovered["SUEWS_INIT_REAL"]
    assert not untracked, (
        "SUEWS Makefile `?=` variables missing from run_make._MAKE_ENV_KEYS: "
        f"{untracked}"
    )


def test_run_make_sync_builddir_invalidates_only_on_env_change(tmp_path: Path) -> None:
    # ARRANGE
    helper = _load_helper("suews_run_make", "src/supy/run_make.py")
    suews_dir = _make_synthetic_suews_tree(tmp_path)
    build_dir = tmp_path / "build" / "cp312"
    supy_build = build_dir / "src" / "supy"
    (supy_build / "bin").mkdir(parents=True)

    expected_outputs = {
        supy_build / "suews_objects",
        supy_build / "suews_bridge.abi3.so",
        supy_build / "suews-engine",
        supy_build / "bin" / "suews-engine",
    }
    for path in expected_outputs:
        path.write_bytes(b"zero-build")

    fp_zero = helper.compute_build_fingerprint(
        suews_dir, fc=None, profile="release", profile_args=["DEBUG="], environ={}
    )
    helper.write_build_fingerprint(suews_dir, fp_zero)

    # ACT & ASSERT 1: matching env (unset or explicit "zero") leaves stamps untouched
    assert helper.sync_builddir_for_env(suews_dir, build_dir, environ={}) == []
    assert (
        helper.sync_builddir_for_env(
            suews_dir, build_dir, environ={"SUEWS_INIT_REAL": "zero"}
        )
        == []
    )
    assert all(path.is_file() for path in expected_outputs)

    # ACT & ASSERT 2: switching to SUEWS_INIT_REAL=snan invalidates Meson & bridge outputs
    assert (
        set(
            helper.sync_builddir_for_env(
                suews_dir, build_dir, environ={"SUEWS_INIT_REAL": "snan"}
            )
        )
        == expected_outputs
    )
    assert not any(path.exists() for path in expected_outputs)

    # ACT & ASSERT 3: switching SUEWS_BUILD_PROFILE also invalidates stamps
    for path in expected_outputs:
        path.write_bytes(b"snan-build")
    fp_snan = helper.compute_build_fingerprint(
        suews_dir,
        fc=None,
        profile="release",
        profile_args=["DEBUG="],
        environ={"SUEWS_INIT_REAL": "snan"},
    )
    helper.write_build_fingerprint(suews_dir, fp_snan)
    assert (
        set(
            helper.sync_builddir_for_env(
                suews_dir,
                build_dir,
                environ={"SUEWS_INIT_REAL": "snan", "SUEWS_BUILD_PROFILE": "checked"},
            )
        )
        == expected_outputs
    )

    # Root Makefile wires --sync-builddir into both dev and rebuild-meson targets
    root_makefile = (REPO_ROOT / "Makefile").read_text(encoding="utf-8")
    assert root_makefile.count("run_make.py --sync-builddir src/suews") >= 2


@pytest.mark.parametrize(
    ("content", "expected"),
    [
        (
            "out: /src/a\\ b.rs /src/hash\\#file.rs /src/cash$$.rs \\\n"
            " /src/end.rs\n/src/a\\ b.rs:\n# env-dep:SUEWS_VERSION=1.0\n",
            ["/src/a b.rs", "/src/hash#file.rs", "/src/cash$.rs", "/src/end.rs"],
        ),
        (
            "C:\\target\\suews.dll: C:\\repo\\src\\lib.rs C:/repo/a\\ b.rs C:/repo/h\\#x.rs\r\n",
            [r"C:\repo\src\lib.rs", "C:/repo/a b.rs", "C:/repo/h#x.rs"],
        ),
    ],
)
def test_cargo_depfile_paths_and_metadata(
    tmp_path: Path, content: str, expected: list[str]
) -> None:
    helper = _load_helper("cargo_dep_paths", "src/supy/build_rust_bridge.py")
    depfile = tmp_path / "cargo.d"
    depfile.write_text(content, encoding="utf-8")
    assert helper.read_cargo_depfile(depfile) == expected


@pytest.mark.parametrize("library_name", ["libsuews_bridge.d", "suews_bridge.d"])
def test_cargo_inputs_exclude_outputs_and_keep_worktree_git(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch, library_name: str
) -> None:
    helper = _load_helper("cargo_dep_inputs", "src/supy/build_rust_bridge.py")
    root = tmp_path / "linked checkout"
    target = tmp_path / "cargo cache"
    release = target / "release"
    release.mkdir(parents=True)
    root.mkdir()
    git_dir = tmp_path / "common.git" / "worktrees" / "checkout"
    git_dir.mkdir(parents=True)
    (root / ".git").write_text(f"gitdir: {git_dir}\n", encoding="utf-8")
    git_head = git_dir / "HEAD"
    git_head.write_text("ref: refs/heads/example\n", encoding="utf-8")
    source = root / "input #1.txt"
    source.write_text("source", encoding="utf-8")
    generated = release / "build" / "crate" / "out" / "output_cols.rs"
    generated.parent.mkdir(parents=True)
    generated.write_text("generated", encoding="utf-8")
    library = root / "libsuewsphys.a"
    library.write_bytes(b"archive fixture")
    helper.write_depfile(
        release / library_name, "output.so", [source, generated, git_head]
    )
    helper.write_depfile(release / "suews-engine.d", "engine", [source, library])
    # A stale hashed depfile from another feature set must never enter the graph.
    (release / "deps").mkdir()
    (release / "deps" / "stale.d").write_text(
        "out: /missing/stale\n", encoding="utf-8"
    )
    monkeypatch.chdir(root)
    assert set(helper.collect_cargo_depfile_inputs(release, target)) == {
        source.resolve(),
        git_head.resolve(),
        library.resolve(),
    }


def test_cargo_depfile_missing_is_not_silently_ignored(tmp_path: Path) -> None:
    helper = _load_helper("cargo_dep_missing", "src/supy/build_rust_bridge.py")
    with pytest.raises(FileNotFoundError):
        helper.collect_cargo_depfile_inputs(tmp_path, tmp_path)


def test_depfile_writer_windows_and_hash_paths(tmp_path: Path) -> None:
    from pathlib import PureWindowsPath

    helper = _load_helper("cargo_dep_writer", "src/supy/build_rust_bridge.py")
    depfile = tmp_path / "out.d"
    helper.write_depfile(
        depfile, r"C:\build dir\bridge.dll",
        [PureWindowsPath(r"C:\source dir\input#1.rs")],
    )
    assert depfile.read_text(encoding="utf-8") == (
        "C:/build\\ dir/bridge.dll: C:/source\\ dir/input\\#1.rs\n"
    )


@pytest.mark.parametrize("caller", ["build_rust_bridge.py", "run_make.py"])
def test_ninja_consumes_escaped_cargo_inputs(tmp_path: Path, caller: str) -> None:
    """Use Ninja itself as the oracle for spaces, hashes and dollar escaping."""
    ninja = shutil.which("ninja")
    if ninja is None:
        pytest.skip("Ninja is not installed; required for depfile consumer integration")
    helper = _load_helper("cargo_ninja", f"src/supy/{caller}")
    source = tmp_path / "source space#$.txt"
    source.write_text("before", encoding="utf-8")
    # The command models the wrapper writing its output and depfile together.
    helper.write_depfile(tmp_path / "result.d", "result", [source])
    dep_text = (tmp_path / "result.d").read_text(encoding="utf-8")
    script = tmp_path / "build.py"
    script.write_text(
        "from pathlib import Path\n"
        "Path('result').write_text('built', encoding='utf-8')\n"
        f"Path('result.d').write_text({dep_text!r}, encoding='utf-8')\n",
        encoding="utf-8",
    )
    python_executable = sys.executable.replace("$", "$$")
    (tmp_path / "build.ninja").write_text(
        "rule compile\n"
        f'  command = "{python_executable}" build.py\n'
        "  depfile = result.d\n"
        "build result: compile\n",
        encoding="utf-8",
    )

    def run_ninja(*args: str) -> str:
        return subprocess.run(
            [ninja, *args], cwd=tmp_path, check=True, capture_output=True, text=True
        ).stdout

    run_ninja()
    assert "no work to do" in run_ninja()
    source.write_text("after", encoding="utf-8")
    # Avoid relying on filesystem timestamp granularity; dry-run only after
    # setting the input later than the output, with no sleep or retry loop.
    future = (tmp_path / "result").stat().st_mtime_ns + 2_000_000_000
    os.utime(source, ns=(future, future))
    assert "build.py" in run_ninja("-n")


@pytest.mark.parametrize("caller", ["build_rust_bridge.py", "run_make.py"])
def test_build_script_import_from_arbitrary_cwd(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch, caller: str
) -> None:
    # Neither a same-named module in cwd nor an installed supy is required.
    (tmp_path / "_build_depfile.py").write_text(
        "raise RuntimeError('wrong helper')\n", encoding="utf-8"
    )
    script_dir = tmp_path / "scripts with spaces"
    script_dir.mkdir()
    script = script_dir / caller
    shutil.copy2(REPO_ROOT / "src/supy" / caller, script)
    shutil.copy2(
        REPO_ROOT / "src/supy/_build_depfile.py", script_dir / "_build_depfile.py"
    )
    monkeypatch.chdir(tmp_path)
    before = sys.path.copy()
    helper = _load_helper("standalone_build_script", str(script))
    assert sys.path == before
    helper.write_depfile(tmp_path / "probe.d", "probe", [])
    assert (tmp_path / "probe.d").read_text(encoding="utf-8") == "probe:\n"
    # No arguments exercises script-by-path imports, then exits at usage; no build.
    result = subprocess.run(
        [sys.executable, "-I", str(script)],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )
    assert result.returncode == (2 if caller == "build_rust_bridge.py" else 1)
    assert "usage:" in (result.stdout + result.stderr).lower()
    assert "Traceback" not in result.stderr


@pytest.mark.parametrize("caller", ["build_rust_bridge.py", "run_make.py"])
@pytest.mark.parametrize("windows", [False, True])
def test_shared_writer_preserves_format_and_replaces_contents(
    tmp_path: Path, caller: str, windows: bool
) -> None:
    from pathlib import PurePosixPath, PureWindowsPath

    helper = _load_helper("writer_semantics", f"src/supy/{caller}")
    path_type = PureWindowsPath if windows else PurePosixPath
    prefix = "C:/" if windows else "/"
    deps = [
        path_type(prefix + "source dir/a#$1.rs"),
        path_type(prefix + "plain.f95"),
    ]
    target = prefix + "build dir/out#$1"
    raw_target = target.replace("/", "\\") if windows else target
    depfile = tmp_path / "new directory" / "result.d"
    helper.write_depfile(depfile, raw_target, [*deps, deps[0]])
    escaped_target = prefix + "build\\ dir/out\\#$$1"
    escaped_source = prefix + "source\\ dir/a\\#$$1.rs"
    assert depfile.read_text(encoding="utf-8") == (
        escaped_target + ": " + escaped_source + " \\\n  "
        + prefix + "plain.f95 \\\n  " + escaped_source + "\n"
    )
    # Existing semantics: retain order/duplicates, create parents, overwrite and
    # produce an empty rule (not stale prerequisites) on the next invocation.
    helper.write_depfile(depfile, raw_target, [])
    assert depfile.read_text(encoding="utf-8") == escaped_target + ":\n"


def test_shared_helper_is_declared_for_both_targets_and_installed() -> None:
    text = MESON_BUILD.read_text(encoding="utf-8")
    for start, end in (
        ("py.install_sources(", "makefile_deps = files("),
        ("makefile_deps = files(", "build_suews = custom_target("),
        ("rust_bridge_dep_files = files(", "rust_bridge_artifacts = custom_target("),
    ):
        assert "'_build_depfile.py'" in text.split(start, 1)[1].split(end, 1)[0]
    expected = (REPO_ROOT / "src/supy/_build_depfile.py").resolve()
    bridge = _load_helper("bridge_helper_deps", "src/supy/build_rust_bridge.py")
    make = _load_helper("make_helper_deps", "src/supy/run_make.py")
    assert expected in bridge.collect_depfile_inputs(REPO_ROOT)
    assert expected in make.collect_depfile_inputs(REPO_ROOT / "src/suews")


def test_run_make_output_record_preserves_fingerprint_timestamp(tmp_path: Path) -> None:
    helper = _load_helper("make_timestamps", "src/supy/run_make.py")
    suews = _make_synthetic_suews_tree(tmp_path)
    fingerprint = helper.write_build_fingerprint(suews, "unchanged")
    old = fingerprint.stat().st_mtime_ns - 10_000_000_000
    os.utime(fingerprint, ns=(old, old))
    stamp = tmp_path / "build" / "suews_objects"
    depfile = tmp_path / "build" / "suews_objects.d"
    for _ in range(2):
        helper._record_build_outputs(suews, str(stamp), stamp, depfile, "unchanged")
        assert fingerprint.stat().st_mtime_ns == old
        assert stamp.stat().st_mtime_ns >= old
        assert fingerprint.as_posix() in depfile.read_text(encoding="utf-8")
    helper._record_build_outputs(suews, str(stamp), stamp, None, "unchanged")
    assert fingerprint.stat().st_mtime_ns == old


@pytest.mark.parametrize("missing", ["library", "engine"])
def test_missing_cargo_depfile_does_not_publish_outputs(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch, missing: str
) -> None:
    helper = _load_helper("bridge_fail_closed", "src/supy/build_rust_bridge.py")
    target = tmp_path / "cargo"
    release = target / "release"
    if os.name == "nt":
        release = target / "x86_64-pc-windows-gnu" / "release"
    release.mkdir(parents=True)
    ext = release / "fake-extension"
    cli = release / "fake-cli"
    ext.write_bytes(b"new extension")
    cli.write_bytes(b"new cli")
    present = "suews-engine.d" if missing == "library" else "libsuews_bridge.d"
    (release / present).write_text("artifact: input.rs\n", encoding="utf-8")
    output_ext = tmp_path / "extension"
    output_cli = tmp_path / "engine"
    output_ext.write_bytes(b"old extension")
    output_cli.write_bytes(b"old cli")
    depfile = tmp_path / "meson.d"
    depfile.write_text("old: inputs\n", encoding="utf-8")
    outputs = [output_ext, output_cli, depfile]
    before = [(p.read_bytes(), p.stat().st_mtime_ns) for p in outputs]
    monkeypatch.setenv("CARGO_TARGET_DIR", str(target))
    monkeypatch.setattr(helper.subprocess, "run", lambda *args, **kwargs: None)
    monkeypatch.setattr(helper, "_find_extension_artifact", lambda _: ext)
    monkeypatch.setattr(helper, "_find_cli_artifact", lambda _: cli)
    monkeypatch.setattr(
        sys, "argv",
        ["build_rust_bridge.py", str(tmp_path), str(output_ext), str(output_cli), str(depfile)],
    )
    with pytest.raises(FileNotFoundError):
        helper.main()
    assert [(p.read_bytes(), p.stat().st_mtime_ns) for p in outputs] == before
    # The legacy three-argument invocation does not request dependency output.
    monkeypatch.setattr(sys, "argv", sys.argv[:-1])
    assert helper.main() == 0
    assert output_ext.read_bytes() == b"new extension"
    assert output_cli.read_bytes() == b"new cli"
    assert (depfile.read_bytes(), depfile.stat().st_mtime_ns) == before[2]
