"""One hosted Windows archive-mtime experiment for #2147; never run on a Mac.

The source copies remain at the two public revisions below. No source patches,
synthetic targets, dry runs, cache restores or wheel/release publication occur.
Only src/suews/lib/libsuewsphys.a is touched, using the runner's current time.

Build prerequisites come from .github/actions/build-suews/action.yml (UCRT64,
CPython 3.12, flags and setjmp), pyproject.toml (build requirements and limited
API), and src/supy/build_rust_bridge.py (features, GNU target and copy2 outputs).
src/suews_bridge/build.rs names libsuewsdriver.a, libsuewsphys.a, libspartacus.a
and libsuewsutil.a under src/suews/lib. Its generated libsuews_bridge.a lives
inside Cargo's output directory and is not the archive being touched.

Evidence includes the actual top-level Cargo libsuews_bridge.d and/or
suews_bridge.d, suews-engine.d, Meson's rust_bridge_artifacts.d and
suews_objects.d, plus Cargo build-script/fingerprint records. A missing control
fails the job; neither elapsed time nor changed hashes alone counts as a pass.
"""

import argparse
import hashlib
import json
import ntpath
import os
from pathlib import Path
import platform
import re
import shutil
import struct
import subprocess
import sys
import sysconfig
import time
import tomllib
import traceback
import zipfile


REVISIONS = {
    "parent": "8bc069e94294b31199ac593ce57145871b501e22",
    "candidate": "f64fa20607fe31b75c6ee5687499e1e8575e60a9",
}
TARGET = "x86_64-pc-windows-gnu"
FEATURES = "python-extension,physics,arrow-output"
UCRT = Path("C:/msys64/ucrt64")
MSYS = Path("C:/msys64/usr/bin")
ARCHIVES = ("libsuewsdriver.a", "libsuewsphys.a", "libspartacus.a", "libsuewsutil.a")
BUILD_ENV = {
    "CC": "gcc",
    "CXX": "g++",
    "FC": "gfortran",
    "CFLAGS": "-D__USE_MINGW_ANSI_STDIO=1 -mlong-double-64",
    "CXXFLAGS": "-D__USE_MINGW_ANSI_STDIO=1 -mlong-double-64",
    "FCFLAGS": "-fno-optimize-sibling-calls -ffree-line-length-none",
    "LDFLAGS": "-lucrt -static-libgcc -static-libgfortran -LC:/msys64/ucrt64/lib -lsetjmp_compat",
    "SUEWS_BUILD_PROFILE": "release",
    "SUEWS_INIT_REAL": "zero",
    "CARGO_TARGET_X86_64_PC_WINDOWS_GNU_LINKER": "gcc",
    # The unmodified build wrapper has no -v option. Cargo's documented verbose
    # setting exposes its rustc commands; fingerprint logging gives dirty reasons.
    "CARGO_TERM_VERBOSE": "true",
    "CARGO_TERM_COLOR": "never",
    "CARGO_LOG": "cargo::core::compiler::fingerprint=info",
    "MSYSTEM": "UCRT64",
    "MSYS2_PATH_TYPE": "inherit",
    "CHERE_INVOKING": "1",
    "SHELL": "C:/msys64/usr/bin/sh.exe",
    "MAKESHELL": "C:/msys64/usr/bin/sh.exe",
    "PIP_ONLY_BINARY": ":all:",
    "PIP_PREFER_BINARY": "1",
}


def require(condition: bool, message: str) -> None:
    """Fail explicitly, including under python -O."""
    if not condition:
        raise RuntimeError(message)


def write_json(path: Path, value: object) -> None:
    """Write durable, readable evidence."""
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(value, indent=2, sort_keys=True) + "\n", encoding="utf-8")


def run(command: list[str], cwd: Path, env: dict[str, str], log: Path,
        *, check: bool = True) -> str:
    """Keep merged child output on disk even on error or process termination."""
    log.parent.mkdir(parents=True, exist_ok=True)
    # CreateProcess does not use the supplied child environment's PATH to find
    # the executable. Resolve it ourselves; nested Python receives this PATH.
    executable = shutil.which(command[0], path=env["PATH"])
    require(executable is not None, f"Executable missing: {command[0]}")
    command = [executable, *command[1:]]
    record = {"argv": command, "cwd": str(cwd), "start_ns": time.time_ns()}
    write_json(log.with_suffix(".command.json"), record)
    print(f"Running {command!r} in {cwd}; transcript: {log}", flush=True)
    try:
        with log.open("w", encoding="utf-8") as stream:
            result = subprocess.run(command, cwd=cwd, env=env, stdout=stream,
                                    stderr=subprocess.STDOUT, timeout=1200, check=False)
        record["returncode"] = result.returncode
    finally:
        record["end_ns"] = time.time_ns()
        write_json(log.with_suffix(".command.json"), record)
    text = log.read_text(encoding="utf-8", errors="replace")
    print(text, flush=True)
    if check:
        require(result.returncode == 0, f"Command failed ({result.returncode}): {log}")
    return text


def metadata(path: Path) -> dict:
    """Record nanosecond timestamps and SHA256 without changing file contents."""
    if not path.is_file():
        return {"path": str(path), "missing": True}
    stat = path.stat()
    with path.open("rb") as stream:
        digest = hashlib.file_digest(stream, "sha256").hexdigest()
    return {"path": str(path), "mtime_ns": stat.st_mtime_ns,
            "size": stat.st_size, "sha256": digest}


def layout(workspace: Path, root: Path, case: str) -> tuple[Path, Path, Path, Path]:
    """Keep independent source, Meson, Cargo and evidence directories per case."""
    return (workspace / case, root / case / "meson", root / case / "cargo",
            root / "evidence" / case)


def artefacts(source: Path, build: Path, cargo: Path) -> dict[str, Path]:
    """Name both native Cargo outputs and the two Meson copies separately."""
    release = cargo / TARGET / "release"
    bridge = release / "suews_bridge.dll"
    if not bridge.is_file() and (release / "libsuews_bridge.dll").is_file():
        bridge = release / "libsuews_bridge.dll"
    return {
        "outer_bridge": build / "src/supy/suews_bridge.pyd",
        "outer_engine": build / "src/supy/suews-engine.exe",
        "inner_bridge": bridge,
        "inner_engine": release / "suews-engine.exe",
        "archive": source / "src/suews/lib/libsuewsphys.a",
        "ninja_log": build / ".ninja_log",
    }


def snapshot(paths: dict[str, Path], destination: Path) -> dict:
    """Save measurements before evaluating any assertion about them."""
    state = {key: metadata(path) for key, path in paths.items()}
    write_json(destination, {"wall_ns": time.time_ns(), "files": state})
    return state


def preserve(source: Path, build: Path, cargo: Path, evidence: Path, stage: str) -> None:
    """Archive dependency records and real physics archives, including partial builds."""
    evidence.mkdir(parents=True, exist_ok=True)
    snapshot(artefacts(source, build, cargo), evidence / f"{stage}-state.json")
    paths = []
    for base, patterns in (
        (source, ["src/suews/lib/*.a", "src/suews/.suews_build_fingerprint",
                  "src/supy/build_rust_bridge.py", "src/supy/run_make.py",
                  "src/supy/_build_depfile.py", "src/supy/meson.build",
                  "src/suews_bridge/build.rs", "src/suews_bridge/Cargo.lock"]),
        (build, ["build.ninja", ".ninja_log", ".ninja_deps", "src/supy/*.d",
                 "meson-logs/*", "meson-info/*.json"]),
        (cargo, [f"{TARGET}/release/*.d", "**/build/suews_bridge-*/output",
                 "**/build/suews_bridge-*/stderr", "**/build/suews_bridge-*/out/*.a",
                 "**/.fingerprint/suews_bridge-*/*"]),
    ):
        label = {source: "source", build: "meson", cargo: "cargo"}[base]
        for pattern in patterns:
            paths.extend((path, f"{label}/{path.relative_to(base).as_posix()}")
                         for path in base.glob(pattern) if path.is_file())
    with zipfile.ZipFile(evidence / f"{stage}-dependencies.zip", "w",
                         compression=zipfile.ZIP_DEFLATED) as archive:
        manifest = {}
        for path, name in sorted(set(paths)):
            archive.write(path, name)
            manifest[name] = metadata(path)
        archive.writestr("manifest.json", json.dumps(manifest, indent=2, sort_keys=True))


def check_sources(workspace: Path, root: Path, env: dict[str, str]) -> None:
    """Check exact public commits, ancestry and clean tracked sources before setup."""
    for case, expected in REVISIONS.items():
        source, _, _, evidence = layout(workspace, root, case)
        actual = run(["git", "rev-parse", "HEAD"], source, env, evidence / "revision.log").strip()
        require(actual == expected, f"{case}: expected {expected}, found {actual}")
        parents = run(["git", "show", "-s", "--format=%P", "HEAD"], source, env,
                      evidence / "parents.log").split()
        require(len(parents) == 1, f"{case}: source must not be a merge commit")
        if case == "candidate":
            require(parents == [REVISIONS["parent"]], "Candidate is not the proposed direct child")
        dirty = run(["git", "status", "--porcelain", "--untracked-files=no"], source, env,
                    evidence / "source-before.log")
        require(not dirty.strip(), f"{case}: tracked source is dirty")


def prepare(workspace: Path, root: Path) -> dict[str, str]:
    """Reuse the successful wheel's setup and expose the same tools to nested shells."""
    env = os.environ.copy()
    # Reject inherited alternate builds instead of silently changing their meaning.
    for name in ("RUSTFLAGS", "CARGO_ENCODED_RUSTFLAGS", "SUEWS_BRIDGE_LINT_ONLY",
                 "SUEWS_FC", "SINGLE_PRECISION", "NETCDF4", "PROFILE", "MAKEFLAGS",
                 "PYO3_CONFIG_FILE", "RUSTC_WRAPPER", "RUSTC_WORKSPACE_WRAPPER"):
        require(not env.get(name), f"Uncontrolled build setting: {name}")
    env.update(BUILD_ENV)
    env["CARGO_HOME"] = str(root / "cargo-home")
    env["PYO3_PYTHON"] = sys.executable
    env["PATH"] = os.pathsep.join([
        str(UCRT / "bin"), str(MSYS), sysconfig.get_path("scripts"),
        str(Path(sys.executable).parent), env["PATH"],
    ])
    check_sources(workspace, root, env)
    evidence = root / "evidence" / "setup"
    write_json(evidence / "environment.json", {name: env[name] for name in
               [*BUILD_ENV, "PATH", "CARGO_HOME", "PYO3_PYTHON"]})
    write_json(evidence / "python.json", {
        "executable": sys.executable, "version": sys.version,
        "implementation": platform.python_implementation(),
        "machine": platform.machine(), "pointer_bits": struct.calcsize("P") * 8,
        "platform": sysconfig.get_platform(),
        "runner_image": os.environ.get("ImageVersion"),
    })
    require(platform.python_implementation() == "CPython" and sys.version_info[:2] == (3, 12)
            and struct.calcsize("P") == 8 and sysconfig.get_platform() == "win-amd64",
            "Requires native x64 Windows CPython 3.12")
    bash = str(MSYS / "bash.exe")
    # The first core update may terminate bash. Only that invocation is tolerated;
    # the second full update and every required package installation must succeed.
    run([bash, "-lc", "pacman -Syu --noconfirm --disable-download-timeout"], root, env,
        evidence / "pacman-core.log", check=False)
    run([bash, "-lc", "pacman -Syu --noconfirm --disable-download-timeout"], root, env,
        evidence / "pacman-update.log")
    packages = ["mingw-w64-ucrt-x86_64-" + name for name in
                ("gcc", "gcc-fortran", "binutils", "make", "openblas")]
    # MSYS make is named make.exe (the UCRT package supplies mingw32-make.exe).
    # Pin modern GNU utilities ahead of Windows find.exe / Chocolatey / Git sh.
    packages += ["make", "coreutils", "findutils", "diffutils", "grep", "sed", "gawk"]
    install = "pacman -S --needed --noconfirm --disable-download-timeout " + " ".join(packages)
    run([bash, "-lc", f"for i in 1 2 3; do {install} && exit 0; sleep 15; done; exit 1"],
        root, env, evidence / "pacman-install.log")
    requirements = tomllib.loads((workspace / "candidate/pyproject.toml").read_text(
        encoding="utf-8"))["build-system"]["requires"]
    run([sys.executable, "-m", "pip", "install", *requirements], root, env,
        evidence / "python-build-requirements.log")
    run(["rustup", "target", "add", TARGET], root, env, evidence / "rust-target.log")
    for tool in ("gcc", "g++", "gfortran", "ar", "nm", "make", "sh", "find", "sed"):
        resolved = shutil.which(tool, path=env["PATH"])
        expected_dir = UCRT / "bin" if tool in {"gcc", "g++", "gfortran", "ar", "nm"} else MSYS
        require(resolved is not None and Path(resolved).parent == expected_dir,
                f"Wrong tool resolution for {tool}: {resolved}")
    # No login shell here: it must inherit the exact native PATH used by Ninja,
    # Python, run_make.py, recursive Make, Cargo and the Fortran build script.
    run([str(MSYS / "sh.exe"), "-c",
         'set -eu; for tool in gcc gfortran make sh find sed; do '
         'cygpath -aw "$(command -v "$tool")"; done; gcc --version; make --version'],
        root, env, evidence / "nested-tool-resolution.log")
    for tool in ("gcc", "g++", "gfortran", "ar", "nm", "make", "sh", "find", "sed",
                 "meson", "ninja", "cargo", "rustc", "rustup", "git"):
        run([tool, "--version"], root, env, evidence / f"version-{tool.replace('+', 'p')}.log")
    run(["rustc", "-vV"], root, env, evidence / "rust-host.log")
    run([str(MSYS / "pacman.exe"), "-Q"], root, env, evidence / "packages.log")
    run([sys.executable, "-m", "pip", "freeze"], root, env, evidence / "pip-freeze.log")
    compat = root / "setjmp_compat.c"
    compat.write_text("int _setjmpex(void* buf) { extern int __intrinsic_setjmpex(void*); "
                      "return __intrinsic_setjmpex(buf); }\n", encoding="utf-8")
    run(["gcc", "-c", str(compat), "-o", "setjmp_compat.o"], root, env, evidence / "setjmp-cc.log")
    run(["ar", "rcs", "libsetjmp_compat.a", "setjmp_compat.o"], root, env, evidence / "setjmp-ar.log")
    run(["nm", "libsetjmp_compat.a"], root, env, evidence / "setjmp-nm.log")
    for directory in (UCRT / "lib", UCRT / "x86_64-w64-mingw32/lib"):
        shutil.copy2(root / "libsetjmp_compat.a", directory / "libsetjmp_compat.a")
    write_json(evidence / "setjmp.json", [metadata(directory / "libsetjmp_compat.a") for directory
               in (UCRT / "lib", UCRT / "x86_64-w64-mingw32/lib")])
    return env


def prerequisite_paths(depfile: Path, cwd: Path) -> set[str]:
    """Read Make tokens independently of the product parser under examination."""
    content = depfile.read_text(encoding="utf-8").replace("\\\n", " ")
    result = set()
    for line in content.splitlines():
        if not line.strip() or line.lstrip().startswith("#"):
            continue
        rule = re.split(r":(?:\s|$)", line, maxsplit=1)
        require(len(rule) == 2, f"Malformed rule: {depfile}: {line}")
        for token in re.findall(r"(?:\\[ \t#\\]|[^\s])+", rule[1]):
            if token.startswith("#"):
                break
            token = re.sub(r"\\([ \t#\\])", r"\1", token).replace("$$", "$")
            path = Path(token)
            result.add(ntpath.normcase(str((cwd / path).resolve())))
    return result


def check_dependencies(source: Path, build: Path, cargo: Path, evidence: Path, case: str) -> None:
    """Require the concrete archive in both Cargo dep-info outputs and fix depfile."""
    release = cargo / TARGET / "release"
    libraries = [release / name for name in ("libsuews_bridge.d", "suews_bridge.d")
                 if (release / name).is_file()]
    require(libraries, "No top-level Cargo library dep-info: control is inconclusive")
    files = [*libraries, release / "suews-engine.d", build / "src/supy/rust_bridge_artifacts.d"]
    records = {}
    for path in files:
        require(path.is_file(), f"Missing prerequisite record: {path}")
        records[str(path)] = sorted(prerequisite_paths(path, build))
    write_json(evidence / "prerequisites.json", records)
    for name in ARCHIVES:
        archive = source / "src/suews/lib" / name
        require(archive.is_file(), f"Missing actual physics prerequisite: {archive}")
        normal = ntpath.normcase(str(archive.resolve()))
        for path in files[:-1]:
            require(normal in records[str(path)], f"{archive} absent from Cargo prerequisites: {path}")
        require((normal in records[str(files[-1])]) == (case == "candidate"),
                f"Unexpected {case} outer dependency for {archive}")


def no_work(log: str, before: dict, after: dict) -> None:
    """Require real Ninja no-work, unchanged binaries and an unchanged execution log."""
    require("ninja: no work to do." in log.splitlines(), "Ninja did not report no work")
    require(not re.search(r"^\[\d+/\d+\]", log, re.MULTILINE), "Ninja executed a command")
    require(before == after, "No-work invocation changed measured files")


def require_link(log: str, before: dict, after: dict, archive_ns: int, *, copies: bool) -> None:
    """Require real rustc link commands and newly produced native files, not just a copy."""
    for crate in ("suews_bridge", "suews_engine"):
        require(any("Running " in line and "rustc" in line
                    and f"--crate-name {crate} " in line and "--emit=dep-info,link" in line
                    for line in log.splitlines()), f"No real rustc link invocation for {crate}")
    for name in ("inner_bridge", "inner_engine"):
        require(after[name]["mtime_ns"] > max(before[name]["mtime_ns"], archive_ns),
                f"No fresh native Cargo output: {name}")
    if copies:
        require("build_rust_bridge.py" in log, "Meson bridge wrapper did not execute")
        for kind in ("bridge", "engine"):
            inner, outer = after[f"inner_{kind}"], after[f"outer_{kind}"]
            require(outer["mtime_ns"] > max(before[f"outer_{kind}"]["mtime_ns"], archive_ns),
                    f"No fresh Meson output: {kind}")
            require((outer["sha256"], outer["mtime_ns"]) == (inner["sha256"], inner["mtime_ns"]),
                    f"Meson output is not the fresh Cargo copy: {kind}")
    require(after["archive"] == before["archive"], "Build changed the touched archive")


def touch_archive(paths: dict[str, Path], before: dict, evidence: Path) -> dict:
    """Wait for real wall time, touch once and verify the archive content did not change."""
    threshold = max(before[name]["mtime_ns"] for name in paths if name != "ninja_log")
    started, deadline = time.time_ns(), time.monotonic() + 15
    while time.time_ns() <= threshold + 2_000_000_000:
        require(time.monotonic() < deadline, "Runner clock did not advance beyond output mtimes")
        time.sleep(0.05)
    touch_start = time.time_ns()
    os.utime(paths["archive"], None)  # Never manufacture a future timestamp.
    touch_end = time.time_ns()
    after = snapshot(paths, evidence / "after-touch-state.json")
    write_json(evidence / "touch-clock.json", {
        "wait_start_ns": started, "previous_latest_mtime_ns": threshold,
        "touch_start_ns": touch_start, "touch_end_ns": touch_end,
        "archive_mtime_ns": after["archive"]["mtime_ns"],
    })
    require(threshold < after["archive"]["mtime_ns"] <= touch_end, "Archive timestamp control failed")
    require(after["archive"]["sha256"] == before["archive"]["sha256"], "Touch changed archive bytes")
    require(after["archive"]["size"] == before["archive"]["size"], "Touch changed archive size")
    for name in paths:
        if name != "archive":
            require(after[name] == before[name], f"Touch changed another measured file: {name}")
    return after


def experiment(workspace: Path, root: Path, case: str, common_env: dict[str, str]) -> None:
    """Perform the parent negative control or the candidate relink and no-edit sequence."""
    source, build, cargo, evidence = layout(workspace, root, case)
    env = dict(common_env, CARGO_TARGET_DIR=str(cargo))
    write_json(evidence / "case-environment.json", {"CARGO_TARGET_DIR": str(cargo)})
    build.parent.mkdir(parents=True, exist_ok=True)
    native = evidence / "python-native.ini"
    native.write_text(f"[binaries]\npython = '{Path(sys.executable).as_posix()}'\n", encoding="utf-8")
    run(["meson", "setup", str(build), str(source), "-Dbuildtype=release",
         "-Db_ndebug=if-release", "-Db_vscrt=md", "-Dpython.allow_limited_api=true",
         "--native-file=" + str(native)], source, env, evidence / "meson-setup.log")
    # Target the actual two-output custom target, excluding the unrelated
    # always-stale knowledge-pack target. Keep this command identical every time.
    ninja = ["ninja", "-C", str(build), "-d", "keepdepfile", "-d", "explain", "-v",
             "src/supy/suews_bridge.pyd", "src/supy/suews-engine.exe"]
    run(ninja, build, env, evidence / "01-initial-build.log")
    paths = artefacts(source, build, cargo)
    initial = snapshot(paths, evidence / "01-initial-state.json")
    preserve(source, build, cargo, evidence, "pre")
    require(all(not item.get("missing") for item in initial.values()), "Missing native build control")
    check_dependencies(source, build, cargo, evidence, case)
    log = run(ninja, build, env, evidence / "02-no-edit.log")
    stable = snapshot(paths, evidence / "02-no-edit-state.json")
    no_work(log, initial, stable)
    touched = touch_archive(paths, stable, evidence)
    log = run(ninja, build, env, evidence / "03-touched-outer.log")
    changed = snapshot(paths, evidence / "03-touched-outer-state.json")
    if case == "parent":
        no_work(log, touched, changed)
        # Same cwd, target dir, profile, features, GNU target, linker and env as
        # the unmodified wrapper. -vv changes diagnostics only.
        direct = ["cargo", "build", "--manifest-path", str(source / "src/suews_bridge/Cargo.toml"),
                  "--release", "--features", FEATURES, "--target", TARGET, "-vv"]
        log = run(direct, build, env, evidence / "04-direct-cargo.log")
        rebuilt = snapshot(paths, evidence / "04-direct-cargo-state.json")
        require(any("libsuewsphys.a" in line and ("Dirty" in line or "dirty" in line or "changed" in line)
                    for line in log.splitlines()),
                "Direct Cargo did not attribute the dirty crate to the touched physics archive")
        require_link(log, changed, rebuilt, touched["archive"]["mtime_ns"], copies=False)
        for name in ("outer_bridge", "outer_engine", "ninja_log"):
            require(rebuilt[name] == changed[name], f"Direct Cargo unexpectedly changed {name}")
    else:
        require_link(log, touched, changed, touched["archive"]["mtime_ns"], copies=True)
        for step in (4, 5):
            log = run(ninja, build, env, evidence / f"0{step}-no-edit.log")
            after = snapshot(paths, evidence / f"0{step}-no-edit-state.json")
            no_work(log, changed, after)
            changed = after
    dirty = run(["git", "diff", "--exit-code", "HEAD", "--"], source, env,
                evidence / "source-after.log")
    require(not dirty.strip(), f"{case}: tracked source changed during experiment")


def collect(workspace: Path, root: Path) -> None:
    """Retain a post archive for each case even if setup or a build failed."""
    failures = []
    for case in REVISIONS:
        source, build, cargo, evidence = layout(workspace, root, case)
        try:
            # Missing pre evidence is recorded as missing, never filled with
            # post-build files masquerading as a pre-build observation.
            evidence.mkdir(parents=True, exist_ok=True)
            if not (evidence / "pre-dependencies.zip").exists():
                with zipfile.ZipFile(evidence / "pre-dependencies.zip", "w") as archive:
                    archive.writestr("manifest.json", json.dumps({
                        "status": "not-reached", "reason": "Initial build did not reach the pre-touch snapshot",
                    }, indent=2))
            preserve(source, build, cargo, evidence, "post")
        except Exception:
            failures.append({"case": case, "error": traceback.format_exc()})
    if failures:
        write_json(root / "evidence/collection-errors.json", failures)
        raise RuntimeError("Failed to preserve some dependency evidence")


def main() -> int:
    """Run only inside the authorised Windows hosted job, with fail-closed results."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("mode", choices=("run", "collect"))
    args = parser.parse_args()
    require(os.name == "nt" and os.environ.get("GITHUB_ACTIONS") == "true",
            "This helper runs only in Windows GitHub Actions")
    require(os.environ.get("GITHUB_REF") == "refs/heads/evidence/2147-windows-incremental-20261120"
            and os.environ.get("GITHUB_EVENT_NAME") == "push"
            and os.environ.get("GITHUB_REPOSITORY") == "UMEP-dev/SUEWS",
            "Unexpected repository, event or evidence branch")
    workspace = Path(os.environ["GITHUB_WORKSPACE"]).resolve()
    root = Path(os.environ["RUNNER_TEMP"]) / "windows-2147"
    root.mkdir(parents=True, exist_ok=True)
    if args.mode == "collect":
        collect(workspace, root)
        return 0
    results = {case: {"revision": sha, "status": "not-run"} for case, sha in REVISIONS.items()}
    result_path = root / "evidence/results.json"
    write_json(result_path, results)
    try:
        env = prepare(workspace, root)
        for case in REVISIONS:
            try:
                experiment(workspace, root, case, env)
                results[case]["status"] = "passed"
            except Exception:
                results[case].update(status="failed", error=traceback.format_exc())
                print(results[case]["error"], flush=True)
            finally:
                write_json(result_path, results)
                source, build, cargo, evidence = layout(workspace, root, case)
                preserve(source, build, cargo, evidence, "post")
    except Exception:
        write_json(root / "evidence/setup-or-preservation-error.json", {"error": traceback.format_exc()})
        raise
    finally:
        collect(workspace, root)
    return 0 if all(item["status"] == "passed" for item in results.values()) else 1


if __name__ == "__main__":
    sys.exit(main())
