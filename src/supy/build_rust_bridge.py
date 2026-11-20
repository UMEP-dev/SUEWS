"""Build helper for embedding the Rust bridge into the Meson package build.

Invoked by ``src/supy/meson.build`` after the Fortran kernel static libraries
are built. Compiles ``src/suews_bridge/Cargo.toml`` with
``python-extension,physics,arrow-output`` (using the MinGW GNU target on
Windows so gfortran symbols link cleanly), copies both the PyO3 extension
library (``suews_bridge``) and standalone ``suews-engine`` CLI into the Meson
staging outputs, and emits a Ninja depfile tracking Rust, C-API Fortran and
driver inputs so incremental rebuilds stay accurate.
"""

from __future__ import annotations

import importlib.util
import os
from pathlib import Path
import re
import shutil
import stat
import subprocess
import sys

# Load by sibling path: Meson executes these scripts directly, and tests load
# them with spec_from_file_location without adding this directory to sys.path.
_DEPFILE_HELPER = Path(__file__).resolve().with_name("_build_depfile.py")
_depfile_spec = importlib.util.spec_from_file_location(
    "_suews_build_depfile", _DEPFILE_HELPER
)
assert _depfile_spec is not None and _depfile_spec.loader is not None
_depfile_module = importlib.util.module_from_spec(_depfile_spec)
_depfile_spec.loader.exec_module(_depfile_module)
write_depfile = _depfile_module.write_depfile
_escape_depfile_token = _depfile_module._escape_depfile_token


def _copy_file(src: Path, dst: Path, make_executable: bool = False) -> None:
    dst.parent.mkdir(parents=True, exist_ok=True)
    shutil.copy2(src, dst)
    if make_executable and os.name != "nt":
        mode = dst.stat().st_mode
        dst.chmod(mode | stat.S_IXUSR | stat.S_IXGRP | stat.S_IXOTH)


def _find_extension_artifact(target_dir: Path) -> Path:
    if sys.platform == "darwin":
        candidates = [target_dir / "libsuews_bridge.dylib"]
    elif os.name == "nt":
        candidates = [
            target_dir / "suews_bridge.dll",
            target_dir / "libsuews_bridge.dll",
        ]
    else:
        candidates = [target_dir / "libsuews_bridge.so"]

    for candidate in candidates:
        if candidate.exists():
            return candidate

    joined = ", ".join(str(p.name) for p in candidates)
    raise FileNotFoundError(
        f"Rust bridge extension artifact not found in {target_dir} (tried: {joined})"
    )


def _find_cli_artifact(target_dir: Path) -> Path:
    binary = target_dir / ("suews-engine.exe" if os.name == "nt" else "suews-engine")
    if not binary.exists():
        raise FileNotFoundError(f"Rust bridge CLI artifact not found: {binary}")
    return binary


_C_API_SOURCE_SUFFIXES = {".f90", ".f95", ".f", ".c", ".h"}


def read_cargo_depfile(depfile: Path) -> list[str]:
    """Read rule prerequisites, not targets or rustc's env-dep comments.

    Do not use shell tokenisation: backslashes in Windows paths are literal
    unless they escape Make whitespace, a hash, or another backslash.
    A drive-letter colon is not the rule separator (colon plus whitespace).
    """
    text = depfile.read_text(encoding="utf-8")
    text = text.replace("\\\n", " ")
    deps: list[str] = []
    for line in text.splitlines():
        if not line.strip() or line.lstrip().startswith("#"):
            continue
        rule = re.search(r":(?:\s|$)", line)
        if rule is None:
            raise ValueError(f"Invalid Cargo dep-info rule in {depfile}: {line!r}")
        value = line[rule.end() :]
        token: list[str] = []
        i = 0
        while i < len(value):
            char = value[i]
            if char == "\\" and i + 1 < len(value) and value[i + 1] in " \t#\\":
                i += 1
                token.append(value[i])
            elif char == "$" and value[i : i + 2] == "$$":
                token.append("$")
                i += 1
            elif char == "#":
                break
            elif char.isspace():
                if token:
                    deps.append("".join(token))
                    token = []
            else:
                token.append(char)
            i += 1
        if token:
            deps.append("".join(token))
    return deps


def collect_cargo_depfile_inputs(target_dir: Path, base_target: Path) -> list[Path]:
    """Collect both built artefacts' inputs, excluding Cargo-owned outputs.

    Cargo's top-level dep-info includes transitive and build-script inputs.
    Generated include! files under the target directory must remain Cargo's
    responsibility: exposing them to Ninja can make the wrapper rebuild itself.
    Relative dep-info paths are interpreted from the Cargo invocation directory.
    """
    generated_root = base_target.resolve()
    deps: set[Path] = set()
    # rlib and cdylib naming differs across platforms; consume only these
    # artefacts' top-level files, never stale hashed files under deps/.
    library_depfiles = [
        path
        for name in ("libsuews_bridge.d", "suews_bridge.d")
        if (path := target_dir / name).is_file()
    ]
    if not library_depfiles:
        raise FileNotFoundError(f"Cargo library dep-info not found in {target_dir}")
    for depfile in [*library_depfiles, target_dir / "suews-engine.d"]:
        for token in read_cargo_depfile(depfile):
            path = Path(token).resolve()
            if not path.is_relative_to(generated_root):
                deps.add(path)
    return sorted(deps)


def collect_depfile_inputs(repo_root: Path) -> list[Path]:
    """Return sorted Rust, C-API Fortran, and build inputs for the bridge target."""
    bridge_dir = repo_root / "src" / "suews_bridge"
    deps: list[Path] = [_DEPFILE_HELPER]
    for fixed in (
        bridge_dir / "Cargo.toml",
        bridge_dir / "Cargo.lock",
        bridge_dir / "build.rs",
        repo_root / "src" / "supy" / "build_rust_bridge.py",
        repo_root / "src" / "suews" / "src" / "suews_ctrl_driver.f95",
    ):
        if fixed.is_file():
            deps.append(fixed.resolve())

    src_dir = bridge_dir / "src"
    if src_dir.is_dir():
        for path in sorted(src_dir.rglob("*.rs")):
            if path.is_file():
                deps.append(path.resolve())

    c_api_dir = bridge_dir / "c_api"
    if c_api_dir.is_dir():
        for path in sorted(c_api_dir.rglob("*")):
            if path.is_file() and path.suffix.lower() in _C_API_SOURCE_SUFFIXES:
                deps.append(path.resolve())

    # Deduplicate while preserving deterministic sorted order.
    return sorted(set(deps))


def main() -> int:
    if len(sys.argv) not in {4, 5}:
        print(
            "usage: build_rust_bridge.py <repo_root> <output_ext> <output_cli> [depfile]",
            file=sys.stderr,
        )
        return 2

    repo_root = Path(sys.argv[1]).resolve()
    raw_output_ext = sys.argv[2]
    output_ext = Path(raw_output_ext).resolve()
    output_cli = Path(sys.argv[3]).resolve()
    depfile = Path(sys.argv[4]) if len(sys.argv) == 5 else None

    manifest = repo_root / "src" / "suews_bridge" / "Cargo.toml"

    cmd = [
        "cargo",
        "build",
        "--manifest-path",
        str(manifest),
        "--release",
        "--features",
        "python-extension,physics,arrow-output",
    ]

    env = os.environ.copy()

    # CARGO_TARGET_DIR overrides where cargo writes compiled artifacts.
    # Set in CI to a bind-mounted volume so artifacts survive Docker teardown.
    cargo_target_dir = os.environ.get("CARGO_TARGET_DIR")
    default_target = repo_root / "src" / "suews_bridge" / "target"
    base_target = Path(cargo_target_dir) if cargo_target_dir else default_target

    if os.name == "nt":
        # Windows: Fortran is compiled with MinGW gfortran, so we must use
        # the GNU Rust target to get a compatible linker (gcc instead of
        # MSVC link.exe which cannot resolve MinGW symbols).
        win_target = "x86_64-pc-windows-gnu"
        subprocess.run(["rustup", "target", "add", win_target], check=True)
        cmd.extend(["--target", win_target])
        env["CARGO_TARGET_X86_64_PC_WINDOWS_GNU_LINKER"] = "gcc"
        target_dir = base_target / win_target / "release"
    else:
        target_dir = base_target / "release"

    subprocess.run(cmd, check=True, env=env)

    ext_artifact = _find_extension_artifact(target_dir)
    cli_artifact = _find_cli_artifact(target_dir)

    if depfile is not None:
        deps = collect_depfile_inputs(repo_root)
        deps.extend(collect_cargo_depfile_inputs(target_dir, base_target))
    _copy_file(ext_artifact, output_ext, make_executable=False)
    _copy_file(cli_artifact, output_cli, make_executable=True)
    if depfile is not None:
        write_depfile(depfile, raw_output_ext, sorted(set(deps)))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
