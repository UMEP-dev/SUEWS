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

import os
from pathlib import Path
import shutil
import stat
import subprocess
import sys


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


def _escape_depfile_token(token: str) -> str:
    """Escape a path for Ninja/Make depfile consumption."""
    return token.replace("$", "$$").replace(" ", "\\ ")


def collect_depfile_inputs(repo_root: Path) -> list[Path]:
    """Return sorted Rust, C-API Fortran, and build inputs for the bridge target."""
    bridge_dir = repo_root / "src" / "suews_bridge"
    deps: list[Path] = []
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


def write_depfile(depfile: Path, target: str, deps: list[Path]) -> None:
    """Write a single-target Ninja-compatible depfile."""
    depfile.parent.mkdir(parents=True, exist_ok=True)
    target_tok = _escape_depfile_token(target.replace("\\", "/"))
    dep_toks = [_escape_depfile_token(p.as_posix()) for p in deps]
    content = (
        f"{target_tok}: " + " \\\n  ".join(dep_toks) + "\n"
        if dep_toks
        else f"{target_tok}:\n"
    )
    depfile.write_text(content, encoding="utf-8")


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

    _copy_file(ext_artifact, output_ext, make_executable=False)
    _copy_file(cli_artifact, output_cli, make_executable=True)
    if depfile is not None:
        write_depfile(depfile, raw_output_ext, collect_depfile_inputs(repo_root))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
