"""Build helper for embedding the Rust bridge into the Meson package build."""

from __future__ import annotations

import os
import shutil
import stat
import subprocess
import sys
from pathlib import Path


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
        candidates = [target_dir / "suews_bridge.dll", target_dir / "libsuews_bridge.dll"]
    else:
        candidates = [target_dir / "libsuews_bridge.so"]

    for candidate in candidates:
        if candidate.exists():
            return candidate

    joined = ", ".join(str(p.name) for p in candidates)
    raise FileNotFoundError(f"Rust bridge extension artifact not found in {target_dir} (tried: {joined})")


def _find_cli_artifact(target_dir: Path) -> Path:
    binary = target_dir / ("suews.exe" if os.name == "nt" else "suews")
    if not binary.exists():
        raise FileNotFoundError(f"Rust bridge CLI artifact not found: {binary}")
    return binary


def main() -> int:
    if len(sys.argv) != 4:
        print("usage: build_rust_bridge.py <repo_root> <output_ext> <output_cli>", file=sys.stderr)
        return 2

    repo_root = Path(sys.argv[1]).resolve()
    output_ext = Path(sys.argv[2]).resolve()
    output_cli = Path(sys.argv[3]).resolve()

    manifest = repo_root / "src" / "suews_bridge" / "Cargo.toml"
    target_dir = repo_root / "src" / "suews_bridge" / "target" / "release"

    cmd = [
        "cargo",
        "build",
        "--manifest-path",
        str(manifest),
        "--release",
        "--features",
        "python-extension,physics",
    ]
    subprocess.run(cmd, check=True)

    ext_artifact = _find_extension_artifact(target_dir)
    cli_artifact = _find_cli_artifact(target_dir)

    _copy_file(ext_artifact, output_ext, make_executable=False)
    _copy_file(cli_artifact, output_cli, make_executable=True)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
