#!/usr/bin/env python3
"""Utility script invoked from Meson to build SUEWS via make."""

from __future__ import annotations

import subprocess
import sys
from pathlib import Path


def main() -> None:
    if len(sys.argv) != 3:
        raise SystemExit(
            "Usage: run_make.py <suews_dir> <stamp_file>"
        )

    suews_dir = Path(sys.argv[1]).resolve()
    stamp_file = Path(sys.argv[2])

    # Propagate the exit code from make so Meson can fail fast.
    subprocess.run(['make', '-C', str(suews_dir)], check=True)

    # Ensure an output file exists so Meson can track this custom target.
    stamp_file.parent.mkdir(parents=True, exist_ok=True)
    stamp_file.touch()


if __name__ == '__main__':
    main()
