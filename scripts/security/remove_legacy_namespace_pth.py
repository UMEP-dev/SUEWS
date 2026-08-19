#!/usr/bin/env python3
"""Remove legacy setuptools namespace-package .pth bootstrap files.

These ``*-nspkg.pth`` files are a packaging-era workaround that executes code
at Python startup. Modern namespace packages import correctly without them in
the Sphinx docs stack used by this project, so we strip them after install to
reduce auto-execution surface.
"""

from __future__ import annotations

import argparse
import json
import site
from pathlib import Path


def build_parser() -> argparse.ArgumentParser:
    """Create the CLI parser."""
    parser = argparse.ArgumentParser(
        description="Remove legacy namespace-package .pth files from site-packages."
    )
    parser.add_argument(
        "--site-dir",
        action="append",
        default=[],
        help="Specific site-packages directory to scan. Defaults to the active environment when omitted.",
    )
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Report files that would be removed without deleting them.",
    )
    parser.add_argument(
        "--json",
        action="store_true",
        help="Emit machine-readable JSON instead of text output.",
    )
    return parser


def discover_site_dirs(extra_dirs: list[str] | None = None) -> list[Path]:
    """Return unique site-package directories to scan.

    When explicit directories are provided, scan only those paths. Otherwise,
    fall back to the active environment's site-package directories.
    """
    candidates: list[Path] = []

    if extra_dirs:
        for location in extra_dirs:
            candidates.append(Path(location))
    else:
        getsitepackages = getattr(site, "getsitepackages", None)
        if callable(getsitepackages):
            for location in getsitepackages():
                candidates.append(Path(location))

        if getattr(site, "ENABLE_USER_SITE", False):
            try:
                user_site = site.getusersitepackages()
            except Exception:
                user_site = None
            if user_site:
                candidates.append(Path(user_site))

    unique: list[Path] = []
    seen: set[Path] = set()
    for candidate in candidates:
        resolved = candidate.expanduser().resolve()
        if resolved.exists() and resolved not in seen:
            unique.append(resolved)
            seen.add(resolved)
    return unique


def remove_legacy_namespace_pth(*, extra_site_dirs: list[str] | None = None, dry_run: bool = False) -> tuple[list[Path], list[Path]]:
    """Remove legacy namespace-package bootstrap files from site-packages."""
    site_dirs = discover_site_dirs(extra_site_dirs)
    removed: list[Path] = []

    for site_dir in site_dirs:
        for path in sorted(site_dir.glob("*-nspkg.pth")):
            removed.append(path)
            if not dry_run:
                path.unlink()

    return site_dirs, removed


def main() -> int:
    """Run the CLI."""
    args = build_parser().parse_args()
    site_dirs, removed = remove_legacy_namespace_pth(
        extra_site_dirs=args.site_dir,
        dry_run=args.dry_run,
    )

    if args.json:
        print(
            json.dumps(
                {
                    "site_dirs": [str(path) for path in site_dirs],
                    "removed": [str(path) for path in removed],
                    "dry_run": args.dry_run,
                },
                indent=2,
            )
        )
        return 0

    action = "Would remove" if args.dry_run else "Removed"
    if removed:
        print(f"{action} {len(removed)} legacy namespace-package .pth file(s):")
        for path in removed:
            print(f"  - {path}")
    else:
        print("No legacy namespace-package .pth files found.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
