#!/usr/bin/env python3
"""Audit Python startup hooks for suspicious executable files.

This is aimed at supply-chain incidents that abuse Python startup hooks, such as
the malicious ``litellm_init.pth`` payload reported on 24 March 2026. Python's
``site`` module executes lines that begin with ``import`` inside ``.pth`` files,
which makes unexpected startup hooks a high-signal indicator of compromise.
"""

from __future__ import annotations

import argparse
import json
import site
import sys
from dataclasses import asdict, dataclass
from pathlib import Path


@dataclass(frozen=True)
class AllowedExecutablePthRule:
    """Allow a narrowly-defined executable .pth file."""

    filename: str
    line_prefix: str


@dataclass(frozen=True)
class StartupFinding:
    """A suspicious startup hook discovered during scanning."""

    kind: str
    path: str
    detail: str


DEFAULT_ALLOWED_EXECUTABLE_PTH_RULES = (
    AllowedExecutablePthRule("_virtualenv.pth", "import _virtualenv"),
    AllowedExecutablePthRule(
        "distutils-precedence.pth",
        "import os; var = 'SETUPTOOLS_USE_DISTUTILS'",
    ),
    AllowedExecutablePthRule("a1_coverage.pth", "import sys; exec('import os"),
)

DEFAULT_ALLOWED_EXECUTABLE_PTH_PREFIX_RULES = (
    AllowedExecutablePthRule("__editable__.", "import __editable__"),
)


def build_parser() -> argparse.ArgumentParser:
    """Create the CLI parser."""
    parser = argparse.ArgumentParser(
        description="Audit the active Python environment for suspicious startup hooks."
    )
    parser.add_argument(
        "--site-dir",
        action="append",
        default=[],
        help="Additional site-packages directory to scan. Defaults to the active environment.",
    )
    parser.add_argument(
        "--allow-executable-pth",
        action="append",
        default=[],
        metavar="NAME",
        help="Allow an additional executable .pth filename.",
    )
    parser.add_argument(
        "--allow-customize",
        action="append",
        default=[],
        metavar="NAME",
        help="Allow a sitecustomize.py or usercustomize.py filename.",
    )
    parser.add_argument(
        "--json",
        action="store_true",
        help="Emit machine-readable JSON instead of text output.",
    )
    return parser


def discover_site_dirs(extra_dirs: list[str] | None = None) -> list[Path]:
    """Return unique site-package directories for the active environment."""
    candidates: list[Path] = []

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

    for location in extra_dirs or []:
        candidates.append(Path(location))

    unique: list[Path] = []
    seen: set[Path] = set()
    for candidate in candidates:
        resolved = candidate.expanduser().resolve()
        if resolved.exists() and resolved not in seen:
            unique.append(resolved)
            seen.add(resolved)
    return unique


def is_allowed_executable_pth(
    filename: str,
    stripped_line: str,
    extra_allowed_names: set[str] | None = None,
) -> bool:
    """Return True when an executable .pth file matches a known-safe rule."""
    extra_allowed_names = extra_allowed_names or set()
    if filename in extra_allowed_names:
        return True

    for rule in DEFAULT_ALLOWED_EXECUTABLE_PTH_RULES:
        if filename == rule.filename and stripped_line.startswith(rule.line_prefix):
            return True

    for rule in DEFAULT_ALLOWED_EXECUTABLE_PTH_PREFIX_RULES:
        if filename.startswith(rule.filename) and stripped_line.startswith(rule.line_prefix):
            return True

    return False


def scan_pth_file(
    path: Path,
    *,
    extra_allowed_names: set[str] | None = None,
) -> list[StartupFinding]:
    """Scan a .pth file for unexpected executable lines."""
    findings: list[StartupFinding] = []
    try:
        content = path.read_text(encoding="utf-8")
    except UnicodeDecodeError:
        content = path.read_text(encoding=sys.getfilesystemencoding(), errors="replace")

    for line_number, line in enumerate(content.splitlines(), start=1):
        stripped = line.strip()
        if not stripped or stripped.startswith("#"):
            continue
        if stripped.startswith(("import ", "import\t")) and not is_allowed_executable_pth(
            path.name,
            stripped,
            extra_allowed_names=extra_allowed_names,
        ):
            findings.append(
                StartupFinding(
                    kind="executable_pth",
                    path=str(path),
                    detail=f"line {line_number}: {stripped[:140]}",
                )
            )
    return findings


def scan_site_directory(
    site_dir: Path,
    *,
    extra_allowed_pth_names: set[str] | None = None,
    extra_allowed_customize_names: set[str] | None = None,
) -> list[StartupFinding]:
    """Scan one site-packages directory for suspicious startup hooks."""
    findings: list[StartupFinding] = []
    extra_allowed_customize_names = extra_allowed_customize_names or set()

    for pth_file in sorted(site_dir.glob("*.pth")):
        findings.extend(
            scan_pth_file(
                pth_file,
                extra_allowed_names=extra_allowed_pth_names,
            )
        )

    for name in ("sitecustomize.py", "usercustomize.py"):
        customise_file = site_dir / name
        if customise_file.exists() and name not in extra_allowed_customize_names:
            findings.append(
                StartupFinding(
                    kind="customize_module",
                    path=str(customise_file),
                    detail=f"unexpected {name} present in site-packages",
                )
            )

    return findings


def audit_startup_hooks(
    *,
    extra_site_dirs: list[str] | None = None,
    extra_allowed_pth_names: set[str] | None = None,
    extra_allowed_customize_names: set[str] | None = None,
) -> tuple[list[Path], list[StartupFinding]]:
    """Audit the active environment and return scanned directories and findings."""
    site_dirs = discover_site_dirs(extra_site_dirs)
    findings: list[StartupFinding] = []
    for site_dir in site_dirs:
        findings.extend(
            scan_site_directory(
                site_dir,
                extra_allowed_pth_names=extra_allowed_pth_names,
                extra_allowed_customize_names=extra_allowed_customize_names,
            )
        )
    return site_dirs, findings


def format_text_report(site_dirs: list[Path], findings: list[StartupFinding]) -> str:
    """Render a human-readable report."""
    lines = [
        "Python startup hook audit",
        f"Scanned {len(site_dirs)} site-packages director{'y' if len(site_dirs) == 1 else 'ies'}:",
    ]
    lines.extend(f"  - {path}" for path in site_dirs)

    if not findings:
        lines.append("")
        lines.append("No unexpected executable .pth files or customise modules were found.")
        lines.append("Known-safe hooks allowed by default: _virtualenv, setuptools, coverage, editable installs.")
        return "\n".join(lines)

    lines.append("")
    lines.append("Suspicious startup hooks detected:")
    lines.extend(f"  - [{item.kind}] {item.path} ({item.detail})" for item in findings)
    lines.append("")
    lines.append(
        "If a new dependency was installed recently, inspect the file before running Python again and rotate exposed credentials if compromise is confirmed."
    )
    return "\n".join(lines)


def main() -> int:
    """Run the CLI."""
    parser = build_parser()
    args = parser.parse_args()

    site_dirs, findings = audit_startup_hooks(
        extra_site_dirs=args.site_dir,
        extra_allowed_pth_names=set(args.allow_executable_pth),
        extra_allowed_customize_names=set(args.allow_customize),
    )

    if args.json:
        payload = {
            "site_dirs": [str(path) for path in site_dirs],
            "findings": [asdict(item) for item in findings],
        }
        print(json.dumps(payload, indent=2))
    else:
        print(format_text_report(site_dirs, findings))

    return 1 if findings else 0


if __name__ == "__main__":
    raise SystemExit(main())
