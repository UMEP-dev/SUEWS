#!/usr/bin/env python3
"""
Fortran Naming Convention Checker for SUEWS

Checks Fortran source files against the naming conventions defined in
dev-ref/FORTRAN_NAMING_CONVENTIONS.md

Usage:
    python scripts/check_naming_conventions.py [files...]
    python scripts/check_naming_conventions.py --report report.txt
    python scripts/check_naming_conventions.py --strict  # Exit with error on violations
"""

import argparse
import re
import sys
from pathlib import Path
from typing import List, Dict, Tuple
from dataclasses import dataclass, field


@dataclass
class NamingViolation:
    """Record of a naming convention violation."""

    file: Path
    line_num: int
    severity: str  # 'error', 'warning', 'info'
    category: str  # 'file', 'module', 'subroutine', etc.
    message: str
    suggestion: str = ""


@dataclass
class CheckResult:
    """Results from checking a file."""

    file: Path
    errors: List[NamingViolation] = field(default_factory=list)
    warnings: List[NamingViolation] = field(default_factory=list)
    info: List[NamingViolation] = field(default_factory=list)

    @property
    def has_errors(self) -> bool:
        return len(self.errors) > 0

    @property
    def has_warnings(self) -> bool:
        return len(self.warnings) > 0

    @property
    def total_issues(self) -> int:
        return len(self.errors) + len(self.warnings) + len(self.info)


class FortranNamingChecker:
    """Check Fortran files against SUEWS naming conventions."""

    # File naming pattern: suews_<category>_<name>.f95
    FILE_PATTERN = re.compile(r"^suews_(ctrl|phys|util)_[a-z][a-z0-9_]*\.f95$")
    CATEGORIES = {"ctrl", "phys", "util"}

    # Module naming: MODULE suews_<category>_<name>
    MODULE_PATTERN = re.compile(r"^\s*MODULE\s+([a-zA-Z][a-zA-Z0-9_]*)", re.IGNORECASE)

    # Subroutine/Function patterns
    SUBROUTINE_PATTERN = re.compile(
        r"^\s*SUBROUTINE\s+([a-zA-Z][a-zA-Z0-9_]*)", re.IGNORECASE
    )
    FUNCTION_PATTERN = re.compile(
        r"^\s*FUNCTION\s+([a-zA-Z][a-zA-Z0-9_]*)", re.IGNORECASE
    )

    # PUBLIC/PRIVATE declarations
    PUBLIC_DECL = re.compile(r"^\s*PUBLIC\s*::", re.IGNORECASE)
    PRIVATE_DECL = re.compile(r"^\s*PRIVATE\s*$", re.IGNORECASE)

    # Allowed suffixes for multiple modules per file
    ALLOWED_SUFFIXES = {"const", "types", "ops", "io", "util"}

    def __init__(self, strict: bool = False):
        """
        Initialize checker.

        Parameters
        ----------
        strict : bool
            If True, treat warnings as errors
        """
        self.strict = strict

    def check_file(self, filepath: Path) -> CheckResult:
        """
        Check a single Fortran file.

        Parameters
        ----------
        filepath : Path
            Path to Fortran source file

        Returns
        -------
        CheckResult
            Results containing any violations found
        """
        result = CheckResult(file=filepath)

        # Check file naming
        self._check_filename(filepath, result)

        # Read file content
        try:
            with open(filepath, "r", encoding="utf-8", errors="ignore") as f:
                lines = f.readlines()
        except Exception as e:
            result.errors.append(
                NamingViolation(
                    file=filepath,
                    line_num=0,
                    severity="error",
                    category="file",
                    message=f"Failed to read file: {e}",
                )
            )
            return result

        # Extract expected module name from filename
        expected_module = self._get_expected_module_name(filepath)

        # Check module declarations
        self._check_modules(filepath, lines, expected_module, result)

        # Check subroutines and functions (informational only)
        self._check_subroutines_functions(filepath, lines, result)

        return result

    def _check_filename(self, filepath: Path, result: CheckResult):
        """Check if filename follows convention."""
        filename = filepath.name

        if not filename.endswith(".f95"):
            result.warnings.append(
                NamingViolation(
                    file=filepath,
                    line_num=0,
                    severity="warning",
                    category="file",
                    message=f"File uses .{filename.split('.')[-1]} extension instead of .f95",
                    suggestion="Prefer .f95 extension for consistency",
                )
            )
            return

        if not self.FILE_PATTERN.match(filename):
            # Check if it's close to the pattern
            if filename.startswith("suews_"):
                parts = filename[6:].replace(".f95", "").split("_", 1)
                if len(parts) >= 1 and parts[0] not in self.CATEGORIES:
                    result.errors.append(
                        NamingViolation(
                            file=filepath,
                            line_num=0,
                            severity="error",
                            category="file",
                            message=f"File category '{parts[0]}' not recognized",
                            suggestion=f"Use one of: {', '.join(sorted(self.CATEGORIES))}",
                        )
                    )
                elif not re.match(
                    r"^[a-z][a-z0-9_]*$", parts[1] if len(parts) > 1 else ""
                ):
                    result.errors.append(
                        NamingViolation(
                            file=filepath,
                            line_num=0,
                            severity="error",
                            category="file",
                            message="File name component must be lowercase with underscores",
                            suggestion=f"Example: suews_phys_snow.f95",
                        )
                    )
            else:
                result.errors.append(
                    NamingViolation(
                        file=filepath,
                        line_num=0,
                        severity="error",
                        category="file",
                        message=f"File name does not follow pattern: suews_<category>_<name>.f95",
                        suggestion=f"Expected pattern like: suews_phys_snow.f95",
                    )
                )

    def _get_expected_module_name(self, filepath: Path) -> str:
        """Extract expected module name from filename."""
        filename = filepath.stem  # Remove .f95 extension
        if filename.startswith("suews_"):
            return filename
        return ""

    def _check_modules(
        self,
        filepath: Path,
        lines: List[str],
        expected_module: str,
        result: CheckResult,
    ):
        """Check module declarations."""
        modules_found = []

        for line_num, line in enumerate(lines, start=1):
            # Skip comments
            if line.strip().startswith("!"):
                continue

            match = self.MODULE_PATTERN.match(line)
            if match:
                module_name = match.group(1)

                # Skip 'module procedure' declarations
                if "PROCEDURE" in line.upper():
                    continue

                modules_found.append((line_num, module_name))

                # Check if module name matches expected
                if expected_module:
                    if module_name.lower() == expected_module.lower():
                        # Perfect match
                        result.info.append(
                            NamingViolation(
                                file=filepath,
                                line_num=line_num,
                                severity="info",
                                category="module",
                                message=f"✓ Module '{module_name}' matches filename",
                            )
                        )
                    else:
                        # Check if it's a valid multi-module suffix
                        if module_name.lower().startswith(
                            expected_module.lower() + "_"
                        ):
                            suffix = module_name.lower()[len(expected_module) + 1 :]
                            if suffix in self.ALLOWED_SUFFIXES:
                                result.info.append(
                                    NamingViolation(
                                        file=filepath,
                                        line_num=line_num,
                                        severity="info",
                                        category="module",
                                        message=f"✓ Module '{module_name}' uses allowed suffix '_{suffix}'",
                                    )
                                )
                            else:
                                result.warnings.append(
                                    NamingViolation(
                                        file=filepath,
                                        line_num=line_num,
                                        severity="warning",
                                        category="module",
                                        message=f"Module '{module_name}' uses non-standard suffix",
                                        suggestion=f"Prefer: {expected_module}_{' | '.join(sorted(self.ALLOWED_SUFFIXES))}",
                                    )
                                )
                        else:
                            result.errors.append(
                                NamingViolation(
                                    file=filepath,
                                    line_num=line_num,
                                    severity="error",
                                    category="module",
                                    message=f"Module '{module_name}' does not match filename",
                                    suggestion=f"Expected: MODULE {expected_module} (or {expected_module}_<suffix>)",
                                )
                            )

        # Check module count
        if len(modules_found) == 0:
            result.warnings.append(
                NamingViolation(
                    file=filepath,
                    line_num=0,
                    severity="warning",
                    category="module",
                    message="No MODULE declarations found in file",
                )
            )
        elif len(modules_found) > 1:
            result.info.append(
                NamingViolation(
                    file=filepath,
                    line_num=0,
                    severity="info",
                    category="module",
                    message=f"File contains {len(modules_found)} modules (prefer one per file)",
                    suggestion="Consider splitting into separate files if modules are independent",
                )
            )

    def _check_subroutines_functions(
        self, filepath: Path, lines: List[str], result: CheckResult
    ):
        """Check subroutine and function naming (informational)."""
        in_private_section = False

        for line_num, line in enumerate(lines, start=1):
            # Skip comments
            if line.strip().startswith("!"):
                continue

            # Track PUBLIC/PRIVATE sections
            if self.PRIVATE_DECL.match(line):
                in_private_section = True
                continue
            elif self.PUBLIC_DECL.match(line):
                in_private_section = False
                continue

            # Check subroutines
            match = self.SUBROUTINE_PATTERN.match(line)
            if match:
                name = match.group(1)
                self._check_routine_name(
                    filepath, line_num, "subroutine", name, in_private_section, result
                )

            # Check functions
            match = self.FUNCTION_PATTERN.match(line)
            if match:
                name = match.group(1)
                self._check_routine_name(
                    filepath, line_num, "function", name, in_private_section, result
                )

    def _check_routine_name(
        self,
        filepath: Path,
        line_num: int,
        routine_type: str,
        name: str,
        is_private: bool,
        result: CheckResult,
    ):
        """Check if routine name follows conventions (informational only)."""
        # Expected patterns
        is_snake_case = bool(re.match(r"^[a-z][a-z0-9_]*$", name))
        is_upper_case = bool(re.match(r"^[A-Z][A-Z0-9_]*$", name))
        is_pascal_case = bool(re.match(r"^[A-Z][a-zA-Z0-9]*$", name))

        # New standard: Everything should be snake_case (or UPPERCASE for acronyms)
        if is_snake_case or is_upper_case:
            # Correct! Following the standard
            result.info.append(
                NamingViolation(
                    file=filepath,
                    line_num=line_num,
                    severity="info",
                    category=routine_type,
                    message=f"✓ {routine_type.capitalize()} '{name}' follows snake_case convention",
                )
            )
        elif is_pascal_case:
            # Legacy PascalCase - informational only (not an error)
            result.info.append(
                NamingViolation(
                    file=filepath,
                    line_num=line_num,
                    severity="info",
                    category=routine_type,
                    message=f"{routine_type.capitalize()} '{name}' uses PascalCase (legacy style)",
                    suggestion=f"New standard prefers snake_case: {self._to_snake_case(name)}",
                )
            )
        else:
            # Mixed or unusual naming
            result.warnings.append(
                NamingViolation(
                    file=filepath,
                    line_num=line_num,
                    severity="warning",
                    category=routine_type,
                    message=f"{routine_type.capitalize()} '{name}' uses non-standard naming",
                    suggestion=f"Prefer snake_case: {self._to_snake_case(name)}",
                )
            )

    @staticmethod
    def _to_pascal_case(name: str) -> str:
        """Convert name to PascalCase suggestion."""
        parts = re.split(r"[_\s]+", name.lower())
        return "".join(word.capitalize() for word in parts)

    @staticmethod
    def _to_snake_case(name: str) -> str:
        """Convert name to snake_case suggestion."""
        # Insert underscores before capitals
        s1 = re.sub("(.)([A-Z][a-z]+)", r"\1_\2", name)
        return re.sub("([a-z0-9])([A-Z])", r"\1_\2", s1).lower()


def print_results(results: List[CheckResult], show_info: bool = False):
    """Print check results to console."""
    total_files = len(results)
    total_errors = sum(len(r.errors) for r in results)
    total_warnings = sum(len(r.warnings) for r in results)
    total_info = sum(len(r.info) for r in results)

    for result in results:
        if result.total_issues == 0 and not show_info:
            continue

        print(f"\n{'=' * 80}")
        print(f"File: {result.file}")
        print(f"{'=' * 80}")

        # Print errors
        for violation in result.errors:
            print(f"  ❌ ERROR [{violation.category}] Line {violation.line_num}")
            print(f"     {violation.message}")
            if violation.suggestion:
                print(f"     💡 {violation.suggestion}")

        # Print warnings
        for violation in result.warnings:
            print(f"  ⚠️  WARNING [{violation.category}] Line {violation.line_num}")
            print(f"     {violation.message}")
            if violation.suggestion:
                print(f"     💡 {violation.suggestion}")

        # Print info (if requested)
        if show_info:
            for violation in result.info:
                print(f"  ℹ️  INFO [{violation.category}] Line {violation.line_num}")
                print(f"     {violation.message}")
                if violation.suggestion:
                    print(f"     💡 {violation.suggestion}")

    # Summary
    print(f"\n{'=' * 80}")
    print(f"SUMMARY")
    print(f"{'=' * 80}")
    print(f"Files checked: {total_files}")
    print(f"Errors:        {total_errors}")
    print(f"Warnings:      {total_warnings}")
    if show_info:
        print(f"Info:          {total_info}")

    if total_errors == 0 and total_warnings == 0:
        print("\n✅ All checks passed!")
    elif total_errors == 0:
        print(f"\n⚠️  {total_warnings} warnings found")
    else:
        print(f"\n❌ {total_errors} errors found")


def main():
    parser = argparse.ArgumentParser(
        description="Check Fortran files against SUEWS naming conventions"
    )
    parser.add_argument(
        "files",
        nargs="*",
        help="Fortran files to check (default: all .f95 in src/suews/src/)",
    )
    parser.add_argument(
        "--strict", action="store_true", help="Treat warnings as errors"
    )
    parser.add_argument(
        "--show-info", action="store_true", help="Show informational messages"
    )
    parser.add_argument(
        "--report", type=str, help="Write report to file instead of console"
    )

    args = parser.parse_args()

    # Determine files to check
    if args.files:
        files_to_check = [Path(f) for f in args.files if f.endswith((".f95", ".f90"))]
    else:
        # Default: check all .f95 files in src/suews/src/
        src_dir = Path(__file__).parent.parent / "src" / "suews" / "src"
        if not src_dir.exists():
            print(f"Error: Source directory not found: {src_dir}", file=sys.stderr)
            sys.exit(1)
        files_to_check = sorted(src_dir.glob("*.f95"))

    if not files_to_check:
        print("No Fortran files to check", file=sys.stderr)
        sys.exit(1)

    # Run checker
    checker = FortranNamingChecker(strict=args.strict)
    results = [checker.check_file(f) for f in files_to_check]

    # Print or write report
    if args.report:
        import io

        output = io.StringIO()
        sys.stdout = output
        print_results(results, show_info=args.show_info)
        sys.stdout = sys.__stdout__

        with open(args.report, "w") as f:
            f.write(output.getvalue())
        print(f"Report written to: {args.report}")
    else:
        print_results(results, show_info=args.show_info)

    # Exit code
    total_errors = sum(len(r.errors) for r in results)
    total_warnings = sum(len(r.warnings) for r in results)

    if total_errors > 0:
        sys.exit(1)
    elif args.strict and total_warnings > 0:
        sys.exit(1)
    else:
        sys.exit(0)


if __name__ == "__main__":
    main()
