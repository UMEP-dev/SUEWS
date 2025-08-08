#!/usr/bin/env python3
"""
Validation script for CLAUDE.md integrity.
Checks for placeholder text and ensures critical content is preserved.
"""

import sys
from pathlib import Path
import hashlib
import json
from datetime import datetime

# Critical sections that must be preserved
CRITICAL_SECTIONS = [
    "## Current Investigations and Findings",
    "### QE/QH Discrepancy Investigation",
    "## Configuration Handling and Method Design Pattern",
    "## Documentation & Code Maintenance Principles",
    "## Git Worktrees for Claude Code",
    "## Testing Resources",
    "### Critical Testing Requirements for SUEWS",
    "## Development Tasks and Reminders",
]

# Suspicious placeholder patterns that indicate content loss
PLACEHOLDER_PATTERNS = [
    "[... rest of existing content",
    "[... remaining content",
    "[... content continues",
    "[... unchanged",
    "# rest of",
    "# remaining",
    "[...]",
    "// ...",
    "/* ... */",
    "... (content omitted)",
    "... (rest unchanged)",
    "... rest of",
    "...remaining",
    "[content removed]",
    "[content omitted]",
]


def check_file_integrity(filepath: Path) -> dict:
    """Check CLAUDE.md for integrity issues."""

    if not filepath.exists():
        return {
            "valid": False,
            "error": f"File not found: {filepath}",
            "warnings": [],
            "stats": {},
        }

    content = filepath.read_text()
    lines = content.splitlines()

    warnings = []

    # Check for placeholder patterns
    for i, line in enumerate(lines, 1):
        for pattern in PLACEHOLDER_PATTERNS:
            if pattern.lower() in line.lower():
                warnings.append(
                    f"Line {i}: Suspicious placeholder text found: '{pattern}'"
                )

    # Check for critical sections
    missing_sections = []
    for section in CRITICAL_SECTIONS:
        if section not in content:
            missing_sections.append(section)

    if missing_sections:
        warnings.append(f"Missing critical sections: {', '.join(missing_sections)}")

    # Calculate content hash for tracking changes
    content_hash = hashlib.sha256(content.encode()).hexdigest()

    # Gather statistics
    stats = {
        "lines": len(lines),
        "characters": len(content),
        "sections": content.count("##"),
        "code_blocks": content.count("```"),
        "hash": content_hash[:16],  # Short hash for display
        "timestamp": datetime.now().isoformat(),
    }

    # Check minimum content thresholds
    MIN_LINES = 500  # CLAUDE.md should have at least this many lines
    MIN_CHARS = 20000  # And this many characters

    if stats["lines"] < MIN_LINES:
        warnings.append(
            f"File appears truncated: only {stats['lines']} lines (expected >= {MIN_LINES})"
        )

    if stats["characters"] < MIN_CHARS:
        warnings.append(
            f"File appears truncated: only {stats['characters']} characters (expected >= {MIN_CHARS})"
        )

    return {"valid": len(warnings) == 0, "warnings": warnings, "stats": stats}


def save_snapshot(filepath: Path, snapshot_dir: Path):
    """Save a timestamped snapshot of the file."""

    snapshot_dir.mkdir(parents=True, exist_ok=True)

    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    snapshot_path = snapshot_dir / f"CLAUDE.md.{timestamp}"

    content = filepath.read_text()
    snapshot_path.write_text(content)

    # Also save metadata
    metadata = {
        "timestamp": datetime.now().isoformat(),
        "lines": len(content.splitlines()),
        "characters": len(content),
        "hash": hashlib.sha256(content.encode()).hexdigest(),
    }

    metadata_path = snapshot_dir / f"CLAUDE.md.{timestamp}.json"
    metadata_path.write_text(json.dumps(metadata, indent=2))

    return snapshot_path


def main():
    """Main validation routine."""

    # Paths
    claude_md = Path("CLAUDE.md")
    backup_path = Path("CLAUDE.md.backup")
    snapshot_dir = Path(".claude/snapshots")

    print("=" * 60)
    print("CLAUDE.md Integrity Validator")
    print("=" * 60)

    # Check main file
    print("\nChecking CLAUDE.md...")
    result = check_file_integrity(claude_md)

    print(f"\nFile Statistics:")
    for key, value in result["stats"].items():
        print(f"  {key}: {value}")

    if result["warnings"]:
        print("\n⚠️  WARNINGS DETECTED:")
        for warning in result["warnings"]:
            print(f"  - {warning}")

        # If warnings detected, check backup
        if backup_path.exists():
            print("\n📋 Checking backup file...")
            backup_result = check_file_integrity(backup_path)

            if not backup_result["warnings"]:
                print("  ✓ Backup appears intact")
                print("\n  To restore from backup, run:")
                print("    cp CLAUDE.md.backup CLAUDE.md")
            else:
                print("  ⚠️  Backup also has issues")

        # Save a snapshot for safety
        print("\n💾 Saving snapshot...")
        snapshot_path = save_snapshot(claude_md, snapshot_dir)
        print(f"  Saved to: {snapshot_path}")

        return 1  # Exit with error

    else:
        print("\n✅ CLAUDE.md appears intact!")
        print("  No placeholder text detected")
        print("  All critical sections present")
        return 0


if __name__ == "__main__":
    sys.exit(main())
