#!/usr/bin/env python3
"""
Validation script for AGENTS.md and shared agent-governance integrity.

Enforces:
1. Required sections, minimum size, and absence of truncation placeholders in AGENTS.md.
2. Thin @AGENTS.md compatibility import in CLAUDE.md.
3. Bidirectional 1:1 parity between .claude/rules/**/*.md (including YAML `paths:`
   frontmatter) and the `## Shared rule files` index in AGENTS.md.
4. Valid frontmatter keys in .claude/agents/*.md (`color:`, no `colour:` or `model: sonnet`).
5. Hook target existence in .claude/settings.json and referenced files in AGENTS.md.
6. Plain-ASCII hygiene across shared governance files.
"""

from datetime import datetime
import hashlib
import json
import os
from pathlib import Path
import re
import sys

# Critical sections that must be preserved in AGENTS.md
CRITICAL_SECTIONS = [
    "## Quick Start",
    "## Essential Rules",
    "## Project Structure",
    "## Skills",
    "## Rule loading",
    "## References",
    "## Shared rule files",
]

# Core skill files that must exist
REQUIRED_SKILL_FILES = [
    ".claude/skills/lint-code/SKILL.md",
    ".claude/skills/audit-docs/SKILL.md",
]

# Core rule files that must always exist
REQUIRED_RULE_FILES = [
    ".claude/rules/00-project-essentials.md",
    ".claude/rules/fortran/conventions.md",
    ".claude/rules/python/conventions.md",
    ".claude/rules/docs/conventions.md",
    ".claude/rules/tests/patterns.md",
    ".claude/rules/changelog/format.md",
]

# Suspicious placeholder patterns that indicate accidental truncation
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

SHARED_RULE_LINE_RE = re.compile(
    r"^- \[(?P<label>\.claude/rules/[^\]]+\.md)\]\((?P<target>[^)]+)\) - (?P<scope>.+)\.$"
)
REFERENCE_LINE_RE = re.compile(r"^- `(?P<path>[^`]+)` - .+$")
AUDIT_PROTOCOL_SECTIONS = (
    "## 1. Authority Hierarchy and Surface Separation",
    "## 2. Pre-Dispatch Baseline and Zero-Build Mechanical Suite",
    "## 3. Canonical 8-Seam Partition and Read-Coverage Accounting",
    "## 4. Evidence Record Contract",
    "## 5. Two-Axis Calibration Rubric (Impact Tier x Reachability)",
    "## 6. Mandatory Cross-Model Skeptic Falsification Pass",
    "## 7. Post-Audit Routing Discipline",
)


def check_ascii_clean(filepath: Path, root: Path) -> list[str]:
    """Return warnings for any non-ASCII line in filepath."""
    rel = filepath.relative_to(root).as_posix()
    text = filepath.read_text(encoding="utf-8")
    issues = []
    for lineno, line in enumerate(text.splitlines(), 1):
        if any(ord(ch) > 127 for ch in line):
            issues.append(f"{rel}:{lineno}: Non-ASCII character detected")
            if len(issues) >= 5:
                break
    return issues


def parse_rule_frontmatter_paths(
    filepath: Path, root: Path
) -> tuple[list[str] | None, list[str]]:
    """Parse optional YAML `paths:` frontmatter from a `.claude/rules/*.md` file.

    Returns (paths_or_none, warnings). `None` means the rule is unscoped.
    """
    rel = filepath.relative_to(root).as_posix()
    text = filepath.read_text(encoding="utf-8")
    if not text.startswith("---\n"):
        return None, []

    end_idx = text.find("\n---\n", 4)
    if end_idx == -1:
        return None, [f"{rel}: Unterminated YAML frontmatter block"]

    fm_lines = text[4:end_idx].splitlines()
    warnings: list[str] = []
    paths: list[str] = []
    in_paths = False

    for raw in fm_lines:
        stripped = raw.strip()
        if not stripped or stripped.startswith("#"):
            continue
        if not raw.startswith((" ", "\t")):
            key, sep, rest = stripped.partition(":")
            if not sep:
                warnings.append(f"{rel}: Malformed frontmatter line: {raw!r}")
                continue
            if key == "paths":
                in_paths = True
                inline = rest.strip()
                if inline:
                    warnings.append(
                        f"{rel}: Use block list items under `paths:` rather than inline {inline!r}"
                    )
            else:
                in_paths = False
                warnings.append(
                    f"{rel}: Unexpected frontmatter key {key!r} (only `paths` allowed)"
                )
        elif in_paths:
            if not stripped.startswith("- "):
                warnings.append(
                    f"{rel}: Expected `- <glob>` under `paths:`, got {raw!r}"
                )
                continue
            item = stripped[2:].strip()
            if len(item) >= 2 and item[0] == item[-1] and item[0] in {"'", '"'}:
                item = item[1:-1]
            paths.append(item)

    if not paths and not warnings:
        warnings.append(
            f"{rel}: Empty `paths:` frontmatter (omit frontmatter for unscoped rules)"
        )

    return paths, warnings


def parse_shared_rule_index(
    content: str,
) -> tuple[dict[str, list[str] | None], list[str]]:
    """Extract `.claude/rules/*.md` entries and their scopes from `## Shared rule files`."""
    warnings: list[str] = []
    entries: dict[str, list[str] | None] = {}

    marker = "## Shared rule files"
    idx = content.find(marker)
    if idx == -1:
        return entries, warnings

    tail = content[idx + len(marker) :]
    next_sec = re.search(r"^## ", tail, flags=re.MULTILINE)
    section_body = tail[: next_sec.start()] if next_sec else tail

    for raw_line in section_body.splitlines():
        line = raw_line.strip()
        if not line.startswith("- "):
            continue
        match = SHARED_RULE_LINE_RE.match(line)
        if not match:
            warnings.append(
                f"AGENTS.md `## Shared rule files`: Malformed bullet line: {line!r}"
            )
            continue
        label = match.group("label")
        target = match.group("target")
        scope_raw = match.group("scope").split(";", 1)[0].strip()

        if label != target:
            warnings.append(
                f"AGENTS.md `## Shared rule files`: Link text {label!r} != target {target!r}"
            )
        if label in entries:
            warnings.append(
                f"AGENTS.md `## Shared rule files`: Duplicate rule entry {label!r}"
            )

        if scope_raw == "all work":
            entries[label] = None
        else:
            globs = re.findall(r"`([^`]+)`", scope_raw)
            expected_rendered = ", ".join(f"`{g}`" for g in globs)
            if not globs or scope_raw != expected_rendered:
                warnings.append(
                    f"AGENTS.md `## Shared rule files`: Scope for {label} must be "
                    f"`all work` or comma-separated backticked globs (got {scope_raw!r})"
                )
            entries[label] = globs

    if not entries and not warnings:
        warnings.append("AGENTS.md `## Shared rule files` contains no rule bullets")

    return entries, warnings


def parse_references_section(content: str) -> tuple[list[str], list[str]]:
    """Extract repo-relative file and directory paths from `## References` in AGENTS.md."""
    warnings: list[str] = []
    refs: list[str] = []

    marker = "## References"
    idx = content.find(marker)
    if idx == -1:
        return refs, warnings

    tail = content[idx + len(marker) :]
    next_sec = re.search(r"^## ", tail, flags=re.MULTILINE)
    section_body = tail[: next_sec.start()] if next_sec else tail

    for raw_line in section_body.splitlines():
        line = raw_line.strip()
        if not line.startswith("- "):
            continue
        match = REFERENCE_LINE_RE.match(line)
        if not match:
            warnings.append(
                f"AGENTS.md `## References`: Malformed bullet line: {line!r}"
            )
            continue
        refs.append(match.group("path"))

    if not refs and not warnings:
        warnings.append("AGENTS.md `## References` contains no reference bullets")

    return refs, warnings


def check_references(root: Path, referenced_paths: list[str]) -> list[str]:
    """Verify every path listed under `## References` in AGENTS.md exists and check ASCII on governance docs."""
    warnings: list[str] = []
    for ref in referenced_paths:
        if ref.endswith("/"):
            if not (root / ref.rstrip("/")).is_dir():
                warnings.append(f"AGENTS.md `## References`: Missing directory {ref}")
            continue
        target = root / ref
        if not target.is_file():
            warnings.append(f"AGENTS.md `## References`: Missing file {ref}")
            continue
        if ref.startswith((".claude/", "dev-ref/")) and target.suffix == ".md":
            warnings.extend(check_ascii_clean(target, root))

    protocol = root / "dev-ref" / "CODEBASE_AUDIT_PROTOCOL.md"
    if protocol.is_file():
        proto_text = protocol.read_text(encoding="utf-8")
        missing_proto = [s for s in AUDIT_PROTOCOL_SECTIONS if s not in proto_text]
        if missing_proto:
            warnings.append(
                "dev-ref/CODEBASE_AUDIT_PROTOCOL.md: Missing sections: "
                + ", ".join(missing_proto)
            )
    return warnings


def check_rules_sync(
    root: Path, indexed_rules: dict[str, list[str] | None]
) -> list[str]:
    """Verify 1:1 parity between `.claude/rules/**/*.md` on disk and AGENTS.md index."""
    warnings: list[str] = []
    rules_dir = root / ".claude" / "rules"
    if not rules_dir.is_dir():
        warnings.append("Missing .claude/rules directory")
        return warnings

    disk_files = {
        p.relative_to(root).as_posix(): p for p in sorted(rules_dir.rglob("*.md"))
    }

    for indexed_path in sorted(indexed_rules):
        if indexed_path not in disk_files:
            warnings.append(f"AGENTS.md indexes missing rule file: {indexed_path}")

    for rel_path in sorted(disk_files):
        if rel_path not in indexed_rules:
            warnings.append(
                f"Unindexed rule file on disk (missing from AGENTS.md `## Shared rule files`): {rel_path}"
            )

    for rel_path, rule_file in sorted(disk_files.items()):
        warnings.extend(check_ascii_clean(rule_file, root))
        fm_paths, fm_warnings = parse_rule_frontmatter_paths(rule_file, root)
        warnings.extend(fm_warnings)
        if rel_path not in indexed_rules:
            continue
        indexed_scope = indexed_rules[rel_path]
        if fm_paths != indexed_scope:
            warnings.append(
                f"Scope mismatch for {rel_path}: frontmatter paths={fm_paths} "
                f"!= AGENTS.md index={indexed_scope}"
            )

    return warnings


def check_agents_dir(root: Path) -> list[str]:
    """Validate `.claude/agents/*.md` frontmatter and ASCII hygiene when present."""
    warnings: list[str] = []
    agents_dir = root / ".claude" / "agents"
    if not agents_dir.is_dir():
        return warnings

    for agent_file in sorted(agents_dir.glob("*.md")):
        rel = agent_file.relative_to(root).as_posix()
        warnings.extend(check_ascii_clean(agent_file, root))
        text = agent_file.read_text(encoding="utf-8")
        if not text.startswith("---\n"):
            warnings.append(f"{rel}: Missing YAML frontmatter")
            continue
        end_idx = text.find("\n---\n", 4)
        if end_idx == -1:
            warnings.append(f"{rel}: Unterminated YAML frontmatter")
            continue
        fm = text[4:end_idx]
        for line in fm.splitlines():
            key, sep, val = line.partition(":")
            if not sep:
                continue
            k = key.strip()
            v = val.strip()
            if k == "colour":
                warnings.append(
                    f"{rel}: Use `color:` frontmatter key, not British `colour:`"
                )
            if k == "model" and v in {"sonnet", "haiku"}:
                warnings.append(
                    f"{rel}: Do not pin `model: {v}` in subagent frontmatter (omit `model:` to inherit)"
                )
    return warnings


def _check_settings_hook_commands(settings: dict, root: Path) -> list[str]:
    """Verify every `$CLAUDE_PROJECT_DIR/...` hook script exists and is executable."""
    warnings: list[str] = []
    prefix = "$CLAUDE_PROJECT_DIR/"
    hooks = settings.get("hooks", {})
    for event_name, groups in hooks.items():
        if not isinstance(groups, list):
            continue
        for group in groups:
            for hook in group.get("hooks", []):
                cmd = hook.get("command", "")
                if not cmd.startswith(prefix):
                    continue
                rel_target = cmd[len(prefix) :].split()[0]
                target_path = root / rel_target
                if not target_path.is_file():
                    warnings.append(
                        f".claude/settings.json ({event_name}): Missing hook script {rel_target}"
                    )
                elif not os.access(target_path, os.X_OK):
                    warnings.append(
                        f".claude/settings.json ({event_name}): Hook script not executable: {rel_target}"
                    )
    return warnings


def check_hooks_and_settings(root: Path) -> list[str]:
    """Validate `.claude/settings.json` hook targets and ASCII hygiene on hooks/scripts."""
    warnings: list[str] = []
    settings_path = root / ".claude" / "settings.json"
    if settings_path.is_file():
        warnings.extend(check_ascii_clean(settings_path, root))
        try:
            settings = json.loads(settings_path.read_text(encoding="utf-8"))
        except json.JSONDecodeError as exc:
            warnings.append(f".claude/settings.json: Invalid JSON: {exc}")
            settings = {}
        warnings.extend(_check_settings_hook_commands(settings, root))

    for subdir in (".claude/hooks", ".claude/scripts"):
        dir_path = root / subdir
        if dir_path.is_dir():
            for p in sorted(dir_path.iterdir()):
                if p.is_file() and p.suffix in {".sh", ".py"}:
                    warnings.extend(check_ascii_clean(p, root))

    readme_path = root / ".claude" / "README.md"
    agents_file = root / "AGENTS.md"
    referenced = (
        set(parse_references_section(agents_file.read_text(encoding="utf-8"))[0])
        if agents_file.is_file()
        else set()
    )
    if readme_path.is_file() and ".claude/README.md" not in referenced:
        warnings.extend(check_ascii_clean(readme_path, root))

    return warnings


def check_file_integrity(filepath: Path) -> dict:
    """Check AGENTS.md and shared `.claude/` governance for integrity issues."""
    if not filepath.exists():
        return {
            "valid": False,
            "error": f"File not found: {filepath}",
            "warnings": [],
            "stats": {},
        }

    root = filepath.parent
    content = filepath.read_text(encoding="utf-8")
    lines = content.splitlines()
    warnings: list[str] = []

    warnings.extend(check_ascii_clean(filepath, root))

    # Check for placeholder patterns
    for i, line in enumerate(lines, 1):
        for pattern in PLACEHOLDER_PATTERNS:
            if pattern.lower() in line.lower():
                warnings.append(
                    f"Line {i}: Suspicious placeholder text found: '{pattern}'"
                )

    # Check for critical sections
    missing_sections = [s for s in CRITICAL_SECTIONS if s not in content]
    if missing_sections:
        warnings.append(f"Missing critical sections: {', '.join(missing_sections)}")

    # Check that required skill files exist
    missing_skills = [sf for sf in REQUIRED_SKILL_FILES if not (root / sf).exists()]
    if missing_skills:
        warnings.append(f"Missing skill files: {', '.join(missing_skills)}")

    # Check that required rule files exist
    missing_rules = [rf for rf in REQUIRED_RULE_FILES if not (root / rf).exists()]
    if missing_rules:
        warnings.append(f"Missing rule files: {', '.join(missing_rules)}")

    # Check `## References` existence and audit-protocol integrity
    referenced_paths, ref_warnings = parse_references_section(content)
    warnings.extend(ref_warnings)
    if "## References" in content:
        warnings.extend(check_references(root, referenced_paths))

    # Check `## Shared rule files` 1:1 parity against `.claude/rules/`
    indexed_rules, index_warnings = parse_shared_rule_index(content)
    warnings.extend(index_warnings)
    if "## Shared rule files" in content:
        warnings.extend(check_rules_sync(root, indexed_rules))

    # Check `.claude/agents/`, `.claude/hooks/`, `.claude/scripts/`, and `.claude/settings.json`
    warnings.extend(check_agents_dir(root))
    warnings.extend(check_hooks_and_settings(root))

    content_hash = hashlib.sha256(content.encode("utf-8")).hexdigest()
    stats = {
        "lines": len(lines),
        "characters": len(content),
        "sections": content.count("##"),
        "code_blocks": content.count("```"),
        "indexed_rules": len(indexed_rules),
        "hash": content_hash[:16],
        "timestamp": datetime.now().isoformat(),
    }

    min_lines = 45
    min_chars = 1200

    if stats["lines"] < min_lines:
        warnings.append(
            f"File appears truncated: only {stats['lines']} lines (expected >= {min_lines})"
        )

    if stats["characters"] < min_chars:
        warnings.append(
            f"File appears truncated: only {stats['characters']} characters (expected >= {min_chars})"
        )

    return {"valid": len(warnings) == 0, "warnings": warnings, "stats": stats}


def save_snapshot(filepath: Path, snapshot_dir: Path) -> Path:
    """Save a timestamped snapshot of AGENTS.md."""
    snapshot_dir.mkdir(parents=True, exist_ok=True)

    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    snapshot_path = snapshot_dir / f"AGENTS.md.{timestamp}"

    content = filepath.read_text(encoding="utf-8")
    snapshot_path.write_text(content, encoding="utf-8")

    metadata = {
        "timestamp": datetime.now().isoformat(),
        "lines": len(content.splitlines()),
        "characters": len(content),
        "hash": hashlib.sha256(content.encode("utf-8")).hexdigest(),
    }

    metadata_path = snapshot_dir / f"AGENTS.md.{timestamp}.json"
    metadata_path.write_text(json.dumps(metadata, indent=2), encoding="utf-8")

    return snapshot_path


def main() -> int:
    """Main validation routine."""
    agents_md = Path("AGENTS.md")
    backup_path = Path("AGENTS.md.backup")
    snapshot_dir = Path(".claude/snapshots")

    print("=" * 60)
    print("AGENTS.md Integrity Validator")
    print("=" * 60)

    print("\nChecking AGENTS.md...")
    result = check_file_integrity(agents_md)

    print("\nFile Statistics:")
    for key, value in result["stats"].items():
        print(f"  {key}: {value}")

    loader = Path("CLAUDE.md")
    if not loader.is_file():
        result["warnings"].append("Missing CLAUDE.md compatibility import")
    else:
        result["warnings"].extend(check_ascii_clean(loader, Path(".")))
        loader_content = re.sub(
            r"<!--.*?-->", "", loader.read_text(encoding="utf-8"), flags=re.DOTALL
        )
        if loader_content.strip() != "@AGENTS.md":
            result["warnings"].append(
                "CLAUDE.md must contain only @AGENTS.md and optional comments"
            )

    if not result["valid"] or result["warnings"]:
        if result.get("error"):
            print(result["error"])
        print("\nWARNINGS DETECTED:")
        for warning in result["warnings"]:
            print(f"  - {warning}")

        if backup_path.exists():
            print("\nChecking backup file...")
            backup_result = check_file_integrity(backup_path)

            if not backup_result["warnings"]:
                print("  OK: Backup appears intact")
                print("\n  To restore from backup, run:")
                print("    cp AGENTS.md.backup AGENTS.md")
            else:
                print("  WARNING: Backup also has issues")

        print("\nSaving snapshot...")
        if agents_md.exists():
            snapshot_path = save_snapshot(agents_md, snapshot_dir)
            print(f"  Saved to: {snapshot_path}")

        return 1

    print("\nOK: AGENTS.md and shared .claude governance appear intact!")
    print("  No placeholder text or non-ASCII characters detected")
    print("  All critical sections and rule index entries synced 1:1")
    return 0


if __name__ == "__main__":
    sys.exit(main())
