"""Regression checks for AGENTS.md, CLAUDE.md, and shared `.claude/` governance."""

import importlib.util
import json
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest

SCRIPT = Path(__file__).with_name("validate-claude-md.py")
ROOT = SCRIPT.parents[2]
SPEC = importlib.util.spec_from_file_location("instruction_validator", SCRIPT)
VALIDATOR = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(VALIDATOR)


class SharedInstructionValidation(unittest.TestCase):
    """Verify validate-claude-md.py catches drift across shared agent governance files."""

    def setUp(self):
        """Create a minimal valid governance fixture tree mirroring AGENTS.md."""
        self.directory = tempfile.TemporaryDirectory()
        self.addCleanup(self.directory.cleanup)
        self.root = Path(self.directory.name)

        agents_content = (ROOT / "AGENTS.md").read_text(encoding="utf-8")
        (self.root / "AGENTS.md").write_text(agents_content, encoding="utf-8")
        (self.root / "CLAUDE.md").write_text(
            "@AGENTS.md\n<!-- compatibility -->\n", encoding="utf-8"
        )

        referenced_paths, ref_warnings = VALIDATOR.parse_references_section(
            agents_content
        )
        self.assertEqual(ref_warnings, [])
        for ref in referenced_paths:
            if ref.endswith("/"):
                (self.root / ref.rstrip("/")).mkdir(parents=True, exist_ok=True)
            else:
                target = self.root / ref
                target.parent.mkdir(parents=True, exist_ok=True)
                if ref == "dev-ref/CODEBASE_AUDIT_PROTOCOL.md":
                    sections = "\n\n".join(VALIDATOR.AUDIT_PROTOCOL_SECTIONS)
                    target.write_text(f"# Protocol\n\n{sections}\n", encoding="utf-8")
                else:
                    target.write_text("# Reference\n", encoding="utf-8")

        for name in VALIDATOR.REQUIRED_SKILL_FILES:
            path = self.root / name
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_text("# Skill guidance\n", encoding="utf-8")

        indexed_rules, parse_warnings = VALIDATOR.parse_shared_rule_index(
            agents_content
        )
        self.assertEqual(parse_warnings, [])
        for rel_path, globs in indexed_rules.items():
            rule_path = self.root / rel_path
            rule_path.parent.mkdir(parents=True, exist_ok=True)
            if globs is None:
                rule_path.write_text("# Unscoped rule\n", encoding="utf-8")
            else:
                yaml_items = "\n".join(f"  - {g}" for g in globs)
                rule_path.write_text(
                    f"---\npaths:\n{yaml_items}\n---\n\n# Scoped rule\n",
                    encoding="utf-8",
                )

    def run_validator(self, cwd: Path | None = None):
        """Execute validate-claude-md.py in ``cwd`` and return CompletedProcess."""
        return subprocess.run(
            [sys.executable, str(SCRIPT)],
            cwd=cwd or self.root,
            capture_output=True,
            text=True,
            encoding="utf-8",
            check=False,
        )

    def test_valid_shared_instructions_and_loader(self):
        """Synthetic valid governance tree passes with exit code 0."""
        result = self.run_validator()
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)

    def test_live_repository_passes_validation(self):
        """Live repository root passes all governance integrity checks."""
        result = self.run_validator(cwd=ROOT)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)

    def test_missing_canonical_file_fails_without_traceback(self):
        """Missing AGENTS.md exits 1 cleanly without an unhandled traceback."""
        (self.root / "AGENTS.md").unlink()
        result = self.run_validator()
        self.assertEqual(result.returncode, 1)
        self.assertIn("File not found", result.stdout)
        self.assertNotIn("Traceback", result.stderr)

    def test_truncated_canonical_file_fails(self):
        """Truncated AGENTS.md missing critical sections exits 1."""
        (self.root / "AGENTS.md").write_text("# AGENTS.md\n", encoding="utf-8")
        result = self.run_validator()
        self.assertEqual(result.returncode, 1)
        self.assertIn("Missing critical sections", result.stdout)

    def test_wrong_import_fails(self):
        """CLAUDE.md importing anything other than @AGENTS.md exits 1."""
        (self.root / "CLAUDE.md").write_text("@OTHER.md\n", encoding="utf-8")
        result = self.run_validator()
        self.assertEqual(result.returncode, 1)
        self.assertIn("must contain only @AGENTS.md", result.stdout)

    def test_missing_loader_fails(self):
        """Missing CLAUDE.md compatibility loader exits 1."""
        (self.root / "CLAUDE.md").unlink()
        result = self.run_validator()
        self.assertEqual(result.returncode, 1)
        self.assertIn("Missing CLAUDE.md compatibility import", result.stdout)

    def test_duplicate_loader_instructions_fail(self):
        """Extra prose instructions inside CLAUDE.md exit 1."""
        (self.root / "CLAUDE.md").write_text(
            "@AGENTS.md\n\nDifferent rules\n", encoding="utf-8"
        )
        result = self.run_validator()
        self.assertEqual(result.returncode, 1)
        self.assertIn("must contain only @AGENTS.md", result.stdout)

    def test_unindexed_rule_file_on_disk_fails(self):
        """Unindexed .claude/rules/*.md file on disk exits 1."""
        orphan = self.root / ".claude/rules/orphan-rule.md"
        orphan.write_text("# Orphan\n", encoding="utf-8")
        result = self.run_validator()
        self.assertEqual(result.returncode, 1)
        self.assertIn("Unindexed rule file on disk", result.stdout)

    def test_frontmatter_scope_drift_fails(self):
        """Mismatch between rule paths frontmatter and AGENTS.md index exits 1."""
        rule = self.root / ".claude/rules/changelog/format.md"
        rule.write_text(
            "---\npaths:\n  - OTHER.md\n---\n\n# Drifted scope\n",
            encoding="utf-8",
        )
        result = self.run_validator()
        self.assertEqual(result.returncode, 1)
        self.assertIn(
            "Scope mismatch for .claude/rules/changelog/format.md", result.stdout
        )

    def test_agent_frontmatter_colour_and_sonnet_fail(self):
        """British colour key or pinned sonnet model in .claude/agents/*.md exits 1."""
        agents_dir = self.root / ".claude/agents"
        agents_dir.mkdir(parents=True, exist_ok=True)
        (agents_dir / "bad-agent.md").write_text(
            "---\nname: bad\nmodel: sonnet\ncolour: red\n---\n\nBody\n",
            encoding="utf-8",
        )
        result = self.run_validator()
        self.assertEqual(result.returncode, 1)
        self.assertIn("Use `color:` frontmatter key", result.stdout)
        self.assertIn("Do not pin `model: sonnet`", result.stdout)

    def test_missing_settings_hook_target_fails(self):
        """Non-existent hook target in .claude/settings.json exits 1."""
        settings = {
            "hooks": {
                "SessionStart": [
                    {
                        "hooks": [
                            {
                                "type": "command",
                                "command": "$CLAUDE_PROJECT_DIR/.claude/hooks/missing.sh",
                            }
                        ]
                    }
                ]
            }
        }
        (self.root / ".claude/settings.json").write_text(
            json.dumps(settings), encoding="utf-8"
        )
        result = self.run_validator()
        self.assertEqual(result.returncode, 1)
        self.assertIn("Missing hook script .claude/hooks/missing.sh", result.stdout)

    def test_non_ascii_in_rule_fails(self):
        """Non-ASCII characters in .claude/rules/*.md exit 1."""
        rule = self.root / ".claude/rules/00-project-essentials.md"
        rule.write_text("# Rule \u2014 non-ascii em-dash\n", encoding="utf-8")
        result = self.run_validator()
        self.assertEqual(result.returncode, 1)
        self.assertIn("Non-ASCII character detected", result.stdout)

    def test_missing_referenced_audit_protocol_fails(self):
        """Missing file listed under AGENTS.md ## References exits 1."""
        (self.root / "dev-ref/CODEBASE_AUDIT_PROTOCOL.md").unlink()
        result = self.run_validator()
        self.assertEqual(result.returncode, 1)
        self.assertIn(
            "AGENTS.md `## References`: Missing file dev-ref/CODEBASE_AUDIT_PROTOCOL.md",
            result.stdout,
        )

    def test_non_ascii_or_missing_sections_in_audit_protocol_fails(self):
        """Non-ASCII or missing canonical sections in CODEBASE_AUDIT_PROTOCOL.md exits 1."""
        (self.root / "dev-ref/CODEBASE_AUDIT_PROTOCOL.md").write_text(
            "# Truncated \u2014 non-ascii\n", encoding="utf-8"
        )
        result = self.run_validator()
        self.assertEqual(result.returncode, 1)
        self.assertIn("Non-ASCII character detected", result.stdout)
        self.assertIn(
            "dev-ref/CODEBASE_AUDIT_PROTOCOL.md: Missing sections", result.stdout
        )


if __name__ == "__main__":
    unittest.main()
