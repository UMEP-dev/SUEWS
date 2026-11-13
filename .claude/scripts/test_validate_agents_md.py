"""Regression checks for the shared instruction document and import loader."""

import importlib.util
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
    def setUp(self):
        self.directory = tempfile.TemporaryDirectory()
        self.addCleanup(self.directory.cleanup)
        self.root = Path(self.directory.name)
        (self.root / "AGENTS.md").write_text(
            (ROOT / "AGENTS.md").read_text(encoding="utf-8"), encoding="utf-8"
        )
        (self.root / "CLAUDE.md").write_text(
            "@AGENTS.md\n<!-- compatibility -->\n", encoding="utf-8"
        )
        for name in VALIDATOR.REQUIRED_SKILL_FILES + VALIDATOR.REQUIRED_RULE_FILES:
            path = self.root / name
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_text("# Existing guidance\n", encoding="utf-8")
        (self.root / "MAINTAINERS.md").write_text(
            "# Maintainers\n\n- Software maintainer: Example (`@example-person`).\n",
            encoding="utf-8",
        )

    def run_validator(self):
        return subprocess.run(
            [sys.executable, str(SCRIPT)], cwd=self.root,
            capture_output=True, text=True, encoding="utf-8",
        )

    def test_valid_shared_instructions_and_loader(self):
        result = self.run_validator()
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)

    def test_missing_canonical_file_fails_without_traceback(self):
        (self.root / "AGENTS.md").unlink()
        result = self.run_validator()
        self.assertEqual(result.returncode, 1)
        self.assertIn("File not found", result.stdout)
        self.assertNotIn("Traceback", result.stderr)

    def test_truncated_canonical_file_fails(self):
        (self.root / "AGENTS.md").write_text("# AGENTS.md\n", encoding="utf-8")
        result = self.run_validator()
        self.assertEqual(result.returncode, 1)
        self.assertIn("Missing critical sections", result.stdout)

    def test_missing_core_principles_fails(self):
        path = self.root / "AGENTS.md"
        text = path.read_text(encoding="utf-8")
        path.write_text(
            text.replace("## Core principles", "## Preamble"), encoding="utf-8"
        )
        result = self.run_validator()
        self.assertEqual(result.returncode, 1)
        self.assertIn("## Core principles", result.stdout)

    def test_missing_roster_fails(self):
        (self.root / "MAINTAINERS.md").unlink()
        result = self.run_validator()
        self.assertEqual(result.returncode, 1)
        self.assertIn("MAINTAINERS.md", result.stdout)

    def test_person_named_outside_roster_fails(self):
        rule = self.root / ".claude" / "rules" / "00-project-essentials.md"
        rule.write_text("Ask @example-person to approve.\n", encoding="utf-8")
        result = self.run_validator()
        self.assertEqual(result.returncode, 1)
        self.assertIn("names a person", result.stdout)

    def test_wrong_import_fails(self):
        (self.root / "CLAUDE.md").write_text("@OTHER.md\n", encoding="utf-8")
        result = self.run_validator()
        self.assertEqual(result.returncode, 1)
        self.assertIn("must contain only @AGENTS.md", result.stdout)

    def test_missing_loader_fails(self):
        (self.root / "CLAUDE.md").unlink()
        result = self.run_validator()
        self.assertEqual(result.returncode, 1)
        self.assertIn("Missing CLAUDE.md compatibility import", result.stdout)

    def test_duplicate_loader_instructions_fail(self):
        (self.root / "CLAUDE.md").write_text(
            "@AGENTS.md\n\nDifferent rules\n", encoding="utf-8"
        )
        result = self.run_validator()
        self.assertEqual(result.returncode, 1)


if __name__ == "__main__":
    unittest.main()
