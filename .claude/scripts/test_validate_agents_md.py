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
        (self.root / "AGENTS.md").write_text((ROOT / "AGENTS.md").read_text())
        (self.root / "CLAUDE.md").write_text("@AGENTS.md\n<!-- compatibility -->\n")
        for name in VALIDATOR.REQUIRED_SKILL_FILES + VALIDATOR.REQUIRED_RULE_FILES:
            path = self.root / name
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_text("# Existing guidance\n")

    def run_validator(self):
        return subprocess.run(
            [sys.executable, str(SCRIPT)], cwd=self.root,
            capture_output=True, text=True,
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
        (self.root / "AGENTS.md").write_text("# AGENTS.md\n")
        result = self.run_validator()
        self.assertEqual(result.returncode, 1)
        self.assertIn("Missing critical sections", result.stdout)

    def test_wrong_import_fails(self):
        (self.root / "CLAUDE.md").write_text("@OTHER.md\n")
        result = self.run_validator()
        self.assertEqual(result.returncode, 1)
        self.assertIn("must contain only @AGENTS.md", result.stdout)

    def test_missing_loader_fails(self):
        (self.root / "CLAUDE.md").unlink()
        result = self.run_validator()
        self.assertEqual(result.returncode, 1)
        self.assertIn("Missing CLAUDE.md compatibility import", result.stdout)

    def test_duplicate_loader_instructions_fail(self):
        (self.root / "CLAUDE.md").write_text("@AGENTS.md\n\nDifferent rules\n")
        result = self.run_validator()
        self.assertEqual(result.returncode, 1)


if __name__ == "__main__":
    unittest.main()
