"""Regression checks for AGENTS.md, CLAUDE.md, and shared `.claude/` governance."""

import importlib.util
import json
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest

try:
    import pytest
except ImportError:

    class _StdlibMark:
        @staticmethod
        def contract(*_ids: str) -> None:
            return None

    class _StdlibPytest:
        mark = _StdlibMark()

    pytest = _StdlibPytest()

pytestmark = pytest.mark.contract("C-TOOLING")

SCRIPT = Path(__file__).with_name("validate-claude-md.py")
ROOT = SCRIPT.parents[2]
SPEC = importlib.util.spec_from_file_location("instruction_validator", SCRIPT)
VALIDATOR = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(VALIDATOR)


class GovernanceFixture(unittest.TestCase):
    """Build a minimal valid governance tree in a temporary directory."""

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

        (self.root / "MAINTAINERS.md").write_text(
            "# Maintainers\n\n- Software maintainer: Example (`@example-person`).\n",
            encoding="utf-8",
        )

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

    def write_settings(self, settings):
        """Write ``settings`` as the fixture's .claude/settings.json."""
        path = self.root / ".claude/settings.json"
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(json.dumps(settings), encoding="utf-8")

    def write_hook_script(self, name: str, mode: int) -> None:
        """Create .claude/hooks/<name> with file mode ``mode``."""
        script = self.root / ".claude/hooks" / name
        script.parent.mkdir(parents=True, exist_ok=True)
        script.write_text("#!/bin/sh\nexit 0\n", encoding="utf-8")
        script.chmod(mode)


def hook_settings(*commands: str) -> dict:
    """Return a settings object with one SessionStart hook per command."""
    hooks = [{"type": "command", "command": c} for c in commands]
    return {"hooks": {"SessionStart": [{"hooks": hooks}]}}


class SharedInstructionValidation(GovernanceFixture):
    """Verify validate-claude-md.py catches drift across shared agent governance files."""

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

    def test_missing_core_principles_fails(self):
        """AGENTS.md without its Core principles section exits 1."""
        path = self.root / "AGENTS.md"
        text = path.read_text(encoding="utf-8")
        path.write_text(
            text.replace("## Core principles", "## Preamble"), encoding="utf-8"
        )
        result = self.run_validator()
        self.assertEqual(result.returncode, 1)
        self.assertIn("## Core principles", result.stdout)

    def test_missing_roster_fails(self):
        """Missing MAINTAINERS.md exits 1."""
        (self.root / "MAINTAINERS.md").unlink()
        result = self.run_validator()
        self.assertEqual(result.returncode, 1)
        self.assertIn("MAINTAINERS.md", result.stdout)

    def test_person_named_outside_roster_fails(self):
        """A roster handle written in a rule file exits 1."""
        rule = self.root / ".claude" / "rules" / "00-project-essentials.md"
        rule.write_text("Ask @example-person to approve.\n", encoding="utf-8")
        result = self.run_validator()
        self.assertEqual(result.returncode, 1)
        self.assertIn("names a person", result.stdout)

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
        self.assertIn("Do not pin `model:`", result.stdout)

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


class GovernanceEdgeCaseValidation(GovernanceFixture):
    """Edge cases in rule parity, hook commands and agent frontmatter."""

    def test_indexed_rule_missing_on_disk_fails(self):
        """A rule listed in AGENTS.md but absent from .claude/rules/ exits 1."""
        (self.root / ".claude/rules/code-design.md").unlink()
        result = self.run_validator()
        self.assertEqual(result.returncode, 1)
        self.assertIn(
            "AGENTS.md indexes missing rule file: .claude/rules/code-design.md",
            result.stdout,
        )

    def test_malformed_rule_frontmatter_fails(self):
        """A rule frontmatter line without a key, or an unknown key, exits 1."""
        rule = self.root / ".claude/rules/changelog/format.md"
        rule.write_text("---\nnot a key\nscope: x\n---\n\n# Bad\n", encoding="utf-8")
        result = self.run_validator()
        self.assertEqual(result.returncode, 1)
        self.assertIn("Malformed frontmatter line", result.stdout)
        self.assertIn("Unexpected frontmatter key 'scope'", result.stdout)

    def test_non_executable_hook_script_fails(self):
        """A hook script run directly but lacking the executable bit exits 1."""
        self.write_hook_script("plain.sh", 0o644)
        self.write_settings(hook_settings("$CLAUDE_PROJECT_DIR/.claude/hooks/plain.sh"))
        result = self.run_validator()
        self.assertEqual(result.returncode, 1)
        self.assertIn(
            "Hook script not executable: .claude/hooks/plain.sh", result.stdout
        )

    def test_interpreter_hook_script_need_not_be_executable(self):
        """A script passed to an interpreter only has to exist."""
        self.write_hook_script("plain.sh", 0o644)
        self.write_settings(
            hook_settings("bash $CLAUDE_PROJECT_DIR/.claude/hooks/plain.sh")
        )
        result = self.run_validator()
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)

    def test_quoted_and_braced_hook_targets_are_checked(self):
        """Quoted, braced and interpreter-prefixed hook paths are all resolved."""
        self.write_settings(
            hook_settings(
                '"$CLAUDE_PROJECT_DIR"/.claude/hooks/a.sh',
                '"$CLAUDE_PROJECT_DIR/.claude/hooks/b.sh"',
                "${CLAUDE_PROJECT_DIR}/.claude/hooks/c.sh",
                "bash $CLAUDE_PROJECT_DIR/.claude/hooks/d.sh",
            )
        )
        result = self.run_validator()
        self.assertEqual(result.returncode, 1)
        for name in ("a", "b", "c"):
            self.assertIn(f"Missing hook script .claude/hooks/{name}.sh", result.stdout)
        self.assertIn("Missing hook path .claude/hooks/d.sh", result.stdout)

    def test_directory_argument_in_hook_command_passes(self):
        """A `$CLAUDE_PROJECT_DIR/<dir>` argument only has to exist."""
        self.write_hook_script("run.sh", 0o755)
        self.write_settings(
            hook_settings(
                'cd "$CLAUDE_PROJECT_DIR/.claude" && '
                "$CLAUDE_PROJECT_DIR/.claude/hooks/run.sh"
            )
        )
        result = self.run_validator()
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)

    def test_malformed_settings_shape_fails_without_traceback(self):
        """A non-object settings root or `hooks` value is reported, not raised."""
        for settings in ([], {"hooks": []}):
            self.write_settings(settings)
            result = self.run_validator()
            self.assertEqual(result.returncode, 1)
            self.assertIn(".claude/settings.json", result.stdout)
            self.assertNotIn("Traceback", result.stderr)

    def test_null_hooks_passes(self):
        """`"hooks": null` means no hooks and passes."""
        self.write_settings({"hooks": None})
        result = self.run_validator()
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)

    def test_agent_frontmatter_model_and_required_keys(self):
        """Any top-level `model:` or a missing description exits 1."""
        agents_dir = self.root / ".claude/agents"
        agents_dir.mkdir(parents=True, exist_ok=True)
        (agents_dir / "pinned.md").write_text(
            '---\nname: pinned\nmodel: "opus"\n---\n\nBody\n', encoding="utf-8"
        )
        result = self.run_validator()
        self.assertEqual(result.returncode, 1)
        self.assertIn(".claude/agents/pinned.md: Do not pin `model:`", result.stdout)
        self.assertIn("Missing required frontmatter keys: description", result.stdout)

    def test_agent_frontmatter_ignores_indented_and_comment_lines(self):
        """Indented continuations and comments are not read as top-level keys."""
        agents_dir = self.root / ".claude/agents"
        agents_dir.mkdir(parents=True, exist_ok=True)
        (agents_dir / "ok.md").write_text(
            "---\nname: ok\ndescription: |\n  model: sonnet\n  colour: red\n"
            "# model: haiku\ncolor: blue\n---\n\nBody\n",
            encoding="utf-8",
        )
        result = self.run_validator()
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)


class SharedRuleIndexParsing(unittest.TestCase):
    """Parse the `## Shared rule files` index without a fixture tree."""

    def test_principle_suffix_is_accepted(self):
        """Index entries ending in `; Principle N` keep their scope globs."""
        entries, warnings = VALIDATOR.parse_shared_rule_index(
            "## Shared rule files\n\n"
            "- [.claude/rules/a.md](.claude/rules/a.md) - all work; Principle 1, 2, 3.\n"
            "- [.claude/rules/b.md](.claude/rules/b.md) - `x/**`, `y.md`; "
            "also load when changing x; Principle 1.\n"
        )
        self.assertEqual(warnings, [])
        self.assertEqual(
            entries,
            {".claude/rules/a.md": None, ".claude/rules/b.md": ["x/**", "y.md"]},
        )

    def test_malformed_principle_suffix_fails(self):
        """A `; Principle` note that is not `Principle N[, N]` is reported."""
        _, warnings = VALIDATOR.parse_shared_rule_index(
            "## Shared rule files\n\n"
            "- [.claude/rules/a.md](.claude/rules/a.md) - all work; Principle one.\n"
        )
        self.assertTrue(
            any("Malformed principle note" in w for w in warnings), warnings
        )

    def test_demoted_heading_is_not_the_index(self):
        """Only a whole-line `## Shared rule files` heading starts the index."""
        entries, _ = VALIDATOR.parse_shared_rule_index(
            "See `## Shared rule files` below.\n\n### Shared rule files\n\n"
            "- [.claude/rules/a.md](.claude/rules/a.md) - all work.\n"
        )
        self.assertEqual(entries, {})

    def test_unindented_paths_list_and_trailing_comment(self):
        """`paths: # note` followed by unindented `- glob` items parses cleanly."""
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            rule = root / "rule.md"
            rule.write_text(
                "---\npaths: # scoped\n- a/**\n- 'b.md'\n---\n\n# Rule\n",
                encoding="utf-8",
            )
            paths, warnings = VALIDATOR.parse_rule_frontmatter_paths(rule, root)
        self.assertEqual(warnings, [])
        self.assertEqual(paths, ["a/**", "b.md"])


if __name__ == "__main__":
    unittest.main()
