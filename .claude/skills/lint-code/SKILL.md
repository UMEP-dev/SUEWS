---
name: lint-code
description: Check code style and repository static/contract invariants across Python, Fortran, Rust, tests, CI, and governance files.
---

# Lint Code

Check code against project conventions in `.claude/rules/` and the repository's
static and contract linters under `scripts/lint/`, `scripts/suews/`, and
`.claude/scripts/`.

## Workflow

1. **Standard formatters and linters**
   - Python: `ruff check` and `ruff format --check src test`
   - Fortran: `fprettify --diff src/suews/src/*.f95`
   - Rust (`src/suews_bridge/`):
     ```bash
     SUEWS_BRIDGE_LINT_ONLY=1 cargo clippy --manifest-path src/suews_bridge/Cargo.toml --all-features --all-targets -- -D warnings
     ```
2. **Scoped docstring check for changed Python files**
   - Run `ruff check --select D` on `git diff --name-only origin/master...HEAD -- '*.py'`
     to surface numpy-style docstring violations separately in the report.
   - Legacy debt is parked in `[lint.per-file-ignores]` in `.ruff.toml` (seeded
     from the whole repo, not just `src/supy/`); any finding reported here is
     new debt introduced on this branch.
3. **Argument-free repository static checks** (pass NO CLI flags to these
   scripts -- they reject unexpected arguments):
   ```bash
   python scripts/lint/check_saved_fortran_locals.py
   python scripts/lint/check_test_markers.py
   python scripts/lint/check_packaged_data_paths.py
   # Requires a built supy .venv (runs pytest --collect-only via test/conftest.py):
   python scripts/lint/check_physics_tier_delta.py
   ```
4. **Path, contract, and diff checks** (run all cheap tree checks; run diff-based
   or CI-artefact checks when relevant paths or metrics are in scope):
   ```bash
   python scripts/lint/check_pathlib_encoding.py src test scripts mcp docs .claude plugins
   python scripts/lint/check_rust_yaml_aliases.py
   python scripts/lint/check_schema_version_bump.py --base origin/master
   python scripts/lint/check_data_interface_version_history.py --base origin/master
   python scripts/lint/check_forcing_contract_artefacts.py
   python scripts/lint/check_output_contract_artefacts.py
   python scripts/lint/check_knowledge_pack_freshness.py --base origin/master
   # When auditing pytest cost markers against downloaded nightly CI metrics:
   # python scripts/lint/check_cost_markers.py <ci-metrics-dir-or-file>
   ```
5. **Fortran, bridge manifest, and agent-governance checks**
   ```bash
   # Scope check_naming_conventions.py to new/changed .f95 files (full tree has legacy pre-2025 modules):
   CHANGED_F95=$(git diff --name-only origin/master...HEAD -- 'src/suews/src/*.f95')
   [ -n "$CHANGED_F95" ] && echo "$CHANGED_F95" | xargs python scripts/suews/check_naming_conventions.py
   python scripts/suews/check_bridge_manifest.py
   python .claude/scripts/validate-claude-md.py
   ```
6. **Rule-based review** against `.claude/rules/` for touched surfaces; report
   findings grouped by surface and severity.

## File Types and Rule Sources

| Surface | Rules |
|---------|-------|
| Fortran | `.claude/rules/fortran/conventions.md`, `.claude/rules/fortran/error-reporting.md` |
| Python | `.claude/rules/python/conventions.md` |
| Rust & Bridge | `.claude/rules/rust/conventions.md`, `.claude/rules/rust/bridge-contract.md` |
| Tests | `.claude/rules/tests/patterns.md` |
| CI Workflows | `.claude/rules/ci/conventions.md` |
| CHANGELOG | `.claude/rules/changelog/format.md` |

## Output Format

```text
[code-style] Analysis

=== Fortran ===
  file.f95 (symbol): issue description

=== Python ===
  file.py (symbol): issue description

=== Rust / Bridge ===
  file.rs (symbol): issue description

=== Static & Contract Lints ===
  check_name: [OK] | [X] summary

=== Docstrings ===
  file.py (symbol): Dxxx description (numpy-style rule)

Summary: N files, M issues
```

If a changed file still shows docstring findings after a good-faith fix, check whether
the file is in `[lint.per-file-ignores]` in `.ruff.toml` -- if so, either (a) finish
cleaning it and remove its entry from that table, or (b) leave it alone (existing debt).

## References

- `.claude/rules/` - Full conventions
- `references/quick-checks.md` - Command catalogue and common issues
- Documentation prose is out of scope here: house style lives in
  `.claude/rules/docs/conventions.md`, `audit-docs` checks non-ASCII and `.bib`
  topic tags, and the `docs-reviewer` agent reviews doc-code drift.
