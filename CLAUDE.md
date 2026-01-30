# CLAUDE.md

Guidance for Claude Code in this repository.

## Quick Start

```bash
uv venv && source .venv/bin/activate && make dev && make test-smoke
```

Full setup: `/setup-dev` | Style check: `/lint-code` | Build check: `/verify-build`

## Essential Rules

- **British English** (exception: numpy/scipy conventions like "analyze")
- **No emoji** in print/logging - plain ASCII only
- **Test before commit**: `make test-smoke`
- **Git remote**: `origin` only (`git@github.com:UMEP-dev/SUEWS.git`)
- **New source files**: Add to `meson.build`

## Project Structure

| Directory | Purpose | Rules |
|-----------|---------|-------|
| `src/suews/src/` | Fortran | `.claude/rules/fortran/` |
| `src/supy/` | Python | `.claude/rules/python/` |
| `docs/` | Documentation | `.claude/rules/docs/` |
| `test/` | Tests | `.claude/rules/tests/` |
| `.github/workflows/` | CI/Actions | `.claude/rules/ci/` |

## Skills

- `/setup-dev` - Environment setup
- `/lint-code` - Check code style
- `/sync-docs` - Doc-code consistency
- `/verify-build` - Build configuration
- `/audit-pr` - Review pull requests
- `/log-changes` - Update CHANGELOG
- `/prep-release` - Prepare releases
- `/examine-issue` - Analyse GitHub issues
- `/gh-post` - Post figures to GitHub PRs/Issues

## Auto-Loaded Rules

Rules in `.claude/rules/` load automatically based on files being edited.

## References

- `.claude/README.md` - Full workspace documentation
- `.claude/skills/` - Detailed skill workflows
- `.claude/rules/` - Style conventions
