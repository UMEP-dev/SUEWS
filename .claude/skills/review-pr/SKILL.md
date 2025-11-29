---
name: review-pr
description: Review SUEWS pull requests comprehensively. Use when reviewing PRs, checking code before merge, or when asked to review changes. Performs scientific validity assessment for physics modules, code style checking (Fortran and Python conventions), test coverage analysis, documentation verification, and merge readiness evaluation. Posts line-specific comments via GitHub API and provides concise summary. Coordinates with lint-code, sync-docs, and verify-build skills.
---

# SUEWS PR Review

Comprehensive PR review with GitHub integration. Drafts comments for human approval before posting.

## Quick Start

```bash
# Get PR info
gh pr view {pr} --json number,title,files,commits
gh pr diff {pr} --name-only
```

---

## Step 1: PR Context

Gather PR information:

```bash
# Changed files
gh pr diff {pr} --name-only

# Full diff
gh pr diff {pr}

# Structured file info with line counts
gh api repos/UMEP-dev/SUEWS/pulls/{pr}/files --jq '.[] | {filename, status, additions, deletions}'
```

Classify changes:
- **Physics** (`src/suews/src/suews_phys_*.f95`) → requires scientific review
- **Utility** (`src/suews/src/suews_util_*.f95`) → code review only
- **Control** (`src/suews/src/suews_ctrl_*.f95`) → code review only
- **Python** (`src/supy/*.py`) → code review only

---

## Step 2: Code Style

Apply lint-code skill checks. Reference: `dev-ref/CODING_GUIDELINES.md`

### Fortran

| Check | Pattern | Example Issue |
|-------|---------|---------------|
| File naming | `suews_<cat>_<name>.f95` | `snow.f95` → `suews_phys_snow.f95` |
| Module naming | `module_<cat>_<name>` | `Snow_Module` → `module_phys_snow` |
| IMPLICIT NONE | Required in all modules | Missing `IMPLICIT NONE` |
| Type naming | `TYPE :: dts_<name>` | `snowstate` → `dts_snow_state` |
| Precision | `KIND(1D0)` or `REAL64` | Bare `REAL` usage |
| Units | `! [K]`, `! [W m-2]` | Missing unit annotation |

### Python

| Check | Pattern | Example Issue |
|-------|---------|---------------|
| DataFrame prefix | `df_` | `forcing` → `df_forcing` |
| Dict prefix | `dict_` | `state` → `dict_state` |
| Path prefix | `path_` | `file` → `path_file` |
| Logging | `logger_supy` | Using `print()` |
| Paths | `pathlib.Path` | Using `os.path` |
| Type hints | Required for public functions | Missing hints |
| Docstrings | NumPy style | Google style |

---

## Step 3: Scientific Review

**Only for physics changes** (`suews_phys_*.f95`).

### Module Identification

Map files to module labels from `dev-ref/REVIEW_PROCESS.md`:

| File Pattern | Module Label | Reviewers |
|--------------|--------------|-----------|
| `suews_phys_stebbs` | `module:stebbs` | @yiqing1021, @denisehertwig |
| `suews_phys_rslprof` | `module:rslprof` | @vitorlavor, @suegrimmond |
| `suews_phys_spartacus` | `module:spartacus` | @suegrimmond, @yiqing1021 |
| `suews_phys_snow` | `module:snow` | @havum, @ljarvi |
| `suews_phys_ehc` | `module:ehc` | @sunt05 |
| `suews_phys_anohm` | `module:anohm` | @sunt05 |
| General | Overall | @sunt05, @MatthewPaskin |

### Validation Checks

1. **Equations**: Verify against literature/documentation
2. **Units**: Check dimensional consistency
3. **Boundary conditions**: Verify edge case handling
4. **Conservation**: Energy balance `Rn = QH + QE + QS + QF`

### AI-Assisted Changes

Flag for extra scrutiny:
- Verify physical reasoning, not just code correctness
- Check for subtle errors in equations
- Ensure consistency with SUEWS physics

---

## Step 4: Testing Review

Reference: `dev-ref/testing/TESTING_GUIDELINES.md`

| Requirement | Target |
|-------------|--------|
| New code coverage | ≥80% |
| Critical paths | 95-100% |
| Physics validation | Required for physics changes |

Check for:
- FIRST principles (Fast, Independent, Repeatable, Self-validating, Timely)
- AAA pattern (Arrange-Act-Assert)
- Physics tests verify conservation laws
- Tolerance-based assertions (`np.allclose()`)

---

## Step 5: Documentation Review

| Check | Requirement |
|-------|-------------|
| CHANGELOG | Entry with correct category |
| PR description | Scientific rationale (if physics) |
| User docs | Updated if user-facing |

CHANGELOG categories: `[feature]`, `[bugfix]`, `[change]`, `[maintenance]`, `[doc]`

---

## Step 6: Build Review

```bash
# Check CI status
gh pr checks {pr}

# Verify meson.build includes new files
# New .f95 files must be in src/suews/meson.build
# New .py files must be in appropriate __init__.py
```

---

## Step 7: Draft Comments

Present drafted comments to human reviewer:

```
=== DRAFT LINE COMMENTS ===

1. src/suews/src/suews_phys_snow.f95:42
   > Fortran: Use `REAL(KIND(1D0))` for consistent precision

2. src/supy/_load.py:78
   > Python: Rename to `df_output` (DataFrame prefix convention)

=== DRAFT SUMMARY ===

## PR Review Summary

**Status**: Needs attention

| Category | Status |
|----------|--------|
| Code Style | FAIL |
| Scientific | PASS |
| Testing | PASS |
| Documentation | PASS |

### Key Findings
- 2 code style issues (see line comments)

### Suggested Reviewers
@sunt05 (module:snow)

---
*2 line comments posted for specific issues*
```

---

## Step 8: Human Approval

Ask reviewer:

> Review the drafted comments above. Reply with:
> - `approve` - post all comments as drafted
> - `approve with edits` - provide edited versions
> - `skip line comments` - post only summary
> - `cancel` - don't post any comments

**Wait for explicit approval before posting.**

---

## Step 9: Post Comments

Only after approval:

**Line comments:**
```bash
gh api repos/UMEP-dev/SUEWS/pulls/{pr}/comments \
  -f body="Fortran: Use \`REAL(KIND(1D0))\` for consistent precision" \
  -f commit_id="$COMMIT_SHA" \
  -f path="src/suews/src/suews_phys_snow.f95" \
  -f line=42 \
  -f side="RIGHT"
```

**Summary comment:**
```bash
gh api repos/UMEP-dev/SUEWS/issues/{pr}/comments \
  -f body="$SUMMARY_MARKDOWN"
```

Report: "Posted N line comments and 1 summary comment to PR #X"

---

## References

- Detailed checklist: `references/review-checklist.md`
- Coding guidelines: `dev-ref/CODING_GUIDELINES.md`
- Fortran conventions: `dev-ref/FORTRAN_NAMING_CONVENTIONS.md`
- Review process: `dev-ref/REVIEW_PROCESS.md`
- Testing guidelines: `dev-ref/testing/TESTING_GUIDELINES.md`
