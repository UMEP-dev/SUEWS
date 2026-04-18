# SUEWS PR Review Checklist

Detailed checklist for comprehensive PR review.

---

## Code Style Checklist

### Fortran (Critical)

- [ ] File naming: `suews_<category>_<name>.f95`
  - Categories: `ctrl`, `phys`, `util`
- [ ] Module naming: `module_<category>_<name>`
- [ ] `IMPLICIT NONE` present in all modules
- [ ] Type naming: `TYPE :: dts_<name>`
- [ ] Subroutine/function naming: `snake_case`
- [ ] Constants: `REAL(KIND(1D0)), PARAMETER :: UPPERCASE`
- [ ] Variables: `lowercase_with_underscores`
- [ ] Unit annotations: `! [K]`, `! [W m-2]`, `! [-]`
- [ ] Precision: `KIND(1D0)` or `REAL64`, never bare `REAL`
- [ ] No exact floating-point equality comparisons

### Python (Critical)

- [ ] Config separation: No config objects in low-level functions
- [ ] Deep copy: `copy.deepcopy()` for mutable state
- [ ] Variable prefixes:
  - `df_` for DataFrames
  - `dict_` for dictionaries
  - `list_` for lists
  - `ser_` for Series
  - `path_` for Paths
- [ ] Logging: `logger_supy` not `print()`
- [ ] Path handling: `pathlib.Path` not `os.path`
- [ ] Type hints: Complete for public functions
- [ ] Docstrings: NumPy style (not Google)
- [ ] British English spelling

### Exceptions

- CLI files (`cmd/`) may use `print()`
- Test files have relaxed requirements
- Generated files excluded (`_suews_driver.py`, `_version.py`)

---

## Scientific Review Checklist

### Physics Validation

- [ ] Equations match literature/documentation
- [ ] Units are dimensionally consistent
- [ ] Boundary conditions handled correctly
- [ ] Conservation principles verified:
  - Energy: `Rn = QH + QE + QS + QF`
  - Water balance maintained
- [ ] Numerical stability considered
- [ ] Edge cases handled

### AI-Assisted Changes

- [ ] Physical reasoning verified (not just code correctness)
- [ ] Equations checked for subtle errors
- [ ] Consistency with SUEWS physics confirmed
- [ ] Human scientific review completed

---

## Testing Checklist

### Coverage Requirements

- [ ] New code: ≥80% coverage
- [ ] Critical paths: 95-100% coverage
- [ ] Physics changes: Validation tests present

### Test Quality

- [ ] FIRST principles followed:
  - **F**ast - runs quickly
  - **I**ndependent - no test dependencies
  - **R**epeatable - deterministic results
  - **S**elf-validating - clear pass/fail
  - **T**imely - written with code
- [ ] AAA pattern used (Arrange-Act-Assert)
- [ ] Descriptive test names
- [ ] Physics references documented in docstrings

### Scientific Testing

- [ ] Energy balance verification
- [ ] Water conservation checks
- [ ] Tolerance-based assertions (`np.allclose()`)
- [ ] Benchmark comparisons where applicable

---

## Scientific PR Dev-Note Demo Checklist

Applies to any PR that introduces or materially changes a scientific
component (physics, numerics, calibration, benchmark). Missing artefacts
are a **blocking** review finding — flag and request the author add them
before approving merge, unless a maintainer explicitly agrees the change
is not scientific.

### Triggers — a PR is "scientific" if it touches

- [ ] Any file matching `src/suews/src/suews_phys_*.f95`
- [ ] `src/suews/src/suews_ctrl_daily_state.f95`
- [ ] `src/supy/data_model/` (new physics-facing fields, enums, validators)
- [ ] New YAML parameters that alter model output
- [ ] A CHANGELOG entry tagged `[feature]` or `[change]` that is not pure
      tooling / build / docs
- [ ] Benchmark additions or regressions against published reference runs

### Required artefacts

- [ ] Summary RST at `docs/source/development/<slug>.rst`
- [ ] Dashboard HTML at `docs/source/_extra/dev-notes/<slug>/dashboard.html`
- [ ] Figures sit alongside the dashboard HTML (self-contained by default),
      referenced via plain relative paths such as `src="fig.png"`
- [ ] `development/index.rst` toctree updated to include the new page

### Only when the dev-note ships with genuinely heavy assets

A GitHub Release is only required when individual figures exceed ~5 MB
or when the dev-note ships with videos, NetCDF, or large PDFs. In that
case additionally:

- [ ] Heavy assets uploaded to a GitHub Release tagged `dev-notes-<slug>`
- [ ] Committed HTML references those assets via release URLs of the form
      `https://github.com/UMEP-dev/SUEWS/releases/download/dev-notes-<slug>/<file>`
- [ ] Spot-check 1–2 release URLs return HTTP 200 (release uploaded, not
      just created as a draft)

For the common case (small figures), none of the release-related items
apply and the dev-note is fully self-contained.

### Content expectations for the dashboard

- [ ] Scope — which issue / which PRs this archives
- [ ] Evidence — at least one sensitivity sweep OR before/after comparison
      OR calibration table that substantiates the scientific claim
- [ ] Governing equations or algorithmic description where applicable
- [ ] Test-run status at branch-head (pass count, tier)
- [ ] Known follow-ups or limitations flagged explicitly

### How to handle a missing dev-note

When a scientific PR arrives without a dev-note:

1. Flag the omission in the review summary with "Dev-note archive: FAIL"
2. Post a blocking review comment pointing the author at
   `scripts/suews/build_dev_note.py` and the
   `docs/source/development/index.rst` index for prior examples
3. Do not approve the PR for merge until the dev-note lands, unless a
   maintainer explicitly confirms the change is non-scientific
4. If the scope is ambiguous, ask the author to classify in the PR
   description and have a maintainer confirm rather than guess

---

## Documentation Checklist

### CHANGELOG

- [ ] Entry present under correct date
- [ ] Correct category used:
  - `[feature]` - New functionality
  - `[bugfix]` - Bug fixes (link issue)
  - `[change]` - User-facing changes
  - `[maintenance]` - Internal/dev tooling
  - `[doc]` - Documentation only
- [ ] Concise description of change
- [ ] Issue/PR linked where applicable

### PR Description

- [ ] Clear summary of changes
- [ ] Scientific rationale (if physics changes)
- [ ] Testing approach described
- [ ] Breaking changes noted

### User Documentation

- [ ] Updated if user-facing changes
- [ ] RST/Markdown syntax correct
- [ ] Cross-references working
- [ ] Images in correct location

---

## Build Checklist

### Source Files

- [ ] New Fortran files in `src/suews/meson.build`
- [ ] New Python files in appropriate `__init__.py`

### CI Status

- [ ] All CI checks passing
- [ ] No regression in test coverage
- [ ] Documentation builds successfully

---

## Refactoring Review

When a PR is a refactoring (no intended behavioural change), apply these additional checks:

### Behavioural Preservation

- [ ] Test suite results match pre-refactoring baseline (same pass count, no new failures)
- [ ] No change in public API signatures (function names, parameters, return types)
- [ ] No change in output values for identical inputs (compare numerical results)
- [ ] No new dependencies introduced
- [ ] No change in error handling behaviour (same exceptions for same invalid inputs)

### Structural Assessment

- [ ] File renames tracked correctly (git detects as rename, not delete+create)
- [ ] Import paths updated consistently across codebase
- [ ] If files renamed: `meson.build` updated, documentation cross-references updated
- [ ] No dead code left behind (unused imports, unreachable branches)

### Red Flags

Flag for closer review if ANY of:
- Test count changed (tests added is OK, tests removed is a red flag)
- Numerical output differs even slightly (may indicate unintended logic change)
- New `# TODO` or `# FIXME` comments introduced
- Functions moved between modules (may break external imports)

---

## Merge Criteria

PR can be merged when ALL of:

- [ ] CI tests pass
- [ ] Code review approved
- [ ] Scientific review approved (if physics changes)
- [ ] Dev-note archived under `docs/source/development/` (if scientific change)
- [ ] Documentation updated
- [ ] CHANGELOG entry present
- [ ] No blocking issues

---

## Module → Reviewer Mapping

| Module Label | Files | Reviewers |
|--------------|-------|-----------|
| `module:stebbs` | `suews_phys_stebbs*` | @yiqing1021, @denisehertwig |
| `module:rslprof` | `suews_phys_rslprof*` | @vitorlavor, @suegrimmond |
| `module:spartacus` | `suews_phys_spartacus*` | @suegrimmond, @yiqing1021 |
| `module:biogenco2` | `suews_phys_biogen*` | @havum, @ljarvi |
| `module:snow` | `suews_phys_snow*` | @havum, @ljarvi |
| `module:ehc` | `suews_phys_ehc*` | @sunt05 |
| `module:anohm` | `suews_phys_anohm*` | @sunt05 |
| `module:ohm` | `suews_phys_ohm*` | Seeking reviewer |
| `module:estm` | `suews_phys_estm*` | Seeking reviewer |
| `module:lumps` | `suews_phys_lumps*` | Seeking reviewer |
| `module:narp` | `suews_phys_narp*` | Seeking reviewer |
| `module:evap` | `suews_phys_evap*` | Seeking reviewer |
| `module:waterdist` | `suews_phys_waterdist*` | Seeking reviewer |
| Overall | General PRs | @sunt05, @MatthewPaskin, @dayantur |
