# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## ⚠️ CLAUDE.md Protection Active

This file is protected against accidental truncation or content loss:
- **Automatic validation** on every Git commit (pre-commit hook installed)
- **GitHub Actions** validates on push/PR
- **Backup system** maintains CLAUDE.md.backup
- **Snapshots** saved when issues detected (.claude/snapshots/)

**For initial setup or re-installation:** Run `bash .claude/scripts/setup-claude-protection.sh`
**To validate manually:** Run `python3 .claude/scripts/validate-claude-md.py`

## Style Guidelines

- **Language**: Use British English for all documentation, code comments, and communication

## Documentation Structure
- **Developer reference**: `dev-ref/` - Coding guidelines, testing patterns, interfaces
- **Testing patterns**: `dev-ref/testing/` - Test design, error handling, CI tiers
- **User documentation**: `docs/` - Sphinx-generated user-facing documentation

## Git Worktrees for Claude Code

This repository uses nested git worktrees to enable parallel development with Claude Code. All worktrees are located under `worktrees/` directory for Claude Code accessibility.

### Worktree Structure
```
SUEWS/
├── worktrees/              # All worktrees nested here (in .gitignore)
│   ├── core-bugs/         # feature/core-runtime-fixes
│   ├── enhancements/      # feature/infrastructure-enhancements
│   ├── fast-dev-build/    # feature/fast-dev-build
│   └── ...
└── .claude/              # Claude Code workspace
```

### Working with Worktrees

**Quick Start:** See `.claude/howto/setup-worktree.md` for complete worktree setup instructions.

**Key Resources:**
- `.claude/howto/setup-worktree.md` - Complete setup guide
- `.claude/howto/setup-environment.md` - Environment options comparison
- `.claude/reference/uv-adoption.md` - UV details and Python 3.13 notes
- `.claude/reference/core-requirements.txt` - Package list
- **Quick setup with uv**: `make setup && source .venv/bin/activate && make dev`
- **Quick setup with mamba**: `mamba activate suews-dev && make dev`

#### Legacy Mamba Setup

For mamba-based setup, see `.claude/reference/environment-types.md`.

### Best Practices
- Always create worktrees under `worktrees/` directory
- Use descriptive names matching the feature
- **Use uv for speed** - setup takes seconds, not minutes
- **Currently: activate environment** due to Python 3.13 compatibility
- See cleanup commands in `.claude/howto/setup-worktree.md`
- Use correct pip package names: `matplotlib` (not matplotlib-base), `tables` (not pytables)

### Build System and Testing

**CRITICAL**: Each worktree MUST use a separate Python environment.

**Recommended quick setup**: 
- With uv: `make setup && source .venv/bin/activate && make dev`
- With mamba: `mamba activate suews-dev && make dev`

For complete build and testing information, see:
- `.claude/reference/build-isolation.md` - Why isolation is required
- `.claude/howto/setup-worktree.md` - Setup instructions
- `.claude/howto/setup-environment.md` - Environment options

### Current Development Status
- Active branches and development work are tracked via GitHub issues and pull requests
- For parallel development instructions, see `.claude/howto/parallel-development.md`

### Claude Code Resources
- `.claude/README.md` - Overview of the .claude directory structure and purpose
- `.claude/howto/` - Step-by-step guides for common tasks
- `.claude/howto/beta-testing.md` - Guide for testing development versions
- `.claude/reference/` - Technical documentation and analysis
- `.claude/templates/` - Reusable templates for consistency

## Worktree Context Management

### Branch-Specific Development
When working in a git worktree or on a specific feature branch:

1. **First, identify current branch:**
   ```bash
   git branch --show-current
   ```

2. **Check the corresponding GitHub issue or PR** for context and requirements

3. **Follow standard development practices** as outlined in the documentation

### IMPORTANT: Updating Plans During Work

For detailed instructions on working with plans in worktrees, see:
- `.claude/howto/worktree-workflow.md` - Complete workflow guide
- `.claude/howto/parallel-development.md` - Multi-agent considerations

### Development and Testing Workflow

See `.claude/reference/build-isolation.md` for complete testing and build workflow.



## Git and GitHub Tips

- **IMPORTANT**: Always use `origin` as the only git remote for this repository
- When using gh cli, first check remotes with `git remote -v`
- If multiple remotes exist, remove all except `origin`:
  ```bash
  git remote remove upstream
  git remote remove Urban-Meteorology-Reading
  # Keep only: origin -> git@github.com:UMEP-dev/SUEWS.git
  ```

## Style and Language Guidelines

- Any human writing in this project should use British English - docs/code annotations etc

## Testing Resources

### Benchmark Test Files
- For testing: configuration file `p_config = Path("test/benchmark1/benchmark1.yml")` 
- For testing: forcing data file `p_forcing = Path("test/benchmark1/forcing/Kc1_2011_data_5.txt")`

### Critical Testing Requirements for SUEWS

**IMPORTANT**: Before committing any changes, ALWAYS run the full test suite:

```bash
# Run all tests (as per Makefile)
make test
# This executes: python -m pytest test -v --tb=short
```

The test suite includes several critical tests:
- **Benchmark Test** (`test_benchmark1_same`): Validates SUEWS model outputs against known good results
- **Precheck Tests**: Validate input data preprocessing and validation logic
- **Data Model Tests**: Ensure data structures work correctly
- **Conditional Validation Tests**: Check physics option compatibility

### Benchmark Test Details

The benchmark test (`test_supy.py::TestSuPy::test_benchmark1_same`) is particularly critical as it:
- Loads configuration from `test/benchmark1/benchmark1.yml`
- Runs a full year SUEWS simulation with real forcing data
- Compares outputs against pre-computed results (`benchmark1.pkl`)
- Validates key physics variables within 0.8% tolerance:
  - QN (Net all-wave radiation)
  - QF (Anthropogenic heat flux)
  - QS (Storage heat flux)
  - QE (Latent heat flux)
  - QH (Sensible heat flux)
  - T2 (2m air temperature)
  - RH2 (2m relative humidity)
  - U10 (10m wind speed)

If the benchmark test fails after your changes:
1. Check if changes affect model physics calculations
2. Verify data structures and field mappings remain compatible
3. Review any modifications to the Fortran-Python interface
4. Ensure all required model physics options are properly initialised
5. Check that field name changes are consistently applied everywhere

Common locations to debug benchmark failures:
- `src/supy/_run.py` - Model execution logic
- `src/supy/data_model/` - Data structures and validation
- `src/supy/_load.py` - Data loading and preprocessing
- `test/benchmark1/` - Benchmark configuration and expected results

## Documentation Guidelines

- Remember the yaml rst files are generated - so modify the `generate_datamodel_rst.py` script rather than the rst files if edits are needed

## Development Tasks and Reminders

- **Remember to check if a .venv with editable supy has already been up - if so, dont rebuild but carry on with using/fixing/debugging**
- **Remember to include new files in meson.build appropriately**
- **IMPORTANT**: When creating new source files that are part of the build, always update the corresponding meson.build file:
  - Python files (.py) in src/supy/
  - Fortran files (.f90, .f95) in src/suews/src/
  - Any other source files that need to be compiled or installed
- **Every time after conversation compaction, remember to source .venv to use python if such venv exists**
- **Every time after new conversion, remember to source .venv to use python if such venv exists**

## Configuration Handling and Method Design Pattern

### Principle: Separation of Concerns Between Configuration and Implementation

When working with configuration objects and implementation methods in SUEWS/SuPy, follow this strict separation:

1. **High-Level Classes (e.g., SUEWSSimulation)**: 
   - **DO**: Parse and interpret configuration objects
   - **DO**: Extract specific values from nested config structures
   - **DO**: Handle RefValue wrappers and type conversions
   - **DO**: Transform config data into concrete parameters
   - **DON'T**: Pass configuration objects to lower-level methods

2. **Low-Level Methods (e.g., save_supy, run_supy)**:
   - **DO**: Accept explicit, typed parameters (int, str, float, etc.)
   - **DO**: Focus on the core functionality without config knowledge
   - **DON'T**: Accept configuration objects as parameters
   - **DON'T**: Import or depend on configuration classes

### Example Pattern:

**WRONG Approach:**
```python
# High-level class passing config directly
def save(self, output_path, format=None):
    if format == "txt":
        # DON'T do this - passing config object to low-level method
        save_supy(df_output, df_state, output_config=self._config.output)
```

**CORRECT Approach:**
```python
# High-level class extracting and transforming config
def save(self, output_path, format=None):
    if format == "txt":
        # Extract specific parameters from config
        freq_s = 3600  # default
        if self._config and hasattr(self._config.output, 'freq'):
            freq_s = self._config.output.freq.value  # Handle RefValue
        
        # Pass concrete parameters to low-level method
        save_supy(df_output, df_state, freq_s=int(freq_s), site=site_name)
```

### Rationale:

1. **Reusability**: Low-level methods remain usable without config objects
2. **Testing**: Easier to test methods with explicit parameters
3. **Clarity**: Clear contracts - methods declare exactly what they need
4. **Flexibility**: Config structure can change without affecting core methods
5. **Backwards Compatibility**: Existing code using explicit parameters continues to work

### Implementation Checklist:

When implementing a feature that uses configuration:

- [ ] Identify what concrete parameters the low-level method needs
- [ ] Extract these values in the high-level class
- [ ] Handle RefValue wrappers (check for `.value` attribute)
- [ ] Convert types as needed (e.g., ensure integers for numeric parameters)
- [ ] Pass only primitive types or simple objects to low-level methods
- [ ] Keep configuration parsing logic in one place (the high-level class)

This pattern ensures clean architecture and maintains the intended separation between configuration management and core functionality.

## Current Investigations and Findings

### QE/QH Discrepancy Investigation (Branch: matthewp/testing_sample_data) - ✅ **RESOLVED**

**Issue**: Tests pass individually but fail when run in full test suite, specifically `test_sample_output_validation` with QE/QH mismatches.

**Root Cause Identified**: Uninitialized Fortran variables in derived types cause state pollution between test runs, compounded by exact floating-point equality checks.

#### Key Findings:
1. **Critical Code Location**: `src/suews/src/suews_phys_atmmoiststab.f95` lines 243 and 288
   ```fortran
   IF (H == 0.) THEN
   ```
   These exact equality checks behave differently with compiler optimizations.

2. **Compiler Testing Results**:
   - Fast build (`-O1`): Fails sample output test in full suite without pytest-order
   - Slow build (`-O0 -fcheck=all`): Also fails sample output test in full suite without pytest-order
   - **Both configurations affected by state leakage, but severity varies**

3. **Causal Chain**:
   - Atmospheric stability calculations use exact floating-point equality
   - Different compiler optimizations affect floating-point behaviour
   - This cascades through resistance calculations into QE/QH computations
   - Results in different model outputs depending on test execution order

#### Technical Details:
- **Affected Module**: `suews_phys_atmmoiststab.f95` - Atmospheric stability calculations
- **Impact**: QE (Latent Heat Flux) and QH (Sensible Heat Flux) calculations
- **Masking Factor**: pytest-order was controlling test execution order
- **Build Configuration**: Both fast and slow builds affected when pytest-order removed

#### Files Investigated:
- `src/suews/src/suews_phys_atmmoiststab.f95` - Contains problematic exact equality checks
- `src/suews/src/suews_phys_resist.f95` - Aerodynamic resistance calculations
- `src/suews/src/suews_phys_evap.f95` - Evaporation calculations
- `src/supy_driver/meson.build` - Compiler flag configuration
- `meson_options.txt` - Fast build option definition
- `test/test_sample_output.py` - Failing test case

#### Recommendations:
1. **Replace exact equality checks** with epsilon-based comparisons
2. **Ensure proper Fortran variable initialization** to prevent state leakage
3. **Add comprehensive tests** for floating-point stability
4. **Implement state isolation** between test runs

#### Status: Investigation Complete - ✅ **FULLY RESOLVED**
- [x] Identified root cause of QE/QH discrepancies
- [x] Tested both compiler configurations
- [x] Confirmed state leakage affects both build types
- [x] Documented exact code locations needing fixes
- [x] Created comprehensive test suite for floating-point stability
- [x] Implemented fixes for exact equality checks
- [x] Added general floating-point epsilon constant (`eps_fp = 1.0E-12`)
- [x] Fixed both problematic `IF (H == 0.)` checks in `suews_phys_atmmoiststab.f95`
- [x] **COMPLETE RESOLUTION**: Initialized all atmospheric state variables in `atm_state` type
- [x] **FULL TEST SUITE PASSES**: All tests now pass individually and in full suite
- [x] **EXECUTION ORDER INDEPENDENCE**: Results identical regardless of test execution order

#### Fix Details:
**Location**: `/src/suews/src/suews_ctrl_const.f95` - Added to `PhysConstants` module:
```fortran
REAL(KIND(1D0)), PARAMETER :: eps_fp = 1.0E-12 !Epsilon for floating-point near-zero comparisons
```

**Fixes Applied**: `/src/suews/src/suews_phys_atmmoiststab.f95` - Changed exact equality checks:
```fortran
! Before: IF (H == 0.) THEN
! After:  IF (ABS(H) <= eps_fp) THEN
```

**Final Solution**: `/src/suews/src/suews_ctrl_type.f95` - Initialized all atmospheric state variables:
```fortran
! Critical atmospheric state variables now initialized:
REAL(KIND(1D0)) :: L_mod = 0.0D0 !Obukhov length [m]
REAL(KIND(1D0)) :: zL = 0.0D0 ! Stability scale [-]
REAL(KIND(1D0)) :: RA_h = 0.0D0 ! aerodynamic resistance [s m-1]
REAL(KIND(1D0)) :: RS = 0.0D0 ! surface resistance [s m-1]
REAL(KIND(1D0)) :: UStar = 0.0D0 !friction velocity [m s-1]
REAL(KIND(1D0)) :: TStar = 0.0D0 !T*, temperature scale [-]
REAL(KIND(1D0)) :: RB = 0.0D0 !boundary layer resistance shuttleworth
REAL(KIND(1D0)) :: rss_surf = 0.0D0 ! surface resistance [s m-1]
! ... and ALL other atmospheric variables
```

#### Results:
- ✅ **COMPLETE SUCCESS**: Fixed all state leakage issues
- ✅ **PERFECT RESOLUTION**: QE/QH difference reduced to 0.000000 (complete elimination)
- ✅ Individual `test_sample_output_validation` passes
- ✅ **FULL TEST SUITE PASSES**: All tests pass in any execution order
- ✅ Floating-point stability tests all pass
- ✅ **EXECUTION ORDER INDEPENDENCE**: Results identical regardless of test sequence
- ✅ **ISSUE COMPLETELY RESOLVED**: No remaining state pollution detected

#### Comparison: Before vs After Complete Fix
**Before Fix (Original Code)**:
- QE failures: 288 points, max relative diff: **37.87%**
- QH failures: Similar magnitude
- Failed at indices around 286-335 (first day of data)
- Tests fail in full suite, pass individually

**After Complete Fix (Epsilon + Variable Initialization)**:
- QE failures: **0 points**, max relative diff: **0.000000%**
- QH failures: **0 points**, max relative diff: **0.000000%**
- **NO FAILURES**: All tests pass in any execution order
- **IDENTICAL RESULTS**: First run = Second run = Nth run

**Impact**: Complete elimination of QE/QH discrepancies and state pollution

#### ✅ **COMPLETE SOLUTION IMPLEMENTED**:
1. **✅ FIXED**: Exact equality checks replaced with epsilon-based comparisons
2. **✅ RESOLVED**: All atmospheric state variables initialized in `atm_state` type
3. **✅ SYSTEMATIC SOLUTION**: GitHub Issue #504 created for comprehensive variable initialization
4. **✅ PREVENTION**: Coding standards established for floating-point comparisons
5. **✅ VALIDATION**: All tests pass across both compiler configurations

#### **Systematic Fix in Progress** (GitHub Issue #504):
- **492 REAL variables** identified without initialization across 34 types
- **59 INTEGER variables** identified without initialization  
- **Priority types**: HEAT_STATE, HYDRO_STATE, STEBBS_STATE, ROUGHNESS_STATE
- **Implementation plan**: 4-phase approach over 4 weeks
- **GitHub Issue**: https://github.com/UMEP-dev/SUEWS/issues/504

#### New Test Suite Added:
1. **test_fortran_stability.py** - Comprehensive floating-point stability tests
   - Tests atmospheric stability calculations with exact equality edge cases
   - Validates compiler optimization consistency
   - Tests state isolation between runs
   - Covers atmospheric stability edge cases (stable, unstable, neutral)
   - Tests QE/QH consistency across execution scenarios

2. **test_exact_equality_fix.py** - Specific tests for the exact equality fix
   - Tests boundary conditions around H = 0 that trigger exact equality
   - Validates compiler independence near zero values
   - Tests epsilon vs exact equality behaviour
   - Regression testing against known good values
   - Multiple execution consistency validation

3. **test_execution_order_independence.py** - Tests for execution order independence
   - Tests identical runs in different execution orders
   - Recreates the sample_output_validation failure scenario
   - Tests random execution order scenarios
   - Simulates pytest execution order issues
   - Specifically validates QE/QH order independence

4. **test_floating_point_stability.py** - Working test suite for immediate use
   - Tests repeated runs produce identical results
   - Validates execution order independence
   - Tests low wind conditions that trigger problematic code paths
   - Validates compiler consistency across optimization levels
   - Tests zero boundary conditions and atmospheric stability transitions
   - Includes state isolation testing after simulation failures

#### Test Suite Status:
- [x] All tests use correct supy API (sp.load_SampleData(), sp.run_supy())
- [x] Tests access QE/QH via result.SUEWS['QE'] and result.SUEWS['QH']
- [x] Tests validated and working correctly
- [x] Tests specifically target the identified issues in the investigation

#### **Lessons Learned & Best Practices**:

1. **Variable Initialization is Critical**: 
   - Always initialize ALL Fortran variables in derived types
   - Use `= 0.0D0` for REAL variables, `= 0` for INTEGER variables
   - Don't rely on compiler-dependent default initialization

2. **Avoid Exact Floating-Point Equality**: 
   - Replace `IF (variable == 0.)` with `IF (ABS(variable) <= eps_fp)`
   - Use epsilon-based comparisons for all floating-point operations
   - Define a consistent epsilon constant (`eps_fp = 1.0E-12`)

3. **Test Execution Order Independence**:
   - Always test both individual tests AND full test suite
   - Create specific tests for execution order independence
   - Use floating-point stability tests to catch state pollution

4. **Systematic Debugging Approach**:
   - Start with simple reproduction cases
   - Use manual test scripts to isolate issues
   - Investigate compiler-dependent behavior
   - Document findings thoroughly for future reference

5. **State Pollution Prevention**:
   - Never assume variables are initialized to zero
   - Always provide explicit default values in type definitions
   - Test for state leakage between function calls
   - Use comprehensive test suites to catch edge cases

## Documentation & Code Maintenance Principles


### Documentation Updates for Code Changes

When making code changes to SUEWS/SuPy:
- **Always update relevant documentation** in `docs/` directory when functionality changes
- Update Sphinx documentation for user-facing changes
- **Documentation generation scripts** (run ONLY when specific changes occur):
  - Run `python docs/generate_datamodel_rst.py` - ONLY when Pydantic data model structure changes (adding/removing fields, changing types)
  - This script is NOT run for routine CHANGELOG updates or validator migrations
- Ensure examples and tutorials reflect current API
- Update parameter tables and input file documentation as needed
- **Update CHANGELOG.md promptly** for remarkable changes using categories:
  - [feature]: New features
  - [bugfix]: Bug fixes (also create GitHub issue)
  - [change]: User-facing changes
  - [maintenance]: Codebase maintenance (including Claude Code development aspects AND updates to CLAUDE.md)
  - [doc]: Documentation updates (user-facing documentation in docs/, NOT CLAUDE.md)
- **IMPORTANT**: Updates to CLAUDE.md should be categorised as [maintenance], not [doc]
- **CRITICAL CHANGELOG.md RULES**:
  - **DO NOT** modify or regenerate the Annual Statistics table (if present) - it causes merge conflicts
  - **DO NOT** run `.claude/scripts/changelog_helper.py` unless explicitly requested via `/log-changes` slash command
  - Only add new entries under the appropriate date heading
  - Keep existing entries and structure intact
  - Simply append new entries without restructuring

### RST (reStructuredText) Writing Rules

**CRITICAL: RST markup cannot be nested or overlayed!**

- **WRONG**: `**:doc:`link text <target>`**` - Cannot combine bold with doc reference
- **WRONG**: `*:option:`parameter`*` - Cannot combine italic with option reference  
- **CORRECT**: Use markup separately: `:doc:`link text <target>` or make text bold separately from the link

Common RST pitfalls to avoid:
1. No nested inline markup (no bold inside links, no links inside emphasis, etc.)
2. Inline markup must be separated by whitespace or punctuation from surrounding text
3. Use backslashes to escape special characters when needed
4. Remember that `**text**` is bold, `*text*` is italic, and they cannot contain other markup

When generating RST programmatically:
- Keep `:doc:`, `:ref:`, `:option:` and other role references standalone
- Apply text formatting (bold, italic) to separate text elements only
- For emphasis, structure the document layout rather than relying on nested formatting

### Documentation Principles

1. **Single Source of Truth (DRY)**
   - Every piece of information should exist in exactly ONE place
   - Example: Package lists in one file, referenced everywhere else

2. **Reference Over Duplication**
   - Use `See: path/to/doc.md` instead of copying content
   - Example: `For complete setup, see .claude/howto/setup-worktree.md`

3. **Clear Documentation Hierarchy**
   ```
   .claude/
   ├── howto/          # Step-by-step guides (practical)
   ├── reference/      # Technical details & specifications
   ├── templates/      # Reusable templates
   ```

4. **Focused Documents**
   - Each file should have ONE clear purpose
   - Example: `build-isolation.md` only explains why isolation is needed

5. **Brief Overview Pattern**
   - Main files (like CLAUDE.md) should be concise overviews
   - Details go in sub-documents with clear references

6. **Centralize Common Lists**
   - Package lists, commands, requirements → single file
   - Example: `core-requirements.txt` instead of inline lists everywhere

### Code Principles

7. **Single Responsibility**
   - Each function/class does ONE thing well
   - Example: `save_supy()` only saves, doesn't validate or transform

8. **Explicit Over Implicit**
   ```python
   # Good: Clear what parameters are needed
   save_supy(df_output, df_state, freq_s=3600, site="London")
   
   # Bad: Hidden configuration dependencies
   save_supy(df_output, df_state, config_obj)
   ```

9. **Extract Common Patterns**
   - Common operations in utility functions
   - Example: Validation logic in `validation_utils.py`

10. **Configuration Over Code Duplication**
    ```python
    # Define once, use everywhere
    PACKAGE_MAPPING = {
        'matplotlib-base': 'matplotlib',  # mamba → pip name
        'pytables': 'tables'
    }
    ```

11. **Composition Over Complex Inheritance**
    ```python
    # Flexible and testable
    class Model:
        def __init__(self, validator, processor, saver):
            self.validator = validator
            self.processor = processor
            self.saver = saver
    ```

12. **Version/Platform Isolation**
    ```python
    # compat.py - isolate compatibility code
    if sys.version_info >= (3, 13):
        from new_module import feature
    else:
        from old_module import feature
    ```

### Maintenance Best Practices

- **Important information first**: Style guidelines, critical warnings at the top
- **Progressive disclosure**: Quick start → Details → Troubleshooting
- **Cross-reference related content**: "See also:" sections for navigation
- **Use templates for repetitive patterns**: Avoid explaining the same structure multiple times
- **Document package name differences ONCE**: Create mappings, reference everywhere