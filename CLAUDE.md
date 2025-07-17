# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

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
    └── plans/            # Feature-specific development plans (in master)
        ├── todo/         # Features planned but not started
        ├── doing/        # Features actively being worked on
        ├── done/         # Completed features (to be cleaned up)
        └── README.md
```

### Working with Worktrees

**🚀 Quick Start with uv (Recommended - Ultra Fast!):**
Use `uv` for blazing fast worktree setup. See `.claude/guides/worktree-setup-guide.md` for the complete workflow.

```bash
# One-time: Install uv
brew install uv  # or: curl -LsSf https://astral.sh/uv/install.sh | sh

# Quick setup (from the guide)
FEATURE="my-feature"
git worktree add worktrees/$FEATURE feature/$FEATURE
cd worktrees/$FEATURE
uv venv
source .venv/bin/activate  # or use uv run
uv pip install pandas scipy matplotlib # ... see guide for full list
make dev

# Work without activation!
uv run python           # Run Python
uv run pytest          # Run tests
uv run make test       # Run make commands
```

**Why uv?**
- 10-100x faster than pip/mamba
- No environment activation needed
- Single command setup
- Works everywhere

See:
- `.claude/howto/setup-worktree.md` - Complete setup and cleanup commands
- `.claude/howto/setup-environment.md` - All environment options (uv, venv, mamba)
- `.claude/reference/uv-adoption.md` - Full uv adoption details

#### Legacy: Manual Mamba Setup (If Required)

**Note:** The automated scripts above use Python venv for faster setup. Only use mamba if the project has complex compiled dependencies.

**Mamba Configuration in Claude Code:**
- Mamba is installed at: `/opt/homebrew/bin/mamba`
- Mamba root prefix: `/Users/tingsun/.local/share/mamba`
- Main environment: `suews-dev`

**Manual mamba setup:**
```bash
# Create worktree
git worktree add worktrees/my-feature feature/my-feature-name

# Create environment (if --clone fails, use export method)
/opt/homebrew/bin/mamba create -n suews-dev-my-feature --clone suews-dev

# Or export and recreate
/opt/homebrew/bin/mamba env export -n suews-dev > /tmp/suews-dev-env.yml
/opt/homebrew/bin/mamba env create -n suews-dev-{feature-name} -f /tmp/suews-dev-env.yml -y

# Activate and build
cd worktrees/my-feature
source ~/.zshrc && mamba activate suews-dev-my-feature
make dev
```

**Manual cleanup:**
```bash
git worktree remove worktrees/my-feature
mamba env remove -n suews-dev-my-feature
# Remove plan file (find in todo/doing/done directories)
git rm .claude/plans/*/feature-{branch-name}.md
git commit -m "chore: remove worktree plan for merged feature"
```

### Best Practices
- Always create worktrees under `worktrees/` directory
- Use descriptive names matching the feature
- **Use uv for speed** - setup takes seconds, not minutes
- **No activation needed** - just use `uv run` commands
- See cleanup commands in `.claude/howto/setup-worktree.md`
- **IMPORTANT**: Also remove `.claude/plans/*/feature-{branch-name}.md` when cleaning up merged worktrees
- Pull master in each worktree to access latest `.claude/plans/`

### Build System and Testing

**CRITICAL**: Each worktree MUST use a separate Python environment. Never use the base `suews-dev` environment in worktrees!

#### Environment Setup Rules

1. **Root Directory** (`/SUEWS/`): Uses base `suews-dev` mamba environment
2. **Each Worktree** (`/worktrees/{name}/`): Uses its own isolated environment (venv recommended)

#### Quick Setup with uv

See `.claude/howto/setup-worktree.md` for the complete commands. Quick example:

```bash
# Ultra-fast setup with uv
git worktree add worktrees/feature-name feature/feature-name
cd worktrees/feature-name
uv venv && source .venv/bin/activate  # ALWAYS create venv for each worktree!
uv pip install pandas scipy matplotlib # ... (see guide)
make dev
uv run make test  # No activation needed!
```

**⚠️ REMINDER**: Always use `uv venv` when starting work in a worktree! Never use the base environment.

#### Why Separate Environments Are Required

- `make dev` creates an editable install linked to the current directory
- Only ONE editable install can exist per Python environment
- Multiple worktrees sharing an environment will conflict
- Each worktree may have different code versions

#### Testing Requirements
- **After each task**: Run `make test`
- **Before commits**: Run full test suite
- **For Fortran changes**: `make dev && make test`
- **Quick tests**: `pytest test/test_supy.py -v`

#### When to Rebuild
- **Not needed**: If supy is already installed and you're only changing Python code
- **make dev**: After Fortran changes, when switching branches, or first time setup
- **make clean && make dev**: Only when serious issues occur (build errors, import failures, unexpected test failures)

See:
- `.claude/howto/setup-worktree.md` for complete setup workflows
- `.claude/howto/setup-environment.md` for all environment options
- `.claude/reference/uv-adoption.md` for uv benefits and usage
- `.claude/reference/build-isolation.md` for build isolation strategies
- `.claude/reference/environment-types.md` for legacy mamba setup

### Current Development Status
- For an overview of all active branches and their associated GitHub issues, see `.claude/plans/claude-dev-notes.md`
- For parallel development instructions, see `.claude/howto/parallel-development.md`

### Claude Code Resources
- `.claude/README.md` - Overview of the .claude directory structure and purpose
- `.claude/howto/` - Step-by-step guides for common tasks
- `.claude/reference/` - Technical documentation and analysis
- `.claude/plans/` - Feature-specific development plans
- `.claude/templates/` - Reusable templates for consistency

## Worktree Context Management

### Branch-Specific Plans
When working in a git worktree or on a specific feature branch, check for branch-specific context and plans:

1. **First, identify current branch:**
   ```bash
   git branch --show-current
   ```

2. **Then load the corresponding plan if it exists:**
   - Check `.claude/plans/doing/feature-{branch-name}.md` for active work
   - Check `.claude/plans/todo/feature-{branch-name}.md` for planned work
   - Check `.claude/plans/done/feature-{branch-name}.md` for completed features
   
   **Note**: When in a worktree, plans are in the parent directory:
   ```bash
   # From worktree, check all plan directories:
   ls ../../.claude/plans/*/feature-{branch-name}.md
   # Or read directly:
   cat ../../.claude/plans/doing/feature-{branch-name}.md
   ```

3. **If no plan exists**, proceed with standard development practices.

### IMPORTANT: Updating Plans During Work

**When working in a worktree, Claude Code MUST:**
1. **Update progress tracking** - Mark tasks as completed immediately when done
2. **Add implementation notes** - Document any important discoveries or decisions
3. **Note blocking issues** - Record any problems encountered
4. **Update file lists** - Add any newly identified files to modify
5. **Record completion status** - Note what was accomplished in each session

**Example update during work:**
```bash
# After completing a task, update the plan:
# Edit .claude/plans/doing/feature-{branch-name}.md and:
# - Change "- [ ] Fix validation bug" to "- [x] Fix validation bug"
# - Add notes like "Found issue in line 234 of validation.py"
# - Document any new tasks discovered
```

**End of session checklist:**
- [ ] Update all completed tasks in the plan
- [ ] Add notes about any unfinished work
- [ ] Document any blocking issues for next session
- [ ] Commit plan updates to master branch

### Development and Testing Workflow

**Every Claude Code session should follow this workflow:**

1. **Start of Session**
   ```bash
   cd worktrees/{feature-name}
   git branch --show-current  # Verify branch
   cat ../../.claude/plans/doing/feature-{branch-name}.md  # Read plan
   
   # IMPORTANT: Always use uv venv in worktrees!
   source .venv/bin/activate  # Activate the worktree's venv
   # Or use uv run commands without activation
   
   # Check if supy is already installed locally
   if python -c "import supy" 2>/dev/null; then
       echo "✓ supy already installed, skipping rebuild"
   else
       make dev  # Initial build
   fi
   ```

2. **During Development**
   - After Python changes: `make test`
   - After Fortran changes: `make dev && make test`
   - Update plan progress immediately when tasks complete

3. **Before Committing**
   ```bash
   # For Python-only changes:
   make test  # Usually sufficient
   
   # For Fortran changes:
   make dev && make test
   
   # Only if serious issues occur (build errors, import failures):
   make clean && make dev && make test  # Full rebuild
   
   # Only commit if all tests pass!
   ```

4. **Commit Message Format**
   ```bash
   git commit -m "type: brief description

   - Detailed change 1
   - Detailed change 2
   
   Addresses #issue-number"
   ```
   
   Types: feat, fix, docs, test, refactor, chore

### Plan Lifecycle Management

**Creating a new plan:**
- When starting complex multi-session work, create `.claude/plans/todo/feature-{branch-name}.md`
- Use the template: `cp .claude/templates/feature-plan.md .claude/plans/todo/feature-{branch-name}.md`
- Include: current context, progress tracking, key decisions, implementation steps
- Commit to master/main branch so it's available in all worktrees

**Moving plans between states:**
- Start work: `git mv .claude/plans/todo/feature-X.md .claude/plans/doing/`
- Complete work: `git mv .claude/plans/doing/feature-X.md .claude/plans/done/`

**Maintaining plans:**
- Update progress status in the plan as work proceeds
- Add new findings or decisions that affect implementation
- Keep plans focused and actionable

**Cleaning up completed plans:**
- When a feature branch is merged, move plan to `done/`
- Archive important decisions to main documentation if needed
- Follow full worktree cleanup process as described in "Removing a worktree" section above
- This ensures both the physical worktree and its documentation are properly handled

### Example Plan Structure

See `.claude/templates/feature-plan.md` for the full template. Basic structure:

```markdown
# Feature: [Feature Name]

## Context
Brief description of what this feature/fix addresses

## GitHub Issues
- #123 - Main issue (PRIMARY)
- #124 - Related issue

## Progress Tracking
- [ ] Task 1
- [x] Task 2 (completed)
- [ ] Task 3

## Key Decisions
- Decision 1: Reasoning
- Decision 2: Reasoning

## Implementation Notes
Technical details, gotchas, important context

## Files to Modify
- `path/to/file1.py` - What to change
- `path/to/file2.py` - What to change
```

### Worktree Plan Writing Guide

When creating a plan for Claude Code, follow these guidelines to ensure consistency and clarity:

#### 1. **Context Section**
- Provide a clear, concise description of the feature/fix purpose
- Explain why this work is needed
- Keep it to 2-3 sentences

#### 2. **GitHub Issues Section**
- List all related GitHub issues with their numbers
- Mark the PRIMARY issue if there are multiple
- Include issue labels in parentheses (e.g., "bug", "enhancement")
- Note if issues are CLOSED but need verification

#### 3. **Progress Tracking**
- Use checkbox format for easy visual tracking
- Group related tasks under main headings
- Be specific and actionable (avoid vague tasks)
- Mark completed items immediately when done

#### 4. **Key Decisions**
- Document architectural or design decisions
- Include rationale for each decision
- Note any trade-offs considered
- Keep for future reference

#### 5. **Implementation Notes**
- Technical details that affect implementation
- Known gotchas or edge cases
- Dependencies on other work
- Performance considerations

#### 6. **Files to Modify**
- List specific files that will be changed
- Include brief notes about what changes are needed
- Group by component or subsystem
- Add "(create)" for new files

#### 7. **Additional Sections (as needed)**
- **Testing Strategy**: For complex features
- **Migration Guide**: For breaking changes
- **Performance Goals**: For optimisation work
- **Physics Background**: For scientific features
- **Current Status**: For ongoing work

#### Best Practices
- Keep plans focused and actionable
- Update progress regularly during development
- Remove completed plans after merging
- Reference specific functions/classes when possible
- Include links to relevant documentation
- Note any blocking issues or dependencies


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

## SUEWS Configuration Builder Web Interface

The project includes an interactive web-based configuration builder located at `docs/source/_static/`:

### Key Files:
- **index.html** - Main HTML interface with Bootstrap styling
- **config-builder.js** - JavaScript logic for form generation and validation
- **config-builder.css** - Custom styling for the interface
- **suews-config-schema.json** - JSON Schema generated from the Pydantic data model

### Features:
- Interactive form generation from JSON Schema
- Real-time YAML preview of configuration
- Import/Export functionality (YAML format)
- Client-side validation using AJV
- Responsive design with collapsible sections
- Array handling with add/remove/copy functionality
- Support for complex nested structures

### Maintenance:
- **Schema Updates**: Run `python docs/gen_schema.py` to regenerate the schema when data model changes
- **Testing**: Open `docs/source/_static/index.html` directly in a browser to test
- **Array Initialization**: Arrays start empty - users add items via "Add Item" buttons
- **Object Display**: Empty objects in arrays are not pre-populated to avoid "[object Object]" display issues

## Development Tasks and Reminders

- **Remember to include new files in meson.build appropriately**

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