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
    └── worktree-plans/   # Branch-specific context (in master)
        ├── README.md
        ├── feature-core-runtime-fixes.md
        └── ...
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
git rm .claude/worktree-plans/feature-{branch-name}.md
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
- **Benchmark Test** (`test_benchmark.py::test_benchmark1_same`): Validates SUEWS model outputs against known good results - **MUST NEVER BE SKIPPED**
- **Sample Output Test** (`test_supy.py::test_is_sample_output_same`): Validates consistency with sample data
- **Precheck Tests**: Validate input data preprocessing and validation logic
- **Data Model Tests**: Ensure data structures work correctly
- **Conditional Validation Tests**: Check physics option compatibility

### Benchmark Test Details

**CRITICAL**: The benchmark test has been moved to `test_benchmark.py` to avoid test interaction issues with shared global state in the Fortran module.

The benchmark test (`test_benchmark.py::TestBenchmark::test_benchmark1_same`) is particularly critical as it:
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