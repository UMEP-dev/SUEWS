# Feature Development Workflow

> **Purpose**: Guide for managing large features through incremental PRs and effective branch strategies
> 
> **Audience**: Core developers and contributors working on substantial features
>
> **Last Updated**: 28 October 2025

## Overview

This guide provides strategies for breaking down large features into manageable, reviewable PRs whilst maintaining code quality and scientific validity.

## When to Use Incremental PRs

### Use Incremental Strategy When:
- ✅ Feature requires >500 lines of code changes
- ✅ Multiple independent components can be separated
- ✅ Early feedback needed on architectural decisions
- ✅ Feature spans multiple modules or subsystems
- ✅ Implementation will take >1 week
- ✅ Testing can be done incrementally

### Single PR is Fine When:
- Small, focused changes (<200 lines)
- Bug fixes with clear scope
- Documentation updates
- Simple refactoring within one module

## Breaking Down Features: Strategies

### 1. Bottom-Up Approach (Recommended)

Build foundation first, then higher-level features:

```
PR1: Add low-level utility functions
  ↓
PR2: Implement core algorithm/physics
  ↓
PR3: Add configuration interface
  ↓
PR4: Integrate with existing modules
  ↓
PR5: Add user-facing API
```

**Example: Adding New Physics Module**
```
PR1: Add Fortran subroutines for new physics calculations
PR2: Create Python wrapper for new module
PR3: Add configuration schema and validation
PR4: Update site configuration to support new parameters
PR5: Add documentation and examples
```

**Advantages:**
- Each PR is independently testable
- Earlier PRs can be used immediately
- Easier to review (smaller, focused changes)
- Safer rollback if issues discovered

### 2. Horizontal Slice Approach

Implement complete vertical slice for subset of functionality:

```
PR1: Implement feature for single surface type
PR2: Extend to all surface types
PR3: Add advanced options/variants
```

**Example: Improving Snow Module**
```
PR1: Enhanced snow physics for built surface only
PR2: Extend to paved, vegetation surfaces
PR3: Add optional detailed snow age tracking
```

### 3. Preparatory Refactoring

Clean up before adding new features:

```
PR1: Refactor existing code for better structure
  ↓
PR2: Add new feature using improved structure
```

**Example:**
```
PR1: Extract common resistance calculations to utility module
PR2: Add new resistance scheme using extracted utilities
```

## Managing Dependencies Between PRs

### Stacked PRs Pattern

When PRs depend on each other sequentially:

#### Setup

**Using Git Worktree:**
```bash
# Create worktree for PR1
git worktree add ../suews-step1 -b feat/step1-foundation
cd ../suews-step1
# Implement PR1
git push origin feat/step1-foundation
# Create PR1 targeting master

# Create worktree for PR2 (based on PR1)
git worktree add ../suews-step2 -b feat/step2-core-logic feat/step1-foundation
cd ../suews-step2
# Implement PR2 (depends on PR1)
git push origin feat/step2-core-logic
# Create PR2 targeting feat/step1-foundation (NOT master)
```

**Using Branch Switching (Single Directory):**
```bash
# Work on PR1
git checkout -b feat/step1-foundation
# Implement PR1
git push origin feat/step1-foundation
# Create PR1 targeting master

# Work on PR2 (based on PR1)
git checkout feat/step1-foundation
git checkout -b feat/step2-core-logic
# Implement PR2 (depends on PR1)
git push origin feat/step2-core-logic
# Create PR2 targeting feat/step1-foundation (NOT master)
```

#### Review Process
1. PR1 reviewed and approved first
2. PR2 reviewed (reviewers see only PR2 changes, not PR1)
3. Merge PR1 to master
4. Update PR2 to target master (GitHub does this automatically)
5. Merge PR2 to master

#### Keeping Stacked PRs in Sync
```bash
# If master updated whilst PR1 pending
cd workspace1
git checkout feat/step1-foundation
git rebase master
git push --force-with-lease

# Update dependent PR2
cd workspace2
git checkout feat/step2-core-logic
git rebase feat/step1-foundation
git push --force-with-lease
```

### Parallel Independent PRs

When components can be developed independently:

**Using Git Worktree:**
```bash
# Create worktree for component A
git worktree add ../suews-component-a -b feat/component-a
cd ../suews-component-a
# Work on component A

# Create worktree for component B (parallel)
git worktree add ../suews-component-b -b feat/component-b
cd ../suews-component-b
# Work on component B

# Both PRs target master independently
```

**Using Branch Switching:**
```bash
# Work on component A
git checkout -b feat/component-a
# Commit changes, push
git checkout master

# Work on component B
git checkout -b feat/component-b
# Commit changes, push

# Switch between branches as needed
```

**Best for:**
- Different physics modules
- Documentation vs code
- Tests for existing features
- Independent bug fixes

## Parallel Workspace Development

Working on multiple related PRs simultaneously is easier with separate workspaces. This can be achieved with:

### Tools for Parallel Development

**Git Worktree (Built-in, Recommended)**
```bash
# Create worktree for PR1
git worktree add ../suews-foundation feat/step1-foundation

# Create worktree for PR2
git worktree add ../suews-integration feat/step2-integration

# Work in each directory independently
cd ../suews-foundation    # Work on PR1
cd ../suews-integration   # Work on PR2

# Clean up when done
git worktree remove ../suews-foundation
```

**Alternative Tools:**
- **Conductor**: Mac app with visual workspace management
- **Separate clones**: Simple but uses more disk space
- **Branch switching**: `git checkout` between branches (slower, single workspace)

### Workspace Strategy Patterns

**Pattern 1: Sequential Development**
```
Worktree/workspace 1 → PR1 (core implementation)
Worktree/workspace 2 → PR2 (depends on PR1)
Worktree/workspace 3 → PR3 (depends on PR2)
```

**Pattern 2: Parallel + Integration**
```
Worktree/workspace 1 → PR1 (physics changes)
Worktree/workspace 2 → PR2 (configuration changes)
Worktree/workspace 3 → PR3 (integrate PR1 + PR2)
```

**Pattern 3: Experimental + Production**
```
Worktree/workspace 1 → Try different approaches
Worktree/workspace 2 → Final implementation
```

### Benefits of Parallel Workspaces
- Work on multiple PRs simultaneously without branch switching
- Test integration before merging dependent PRs
- Maintain separate environments for conflicting experiments
- Easy comparison between approaches
- Each workspace can have its own virtual environment

## PR Size Guidelines

### Target Sizes
- **Ideal**: 100-300 lines changed
- **Acceptable**: 300-500 lines
- **Large**: 500-800 lines (needs strong justification)
- **Too Large**: >800 lines (break it down)

**Lines exclude:**
- Auto-generated files
- Test fixtures/data
- Documentation formatting

### Measuring PR Size
```bash
# Count changes in current branch
git diff master --shortstat

# Detailed breakdown
git diff master --stat
```

## Documentation Strategy

### Document Incrementally
Each PR should include relevant documentation:

**PR1 (Core Implementation):**
```markdown
# In PR description
## Partial Implementation
This PR implements the foundation for [feature].
Remaining work tracked in #XXX.

## Documentation
Added internal docstrings for new functions.
User documentation will be added in final PR.
```

**Final PR (User-Facing):**
- Complete user guide
- API documentation
- Examples and tutorials
- CHANGELOG entry (comprehensive)

### Maintaining Documentation Consistency
- Use `<!-- TODO: Complete in PR#XXX -->` markers
- Link related PRs in documentation
- Update main docs only in final PR to avoid conflicts

## Testing Strategy for Incremental PRs

### Each PR Must:
1. ✅ Pass all existing tests
2. ✅ Add tests for new functionality
3. ✅ Not break benchmark tests (or document expected changes)

### Testing Levels

**PR1 (Foundation):**
```python
# Unit tests for new utilities
def test_new_calculation():
    assert calculate_new_parameter(...) == expected
```

**PR2 (Integration):**
```python
# Integration tests
def test_module_integration():
    # Test PR1 + PR2 work together
```

**Final PR:**
```python
# End-to-end tests
def test_complete_feature():
    # Full workflow test
```

## Communication & Tracking

### Creating the Feature Issue

**Before starting**, create a tracking issue:

```markdown
## Feature: [Name]

### Overview
[Description of complete feature]

### Implementation Plan
- [ ] PR1: Foundation (#link when created)
- [ ] PR2: Core logic (#link)
- [ ] PR3: Integration (#link)
- [ ] PR4: Documentation (#link)

### Dependencies
- Depends on #XXX
- Related to #YYY

### Timeline
Target completion: [Date or milestone]
```

### PR Descriptions for Incremental Work

```markdown
## Part X/N: [Component Name]

### Context
This is PR X of N implementing [feature] (tracking issue #XXX)

### This PR Adds
- Specific change 1
- Specific change 2

### Not in This PR (Future Work)
- Component Y (will be in PR#XXX)
- Component Z (will be in PR#YYY)

### Testing
[How this partial implementation is tested]

### Next Steps
Next PR will add [brief description]
```

## Merge Strategies

### For Stacked PRs

**Option 1: Squash Each PR (Recommended)**
```
feat/step1 → squash merge to master (1 commit)
feat/step2 → squash merge to master (1 commit)
feat/step3 → squash merge to master (1 commit)
```

**Advantages:**
- Clean history
- Each PR becomes one atomic commit
- Easy to revert if needed

**Option 2: Rebase and Merge**
```
Preserves individual commits from each PR
```

**Use when:**
- Commits have meaningful history
- Scientific validation tied to specific commits

### Resolving Conflicts

**If master updated during PR development:**

```bash
# Update your branch
git checkout feat/your-feature
git fetch origin
git rebase origin/master

# Resolve conflicts
git add resolved_files
git rebase --continue

# Force push (safe with --force-with-lease)
git push --force-with-lease origin feat/your-feature
```

**For stacked PRs:**
Must rebase in order (PR1, then PR2, then PR3)

## Common Patterns & Examples

### Pattern: New Physics Module

**Full Feature:** Add SPARTACUS radiation module

```
PR1: suews_phys_spartacus.f95 (core calculations)
  - Fortran implementation
  - Unit tests for physics
  - Scientific validation

PR2: Python wrapper (src/supy/_spartacus.py)
  - ctypes interface
  - Python tests
  - Integration with existing code

PR3: Configuration schema
  - Pydantic models
  - YAML structure
  - Validation rules

PR4: Site configuration integration
  - Update site config to include SPARTACUS params
  - Migration logic if needed

PR5: Documentation
  - User guide
  - Examples
  - CHANGELOG
```

### Pattern: Major Refactoring

**Full Feature:** Modernise configuration system

```
PR1: Add new configuration classes (no behaviour change)
  - Pydantic models
  - Tests for new classes
  - Parallel to existing system

PR2: Add migration path
  - Functions to convert old → new
  - Tests for migration
  - Deprecation warnings

PR3: Update internal code to use new system
  - Refactor module by module
  - Each module gets tests

PR4: Remove old system
  - Clean up deprecated code
  - Update all documentation
```

### Pattern: Performance Optimisation

```
PR1: Add benchmarking infrastructure
  - Benchmark tests
  - Performance metrics

PR2: Optimise hot path (component A)
  - Implementation
  - Verify no behaviour change
  - Benchmark improvements

PR3: Optimise component B
  - Similar to PR2

PR4: Documentation
  - Performance guide
  - Benchmark results
```

## Checklist for Feature Planning

Before starting large feature:

- [ ] Created tracking issue with implementation plan
- [ ] Identified logical breakpoints for PRs
- [ ] Each sub-PR can be tested independently
- [ ] Each sub-PR adds value (not just scaffolding)
- [ ] Considered review burden (each PR reviewable in <30 min)
- [ ] Planned documentation strategy
- [ ] Identified potential conflicts with ongoing work
- [ ] Confirmed approach with maintainers (if major change)

## Troubleshooting

### "My stacked PRs have conflicts"

**Solution:** Rebase in order
```bash
# Workspace 1 (PR1)
git checkout feat/step1
git rebase master
git push --force-with-lease

# Workspace 2 (PR2)
git checkout feat/step2
git rebase feat/step1
git push --force-with-lease
```

### "Reviewers want changes in PR1 but PR2 depends on it"

**Solution:**
1. Make changes in PR1
2. Update PR2 to incorporate changes:
   ```bash
   git checkout feat/step2
   git rebase feat/step1
   ```

### "Master moved ahead, many conflicts"

**Prevention:** Rebase regularly (daily for active features)

**Solution:** Consider squashing intermediate commits before rebasing:
```bash
git rebase -i master
# Squash related commits
git push --force-with-lease
```

### "Final integration reveals issues between PRs"

**Prevention:**
- Create integration worktree/workspace to test all PRs together
- Add integration tests early

**Solution:**
- Create final "integration" PR that fixes cross-PR issues
- Or fix in last PR before merging

## Best Practices Summary

1. **Plan before coding**: Sketch out PR breakdown first
2. **Bottom-up usually best**: Foundation before features
3. **Independent is better**: Parallel PRs > stacked PRs when possible
4. **Test incrementally**: Each PR fully tested
5. **Communicate clearly**: Link PRs, update tracking issue
6. **Review burden**: Keep PRs reviewable (<30 min each)
7. **Document intent**: Explain what's NOT in each PR
8. **Sync frequently**: Rebase daily during active development
9. **Use parallel workspaces**: Git worktree for complex features
10. **Scientific review**: Plan module-level review for physics PRs

## See Also

- `REVIEW_PROCESS.md` - Scientific review workflow
- `CODING_GUIDELINES.md` - Git and commit conventions
- `RELEASE_MANUAL.md` - Hotfix branch strategy
- `.claude/reference/maintenance-principles.md` - DRY principles

---

*This guide helps manage complex feature development through incremental, reviewable PRs.*
*Last updated: 28 October 2025*
*Owner: SUEWS Development Team*
*Location: `/dev-ref/FEATURE_DEVELOPMENT_WORKFLOW.md`*
