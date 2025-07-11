# Worktree Build System Analysis and Solutions

## Current Build System Analysis

### Build Configuration
- **Build Tool**: Meson (modern Python build system)
- **Build Directory**: `build/` in the project root
- **Development Install**: `make dev` or `make dev-fast` (editable install)
- **Testing**: `make test` runs pytest

### Potential Conflicts Between Worktrees

#### IDENTIFIED ISSUES:

1. **Shared Build Directory**
   - All worktrees share the same `.git` directory
   - Meson creates `build/` directory in the current working directory
   - Each worktree will create its own `build/` directory - NO CONFLICT

2. **Python Editable Install**
   - `pip install --editable .` creates links to the current directory
   - Multiple worktrees can have different code versions
   - **CONFLICT**: Only one editable install can be active at a time in the same Python environment

3. **Fortran Object Files**
   - Built in `src/suews/lib/` and `src/suews/mod/`
   - Each worktree has its own copy - NO CONFLICT

4. **Shared Python Environment**
   - If using the same conda/mamba environment across worktrees
   - **MAJOR CONFLICT**: Can only have one version of supy installed

## Solutions for Build Isolation

### Solution 1: Separate Python Environments (RECOMMENDED)
Create a unique conda/mamba environment for each worktree:

```bash
# For each worktree
mamba create -n suews-dev-{feature-name} --clone suews-dev
mamba activate suews-dev-{feature-name}
cd worktrees/{feature-name}
make dev
```

**Pros**: Complete isolation, no conflicts
**Cons**: More disk space, need to switch environments

### Solution 2: Build Directory Isolation
Modify build to use unique directories:

```bash
# Set build directory per worktree
export MESON_BUILD_DIR="build-$(git branch --show-current)"
```

**Note**: Would require Makefile modifications

### Solution 3: Sequential Development
Work on one worktree at a time:

```bash
# Before switching worktrees
make clean
# In new worktree
make dev
```

**Pros**: Simple, no changes needed
**Cons**: Can't run multiple agents simultaneously

## Updated Agent Instructions

### Agent Workflow with Testing

```markdown
## Development Workflow for Claude Code Agents

### Initial Setup (per worktree)
1. Navigate to your worktree:
   ```bash
   cd worktrees/{feature-name}
   ```

2. Check current branch and read plan:
   ```bash
   git branch --show-current
   cat ../../.claude-plans/feature-{branch-name}.md
   ```

3. Create isolated environment (RECOMMENDED):
   ```bash
   mamba create -n suews-dev-{feature-name} --clone suews-dev
   mamba activate suews-dev-{feature-name}
   ```

4. Build development version:
   ```bash
   make dev  # or make dev-fast for faster builds
   ```

### During Development

#### After Each Code Change:
1. **Quick Test** (for Python changes):
   ```bash
   make test
   ```

2. **Full Rebuild** (for Fortran changes):
   ```bash
   make clean && make dev
   make test
   ```

#### Testing Requirements:
- Run `make test` after completing each task
- Fix any failing tests before marking task complete
- Add new tests for bug fixes and features

### Committing Work

1. **Run full test suite**:
   ```bash
   make clean && make dev && make test
   ```

2. **Update plan progress**:
   - Edit `../../.claude-plans/feature-{branch-name}.md`
   - Mark completed tasks with [x]
   - Add implementation notes

3. **Commit changes**:
   ```bash
   git add -A
   git commit -m "feat: description of changes

   - Detail 1
   - Detail 2
   
   Addresses #issue-number"
   ```

4. **Push to remote**:
   ```bash
   git push origin feature/{branch-name}
   ```

### End of Session Checklist
- [ ] All tests passing (`make test`)
- [ ] Plan updated with progress
- [ ] Changes committed with descriptive message
- [ ] Implementation notes added to plan
- [ ] Any blocking issues documented
```

## Recommended Setup for Multiple Agents

### Option A: Isolated Environments (Best for Parallel Work)
```bash
# Agent 1
mamba create -n suews-core-fixes --clone suews-dev
mamba activate suews-core-fixes
cd worktrees/core-bugs
make dev

# Agent 2 (different terminal)
mamba create -n suews-defaults --clone suews-dev
mamba activate suews-defaults
cd worktrees/default-values
make dev
```

### Option B: Sequential Work (Simpler Setup)
Each agent works in sequence, cleaning before switching:
```bash
# Agent finishes work
make clean
# Next agent starts
cd ../other-worktree
make dev
```

## Testing Strategy

### Test Commands by Scope:
1. **Unit tests only**: `pytest test/test_supy.py -v`
2. **Quick validation**: `pytest test/test_suews_simulation.py::test_single_step -v`
3. **Full test suite**: `make test`
4. **Benchmark test**: `pytest test/test_suews_simulation.py::test_benchmark -v`

### Performance Considerations:
- `make dev-fast` - Faster compilation for development
- Run specific tests during development
- Full test suite before committing

## Summary
- **Primary Issue**: Python editable installs conflict between worktrees
- **Recommended Solution**: Use separate conda environments per worktree
- **Testing**: Run `make test` frequently, especially before commits
- **Build Commands**: `make dev` → `make test` → commit → push