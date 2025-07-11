# Parallel Development with Multiple Claude Code Agents

This guide explains how to run multiple Claude Code instances in parallel, each working on different features simultaneously.

## When to Use Parallel Development

- Multiple independent features need development
- Different team members working on separate branches
- Testing different approaches simultaneously
- Maximizing development throughput

## Prerequisites

- Multiple terminal windows/tabs
- Separate worktrees for each feature
- Isolated environments (using uv recommended)
- Feature plans in `.claude/plans/`

## Quick Start Commands

### Setup Multiple Worktrees

```bash
# Terminal 1: Feature A
FEATURE_A="feature-a"
git worktree add worktrees/$FEATURE_A feature/$FEATURE_A
cd worktrees/$FEATURE_A
uv venv && source .venv/bin/activate
uv pip install -r requirements.txt  # Install dependencies
make dev

# Terminal 2: Feature B
FEATURE_B="feature-b"
git worktree add worktrees/$FEATURE_B feature/$FEATURE_B
cd worktrees/$FEATURE_B
uv venv && source .venv/bin/activate
uv pip install -r requirements.txt
make dev
```

### Launch Claude Code Agents

#### Agent 1: First Feature
```bash
# In Terminal 1
cd worktrees/feature-a
claude --no-chat
```

**First prompt**: 
```
Read ../../.claude/plans/doing/feature-a.md and start working on the first uncompleted task. Run make dev first if not already done.
```

#### Agent 2: Second Feature
```bash
# In Terminal 2
cd worktrees/feature-b
claude --no-chat
```

**First prompt**:
```
Read ../../.claude/plans/doing/feature-b.md and start working on the tasks. Run make dev first if not already done.
```

## Environment Isolation

**CRITICAL**: Each worktree MUST have its own environment!

### With uv (Recommended)
```bash
cd worktrees/my-feature
uv venv
source .venv/bin/activate
# Each worktree has .venv/ directory
```

### With mamba (Legacy)
```bash
# Never use the base environment in worktrees
mamba create -n suews-dev-feature-a --clone suews-dev
mamba activate suews-dev-feature-a
```

## Agent Instructions Template

When launching each agent, provide clear instructions:

```
1. Check current branch: git branch --show-current
2. Read the feature plan: cat ../../.claude/plans/doing/[feature-name].md
3. Ensure environment is set up: make dev
4. Start with the first uncompleted task
5. Run tests after each change: make test
6. Update the plan progress as you work
7. Commit with clear messages when tasks are complete
```

## Managing Multiple Agents

### Monitor Progress
- Check each agent's output regularly
- Review commits: `git log --oneline -10`
- Run tests: `make test`
- Update plans in `.claude/plans/doing/`

### Avoid Conflicts
- Work on independent features
- Don't modify shared files simultaneously
- Communicate through plan updates
- Use separate branches always

### Resource Management
- Each agent uses ~2-4GB RAM
- Monitor CPU usage
- Close unused agents
- Clean up completed worktrees

## Example Workflow

### High Priority Bug Fixes
```bash
# Agent 1: Critical runtime fixes
cd worktrees/core-bugs
claude --no-chat
# "Work on issue #391 from the plan"

# Agent 2: Default value audit
cd worktrees/default-values
claude --no-chat
# "Work on issue #428 to remove problematic defaults"
```

### Feature Development
```bash
# Agent 1: New feature implementation
cd worktrees/new-feature
claude --no-chat
# "Implement the API endpoints from the plan"

# Agent 2: Testing and documentation
cd worktrees/new-feature-tests
claude --no-chat
# "Write tests for the new API endpoints"
```

## Cleanup

After completing work:

```bash
# For each completed feature
FEATURE="completed-feature"

# 1. Ensure all changes are committed and pushed
git push origin feature/$FEATURE

# 2. Move plan to done
mv .claude/plans/doing/feature-$FEATURE.md .claude/plans/done/

# 3. Remove worktree
git worktree remove worktrees/$FEATURE

# 4. Clean up branch after merge
git branch -d feature/$FEATURE
git push origin --delete feature/$FEATURE
```

## Tips for Success

1. **Start Simple**: Begin with 2 agents, add more as needed
2. **Clear Plans**: Ensure each feature plan is detailed
3. **Regular Syncs**: Pull changes in master regularly
4. **Test Often**: Run tests after each significant change
5. **Document Progress**: Update plans as work proceeds

## Troubleshooting

### "Already being edited" errors
- Each worktree needs its own environment
- Don't share environments between agents

### Import errors
- Run `make dev` in each worktree
- Ensure environment is activated

### Git conflicts
- Work on independent features
- Communicate through plan updates
- Resolve conflicts in one agent at a time