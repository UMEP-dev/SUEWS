# Claude Plans Directory

This directory contains branch-specific documentation and plans for Claude Code sessions when working with git worktrees or feature branches.

## Purpose

When working on features across multiple Claude Code sessions or in different git worktrees, context is lost between sessions. These plan files serve as a "context bridge" to maintain continuity and provide appropriate documentation depth based on feature complexity.

## Unified Structure

Plans can be either simple (single file) or complex (directory with multiple files):

### Simple Features (Single File)
```
.claude/plans/doing/
└── feature-bug-fix.md          # Just a README for simple features
```

### Complex Features (Directory)
```
.claude/plans/doing/
└── feature-yaml-config/        # Directory for complex features
    ├── README.md              # Status tracking and overview (required)
    ├── requirements.md        # Detailed requirements (optional)
    ├── design.md             # Technical design (optional)
    └── tasks.md              # Task breakdown (optional)
```

## How It Works

1. **Claude Code automatically checks** for a plan matching the current branch name
2. **Plans are committed to master** so they're available in all worktrees after pulling
3. **Simple features** use a single markdown file for lightweight tracking
4. **Complex features** use a directory structure with optional specification files

### Why Plans Live in Master Branch

Plans are intentionally kept in the master branch rather than feature branches because:
- **Single source of truth**: All worktrees and Claude sessions see the same plan
- **No merge conflicts**: Plans don't interfere with code changes in feature branches
- **Easy sharing**: Multiple Claude sessions can work on the same feature with shared context
- **Clean feature branches**: Feature branches contain only code changes, not documentation

### Accessing Plans from Worktrees

When working in a worktree, plans are accessed via relative paths:
```bash
# For simple features
cat ../../.claude/plans/doing/feature-my-feature.md

# For complex features
cat ../../.claude/plans/doing/feature-my-feature/README.md
cat ../../.claude/plans/doing/feature-my-feature/requirements.md
```

### Updating Plans from Worktrees

Since plans live in master, updating them requires switching branches. See the detailed workflow in `CLAUDE.md` under "IMPORTANT: Updating Plans During Work". The basic flow is:
1. Edit plan using relative path from worktree
2. Switch to master in main repo to commit changes
3. Return to worktree to continue development

## Naming Conventions

### Simple Features
- File: `feature-{branch-name}.md`
- Example: `feature-bug-fix.md`

### Complex Features
- Directory: `feature-{branch-name}/`
- Files inside: `README.md`, `requirements.md`, `design.md`, `tasks.md`
- Example: `feature-yaml-config/README.md`

## Creating a New Plan

### For Simple Features

Use `/worktree new` which creates a single file from the template.

### For Complex Features

1. Create directory structure:
```bash
mkdir -p .claude/plans/todo/feature-{name}
```

2. Create README.md for status tracking (use as template):
```markdown
# Feature: [Feature Name]

## Lead Developer
- **GitHub**: @[username]
- **Started**: [YYYY-MM-DD]

## Context
Brief overview linking to detailed docs below.

## Specification
- [Requirements](requirements.md) - What we're building
- [Design](design.md) - How we're building it
- [Tasks](tasks.md) - Implementation steps

## Current Status
- **Phase**: Requirements gathering
- **Blockers**: None
- **Last Updated**: 2024-01-22
```

3. Add specification files as needed:
- `requirements.md` - User stories in EARS notation
- `design.md` - Technical architecture
- `tasks.md` - Implementation breakdown

## When to Use Each Structure

### Use Simple (Single File)
- Bug fixes (< 1 day)
- Small enhancements
- Documentation updates
- Simple refactoring
- Single developer work

### Use Complex (Directory)
- New features with API changes
- Multi-component changes
- Features requiring formal requirements
- Multi-developer collaboration
- Changes spanning multiple PRs

## EARS Notation (For Complex Features)

When writing requirements.md, use EARS notation:
```
WHEN [event/condition]
THE SYSTEM SHALL [expected behavior]
```

This ensures requirements are:
- Clear and unambiguous
- Testable
- Traceable to implementation

## Lifecycle

1. **Create** when starting a feature (simple or complex)
2. **Update** progress in README.md regularly
3. **Move** between todo/doing/done directories
4. **Archive** when feature is merged

## Cleanup

When a feature is complete and merged:
```bash
# For simple features
git mv .claude/plans/doing/feature-{name}.md .claude/plans/done/
git commit -m "Archive completed plan for feature/{name}"

# For complex features
git mv .claude/plans/doing/feature-{name}/ .claude/plans/done/
git commit -m "Archive completed plan for feature/{name}"
```

## Migration from Old Structure

If you have existing specs in `.claude/specs/`, move them:
```bash
# Move spec to plans directory
mv .claude/specs/feature-name .claude/plans/doing/

# Update references in worktree documentation
```

## Current Plans

- `todo/` - Planned features not yet started
- `doing/` - Active development
- `done/` - Completed features (archived)